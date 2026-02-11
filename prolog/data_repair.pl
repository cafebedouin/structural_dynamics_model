:- module(data_repair, [
    repair_interval/1,           % Legacy: computes + persists (backward compat)
    repair_interval/2,           % Pure: repair_interval(+ID, -Results)
    bridge_v34_data/2,           % Pure: bridge_v34_data(+ID, -BridgeResults)
    impute_missing_metrics/2,    % Pure: impute_missing_metrics(+ID, -ImputeResults)
    persist_bridge_results/1     % Asserts bridge_result list into narrative_ontology
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(domain_priors). % NEW: Hook into the Epistemic Prior Library
:- use_module(signature_mapper).
:- use_module(constraint_indexing).
:- use_module(drl_core).

% Ensure we can add facts to the ontology's measurement predicate
:- dynamic narrative_ontology:measurement/5.

/* ============================================================
   DATA REPAIR — Stage 1 of Validation Pipeline
   ============================================================
   Imputation engine. Fills missing measurement/5 facts using
   domain-specific epistemic priors before tests run. Also
   bridges v3.4 testset data (constraint_classification/3,
   domain_priors) into the narrative_ontology format
   (constraint_claim/2, constraint_metric/3).

   Run by scenario_manager:load_and_run/2 BEFORE test_harness.

   See also: data_verification.pl (Stage 2 — verifies after repair),
             data_validation.pl (Stage 3 — audits quality after tests).
   ============================================================ */

/* ============================================================
   REPAIR ORCHESTRATOR — Pure Return-Value API (Issue #6 resolved)
   ============================================================
   Exports:
     repair_interval/1         — Legacy wrapper: compute + persist (backward compat)
     repair_interval/2         — Pure: repair_interval(+ID, -Results)
     bridge_v34_data/2         — Pure: bridge_v34_data(+ID, -BridgeResults)
     impute_missing_metrics/2  — Pure: impute_missing_metrics(+ID, -ImputeResults)
     persist_bridge_results/1  — Asserts bridge_result list into narrative_ontology

   Return type: bridge_result(Fact) where Fact is the ontology term,
   e.g. bridge_result(constraint_metric(ID, Key, Val)).

   Accumulator threading: each pure sub-stage receives AccIn,
   appends bridge_result(Fact) terms, produces AccOut. The helper
   acc_has/2 checks both the accumulator and the existing DB.
   ============================================================ */

/* ============================================================
   ACCUMULATOR HELPERS
   ============================================================ */

%% acc_has(+Acc, +FactPattern)
%  Check if FactPattern exists in the accumulator list (as a
%  bridge_result wrapper) or in the narrative_ontology database.
acc_has(Acc, Fact) :-
    (   member(bridge_result(Fact), Acc)
    ->  true
    ;   narrative_ontology:Fact
    ).

%% temp_assert_metrics(+Acc, -Refs)
%  Temporarily asserts constraint_metric facts from Acc into the DB.
%  Returns clause references for cleanup via temp_retract_metrics/1.
%  Follows the setup_call_cleanup bracket pattern from psych_bridge.pl.
temp_assert_metrics(Acc, Refs) :-
    findall(Ref,
        (   member(bridge_result(constraint_metric(ID, Key, Val)), Acc),
            \+ narrative_ontology:constraint_metric(ID, Key, Val),
            asserta(narrative_ontology:constraint_metric(ID, Key, Val), Ref)
        ),
        Refs).

%% temp_retract_metrics(+Refs)
%  Retracts temporarily asserted metrics using clause references.
temp_retract_metrics(Refs) :-
    forall(member(Ref, Refs), erase(Ref)).

/* ============================================================
   PURE BRIDGE PREDICATES (AccIn/AccOut pattern)

   Each replaces a corresponding assertz-based internal predicate.
   Receives AccIn, appends bridge_result(Fact) terms, produces AccOut.
   ============================================================ */

%% bridge_domain_metrics_pure(+IntervalID, +AccIn, -AccOut)
%  Pure version of bridge_domain_metrics/1.
%  Maps domain_priors predicates to bridge_result(constraint_metric(...)).
bridge_domain_metrics_pure(IntervalID, AccIn, AccOut) :-
    config:param(extractiveness_metric_name, ExtMetricKey),
    bridge_single_metric_pure(IntervalID, base_extractiveness, ExtMetricKey, AccIn, Acc1),
    config:param(suppression_metric_name, SuppMetricKey),
    bridge_single_metric_pure(IntervalID, suppression_score, SuppMetricKey, Acc1, Acc2),
    config:param(theater_metric_name, TheaterMetricKey),
    bridge_single_metric_pure(IntervalID, TheaterMetricKey, TheaterMetricKey, Acc2, AccOut).

%% bridge_single_metric_pure(+IntervalID, +PriorPred, +MetricKey, +AccIn, -AccOut)
%  Pure version of bridge_single_metric/3.
%  Checks acc before computing; appends bridge_result on success.
bridge_single_metric_pure(IntervalID, PriorPred, MetricKey, AccIn, AccOut) :-
    (   acc_has(AccIn, constraint_metric(IntervalID, MetricKey, _))
    ->  AccOut = AccIn
    ;   (   Goal =.. [PriorPred, IntervalID, Value],
            catch(domain_priors:call(Goal), _, fail)
        ->  AccOut = [bridge_result(constraint_metric(IntervalID, MetricKey, Value)) | AccIn],
            format('  [BRIDGE] Derived metric ~w = ~w for ~w from domain_priors~n', [MetricKey, Value, IntervalID])
        ;   (   get_default_metric(MetricKey, DefaultValue)
            ->  AccOut = [bridge_result(constraint_metric(IntervalID, MetricKey, DefaultValue)) | AccIn],
                format('  [BRIDGE] Derived metric ~w = ~w for ~w from config default~n', [MetricKey, DefaultValue, IntervalID])
            ;   AccOut = AccIn
            )
        )
    ).

%% bridge_beneficiary_victim_pure(+IntervalID, +AccIn, -AccOut)
%  Pure version of bridge_beneficiary_victim/1.
%  Reads metrics from accumulator via acc_has/2.
bridge_beneficiary_victim_pure(IntervalID, AccIn, AccOut) :-
    % Beneficiary
    (   acc_has(AccIn, constraint_beneficiary(IntervalID, _))
    ->  Acc1 = AccIn
    ;   (   acc_has(AccIn, constraint_metric(IntervalID, extractiveness, E)),
            E > 0.46,
            acc_has(AccIn, constraint_metric(IntervalID, suppression_requirement, S)),
            S > 0.40
        ->  Acc1 = [bridge_result(constraint_beneficiary(IntervalID, inferred_institutional)) | AccIn],
            format('  [BRIDGE] Derived constraint_beneficiary(~w, inferred_institutional) from metrics (E=~2f, S=~2f)~n',
                   [IntervalID, E, S])
        ;   Acc1 = AccIn
        )
    ),
    % Victim
    (   acc_has(Acc1, constraint_victim(IntervalID, _))
    ->  AccOut = Acc1
    ;   (   acc_has(Acc1, constraint_metric(IntervalID, extractiveness, E2)),
            E2 > 0.46,
            acc_has(Acc1, constraint_metric(IntervalID, suppression_requirement, S2)),
            S2 > 0.40
        ->  AccOut = [bridge_result(constraint_victim(IntervalID, inferred_subject)) | Acc1],
            format('  [BRIDGE] Derived constraint_victim(~w, inferred_subject) from metrics (E=~2f, S=~2f)~n',
                   [IntervalID, E2, S2])
        ;   AccOut = Acc1
        )
    ).

%% bridge_scaffold_markers_pure(+IntervalID, +AccIn, -AccOut)
%  Pure version of bridge_scaffold_markers/1.
%  Checks acc for existing beneficiary before deriving scaffold markers.
bridge_scaffold_markers_pure(IntervalID, AccIn, AccOut) :-
    % Check if any context declares this constraint as scaffold
    (   constraint_indexing:constraint_classification(IntervalID, scaffold, _)
    ->  % Bridge has_sunset_clause (scaffold implies sunset by definition)
        (   acc_has(AccIn, has_sunset_clause(IntervalID))
        ->  Acc1 = AccIn
        ;   Acc1 = [bridge_result(has_sunset_clause(IntervalID)) | AccIn],
            format('  [BRIDGE] Derived has_sunset_clause(~w) from scaffold declaration~n',
                   [IntervalID])
        ),
        % Bridge has_coordination_function via constraint_beneficiary
        (   acc_has(Acc1, constraint_beneficiary(IntervalID, _))
        ->  AccOut = Acc1
        ;   AccOut = [bridge_result(constraint_beneficiary(IntervalID, coordinated_group)) | Acc1],
            format('  [BRIDGE] Derived constraint_beneficiary(~w, coordinated_group) from scaffold declaration~n',
                   [IntervalID])
        )
    ;   AccOut = AccIn  % Not declared as scaffold, nothing to bridge
    ).

%% ACKNOWLEDGED IMPURITY: setup_call_cleanup temp-assert bracket
%  dr_type/2 reads constraint_metric/3 from the DB, so accumulated
%  metrics must be temporarily visible via asserta + erase(Ref).
%  setup_call_cleanup guarantees cleanup even on exception.
%  Eliminating this would require a metrics-argument variant of
%  drl_core:dr_type — high cost, marginal benefit. Accepted.

%% bridge_constraint_claim_pure(+IntervalID, +AccIn, -AccOut)
%  Pure version of bridge_constraint_claim/1.
%  Uses setup_call_cleanup temp-assert bracket for dr_type Priority 2.
bridge_constraint_claim_pure(IntervalID, AccIn, AccOut) :-
    (   acc_has(AccIn, constraint_claim(IntervalID, _))
    ->  AccOut = AccIn  % Already has a claim
    ;   % PRIORITY 1: Use declared analytical perspective classification
        (   constraint_indexing:default_context(AnalyticalCtx),
            constraint_indexing:constraint_classification(IntervalID, DeclaredType, AnalyticalCtx)
        ->  AccOut = [bridge_result(constraint_claim(IntervalID, DeclaredType)) | AccIn],
            format('  [BRIDGE] Derived constraint_claim(~w, ~w) from declared analytical classification~n',
                   [IntervalID, DeclaredType])
        ;   % PRIORITY 2: Compute from dr_type (temp-assert bracket for accumulated metrics)
            (   setup_call_cleanup(
                    temp_assert_metrics(AccIn, Refs),
                    (   catch(drl_core:dr_type(IntervalID, ComputedType), _, fail),
                        ComputedType \= unknown
                    ),
                    temp_retract_metrics(Refs)
                )
            ->  AccOut = [bridge_result(constraint_claim(IntervalID, ComputedType)) | AccIn],
                format('  [BRIDGE] Derived constraint_claim(~w, ~w) from computed analytical classification~n',
                       [IntervalID, ComputedType])
            ;   % PRIORITY 3: Use first declared classification
                (   constraint_indexing:constraint_classification(IntervalID, FallbackType, _)
                ->  AccOut = [bridge_result(constraint_claim(IntervalID, FallbackType)) | AccIn],
                    format('  [BRIDGE] Derived constraint_claim(~w, ~w) from first indexed classification~n',
                           [IntervalID, FallbackType])
                ;   AccOut = AccIn
                )
            )
        )
    ).

%% bridge_omega_variables_pure(+IntervalID, +AccIn, -AccOut)
%  Pure version of bridge_omega_variables/1.
%  Uses findall to collect omega_variable terms instead of forall+assertz.
bridge_omega_variables_pure(IntervalID, AccIn, AccOut) :-
    % Check for omega_variable/5 in the testset module
    (   current_module(IntervalID),
        predicate_property(IntervalID:omega_variable(_,_,_,_,_), defined)
    ->  findall(
            bridge_result(omega_variable(OID, empirical, Question)),
            (   catch(IntervalID:omega_variable(OID, Question, _Measurement, _Resolution, _Conf), _, fail),
                \+ acc_has(AccIn, omega_variable(OID, _, _))
            ),
            Omega5Results
        ),
        forall(member(bridge_result(omega_variable(OID5, _, _)), Omega5Results),
               format('  [BRIDGE] Imported omega ~w from module ~w~n', [OID5, IntervalID]))
    ;   Omega5Results = []
    ),
    append(Omega5Results, AccIn, Acc1),
    % Also check for omega_variable/3 in the testset module
    (   current_module(IntervalID),
        predicate_property(IntervalID:omega_variable(_,_,_), defined)
    ->  findall(
            bridge_result(omega_variable(OID3, Type3, Desc3)),
            (   catch(IntervalID:omega_variable(OID3, Type3, Desc3), _, fail),
                \+ acc_has(Acc1, omega_variable(OID3, _, _))
            ),
            Omega3Results
        ),
        forall(member(bridge_result(omega_variable(OID3b, _, _)), Omega3Results),
               format('  [BRIDGE] Imported omega ~w from module ~w~n', [OID3b, IntervalID]))
    ;   Omega3Results = []
    ),
    append(Omega3Results, Acc1, AccOut).

/* ============================================================
   EXPORTED PURE API
   ============================================================ */

%% bridge_v34_data(+IntervalID, -Results)
%  Pure bridge pipeline: derives constraint_claim, constraint_metric,
%  beneficiary/victim, scaffold markers, and omega_variable facts from
%  v3.4 indexed data. Returns bridge_result(...) terms without asserting.
%  Bridge ordering preserved: metrics -> beneficiary/victim -> scaffold -> claim -> omega.
bridge_v34_data(IntervalID, Results) :-
    bridge_domain_metrics_pure(IntervalID, [], Acc1),
    bridge_beneficiary_victim_pure(IntervalID, Acc1, Acc2),
    bridge_scaffold_markers_pure(IntervalID, Acc2, Acc3),
    bridge_constraint_claim_pure(IntervalID, Acc3, Acc4),
    bridge_omega_variables_pure(IntervalID, Acc4, Results).

%% impute_missing_metrics(+IntervalID, -Results)
%  Pure version of repair_point + ensure_metric_exists.
%  Returns bridge_result(measurement(...)) terms for all missing
%  measurements without asserting into the database.
impute_missing_metrics(IntervalID, Results) :-
    narrative_ontology:interval(IntervalID, T0, Tn),
    findall(
        bridge_result(measurement(SyntheticID, IntervalID, Metric, Time, Value)),
        (   config:level(L),
            member(Time, [T0, Tn]),
            member(Metric, [accessibility_collapse(L), stakes_inflation(L),
                           suppression(L), resistance(L)]),
            \+ narrative_ontology:measurement(_, _, Metric, Time, _),
            domain_priors:get_prior(IntervalID, Metric, Value),
            (domain_priors:is_known_domain(IntervalID) -> true ; domain_priors:flag_novelty(IntervalID)),
            gensym(repair_m_, SyntheticID)
        ),
        Results
    ),
    forall(member(bridge_result(measurement(_, _, M, T, V)), Results),
           format('  [FIXED] Imputed ~w for ~w at T=~w~n', [V, M, T])).

%% repair_interval(+IntervalID, -Results)
%  Pure version of repair_interval/1. Returns all bridge and impute
%  results as bridge_result(...) terms without modifying the database.
repair_interval(IntervalID, Results) :-
    bridge_v34_data(IntervalID, BridgeResults),
    impute_missing_metrics(IntervalID, ImputeResults),
    append(BridgeResults, ImputeResults, Results).

/* ============================================================
   PERSIST LAYER
   ============================================================
   persist_bridge_results/1 asserts a list of bridge_result(...)
   terms into narrative_ontology. Each fact type has a duplicate
   guard (check-before-assert) mirroring the original bridge logic.
   ============================================================ */

%% persist_bridge_results(+Results)
%  Asserts a list of bridge_result(...) terms into narrative_ontology.
persist_bridge_results([]).
persist_bridge_results([bridge_result(Fact) | Rest]) :-
    persist_single(Fact),
    persist_bridge_results(Rest).

persist_single(constraint_metric(ID, Key, Val)) :-
    (   narrative_ontology:constraint_metric(ID, Key, _)
    ->  true
    ;   assertz(narrative_ontology:constraint_metric(ID, Key, Val))
    ).
persist_single(constraint_claim(ID, Type)) :-
    (   narrative_ontology:constraint_claim(ID, _)
    ->  true
    ;   assertz(narrative_ontology:constraint_claim(ID, Type))
    ).
persist_single(constraint_beneficiary(ID, Actor)) :-
    (   narrative_ontology:constraint_beneficiary(ID, _)
    ->  true
    ;   assertz(narrative_ontology:constraint_beneficiary(ID, Actor))
    ).
persist_single(constraint_victim(ID, Actor)) :-
    (   narrative_ontology:constraint_victim(ID, _)
    ->  true
    ;   assertz(narrative_ontology:constraint_victim(ID, Actor))
    ).
persist_single(has_sunset_clause(ID)) :-
    (   narrative_ontology:has_sunset_clause(ID)
    ->  true
    ;   assertz(narrative_ontology:has_sunset_clause(ID))
    ).
persist_single(omega_variable(OID, Type, Desc)) :-
    (   narrative_ontology:omega_variable(OID, _, _)
    ->  true
    ;   assertz(narrative_ontology:omega_variable(OID, Type, Desc))
    ).
persist_single(measurement(SyntheticID, IntervalID, Metric, Time, Value)) :-
    (   narrative_ontology:measurement(_, _, Metric, Time, _)
    ->  true
    ;   assertz(narrative_ontology:measurement(SyntheticID, IntervalID, Metric, Time, Value))
    ).

/* ============================================================
   LEGACY WRAPPER (backward compat)
   ============================================================ */

%% repair_interval(+IntervalID)
% Legacy wrapper: computes bridge + impute results, then persists.
% Callers (scenario_manager, test_harness) use this without changes.
repair_interval(IntervalID) :-
    (   narrative_ontology:interval(IntervalID, _, _)
    ->  format('~n[REPAIR] Auditing vectors for: ~w...~n', [IntervalID]),

        % 0. V3.4 DATA BRIDGE: Derive constraint_claim/2 and constraint_metric/3
        %    from indexed classifications and domain_priors when missing.
        bridge_v34_data(IntervalID, BridgeResults),
        persist_bridge_results(BridgeResults),

        % 1. PILLAR REMAPPING: Fix non-standard claims before verification
        % COMMENTED OUT FOR DEBUGGING
        % forall(narrative_ontology:constraint_claim(C, Type),
        %        (   signature_mapper:map_custom_pillar(C, Type, Standard),
        %            (Type \= Standard ->
        %             retract(narrative_ontology:constraint_claim(C, Type)),
        %             assertz(narrative_ontology:constraint_claim(C, Standard)),
        %             format('  [FIXED] Remapped ~w: ~w -> ~w~n', [C, Type, Standard])
        %            ; true)
        %        )),

        % 2. VECTOR REPAIR: Impute missing measurements
        impute_missing_metrics(IntervalID, ImputeResults),
        persist_bridge_results(ImputeResults)
    ;   format('~n[ERROR] Interval ~w not found.~n', [IntervalID]),
        false
    ).

%% get_default_metric(+MetricKey, -DefaultValue)
%  Fetches the default value for a given metric from the config module.
get_default_metric(MetricKey, DefaultValue) :-
    (MetricKey == extractiveness -> config:param(default_extractiveness, DefaultValue));
    (MetricKey == suppression_requirement -> config:param(default_suppression, DefaultValue));
    (MetricKey == theater_ratio -> config:param(default_theater, DefaultValue)).
