:- module(data_repair, [
    repair_interval/1,           % Legacy: computes + persists (backward compat)
    repair_interval/2,           % Pure: repair_interval(+ID, -Results)
    bridge_v34_data/2,           % Pure: bridge_v34_data(+ID, -BridgeResults)
    impute_missing_metrics/2,    % Pure: impute_missing_metrics(+ID, -ImputeResults)
    persist_bridge_results/1,    % Asserts bridge_result list into narrative_ontology
    grid_provenance/2            % Per-slot provenance census of the leveled grid (OQ-93)
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
% FABRICATION BAN (OQ-92 Rulings block, settled by the OQ-90 HALT; enforcement
% site per the step-3 preregistration): narrative_ontology:stakeholder_gain_flow/2
% and fixing_cost_class/2 are NEVER synthesized here or anywhere — no repair,
% bridge, or imputation clause may infer the receipt surface from metrics.
% Inferring capture from extraction metrics is the has_computed_capturer
% counterfeit re-entering through a side door (a capture-adjacent fact
% synthesized from the metrics it feeds back into). Absent stays absent,
% fail-closed. The beneficiary bridge below predates the ban and is OQ-93
% shim-family (the grid imputation arm itself was retired 2026-06-11,
% OQ-93 ruling (b)); do not extend it.
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

% TOMBSTONE — bridge_omega_variables_pure/3 RETIRED (OQ-111, 2026-06-18).
%  This predicate tried to import a testset's authored omega_variable facts by
%  querying module `IntervalID` (the bare interval id). But every testset
%  declares its facts in module `constraint_<id>` (e.g. interval
%  `border_control_legitimacy__freedom_of_movement_primary` lives in module
%  `constraint_border_control_legitimacy__freedom_of_movement_primary`), so the
%  `current_module(IntervalID)` guards ALWAYS missed and the predicate returned
%  the input accumulator unchanged — it imported ZERO omegas on every report run
%  (Build Discipline Pattern 6: success-shaped absence). Wrong-module premise =
%  OQ-99's twin; OQ-99 fixed the same bug in report_generator.pl via
%  atom_concat(constraint_, Id, Module).
%
%  NOT fixed — RETIRED. Authored omegas already reach reports without this
%  bridge: testsets author `narrative_ontology:omega_variable/3` (3-arity,
%  qualified — global) directly (report_generator.pl:709 enumerates them), and
%  report_generator.pl:776-794 renders each matching 5-arity authored protocol.
%  The bridge's only genuine purpose was v3.4-LEGACY *unpaired* testsets (5-arity
%  omega authored in their own module with NO narrative_ontology 3-arity sibling).
%  The live corpus is 100% paired; the only unpaired inputs live in
%  prolog/archives/datasets/*, which the operator ruled OUT OF SCOPE
%  (no backward-compatibility, 2026-06-18). Removal is behavior-preserving
%  (zero-diff) precisely because the predicate already returned []. If
%  archive v3.4-unpaired omega rendering is ever wanted again, see
%  docs/design/design_gaps.md and re-key on `constraint_<id>` per OQ-99.

/* ============================================================
   EXPORTED PURE API
   ============================================================ */

%% bridge_v34_data(+IntervalID, -Results)
%  Pure bridge pipeline: derives constraint_claim, constraint_metric,
%  beneficiary/victim, and scaffold markers from v3.4 indexed data. Returns
%  bridge_result(...) terms without asserting. (Omega bridging was removed —
%  OQ-111, 2026-06-18; see tombstone above.)
%  Bridge ordering preserved: metrics -> beneficiary/victim -> scaffold -> claim.
bridge_v34_data(IntervalID, Results) :-
    bridge_domain_metrics_pure(IntervalID, [], Acc1),
    bridge_beneficiary_victim_pure(IntervalID, Acc1, Acc2),
    bridge_scaffold_markers_pure(IntervalID, Acc2, Acc3),
    bridge_constraint_claim_pure(IntervalID, Acc3, Results).

%% impute_missing_metrics(+IntervalID, -Results)
%  OQ-93 ruling (b) (2026-06-10; imputation arm RETIRED 2026-06-11):
%  the grid is authored-or-absent, permanently. NO facts are ever
%  manufactured; the absent count is reported loud so the suite stays green
%  on an EXPECTED-AND-WITNESSED absence, never on manufactured filler. The
%  prior-flavored manufacture arm (repair_m_* facts from domain_priors
%  category profiles) is gone with the grid_shim_enabled flag;
%  source_class/2 keeps its imputed bucket for archive replays.
impute_missing_metrics(IntervalID, []) :-
    narrative_ontology:interval(IntervalID, T0, Tn),
    findall(Metric-Time,
        (   config:level(L),
            member(Time, [T0, Tn]),
            member(Metric, [accessibility_collapse(L), stakes_inflation(L),
                           suppression(L), resistance(L)]),
            \+ narrative_ontology:measurement(_, _, Metric, Time, _)
        ),
        AbsentSlots),
    length(AbsentSlots, NAbsent),
    format('  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): ~w/32 grid points absent — expected-and-witnessed~n',
           [NAbsent]).

/* ============================================================
   GRID PROVENANCE (OQ-93)
   ============================================================
   The leveled measurement grid (4 metrics x config:level x {T0,Tn})
   WAS unauthorable under the pre-OQ-93 generation schema (empty vocabulary
   intersection — census: audits/2026-06-09_imputation_shim_census/); since
   OQ-93 RESOLVED 2026-06-11 it is opt-in by story focus (authored-or-absent).
   Three distinct sources can populate a slot, and they must never
   collapse to one bucket at a read site:
     authored  — any source ID other than the two synthetic classes
     injected  — m_gen (scenario_manager's hardcoded 0.5 anchors)
     imputed   — repair_m_* (domain-prior imputation, this module)
   ============================================================ */

%% grid_provenance(+IntervalID, -prov(Authored, Injected, Imputed, Absent, Total))
%  Counts every grid slot by the source class of the fact occupying it
%  (first fact per slot, mirroring persist_single's occupancy guard).
%  Absent > 0 means no fact occupies the slot (authored-or-absent regime).
%  INTERVAL-SCOPED (2026-06-11, flip promotion): the measurement read was
%  interval-ANONYMOUS (`measurement(SrcID, _, Metric, Time, _)`) — the same
%  cross-constraint leakage class as the OQ-93 build-unit-1 coercion_vector
%  fix, latent while at most ONE loaded interval ever had grid facts
%  (load_and_run clears the KB). First corpus-wide pipeline with 10 authored
%  grids: 56/58 constraints read other stories' grid points as their own
%  (witnessed: audits/2026-06-11_oq93_grid_migration/flip_promotion_witness.txt).
grid_provenance(IntervalID, prov(A, I, P, Abs, Total)) :-
    narrative_ontology:interval(IntervalID, T0, Tn),
    findall(Class,
        (   config:level(L),
            member(Time, [T0, Tn]),
            member(Metric, [accessibility_collapse(L), stakes_inflation(L),
                           suppression(L), resistance(L)]),
            (   narrative_ontology:measurement(SrcID, IntervalID, Metric, Time, _)
            ->  source_class(SrcID, Class)
            ;   Class = absent
            )
        ),
        Classes),
    length(Classes, Total),
    aggregate_all(count, member(authored, Classes), A),
    aggregate_all(count, member(injected, Classes), I),
    aggregate_all(count, member(imputed,  Classes), P),
    aggregate_all(count, member(absent,   Classes), Abs).

source_class(m_gen, injected) :- !.
source_class(ID, imputed) :-
    atom(ID), sub_atom(ID, 0, _, _, repair_m_), !.
source_class(_, authored).

%% stray_injected_count(+IntervalID, -S)
%  m_gen facts for this interval that sit on NO grid slot — the
%  injector uses hardcoded t=[0,10], so any interval with Tn =\= 10
%  strands 4 fabricated 0.5s mid-timeline (they feed coercion_gradient
%  and unbound-metric time-point collectors).
stray_injected_count(IntervalID, S) :-
    narrative_ontology:interval(IntervalID, T0, Tn),
    aggregate_all(count,
        (   narrative_ontology:measurement(m_gen, IntervalID, _, T, _),
            T \= T0, T \= Tn
        ),
        S).

%% report_grid_provenance(+IntervalID)
%  Printed by the legacy wrapper AFTER persistence, so every count is
%  read back from the fact store (bucket truth is witnessed, not
%  inferred from the impute result list).
report_grid_provenance(IntervalID) :-
    grid_provenance(IntervalID, prov(A, I, P, Abs, Total)),
    format('  [PROVENANCE] grid ~w = authored ~w + injected-0.5 ~w (m_gen) + imputed-from-priors ~w (repair_m_*)~n',
           [Total, A, I, P]),
    format('  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md~n'),
    (   Abs > 0
    ->  format('  [WARN] ~w grid slots still absent after repair~n', [Abs])
    ;   true
    ),
    (   stray_injected_count(IntervalID, S), S > 0
    ->  format('  [WARN] ~w stray injected 0.5 anchors off-grid (m_gen at hardcoded t=[0,10], interval endpoints differ)~n', [S])
    ;   true
    ).

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
% persist_single(omega_variable(...)) RETIRED with bridge_omega_variables_pure/3
% (OQ-111, 2026-06-18): that predicate was the only producer of
% bridge_result(omega_variable(...)) terms, so this dispatch clause is now
% unreachable. Authored narrative_ontology:omega_variable/3 facts are asserted
% at testset load, never through persist_single — they are unaffected.
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
        persist_bridge_results(ImputeResults),

        % 3. PROVENANCE LINE (OQ-93): counted from the store post-persist
        report_grid_provenance(IntervalID)
    ;   format('~n[ERROR] Interval ~w not found.~n', [IntervalID]),
        false
    ).

%% get_default_metric(+MetricKey, -DefaultValue)
%  Fetches the default value for a given metric from the config module.
get_default_metric(MetricKey, DefaultValue) :-
    (MetricKey == extractiveness -> config:param(default_extractiveness, DefaultValue));
    (MetricKey == suppression_requirement -> config:param(default_suppression, DefaultValue));
    (MetricKey == theater_ratio -> config:param(default_theater, DefaultValue)).
