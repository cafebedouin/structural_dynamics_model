:- module(data_repair, [
    repair_interval/1
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
   REPAIR ORCHESTRATOR
   ============================================================ */

%% repair_interval(+IntervalID)
% Audits the measurement vectors for a given interval and repairs gaps
% using domain-specific epistemic priors.
repair_interval(IntervalID) :-
    (   narrative_ontology:interval(IntervalID, T0, Tn)
    ->  format('~n[REPAIR] Auditing vectors for: ~w...~n', [IntervalID]),

        % 0. V3.4 DATA BRIDGE: Derive constraint_claim/2 and constraint_metric/3
        %    from indexed classifications and domain_priors when missing.
        bridge_v34_data(IntervalID),

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
        forall(config:level(L), 
               ( repair_point(L, T0, IntervalID), 
                 repair_point(L, Tn, IntervalID) 
               ))
    ;   format('~n[ERROR] Interval ~w not found.~n', [IntervalID]),
        false
    ).

%% repair_point(+Level, +Time, +IntervalID)
% Iterates through the 4-component coercion vector at a specific time point.
repair_point(Level, Time, IntervalID) :-
    Components = [accessibility_collapse(Level), stakes_inflation(Level), 
                  suppression(Level), resistance(Level)],
    forall(member(Metric, Components), 
           ensure_metric_exists(Metric, Time, IntervalID)).

%% ensure_metric_exists(+Metric, +Time, +IntervalID)
% Core v3.2 Imputation Logic:
% 1. Checks for existing data.
% 2. Resolves prior based on domain type.
% 3. Flags novelty if the domain is unmapped.
ensure_metric_exists(Metric, Time, IntervalID) :-
    % Look directly into the ontology for existing measurement
    narrative_ontology:measurement(_, _, Metric, Time, _)
    ->  true
    ;   (   % NEW: Fetch prior value instead of hard-coded 0.5
            domain_priors:get_prior(IntervalID, Metric, Value),
            
            % NEW: Surface novelty alert to the LLM/User
            (domain_priors:is_known_domain(IntervalID) -> true ; domain_priors:flag_novelty(IntervalID)),
            
            gensym(repair_m_, SyntheticID),
            % Assert the synthetic fact into the global ontology
            assertz(narrative_ontology:measurement(SyntheticID, IntervalID, Metric, Time, Value)),
            format('  [FIXED] Imputed ~w for ~w at T=~w~n', [Value, Metric, Time])
    ).

/* ============================================================
   V3.4 DATA BRIDGE

   v3.4 testsets define constraint_classification/3 and
   domain_priors but NOT constraint_claim/2 or
   constraint_metric/3. This bridge auto-derives the missing
   ontology facts so the audit framework can find them.
   ============================================================ */

%% bridge_v34_data(+IntervalID)
%  Master bridge: derives constraint_claim, constraint_metric,
%  and omega_variable facts from v3.4 indexed data.
bridge_v34_data(IntervalID) :-
    % Metrics must be bridged FIRST because bridge_constraint_claim
    % uses drl_core:dr_type/2 which queries constraint_metric/3.
    bridge_domain_metrics(IntervalID),
    bridge_beneficiary_victim(IntervalID),
    bridge_scaffold_markers(IntervalID),
    bridge_constraint_claim(IntervalID),
    bridge_omega_variables(IntervalID).

%% bridge_constraint_claim(+IntervalID)
%  If no constraint_claim/2 exists for IntervalID, derive one using:
%  PRIORITY 1: Declared analytical perspective classification from testset data
%  PRIORITY 2: Computed classification from dr_type (now using raw suppression)
%  PRIORITY 3: First declared classification from any context
bridge_constraint_claim(IntervalID) :-
    (   narrative_ontology:constraint_claim(IntervalID, _)
    ->  true  % Already has a claim
    ;   % PRIORITY 1: Use declared analytical perspective classification
        (   constraint_indexing:default_context(AnalyticalCtx),
            constraint_indexing:constraint_classification(IntervalID, DeclaredType, AnalyticalCtx)
        ->  assertz(narrative_ontology:constraint_claim(IntervalID, DeclaredType)),
            format('  [BRIDGE] Derived constraint_claim(~w, ~w) from declared analytical classification~n',
                   [IntervalID, DeclaredType])
        ;   % PRIORITY 2: Compute from dr_type (now fixed with raw suppression)
            (   catch(drl_core:dr_type(IntervalID, ComputedType), _, fail),
                ComputedType \= unknown
            ->  assertz(narrative_ontology:constraint_claim(IntervalID, ComputedType)),
                format('  [BRIDGE] Derived constraint_claim(~w, ~w) from computed analytical classification~n',
                       [IntervalID, ComputedType])
            ;   % PRIORITY 3: Use first declared classification
                (   constraint_indexing:constraint_classification(IntervalID, FallbackType, _)
                ->  assertz(narrative_ontology:constraint_claim(IntervalID, FallbackType)),
                    format('  [BRIDGE] Derived constraint_claim(~w, ~w) from first indexed classification~n',
                           [IntervalID, FallbackType])
                ;   true
                )
            )
        )
    ).

%% bridge_domain_metrics(+IntervalID)
%  Maps domain_priors predicates to narrative_ontology:constraint_metric/3
%  so that constraint_bridge:constraint_status/3 can find them.
%
%  NOTE: PriorPred (1st arg to bridge_single_metric) is the domain_priors
%  PREDICATE name. MetricKey (2nd arg) is the constraint_metric KEY name.
%  These differ for extractiveness (predicate=base_extractiveness, key=extractiveness)
%  and suppression (predicate=suppression_score, key=suppression_requirement).
bridge_domain_metrics(IntervalID) :-
    config:param(extractiveness_metric_name, ExtMetricKey),
    bridge_single_metric(IntervalID, base_extractiveness, ExtMetricKey),
    config:param(suppression_metric_name, SuppMetricKey),
    bridge_single_metric(IntervalID, suppression_score, SuppMetricKey),
    config:param(theater_metric_name, TheaterMetricKey),
    bridge_single_metric(IntervalID, TheaterMetricKey, TheaterMetricKey).

bridge_single_metric(IntervalID, PriorPred, MetricKey) :-
    (   narrative_ontology:constraint_metric(IntervalID, MetricKey, _)
    ->  true  % Already exists
    ;   (   Goal =.. [PriorPred, IntervalID, Value],
            catch(domain_priors:call(Goal), _, fail)
        ->  assertz(narrative_ontology:constraint_metric(IntervalID, MetricKey, Value)),
            format('  [BRIDGE] Derived metric ~w = ~w for ~w from domain_priors~n', [MetricKey, Value, IntervalID])
        ;   % NEW FALLBACK: If domain_prior is missing, get from config default
            (   get_default_metric(MetricKey, DefaultValue)
            ->  assertz(narrative_ontology:constraint_metric(IntervalID, MetricKey, DefaultValue)),
                format('  [BRIDGE] Derived metric ~w = ~w for ~w from config default~n', [MetricKey, DefaultValue, IntervalID])
            ;   true % No domain prior or default config available
            )
        )
    ).

%% get_default_metric(+MetricKey, -DefaultValue)
%  Fetches the default value for a given metric from the config module.
get_default_metric(MetricKey, DefaultValue) :-
    (MetricKey == extractiveness -> config:param(default_extractiveness, DefaultValue));
    (MetricKey == suppression_requirement -> config:param(default_suppression, DefaultValue));
    (MetricKey == theater_ratio -> config:param(default_theater, DefaultValue)).


%% bridge_omega_variables(+IntervalID)
%  v3.4 testsets define omega_variable/5 in their own module namespace.
%  This bridges them into narrative_ontology:omega_variable/3.
bridge_omega_variables(IntervalID) :-
    (   current_module(IntervalID),
        predicate_property(IntervalID:omega_variable(_,_,_,_,_), defined)
    ->  forall(
            (   catch(IntervalID:omega_variable(OID, Question, _Measurement, _Resolution, _Conf), _, fail),
                \+ narrative_ontology:omega_variable(OID, _, _)
            ),
            (   assertz(narrative_ontology:omega_variable(OID, empirical, Question)),
                format('  [BRIDGE] Imported omega ~w from module ~w~n', [OID, IntervalID])
            ))
    ;   true  % Module not loaded or no omega_variable/5
    ),
    % Also check for omega_variable/3 in the testset module
    (   current_module(IntervalID),
        predicate_property(IntervalID:omega_variable(_,_,_), defined)
    ->  forall(
            (   catch(IntervalID:omega_variable(OID3, Type3, Desc3), _, fail),
                \+ narrative_ontology:omega_variable(OID3, _, _)
            ),
            (   assertz(narrative_ontology:omega_variable(OID3, Type3, Desc3)),
                format('  [BRIDGE] Imported omega ~w from module ~w~n', [OID3, IntervalID])
            ))
    ;   true
    ).

/* ============================================================
   V3.4 BENEFICIARY/VICTIM BRIDGE

   Tangled Rope detection via drl_core:is_tangled_rope/3
   requires has_coordination_function/1 (needs constraint_beneficiary/2)
   and has_asymmetric_extraction/1 (needs constraint_victim/2).

   Most v3.4 testsets don't declare these. When a constraint has
   high extraction (> 0.46) AND high suppression (> 0.40), it
   structurally implies both a beneficiary and a victim exist.
   This bridge asserts synthetic beneficiary/victim facts so the
   tangled rope gate can fire on metric-qualifying constraints.
   ============================================================ */

%% bridge_beneficiary_victim(+IntervalID)
%  Auto-derives constraint_beneficiary/2 and constraint_victim/2
%  for high-extraction, high-suppression constraints that lack them.
bridge_beneficiary_victim(IntervalID) :-
    % Only act if both facts are missing
    (   narrative_ontology:constraint_beneficiary(IntervalID, _)
    ->  true  % Already has beneficiary
    ;   (   narrative_ontology:constraint_metric(IntervalID, extractiveness, E),
            E > 0.46,
            narrative_ontology:constraint_metric(IntervalID, suppression_requirement, S),
            S > 0.40
        ->  assertz(narrative_ontology:constraint_beneficiary(IntervalID, inferred_institutional)),
            format('  [BRIDGE] Derived constraint_beneficiary(~w, inferred_institutional) from metrics (E=~2f, S=~2f)~n',
                   [IntervalID, E, S])
        ;   true
        )
    ),
    (   narrative_ontology:constraint_victim(IntervalID, _)
    ->  true  % Already has victim
    ;   (   narrative_ontology:constraint_metric(IntervalID, extractiveness, E2),
            E2 > 0.46,
            narrative_ontology:constraint_metric(IntervalID, suppression_requirement, S2),
            S2 > 0.40
        ->  assertz(narrative_ontology:constraint_victim(IntervalID, inferred_subject)),
            format('  [BRIDGE] Derived constraint_victim(~w, inferred_subject) from metrics (E=~2f, S=~2f)~n',
                   [IntervalID, E2, S2])
        ;   true
        )
    ).

/* ============================================================
   V3.4 SCAFFOLD MARKER BRIDGE

   Scaffold classification requires has_coordination_function/1
   and has_sunset_clause/1, which are structural PROPERTIES of
   the constraint (not classifications). When a testset declares
   a constraint as scaffold in any context, that declaration
   provides evidence that these properties hold. The classification
   gate still evaluates metrics (extraction, suppression, time
   horizon, theater ratio) independently.

   Without this bridge, scaffolds fail because:
   1. has_sunset_clause/1 defaults to fail in narrative_ontology
   2. has_coordination_function/1 requires constraint_beneficiary,
      which is only bridged for high-extraction constraints (>0.46),
      but scaffolds have low extraction (<=0.30) by definition.
   ============================================================ */

%% bridge_scaffold_markers(+IntervalID)
%  Derives has_sunset_clause and has_coordination_function from
%  scaffold declarations. These are structural properties, not
%  classification passthrough.
bridge_scaffold_markers(IntervalID) :-
    % Check if any context declares this constraint as scaffold
    (   constraint_indexing:constraint_classification(IntervalID, scaffold, _)
    ->  % Bridge has_sunset_clause (scaffold implies sunset by definition)
        (   narrative_ontology:has_sunset_clause(IntervalID)
        ->  true
        ;   assertz(narrative_ontology:has_sunset_clause(IntervalID)),
            format('  [BRIDGE] Derived has_sunset_clause(~w) from scaffold declaration~n',
                   [IntervalID])
        ),
        % Bridge has_coordination_function via constraint_beneficiary
        (   narrative_ontology:constraint_beneficiary(IntervalID, _)
        ->  true
        ;   assertz(narrative_ontology:constraint_beneficiary(IntervalID, coordinated_group)),
            format('  [BRIDGE] Derived constraint_beneficiary(~w, coordinated_group) from scaffold declaration~n',
                   [IntervalID])
        )
    ;   true  % Not declared as scaffold, nothing to bridge
    ).
