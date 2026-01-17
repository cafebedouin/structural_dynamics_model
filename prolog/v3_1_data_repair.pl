:- module(v3_1_data_repair, [
    repair_interval/1
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(domain_priors). % NEW: Hook into the Epistemic Prior Library

% Ensure we can add facts to the ontology's measurement predicate
:- dynamic narrative_ontology:measurement/5.

/* ============================================================
   REPAIR ORCHESTRATOR
   ============================================================ */

%% repair_interval(+IntervalID)
% Audits the measurement vectors for a given interval and repairs gaps
% using domain-specific epistemic priors.
repair_interval(IntervalID) :-
    (   narrative_ontology:interval(IntervalID, T0, Tn)
    ->  format('~n[REPAIR] Auditing vectors for: ~w...~n', [IntervalID]),
        % Audit both endpoints for all analytical levels
        forall(v3_1_config:level(L), 
               ( repair_point(L, T0, IntervalID), 
                 repair_point(L, Tn, IntervalID) 
               )),
        true
    ;   format('~n[ERROR] Interval ~w not found in database.~n', [IntervalID]),
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
