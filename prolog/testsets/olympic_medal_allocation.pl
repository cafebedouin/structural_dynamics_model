% ============================================================================
% CONSTRAINT STORY: olympic_medal_allocation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_olympic_medal_allocation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: olympic_medal_allocation
 *   human_readable: Olympic Medal Allocation System
 *   domain: social
 *
 * SUMMARY:
 *   The Olympic medal allocation system is designed to fairly distribute medals
 *   among participating nations based on athletic performance. However,
 *   factors like historical advantages, resource disparities, and strategic
 *   gaming can lead to uneven distribution, creating a tangled rope scenario.
 *
 * KEY AGENTS (by structural relationship):
 *   - Smaller nations: Primary target (powerless/trapped) — face systemic disadvantages in medal acquisition.
 *   - Dominant sporting nations: Primary beneficiary (institutional/arbitrage) — benefit from existing infrastructure, resources, and historical advantages.
 *   - IOC (International Olympic Committee): Secondary actor (institutional/constrained) — tasked with maintaining fairness but constrained by existing power dynamics.
 *   - Analytical observers: Analytical observer (analytical/analytical) — seek to understand the fairness and effectiveness of the allocation system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(olympic_medal_allocation, 0.35).  % Moderate extraction: some nations benefit disproportionately.
domain_priors:suppression_score(olympic_medal_allocation, 0.50).    % Suppression: Resource disparities limit smaller nations' ability to compete.
domain_priors:theater_ratio(olympic_medal_allocation, 0.20).        % Low theater: Primarily driven by athletic performance, not performative aspects.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(olympic_medal_allocation, extractiveness, 0.35).
narrative_ontology:constraint_metric(olympic_medal_allocation, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(olympic_medal_allocation, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(olympic_medal_allocation, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(olympic_medal_allocation). % Requires enforcement of rules to maintain perceived fairness.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(olympic_medal_allocation, dominant_sporting_nations).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(olympic_medal_allocation, smaller_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: SMALLER NATIONS (SNARE)
constraint_indexing:constraint_classification(olympic_medal_allocation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMINANT SPORTING NATIONS (ROPE)
constraint_indexing:constraint_classification(olympic_medal_allocation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
constraint_indexing:constraint_classification(olympic_medal_allocation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% INTER-INSTITUTIONAL PERSPECTIVE: IOC (ROPE but with Constrained Exit)
% The IOC's perspective: They benefit from the overall system (Rope),
% but they are constrained by the existing power dynamics and political
% considerations. They can't unilaterally change the rules without
% risking the system's stability.
constraint_indexing:constraint_classification(olympic_medal_allocation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(olympic_medal_allocation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(olympic_medal_allocation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(olympic_medal_allocation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(olympic_medal_allocation, ExtMetricName, E),
    E >= 0.30. % Tangled Rope requires E >= 0.30.

:- end_tests(olympic_medal_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Olympic medal allocation system, while intended for fair competition,
 *   exhibits characteristics of a tangled rope. Dominant sporting nations
 *   possess significant advantages due to infrastructure, resources, and
 *   historical successes, leading to a skewed distribution of medals.
 *   Smaller nations face systemic disadvantages.
 *
 * PERSPECTIVAL GAP:
 *   Smaller nations perceive the system as a snare due to their limited
 *   opportunities to compete effectively against wealthier, more established
 *   sporting powers. Dominant nations view it as a rope, as it largely
 *   reflects their investment and success, while the analytical observer sees the tangled rope dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant sporting nations benefit from the system, as their established
 *   infrastructure and resources allow them to consistently win more medals.
 *   Smaller nations bear the cost, as they lack the resources and infrastructure
 *   to compete on an equal footing. The IOC aims to manage the system fairly but
 *   is constrained by existing power dynamics and political considerations.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 * The IOC, while technically an institution designed to foster global
 * sporting equity, is heavily influenced by the dominant sporting nations
 * due to their economic and political power. Thus, changes to the medal
 * allocation system which might benefit smaller nations are difficult to
 * enact due to resistance from those dominant players. The IOC's exit options
 * are therefore "constrained," whereas the dominant nations have "arbitrage"
 * (they can leverage their power). This is reflected in the differing
 * classifications based on agent power.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling coordination as pure extraction
 *   by acknowledging the intended function of the Olympic Games (promoting
 *   international cooperation and athletic achievement) while recognizing the
 *   asymmetric extraction occurring due to existing power imbalances.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_olympic_fairness,
    'To what extent can the Olympic system truly level the playing field for smaller nations?',
    'Longitudinal analysis of resource allocation, rule changes, and medal distribution.',
    'If true, system is closer to a rope; if false, closer to a snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(olympic_medal_allocation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(olympic_tr_t0, olympic_medal_allocation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(olympic_tr_t5, olympic_medal_allocation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(olympic_tr_t10, olympic_medal_allocation, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(olympic_ex_t0, olympic_medal_allocation, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(olympic_ex_t5, olympic_medal_allocation, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(olympic_ex_t10, olympic_medal_allocation, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(olympic_medal_allocation, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(olympic_medal_allocation, 0.25).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(olympic_medal_allocation, fair_resource_distribution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(olympic_medal_allocation, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */