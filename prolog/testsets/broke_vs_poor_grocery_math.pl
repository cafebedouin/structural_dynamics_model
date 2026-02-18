% ============================================================================
% CONSTRAINT STORY: broke_vs_poor_grocery_math
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_broke_vs_poor_grocery_math, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: broke_vs_poor_grocery_math
 * human_readable: The Cognitive Load of Poverty (Grocery Math)
 * domain: economic/social
 * * SUMMARY:
 * This constraint models the cognitive load imposed by poverty, exemplified by "grocery math"—the mandatory mental tallying of every item's cost to avoid exceeding available funds at checkout. This distinguishes being "poor" (a persistent state of scarcity) from being "broke" (a temporary lack of funds). The constraint is the mental burden itself, enforced by the hard limit of one's budget.
 * * KEY AGENTS:
 * - The Poor Individual: Subject (Powerless), experiences the constraint as a Snare.
 * - The Broke Individual: Agent with temporary liquidity issues (Moderate), experiences the constraint as a Rope.
 * - The Social Welfare System: Observer (Institutional), views the phenomenon as a systemic, unchangeable feature of the landscape (Mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(broke_vs_poor_grocery_math, 0.80). % High cognitive/temporal extraction. Snare extraction >= 0.46
domain_priors:suppression_score(broke_vs_poor_grocery_math, 0.60).   % The alternative ("not counting") is suppressed by the immediate risk of insufficient funds.
domain_priors:theater_ratio(broke_vs_poor_grocery_math, 0.10).       % This is a real, non-performative activity. Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, extractiveness, 0.80).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as a necessary consequence of scarcity, enforced by the checkout.
narrative_ontology:constraint_claim(broke_vs_poor_grocery_math, tangled_rope).
narrative_ontology:human_readable(broke_vs_poor_grocery_math, "The Cognitive Load of Poverty (Grocery Math)").
narrative_ontology:topic_domain(broke_vs_poor_grocery_math, "economic/social").

% Binary flags
domain_priors:requires_active_enforcement(broke_vs_poor_grocery_math). % Enforced by the point-of-sale system.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(broke_vs_poor_grocery_math, economic_system_beneficiaries).
narrative_ontology:constraint_victim(broke_vs_poor_grocery_math, individuals_in_poverty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE POOR INDIVIDUAL (SNARE)
% For the person in poverty, the constant mental calculation is a Snare. It
% extracts cognitive bandwidth, time, and emotional energy, trapping them in a
% cycle of survival-focused thinking.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BROKE INDIVIDUAL (ROPE)
% For a person who is temporarily "broke" but has reliable income expected,
% the need for careful budgeting is a temporary coordination tool (Rope).
% They coordinate their current spending with future income. The cognitive load
% is lower because an exit is visible and guaranteed.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SOCIAL WELFARE SYSTEM (MOUNTAIN)
% From an institutional perspective (e.g., a social worker or welfare agency),
% the phenomenon of "grocery math" is a symptom of the larger, seemingly
% intractable problem of poverty. It appears as a Mountain—a persistent,
% systemic feature of the economic landscape that they must work around but
% cannot easily change.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(broke_vs_poor_grocery_math_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between all three key agents.
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypeModerate, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeModerate == rope,
    TypeInstitutional == mountain,
    TypePowerless \= TypeModerate,
    TypeModerate \= TypeInstitutional.

test(threshold_validation_snare) :-
    % Verify the base extractiveness meets the Snare threshold.
    narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, extractiveness, E),
    E >= 0.46.

:- end_tests(broke_vs_poor_grocery_math_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the perspectival gap between poverty and temporary
 * financial hardship. The base extractiveness (0.80) is high because the primary
 * extraction is not monetary but cognitive and temporal—the constant, draining
 * mental effort required for basic survival tasks.
 * - The 'Poor' individual sees a Snare because there is no clear exit; the cognitive load is a permanent feature of their reality.
 * - The 'Broke' individual sees a Rope because their situation is temporary; budgeting is a tool to coordinate with near-future income.
 * - The 'Institutional' actor sees a Mountain because from their systemic viewpoint, this behavior is an emergent property of a vast, slow-moving economic system that is resistant to change.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is not a Tangled Rope because it lacks a genuine, designed coordination function for the victims. The "grocery math" is an emergent coping mechanism, not a system designed for coordination that also happens to be extractive. It is a pure Snare from the victim's perspective, a direct consequence of systemic economic pressures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_broke_vs_poor_grocery_math,
    "Is the 'grocery math' requirement a functional necessity of limited individual funds (Mountain) or an extractive systemic failure of the market (Snare)?",
    "Audit of grocery pricing volatility vs. base survival wages, and access to affordable, nutritious food for low-income populations.",
    "If necessity, it's a personal Mountain. If systemic failure, it's a societal Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(broke_vs_poor_grocery_math, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint represents a persistent state of poverty. The cognitive
% extraction has remained consistently high over the observed interval, and
% the activity is not performative.
%
% Theater ratio over time (consistently low):
narrative_ontology:measurement(bvp_tr_t0, broke_vs_poor_grocery_math, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bvp_tr_t5, broke_vs_poor_grocery_math, theater_ratio, 5, 0.10).
narrative_ontology:measurement(bvp_tr_t10, broke_vs_poor_grocery_math, theater_ratio, 10, 0.10).

% Extraction over time (consistently high):
narrative_ontology:measurement(bvp_ex_t0, broke_vs_poor_grocery_math, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(bvp_ex_t5, broke_vs_poor_grocery_math, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(bvp_ex_t10, broke_vs_poor_grocery_math, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is an emergent property of a larger system, not a designed
% coordination mechanism itself. Therefore, Boltzmann data is not applicable.
% narrative_ontology:coordination_type(broke_vs_poor_grocery_math, ...).

% This constraint is affected by broader economic policies.
narrative_ontology:affects_constraint(minimum_wage_policy, broke_vs_poor_grocery_math).
narrative_ontology:affects_constraint(food_subsidy_programs, broke_vs_poor_grocery_math).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (broke_vs_poor_grocery_math)
% ============================================================================

% --- Analytical perspective classification ---
% chi = 0.8 * 1.15 (analytical) * 1.2 (global) = 1.104
% Classification: snare
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
