% ============================================================================
% CONSTRAINT STORY: cognitive_energy_budget
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_energy_budget, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cognitive_energy_budget
 *   human_readable: The Attention Exhaustion Trap
 *   domain: cognitive/technological/social
 *
 * SUMMARY:
 *   The attention exhaustion trap describes the way modern society and the
 *   internet have overloaded our limited daily cognitive energy. Increased
 *   reliance on technology and constant information streams extract from
 *   individual's limited cognitive bandwidth, causing exhaustion and
 *   diminished decision-making capabilities. This dynamic benefits certain
 *   platforms and entities at the expense of individual well-being and
 *   collective cognitive function.
 *
 * KEY AGENTS:
 *   - Individual Cognitive Function: Primary victim (powerless/trapped) - experiences constant cognitive overload.
 *   - Attention Economy Platforms: Primary beneficiary (institutional/arbitrage) - profits from aggregated user attention.
 *   - Informed Decision Making: Secondary victim (moderate/constrained) - ability to make informed choices diminishes with cognitive fatigue.
 *   - Legacy Educational Systems: Institutional actors perpetuating outdated, ineffective learning paradigms (institutional/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_energy_budget, 0.55).
domain_priors:suppression_score(cognitive_energy_budget, 0.6).
domain_priors:theater_ratio(cognitive_energy_budget, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_energy_budget, extractiveness, 0.55).
narrative_ontology:constraint_metric(cognitive_energy_budget, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(cognitive_energy_budget, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_energy_budget, tangled_rope).
narrative_ontology:human_readable(cognitive_energy_budget, "The Attention Exhaustion Trap").
narrative_ontology:topic_domain(cognitive_energy_budget, "cognitive/technological/social").

domain_priors:requires_active_enforcement(cognitive_energy_budget).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_energy_budget, attention_economy_platforms).
narrative_ontology:constraint_beneficiary(cognitive_energy_budget, information_overload_profiteers).
narrative_ontology:constraint_victim(cognitive_energy_budget, individual_cognitive_function).
narrative_ontology:constraint_victim(cognitive_energy_budget, informed_decision_making).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Exhausted Individual (Snare). Faced with relentless demands on their attention, individuals often feel trapped, unable to escape the constant barrage of notifications, information, and stimuli. This leads to cognitive overload, reduced focus, and diminished decision-making capacity. High extractiveness and suppression. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.93
constraint_indexing:constraint_classification(cognitive_energy_budget, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Attention Economy Platforms (Rope). These platforms benefit from the aggregate attention of users, creating a collective coordination effect. The extraction from individuals is low compared to the overall benefit derived from the platform's functionality and user base. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.066
constraint_indexing:constraint_classification(cognitive_energy_budget, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: The Analytical Observer (Tangled Rope). From a high-level perspective, the attention economy exhibits both coordination (connecting people and information) and extraction (overwhelming individuals and eroding cognitive function). d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76
constraint_indexing:constraint_classification(cognitive_energy_budget, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 4: Legacy Educational Systems (Piton). Traditional education struggles to adapt to the digital age, often perpetuating outdated methods that fail to equip individuals with the skills needed to navigate information overload and maintain cognitive well-being. High theater ratio; constrained exit. d=0.75, f(d)=1.10, σ=1.0 -> χ=0.605. Although not as extractive as snare, the lack of benefit from its intended function leads it to be considered piton.
constraint_indexing:constraint_classification(cognitive_energy_budget, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_energy_budget_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_energy_budget, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_energy_budget, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cognitive_energy_budget, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_energy_budget, TR),
    TR >= 0.70.

:- end_tests(cognitive_energy_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is rated at 0.55 because individuals are significantly drained by modern information overload. Suppression at 0.60 because individuals are not completely trapped, but have difficulty escaping the bombardment of information due to societal and professional pressure.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap comes from the beneficiaries benefiting from users spending more time on the application while the end user suffers from lowered cognitive ability and increased cognitive strain. The analytical observer sees both points and must balance the benefit to the platforms with the harm done to the user.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual Cognitive Function: Victim + trapped → d=0.95, f(d)=1.42. Attention Economy Platforms: Beneficiary + arbitrage → d=0.05, f(d)=-0.12. Analytical Observer: Analytical → d=0.72, f(d)=1.15. Legacy Educational Systems: d=0.75 because though not necessarily trapped, these actors face high risks and opportunity costs from altering their long set-in-stone practices.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_variability,
    'How much does individual cognitive capacity vary?',
    'Neurobiological studies; cognitive testing across demographics',
    'Impact: High individual variability would mean different people experience different levels of extractiveness. Low variability means a more uniform snare across individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_variability, empirical, 'Degree of variation in cognitive energy budgets.').

omega_variable(
    platform_governance_effectiveness,
    'How effective are platform governance interventions for reducing extractive behaviors?',
    'A/B testing of different platform policies; longitudinal studies of user behavior',
    'Impact: If interventions are highly effective, the tangled_rope classification might shift towards rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_governance_effectiveness, empirical, 'Whether platforms can effectively govern attention extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_energy_budget, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cogn_tr_t0, cognitive_energy_budget, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cogn_tr_t5, cognitive_energy_budget, theater_ratio, 5, 0.2).
narrative_ontology:measurement(cogn_tr_t10, cognitive_energy_budget, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(cogn_be_t0, cognitive_energy_budget, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cogn_be_t5, cognitive_energy_budget, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cogn_be_t10, cognitive_energy_budget, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_energy_budget, information_standard).
narrative_ontology:affects_constraint(cognitive_energy_budget, social_media_addiction).
narrative_ontology:affects_constraint(cognitive_energy_budget, misinformation_proliferation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
