% ============================================================================
% CONSTRAINT STORY: cognitive_energy_budget
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-28
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
 *   The Attention Exhaustion Trap describes the finite biological limit of an
 *   individual's daily cognitive energy being increasingly exploited by the
 *   demands of the modern information environment. Constant notifications,
 *   personalized content feeds, and gamified engagement mechanics extract
 *   attention, leading to cognitive fatigue and diminished decision-making
 *   capacity. This constraint manifests as a snare for individuals, a tangled
 *   rope for informed decision-making, and a rope for attention economy
 *   platforms. The performative aspects of legacy media ethics also play a
 *   role, but are insufficient to address this problem.
 *
 * KEY AGENTS:
 *   - Individual Cognitive Capacity: Primary victim (powerless/trapped) - Bears the brunt of attention extraction and has no exit from biological limits.
 *   - Informed Decision-Making: Secondary victim (moderate/constrained) - Constrained by limited cognitive resources and access to unbiased information.
 *   - Attention Economy Platforms: Primary beneficiary (institutional/arbitrage) - Benefit from capturing user attention and monetizing engagement.
 *   - Legacy Media Ethics: Institutional actor (analytical/analytical) - Traditional journalistic filters and ethical standards are insufficient.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_energy_budget, 0.65).
domain_priors:suppression_score(cognitive_energy_budget, 0.75).
domain_priors:theater_ratio(cognitive_energy_budget, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_energy_budget, extractiveness, 0.65).
narrative_ontology:constraint_metric(cognitive_energy_budget, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(cognitive_energy_budget, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_energy_budget, snare).
narrative_ontology:human_readable(cognitive_energy_budget, "The Attention Exhaustion Trap").
narrative_ontology:topic_domain(cognitive_energy_budget, "cognitive/technological/social").

domain_priors:requires_active_enforcement(cognitive_energy_budget).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_energy_budget, attention_economy_platforms).
narrative_ontology:constraint_victim(cognitive_energy_budget, individual_cognitive_capacity).
narrative_ontology:constraint_victim(cognitive_energy_budget, informed_decision_making).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual, with a finite cognitive energy budget, is trapped in a system that constantly demands attention, leading to exhaustion and reduced capacity for informed decision-making.
constraint_indexing:constraint_classification(cognitive_energy_budget, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% The ability to make informed decisions is constrained by the limited cognitive resources available due to constant attention demands. While access to information is abundant, the capacity to process it effectively is diminished.
constraint_indexing:constraint_classification(cognitive_energy_budget, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Attention economy platforms benefit from capturing user attention, viewing it as a resource to be monetized. They have the ability to arbitrage different engagement strategies to optimize attention capture.
constraint_indexing:constraint_classification(cognitive_energy_budget, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Traditional journalistic ethics aimed to protect cognitive capacity by filtering information. However, the algorithmic amplification of the attention economy undermines this protection, rendering such protections largely performative.
constraint_indexing:constraint_classification(cognitive_energy_budget, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_energy_budget_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_energy_budget, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_energy_budget, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.65): The system extracts a significant portion of an individual's cognitive energy daily, leading to fatigue and reduced decision-making quality. Suppression (0.75): High suppression due to the pervasive nature of attention demands and the addictive design of many platforms. Alternatives are limited as most online environments operate under the same extraction principle. Theater Ratio (0.75): High performative activity, suggesting a functional but potentially harmful system.
 *
 * PERSPECTIVAL GAP:
 *   The individual perspective sees a snare, trapped within a system that continuously demands attention. Informed decision-making views the situation as a tangled rope because there is abundant information, but the means to process it effectively are constrained. The attention economy platforms see it as a rope because they perceive the system as an efficient way to capture and monetize user attention. The piton perspective represents how legacy media ethics offer limited and eroded protections from the attention trap.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (attention platforms) have arbitrage exit options and benefit from the attention extraction. The victims (individuals, informed decision-making) have limited or no exit and bear the costs of cognitive exhaustion. Platform algorithms are designed to maximize attention capture, thereby increasing the extractiveness of this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a snare because individuals are trapped and have limited ability to avoid attention-demanding stimuli. Differentiating the constraint from a 'rope' requires highlighting the unequal power dynamic between the platforms and the individuals, where the platform dictates cognitive load.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_threshold,
    'What is the sustainable threshold of daily attention demand beyond which cognitive exhaustion significantly impairs decision-making?',
    'Cognitive load experiments, longitudinal studies on attention allocation, and analysis of decision-making quality under varying attention demands.',
    'If the threshold is low, the system is highly extractive, justifying stronger regulation. If the threshold is high, current levels of attention demand may be sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_threshold, empirical, 'Defines the threshold of sustainable daily attention demand.').

omega_variable(
    algorithmic_bias_amplification,
    'To what extent do algorithmic biases in attention economy platforms amplify the extractive effects of the attention exhaustion trap?',
    'Auditing algorithms for bias, analyzing the correlation between biased content exposure and cognitive exhaustion, and conducting user studies on the impact of biased content on decision-making.',
    'If algorithmic biases significantly amplify the trap, mitigating them becomes a priority. If the impact is minimal, other factors may be more critical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_bias_amplification, empirical, 'Quantifies the amplification of the attention trap through algorithmic bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_energy_budget, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cogn_tr_t0, cognitive_energy_budget, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cogn_tr_t5, cognitive_energy_budget, theater_ratio, 5, 0.65).
narrative_ontology:measurement(cogn_tr_t10, cognitive_energy_budget, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(cogn_be_t0, cognitive_energy_budget, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cogn_be_t5, cognitive_energy_budget, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cogn_be_t10, cognitive_energy_budget, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_energy_budget, information_standard).
narrative_ontology:affects_constraint(cognitive_energy_budget, information_overload).
narrative_ontology:affects_constraint(cognitive_energy_budget, digital_addiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
