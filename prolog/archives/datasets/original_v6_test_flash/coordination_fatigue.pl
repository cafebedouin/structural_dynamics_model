% ============================================================================
% CONSTRAINT STORY: coordination_fatigue
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordination_fatigue, []).

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
 *   constraint_id: coordination_fatigue
 *   human_readable: The Consensus Exhaustion Loop
 *   domain: social/organizational/technological
 *
 * SUMMARY:
 *   In hyper-connected networks, the energy to maintain consensus and
 *   synchronization can exceed the creative output. The constant need for
 *   participation and discussion leads to exhaustion, burnout, and decreased
 *   productivity. This creates a loop where individuals are compelled to
 *   engage in consensus-building even when it hinders their ability to focus
 *   on actual creative work.
 *
 * KEY AGENTS:
 *   - Individual Contributors: Primary target (powerless/trapped) - bears the brunt of consensus exhaustion.
 *   - Quiet Experts: Secondary target (moderate/constrained) - valuable insights are underutilized.
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) - benefits from increased engagement, even if it's unproductive.
 *   - Loudest Voices: Secondary beneficiary (powerful/mobile) - thrive in discussion-oriented environments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coordination_fatigue, 0.55).
domain_priors:suppression_score(coordination_fatigue, 0.4).
domain_priors:theater_ratio(coordination_fatigue, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coordination_fatigue, extractiveness, 0.55).
narrative_ontology:constraint_metric(coordination_fatigue, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(coordination_fatigue, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coordination_fatigue, tangled_rope).
narrative_ontology:human_readable(coordination_fatigue, "The Consensus Exhaustion Loop").
narrative_ontology:topic_domain(coordination_fatigue, "social/organizational/technological").

domain_priors:requires_active_enforcement(coordination_fatigue).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coordination_fatigue, platform_operators).
narrative_ontology:constraint_beneficiary(coordination_fatigue, loudest_voices).
narrative_ontology:constraint_victim(coordination_fatigue, individual_contributors).
narrative_ontology:constraint_victim(coordination_fatigue, quiet_experts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of an individual contributor who feels overwhelmed by the constant need to participate in discussions and consensus-building, leading to burnout and decreased productivity. They lack the power to change the system and feel trapped.
constraint_indexing:constraint_classification(coordination_fatigue, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective of a quiet expert who has valuable insights but is often drowned out by louder voices and feels their expertise is underutilized due to the emphasis on consensus. They are somewhat constrained in their ability to exit because their expertise is valuable but the environment is not conducive to their work.
constraint_indexing:constraint_classification(coordination_fatigue, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the platform operator who benefits from increased engagement and activity, even if it leads to consensus exhaustion for some members. They can arbitrage by focusing on metrics that indicate activity rather than actual creative output, thus benefitting from the loop.
constraint_indexing:constraint_classification(coordination_fatigue, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of the loudest voices who thrive in environments that prioritize discussion and consensus, as they can exert influence and shape outcomes. They are mobile, as they can move to other platforms if this one becomes less favorable to them.
constraint_indexing:constraint_classification(coordination_fatigue, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% From a systemic perspective, consensus exhaustion is a tangled rope. It has the appearance of coordination, but actively extracts from the participants. The high degree of interconnectivity has turned the system to be net negative towards content creation. 
constraint_indexing:constraint_classification(coordination_fatigue, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_fatigue_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coordination_fatigue, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_fatigue, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coordination_fatigue, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coordination_fatigue_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The system extracts time and energy from participants, diminishing their ability to focus on creative output. Suppression (0.40): Moderate. Individuals feel pressured to participate in discussions and decision-making, limiting their autonomy and ability to pursue alternative activities. Theater ratio (0.30): Low. The performative aspect of consensus-building is present but not dominant.
 *
 * PERSPECTIVAL GAP:
 *   Individual contributors experience it as a snare because of their lack of power or escape. The expert is tangled, because he wants to contribute, but is drowned out. The platform operator sees a rope because the activity helps his numbers. The loud voices also see a tangled rope because they thrive in this environment, but see its potential for misuse. The analyst sees the whole loop as a tangled rope, as the system is actively enforcing this dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   The platform operator benefits from high engagement, so they are beneficiaries. Individual contributors and quiet experts are victims, as they lose focus and energy. Loudest voices benefit by influencing discussions, but can also be negatively affected if things get out of control.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents labeling coordination as extraction by considering the purpose of consensus-building. If the goal is to facilitate creative output, then the process should be evaluated based on its effectiveness in achieving that goal. If the process is counterproductive, it's likely extraction masquerading as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_consensus_threshold,
    'What is the optimal level of consensus required for effective decision-making in a networked environment?',
    'Empirical studies analyzing the relationship between consensus levels, decision quality, and member satisfaction in various networked environments.',
    'Determines whether the current emphasis on consensus is excessive and detrimental to creative output.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_consensus_threshold, empirical, 'Optimal level of consensus for effective decision-making.').

omega_variable(
    platform_design_bias,
    'To what extent do platform design features contribute to consensus exhaustion?',
    'Comparative analysis of different platform designs and their impact on member engagement and productivity.',
    'Identifies specific design elements that exacerbate consensus exhaustion and informs the development of more sustainable platforms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_design_bias, conceptual, 'Impact of platform design features on consensus exhaustion.').

omega_variable(
    alternative_governance_models,
    'Are there alternative governance models that can mitigate consensus exhaustion while still ensuring effective decision-making?',
    'Exploration and evaluation of alternative governance models, such as liquid democracy, delegation-based systems, and direct voting mechanisms.',
    'Provides potential solutions for reducing consensus exhaustion and improving overall platform effectiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_governance_models, preference, 'Viability of alternative governance models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coordination_fatigue, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coor_tr_t0, coordination_fatigue, theater_ratio, 0, 0.15).
narrative_ontology:measurement(coor_tr_t5, coordination_fatigue, theater_ratio, 5, 0.22).
narrative_ontology:measurement(coor_tr_t10, coordination_fatigue, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(coor_be_t0, coordination_fatigue, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(coor_be_t5, coordination_fatigue, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(coor_be_t10, coordination_fatigue, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coordination_fatigue, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
