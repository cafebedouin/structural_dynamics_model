% ============================================================================
% CONSTRAINT STORY: landscape_of_fear_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_landscape_of_fear_2026, []).

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
 *   constraint_id: landscape_of_fear_2026
 *   human_readable: The Landscape of Fear
 *   domain: biological
 *
 * SUMMARY:
 *   The "landscape of fear" describes how the mere presence or perception of
 *   predators constrains prey behavior, affecting feeding patterns,
 *   reproductive rates, and survival far beyond direct predation. This
 *   phenomenon has profound impacts on ecosystem dynamics and can even affect
 *   human activities such as agriculture.
 *
 * KEY AGENTS:
 *   - Prey Populations: Primary target (powerless/trapped) - constrained by predator presence, affects behaviors, feeding
 *   - Predators: Primary beneficiary (institutional/arbitrage) - benefits from concentrated prey making hunting more efficient
 *   - Ecosystem Health: Secondary beneficiary (analytical/mobile) - the regulation of populations may improve ecosystem health overall
 *   - Agricultural Output: Secondary target (moderate/constrained) - If prey are pests, reduced output from limited pest reduction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(landscape_of_fear_2026, 0.55).
domain_priors:suppression_score(landscape_of_fear_2026, 0.7).
domain_priors:theater_ratio(landscape_of_fear_2026, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(landscape_of_fear_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(landscape_of_fear_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(landscape_of_fear_2026, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(landscape_of_fear_2026, tangled_rope).
narrative_ontology:human_readable(landscape_of_fear_2026, "The Landscape of Fear").
narrative_ontology:topic_domain(landscape_of_fear_2026, "biological").

domain_priors:requires_active_enforcement(landscape_of_fear_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(landscape_of_fear_2026, predators).
narrative_ontology:constraint_beneficiary(landscape_of_fear_2026, ecosystem_health).
narrative_ontology:constraint_victim(landscape_of_fear_2026, prey_populations).
narrative_ontology:constraint_victim(landscape_of_fear_2026, agricultural_output).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Prey animals experience the landscape of fear as a snare, constantly threatened and unable to fully utilize resources, leading to reduced fitness and population sizes. Trapped within their habitat, the perceived threat suppresses optimal foraging and movement.
constraint_indexing:constraint_classification(landscape_of_fear_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Predators benefit from the landscape of fear as it concentrates prey, making hunting more efficient. As an institution, this benefit is maintained across generations, creating an environment that favors predator success. They can 'arbitrage' the locations within the landscape to optimize hunting.
constraint_indexing:constraint_classification(landscape_of_fear_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Perspective 3: From an analytical perspective, the landscape of fear represents a tangled rope. It structures ecosystem dynamics by both coordinating predator-prey interactions and creating inherent extraction. Researchers observing the system see the complexities in prey behavior and population distribution.
constraint_indexing:constraint_classification(landscape_of_fear_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 4: Ranchers with livestock in open grazing areas view the landscape of fear as a tangled rope, as their livestock is preyed upon, but the predators also may help keep the general ecosystem healthy (reducing invasive species, etc.) They are constrained in their ability to move their livestock.
constraint_indexing:constraint_classification(landscape_of_fear_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(landscape_of_fear_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(landscape_of_fear_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(landscape_of_fear_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(landscape_of_fear_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(landscape_of_fear_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness value (0.55) reflects a mix of direct consumption by predators and indirect costs due to altered prey behavior, such as reduced foraging time and increased vigilance. The suppression value (0.70) is high due to the strong inhibitory effect of predator presence on prey activities.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises because what benefits the predators (concentrated prey) directly harms the prey (reduced fitness and population sizes). The analytical view sees that this complex relationship can also positively affect the ecosystem overall. A secondary target of agricultural output represents the view that decreased prey reduces the harm to crop production.
 *
 * DIRECTIONALITY LOGIC:
 *   The Landscape of Fear benefits the predator populations, while also coordinating the ecosystem as a whole. The existence of predators changes the behavior of the prey, so the ecosystem as a whole is structured based on those behavioral changes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predator_threat_perception,
    'How much does the perceived threat from predators vary across prey species and environments?',
    'Conducting behavioral experiments with various prey species to measure their responses to different predator cues.',
    'If the threat perception is uniform, the landscape of fear acts as a more consistent snare. If the threat perception varies, the landscape of fear is more fragmented and complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predator_threat_perception, empirical, 'Quantifies the perceived threat from predators.').

omega_variable(
    ecosystem_feedback_strength,
    'How strongly does the altered prey behavior influence overall ecosystem health and resilience?',
    'Developing ecological models to simulate the effects of different prey behavior patterns on plant communities and nutrient cycling.',
    'If the feedback is strong, the landscape of fear serves as an important regulator. If the feedback is weak, the landscape of fear is primarily extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_feedback_strength, conceptual, 'Models the ecosystem feedback.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(landscape_of_fear_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(land_tr_t0, landscape_of_fear_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(land_tr_t5, landscape_of_fear_2026, theater_ratio, 5, 0.15).
narrative_ontology:measurement(land_tr_t10, landscape_of_fear_2026, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(land_be_t0, landscape_of_fear_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(land_be_t5, landscape_of_fear_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(land_be_t10, landscape_of_fear_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(landscape_of_fear_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
