% ============================================================================
% CONSTRAINT STORY: coffee_cardiovascular_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coffee_cardiovascular_2026, []).

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
 *   constraint_id: coffee_cardiovascular_2026
 *   human_readable: The Caffeine Paradox Realignment
 *   domain: medical/health
 *
 * SUMMARY:
 *   For years, cardiologists cautioned against coffee for patients with
 *   atrial fibrillation (AF). However, recent studies have challenged this,
 *   suggesting that moderate coffee consumption is not harmful and may even
 *   have some benefits. This shift represents a 'Caffeine Paradox,' where
 *   outdated medical advice restricted patients unnecessarily. The constraint
 *   reflects the tension between initial fears, emerging research, and
 *   ongoing individual variability.
 *
 * KEY AGENTS:
 *   - Coffee Drinkers: Beneficiaries from relaxed guidelines (moderate/constrained)
 *   - Medical Researchers: Beneficiaries from improved knowledge (institutional/analytical)
 *   - Atrial Fibrillation Patients: Victims of outdated guidelines and anxieties (powerless/trapped)
 *   - Outdated Medical Guidelines: Institutional inertia (institutional/constrained)
 *   - Analytical Observer: Synthesizing research and variability (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coffee_cardiovascular_2026, 0.55).
domain_priors:suppression_score(coffee_cardiovascular_2026, 0.4).
domain_priors:theater_ratio(coffee_cardiovascular_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coffee_cardiovascular_2026, tangled_rope).
narrative_ontology:human_readable(coffee_cardiovascular_2026, "The Caffeine Paradox Realignment").
narrative_ontology:topic_domain(coffee_cardiovascular_2026, "medical/health").

domain_priors:requires_active_enforcement(coffee_cardiovascular_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coffee_cardiovascular_2026, coffee_drinkers).
narrative_ontology:constraint_beneficiary(coffee_cardiovascular_2026, medical_researchers).
narrative_ontology:constraint_victim(coffee_cardiovascular_2026, atrial_fibrillation_patients).
narrative_ontology:constraint_victim(coffee_cardiovascular_2026, outdated_medical_guidelines).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% AF patients initially face a Snare: outdated guidelines restrict their coffee consumption, leading to unnecessary anxiety and reduced quality of life. Trapped due to fear of exacerbating their condition.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% The medical research community benefits from clarifying the actual effects of caffeine on AF. This clarification enhances their credibility and provides evidence-based guidelines. View as rope as correct information emerges.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Coffee drinkers with AF experience a Tangled Rope. Initially constrained by medical advice, they now benefit from updated guidelines allowing moderate consumption. But remain constrained by the need to monitor their individual response to caffeine.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Outdated medical guidelines, representing a Piton, persist despite new evidence. These guidelines, once intended to protect AF patients, now impose unnecessary restrictions. Inertia prevents immediate update and continued incorrect messaging.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From an analytical perspective, the Caffeine Paradox represents a Tangled Rope. The initial fear and restriction stemmed from a lack of comprehensive data. New research has clarified that moderate coffee consumption is not harmful and may even be beneficial. Still, the need to individualize medical advice introduces some level of extraction.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coffee_cardiovascular_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coffee_cardiovascular_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coffee_cardiovascular_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coffee_cardiovascular_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coffee_cardiovascular_2026, TR),
    TR >= 0.70.

:- end_tests(coffee_cardiovascular_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The initial restriction on coffee represents a form of extraction from AF patients. Suppression (0.40): Moderate. While guidelines are evolving, some level of suppression persists due to lingering fears and the need for individualized advice. Theater Ratio (0.30): Relatively low. Evidence suggests the original caution had limited empirical validation (low theater).
 *
 * PERSPECTIVAL GAP:
 *   AF patients trapped by outdated guidelines see a Snare. Medical researchers building clarifying research see a Rope. Coffee drinkers with AF see a constrained but improving Tangled Rope. Outdated guidelines represent an inertial Piton. The analytical observer sees mixed dynamics (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   AF patients initially have high d (close to 1) from guideline extraction. Medical researchers have low d (close to 0) as they are the coordinating influence. Coffee drinkers with AF have d approaching 0.5 as they benefit, constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_caffeine_sensitivity,
    'How much does individual caffeine sensitivity vary, and how can it be accurately assessed?',
    'Large-scale studies correlating caffeine intake with AF incidence, accounting for genetic and environmental factors.',
    'Determines the precision of personalized recommendations regarding caffeine consumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_caffeine_sensitivity, empirical, 'The degree of variability in individual response to caffeine.').

omega_variable(
    longterm_effects_moderate_caffeine,
    'What are the long-term effects of moderate caffeine consumption on AF patients'' cardiovascular health?',
    'Longitudinal studies tracking cardiovascular outcomes in AF patients who consume moderate amounts of caffeine.',
    'Determines the sustainability and safety of current caffeine recommendations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(longterm_effects_moderate_caffeine, empirical, 'The long-term impact of moderate caffeine intake in AF patients.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coffee_cardiovascular_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coff_tr_t0, coffee_cardiovascular_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(coff_tr_t5, coffee_cardiovascular_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(coff_tr_t10, coffee_cardiovascular_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(coff_be_t0, coffee_cardiovascular_2026, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(coff_be_t5, coffee_cardiovascular_2026, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(coff_be_t10, coffee_cardiovascular_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coffee_cardiovascular_2026, information_standard).
narrative_ontology:affects_constraint(coffee_cardiovascular_2026, general_cardiovascular_recommendations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
