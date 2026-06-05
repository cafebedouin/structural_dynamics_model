% ============================================================================
% CONSTRAINT STORY: antifragility_u2_sed_r3
% ============================================================================
% Version: 3.5 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-23
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_sed_r3, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antifragility_u2_sed_r3
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes the property of systems that increase in
 *   capability, resilience, or robustness as a result of stressors, shocks,
 *   volatility, noise, mistakes, faults, attacks, or failures. While this
 *   appears as a natural law (Mountain) from a civilizational perspective,
 *   its implementation within human systems creates a stark perspectival gap.
 *   For the informed practitioner with agency, it is a tool for personal gain
 *   (Rope). For the subject optimized for efficiency within a fragile system,
 *   it is a mechanism of extraction where they bear the costs of volatility
 *   from which others benefit (Snare).
 *
 * KEY AGENTS:
 *   - The Optimized Serf: Primary target (powerless/trapped) - bears the costs of fragility.
 *   - The Barbell Practitioner: Primary beneficiary (moderate/arbitrage) - harvests upside from volatility.
 *   - The Fragilista/Bureaucrat: Institutional enforcer (institutional/constrained) - seeks to eliminate volatility, thereby creating systemic fragility.
 *   - The Evolutionary Observer: Analytical observer - sees the entire dynamic as a selection mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_sed_r3, 0.75).
domain_priors:suppression_score(antifragility_u2_sed_r3, 0.65).
domain_priors:theater_ratio(antifragility_u2_sed_r3, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_sed_r3, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_sed_r3, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_sed_r3, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_sed_r3, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_sed_r3, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_sed_r3, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_sed_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_sed_r3, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_sed_r3, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_sed_r3, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OPTIMIZED SERF (SNARE) - Experiences the downside of volatility without access to the upside.
constraint_indexing:constraint_classification(antifragility_u2_sed_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BARBELL PRACTITIONER (ROPE) - Actively uses volatility to gain, viewing the system as a tool for coordination with optionality.
constraint_indexing:constraint_classification(antifragility_u2_sed_r3, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE BUREAUCRAT (TANGLED ROPE) - Manages the system to suppress immediate volatility (coordination) which inadvertently concentrates fragility and enables extraction.
constraint_indexing:constraint_classification(antifragility_u2_sed_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE EVOLUTIONARY OBSERVER (MOUNTAIN) - Sees the process as a fundamental law of complex systems and selection, indifferent to the fate of individual components.
constraint_indexing:constraint_classification(antifragility_u2_sed_r3, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_sed_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_sed_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_sed_r3, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_sed_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_sed_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness score (0.75) reflects the core asymmetry: the antifragile agent benefits from convexity, harvesting disproportionate upside from volatility while externalizing the downside (tail risk) onto the fragile. Suppression (0.65) is high because modern systems are optimized for short-term efficiency and predictability, making robust or antifragile strategies non-obvious and costly to pursue for the average person.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. The practitioner sees a Rope for navigating reality. The serf, whose stability is sacrificed for the practitioner's gain, sees a Snare. The bureaucrat, trying to manage the system, sees a Tangled Rope of competing demands for stability and growth. The analyst sees an indifferent Mountain of natural selection.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those who can structure their affairs to have 'more upside than downside' from random events. This includes traders, entrepreneurs, and researchers with optionality. Victims are those in roles with concave payoffs: salaried employees in fragile industries, over-optimized supply chains, and citizens of nations with debt-fueled 'stability'. The system extracts resilience from the victims to subsidize the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the constraint as a Tangled Rope from the institutional perspective. This correctly identifies that the system has a genuine coordination function (maintaining short-term societal stability) that is inextricably linked to a highly extractive function (concentrating tail risk onto a vulnerable population). A simple Snare classification would miss the coordination element that gives the system its legitimacy and persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction (0.75) a functional necessity for evolutionary selection, or a predatory feature of a specific socio-economic arrangement?',
    'Comparative analysis of systems with and without 'skin in the game' mechanisms for beneficiaries. If downside is socialized while upside is privatized, it points to predatory extraction.',
    'If it is a functional necessity, the constraint is closer to a Mountain. If it is predatory, it is a Snare at its core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or a predatory artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_sed_r3, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_sed_r3, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_sed_r3, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_sed_r3, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_sed_r3, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_sed_r3, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_sed_r3, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
