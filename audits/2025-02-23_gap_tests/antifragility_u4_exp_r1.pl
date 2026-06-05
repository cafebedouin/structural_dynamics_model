% ============================================================================
% CONSTRAINT STORY: antifragility_u4_exp_r1
% ============================================================================
% Version: 3.4 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-05
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u4_exp_r1, []).

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
 *   constraint_id: antifragility_u4_exp_r1
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness as a result of stressors, shocks, volatility, and
 *   randomness. While it may be a fundamental property of evolution (a
 *   Mountain), its application in social and economic systems creates a stark
 *   perspectival gap. For the informed practitioner with capital and exit
 *   options, it is a Rope for coordinating personal gain. For the fragile
 *   subject trapped within the system, it is a Snare that externalizes
 *   downside risk onto them.
 *
 * KEY AGENTS:
 *   - The Optimized Serf: Primary target (powerless/trapped) - bears the costs of volatility.
 *   - The Barbell Practitioner: Primary beneficiary (moderate/arbitrage) - harvests the upside from volatility.
 *   - The Fragilista/Bureaucrat: Institutional enforcer (institutional/constrained) - creates fragile systems through attempts to eliminate volatility.
 *   - The Evolutionary Observer: Analytical observer (analytical/analytical) - views the process over civilizational timescales.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u4_exp_r1, 0.75).
domain_priors:suppression_score(antifragility_u4_exp_r1, 0.65).
domain_priors:theater_ratio(antifragility_u4_exp_r1, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u4_exp_r1, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u4_exp_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u4_exp_r1, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u4_exp_r1, tangled_rope).
narrative_ontology:human_readable(antifragility_u4_exp_r1, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u4_exp_r1, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u4_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u4_exp_r1, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u4_exp_r1, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u4_exp_r1, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OPTIMIZED SERF (SNARE). Experiences volatility as pure, uncompensated downside risk imposed by an inescapable system.
constraint_indexing:constraint_classification(antifragility_u4_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BARBELL PRACTITIONER (ROPE). Uses the principle as a coordination tool for personal gain, with the ability to exit failing systems.
constraint_indexing:constraint_classification(antifragility_u4_exp_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE BUREAUCRAT (TANGLED ROPE). Attempts to manage the system for stability (coordination) but is constrained by its rules, inadvertently creating the fragility that enables extraction.
constraint_indexing:constraint_classification(antifragility_u4_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE EVOLUTIONARY OBSERVER (MOUNTAIN). Views antifragility as a fundamental, unchangeable property of complex adaptive systems over long time horizons.
constraint_indexing:constraint_classification(antifragility_u4_exp_r1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u4_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u4_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u4_exp_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u4_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u4_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (0.75) represents the 'convexity bias' inherent in the system's application: a small number of agents capture unbounded upside from volatility, while the costs (bounded but severe downside) are socialized or borne by a much larger, more fragile population. Suppression (0.65) is high because the system actively punishes stability-seeking and optimization, framing them as 'fragile' and thus justifying their failure.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. The practitioner sees a Rope because they only interact with the upside. The serf sees a Snare because they only experience the downside. The bureaucrat sees a Tangled Rope, acknowledging the coordination goal (stability) but also the extractive outcome. The analytical observer, abstracting away individual experience, sees a Mountain—an impersonal law of system dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural positions. The beneficiary is the 'antifragile_practitioner' who has arbitrage exit options, leading to a low or negative effective extraction (χ). The victims are the 'optimized_serfs' and 'fragile_institutions' who are trapped, leading to a high d-value and thus a high χ. This asymmetry is the core of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by refusing to accept the 'Mountain' or 'Rope' claims at face value. The analytical classification as Tangled Rope correctly identifies that a principle with a valid coordination function (adapting to change) has been weaponized into a mechanism for asymmetric extraction. It requires active enforcement (e.g., policies that encourage volatility, removal of safety nets) to function, which distinguishes it from a true Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the observed extraction a functional necessity for system evolution or a predatory feature of its human implementation?',
    'System-wide audit of 'Skin in the Game' metrics, determining if those who create volatility also bear its downside.',
    'If necessity: Mountain. If predatory: Snare. The current Tangled Rope classification reflects this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or a predatory implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u4_exp_r1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u4_exp_r1, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u4_exp_r1, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u4_exp_r1, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u4_exp_r1, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u4_exp_r1, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u4_exp_r1, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
