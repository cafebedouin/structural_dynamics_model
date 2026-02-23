% ============================================================================
% CONSTRAINT STORY: antifragility_u2_sed_r5
% ============================================================================
% Version: 3.5 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_sed_r5, []).

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
 *   constraint_id: antifragility_u2_sed_r5
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness as a result of stressors, shocks, volatility, noise,
 *   mistakes, faults, attacks, or failures. While it can be seen as a
 *   fundamental property of all surviving complex systems (a Mountain), its
 *   application in human systems creates a stark perspectival gap. For the
 *   practitioner with agency and options, it is a tool for growth (a Rope).
 *   For the subject without agency, whose fragility provides the fuel for
 *   others' antifragility, it is an extractive trap (a Snare).
 *
 * KEY AGENTS:
 *   - The Optimized Serf: Primary target (powerless/trapped) - bears the costs of volatility.
 *   - The Barbell Practitioner: Primary beneficiary (moderate/arbitrage) - harvests the upside from volatility.
 *   - The Fragilista/Bureaucrat: Institutional enforcer (institutional/constrained) - creates fragile systems through misguided optimization.
 *   - The Evolutionary Observer: Analytical observer - sees the entire dynamic as a natural law of selection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_sed_r5, 0.75).
domain_priors:suppression_score(antifragility_u2_sed_r5, 0.65).
domain_priors:theater_ratio(antifragility_u2_sed_r5, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_sed_r5, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_sed_r5, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_sed_r5, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_sed_r5, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_sed_r5, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_sed_r5, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_sed_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_sed_r5, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_sed_r5, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_sed_r5, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OPTIMIZED SERF (SNARE) - Experiences downside volatility without access to the upside, trapped in a system optimized for fragility.
constraint_indexing:constraint_classification(antifragility_u2_sed_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BARBELL PRACTITIONER (ROPE) - Uses antifragility as a coordination tool to selectively engage with volatility for personal gain, with options to exit losing positions.
constraint_indexing:constraint_classification(antifragility_u2_sed_r5, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE BUREAUCRAT (TANGLED ROPE) - Manages a system that claims to reduce risk (coordination) but inadvertently creates systemic fragility, from which others extract value.
constraint_indexing:constraint_classification(antifragility_u2_sed_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE EVOLUTIONARY OBSERVER (MOUNTAIN) - Views antifragility as a fundamental, unchangeable property of any complex system that survives over long time horizons.
constraint_indexing:constraint_classification(antifragility_u2_sed_r5, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_sed_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_sed_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_sed_r5, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_sed_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_sed_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.75 reflects the core dynamic of 'convexity bias' where one party (the beneficiary) harvests unbounded upside from volatility, while the downside is capped and externalized to another party (the victim). Suppression (0.65) is high because modern optimized systems systematically remove the redundancies, buffers, and slack that would allow individuals to resist this dynamic.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The powerless victim sees a Snare because they are harmed by volatility they cannot escape. The empowered beneficiary sees a Rope, a strategy for navigating chaos. The institutional manager sees a Tangled Rope, a necessary but flawed system for maintaining order. The detached analyst sees a Mountain, an inalterable law of nature. The system's ability to appear as all four simultaneously is the source of its power and persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationship to volatility. Beneficiaries ('antifragile_practitioner') are those who can structure their affairs to have more upside than downside from random events. Victims ('optimized_serfs', 'fragile_institutions') are those whose affairs are structured, often by others, to have more downside than upside from the same events. The system actively transfers wealth and resilience from the latter to the former.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the constraint as a Tangled Rope from the analytical perspective. This avoids mislabeling it as a pure Mountain (ignoring the victims) or a pure Snare (ignoring the genuine adaptive function it serves at a systemic level). The Tangled Rope classification correctly identifies that a genuine coordination function (system survival and adaptation) is coupled with severe, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction observed in antifragile systems a functional necessity for evolutionary selection, or a predatory feature of specific social/economic designs?',
    'Comparative analysis of antifragile dynamics in natural vs. artificial systems; audit of 'skin in the game' metrics for beneficiaries.',
    'If it is a functional necessity, the constraint is closer to a Mountain. If it is a predatory design, it is a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or a predatory design feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_sed_r5, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_sed_r5, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_sed_r5, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_sed_r5, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_sed_r5, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_sed_r5, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_sed_r5, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
