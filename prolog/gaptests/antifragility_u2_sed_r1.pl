% ============================================================================
% CONSTRAINT STORY: antifragility_u2_sed_r1
% ============================================================================
% Version: 3.4 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-05
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_sed_r1, []).

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
 *   constraint_id: antifragility_u2_sed_r1
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness as a result of stressors, shocks, volatility, noise,
 *   mistakes, faults, attacks, or failures. While this appears to be a
 *   fundamental property of all complex adaptive systems (a Mountain), its
 *   application in social and economic domains creates a severe perspectival
 *   gap. For the agent who can structure their affairs to harvest upside from
 *   volatility while externalizing the downside (the practitioner), it is a
 *   Rope. For the agent who is forced to absorb that downside (the fragile
 *   subject), it is a Snare.
 *
 * KEY AGENTS:
 *   - The Optimized Serf (Victim): A person or institution optimized for stability and efficiency, whose fragility is exposed and exploited by systemic volatility. (powerless/trapped)
 *   - The Barbell Practitioner (Beneficiary): An agent who adopts a bimodal strategy of extreme risk aversion in one domain and extreme risk-seeking in another, gaining from volatility. (moderate/arbitrage)
 *   - The Fragilista/Bureaucrat (Enforcer): An institutional actor who promotes policies that create fragility (e.g., over-optimization, debt-loading) while being insulated from the consequences. (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_sed_r1, 0.75).
domain_priors:suppression_score(antifragility_u2_sed_r1, 0.65).
domain_priors:theater_ratio(antifragility_u2_sed_r1, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_sed_r1, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_sed_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_sed_r1, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_sed_r1, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_sed_r1, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_sed_r1, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_sed_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_sed_r1, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_sed_r1, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_sed_r1, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OPTIMIZED SERF (SNARE)
constraint_indexing:constraint_classification(antifragility_u2_sed_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BARBELL PRACTITIONER (ROPE)
constraint_indexing:constraint_classification(antifragility_u2_sed_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE BUREAUCRAT (TANGLED ROPE)
constraint_indexing:constraint_classification(antifragility_u2_sed_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE EVOLUTIONARY OBSERVER (MOUNTAIN)
constraint_indexing:constraint_classification(antifragility_u2_sed_r1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_sed_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_sed_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_sed_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_sed_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_sed_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (0.75) reflects the 'convexity bias' inherent in the concept: the antifragile agent has a convex payoff from volatility (unlimited upside, limited downside), while the fragile agent has a concave payoff (limited upside, catastrophic downside). The system structurally transfers resources from the latter to the former. Suppression (0.65) is high because modern economic and social systems often penalize non-participation, forcing individuals and firms into fragile positions where they must absorb volatility generated elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The practitioner sees a Rope for navigating a complex world. The victim experiences a Snare that punishes them for the stability the system claims to value. The institutional actor sees a Tangled Rope—a necessary tool for managing risk that also creates systemic harm. The analytical observer, on a long enough timescale, sees a Mountain—the unforgiving logic of evolution. This four-way gap is characteristic of highly abstract principles applied to human systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the clear beneficiary/victim structure. The 'antifragile_practitioner' is the beneficiary, positioned to gain from disorder. The 'optimized_serfs' and 'fragile_institutions' are the victims, bearing the costs of that disorder. This creates a high directionality (d ≈ 1.0) for the victim, leading to a Snare classification, and a low directionality (d ≈ 0.15) for the beneficiary, leading to a Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by acknowledging the dual nature of the constraint. A naive analysis might label it a pure Snare (focusing only on the victim) or a pure Rope (focusing only on the practitioner). The Tangled Rope classification from the institutional and analytical perspectives correctly identifies that a genuine coordination/survival function (adapting to volatility) is intrinsically coupled with a severe extractive mechanism. The system cannot simply remove the extraction without destroying the function, and vice-versa.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the 0.75 extraction a functional necessity for evolution or predatory rent-seeking?',
    'Audit of Skin-in-the-Game metrics across the system; measurement of downside externalization.',
    'If necessity: Mountain. If predatory: Snare. The current classification as Tangled Rope reflects this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or predatory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_sed_r1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_sed_r1, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_sed_r1, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_sed_r1, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_sed_r1, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_sed_r1, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_sed_r1, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
