% ============================================================================
% CONSTRAINT STORY: helsinki_bus_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_helsinki_bus_theory, []).

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
 *   constraint_id: helsinki_bus_theory
 *   human_readable: The Helsinki Bus Station Theory (Creative Persistence)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Helsinki Bus Station Theory posits that a creative individual will
 *   achieve originality only after a prolonged period of imitation and
 *   convergence. This story examines the constraints this places on early
 *   career creators, and the benefits it provides for established creators.
 *   The 'bus station' period involves pressure to conform, the need to build
 *   skills within accepted frameworks, and the limitations this places on
 *   originality.
 *
 * KEY AGENTS:
 *   - Early Career Creators: Victims (powerless/trapped) - Constrained by the need to imitate
 *   - Established Creators: Beneficiaries (institutional/arbitrage) - Benefit from perpetuation of existing styles
 *   - Emerging Disruptors: Navigators (moderate/mobile) - Learn within the established structures and then create their own path
 *   - Analytical Observer: Analysts (analytical/analytical) - Sees the situation as a process, with both constraint and benefits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(helsinki_bus_theory, 0.55).
domain_priors:suppression_score(helsinki_bus_theory, 0.4).
domain_priors:theater_ratio(helsinki_bus_theory, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(helsinki_bus_theory, extractiveness, 0.55).
narrative_ontology:constraint_metric(helsinki_bus_theory, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(helsinki_bus_theory, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(helsinki_bus_theory, tangled_rope).
narrative_ontology:human_readable(helsinki_bus_theory, "The Helsinki Bus Station Theory (Creative Persistence)").
narrative_ontology:topic_domain(helsinki_bus_theory, "social/psychological").

domain_priors:requires_active_enforcement(helsinki_bus_theory).
narrative_ontology:has_sunset_clause(helsinki_bus_theory).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(helsinki_bus_theory, established_creators).
narrative_ontology:constraint_victim(helsinki_bus_theory, early_career_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of early career creators who feel trapped by the need to imitate established styles to gain recognition, limiting their originality.
constraint_indexing:constraint_classification(helsinki_bus_theory, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective of established creators who benefit from the perpetuation of existing styles and conventions, maintaining their dominance and influence.
constraint_indexing:constraint_classification(helsinki_bus_theory, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical perspective viewing the theory as a necessary stage of creative development, involving both constraints and benefits for different actors.
constraint_indexing:constraint_classification(helsinki_bus_theory, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective of emerging creators who are able to navigate this period by building upon, remixing, and innovating within established styles, developing skills and networks that will allow them to break free and forge their own paths.
constraint_indexing:constraint_classification(helsinki_bus_theory, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(helsinki_bus_theory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(helsinki_bus_theory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(helsinki_bus_theory, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(helsinki_bus_theory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(helsinki_bus_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because early-career creators are under pressure to conform to existing standards to gain recognition, but not completely without agency. Suppression is moderate, reflecting some, but not total, limitation on creative expression. The theater ratio is low to moderate, as the period is primarily focused on skill development rather than show.
 *
 * PERSPECTIVAL GAP:
 *   The early career creator sees a snare because they are trapped in imitation. The established creator sees a rope because it maintains their dominance. The analytical observer sees a tangled rope because the phase is a structural feature of how skill and recognition is gained, even if it limits originality. The emerging creator sees a scaffold, since they are building skills to eventually create their own path.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the power level and exit options. Early career creators are powerless and trapped, experiencing maximum extraction. Established creators are institutional and have arbitrage, experiencing net benefit. The analytical observer sees both benefit and harm, resulting in a moderate directionality. Emerging disruptors are mobile, making them see it as a less harmful tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This scenario is an example of tangled rope as it has asymmetric extraction and requires active enforcement to maintain established styles and conventions. It's not pure extraction, because some level of skill and creativity still required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originality_threshold,
    'At what point does imitation become detrimental to true creative expression?',
    'Longitudinal studies tracking the creative output of individuals who initially imitated vs. those who pursued originality from the outset.',
    'Determines whether the ''bus station'' phase is a necessary evil or an avoidable hindrance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originality_threshold, empirical, 'Threshold for when imitation becomes detrimental').

omega_variable(
    network_value_established_styles,
    'Does strict adherence to established styles provide actual benefits in terms of visibility or career progression, or is this a false belief perpetuated by the creative industries?',
    'Statistical analysis of career outcomes for creators who heavily imitate vs. those who experiment more.',
    'If imitation does not offer career benefits, the ''bus station'' phase may be a largely performative and wasteful exercise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_value_established_styles, empirical, 'Whether strict adherence provides real value').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(helsinki_bus_theory, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hels_tr_t0, helsinki_bus_theory, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hels_tr_t5, helsinki_bus_theory, theater_ratio, 5, 0.2).
narrative_ontology:measurement(hels_tr_t10, helsinki_bus_theory, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(hels_be_t0, helsinki_bus_theory, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hels_be_t5, helsinki_bus_theory, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(hels_be_t10, helsinki_bus_theory, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
