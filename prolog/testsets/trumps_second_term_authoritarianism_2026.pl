% ============================================================================
% CONSTRAINT STORY: trump_second_term_authoritarianism_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_trump_second_term_authoritarianism_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: trump_second_term_authoritarianism_2026
 * human_readable: Electoral Authoritarianism (Trump II, Year One)
 * domain: political
 * * SUMMARY:
 * A year into a hypothetical second Trump term, the U.S. political system is modeled as an "electoral authoritarianism" regime. The constraint operates through the normalization of paramilitary force, summary execution of protesters, and the systemic dismantling of democratic infrastructure (elections, media, universities). It represents a rapid shift from a system with checks and balances to one governed by coercive force and loyalty tests.
 * * KEY AGENTS:
 * - Protester/Dissident: Subject (Powerless), facing lethal consequences for dissent.
 * - Administration Loyalist: Beneficiary (Institutional), gaining power and access within the new order.
 * - Analytical Observer: Auditor (Analytical), observing the structural collapse of democratic norms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(trump_second_term_authoritarianism_2026, 0.85). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(trump_second_term_authoritarianism_2026, 0.90).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(trump_second_term_authoritarianism_2026, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(trump_second_term_authoritarianism_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(trump_second_term_authoritarianism_2026, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(trump_second_term_authoritarianism_2026, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The regime claims its actions are necessary for law and order.
narrative_ontology:constraint_claim(trump_second_term_authoritarianism_2026, tangled_rope).
narrative_ontology:human_readable(trump_second_term_authoritarianism_2026, "Electoral Authoritarianism (Trump II, Year One)").
narrative_ontology:topic_domain(trump_second_term_authoritarianism_2026, "political").

% Binary flags
domain_priors:requires_active_enforcement(trump_second_term_authoritarianism_2026). % Requires paramilitary deployment and judicial pressure.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(trump_second_term_authoritarianism_2026, administration_loyalists).
narrative_ontology:constraint_victim(trump_second_term_authoritarianism_2026, political_dissidents).
narrative_ontology:constraint_victim(trump_second_term_authoritarianism_2026, independent_media).
narrative_ontology:constraint_victim(trump_second_term_authoritarianism_2026, university_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE DISSIDENT (SNARE)
% For the protester, the state is a coercive Snare. The penalty for dissent has
% shifted from legal to lethal, making participation a calculation of survival.
constraint_indexing:constraint_classification(trump_second_term_authoritarianism_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ADMINISTRATION LOYALIST (ROPE)
% For those aligned with the administration, the new order is a beneficial
% coordination mechanism (Rope) that provides access, influence, and career advancement.
constraint_indexing:constraint_classification(trump_second_term_authoritarianism_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a detached, historical viewpoint, the transition to authoritarianism appears
% as an inevitable structural closure (Mountain). The system's internal logic
% seems to be collapsing toward a fixed, oppressive state.
constraint_indexing:constraint_classification(trump_second_term_authoritarianism_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_second_term_authoritarianism_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(trump_second_term_authoritarianism_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_second_term_authoritarianism_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = rope.

test(threshold_validation) :-
    % Verify the base extractiveness meets the Snare threshold.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(trump_second_term_authoritarianism_2026, ExtMetricName, E),
    E >= 0.46.

:- end_tests(trump_second_term_authoritarianism_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system undergoing rapid, violent transformation. The
 * base extractiveness (0.85) represents the seizure of civil liberties, safety,
 * and democratic process. The suppression score (0.90) reflects the use of
 * lethal force and systemic dismantling of alternatives (media, courts).
 * The PERSPECTIVAL GAP is extreme: for loyalists, it's a Rope to power; for
 * dissidents, it's a lethal Snare. The analytical view of Mountain reflects
 * a sense of structural inevitability or "state collapse" where the degrees
 * of freedom for the system itself seem to be disappearing.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint's high extraction (0.85) requires mandatrophy analysis. The
 * system correctly avoids misclassifying this as a Tangled Rope. While there
 * is a "coordination" function for beneficiaries (the Rope perspective), its
 * existence is entirely parasitic on the extreme, non-consensual extraction
 * from victims. The primary function is raw power consolidation via coercion,
 * not complex coordination with unfortunate side effects. Therefore, the
 * Snare/Mountain classifications are structurally sound.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_midterm_2026_integrity,
    "How free and fair will the 2026 elections be under systematically restrictive executive actions?",
    "Monitor the ratio of purged voters to actual turnout and the prevalence of military deployment on election day.",
    "If Mountain: The transition to electoral authoritarianism is complete. If still contested: Resistance remains viable.",
    confidence_without_resolution(low)
).

omega_variable(
    omega_lethal_deterrence_threshold,
    "Will the 'lethal mental calculus' of summary executions permanently suppress mass protest?",
    "Compare mass protest turnout levels post-incident vs. previous high-water marks (e.g., 2020).",
    "If Snare is permanent: Public space is effectively dead. If protests adapt: A new form of resistance emerges.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trump_second_term_authoritarianism_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the rapid intensification of the authoritarian constraint
% over its first year (mapped to the 0-10 interval).
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (remains low as actions are brutally functional):
narrative_ontology:measurement(ta26_tr_t0, trump_second_term_authoritarianism_2026, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ta26_tr_t5, trump_second_term_authoritarianism_2026, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ta26_tr_t10, trump_second_term_authoritarianism_2026, theater_ratio, 10, 0.10).

% Extraction over time (extraction of civil liberties accelerates):
narrative_ontology:measurement(ta26_ex_t0, trump_second_term_authoritarianism_2026, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(ta26_ex_t5, trump_second_term_authoritarianism_2026, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ta26_ex_t10, trump_second_term_authoritarianism_2026, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The "coordination" is the enforcement of a new political order.
narrative_ontology:coordination_type(trump_second_term_authoritarianism_2026, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */