% ============================================================================
% CONSTRAINT STORY: asshole_filter_2015
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asshole_filter_2015, []).

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
 *   constraint_id: asshole_filter_2015
 *   human_readable: The Asshole Filter
 *   domain: psychological/social
 *
 * SUMMARY:
 *   The 'Asshole Filter' is a systemic social or organizational constraint
 *   where stated rules and boundaries are inconsistently enforced. This
 *   dynamic unintentionally repels cooperative individuals who respect the
 *   stated rules, while attracting or retaining transgressive individuals who
 *   learn that the rules can be ignored for personal gain. The core mechanism
 *   is the gap between the 'theater' of the official rules and the reality of
 *   what behavior is actually rewarded. Over time, this degrades the social
 *   environment, penalizes good actors, and can lead the system's maintainer
 *   to falsely conclude that most people are inherently transgressive.
 *
 * KEY AGENTS:
 *   - Cooperative Individuals: Primary victims (powerless/trapped) — they follow the stated rules and are systematically disadvantaged.
 *   - Transgressive Individuals: Primary beneficiaries (powerful/arbitrage) — they ignore the stated rules and are rewarded with access, resources, or faster outcomes.
 *   - System Maintainers: Institutional actors (institutional/constrained) — they create the filter, often unintentionally, through conflict avoidance. They are short-term beneficiaries (avoiding conflict) but long-term victims (presiding over a toxic culture).
 *   - Organizational Culture: Abstract victim (powerless/trapped) — the collective trust and morale of the group, which is degraded by the filter.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asshole_filter_2015, 0.6).
domain_priors:suppression_score(asshole_filter_2015, 0.65).
domain_priors:theater_ratio(asshole_filter_2015, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asshole_filter_2015, extractiveness, 0.6).
narrative_ontology:constraint_metric(asshole_filter_2015, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(asshole_filter_2015, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asshole_filter_2015, tangled_rope).
narrative_ontology:human_readable(asshole_filter_2015, "The Asshole Filter").
narrative_ontology:topic_domain(asshole_filter_2015, "psychological/social").

domain_priors:requires_active_enforcement(asshole_filter_2015).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asshole_filter_2015, transgressive_individuals).
narrative_ontology:constraint_beneficiary(asshole_filter_2015, system_maintainers_short_term).
narrative_ontology:constraint_victim(asshole_filter_2015, cooperative_individuals).
narrative_ontology:constraint_victim(asshole_filter_2015, organizational_culture).
narrative_ontology:constraint_victim(asshole_filter_2015, system_maintainers_long_term).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COOPERATIVE INDIVIDUAL (SNARE) — Follows the stated rules and is penalized for it. They are trapped in the system (e.g., by employment) and bear the full cost of the filter's extraction of opportunity and morale. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.68. This high effective extraction crosses the Snare threshold (≥0.66).
constraint_indexing:constraint_classification(asshole_filter_2015, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TRANSGRESSIVE INDIVIDUAL (ROPE) — Exploits the gap between stated and enforced rules for personal gain. From their perspective, the filter is a simple, effective coordination mechanism to get what they want. As a beneficiary with arbitrage exit, their directionality is low. d≈0.10, f(d)≈-0.07, σ=0.8 → χ≈-0.03. Negative extraction signifies a net subsidy.
constraint_indexing:constraint_classification(asshole_filter_2015, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: SYSTEM MAINTAINER (PITON) — The manager or leader who created the filter through conflict avoidance. Over time, they see the stated rules have become pure theater. The original function (coordination) has atrophied, but the rules persist inertially. The high theater_ratio (0.75) satisfies the Piton gate (≥0.70).
constraint_indexing:constraint_classification(asshole_filter_2015, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: NEW HIRE (SCAFFOLD) — A newcomer who observes the toxic system but has low switching costs. For them, the filter is a temporary problem. Their personal 'sunset clause' is their notice period. They will either see the system reformed or they will leave. The constraint is a temporary support for transgressors that they do not have to tolerate long-term.
constraint_indexing:constraint_classification(asshole_filter_2015, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The default analytical view. It recognizes both the claimed coordination function (the stated rules) and the severe, asymmetric extraction enabled by non-enforcement. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.83. This falls squarely in the Tangled Rope classification range (0.40 ≤ χ ≤ 0.90).
constraint_indexing:constraint_classification(asshole_filter_2015, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asshole_filter_2015_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asshole_filter_2015, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asshole_filter_2015, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asshole_filter_2015, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asshole_filter_2015, TR),
    TR >= 0.70.

:- end_tests(asshole_filter_2015_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.60): High. The filter allows transgressive individuals to extract significant value (opportunity, attention, resources) at the direct expense of cooperative ones. Suppression (0.65): High. For those trapped in the system (e.g., an employee), the options are to conform to the toxic norm, be exploited, or exit at high personal cost. This lack of viable alternatives is a form of coercion. Theater Ratio (0.75): Very High. The gap between the stated rules (e.g., 'please use the proper channels') and the enforced reality (rewarding those who bypass channels) is the central feature of the constraint. This high ratio enables the Piton classification from the long-term observer's perspective.
 *
 * PERSPECTIVAL GAP:
 *   The filter is a powerful example of perspectival divergence. For the cooperative victim, it's a Snare they cannot escape. For the transgressive beneficiary, it's a simple and effective Rope for getting ahead. For the system maintainer who sees the long-term decay, the original rules have become a functionless, inertial Piton. For a new hire with the ability to leave, the filter is a temporary Scaffold supporting a bad system that they can choose to exit. The analytical observer sees the combination of a coordination claim and an extractive reality, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The classification for each agent is derived directly from their structural position. The 'Cooperative Individual' is a victim with trapped exit options, leading to a high directionality (d≈0.95) and a Snare classification. The 'Transgressive Individual' is a beneficiary with arbitrage exit options, yielding a low, negative directionality (d≈0.10) and a Rope classification. The analytical observer's canonical directionality (d≈0.72) correctly identifies the underlying Tangled Rope structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves a common mandatrophy where a system is misidentified based on its stated purpose. The 'Asshole Filter' is often created by someone who believes they are establishing a Rope (a fair, clear set of rules for coordination). However, the structural analysis of beneficiaries, victims, and enforcement reveals its true nature as a Tangled Rope or Snare. Deferential Realism prevents this mislabeling by focusing on the observed structural reality of extraction and suppression, rather than the creator's intent or the system's performative claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_outcome,
    'Is the filter an unintentional outcome of conflict avoidance, or a deliberate (if unstated) strategy to attract and reward aggressive ''go-getters''?',
    'Behavioral analysis of system maintainers; comparison of their stated values vs. their enforcement actions.',
    'If intentional, the system is a more malicious Snare from the maintainer''s perspective. If unintentional, it''s a Piton or Tangled Rope born of incompetence. The structure for the victim remains a Snare regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_outcome, empirical, 'Distinguishing between unintentional incompetence and deliberate strategy in the filter''s creation.').

omega_variable(
    repulsion_vs_attraction,
    'Is the filter''s primary mechanism repelling cooperative people, or is it actively attracting transgressive ones?',
    'Longitudinal study of cohort composition in organizations with known filters; entry/exit interviews focusing on norm perception.',
    'If repulsion is primary, interventions should focus on supporting and rewarding cooperative actors. If attraction is primary, interventions must focus on robustly enforcing boundaries to make the environment unattractive to transgressors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(repulsion_vs_attraction, empirical, 'Determining if the filter works by pushing good actors out or pulling bad actors in.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asshole_filter_2015, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(assh_tr_t0, asshole_filter_2015, theater_ratio, 0, 0.2).
narrative_ontology:measurement(assh_tr_t4, asshole_filter_2015, theater_ratio, 4, 0.55).
narrative_ontology:measurement(assh_tr_t9, asshole_filter_2015, theater_ratio, 9, 0.75).

% Extraction over time
narrative_ontology:measurement(assh_be_t0, asshole_filter_2015, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(assh_be_t4, asshole_filter_2015, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(assh_be_t9, asshole_filter_2015, base_extractiveness, 9, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asshole_filter_2015, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
