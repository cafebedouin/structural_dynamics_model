% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint describes the 'existential matrix' reading of territorial
 *   sovereignty legitimacy, where territorial control is seen as a
 *   non-negotiable precondition for collective survival and identity. This
 *   framing renders conflict fundamentally zero-sum, overriding legal or
 *   historical arguments. It is one reading of the
 *   'territorial_sovereignty_legitimacy' kernel. The constraint is classified
 *   as a Snare due to its high extractiveness and suppression, which are
 *   sustained by the perceived existential threat and the suppression of
 *   alternative, compromise-based solutions. The beneficiaries are the
 *   dominant group and nationalist elites who leverage this narrative for
 *   power and territorial gain, while the victims are the subjugated group
 *   and international actors attempting to broker peace.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.9).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.95).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '936a58ad-8650-4223-8726-f702328d082c').
narrative_ontology:cs_kernel_codification('936a58ad-8650-4223-8726-f702328d082c', implicit).
narrative_ontology:cs_authority_grounding('936a58ad-8650-4223-8726-f702328d082c', extraction).
narrative_ontology:cs_interpretation_layer_present('936a58ad-8650-4223-8726-f702328d082c').
narrative_ontology:cs_reading_relation('936a58ad-8650-4223-8726-f702328d082c', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('936a58ad-8650-4223-8726-f702328d082c', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('936a58ad-8650-4223-8726-f702328d082c', foundational, territorial_control_is_existential_precondition).
narrative_ontology:cs_axiom_status(territorial_control_is_existential_precondition, holdable).
narrative_ontology:cs_axiom_grounding('936a58ad-8650-4223-8726-f702328d082c', territorial_control_is_existential_precondition, empirically_contingent).
narrative_ontology:cs_axiom('936a58ad-8650-4223-8726-f702328d082c', foundational, collective_identity_requires_exclusive_territory).
narrative_ontology:cs_axiom_status(collective_identity_requires_exclusive_territory, holdable).
narrative_ontology:cs_axiom_grounding('936a58ad-8650-4223-8726-f702328d082c', collective_identity_requires_exclusive_territory, deontological).
narrative_ontology:cs_reference_frame('936a58ad-8650-4223-8726-f702328d082c', pre_state_formation_existential_struggle).
narrative_ontology:cs_drift_state('936a58ad-8650-4223-8726-f702328d082c', contemporary_international_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('936a58ad-8650-4223-8726-f702328d082c', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_ethnic_group).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, nationalist_political_elites).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subjugated_ethnic_group).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, international_peace_brokers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because this reading justifies the complete appropriation of territory and resources by one group at the expense of another, with no room for shared benefit. Suppression is also very high (0.95) as any challenge to the zero-sum nature of the conflict, or any attempt to introduce alternative frameworks (like international law or human rights), is actively suppressed or dismissed as an existential threat. Theater ratio is low (0.1) because the existential threat is genuinely perceived by the dominant group, making the actions taken to secure territory appear functional rather than performative, even if the underlying premise is contested. The increasing extractiveness and suppression over time reflect the hardening of this narrative and the escalating conflict it justifies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dominant group, this is a Mountain or a Rope, an unchangeable reality or a necessary coordination for survival. From the perspective of the subjugated group and international observers, it is a Snare, a constructed constraint that extracts and suppresses under the guise of existential necessity. The engine's classification as Snare reflects the objective structural reality of extraction and suppression, regardless of the subjective framing by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant ethnic group and nationalist political elites are full beneficiaries (d=0.0-0.1) as the constraint directly legitimizes their control and power. The subjugated ethnic group is the primary target (d=1.0) as they bear the full cost of territorial loss and suppression. International peace brokers are also victims (d=0.8-0.9) as their efforts are consistently undermined by the constraint's logic. Human rights advocates are excluded, their arguments rendered irrelevant by the existential framing.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the 'founding problem' (perceived existential threat) is actively maintained and reinforced by the beneficiaries. The classification as Snare prevents mislabeling this as a 'natural' or 'inevitable' conflict, highlighting the active extraction and suppression inherent in this particular reading of sovereignty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_threat_objectivity,
    'Is the perceived existential threat an objective reality, or a constructed narrative used to justify extraction and suppression?',
    'Independent, multi-disciplinary analysis of security, demographic, and historical data, combined with a deconstruction of political rhetoric and its impact on public perception.',
    'If objectively real, the constraint might lean towards a Mountain or a highly coercive Rope (though still extractive). If primarily constructed, it firmly entrenches the Snare classification, highlighting the manufactured nature of the ''necessity''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_threat_objectivity, empirical, 'Distinguishing between genuine existential threat and politically constructed narratives.').

omega_variable(
    alternative_legitimacy_framings,
    'To what extent do alternative juridical or self-determination framings of sovereignty offer viable, non-zero-sum pathways to resolution, and how are they suppressed by the existential matrix?',
    'Analysis of historical attempts at compromise, the success/failure of international mediation, and the political space afforded to alternative narratives within the contested territory.',
    'If alternative framings are genuinely viable but systematically suppressed, it reinforces the Snare classification by highlighting the active suppression of exits. If they are structurally unworkable, it might suggest a more intractable, Mountain-like quality to the conflict, though still with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_framings, conceptual, 'Assessing the viability and suppression of alternative sovereignty legitimacy frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1993, 0.85).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1993, 0.9).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_sovereignty_legitimacy' kernel. This 'existential_matrix_reading' emphasizes collective survival and identity as the primary drivers, making conflict zero-sum. It directly influences and is influenced by the 'covenant_continuity_reading' and 'self_determination_reading' by shaping the political and military context in which those claims are made.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
