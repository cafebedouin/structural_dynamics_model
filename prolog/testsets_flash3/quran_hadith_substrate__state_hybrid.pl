% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Application of Quran and Hadith
 *   domain: legal/political/religious
 *
 * SUMMARY:
 *   This constraint describes the 'state_hybrid' reading of the Quran/Hadith
 *   substrate, where a state selectively applies classical Islamic law in
 *   certain domains (e.g., family, criminal) for legitimacy, while adopting
 *   secular or reformist frameworks in others (e.g., commercial,
 *   administrative). This approach allows state elites to instrumentalize
 *   religious authority without fully committing to a comprehensive sharia
 *   system, leading to variable suppression depending on regime incentives.
 *   The claimed type is 'tangled_rope' because it serves a coordination
 *   function (state legitimacy) but involves asymmetric extraction from both
 *   traditionalist and reformist factions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.35).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.6).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.35).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Application of Quran and Hadith").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal/political/religious").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '08f3306f-bc60-45ab-a301-b985eb705a45').
narrative_ontology:cs_kernel_codification('08f3306f-bc60-45ab-a301-b985eb705a45', formalized).
narrative_ontology:cs_authority_grounding('08f3306f-bc60-45ab-a301-b985eb705a45', extraction).
narrative_ontology:cs_interpretation_layer_present('08f3306f-bc60-45ab-a301-b985eb705a45').
narrative_ontology:cs_reading_relation('08f3306f-bc60-45ab-a301-b985eb705a45', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('08f3306f-bc60-45ab-a301-b985eb705a45', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('08f3306f-bc60-45ab-a301-b985eb705a45', foundational, state_sovereignty_over_doctrinal_uniformity).
narrative_ontology:cs_axiom_status(state_sovereignty_over_doctrinal_uniformity, holdable).
narrative_ontology:cs_axiom_grounding('08f3306f-bc60-45ab-a301-b985eb705a45', state_sovereignty_over_doctrinal_uniformity, conventional).
narrative_ontology:cs_axiom('08f3306f-bc60-45ab-a301-b985eb705a45', foundational, selective_application_for_public_interest).
narrative_ontology:cs_axiom_status(selective_application_for_public_interest, holdable).
narrative_ontology:cs_axiom_grounding('08f3306f-bc60-45ab-a301-b985eb705a45', selective_application_for_public_interest, instrumental).
narrative_ontology:cs_reference_frame('08f3306f-bc60-45ab-a301-b985eb705a45', post_colonial_state_building).
narrative_ontology:cs_drift_state('08f3306f-bc60-45ab-a301-b985eb705a45', contemporary_globalized_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('08f3306f-bc60-45ab-a301-b985eb705a45', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_interests).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_activists).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, citizens_under_hybrid_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Instrumentalize selective application of Islamic law to bolster political legitimacy, particularly in social and moral domains, while maintaining flexibility for economic and administrative policies that may align with secular or reformist principles. They benefit from stability and control.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the state's adoption of secular or reformist frameworks in commercial law, which often align with international business practices and facilitate economic growth. They are largely unburdened by classical sharia restrictions in their operations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_interests, beneficiary,
    powerful, biographical, mobile, national).

% Bear the cost of seeing their comprehensive vision of sharia truncated and selectively applied. They advocate for full implementation of classical fiqh across all legal domains and resist the state's instrumentalization of religious texts. Their influence is limited by state power.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    organized, generational, constrained, regional).

% Are victims of the state's selective application, as their critical and contextual readings of Islamic law, particularly those challenging classical criminal or family law, are suppressed to maintain state legitimacy. They face risks for advocating for more progressive interpretations.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_activists, payer,
    moderate, generational, constrained, national).

% Live under a legal system that applies different frameworks to different aspects of their lives, leading to potential inconsistencies and a lack of clear legal coherence. They bear the social and personal costs of this hybridity, particularly in family and criminal matters.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, citizens_under_hybrid_law, payer,
    powerless, biographical, trapped, local).

% Monitor the application of Islamic law in state contexts, particularly regarding human rights, gender equality, and criminal justice. They provide external critiques and pressure, influencing the state's legal reforms and international standing.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the state to maintain a degree of religious legitimacy among conservative populations by upholding classical Islamic law in certain domains, while simultaneously engaging with global economic and administrative norms by applying secular or reformist frameworks in others.
% TRANSFER_FUNCTION: Transfers political stability and legitimacy to state elites by selectively appealing to religious authority, while transferring the burden of legal inconsistency and truncated doctrinal visions to traditionalist scholars, reformist activists, and ordinary citizens.
% ABSENT_VOICES: Hardline Islamist groups who advocate for a comprehensive, non-negotiable application of classical sharia across all domains are often suppressed or excluded from the legal discourse, as their vision directly challenges the state's hybrid approach and political sovereignty.
% DISAPPEARANCE_RATIONALE: If this hybrid application vanished overnight, the state would face an immediate legitimacy crisis, as both traditionalist and reformist factions would demand a consistent, comprehensive legal framework. This would lead to significant political instability and a complete reorganization of the legal and social order.
% FOUNDING_PROBLEM: The challenge of reconciling traditional Islamic legal heritage with the demands of modern statecraft, international law, and globalized economies, while maintaining internal political legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: State elites attest the problem is live, citing the need for stability and development. International legal scholars and human rights organizations corroborate the ongoing tension between traditional legal systems and modern governance, though they critique the state's chosen resolution.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) as state elites gain political capital and flexibility, but the direct financial extraction is not as high as a pure rent-seeking snare. Suppression is notable (0.6) because the state actively manages and enforces this selective application, suppressing comprehensive traditionalist demands and critical reformist interpretations that threaten its hybrid model. Theater ratio is moderate (0.4) as the state performs adherence to classical sharia in visible domains while quietly pursuing other agendas in less visible ones. The temporal measurements reflect a period of increasing state control and instrumentalization, followed by a slight relaxation or stabilization.
 *
 * PERSPECTIVAL GAP:
 *   State elites perceive this as a necessary and pragmatic approach to governance, balancing tradition with modernity. Traditionalists see it as an illegitimate fragmentation of divine law, while reformists view it as an opportunistic suppression of progressive interpretations. The engine's per-seat classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are clear beneficiaries, using the constraint for political gain and policy flexibility. Commercial interests also benefit from the secularized economic sphere. Traditionalist scholars and reformist activists are victims, as their respective comprehensive visions of Islamic law are either truncated or suppressed. Citizens under hybrid law experience both the coordination (stability) and extraction (inconsistency, limited legal recourse).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_stability_vs_doctrinal_fidelity,
    'To what extent is the state''s hybrid application driven by genuine attempts to reconcile Islamic tradition with modernity, versus purely instrumental concerns for regime stability and control?',
    'Analysis of legislative debates, judicial rulings, and public discourse over time, particularly during periods of political transition or external pressure, to discern underlying motivations.',
    'If primarily instrumental, the extractiveness and suppression metrics are more accurately attributed to political control rather than a coordination function, potentially reclassifying towards a snare. If genuine reconciliation is dominant, the coordination aspect is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stability_vs_doctrinal_fidelity, conceptual, 'Distinguishing genuine reconciliation from instrumentalization in state legal policy.').

omega_variable(
    suppression_of_alternative_readings,
    'What is the precise mechanism and intensity of suppression applied to traditionalist and reformist readings that challenge the state''s hybrid approach?',
    'Empirical study of censorship, arrests, academic freedom restrictions, and public discourse control targeting scholars and activists advocating for alternative comprehensive legal frameworks.',
    'Higher, more coercive suppression would increase the constraint''s overall suppression metric and push classification towards a snare, indicating a greater reliance on coercion to maintain the hybridity. Lower suppression would suggest more genuine (though managed) contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_readings, empirical, 'Measuring the active suppression of dissenting Islamic legal interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__state_hybrid, theater_ratio, 10, 0.35).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__state_hybrid, theater_ratio, 20, 0.4).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__state_hybrid, theater_ratio, 30, 0.42).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.4).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__state_hybrid, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__state_hybrid, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__state_hybrid, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__state_hybrid, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__state_hybrid, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__state_hybrid, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__state_hybrid, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__reformist_ijtihad).

% DUAL FORMULATION NOTE:
% This constraint is the 'state_hybrid' reading of the 'quran_hadith_substrate' kernel. It coexists with and influences the 'traditionalist_taqlid' and 'reformist_ijtihad' readings, as state policy shapes the environment in which these other interpretations operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
