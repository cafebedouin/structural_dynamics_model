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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Application of Islamic Law
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the selective application of Islamic law by a
 *   state, where classical rulings are adopted in areas like family law and
 *   criminal codes, but reformist or secular frameworks are used in
 *   commercial and administrative law. The state's legitimacy is grounded in
 *   political sovereignty, not pure doctrinal fidelity. This is one reading
 *   of the 'quran_hadith_substrate' kernel, focusing on the state's
 *   instrumentalization of religious authority.
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
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Application of Islamic Law").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'c789a59c-3186-45e8-a8ad-362b98ae55ec').
narrative_ontology:cs_kernel_codification('c789a59c-3186-45e8-a8ad-362b98ae55ec', formalized).
narrative_ontology:cs_authority_grounding('c789a59c-3186-45e8-a8ad-362b98ae55ec', extraction).
narrative_ontology:cs_interpretation_layer_present('c789a59c-3186-45e8-a8ad-362b98ae55ec').
narrative_ontology:cs_reading_relation('c789a59c-3186-45e8-a8ad-362b98ae55ec', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('c789a59c-3186-45e8-a8ad-362b98ae55ec', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('c789a59c-3186-45e8-a8ad-362b98ae55ec', foundational, state_sovereignty_over_comprehensive_sharia).
narrative_ontology:cs_axiom_status(state_sovereignty_over_comprehensive_sharia, holdable).
narrative_ontology:cs_axiom_grounding('c789a59c-3186-45e8-a8ad-362b98ae55ec', state_sovereignty_over_comprehensive_sharia, conventional).
narrative_ontology:cs_axiom('c789a59c-3186-45e8-a8ad-362b98ae55ec', foundational, selective_application_for_legitimacy_and_flexibility).
narrative_ontology:cs_axiom_status(selective_application_for_legitimacy_and_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('c789a59c-3186-45e8-a8ad-362b98ae55ec', selective_application_for_legitimacy_and_flexibility, instrumental).
narrative_ontology:cs_reference_frame('c789a59c-3186-45e8-a8ad-362b98ae55ec', post_colonial_nation_state_pragmatism).
narrative_ontology:cs_drift_state('c789a59c-3186-45e8-a8ad-362b98ae55ec', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c789a59c-3186-45e8-a8ad-362b98ae55ec', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, secular_economic_actors).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_ulama).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_intellectuals).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, citizens_under_hybrid_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Instrumentalizes selective application of Sharia to bolster political legitimacy while maintaining flexibility in economic and administrative policy. Benefits from the perceived religious authority without being bound by comprehensive doctrinal fidelity.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocates for comprehensive application of classical Islamic law. Their authority is undermined by the state's selective adoption, which truncates their vision of a fully Sharia-compliant society. They bear the cost of an incomplete and instrumentalized legal system.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_ulama, payer,
    organized, generational, constrained, national).

% Seeks to re-interpret Islamic law in light of contemporary ethics and public interest. Their critical readings often challenge state authority and are suppressed to maintain regime stability, making them victims of the hybrid system's selective enforcement.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_intellectuals, payer,
    moderate, biographical, constrained, national).

% Benefits from the state's adoption of secular or reformist frameworks in commercial and administrative law, which facilitates integration into global markets and modern governance structures, avoiding the complexities of classical Islamic economic jurisprudence.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, secular_economic_actors, beneficiary,
    powerful, biographical, mobile, national).

% Lives under a legal system that applies different standards based on the area of law, leading to inconsistencies and potential injustices. They bear the costs of legal uncertainty and the instrumentalization of religious principles for political ends.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, citizens_under_hybrid_law, payer,
    powerless, biographical, trapped, national).

% Monitors the application of Islamic law in state contexts, particularly concerning human rights and equality. Their reports can exert external pressure but do not directly alter the state's internal legal framework.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the state to maintain a degree of religious legitimacy among conservative segments of the population by applying classical Sharia in personal status and criminal law, while simultaneously coordinating with global economic and administrative norms in other sectors.
% TRANSFER_FUNCTION: Transfers political stability and international economic integration to state elites, at the cost of doctrinal consistency and comprehensive Sharia application for traditionalists, and suppression of critical thought for reformists.
% ABSENT_VOICES: Advocates for a fully consistent, either purely classical or thoroughly reformist, application of Islamic law are marginalized. Their comprehensive visions are excluded from the state's pragmatic, selective approach.
% DISAPPEARANCE_RATIONALE: If this hybrid application vanished, the state would either have to fully commit to a comprehensive classical Sharia (alienating secular and reformist elements) or fully secularize (alienating conservative religious elements), leading to significant political and social upheaval.
% FOUNDING_PROBLEM: The challenge of governing modern nation-states with diverse populations and global economic ties, while simultaneously seeking religious legitimacy in societies with strong Islamic identities.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists, alongside international legal scholars, corroborate the ongoing tension between religious identity, political legitimacy, and modern governance requirements in many Muslim-majority states. This is attested by academic literature and international reports, not just state narratives.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).

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
 *   Extractiveness is moderate (0.35) as the state gains political stability and economic flexibility, but it's not a pure extraction due to the genuine coordination function of maintaining some religious legitimacy. Suppression is high (0.6) because the state actively suppresses both comprehensive traditionalist and critical reformist interpretations to maintain its hybrid approach. Theater ratio is moderate (0.4) as the 'Islamic' aspect is partly performative to secure legitimacy, while the actual governance is pragmatic. The temporal measurements show fluctuations in extractiveness and suppression, reflecting periods of increased state control or liberalization.
 *
 * PERSPECTIVAL GAP:
 *   State elites perceive this as a necessary and legitimate balancing act for governance, while both traditionalist and reformist groups view it as an instrumentalization and distortion of Islamic law. The engine's per-seat classification will reflect these divergent experiences based on their power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are clear beneficiaries, leveraging the hybrid system for political and economic gains. Traditionalist ulama and reformist intellectuals are victims, as their respective comprehensive visions of Islamic law are either truncated or suppressed. Citizens under hybrid law bear the costs of inconsistency. Secular economic actors benefit from the state's pragmatic approach to commercial law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_sincerity_of_religious_commitment,
    'To what extent is the state''s adoption of classical rulings a genuine religious commitment versus a purely instrumental act for political legitimacy?',
    'Analysis of state policies in areas where religious rulings conflict with political expediency but do not directly threaten stability; examination of judicial independence and the influence of religious scholars not aligned with the state.',
    'If purely instrumental, the extractiveness and theater ratio would be higher, and the constraint would lean more towards a Snare. If genuine, the coordination function would be stronger, supporting a Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_sincerity_of_religious_commitment, conceptual, 'Ambiguity of state''s motivation for applying Islamic law.').

omega_variable(
    suppression_of_dissenting_interpretations,
    'What is the precise mechanism and extent of suppression against traditionalist and reformist interpretations that challenge the state''s hybrid approach?',
    'Empirical study of censorship, arrests, academic freedom restrictions, and funding allocations for religious institutions and scholars.',
    'Higher, more direct suppression would increase the constraint''s Snare-like qualities. If suppression is primarily indirect (e.g., marginalization), it might remain a Tangled Rope but with a higher suppression metric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_dissenting_interpretations, empirical, 'Mechanism and degree of suppression of alternative Islamic legal interpretations.').

omega_variable(
    long_term_legitimacy_of_hybrid_model,
    'Can a legal system grounded in political sovereignty rather than comprehensive doctrinal fidelity maintain long-term religious legitimacy among its populace?',
    'Longitudinal sociological studies of religious belief and legal acceptance, analysis of public discourse, and the rise or fall of opposition movements advocating for alternative legal frameworks.',
    'If long-term legitimacy erodes, the constraint''s stability would decrease, requiring even higher suppression to maintain, potentially shifting it towards a Piton or a more unstable Snare. If it gains acceptance, it might stabilize as a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_legitimacy_of_hybrid_model, empirical, 'Sustainability of state''s hybrid legal model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1950, quran_hadith_substrate__state_hybrid, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(qura_tr_t1970, quran_hadith_substrate__state_hybrid, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(qura_tr_t1990, quran_hadith_substrate__state_hybrid, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(qura_tr_t2010, quran_hadith_substrate__state_hybrid, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(qura_tr_t2024, quran_hadith_substrate__state_hybrid, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(qura_be_t1950, quran_hadith_substrate__state_hybrid, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(qura_be_t1970, quran_hadith_substrate__state_hybrid, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(qura_be_t1990, quran_hadith_substrate__state_hybrid, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(qura_be_t2010, quran_hadith_substrate__state_hybrid, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(qura_be_t2024, quran_hadith_substrate__state_hybrid, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1950, quran_hadith_substrate__state_hybrid, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(qura_su_t1970, quran_hadith_substrate__state_hybrid, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(qura_su_t1990, quran_hadith_substrate__state_hybrid, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(qura_su_t2010, quran_hadith_substrate__state_hybrid, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(qura_su_t2024, quran_hadith_substrate__state_hybrid, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_hadith_substrate' kernel, alongside 'traditionalist_taqlid' and 'reformist_ijtihad'. Each represents a distinct approach to the authority and application of Islamic legal sources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
