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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: State Hybrid Application of Islamic Law
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint describes the 'state_hybrid' reading of the
 *   'quran_hadith_substrate' kernel, where a state selectively applies
 *   classical Islamic rulings in areas like family law and criminal codes,
 *   while adopting reformist or secular frameworks in commercial and
 *   administrative law. The state's legitimacy is grounded in political
 *   sovereignty rather than pure doctrinal fidelity, instrumentalizing
 *   religious law to balance internal religious demands with external
 *   pressures for modernization. The claimed type is 'tangled_rope' because
 *   it serves a coordination function (state legitimacy) but involves
 *   asymmetric extraction (state elites benefit from flexibility and power,
 *   while traditionalists and reformists see their comprehensive visions
 *   suppressed).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.65).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Application of Islamic Law").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'ae817f44-732c-4907-aad2-1b124b6f99ee').
narrative_ontology:cs_kernel_codification('ae817f44-732c-4907-aad2-1b124b6f99ee', formalized).
narrative_ontology:cs_authority_grounding('ae817f44-732c-4907-aad2-1b124b6f99ee', extraction).
narrative_ontology:cs_interpretation_layer_present('ae817f44-732c-4907-aad2-1b124b6f99ee').
narrative_ontology:cs_reading_relation('ae817f44-732c-4907-aad2-1b124b6f99ee', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('ae817f44-732c-4907-aad2-1b124b6f99ee', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('ae817f44-732c-4907-aad2-1b124b6f99ee', foundational, state_sovereignty_over_comprehensive_sharia).
narrative_ontology:cs_axiom_status(state_sovereignty_over_comprehensive_sharia, holdable).
narrative_ontology:cs_axiom_grounding('ae817f44-732c-4907-aad2-1b124b6f99ee', state_sovereignty_over_comprehensive_sharia, conventional).
narrative_ontology:cs_axiom('ae817f44-732c-4907-aad2-1b124b6f99ee', foundational, selective_application_for_public_order).
narrative_ontology:cs_axiom_status(selective_application_for_public_order, holdable).
narrative_ontology:cs_axiom_grounding('ae817f44-732c-4907-aad2-1b124b6f99ee', selective_application_for_public_order, instrumental).
narrative_ontology:cs_reference_frame('ae817f44-732c-4907-aad2-1b124b6f99ee', political_sovereignty_with_religious_legitimacy).
narrative_ontology:cs_drift_state('ae817f44-732c-4907-aad2-1b124b6f99ee', contemporary_globalized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae817f44-732c-4907-aad2-1b124b6f99ee', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, secular_legal_professionals).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars_and_adherents).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_intellectuals_and_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, general_populace).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, general_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Instrumentalize religious law for political legitimacy in certain domains (e.g., family, criminal) while maintaining flexibility and secular frameworks in others (e.g., commercial, administrative) to facilitate modern governance and international integration. They benefit from enhanced legitimacy and policy maneuverability.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocate for a comprehensive application of classical Islamic jurisprudence (Sharia) across all legal domains. They experience the state's selective application as a truncation of their doctrinal vision and a loss of religious purity, leading to frustration and marginalization of their full legal project.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars_and_adherents, payer,
    organized, generational, identity_locked, national).

% Seek to re-interpret Islamic law in light of contemporary ethics, human rights, and public interest (maslaha). Their critical readings and calls for comprehensive reform are often suppressed or ignored by the state when they threaten political stability or the existing hybrid legal order.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_intellectuals_and_activists, payer,
    organized, biographical, constrained, national).

% Experience a legal system that offers religious legitimacy in personal and moral spheres, alongside pragmatic, often secular, approaches in economic and administrative life. They benefit from stability but may bear the costs of legal inconsistencies or the suppression of alternative legal visions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, general_populace, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, general_populace, payer).

% Benefit from the state's adoption of secular frameworks in commercial and administrative law, which allows for modern legal practice, international legal engagement, and professional development. They must, however, navigate the religious elements of the hybrid system.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, secular_legal_professionals, beneficiary,
    moderate, biographical, mobile, national).

% Analyze the state's legal system for human rights compliance, rule of law, and consistency with international standards. They often highlight the hybrid nature and its implications for legal certainty and individual freedoms.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_observers, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for state legitimacy by selectively incorporating religious law, while allowing for pragmatic governance in modern economic and administrative sectors, thereby balancing internal religious demands with external pressures for modernization.
% TRANSFER_FUNCTION: Transfers political legitimacy from religious tradition to the state apparatus, and transfers legal flexibility from comprehensive religious doctrine to state policy, allowing state elites to instrumentalize religious authority for their own ends.
% ABSENT_VOICES: Advocates for a fully secular legal system are often excluded from public discourse on legal reform, as are those advocating for a comprehensive, uncompromised application of traditional Sharia across all domains. Both groups would challenge the state's selective application.
% DISAPPEARANCE_RATIONALE: If this hybrid application vanished, the state would lose a key source of legitimacy, leading to a crisis of governance. Legal systems would either fully secularize (facing religious backlash) or fully adopt a comprehensive religious framework (facing modern governance challenges and international isolation), fundamentally reorganizing the state-society relationship.
% FOUNDING_PROBLEM: To reconcile the need for modern governance and international integration with the demand for religious legitimacy from a populace deeply rooted in Islamic tradition, avoiding both secularist rejection and rigid traditionalist rule.
% FOUNDING_PROBLEM_CORROBORATION: State narratives and some segments of the populace corroborate the ongoing need to balance religious identity with modern state functions. However, traditionalist and reformist scholars often contest the state's sincerity, viewing it as an instrumentalization rather than a genuine reconciliation; legislative-hearing testimony and independent academic analysis from outside the benefiting parties support the instrumentalization reading.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.38, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.38) as the state gains significant political capital and policy flexibility, but the direct financial extraction is not as high as a pure rent-seeking snare. Suppression is substantial (0.65) because the state actively enforces its selective application, marginalizing both traditionalist calls for comprehensive Sharia and reformist critiques that challenge the status quo. Theater ratio is moderate (0.45) as the state maintains a performative adherence to religious principles in some domains to secure legitimacy, while its actual functional governance in other areas is driven by pragmatic or secular considerations. The increasing trend in extractiveness, suppression, and theater ratio over the interval reflects the state's growing instrumentalization of religious law and the hardening of its hybrid legal system.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, this hybrid system is a necessary and legitimate act of governance, balancing tradition and modernity. From the traditionalist perspective, it is an illegitimate truncation of divine law. From the reformist perspective, it is a cynical instrumentalization that stifles genuine ethical and legal progress. The engine's classification as 'tangled_rope' captures this structural asymmetry, where the state's coordination function (legitimacy) is intertwined with extraction from other groups.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are clear beneficiaries (agenda_setter, arbitrage exit) as they gain legitimacy and policy flexibility. Secular legal professionals also benefit from the modern legal frameworks. Traditionalist and reformist groups are victims (payer roles) as their respective comprehensive legal visions are suppressed or co-opted. The general populace experiences a mixed bag of benefits (stability, religious legitimacy) and costs (inconsistent legal application, limited legal evolution).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentalization_vs_genuine_reconciliation,
    'Is the state''s selective application of Islamic law a genuine attempt to reconcile tradition and modernity, or primarily an instrumentalization of religious authority for political power and policy flexibility?',
    'Analysis of legislative intent, judicial independence, and the consistency of legal outcomes with stated religious principles versus political expediency. Longitudinal studies of legal reform processes and their drivers.',
    'If primarily instrumentalization, the constraint''s extractiveness and theater ratio are higher, supporting a stronger ''snare'' component. If genuine reconciliation, the coordination function is more prominent, aligning closer to a ''rope'' or ''scaffold'' (if transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_vs_genuine_reconciliation, conceptual, 'Ambiguity between genuine legal synthesis and political instrumentalization.').

omega_variable(
    impact_on_legal_consistency_and_rights,
    'What is the actual impact of this hybrid legal system on legal consistency, predictability, and the protection of human rights, particularly for vulnerable populations?',
    'Empirical studies of judicial rulings, legal aid access, and human rights reports across different legal domains within the state. Comparative analysis with states employing more unified legal systems.',
    'If the hybridity leads to significant inconsistencies or rights abuses, the suppression and extractiveness metrics are higher, indicating a more coercive and less coordinative structure. If it largely maintains consistency and protects rights, the constraint leans more towards a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_legal_consistency_and_rights, empirical, 'Empirical consequences of legal hybridity on justice and rights.').

omega_variable(
    internal_resistance_to_legal_evolution,
    'How much internal resistance exists within the state apparatus to either further secularization or a more comprehensive, non-selective application of Sharia, and how does this resistance shape the persistence of the hybrid model?',
    'Analysis of internal policy debates, bureaucratic inertia, and the influence of different factions within the state on legal reform initiatives. Examination of public opinion shifts and their impact on state legal policy.',
    'High internal resistance to change suggests the hybrid model is deeply entrenched due to institutional inertia, potentially pushing it towards a ''piton'' if its functional benefits decline. Low resistance would indicate greater adaptability, potentially allowing for evolution towards a ''scaffold'' or ''rope''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_resistance_to_legal_evolution, empirical, 'Extent of internal state resistance to legal evolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1970, quran_hadith_substrate__state_hybrid, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(qura_tr_t1980, quran_hadith_substrate__state_hybrid, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(qura_tr_t1990, quran_hadith_substrate__state_hybrid, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(qura_tr_t2000, quran_hadith_substrate__state_hybrid, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(qura_tr_t2010, quran_hadith_substrate__state_hybrid, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(qura_tr_t2020, quran_hadith_substrate__state_hybrid, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(qura_be_t1970, quran_hadith_substrate__state_hybrid, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(qura_be_t1980, quran_hadith_substrate__state_hybrid, base_extractiveness, 1980, 0.29).
narrative_ontology:measurement(qura_be_t1990, quran_hadith_substrate__state_hybrid, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(qura_be_t2000, quran_hadith_substrate__state_hybrid, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(qura_be_t2010, quran_hadith_substrate__state_hybrid, base_extractiveness, 2010, 0.37).
narrative_ontology:measurement(qura_be_t2020, quran_hadith_substrate__state_hybrid, base_extractiveness, 2020, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1970, quran_hadith_substrate__state_hybrid, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(qura_su_t1980, quran_hadith_substrate__state_hybrid, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(qura_su_t1990, quran_hadith_substrate__state_hybrid, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(qura_su_t2000, quran_hadith_substrate__state_hybrid, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(qura_su_t2010, quran_hadith_substrate__state_hybrid, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(qura_su_t2020, quran_hadith_substrate__state_hybrid, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
