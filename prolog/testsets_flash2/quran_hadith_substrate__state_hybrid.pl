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
 *   This constraint describes the 'state_hybrid' reading of the
 *   'quran_hadith_substrate' kernel, where a state selectively applies
 *   classical Islamic rulings in certain domains (e.g., family, criminal law)
 *   for legitimacy, while adopting secular or reformist frameworks in others
 *   (e.g., commercial, administrative law) for practical governance. This
 *   reading is distinct from 'traditionalist_taqlid' (comprehensive adherence
 *   to classical schools) and 'reformist_ijtihad' (contextual
 *   reinterpretation). The constraint is claimed as a Tangled Rope because it
 *   serves a coordination function (state legitimacy, legal order) but
 *   involves asymmetric extraction from both traditionalist and reformist
 *   groups, requiring active enforcement to maintain this selective
 *   application.
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
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '6e5fbfa4-286f-40c9-a74b-a94c2dc038d4').
narrative_ontology:cs_kernel_codification('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', formalized).
narrative_ontology:cs_authority_grounding('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', extraction).
narrative_ontology:cs_interpretation_layer_present('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4').
narrative_ontology:cs_reading_relation('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', foundational, state_sovereignty_over_comprehensive_sharia).
narrative_ontology:cs_axiom_status(state_sovereignty_over_comprehensive_sharia, holdable).
narrative_ontology:cs_axiom_grounding('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', state_sovereignty_over_comprehensive_sharia, conventional).
narrative_ontology:cs_axiom('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', foundational, selective_application_for_legitimacy_and_flexibility).
narrative_ontology:cs_axiom_status(selective_application_for_legitimacy_and_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', selective_application_for_legitimacy_and_flexibility, instrumental).
narrative_ontology:cs_reference_frame('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', post_colonial_nation_state_formation).
narrative_ontology:cs_drift_state('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', contemporary_globalized_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6e5fbfa4-286f-40c9-a74b-a94c2dc038d4', '').
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

% Instrumentalize selective application of Islamic law to bolster political legitimacy, particularly in social and moral domains, while maintaining flexibility for economic and administrative policies through secular or reformist frameworks. They benefit from stability and control.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from predictable, often secular, commercial and administrative laws that facilitate modern economic activity and international trade, avoiding the complexities or restrictions of classical Islamic jurisprudence in these areas.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_interests, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of seeing their comprehensive vision of Sharia law truncated and selectively applied, undermining the holistic integrity of classical fiqh. Their authority is challenged by the state's selective adoption.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    powerful, generational, identity_locked, regional).

% Are victims of the state's hybrid approach, as their efforts to promote contextual, ethical, and human rights-aligned interpretations of Islamic law are often suppressed or ignored in favor of classical rulings in sensitive areas like family and criminal law, which are used for political legitimation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_activists, payer,
    moderate, generational, constrained, national).

% Experience the inconsistency and potential contradictions of living under a legal system that applies different frameworks to different aspects of their lives, leading to legal uncertainty and potential injustice, particularly in personal status matters.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, citizens_under_hybrid_law, payer,
    powerless, biographical, trapped, local).

% Monitor the application of laws in these states, particularly concerning human rights, and often critique the selective adoption of classical rulings that may conflict with international standards, exerting external pressure for reform.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that attempts to reconcile traditional religious legitimacy with the demands of modern statecraft and globalized economies, allowing for a degree of social order and economic functionality.
% TRANSFER_FUNCTION: Transfers political legitimacy from religious tradition to the state apparatus, in exchange for the state's selective enforcement of certain classical religious norms, while transferring flexibility in economic policy to state elites and commercial interests.
% ABSENT_VOICES: A truly comprehensive, non-instrumental application of Sharia (as envisioned by some traditionalists) is absent, as is a fully secular or consistently reformist legal system. Both are suppressed by the state's hybrid approach, which benefits from their partial exclusion.
% DISAPPEARANCE_RATIONALE: If this hybrid application vanished overnight, the state would face an immediate legitimacy crisis, as its religious grounding would be removed. Legal systems would either fully secularize or revert to more comprehensive traditionalist or reformist interpretations, leading to significant social and political upheaval.
% FOUNDING_PROBLEM: The challenge of governing modern nation-states with diverse populations and global economic ties, while simultaneously seeking legitimacy from deeply ingrained religious traditions.
% FOUNDING_PROBLEM_CORROBORATION: State elites consistently articulate this problem as live, justifying their hybrid approach as a necessary balance. International observers and some academics corroborate the existence of this tension, though they critique the state's chosen resolution.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.35) is moderate, reflecting the state's ability to instrumentalize religious law for political gain, but also the need to maintain some functional legal order. Suppression (0.6) is significant, as the state actively suppresses comprehensive traditionalist interpretations and critical reformist readings that challenge its selective authority. Theater ratio (0.4) is moderate, indicating that while some religious application is genuine, a substantial portion is performative, designed to secure legitimacy rather than fully implement religious doctrine. The metrics show some fluctuation, reflecting shifts in state policy or external pressures over time.
 *
 * PERSPECTIVAL GAP:
 *   State elites perceive this as a necessary and legitimate balancing act, a 'rope' that coordinates diverse demands. Traditionalists see it as a 'snare' that distorts divine law for political expediency. Reformists view it as a 'tangled rope' that selectively enforces outdated norms while suppressing progressive interpretations. The engine's classification will reflect the structural realities of extraction and suppression, independent of these claims.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are clear beneficiaries (low d) as they gain legitimacy and flexibility. Commercial interests also benefit (low d) from predictable secular laws. Traditionalist scholars and reformist activists are victims (high d) as their respective visions of Islamic law are either truncated or suppressed. Citizens under hybrid law are also victims (high d) due to legal inconsistencies. International human rights bodies act as observers (analytical d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_legitimacy_source,
    'To what extent is the state''s legitimacy genuinely derived from its selective application of Islamic law, versus other sources like economic performance or coercive power?',
    'Sociological studies of public opinion on state legitimacy, analysis of political discourse during crises, and comparison with states that adopt purely secular or purely traditionalist legal systems.',
    'If legitimacy is primarily coercive or economic, the religious component of the hybrid system is more theatrical and extractive (higher theater_ratio, higher extractiveness). If genuinely derived, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legitimacy_source, empirical, 'Ambiguity regarding the true source of state legitimacy in hybrid legal systems.').

omega_variable(
    coordination_vs_extraction_balance,
    'What is the optimal balance between religious legitimacy and practical governance in a modern state, and does this hybrid system achieve it?',
    'Comparative legal studies across diverse Islamic-majority states, analysis of legal outcomes for citizens, and assessment against international human rights standards.',
    'If the balance is suboptimal, the constraint leans more towards a Snare (higher extractiveness, higher suppression). If it is a genuinely difficult but necessary compromise, it remains a Tangled Rope with a stronger coordination component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_balance, preference, 'Whether the hybrid system represents a necessary compromise or an extractive imbalance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative legal interpretations structural (state censorship, legal prohibitions) or internalized (self-censorship by scholars, social pressure)?',
    'Post-regime-change analysis: if suppression persists after the state''s coercive mechanisms are removed, reclassify as partially internalized. Analysis of academic and religious discourse for evidence of self-censorship.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making reform harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for legal interpretations.').


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
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__state_hybrid, theater_ratio, 30, 0.45).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.42).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__state_hybrid, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__state_hybrid, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__state_hybrid, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__state_hybrid, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__state_hybrid, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__state_hybrid, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__state_hybrid, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__reformist_ijtihad).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
