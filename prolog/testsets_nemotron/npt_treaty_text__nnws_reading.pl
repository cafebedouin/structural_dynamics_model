% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NNWS Reading of NPT Article VI — Disarmament as Binding Obligation
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint story captures the NNWS reading of the NPT treaty text:
 *   Article VI imposes a binding legal obligation on nuclear-weapon states to
 *   pursue and conclude good-faith negotiations on nuclear disarmament.
 *   Non-proliferation by NNWS is not an unconditional commitment but a
 *   conditional restraint purchased by NWS compliance. The reading is
 *   advanced through Review Conference pressure, working papers, and the TPNW
 *   as a competing normative regime. The constraint's extractiveness is
 *   moderate (0.22) because the enforcement mechanism is diplomatic pressure
 *   and legitimacy costs, not material coercion. Theater ratio is elevated
 *   (0.45) because NWS increasingly perform compliance (action plans, glossy
 *   reports) without structural disarmament progress. The reading coexists
 *   with the NWS reading — different parties hold each — but the TPNW's
 *   emergence creates structural pressure on the NWS reading's viability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.22).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.35).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NNWS Reading of NPT Article VI — Disarmament as Binding Obligation").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '3a8518eb-1271-4fb8-9f71-0c8e4e181273').
narrative_ontology:cs_kernel_codification('3a8518eb-1271-4fb8-9f71-0c8e4e181273', formalized).
narrative_ontology:cs_authority_grounding('3a8518eb-1271-4fb8-9f71-0c8e4e181273', lineage).
narrative_ontology:cs_interpretation_layer_present('3a8518eb-1271-4fb8-9f71-0c8e4e181273').
narrative_ontology:cs_reading_relation('3a8518eb-1271-4fb8-9f71-0c8e4e181273', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a8518eb-1271-4fb8-9f71-0c8e4e181273', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('3a8518eb-1271-4fb8-9f71-0c8e4e181273', foundational, article_vi_obligation_of_result).
narrative_ontology:cs_axiom_status(article_vi_obligation_of_result, holdable).
narrative_ontology:cs_axiom_grounding('3a8518eb-1271-4fb8-9f71-0c8e4e181273', article_vi_obligation_of_result, empirically_contingent).
narrative_ontology:cs_axiom('3a8518eb-1271-4fb8-9f71-0c8e4e181273', foundational, nonproliferation_conditioned_on_disarmament).
narrative_ontology:cs_axiom_status(nonproliferation_conditioned_on_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('3a8518eb-1271-4fb8-9f71-0c8e4e181273', nonproliferation_conditioned_on_disarmament, conventional).
narrative_ontology:cs_reference_frame('3a8518eb-1271-4fb8-9f71-0c8e4e181273', id_1968_reciprocal_bargain).
narrative_ontology:cs_drift_state('3a8518eb-1271-4fb8-9f71-0c8e4e181273', post_tpnw_entry_into_force, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3a8518eb-1271-4fb8-9f71-0c8e4e181273', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nnws_collective).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, tpnw_proponents).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nws_under_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_aligned_movement_npt_caucus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-nuclear-weapon states that collectively use Review Conferences, NPT committees, and the TPNW to press NWS on Article VI. They coordinate diplomatic pressure, submit working papers, and frame non-proliferation compliance as conditional on disarmament progress. Their exit is constrained by security dependencies and the value of the NPT regime itself.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nnws_collective, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nnws_collective, beneficiary).

% Nuclear-weapon states that face recurring diplomatic costs, legitimacy erosion, and regime-stability risks from NNWS pressure. They bear the burden of producing disarmament 'deliverables' (action plans, transparency measures, arsenal reductions) to maintain the non-proliferation bargain. Their exit is constrained by great-power competition and alliance commitments.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nws_under_pressure, payer,
    institutional, biographical, constrained, global).

% States and civil society actors driving the Treaty on the Prohibition of Nuclear Weapons. They gain normative leverage from the NNWS reading's framing of Article VI as binding; the TPNW's existence creates an outside option that sharpens Review Conference pressure. Their exit is mobile — they can advance the TPNW independently of NPT consensus.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_proponents, beneficiary,
    organized, generational, mobile, global).

% The three depositary governments (USA, UK, Russia) that administer the treaty, convene Review Conferences, and set procedural agendas. They occupy a dual position: they are NWS bearing extraction costs, but also the institutional gatekeepers of the regime's procedural legitimacy.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, npt_depositaries, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, npt_depositaries, observer).

% The coordinated NNWS bloc within the Non-Aligned Movement that drafts joint statements, sponsors resolutions, and sustains the institutional memory of the 'conditional bargain' interpretation. They benefit from the reading's normative framework but remain constrained by development and security aid dependencies on NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_aligned_movement_npt_caucus, beneficiary,
    organized, generational, constrained, global).

% NGO coalitions (ICAN, BASIC, Reaching Critical Will, etc.) that provide technical analysis, shaming campaigns, and normative entrepreneurship. They arbitrage across forums (NPT, UNGA, TPNW, CD) and are not structurally trapped by any single institutional channel.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, civil_society_disarmament_networks, observer,
    organized, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a reciprocal bargain: NNWS forego nuclear weapons (non-proliferation) in exchange for NWS pursuing good-faith disarmament negotiations (Article VI). The Review Conference cycle provides a recurring coordination point to assess compliance, adjust expectations, and prevent defection cascades.
% TRANSFER_FUNCTION: Transfers diplomatic legitimacy, regime-stability capital, and normative authority from NWS to NNWS when NWS fail to deliver disarmament progress. NNWS 'pay' with non-proliferation compliance; NWS 'pay' with disarmament action. When NWS default, NNWS withhold legitimacy and amplify alternative regimes (TPNW).
% ABSENT_VOICES: Nuclear-armed states outside the NPT (India, Pakistan, Israel, North Korea) are structurally excluded from the Article VI bargain — they bear no disarmament obligation under this reading but benefit from the non-proliferation regime's restraint on horizontal proliferation. Their absence is the regime's silent structural gap.
% DISAPPEARANCE_RATIONALE: If the Article VI binding-obligation reading vanished, the NPT would lose its reciprocal logic. NNWS would lose the legal basis for conditioning non-proliferation compliance on disarmament; NWS would face no institutional pressure for arsenal reductions; the TPNW would lose its interpretive anchor in the NPT; the Review Conference cycle would lose its substantive agenda.
% FOUNDING_PROBLEM: The 1968 bargain: NNWS accept permanent non-nuclear status in exchange for NWS pursuing nuclear disarmament in good faith, with the ultimate goal of a world free of nuclear weapons. The founding problem was preventing nuclear proliferation while acknowledging the injustice of a permanent two-tier system.
% FOUNDING_PROBLEM_CORROBORATION: NNWS and TPNW proponents attest the problem is live — disarmament remains unfulfilled, the two-tier injustice persists. NWS attest the problem is substantially managed — they cite arsenal reductions since the Cold War, the moratorium on testing, and the NPT's non-proliferation success. Independent legal scholars (e.g., the 1996 ICJ Advisory Opinion, the 2010 Action Plan consensus) corroborate that Article VI creates a binding obligation of result, not merely conduct, but disagree on whether current NWS practice satisfies it.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.22) reflects the diplomatic and legitimacy costs NWS bear when they fail to deliver disarmament progress — measurable in Review Conference failures, NNWS walkouts, and TPNW adhesions. Suppression (0.35) is the cost NNWS pay for maintaining the conditional bargain: they forego nuclear options while waiting for a disarmament that recedes. Theater ratio (0.45) captures the growing gap between NWS performative compliance (transparency measures, P5 process) and substantive disarmament. Accessibility collapse (0.4) is moderate: alternatives (TPNW, hedging, breakout) exist but carry high political costs. Resistance (0.55) is the sustained NNWS diplomatic campaign across five decades. The claim/metric independence is deliberate: the reading CLAIMS rope (genuine coordination) while metrics show a coordination function with significant performative drift — the engine computes the seat-level types.
 *
 * PERSPECTIVAL GAP:
 *   From the NNWS/agenda_setter seat, the constraint is a rope: a genuine coordination mechanism that solves the proliferation-disarmament bargain. From the NWS/payer seat, it trends toward tangled_rope: the coordination function is real (they benefit from non-proliferation) but the extraction (legitimacy costs, diplomatic pressure) is asymmetric and enforced through Review Conference rituals. From the TPNW/beneficiary seat, it is a scaffold: a transitional mechanism whose sunset is the TPNW's normative supremacy. From the civil_society/observer seat, it is a piton: a degraded bargain maintained by institutional inertia. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS_collective and NAM_caucus are structural beneficiaries (d ~0.2): they gain normative leverage and institutional voice from the reading. TPNW_proponents are mobile beneficiaries (d ~0.15): they gain an outside option that strengthens their regime. NWS_under_pressure are constrained payers (d ~0.75): they bear legitimacy costs and must produce deliverables; exit is constrained by alliance structures and great-power competition. NPT_depositaries are dual-positioned (d ~0.5): they administer the regime that extracts from them. Civil_society are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proliferation while acknowledging two-tier injustice) is CONTESTED: NNWS say the injustice persists; NWS say the proliferation-prevention function works. The mandatrophy risk is that the coordination function (non-proliferation) has succeeded while the reciprocal obligation (disarmament) has atrophied — the constraint persists as a performance of the bargain without its substance. Theater_ratio rising from 0.2 to 0.45 tracks this. The reading does not resolve mandatrophy; it structures the contest over whether the mandate has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_character,
    'Is Article VI a binding obligation of result (must achieve disarmament) or obligation of conduct (must negotiate in good faith)?',
    'Authoritative interpretation by ICJ, consensus at Review Conference, or state practice crystallizing into customary law.',
    'If obligation of result, NWS non-compliance is ongoing treaty violation — extraction is higher, NNWS conditional restraint is legally justified. If obligation of conduct, current NWS practice (P5 process, transparency) may satisfy it — extraction lower, NNWS pressure is political not legal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_character, conceptual, 'The core legal ambiguity that structures the NNWS vs NWS reading divergence.').

omega_variable(
    tpnw_npt_regime_relationship,
    'Does the TPNW strengthen the NPT''s Article VI obligation (complementarity) or undermine the NPT regime (competition/fragmentation)?',
    'Observed state behavior: TPNW parties'' NPT participation patterns, Review Conference dynamics, NWS responses to TPNW.',
    'If complementarity, the NNWS reading''s coordination function is reinforced — the TPNW is an enforcement layer. If competition, the NPT''s universality erodes — the constraint''s coordination value declines, theater ratio rises further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_npt_regime_relationship, empirical, 'Whether the outside option (TPNW) stabilizes or destabilizes the NNWS reading''s coordination function.').

omega_variable(
    nws_compliance_metric_ambiguity,
    'What constitutes ''good faith'' disarmament progress measurable enough to satisfy the conditional bargain?',
    'Negotiated metrics at Review Conference (e.g., 2010 Action Plan benchmarks), independent verification regimes, or scholarly consensus on compliance indicators.',
    'Without agreed metrics, NWS can claim compliance while NNWS claim default — the constraint''s coordination function degrades into mutual recrimination, theater ratio rises, extraction becomes performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_compliance_metric_ambiguity, conceptual, 'The measurement gap that enables performative compliance and sustains the theater_ratio trajectory.').

omega_variable(
    committer_framing_kernel_npt_treaty_text,
    'This constraint is the nnws_reading of the npt_treaty_text kernel. How does the sibling nws_reading (non-proliferation binding, disarmament aspirational) structurally differ in its beneficiary/victim assignment and epsilon?',
    'Compare the two readings'' constraint stories: nws_reading assigns beneficiaries=[nws_collective], victims=[nnws_constrained], and claims lower epsilon for NWS. The structural delta is the inversion of the conditional bargain.',
    'The two readings are not the same constraint viewed differently — they instantiate different constraints with different epsilon, different stakeholder structures, different types. The kernel_id + reading_id decomposition is required by epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_kernel_npt_treaty_text, conceptual, 'Commitment-system framing: this reading''s structural distinctness from sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nnws_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2017, npt_treaty_text__nnws_reading, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nnws_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nnws_reading, base_extractiveness, 1995, 0.2).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.23).
narrative_ontology:measurement(npt__be_t2017, npt_treaty_text__nnws_reading, base_extractiveness, 2017, 0.22).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nnws_reading, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nnws_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__nnws_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.33).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(npt__su_t2017, npt_treaty_text__nnws_reading, suppression_requirement, 2017, 0.35).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__nnws_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nnws_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, tpnw_regime_legitimacy).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, p5_process_transparency_measures).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, review_conference_procedural_rules).

% DUAL FORMULATION NOTE:
% NPT_TREATY_TEXT kernel decomposes into three constraint stories: nnws_reading (this story, rope, epsilon=0.22), nws_reading (tangled_rope, epsilon=0.35 for NWS seat), withdrawal_threshold_reading (scaffold, epsilon=0.15). The nnws_reading influences both siblings: it creates downstream pressure on nws_reading via TPNW regime competition, and on withdrawal_threshold_reading by raising the political cost of sovereignty-priority withdrawal. The nnws_reading and nws_reading coexist_with each other (different state coalitions hold each). The withdrawal_threshold_reading is influenced_by both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
