% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing — Nonproliferation-Primary Reading
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates the nonproliferation-primary reading of the NPT's
 *   Article IV/VI pairing kernel: Article III verification is the treaty's
 *   only enforceable mechanism, Article IV peaceful-use rights are
 *   conditioned on satisfying that verification, and Article VI disarmament
 *   is read as aspirational and non-justiciable. Under this reading authority
 *   derives not from a balanced reciprocal bargain but from the weapon
 *   states' security interest in preventing horizontal proliferation — a
 *   structurally asymmetric arrangement that this story's ε and structural
 *   data describe from that reading's own lights. Two sibling readings of the
 *   same kernel (grand_bargain, which treats Article IV and VI as reciprocal
 *   and conditions restraint on disarmament progress; abolitionist, which
 *   treats Article VI as mandating complete disarmament and Article IV as
 *   illegitimate if it perpetuates proliferation risk) are separate
 *   constraint stories with their own ε and structure, linked via
 *   network.affects_constraints — not folded into this one.
 *
 * KEY AGENTS:
 *   - recognized_nuclear_weapon_states: agenda_setter/beneficiary (institutional/arbitrage) — sets interpretation, exempt from enforcement
 *   - nuclear_supplier_group_members: beneficiary (organized/mobile) — profits from administering Article III compliance gate
 *   - non_nuclear_weapon_states: payer (moderate/trapped) — bears permanent restraint with no enforceable reciprocal disarmament
 *   - nuclear_energy_seeking_states_under_scrutiny: payer (moderate/constrained) — bears asymmetric verification burden
 *   - iaea_safeguards_secretariat: agenda_setter/observer (institutional/analytical) — administers the asymmetry it did not design
 *   - disarmament_advocacy_coalition: excluded (organized/trapped) — no forum to contest Article VI non-justiciability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing — Nonproliferation-Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, 'f1cfaea5-9212-42bc-b1a3-b5b6033c65b9').
narrative_ontology:cs_kernel_codification('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', fixed_text).
narrative_ontology:cs_authority_grounding('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', extraction).
narrative_ontology:cs_interpretation_layer_present('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9').
narrative_ontology:cs_reading_relation('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_reading_relation('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', foundational, article_vi_is_aspirational_not_binding).
narrative_ontology:cs_axiom_status(article_vi_is_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', article_vi_is_aspirational_not_binding, conventional).
narrative_ontology:cs_axiom('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', foundational, verification_asymmetry_justified_by_proliferation_risk).
narrative_ontology:cs_axiom_status(verification_asymmetry_justified_by_proliferation_risk, holdable).
narrative_ontology:cs_axiom_grounding('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', verification_asymmetry_justified_by_proliferation_risk, instrumental).
narrative_ontology:cs_reference_frame('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', cold_war_bipolar_security_order).
narrative_ontology:cs_drift_state('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', post_cold_war_multipolar_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1cfaea5-9212-42bc-b1a3-b5b6033c65b9', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group_members).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_energy_seeking_states_under_scrutiny).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five treaty-recognized weapon states (US, Russia, UK, France, China) set the interpretive frame: Article III verification is the enforceable core, Article VI is read as a hortatory goal with no timeline or judicial forum. Their own arsenals sit entirely outside IAEA safeguards and outside any enforcement mechanism the treaty creates. They can modernize, expand, or retain warheads indefinitely while treating disarmament rhetoric as diplomatic cost rather than obligation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states, beneficiary).

% States and firms that supply enrichment, reactor, and fuel-cycle technology under the Article IV promise of peaceful-use assistance. They gain a stable, treaty-sanctioned market and a compliance gate (Article III) they administer and profit from advising on; export control regimes built on this reading generate ongoing consulting, licensing, and diplomatic leverage revenue.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group_members, beneficiary,
    organized, generational, mobile, global).

% The 185+ non-weapon signatories accepted permanent renunciation of nuclear weapons in exchange for the Article IV/VI package. Under this reading, their Article IV peaceful-use rights are perpetually conditioned on satisfying Article III verification demands set by others, while their expectation of reciprocal disarmament under Article VI has no enforcement mechanism, no deadline, and no forum. Withdrawal under Article X is legally available but carries severe diplomatic and security costs, making it a trap in practice rather than a real exit.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states, payer,
    moderate, civilizational, trapped, global).

% States pursuing civilian nuclear infrastructure face intrusive safeguards, sanctions threats, and supplier-group gatekeeping justified as Article III compliance, while comparable dual-use capability held by weapon states draws no equivalent scrutiny. Their peaceful-use right is real on paper but administratively bottlenecked by verification demands that can be escalated or relaxed based on the political alignment of the requesting state, not solely on technical compliance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_energy_seeking_states_under_scrutiny, payer,
    moderate, biographical, constrained, national).

% Administers Article III verification technically, but its mandate and resourcing are set by the weapon-state-dominated Board of Governors; it has no comparable mandate to verify Article VI disarmament progress, which structurally entrenches the asymmetry this reading describes.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_secretariat, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_secretariat, observer).

% Non-weapon states, humanitarian organizations, and NGOs pushing for Article VI to be treated as a binding, time-bound obligation are structurally excluded from the treaty's only enforcement forum (which addresses Article III compliance) and have no comparable venue to litigate disarmament failure; their objections surface in Review Conference statements that produce no binding consequence.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, disarmament_advocacy_coalition, excluded,
    organized, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework preventing the spread of nuclear weapons capability to additional states, using verified peaceful-use assistance as the incentive for non-weapon states to accept permanent restraint, coordinated through an internationally administered inspection regime.
% TRANSFER_FUNCTION: Moves enforceable restraint and compliance burden from weapon states (whose arsenals are exempt from the treaty's verification machinery) onto non-weapon states (whose civilian programs are subject to open-ended verification), while the disarmament obligation that was meant to flow the other way is rendered non-justiciable.
% ABSENT_VOICES: The disarmament advocacy coalition and non-weapon states seeking a binding Article VI timeline have no forum: the treaty's only compliance mechanism (Article III/IAEA safeguards) has jurisdiction over civilian nuclear programs, not over weapon-state arsenals, so their objection to the asymmetry cannot be adjudicated within the treaty's own structure.
% DISAPPEARANCE_RATIONALE: If this reading's authority collapsed (e.g., a binding disarmament timeline were judicially or politically imposed, or verification demands were applied symmetrically), weapon states would lose the diplomatic cover for indefinite arsenal retention, supplier-group gatekeeping would lose its nonproliferation-primary justification, and non-weapon states would gain leverage to condition further restraint on measurable disarmament — the entire compliance architecture built around this asymmetric reading would need renegotiation.
% FOUNDING_PROBLEM: In 1968, the founding problem was preventing additional states from acquiring nuclear weapons during a period when only five states possessed them, while offering non-weapon states a credible incentive (peaceful nuclear technology access) and a credible promise (eventual disarmament) to accept permanent renunciation.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and supplier-group members attest the founding problem — horizontal proliferation risk — remains fully live and justifies continued asymmetric verification. Independent assessments from the UN Institute for Disarmament Research, academic arms-control scholarship, and statements at successive NPT Review Conferences (2015, 2022) document that the disarmament half of the founding bargain is treated by weapon states as unenforceable, while the restraint half is actively enforced — corroboration from outside the weapon-state beneficiary group supports the reading that the bargain's reciprocal structure has been abandoned in practice even as the nonproliferation half persists.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at T=55) reflects a structure where restraint flows one direction (non-weapon states bear it) while the reciprocal disarmament obligation is denied justiciable status — the transfer is real but the return flow the founding bargain implied is foreclosed by this reading's own interpretive move. Suppression (0.72) is high because non-weapon states have no legal forum to compel Article VI compliance and withdrawal (Article X) carries severe costs, functioning as a trap rather than a genuine alternative. Theater ratio rises over the measured interval (0.20 to 0.45) as Review Conferences increasingly produce disarmament rhetoric and consensus statements with no binding follow-through — the performative half of the bargain growing as the substantive half atrophies. All three metrics share one time grid across the 1970-2025 interval (t=0 at entry into force, t=55 at present).
 *
 * PERSPECTIVAL GAP:
 *   From the weapon-state seat, this arrangement is a stable, functioning nonproliferation regime performing exactly as designed — verification is working, and disarmament remains a long-term aspiration appropriately insulated from litigation. From the non-weapon-state seat, the identical structure is experienced as an indefinitely extended asymmetric bargain where their sacrifice was real and permanent while the counterpart obligation was interpretively defanged. The engine computes these as different seat-level types from the same structural data; this story does not adjudicate between them but authors the structural facts each seat's classification is computed from.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states sit near the full-beneficiary end: they administer the interpretive frame, their arsenals are structurally exempt from the treaty's only enforcement mechanism, and their exit options are effectively arbitrage-grade (they can shape verification demands on others while facing none themselves). Non-weapon states sit near the full-target end: they bear the entire enforceable burden (Article III verification), accepted permanent renunciation, and have no comparable mechanism to compel the reciprocal disarmament this reading treats as aspirational. The supplier group occupies a beneficiary position derivative of the weapon-state framing — it profits from administering the compliance gate rather than bearing the underlying security risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing horizontal proliferation) remains substantially live and corroborated even by observers outside the weapon-state beneficiary group — this is not a pure mandatrophy case where the problem has vanished. What has drifted is the reciprocal half of the original bargain: the founding document paired restraint with a disarmament commitment, and this reading's interpretive move (Article VI as non-justiciable) has allowed the coordination function to persist while the reciprocal obligation that legitimated it in 1968 became structurally unenforceable. Tangled Rope captures this precisely: the coordination function (preventing proliferation) is genuine and beneficial even to non-weapon states in a narrow sense, but the same structure now extracts asymmetric restraint without delivering the promised reciprocal disarmament — coordination and extraction riding the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability_ambiguity,
    'Is Article VI''s ''good faith negotiation'' language genuinely non-justiciable as a matter of treaty law, or does this reading''s non-justiciability claim itself reflect the interpretive power of the weapon states who benefit from that reading?',
    'International Court of Justice advisory opinion practice (the 1996 ICJ Nuclear Weapons Advisory Opinion touched on this without fully resolving it), or a binding arbitration proceeding brought by non-weapon states testing whether Article VI creates enforceable obligations.',
    'If Article VI is found to carry enforceable content, the nonproliferation_primary reading''s core structural claim collapses toward the grand_bargain reading, and the extraction/coordination balance shifts substantially since weapon-state disarmament failure would become a legally cognizable breach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_ambiguity, conceptual, 'Whether Article VI''s non-justiciability is a legal fact or an artifact of interpretive power asymmetry.').

omega_variable(
    security_interest_vs_bargain_legitimacy,
    'Does deriving treaty authority from weapon-state security interest (rather than from the reciprocal bargain the treaty''s text and negotiating history describe) undermine the treaty''s own legitimacy claim over time?',
    'Track state party withdrawal threats, Review Conference consensus-document failures (2005, 2015, 2022 patterns), and TPNW ratification trends as behavioral evidence of whether non-weapon states are treating the bargain as still legitimate or as effectively abandoned.',
    'Sustained erosion of Review Conference consensus and continued TPNW growth would support classifying this reading''s authority-grounding as increasingly extractive rather than coordinative, potentially shifting the computed type toward snare at the non-weapon-state seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_interest_vs_bargain_legitimacy, empirical, 'Whether grounding authority in weapon-state security interest is stable or self-undermining.').

omega_variable(
    verification_asymmetry_intentionality,
    'Is the asymmetric verification burden (Article III applies to non-weapon states, no comparable mechanism applies to weapon-state arsenals) a deliberate design feature of this reading, or an artifact of 1968 negotiating constraints that this reading now defends as principled rather than contingent?',
    'Review of NPT negotiating history (ENDC records) to determine whether symmetric verification was proposed and rejected, versus never seriously contemplated given the technology and politics of the era.',
    'If symmetric verification was deliberately rejected by weapon states during negotiation, this strengthens the reading of Article III/VI asymmetry as intentional extraction rather than incidental design; if it was never feasible, the asymmetry is closer to a genuine coordination limitation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_asymmetry_intentionality, empirical, 'Whether the verification asymmetry is a deliberate extractive design choice or an artifact of 1968 constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t11, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 11, 0.26).
narrative_ontology:measurement(npt__tr_t22, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 22, 0.32).
narrative_ontology:measurement(npt__tr_t33, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 33, 0.37).
narrative_ontology:measurement(npt__tr_t44, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 44, 0.41).
narrative_ontology:measurement(npt__tr_t55, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 55, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt__be_t11, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 11, 0.5).
narrative_ontology:measurement(npt__be_t22, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 22, 0.56).
narrative_ontology:measurement(npt__be_t33, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 33, 0.61).
narrative_ontology:measurement(npt__be_t44, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 44, 0.65).
narrative_ontology:measurement(npt__be_t55, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 55, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(npt__su_t11, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 11, 0.6).
narrative_ontology:measurement(npt__su_t22, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 22, 0.64).
narrative_ontology:measurement(npt__su_t33, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 33, 0.67).
narrative_ontology:measurement(npt__su_t44, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 44, 0.7).
narrative_ontology:measurement(npt__su_t55, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 55, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the npt_article_iv_vi_pairing kernel. nonproliferation_primary (this story) treats Article VI as non-justiciable and grounds authority in weapon-state security interest, producing a Tangled Rope classification with high suppression and rising extractiveness. grand_bargain treats Article IV and VI as reciprocal obligations, which would be expected to produce a lower ε and a coordination-weighted classification closer to Rope or Tangled Rope with lower suppression. abolitionist treats Article VI as mandating complete disarmament and Article IV as illegitimate under continued proliferation risk, which would be expected to produce a Snare classification from the abolitionist reading's own lights, with the entire current arrangement read as extractive. Each reading is authored as a separate, ε-invariant constraint per the decomposition principle; this network edge records the kernel relationship, not a shared metric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
