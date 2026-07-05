% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing — Abolitionist Reading (Weapon Possession Categorically Illegal)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the NPT Article IV/VI
 *   kernel: the claim that Article VI imposes a categorical, non-negotiable
 *   disarmament mandate, that Article IV's peaceful-use guarantee is
 *   illegitimate wherever it sustains dual-use proliferation capability, and
 *   that authority for this reading flows from humanitarian law and the
 *   Treaty on the Prohibition of Nuclear Weapons rather than from the NPT's
 *   own internal bargain structure. This is one of three sibling readings of
 *   the same kernel (nonproliferation_primary, grand_bargain) and is
 *   generated here as a single, ε-invariant constraint per Rule 1 — the
 *   sibling readings are not described or averaged into this story; they are
 *   separate constraints linked via cs_structure.reading_relations and
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - tpnw_ratifying_states: agenda_setter (organized/mobile) — asserts and propagates the reading
 *   - humanitarian_disarmament_ngos: beneficiary (organized/mobile) — gains moral and campaign capital
 *   - civilian_nuclear_energy_programs_in_developing_states: payer (powerless/trapped) — loses Article IV protection under the collapsed peaceful/military distinction
 *   - nuclear_weapon_states: excluded (institutional/arbitrage) — reject the reading's authority claim entirely
 *   - iaea_safeguards_apparatus: observer (institutional/analytical) — administers a verification regime the reading does not formally alter
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.58).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.4).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing — Abolitionist Reading (Weapon Possession Categorically Illegal)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '475a6cb2-5276-4dd5-bab8-8ee8b1039889').
narrative_ontology:cs_kernel_codification('475a6cb2-5276-4dd5-bab8-8ee8b1039889', distributed).
narrative_ontology:cs_authority_grounding('475a6cb2-5276-4dd5-bab8-8ee8b1039889', distributed).
narrative_ontology:cs_reading_relation('475a6cb2-5276-4dd5-bab8-8ee8b1039889', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('475a6cb2-5276-4dd5-bab8-8ee8b1039889', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_axiom('475a6cb2-5276-4dd5-bab8-8ee8b1039889', foundational, weapon_possession_categorically_illegal).
narrative_ontology:cs_axiom_status(weapon_possession_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('475a6cb2-5276-4dd5-bab8-8ee8b1039889', weapon_possession_categorically_illegal, deontological).
narrative_ontology:cs_axiom('475a6cb2-5276-4dd5-bab8-8ee8b1039889', foundational, dual_use_capability_itself_constitutes_illegitimacy).
narrative_ontology:cs_axiom_status(dual_use_capability_itself_constitutes_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('475a6cb2-5276-4dd5-bab8-8ee8b1039889', dual_use_capability_itself_constitutes_illegitimacy, empirically_contingent).
narrative_ontology:cs_axiom('475a6cb2-5276-4dd5-bab8-8ee8b1039889', secondary, external_humanitarian_instrument_supersedes_npt_bargain_text).
narrative_ontology:cs_axiom_status(external_humanitarian_instrument_supersedes_npt_bargain_text, holdable).
narrative_ontology:cs_axiom_grounding('475a6cb2-5276-4dd5-bab8-8ee8b1039889', external_humanitarian_instrument_supersedes_npt_bargain_text, conventional).
narrative_ontology:cs_reference_frame('475a6cb2-5276-4dd5-bab8-8ee8b1039889', npt_1968_grand_bargain_text).
narrative_ontology:cs_drift_state('475a6cb2-5276-4dd5-bab8-8ee8b1039889', post_tpnw_adoption_2017, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('475a6cb2-5276-4dd5-bab8-8ee8b1039889', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, humanitarian_disarmament_ngos).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, tpnw_ratifying_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, civilian_nuclear_energy_programs_in_developing_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_weapon_states_seeking_fuel_cycle_technology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_supremacy_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, categorical_weapons_prohibition_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of mostly non-nuclear-weapon states that has ratified the Treaty on the Prohibition of Nuclear Weapons and asserts that this instrument, together with humanitarian law, now supplies the authoritative reading of the NPT's own disarmament obligation. They actively press the claim that Article VI is a categorical mandate and that Article IV's promotion of peaceful nuclear technology is illegitimate wherever it sustains dual-use proliferation risk, in forums like the NPT Review Conferences and UN General Assembly.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_ratifying_states, agenda_setter,
    organized, generational, mobile, global).

% Civil society coalitions (ICAN and allied networks) that built the humanitarian-consequences framing underlying TPNW and now use the abolitionist reading of Article VI to campaign against nuclear weapon state modernization programs and against expansive Article IV cooperation. They gain moral authority, funding, and standing from the reading's uncompromising posture.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanitarian_disarmament_ngos, beneficiary,
    organized, generational, mobile, global).

% States without nuclear weapons that benefit rhetorically and diplomatically from a reading that delegitimizes weapon-state possession outright, strengthening their negotiating position. Some of these same states also operate or aspire to civilian nuclear energy programs, so the reading's collapse of the peaceful/military distinction can cost them access to enrichment or reprocessing cooperation they would otherwise be entitled to under a grand-bargain reading.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_coalition, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_coalition, payer).

% National nuclear power and medical-isotope programs in the Global South that rely on Article IV's promise of assistance with peaceful nuclear technology. Under the abolitionist reading, any dual-use fuel-cycle capability associated with these programs is treated as illegitimate regardless of stated peaceful intent, exposing them to suspension of technical cooperation, export controls, and diplomatic pressure they have no comparable leverage to resist.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, civilian_nuclear_energy_programs_in_developing_states, payer,
    powerless, biographical, trapped, national).

% States pursuing enrichment or reprocessing capacity for energy independence or research find the abolitionist reading collapses the peaceful/military distinction their Article IV rights depend on, treating the underlying capability itself as the harm. They cannot exit the NPT without incurring severe diplomatic and economic costs, and TPNW membership does not restore the technology access Article IV promised.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_weapon_states_seeking_fuel_cycle_technology, payer,
    powerless, biographical, trapped, national).

% The five NPT-recognized weapon states (and de facto nuclear states outside the treaty) reject the abolitionist reading's authority claim entirely, refusing to sign or recognize TPNW and treating Article VI as a good-faith negotiating obligation rather than a categorical mandate. Their objection is heard in Review Conference walkouts and formal non-participation but does not alter the reading's currency among the coalition that holds it.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, excluded,
    institutional, civilizational, arbitrage, global).

% The verification body administering safeguards under Article III watches the abolitionist reading reshape political pressure on dual-use technology transfers without itself adopting the reading's categorical stance; its mandate remains keyed to the grand-bargain and nonproliferation-primary architectures, creating friction when abolitionist advocacy targets safeguarded, compliant programs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, iaea_safeguards_apparatus, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, diffuse).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a coalition of non-nuclear-weapon states and civil society around a single interpretive claim — that humanitarian law and the TPNW supply an authority superseding the NPT's own bargain — allowing them to act with a unified diplomatic and normative posture that would be harder to sustain individually.
% TRANSFER_FUNCTION: Moves normative and diplomatic capital away from nuclear weapon states and toward the abolitionist coalition and TPNW instrument; simultaneously moves material access to peaceful nuclear technology away from non-weapon states whose civilian fuel-cycle programs get swept into the delegitimized dual-use category, regardless of their compliance record under Article III.
% ABSENT_VOICES: States actually operating compliant, safeguarded fuel-cycle programs in the Global South rarely have a seat in the forums where the abolitionist reading is advanced (TPNW meetings, humanitarian-law conferences); their objection — that the reading punishes capability rather than conduct — is mostly voiced instead by nuclear weapon states, whose motives are treated as self-interested and are therefore not registered as a corroborating outside voice.
% DISAPPEARANCE_RATIONALE: If the abolitionist reading vanished, TPNW-aligned states and NGOs assert the world would rearrange catastrophically toward normalized proliferation risk; nuclear weapon states and much of the nonproliferation-primary bloc assert almost nothing would change operationally since TPNW binds no weapon state and the reading has produced no verification or enforcement mechanism of its own — the dispute over what would rearrange is itself unresolved.
% FOUNDING_PROBLEM: The perceived failure of nuclear weapon states to pursue Article VI disarmament in good faith over five decades, and the humanitarian-consequences movement's judgment that incremental arms control was structurally incapable of eliminating existential risk.
% FOUNDING_PROBLEM_CORROBORATION: TPNW states and ICAN attest the founding problem (weapon-state disarmament failure) is live and worsening, citing modernization programs. Independent arms-control scholars outside the TPNW coalition (e.g., at nonproliferation-focused academic institutes) corroborate that disarmament has stalled, but dispute that delegitimizing Article IV peaceful-use cooperation addresses that problem rather than displacing its cost onto uninvolved civilian programs — a critique the abolitionist coalition itself does not raise.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, contested).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.58) and rising because the reading's practical effect is to redirect diplomatic and material costs onto non-weapon states with compliant civilian programs, even though the reading's stated target is weapon-state disarmament failure. Suppression is moderate (0.4): the reading operates through normative and diplomatic pressure, not binding enforcement, since TPNW lacks jurisdiction over non-ratifying weapon states. Theater ratio is moderate and rising (0.30 to 0.45) because an increasing share of the reading's activity is symbolic — Review Conference statements, TPNW meeting declarations — without a corresponding verification or enforcement mechanism that could actually alter weapon-state behavior; the disarmament pressure is increasingly performed rather than operationalized. Accessibility collapse is low-moderate (0.35): the grand_bargain and nonproliferation_primary readings remain fully available and are in fact dominant among the actors with the most material power (weapon states, IAEA). Resistance is high (0.8) because nuclear weapon states and much of the nonproliferation establishment actively and visibly reject the reading's authority claim.
 *
 * DIRECTIONALITY LOGIC:
 *   TPNW-ratifying states and humanitarian NGOs sit near the beneficiary end: they gain normative standing and campaign leverage from the reading without bearing its costs. Civilian nuclear programs in developing states and non-weapon states seeking fuel-cycle technology sit near the target end: the reading's collapse of the peaceful/military distinction converts their compliant, safeguarded activity into presumptively illegitimate capability, and their trapped exit options (dependent on NPT membership and international cooperation for reactor fuel, medical isotopes, and technical assistance) leave them unable to exit the cost. Nuclear weapon states sit outside the constraint's directionality entirely — they hold arbitrage-grade exit (non-ratification of TPNW, non-recognition of the reading's authority) and are excluded rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading was founded on a genuine and still-live problem — weapon-state disarmament stagnation — but the mechanism it has produced (delegitimizing Article IV dual-use cooperation) does not itself advance disarmament; it redistributes cost onto uninvolved civilian programs while leaving weapon-state arsenals untouched. The founding_problem_status is authored 'contested' rather than 'dead' because the underlying disarmament failure is real and corroborated even outside the coalition — but the specific remedy (delegitimizing Article IV) has drifted from problem-solving toward symbolic positioning, which the rising theater_ratio is intended to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_authority_source_dispute,
    'Does authority over the NPT''s own internal bargain (Article IV/VI pairing) properly derive from an external treaty regime (TPNW) that most NPT weapon states have not joined, or only from the NPT''s own text and negotiating history?',
    'Track whether international courts, the UN Security Council, or a supermajority of NPT states parties formally recognize TPNW as authoritative interpretive precedent for NPT obligations; absent such recognition, the authority claim remains a minority normative position asserted by one treaty community.',
    'If TPNW is formally recognized as interpretively authoritative over the NPT, this reading''s classification shifts toward a legitimate emerging rope (coordinating a genuine collective-action problem via a stronger legal instrument); if not, it remains better characterized as sectarian normative pressure that redistributes cost onto compliant non-weapon states without altering weapon-state behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_authority_source_dispute, conceptual, 'Whether TPNW can legitimately supersede the NPT''s own bargain structure as an interpretive authority.').

omega_variable(
    peaceful_military_distinction_validity,
    'Is the collapse of the peaceful-use/military-use distinction in dual-use nuclear technology analytically sound (i.e., does capability alone constitute the relevant harm), or does it conflate technical capability with intent and conduct that IAEA safeguards are specifically designed to distinguish?',
    'Compare proliferation-incident base rates among IAEA-safeguarded civilian programs versus unsafeguarded or non-compliant programs; if safeguarded programs show materially lower diversion risk, the capability-only framing is empirically weaker than the conduct-and-verification framing used by the sibling readings.',
    'If safeguards meaningfully distinguish risk, this reading''s core mechanism (treating capability as illegitimate regardless of compliance) is shown to extract cost from low-risk actors without a corresponding safety benefit, strengthening the snare characterization. If safeguards are shown ineffective at the margin this reading targets, the categorical framing gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peaceful_military_distinction_validity, empirical, 'Whether collapsing the peaceful/military distinction tracks actual proliferation risk or merely capability.').

omega_variable(
    reading_selection_framing,
    'Was the abolitionist framing selected because it is the most textually faithful reading of Article VI''s language (''pursue negotiations in good faith... on a treaty on general and complete disarmament''), or because TPNW''s existence as an external instrument made a categorical reading politically available where it previously was not?',
    'Examine whether abolitionist advocacy predates TPNW''s 2017 adoption in comparable strength, or whether the categorical reading''s prominence tracks TPNW''s diplomatic momentum rather than independent textual analysis of the NPT itself.',
    'If the reading is primarily a product of TPNW''s political availability rather than NPT textual analysis, this weakens the reading''s claim to interpretive authority over the NPT specifically, versus being better understood as an alternative treaty regime competing with, rather than interpreting, the NPT.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_framing, conceptual, 'Whether this reading is genuine NPT interpretation or an externally-motivated competing framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 2017, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t2017, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(npt__tr_t2019, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2019, 0.34).
narrative_ontology:measurement(npt__tr_t2021, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(npt__tr_t2023, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2023, 0.41).
narrative_ontology:measurement(npt__tr_t2025, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2025, 0.44).
narrative_ontology:measurement(npt__tr_t2026, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2026, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t2017, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2017, 0.4).
narrative_ontology:measurement(npt__be_t2019, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2019, 0.46).
narrative_ontology:measurement(npt__be_t2021, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2021, 0.5).
narrative_ontology:measurement(npt__be_t2023, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2023, 0.54).
narrative_ontology:measurement(npt__be_t2025, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2025, 0.57).
narrative_ontology:measurement(npt__be_t2026, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2026, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(npt_article_iv_vi_pairing__abolitionist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the npt_article_iv_vi_pairing kernel, decomposed per the ε-invariance principle: the natural-language label 'the NPT Article IV/VI relationship' covers structurally distinct claims about where interpretive authority lies (NPT's own bargain text vs. external humanitarian/TPNW precedent), whether Article VI is categorical, aspirational, or reciprocal, and whether the peaceful/military distinction survives. Each reading is authored as its own constraint with its own ε, beneficiaries, victims, and classification. This reading (abolitionist) exerts downstream pressure on both siblings: it raises reputational and diplomatic costs for actors who rely on the grand_bargain reading's reciprocal-conditionality logic, and it directly contests the nonproliferation_primary reading's claim that Article VI is non-justiciable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
