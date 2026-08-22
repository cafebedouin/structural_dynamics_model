% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia as Sovereign Islamic Worship Space (Conquest/Waqf Reading)
 *   domain: cultural heritage / sovereignty / religious authority
 *
 * SUMMARY:
 *   This story authors the islamic_sovereignty_reading of the Hagia Sophia
 *   kernel: the claim that the site's legitimacy runs continuously from the
 *   1453 Ottoman conquest through an inalienable Islamic waqf endowment, such
 *   that the 1934 secularization was a suspension of a perpetually valid
 *   title rather than a legitimate settlement. Under this reading the 2020
 *   Council of State ruling and subsequent presidential decree did not create
 *   a new arrangement but corrected an unlawful one. This is one of three
 *   sibling readings of the same kernel (orthodox_restitution_reading,
 *   universal_heritage_reading); each is authored as its own ε-invariant
 *   constraint per DP-001, not as a parameter of a single shared story. The
 *   referent for extractiveness here is the standing arrangement as this
 *   reading's own proponents describe it — a restored sovereign Islamic
 *   worship space — assessed for who actually bears its costs, not for the
 *   reading's rhetorical self-justification.
 *
 * KEY AGENTS:
 *   - akp_political_coalition: agenda_setter (institutional/arbitrage) — engineered the legal reversal and administers the site
 *   - turkish_islamic_constituency: beneficiary (organized/mobile) — gains restored worship access and symbolic vindication
 *   - non_muslim_visitors: payer (powerless/constrained) — bears restricted access
 *   - unesco_heritage_regime: excluded (institutional/trapped) — jurisdiction functionally denied
 *   - secularist_turks: payer (moderate/constrained) — bears ideological defeat with no reversal mechanism
 *   - international_diplomatic_community: observer — registers friction without leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.58).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia as Sovereign Islamic Worship Space (Conquest/Waqf Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural heritage / sovereignty / religious authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '395c9d5a-15af-4b08-a673-569746e65d09').
narrative_ontology:cs_kernel_codification('395c9d5a-15af-4b08-a673-569746e65d09', distributed).
narrative_ontology:cs_authority_grounding('395c9d5a-15af-4b08-a673-569746e65d09', extraction).
narrative_ontology:cs_interpretation_layer_present('395c9d5a-15af-4b08-a673-569746e65d09').
narrative_ontology:cs_reading_relation('395c9d5a-15af-4b08-a673-569746e65d09', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('395c9d5a-15af-4b08-a673-569746e65d09', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('395c9d5a-15af-4b08-a673-569746e65d09', foundational, waqf_endowment_perpetually_binds_the_site).
narrative_ontology:cs_axiom_status(waqf_endowment_perpetually_binds_the_site, holdable).
narrative_ontology:cs_axiom_grounding('395c9d5a-15af-4b08-a673-569746e65d09', waqf_endowment_perpetually_binds_the_site, conventional).
narrative_ontology:cs_axiom('395c9d5a-15af-4b08-a673-569746e65d09', foundational, conquest_establishes_valid_title_transferring_sovereignty).
narrative_ontology:cs_axiom_status(conquest_establishes_valid_title_transferring_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('395c9d5a-15af-4b08-a673-569746e65d09', conquest_establishes_valid_title_transferring_sovereignty, conventional).
narrative_ontology:cs_axiom('395c9d5a-15af-4b08-a673-569746e65d09', secondary, id_1934_secularization_was_ultra_vires_and_void).
narrative_ontology:cs_axiom_status(id_1934_secularization_was_ultra_vires_and_void, holdable).
narrative_ontology:cs_axiom_grounding('395c9d5a-15af-4b08-a673-569746e65d09', id_1934_secularization_was_ultra_vires_and_void, conventional).
narrative_ontology:cs_reference_frame('395c9d5a-15af-4b08-a673-569746e65d09', ottoman_waqf_perpetual_title).
narrative_ontology:cs_drift_state('395c9d5a-15af-4b08-a673-569746e65d09', post_2020_reconversion_decree, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('395c9d5a-15af-4b08-a673-569746e65d09', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engineered the 2020 Council of State ruling reversing the 1934 secularization decree and the subsequent presidential decree reconverting the site to a mosque. Administers the site through the Directorate of Religious Affairs (Diyanet). Collects political legitimacy, religious-conservative electoral consolidation, and a nationalist-Islamist identity signal from controlling the site's status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, beneficiary).

% Gains a restored site of Friday prayer framed as reversing a secularist historical injustice. Experiences the reconversion as vindication of continuous Islamic presence since 1453 and of the waqf's perpetual religious character. Faces no meaningful exit cost; the constraint is pure gain from this seat.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% A diffuse, non-organized symbolic beneficiary: the reconversion is invoked in transnational Islamist discourse as a marker of Muslim restoration and civilizational assertion, without any single body collecting or administering the benefit.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic, beneficiary,
    organized, civilizational, analytical, global).

% Tourists and pilgrims of other faiths now face restricted access during prayer times, mandatory covering of the mosaics during worship hours, and a site experience reframed around Islamic liturgical function rather than open museum access. Can choose not to visit, but the site's unique heritage draw makes that a real cost, not a free alternative.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, constrained, global).

% As custodian of the site's World Heritage listing, UNESCO issued formal concern over the status change but has no enforcement mechanism against a sovereign state's domestic legal reclassification. Its jurisdictional claim over site management is functionally denied by the reconversion, though it retains listing authority on paper.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime, excluded,
    institutional, generational, trapped, global).

% Regard the 1934 museum status as a founding achievement of Kemalist secularism and the reconversion as its symbolic reversal. Bear an ideological and political defeat with no mechanism to reverse the executive/judicial action short of a future change in government; cannot exit the polity's symbolic order.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    moderate, biographical, constrained, national).

% Views the site as the historical seat of Byzantine Christendom and objects to its exclusive Islamic reframing, but holds no legal standing within Turkish domestic law to contest the reclassification. Present in international commentary but absent from the decision-making process entirely.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, greek_orthodox_patriarchate, excluded,
    moderate, generational, trapped, regional).

% States and multilateral bodies register formal objections or statements of concern (Greece, Russia's Orthodox Church, US State Department) without material leverage over Turkish domestic sovereignty. Diplomatic friction is absorbed as a cost of the reconversion but does not alter the underlying constraint.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, international_diplomatic_community, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legally settled answer to who administers the site and under what liturgical rules — ending decades of ambiguous 'secular museum with mosque architecture' status by aligning the site's function with a specific, coherent religious-legal framework (waqf-endowed mosque under continuous Islamic title since 1453).
% TRANSFER_FUNCTION: Moves interpretive and administrative control of the site from the secular museum apparatus (and, symbolically, from competing heritage/ecclesiastical claimants) to the Turkish state's religious authority (Diyanet) and the political coalition that engineered the reversal; moves unrestricted physical/visual access away from non-Muslim visitors during prayer times.
% ABSENT_VOICES: The Ecumenical Patriarchate and international Orthodox Christian communities, who regard the site as their historical cathedral, have no standing in Turkish domestic adjudication and were not party to the Council of State proceeding. UNESCO's technical heritage-management concerns were registered only after the decree, not consulted beforehand.
% DISAPPEARANCE_RATIONALE: If the 2020 reconversion were reversed and museum status restored, the AKP coalition would lose a potent identity-consolidation instrument, Friday prayer congregations would be relocated or ended, the Diyanet's administrative role would dissolve, and international diplomatic friction over the site would substantially subside — real institutional and political arrangements currently depend on this status.
% FOUNDING_PROBLEM: The immediate 2020 problem this reading answers: contested legal ambiguity from 1934's secularization decree, which the reading treats as an illegitimate suspension of a title (conquest + waqf) it holds to have been continuously valid since 1453 and never lawfully extinguishable.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish state and Council of State attest the founding problem (a wrongly suspended perpetual waqf) as live and now corrected. Secularist Turkish legal scholars, the Ecumenical Patriarchate, and UNESCO's advisory bodies — all outside the benefiting coalition — dispute that framing, holding instead that 1934 settled the matter and that the 2020 reversal manufactures a grievance rather than resolving one; no source outside the benefiting parties corroborates the waqf-perpetuity premise as the operative legal fact.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.62) because the reading's operation produces concrete transfers — restricted museum access for non-Muslim visitors, denied jurisdiction for UNESCO, ideological defeat for secularist Turks — layered onto a genuine coordination function (settling decades of contested administrative ambiguity). Suppression (0.58) reflects that the arrangement persists via executive decree and judicial reversal, not open contestation; alternatives (restoring 1934 status, shared administration) are foreclosed by domestic legal finality rather than negotiated away. Theater ratio (0.42) captures that a meaningful share of the site's operation is now performative sovereignty assertion — prayer calls, mosaic-covering rituals during worship — layered atop functioning liturgical use. Accessibility collapse is moderate (0.5): alternatives to the reconversion (litigation, international pressure) still exist procedurally but have no practical purchase against a sovereign domestic ruling. Resistance is real but contained (0.6) — secularist domestic opposition and international diplomatic objection are active but have produced no material reversal.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the arrangement reads as a coordination achievement — resolving 86 years of ambiguous administrative status. From the non-Muslim-visitor and secularist-Turk seats, the identical structure reads as extraction of access and identity: a legal mechanism used to impose a specific religious-national reading on a globally significant site with no meaningful recourse. The engine should compute a materially different per-seat classification for the agenda_setter/beneficiary cluster versus the payer cluster from this same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP coalition sits at the full-beneficiary end: it designed the legal mechanism, administers the outcome, and collects the political consolidation directly (d low). The Turkish Islamic constituency benefits structurally with mobile exit — no cost is imposed on this seat. Non-Muslim visitors and secularist Turks sit toward the full-target end: they bear concrete costs (restricted access; ideological defeat) with constrained exit — they cannot avoid the polity's symbolic order or, for tourists, cannot access the site's unique heritage value elsewhere. UNESCO is trapped structurally: it retains nominal listing authority but no enforcement lever against sovereign reclassification, which is a stronger exclusion than 'constrained.'
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview is essential here: this reading's proponents assert the founding problem (an unlawfully suspended perpetual waqf) is now corrected and therefore resolved — implying no further mandatrophy exists. But founding_problem_status is authored contested precisely because no source outside the benefiting coalition corroborates the waqf-perpetuity premise as controlling law; secularist and international legal opinion holds 1934 settled the matter permanently. This divergence between self-asserted resolution and outside corroboration is the diagnostic the six-questions R5 interview exists to surface — treat the coalition's founding-problem narrative as evidence, not as adjudication.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_perpetuity_vs_state_secularization_authority,
    'Is a waqf endowment under Islamic law genuinely perpetual and unextinguishable by state secularization decree, or does a sovereign state retain plenary authority to secularize religious endowments regardless of their original terms?',
    'Comparative analysis of waqf jurisprudence across historically Ottoman successor states, and examination of whether Turkish constitutional law recognizes any endowment as categorically beyond legislative or executive reach.',
    'If waqf perpetuity is doctrinally sound and state secularization was always ultra vires, this reading''s self-description as ''correction rather than change'' gains legal grounding. If state secularization authority is plenary, the 2020 reversal is better described as a new political act dressed in restorative legal language — raising ε further by removing the coordination-function cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_perpetuity_vs_state_secularization_authority, conceptual, 'Whether waqf perpetuity doctrine constrains sovereign secularization authority.').

omega_variable(
    conquest_title_naturalness_ambiguity,
    'Is title-by-conquest a recognized, stable legal doctrine independent of who currently holds power in Turkey, or is its invocation here a constructed justification that happens to benefit the current political coalition?',
    'Examine whether conquest-derived title claims are applied consistently across other contested Ottoman-era religious sites in Turkey and the broader post-Ottoman world, or whether the doctrine is invoked selectively where it serves current political interests.',
    'Consistent application would support the reading''s claim to a genuine, non-selective legal principle rather than a beneficiary-serving construction; selective application would support classifying the doctrinal claim itself as instrumentally deployed rather than naturally emergent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conquest_title_naturalness_ambiguity, empirical, 'Whether conquest-title doctrine is applied as stable law or selectively invoked.').

omega_variable(
    reading_framing_underdetermination,
    'Is the operative kernel here the physical site''s administrative status, or the deeper legitimacy claim about which historical narrative (conquest/waqf vs. Byzantine founding vs. universal heritage) properly grounds sovereignty over contested sacred space?',
    'Both framings were considered: the administrative-status framing (who runs the site day-to-day) versus the legitimacy-narrative framing (which historical claim is authoritative). This story adopts the legitimacy-narrative framing because the 2020 court ruling explicitly reasoned from historical title, not administrative convenience, and the political stakes documented in commentary track the narrative claim, not logistics.',
    'Under the administrative-status framing alone, this might classify closer to a scaffold (temporary operational arrangement) with lower ε; under the legitimacy-narrative framing adopted here, the tangled_rope classification with moderate-high ε reflects that a durable identity/sovereignty claim, not a transitional administrative fix, is what is actually being contested and enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Alternative framings of the kernel (administrative vs. legitimacy-narrative) and their classification consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 2013, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t2013, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(hagi_tr_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2016, 0.28).
narrative_ontology:measurement(hagi_tr_t2018, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2018, 0.33).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(hagi_tr_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2022, 0.42).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(hagi_be_t2013, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2013, 0.35).
narrative_ontology:measurement(hagi_be_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement(hagi_be_t2018, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(hagi_be_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t2013, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(hagi_su_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(hagi_su_t2018, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2018, 0.45).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(hagi_su_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2022, 0.58).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_restitution_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hagia_sophia_substrate kernel, each authored as a separate ε-invariant story per DP-001. islamic_sovereignty_reading (this file) authors moderate-high ε reflecting concrete access restriction and jurisdictional denial layered on a real administrative-coordination function. orthodox_restitution_reading and universal_heritage_reading author their own independent ε values from their own beneficiary/victim structures and are not averaged with or derived from this file's values. The three form a constraint family linked by shared subject matter, not shared metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
