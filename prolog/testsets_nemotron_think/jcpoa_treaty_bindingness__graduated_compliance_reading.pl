% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Graduated Compliance Framework
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA graduated compliance reading frames the agreement as a scaled
 *   reciprocal commitment where enforcement intensity tracks verified
 *   violation severity. Unlike the binding multilateral reading (which treats
 *   any breach as material breach requiring consensus dissolution) or the
 *   transactional provisional reading (which treats the deal as voidable at
 *   will), this reading instantiates a proportionality logic: Iranian
 *   enrichment increases beyond limits trigger calibrated sanctions
 *   reimposition, not automatic snapback; dispute resolution prioritizes
 *   technical de-escalation through the Joint Commission's working groups
 *   before political escalation. The constraint extracts compliance from Iran
 *   through the threat of graduated sanctions relief withdrawal, while
 *   coordinating non-proliferation verification through the IAEA's
 *   unprecedented access regime. Beneficiaries are the diplomatic
 *   constituency for calibrated reciprocity and economic actors who gained
 *   reversible market access. The claim/metric independence is maintained:
 *   the reading claims tangled_rope (coordination + extraction + enforcement)
 *   while the metrics describe a moderate-extraction, actively enforced
 *   system with genuine verification coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Framework").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '6401dbd4-abf0-43bb-977f-c57a37997f6b').
narrative_ontology:cs_kernel_codification('6401dbd4-abf0-43bb-977f-c57a37997f6b', formalized).
narrative_ontology:cs_authority_grounding('6401dbd4-abf0-43bb-977f-c57a37997f6b', practice).
narrative_ontology:cs_interpretation_layer_present('6401dbd4-abf0-43bb-977f-c57a37997f6b').
narrative_ontology:cs_reading_relation('6401dbd4-abf0-43bb-977f-c57a37997f6b', jcpoa_treaty_bindingness__binding_multilateral_reading, influences).
narrative_ontology:cs_reading_relation('6401dbd4-abf0-43bb-977f-c57a37997f6b', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('6401dbd4-abf0-43bb-977f-c57a37997f6b', foundational, proportional_reciprocity_principle).
narrative_ontology:cs_axiom_status(proportional_reciprocity_principle, holdable).
narrative_ontology:cs_axiom_grounding('6401dbd4-abf0-43bb-977f-c57a37997f6b', proportional_reciprocity_principle, conventional).
narrative_ontology:cs_axiom('6401dbd4-abf0-43bb-977f-c57a37997f6b', foundational, graduated_enforcement_legitimacy).
narrative_ontology:cs_axiom_status(graduated_enforcement_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6401dbd4-abf0-43bb-977f-c57a37997f6b', graduated_enforcement_legitimacy, conventional).
narrative_ontology:cs_axiom('6401dbd4-abf0-43bb-977f-c57a37997f6b', secondary, technical_de_escalation_priority).
narrative_ontology:cs_axiom_status(technical_de_escalation_priority, holdable).
narrative_ontology:cs_axiom_grounding('6401dbd4-abf0-43bb-977f-c57a37997f6b', technical_de_escalation_priority, instrumental).
narrative_ontology:cs_reference_frame('6401dbd4-abf0-43bb-977f-c57a37997f6b', reciprocal_compliance_framework).
narrative_ontology:cs_drift_state('6401dbd4-abf0-43bb-977f-c57a37997f6b', post_us_withdrawal_2018, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6401dbd4-abf0-43bb-977f-c57a37997f6b', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_engagement).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_regime).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_program).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_factions_iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_factions_us_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1_e3_plus_3).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, proportional_reciprocity_in_arms_control).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, graduated_enforcement_legitimacy).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, diplomatic_de_escalation_over_legal_closure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accepts intrusive monitoring, enrichment limits, and facility modifications in exchange for phased sanctions relief. Bears direct compliance costs (centrifuge reductions, Fordow conversion, Arak redesign) and opportunity costs of foregone nuclear capacity. Exit is constrained by international isolation risk and domestic political investment in the deal.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_program, payer,
    institutional, generational, constrained, national).

% Negotiated and administers the framework through the Joint Commission. Controls sanctions architecture and snapback mechanism. Benefits from non-proliferation verification and regional stability. Can pivot to alternative pressure tracks (unilateral sanctions, military threats) if the framework collapses.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1_e3_plus_3, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1_e3_plus_3, beneficiary).

% Gains unprecedented inspection authority (Additional Protocol, T-Section, complementary accesses) that becomes a new institutional standard. Its epistemic authority is vindicated by the framework. Not a direct payer; operates as the technical arbiter of compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_regime, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_regime, observer).

% Champions the deal as proof that adversarial nuclear disputes can be resolved through calibrated reciprocity. Uses the framework's survival as evidence for similar approaches elsewhere (North Korea, future proliferation cases). Loses political capital when the framework degrades but retains intellectual credibility.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% European, Asian, and Iranian firms that entered trade, investment, and energy contracts during relief periods. Gains are real but reversible — contracts include force majeure clauses tied to sanctions snapback. Their engagement creates a constituency for maintenance but not for deepening.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_engagement, beneficiary,
    organized, biographical, mobile, global).

% Opposed the deal from inception; views any nuclear constraint as sovereignty violation. Bears political cost when the framework succeeds (marginalization) and gains when it fails (validation). Cannot exit the identity commitment to nuclear maximalism without factional collapse.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_factions_iran, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_factions_iran, excluded).

% Views the framework as legitimizing an adversary's nuclear threshold. Bears political cost of restraining military options during compliance periods. Gains when violations trigger snapback or collapse. Identity-locked to rejectionist posture — exit requires coalition fracture.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_factions_us_israel, payer,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_factions_us_israel, excluded).

% Excluded from negotiations; views any Iranian sanctions relief as direct threat regardless of nuclear constraints. Would demand equivalent enrichment rights or security guarantees if present. Structural exclusion is a design feature of the P5+1 format.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_rivals_saudi_uae, excluded,
    institutional, generational, trapped, regional).

% Tracks compliance disputes through the Joint Commission's dispute resolution mechanism. Sees the graduated logic in operation: technical disagreements resolved at working level, political disagreements escalated to ministerial level, with proportional remedies calibrated to breach severity.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, joint_commission_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a verified, time-bound constraint on Iran's nuclear program in exchange for phased sanctions relief, solving the coordination problem of mutual distrust through third-party verification (IAEA) and a calibrated dispute resolution ladder that prevents automatic escalation.
% TRANSFER_FUNCTION: Moves sanctions relief (financial access, oil export normalization, investment permissions) from P5+1 to Iran in verified tranches, contingent on IAEA-certified compliance steps. Moves inspection access and nuclear constraints from Iran to the international community. The transfer is bidirectional, sequenced, and reversible at each stage.
% ABSENT_VOICES: Regional rivals (Saudi Arabia, UAE, Israel) were structurally excluded from the negotiating table and would have demanded either equivalent enrichment rights or binding security guarantees. Iranian reformist civil society was excluded; their demand for broader political opening was traded for nuclear specificity. Both absences are structural — the P5+1 format required a closed deal.
% DISAPPEARANCE_RATIONALE: If the graduated framework vanished overnight, Iran would immediately resume unrestricted enrichment (no verification, no limits), P5+1 would trigger full snapback sanctions (or military pressure), regional rivals would pursue hedging programs (Saudi enrichment demand), and the IAEA would lose its most intrusive verification mandate. The nuclear order in the Middle East would reorganize around unrestrained competition.
% FOUNDING_PROBLEM: The 2002-2015 impasse: Iran's advancing nuclear capacity (19,000+ centrifuges, 20% enrichment, Arak plutonium pathway) met escalating sanctions and military threats. Neither side could impose its preferred outcome unilaterally. The founding problem was a credible, verifiable pathway to constrain the program without war or total capitulation.
% FOUNDING_PROBLEM_CORROBORATION: The Obama administration, EU3, and IAEA attest the problem was live and the framework solved it (2015-2018 compliance). The Trump administration, Israeli government, and Iranian hardliners attest the problem was misdiagnosed — Iran's regional behavior and sunset provisions meant the framework deferred rather than solved the threat. Independent arms control experts (IISS, Carnegie, Arms Control Association) corroborate the technical problem was real and substantially addressed during full compliance, but dispute whether the graduation logic survives political polarization.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that sanctions relief is valuable but conditional and reversible — Iran trades nuclear capacity for economic access, but the transfer is sequenced and verifiable, not a one-way concession. Suppression (0.55) captures the snapback mechanism's deterrent force without the automaticity of Chapter VII; the Joint Commission's dispute resolution absorbs friction before suppression activates. Theater ratio (0.28) is low because IAEA verification is operationally real, not performative — the 2015-2018 compliance period demonstrated functional monitoring. Accessibility collapse (0.48) is moderate: alternatives (military strike, maximum pressure, breakout) exist but carry catastrophic risk. Resistance (0.52) reflects sustained political opposition from rejectionist factions in Tehran, Washington, Jerusalem, and Riyadh. The measurement series uses a shared annual grid (2015, 2017, 2018, 2019, 2021, 2023, 2025) capturing the compliance period, US withdrawal, Iranian countermeasures, and negotiation attempts.
 *
 * PERSPECTIVAL GAP:
 *   From Iran's seat, the framework is a snare — maximum constraint for minimum, reversible relief. From P5+1's seat, it is a rope — genuine coordination achieving non-proliferation at acceptable cost. From IAEA's seat, it is a mountain — verification standards that should persist regardless of political fate. From hardliners' seats, it is a piton — theatrical diplomacy masking unchanged enmity. The engine computes these divergences from the structural data; the authored claim (tangled_rope) names the hybrid coordination-extraction logic that the graduation mechanism instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran is the primary target (d near 0.8-0.9): bears compliance costs, constrained exit (breakout invites attack, withdrawal invites snapback), identity-locked hardliners amplify extraction perception. P5+1 are agenda-setters with arbitrage exit (d near 0.1-0.2): they designed the graduation ladder, control snapback, can pivot to alternatives. IAEA sits near analytical (d ~0.5): gains verification authority but bears inspection burden. Pragmatic diplomacy advocates and economic actors are beneficiaries with mobile exit (d ~0.2-0.3): gain from framework survival but can redirect advocacy/investment. Hardliners on all sides are identity-locked payers/excluded (d ~0.7-0.9): extraction perception is fused to ideological commitment. Regional rivals are trapped excluded (d ~0.9): bear security externalities with no voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible verified constraint without war) was substantially solved during 2015-2018 full compliance. The mandate has not atrophied — the nuclear constraint remains live and the verification regime sets a new standard. However, the graduation logic has degraded: US withdrawal converted calibrated reciprocity into unilateral maximum pressure; Iranian countermeasures converted proportional response into deliberate escalation ladder. The constraint persists as a zombie framework — the Joint Commission meets, IAEA verifies, but the proportionality logic is suspended. This is not mandatrophy (function gone, form remains) but mandate suspension (function contested, form preserves optionality for revival). The theater ratio rise in 2018-2019 reflects this suspension: dispute resolution continues but without credible graduated remedies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_mechanism_necessity,
    'Is the graduated enforcement ladder structurally necessary for non-proliferation coordination, or does it function as extraction cover allowing P5+1 to modulate pressure while Iran bears fixed compliance costs?',
    'Counterfactual analysis: compare compliance durability and verification effectiveness under graduated vs. binary (compliance/non-compliance) frameworks in other arms control regimes (INF, New START, NPT safeguards).',
    'If graduation is necessary coordination, the constraint is genuine tangled_rope. If graduation is extraction modulation, the constraint trends toward snare — Iran pays fixed costs for variable, politically contingent relief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_mechanism_necessity, empirical, 'Whether proportionality logic is coordination infrastructure or extraction control').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the graduated_compliance_reading foreclose, coexist with, or influence the binding_multilateral_reading and transactional_provisional_reading in actual diplomatic practice?',
    'Trace Joint Commission dispute resolution records (2015-2025): when parties invoked ''material breach'' vs. ''significant non-performance'' vs. ''proportional response'' language; correlate with remedy selection.',
    'If graduated_compliance forecloses binding_multilateral in practice (parties never treat breaches as material), the kernel has structurally shifted. If all three coexist as live rhetorical positions, the kernel remains genuinely contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between this reading and its sibling readings in the contested kernel').

omega_variable(
    sunset_provisions_extraction,
    'Do the JCPOA''s sunset provisions (expiring enrichment limits, centrifuge R&D restrictions) function as graduated de-escalation coordination or as extracted concessions that Iran can exploit after fixed compliance investment?',
    'Analyze Iranian behavior at sunset thresholds (Year 10, Year 15): does Iran treat expirations as coordinated normalization or as earned rights to expand? Correlate with P5+1 diplomatic positioning on extension.',
    'If sunsets are coordination, the framework is scaffold-like (transitional). If sunsets are extracted concessions, the framework extracts front-loaded compliance for back-loaded latitude — extraction masked as graduation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_provisions_extraction, empirical, 'Whether sunset provisions are transitional coordination or extracted future latitude').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_graduated_tr_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(jcpoa_graduated_tr_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2017, 0.18).
narrative_ontology:measurement(jcpoa_graduated_tr_t2018, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(jcpoa_graduated_tr_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2019, 0.42).
narrative_ontology:measurement(jcpoa_graduated_tr_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(jcpoa_graduated_tr_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2023, 0.25).
narrative_ontology:measurement(jcpoa_graduated_tr_t2025, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(jcpoa_graduated_be_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(jcpoa_graduated_be_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2017, 0.32).
narrative_ontology:measurement(jcpoa_graduated_be_t2018, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(jcpoa_graduated_be_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(jcpoa_graduated_be_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2021, 0.45).
narrative_ontology:measurement(jcpoa_graduated_be_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2023, 0.4).
narrative_ontology:measurement(jcpoa_graduated_be_t2025, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_graduated_su_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(jcpoa_graduated_su_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2017, 0.42).
narrative_ontology:measurement(jcpoa_graduated_su_t2018, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement(jcpoa_graduated_su_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2019, 0.72).
narrative_ontology:measurement(jcpoa_graduated_su_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(jcpoa_graduated_su_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2023, 0.52).
narrative_ontology:measurement(jcpoa_graduated_su_t2025, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, npt_safeguards_standard).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_maximum_pressure_campaign).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, middle_east_nuclear_free_zone_proposals).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, north_korea_denuclearization_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jcpoa_treaty_bindingness kernel. The binding_multilateral_reading (constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading) treats the JCPOA as a classical binding treaty where any material breach triggers collective dissolution. The transactional_provisional_reading (constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading) treats it as a revocable political deal. This reading's ε (0.42) is lower than the transactional reading's (extraction-heavy, ~0.6+) but higher than the binding reading's (coordination-heavy, ~0.25). The three form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, institutional, 0.15).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
