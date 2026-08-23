% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Binding Rulings with Compliance Obligations
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body (DSB) is the 'crown jewel' of the
 *   multilateral trading system — the only international court with
 *   compulsory jurisdiction and enforcement via authorized retaliation. The
 *   binding referee reading holds that panels and the Appellate Body issue
 *   legally binding rulings grounded in treaty text; member states
 *   surrendered policy discretion in covered domains at accession. This
 *   reading sees the system as a genuine coordination achievement: it
 *   replaced power-based unilateralism with law-based dispute resolution. But
 *   the same structure generates asymmetric extraction: major powers shape
 *   jurisprudence and can delay/evade compliance, while developing countries
 *   face immediate retaliation threats and lack legal capacity. The Appellate
 *   Body's 'judicial economy' and precedent-building (not in the DSU text)
 *   expanded obligations beyond what states negotiated. The 2019 crisis (US
 *   blocking Appellate Body appointments) reveals the contested legitimacy:
 *   the US now argues the system exceeds its mandate (judicial activism
 *   reading), while the EU and developing countries defend binding review
 *   (binding referee reading).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.75).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Rulings with Compliance Obligations").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, 'bdb3d8d8-886e-418d-ab75-a25bf7218e76').
narrative_ontology:cs_kernel_codification('bdb3d8d8-886e-418d-ab75-a25bf7218e76', formalized).
narrative_ontology:cs_authority_grounding('bdb3d8d8-886e-418d-ab75-a25bf7218e76', lineage).
narrative_ontology:cs_interpretation_layer_present('bdb3d8d8-886e-418d-ab75-a25bf7218e76').
narrative_ontology:cs_reading_relation('bdb3d8d8-886e-418d-ab75-a25bf7218e76', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdb3d8d8-886e-418d-ab75-a25bf7218e76', wto_dsb_authority__judicial_activism_reading, forecloses).
narrative_ontology:cs_axiom('bdb3d8d8-886e-418d-ab75-a25bf7218e76', foundational, treaty_based_compliance_obligation).
narrative_ontology:cs_axiom_status(treaty_based_compliance_obligation, holdable).
narrative_ontology:cs_axiom_grounding('bdb3d8d8-886e-418d-ab75-a25bf7218e76', treaty_based_compliance_obligation, conventional).
narrative_ontology:cs_axiom('bdb3d8d8-886e-418d-ab75-a25bf7218e76', foundational, sovereignty_traded_for_market_access).
narrative_ontology:cs_axiom_status(sovereignty_traded_for_market_access, holdable).
narrative_ontology:cs_axiom_grounding('bdb3d8d8-886e-418d-ab75-a25bf7218e76', sovereignty_traded_for_market_access, conventional).
narrative_ontology:cs_axiom('bdb3d8d8-886e-418d-ab75-a25bf7218e76', secondary, automatic_adoption_removes_loser_veto).
narrative_ontology:cs_axiom_status(automatic_adoption_removes_loser_veto, holdable).
narrative_ontology:cs_axiom_grounding('bdb3d8d8-886e-418d-ab75-a25bf7218e76', automatic_adoption_removes_loser_veto, conventional).
narrative_ontology:cs_reference_frame('bdb3d8d8-886e-418d-ab75-a25bf7218e76', marrakesh_1994_dsu_mandate).
narrative_ontology:cs_drift_state('bdb3d8d8-886e-418d-ab75-a25bf7218e76', post_appellate_body_crisis_2019, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bdb3d8d8-886e-418d-ab75-a25bf7218e76', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, major_trading_powers).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_oriented_economies).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, dsb_institutional_bureaucracy).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, developing_country_members).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_regulated_industries).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, policy_sovereignty_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, developing_country_members).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, consumers_and_import_users).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, pacta_sunt_servanda_in_trade).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, rules_based_trade_order).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, legalization_of_international_relations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape DSB jurisprudence through litigation strategy and Appellate Body appointments. Use the system to lock in market access for their exporters while retaining flexibility through non-compliance delays and negotiated settlements. The US, EU, and historically Japan/China are the primary architects and users of the dispute system.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, major_trading_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, major_trading_powers, beneficiary).

% Depend on DSB rulings to maintain open markets for their exports. Korea, Singapore, Chile, and similar economies gain enforceable market access commitments they could not secure bilaterally. They lack the power to shape jurisprudence but benefit disproportionately from the system's existence.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, export_oriented_economies, beneficiary,
    powerful, biographical, constrained, global).

% Use home-country governments to bring DSB cases that protect intellectual property, investment rules, and service market access. They capture gains from enforced discipline on host-state regulation while externalizing compliance costs to states. Not formal parties but the real constituency behind many cases.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, multinational_corporations, beneficiary,
    organized, biographical, mobile, global).

% The WTO Secretariat, panelists, and Appellate Body members who administer the system. Their professional identity and institutional legitimacy depend on the binding nature of rulings. They interpret treaty text to fill gaps, creating de facto precedent. Career incentives align with expanding the system's authority.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, dsb_institutional_bureaucracy, agenda_setter,
    institutional, generational, identity_locked, global).

% Face asymmetric compliance costs: limited legal capacity to defend cases, retaliation threats they cannot credibly counter, and pressure to change domestic laws on IP, services, agriculture. Gain some market access but the net transfer is negative. The ACWL (Advisory Centre on WTO Law) partially mitigates but does not equalize.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, developing_country_members, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, developing_country_members, beneficiary).

% Industries forced to adjust to import competition when DSB rulings require tariff reductions or regulatory changes. They bear concentrated costs (job losses, plant closures) while benefits (consumer prices, export access) are diffuse. Politically organized but structurally disadvantaged in the trade-off.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_regulated_industries, payer,
    moderate, immediate, trapped, national).

% Governments that lose discrete policy choices (food safety standards, cultural subsidies, local content requirements) when DSB rulings find them WTO-inconsistent. The sovereign choice was made at accession but the specific applications are discovered through litigation. Exit means leaving WTO — prohibitive for major economies.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, policy_sovereignty_holders, payer,
    powerful, generational, constrained, national).

% Gain lower prices and greater variety from enforced market opening. Diffuse, unorganized, and unaware of the DSB's role. Their benefit is real but politically invisible — they cannot mobilize to defend the system.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, consumers_and_import_users, beneficiary,
    powerless, immediate, mobile, global).

% NGOs, unions, environmental groups who argue DSB rulings undermine labor standards, environmental protection, and public health regulation. They have no standing in DSB proceedings, amicus briefs are discretionary and rarely accepted. Their objections are structural — the system has no mechanism to internalize non-trade values.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, civil_society_labor_environment, excluded,
    moderate, generational, constrained, global).

% Countries outside the WTO (Iran, Algeria, Lebanon, etc.) who face MFN tariffs and cannot access DSB. They are excluded from the coordination benefits but also from the extraction. Their absence shapes the system's legitimacy claims about universality.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, non_wto_members, excluded,
    moderate, generational, mobile, global).

% Analyze whether DSB jurisprudence constitutes legitimate interpretation or judicial legislation. Split between those who see the Appellate Body as completing an incomplete contract and those who see it as usurping member-state authority. Their debate frames the legitimacy contest but does not drive compliance.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, legal_scholars_international_law, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a centralized, rules-based mechanism for resolving trade disputes that would otherwise escalate into unilateral retaliation and trade wars. Solves the credibility problem: states can commit to open markets knowing violations trigger authorized, proportionate retaliation rather than power-based coercion.
% TRANSFER_FUNCTION: Moves policy autonomy from member states to the DSB: when a measure is found inconsistent, the losing state must change its law, compensate, or face retaliation. The transfer is from domestic regulatory sovereignty (concentrated on regulated industries and policy domains) to export interests and the systemic stability of the trading order. Compliance costs flow from weaker to stronger parties via the retaliation asymmetry.
% ABSENT_VOICES: Civil society, labor, environmental, and consumer advocates who would challenge rulings that prioritize trade liberalization over non-trade values. Non-WTO members who bear MFN discrimination without representation. Future generations locked into treaty commitments made decades ago. These voices are structurally excluded — the DSB has no standing mechanism for them, and member states face no domestic political cost for ignoring them.
% DISAPPEARANCE_RATIONALE: If binding DSB rulings vanished overnight, the WTO would revert to the GATT 1947 system: panel reports adopted only by consensus (loser veto), no authorized retaliation, unilateral 'Section 301'-style measures would proliferate. Trade disputes would be resolved by power, not law. The 300+ rulings creating de facto precedent would lose force. Developing countries would lose their only credible enforcement tool against major powers.
% FOUNDING_PROBLEM: The GATT 1947 dispute system failed because the losing party could block adoption of panel reports, rendering rulings unenforceable. Major powers (especially the US) used unilateral retaliation. The Uruguay Round created the DSB with automatic adoption and authorized retaliation to make rulings binding — trading sovereignty for enforceable market access commitments.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (unilateralism, blockage) is attested by the 1994 Marrakesh Agreement negotiators and GATT-era diplomats outside the current beneficiary set. However, developing countries and critical scholars contest whether the solution created new problems: judicialization beyond the mandate, asymmetry in retaliation capacity, and the Appellate Body's precedent-creating function which was not in the original mandate. The 2019 Appellate Body crisis (US blocking appointments) demonstrates the founding problem is contested — the US argues the solution became the problem.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects real compliance costs: changing domestic laws, losing policy space, facing retaliation. But it is not pure extraction — the system delivers measurable trade stability and market access (coordination function). Suppression (0.75) is high because the only exit is leaving the WTO (prohibitive for trading nations), and retaliation authorization makes non-compliance costly. Theater (0.38) captures the growing gap between the DSU text and Appellate Body practice: 'judicial economy' avoids hard questions, 'sequencing' avoids ruling on core claims, compliance panels become ritualized. The rise in all three metrics 1995-2020 tracks the Appellate Body's expansion of its own authority; the slight dip post-2020 reflects the paralyzed Appellate Body and shift to MPIA (Multi-Party Interim Appeal Arrangement) — a partial reversion toward advisory coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the major-power agenda-setter seat, the DSB is a rope: they built it, they use it, it coordinates their trade relations. From the developing-country payer seat, it is a snare: they joined for market access but got binding obligations they didn't negotiate, enforced by retaliation they cannot match. From the DSB bureaucracy seat, it is a mountain: treaty law binds, precedent accumulates, the system's legitimacy is self-evident. The engine computes these divergences from the structural data — the claimed_type (tangled_rope) is this author's structural judgment that BOTH coordination and asymmetric extraction are real and inseparable in the current design.
 *
 * DIRECTIONALITY LOGIC:
 *   Major trading powers sit near d=0.2 (beneficiary): they write the rules, win most cases, can absorb retaliation. Developing countries sit near d=0.8 (target): they lose disproportionately, lack retaliation credibility, face compliance demands on IP/agriculture/services they didn't anticipate. The DSB bureaucracy sits at d=0.1 (institutional beneficiary): its existence depends on binding authority. Domestic regulated industries are trapped payers (d=0.9): they bear concentrated costs with no voice in Geneva. Consumers are diffuse beneficiaries (d=0.3) but unorganized. The directionality derivation from beneficiary/victim + exit captures this gradient — major powers have arbitrage exit (can go bilateral), developing countries are constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (GATT blockage/unilateralism) was live in 1995. By 2019, the Appellate Body's interpretive expansion created a new problem: judicial legislation without democratic authorization. The US blockade is a mandatrophy response — the system's mandate (dispute resolution per DSU) has been overtaken by its practice (common law court creating precedent). The binding referee reading insists the mandate is still live; the judicial activism reading says it is dead. The contested status reflects this genuine disagreement. The system persists because no alternative exists, not because the mandate is fulfilled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the WTO DSB''s authority a single contested kernel with multiple readings, or are these structurally distinct constraints incorrectly labeled as one system?',
    'Decompose into separate constraint stories per ε-invariance: if advisory_coordination and binding_referee have different ε, beneficiaries, victims, and enforcement structures, they are different constraints linked by network.affects_constraints. The test: does changing the observable (e.g., measuring compliance rates vs. measuring precedent-creation) change ε for the same constraint?',
    'If single kernel: readings are interpretive frames on one constraint; classification divergence is perspectival. If multiple constraints: each has its own ε and type; the ''contest'' is category error. This determination drives whether mandatrophy analysis applies to one constraint or a family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the three declared readings map to one constraint or a constraint family.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the DSB''s coordination function (dispute resolution, market access credibility) genuine and separable from its extraction function (asymmetric compliance burdens, precedent expansion), or is coordination the cover for extraction?',
    'Counterfactual: if retaliation authorization were removed but panel reports remained advisory, would states still comply at current rates? If yes, coordination is genuine; if compliance collapses, enforcement IS the function. Also: measure compliance costs for developing vs developed countries on identical obligation types.',
    'If coordination is genuine and separable → tangled_rope (current claim). If coordination is cover → snare. If extraction is negligible → rope. The 2019 crisis (system functions without Appellate Body via MPIA) suggests coordination survives without the extractive precedent function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable or fused.').

omega_variable(
    retaliation_asymmetry_as_extraction,
    'Does the authorized retaliation mechanism function as a coordination enforcement tool or as an extraction amplifier that transfers value from weak to strong members?',
    'Analyze retaliation cases: frequency, magnitude, and outcome by complainant/respondent power. If major powers retaliate and win concessions while developing countries authorize but cannot implement retaliation, the mechanism is extractive. Compare to GATT 1947 where retaliation was unilateral and power-based — is the DSB version structurally different?',
    'If retaliation is extractive amplifier, the constraint''s extraction is higher than the treaty text suggests. This would push classification toward snare for developing-country seats. If it is genuine enforcement, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_asymmetry_as_extraction, empirical, 'Whether the enforcement mechanism itself generates asymmetric extraction beyond the underlying obligations.').

omega_variable(
    appellate_body_precedent_legitimacy,
    'Does the Appellate Body''s de facto precedent system (stare decisis in practice) represent legitimate treaty interpretation or unauthorized judicial legislation?',
    'Track specific rulings where the AB created obligations not in the covered agreements (e.g., ''zeroing'' methodology, ''public body'' definition in subsidies, ''like product'' expansion). Code each as: textual interpretation / gap-filling / novel obligation. Survey state practice: do states treat AB reports as binding precedent in subsequent disputes?',
    'If AB creates novel obligations → judicial_activism reading gains structural support; binding_referee reading must either accept expansion (becoming judicial_activism) or foreclose it (requiring AB reform). The 2019 crisis is this conflict materialized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_body_precedent_legitimacy, conceptual, 'Whether the Appellate Body''s interpretive practice stays within or exceeds the treaty mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t2000, wto_dsb_authority__binding_referee_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t2005, wto_dsb_authority__binding_referee_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t2010, wto_dsb_authority__binding_referee_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t2015, wto_dsb_authority__binding_referee_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t2020, wto_dsb_authority__binding_referee_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t2024, wto_dsb_authority__binding_referee_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(wto_dsb_binding_referee_be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t2000, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t2010, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t2015, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t2020, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2020, 0.69).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t2024, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_binding_referee_su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t2000, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t2010, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t2015, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t2020, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t2024, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__binding_referee_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_mfn_principle).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_national_treatment).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, trips_agreement_enforcement).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, scm_agreement_subsidies).

% DUAL FORMULATION NOTE:
% This constraint (binding_referee_reading) and its siblings (advisory_coordination_reading, judicial_activism_reading) form the wto_dsb_authority constraint family. They share the kernel (DSB authority structure) but instantiate different constraints with different ε: advisory_coordination has low ε (coordination only), binding_referee has moderate-high ε (coordination + asymmetric extraction), judicial_activism has high ε (extraction via ultra vires precedent). The binding_referee reading is upstream: its claim that rulings are binding treaty interpretation is cited as evidence by the judicial_activism reading that the system has exceeded its mandate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, institutional, 0.15).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, powerful, 0.25).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, moderate, 0.7).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, powerless, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
