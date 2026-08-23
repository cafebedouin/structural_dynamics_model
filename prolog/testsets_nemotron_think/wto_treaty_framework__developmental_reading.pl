% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework — Developmental Reading (Permanent S&D, Technology Transfer Core)
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   The WTO treaty framework is a contested kernel. The developmental reading
 *   instantiates a constraint where Special and Differential Treatment (S&D)
 *   provisions are permanent structural accommodations recognizing asymmetric
 *   starting conditions, technology transfer (TRIPS Art 66.2) is a core
 *   enforceable obligation, and policy space for development (tariffs,
 *   subsidies, compulsory licensing) is an equal-status treaty commitment —
 *   not a concession. This reading has moderate extractiveness (0.42) because
 *   it constrains multinational IP rights and developed country technology
 *   firms while coordinating a multilateral trade system. The
 *   market_access_reading (sibling) frames the same treaty as symmetric
 *   universal liberalization with S&D as temporary exceptions.
 *
 * KEY AGENTS:
 *   - global_south_states: Primary beneficiary (organized/constrained) — invokes S&D, defends policy space
 *   - multinational_ip_rights_holders: Primary payer (powerful/arbitrage) — bears compulsory licensing, tech transfer
 *   - developed_country_governments: Agenda setter (institutional/mobile) — negotiates, interprets, pressures for graduation
 *   - least_developed_countries: Beneficiary (powerless/trapped) — depends entirely on structural accommodations
 *   - wto_dispute_settlement_body: Observer (analytical/analytical) — adjudicates reading conflicts
 *   - civil_society_development_ngos: Excluded (moderate/constrained) — advocates, monitors, no standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.42).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.38).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework — Developmental Reading (Permanent S&D, Technology Transfer Core)").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, '9fca800a-06f5-48ef-b6ee-efd9c3df3a98').
narrative_ontology:cs_kernel_codification('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', formalized).
narrative_ontology:cs_authority_grounding('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', lineage).
narrative_ontology:cs_interpretation_layer_present('9fca800a-06f5-48ef-b6ee-efd9c3df3a98').
narrative_ontology:cs_reading_relation('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', wto_treaty_framework__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', foundational, development_as_equal_status_treaty_commitment).
narrative_ontology:cs_axiom_status(development_as_equal_status_treaty_commitment, holdable).
narrative_ontology:cs_axiom_grounding('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', development_as_equal_status_treaty_commitment, deontological).
narrative_ontology:cs_axiom('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', foundational, sd_provisions_permanent_not_transitional).
narrative_ontology:cs_axiom_status(sd_provisions_permanent_not_transitional, holdable).
narrative_ontology:cs_axiom_grounding('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', sd_provisions_permanent_not_transitional, conventional).
narrative_ontology:cs_axiom('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', foundational, technology_transfer_as_core_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_as_core_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', technology_transfer_as_core_obligation, instrumental).
narrative_ontology:cs_reference_frame('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', wto_founding_development_compromise).
narrative_ontology:cs_drift_state('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', contemporary_implementation_gap, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9fca800a-06f5-48ef-b6ee-efd9c3df3a98', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, global_south_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, developing_country_domestic_industries).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_country_technology_firms).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, global_pharmaceutical_corporations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, developed_country_technology_firms).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, developed_country_governments).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developing_country_domestic_industries).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, asymmetric_starting_conditions_require_permanent_accommodation).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, technology_transfer_is_development_prerequisite).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, policy_space_is_sovereign_right_not_concession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and invoke S&D provisions to maintain tariff flexibility, subsidy space for infant industries, and compulsory licensing authority. Depend on the treaty's developmental commitments to pursue industrial policy. Exit from the treaty system would mean loss of market access and dispute settlement protection, but staying requires constant coalition-building to defend developmental provisions against erosion.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_south_states, beneficiary,
    organized, generational, constrained, global).

% Gain protected space to develop behind tariff walls and access compulsory licenses for essential technologies. Also bear costs of compliance with WTO rules that limit certain subsidies and require IP enforcement. Cannot individually exit the national policy framework; their situation is mediated through state policy choices.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developing_country_domestic_industries, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, developing_country_domestic_industries, payer).

% Receive the most extensive S&D flexibilities (longer transition periods, exemption from many obligations). Have minimal negotiating leverage and depend entirely on the treaty's structural accommodations. Exit is not viable — they lack capacity for bilateral alternatives and would face worse terms outside the multilateral system.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_countries, beneficiary,
    powerless, generational, trapped, global).

% Face compulsory licensing, technology transfer requirements, and limits on patent enforcement in developing countries. Use investor-state dispute settlement, bilateral pressure, and TRIPS-plus agreements to constrain the developmental reading's operation. Can shift R&D investment, pricing, and market entry decisions across jurisdictions — high exit mobility at firm level.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders, payer,
    powerful, biographical, arbitrage, global).

% Bear technology transfer obligations and compulsory licensing risks in developing markets. Simultaneously benefit from TRIPS minimum standards, dispute settlement enforcement, and market access commitments. Their dual position reflects the treaty's hybrid character: they pay into the developmental accommodation while collecting from the market access framework.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_country_technology_firms, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, developed_country_technology_firms, beneficiary).

% Specifically targeted by public health flexibilities (compulsory licensing for medicines, paragraph 6 system). Lobby aggressively for TRIPS-plus bilateral agreements and use regulatory capture in home countries to limit developing country use of flexibilities. High mobility — can withdraw from specific markets, tier pricing, or shift R&D portfolios.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_pharmaceutical_corporations, payer,
    powerful, biographical, arbitrage, global).

% Negotiate and interpret the treaty; historically shaped the Uruguay Round bargain that embedded developmental provisions. Now press for 'graduation' and 'differentiation' to limit S&D scope. Benefit from market access commitments they secured. Can pursue plurilateral agreements, bilateral FTAs, or reform initiatives — high institutional exit options.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_country_governments, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, developed_country_governments, beneficiary).

% Adjudicates conflicts between developmental and market access readings. Panels and Appellate Body (when functional) interpret S&D provisions, technology transfer obligations (TRIPS Art 66.2), and public health flexibilities. Their jurisprudence shapes the operational boundary between the two readings. No stake in outcome beyond institutional legitimacy.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_dispute_settlement_body, observer,
    analytical, generational, analytical, global).

% Advocate for stronger developmental readings, monitor implementation, and mobilize political pressure. Formally excluded from WTO decision-making (no standing in disputes, limited observer status). Their exclusion is structural — the treaty's state-centric design has no mechanism for non-state participation in adjudication or negotiation.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, civil_society_development_ngos, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a multilateral trade system with predictable rules, dispute settlement, and market access commitments that reduce transaction costs and prevent trade wars. The developmental reading adds a coordination layer: permanent structural accommodation for asymmetric development levels, enabling Global South states to pursue industrial policy without facing immediate retaliation.
% TRANSFER_FUNCTION: Moves policy autonomy and technology access from developed country IP holders and technology firms to developing country states and domestic industries. Concretely: tariff revenue retained, compulsory licensing enables generic production, technology transfer obligations (TRIPS 66.2) create flows of know-how, subsidy space permits industrial support. The transfer is from concentrated high-power IP holders to diffuse lower-power developmental beneficiaries.
% ABSENT_VOICES: Workers in developing country export sectors (would object if developmental protections raise input costs), consumers in developed countries (would object if IP enforcement weakens innovation incentives), future generations in Global South (would object if technology transfer obligations are not fulfilled). These voices are structurally absent — the treaty has no representation mechanism for them.
% DISAPPEARANCE_RATIONALE: If the developmental reading vanished overnight, the treaty would revert to pure market access logic: S&D provisions would become temporary transitionals, technology transfer obligations would be unenforceable aspirational language, and Global South policy space would collapse to developed-country-defined 'flexibilities.' Developing countries would lose legal basis for industrial policy, compulsory licensing, and tariff autonomy — prompting either mass withdrawal from the system or unilateral non-compliance.
% FOUNDING_PROBLEM: The post-WWII trade system (GATT) was designed by and for industrialized economies. Newly independent developing countries faced a system that locked in their commodity-export, technology-import position. The developmental reading emerged from the 1960s-70s NIEO demand and the 1979 Tokyo Round 'enabling clause' — built to solve the problem of asymmetric starting conditions making symmetric obligations extractive.
% FOUNDING_PROBLEM_CORROBORATION: UNCTAD's founding documents and the 1974 Declaration on the Establishment of a New International Economic Order corroborate the asymmetric starting conditions problem from outside the WTO beneficiary set. The 2015 Addis Ababa Action Agenda (UN, not WTO) reaffirms policy space for development. WTO Secretariat's own 2021 World Trade Report acknowledges 'development divide' persists. No major developed country government formally concedes the founding problem is live — they characterize it as 'addressed' by existing flexibilities.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__developmental_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).
:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the constraint transfers policy autonomy and technology access from powerful IP holders to organized but constrained Global South states. Suppression (0.38) reflects active enforcement — dispute settlement, TRIPS-plus bilateral pressure, and Appellate Body crisis all function to suppress the developmental reading's full operation. Theater ratio (0.28) is significant: developed countries perform commitment to development (Aid for Trade, capacity building) while structurally resisting technology transfer enforcement and S&D permanence. Accessibility collapse (0.45) is moderate — alternatives (bilateral FTAs, unilateral preferences) exist but are worse for Global South. Resistance (0.52) is high: Global South coalitions (G20, G33, African Group) actively defend the reading; developed countries actively resist its full implementation.
 *
 * PERSPECTIVAL GAP:
 *   From the Global South state seat, the constraint is a Rope — genuine coordination with fair accommodation. From the multinational IP holder seat, it is a Snare — extraction via compulsory licensing and forced technology transfer. From the developed country government seat, it is a Tangled Rope — coordination they negotiate but extraction they resist. The engine computes this divergence from the structural data: same constraint, different directionalities (d) produce different effective extractions (χ) and thus different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South states are structural beneficiaries (d ~ 0.15): they collect policy space, tariff revenue, and technology access. Multinational IP holders are structural targets (d ~ 0.85): they bear compulsory licensing, technology transfer obligations, and limits on patent enforcement. Developed country governments are near-symmetric (d ~ 0.45): they set the agenda and benefit from market access but pay in developmental accommodations. LDCs are identity-locked beneficiaries (d ~ 0.10): trapped in the system, dependent on its accommodations, no viable exit. The directionality derivation from beneficiary/victim declarations + exit options + power produces this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The developmental reading prevents mislabeling coordination as pure extraction by naming the founding problem (asymmetric starting conditions) as live and corroborated externally (UNCTAD, NIEO, Addis Ababa). The market_access_reading would call the same provisions 'extractive exceptions' — but the founding problem's persistence (corroborated by UN, not WTO beneficiaries) means the accommodation is not mandatrophy; it remains functional. The treaty's mandate (development + trade) has not atrophied — the developmental pillar is contested but not obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_enforceability,
    'Is TRIPS Article 66.2 (technology transfer to LDCs) an enforceable obligation or a best-effort commitment?',
    'WTO dispute settlement ruling on a complaint by an LDC against a developed country for failure to provide incentives for technology transfer. No such case has been brought; a ruling would clarify legal status.',
    'If enforceable, extraction from developed country firms increases substantially (χ rises for payer seats) and the developmental reading''s coordination function gains teeth. If best-effort, the reading''s extraction is largely performative — theater ratio would be higher than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_transfer_enforceability, empirical, 'Legal enforceability of technology transfer obligation under TRIPS 66.2').

omega_variable(
    sd_permanence_vs_transition,
    'Are S&D provisions structurally permanent accommodations or temporally bounded transitions?',
    'WTO Ministerial Conference decision on ''differentiation'' or ''graduation'' criteria. If criteria are adopted that move countries out of S&D eligibility based on development metrics, the transitional interpretation gains structural force.',
    'If permanent, the developmental reading''s coordination function is stable (low theater). If transitional, the reading''s beneficiaries face a closing window — extraction from them increases over time as they ''graduate'', making the constraint more snare-like for middle-income developing countries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_permanence_vs_transition, conceptual, 'Structural status of S&D provisions: permanent accommodation vs temporary transition').

omega_variable(
    committer_framing_disagreement,
    'Does the developmental reading foreclose the market_access_reading, or do they coexist as competing interpretations within the same treaty framework?',
    'Analyze whether any single WTO member can simultaneously invoke full S&D permanence AND full market access symmetry in its own policy positions without contradiction. Track voting coalitions in Ministerial Conferences.',
    'If forecloses, the kernel is bifurcated — members must choose one reading, making the treaty structurally unstable. If coexists_with (as authored), the treaty sustains productive ambiguity. If influences, the developmental reading''s persistence creates pressure on the market_access_reading to concede limited S&D space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_disagreement, conceptual, 'Structural relationship between developmental and market access readings of the WTO kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.38) primarily structural (dispute settlement, bilateral pressure) or internalized (Global South states self-censoring policy space due to fear of retaliation)?',
    'Compare policy autonomy utilization rates in countries with similar development levels but different exposure to dispute settlement risk (e.g., non-WTO members vs WTO members). If non-members use more policy space, suppression is structural; if similar, internalized component is significant.',
    'If internalized suppression is substantial, effective suppression is higher than measured — the constraint''s extraction is amplified by anticipation. This would increase χ for Global South states and shift per-seat classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in developmental reading operation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dev_reading_tr_t1995, wto_treaty_framework__developmental_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(wto_dev_reading_tr_t2001, wto_treaty_framework__developmental_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(wto_dev_reading_tr_t2005, wto_treaty_framework__developmental_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(wto_dev_reading_tr_t2010, wto_treaty_framework__developmental_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(wto_dev_reading_tr_t2015, wto_treaty_framework__developmental_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(wto_dev_reading_tr_t2020, wto_treaty_framework__developmental_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(wto_dev_reading_tr_t2025, wto_treaty_framework__developmental_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(wto_dev_reading_be_t1995, wto_treaty_framework__developmental_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(wto_dev_reading_be_t2001, wto_treaty_framework__developmental_reading, base_extractiveness, 2001, 0.32).
narrative_ontology:measurement(wto_dev_reading_be_t2005, wto_treaty_framework__developmental_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(wto_dev_reading_be_t2010, wto_treaty_framework__developmental_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(wto_dev_reading_be_t2015, wto_treaty_framework__developmental_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(wto_dev_reading_be_t2020, wto_treaty_framework__developmental_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(wto_dev_reading_be_t2025, wto_treaty_framework__developmental_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(wto_dev_reading_su_t1995, wto_treaty_framework__developmental_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(wto_dev_reading_su_t2001, wto_treaty_framework__developmental_reading, suppression_requirement, 2001, 0.3).
narrative_ontology:measurement(wto_dev_reading_su_t2005, wto_treaty_framework__developmental_reading, suppression_requirement, 2005, 0.33).
narrative_ontology:measurement(wto_dev_reading_su_t2010, wto_treaty_framework__developmental_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(wto_dev_reading_su_t2015, wto_treaty_framework__developmental_reading, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(wto_dev_reading_su_t2020, wto_treaty_framework__developmental_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(wto_dev_reading_su_t2025, wto_treaty_framework__developmental_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__developmental_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__developmental_reading, 0.12).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, trips_agreement__public_health_flexibilities).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, trips_article_66_2_technology_transfer).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, subsidies_agreement__developing_country_flexibilities).

% DUAL FORMULATION NOTE:
% This constraint (developmental_reading) and market_access_reading are sibling constraints from the same kernel (wto_treaty_framework). They differ in ε (0.42 vs ~0.25 estimated), beneficiary/victim structure, and claimed_type (tangled_rope vs rope). The developmental reading has higher extractiveness because it enforces technology transfer and permanent S&D; the market_access_reading has lower extractiveness because it treats these as aspirational/temporary. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__developmental_reading, powerless, 0.1).
constraint_indexing:directionality_override(wto_treaty_framework__developmental_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
