% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__market_access_reading, []).

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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Market Access Reading — Symmetric Liberalization as Universal Obligation
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   The WTO treaty framework is a contested kernel. This story instantiates
 *   the market_access_reading: trade liberalization as symmetric universal
 *   obligation, non-discrimination and market access as primary treaty
 *   purpose, Special and Differential Treatment (S&D) as temporary
 *   transitional exceptions. The sibling developmental_reading treats policy
 *   space for development as equal-status commitment, S&D as permanent
 *   structural accommodation, technology transfer as core obligation. This
 *   reading compresses developing country policy space (tariff bindings, SCM,
 *   TRIPS, TRIMS) while locking in developed country agricultural subsidies
 *   and IP rents. High extractiveness (0.72) from asymmetric lock-in; active
 *   enforcement via DSU (suppression 0.68); moderate theater (0.32) — dispute
 *   settlement is real but jurisprudence systematically narrows development
 *   exceptions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.72).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.68).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Market Access Reading — Symmetric Liberalization as Universal Obligation").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, 'f1a0b39a-b799-4be9-96a3-908ea866c15d').
narrative_ontology:cs_kernel_codification('f1a0b39a-b799-4be9-96a3-908ea866c15d', formalized).
narrative_ontology:cs_authority_grounding('f1a0b39a-b799-4be9-96a3-908ea866c15d', lineage).
narrative_ontology:cs_interpretation_layer_present('f1a0b39a-b799-4be9-96a3-908ea866c15d').
narrative_ontology:cs_reading_relation('f1a0b39a-b799-4be9-96a3-908ea866c15d', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('f1a0b39a-b799-4be9-96a3-908ea866c15d', foundational, symmetric_obligations_universal_application).
narrative_ontology:cs_axiom_status(symmetric_obligations_universal_application, holdable).
narrative_ontology:cs_axiom_grounding('f1a0b39a-b799-4be9-96a3-908ea866c15d', symmetric_obligations_universal_application, conventional).
narrative_ontology:cs_axiom('f1a0b39a-b799-4be9-96a3-908ea866c15d', foundational, special_differential_treatment_temporary_transitional).
narrative_ontology:cs_axiom_status(special_differential_treatment_temporary_transitional, holdable).
narrative_ontology:cs_axiom_grounding('f1a0b39a-b799-4be9-96a3-908ea866c15d', special_differential_treatment_temporary_transitional, conventional).
narrative_ontology:cs_axiom('f1a0b39a-b799-4be9-96a3-908ea866c15d', foundational, non_discrimination_primary_treaty_purpose).
narrative_ontology:cs_axiom_status(non_discrimination_primary_treaty_purpose, holdable).
narrative_ontology:cs_axiom_grounding('f1a0b39a-b799-4be9-96a3-908ea866c15d', non_discrimination_primary_treaty_purpose, conventional).
narrative_ontology:cs_axiom('f1a0b39a-b799-4be9-96a3-908ea866c15d', secondary, market_access_reciprocity_as_development_strategy).
narrative_ontology:cs_axiom_status(market_access_reciprocity_as_development_strategy, holdable).
narrative_ontology:cs_axiom_grounding('f1a0b39a-b799-4be9-96a3-908ea866c15d', market_access_reciprocity_as_development_strategy, instrumental).
narrative_ontology:cs_reference_frame('f1a0b39a-b799-4be9-96a3-908ea866c15d', marrakesh_agreement_1994_original_balance).
narrative_ontology:cs_drift_state('f1a0b39a-b799-4be9-96a3-908ea866c15d', post_doha_impasse_mc12_2022, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f1a0b39a-b799-4be9-96a3-908ea866c15d', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, developed_country_exporters).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, global_value_chain_leaders).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries_developing_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_policy_space).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, smallholder_agricultural_producers_global_south).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_governments).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, most_favored_nation_principle).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, national_treatment_obligation).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, tariff_binding_commitment).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, subsidy_disciplines_scm_agreement).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, trips_minimum_standards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain predictable, non-discriminatory market access across 164+ WTO members. Use investment treaties and trade rules to challenge developing country industrial policies, local content requirements, and technology transfer conditions. Extract rents from intellectual property enforcement and investor-state dispute settlement. Can relocate production, shift supply chains, and forum-shop across jurisdictions.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    powerful, generational, arbitrage, global).

% Shape WTO negotiating agenda through quad/quinquire meetings and green room processes. Secure tariff bindings on industrial goods near zero while maintaining agricultural protection and subsidies. Use S&D transition periods to lock in asymmetry. Benefit from TRIPS and TRIMS agreements that constrain developing country policy tools they themselves used historically.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developed_country_exporters, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, developed_country_exporters, agenda_setter).

% Bound by single undertaking — must accept all agreements including those that compress industrial policy space. Tariff bindings lock in low ceilings; subsidies disciplines (SCM) prohibit export subsidies and local content requirements; TRIPS raises medicine and technology costs. S&D provisions are best-effort, non-justiciable, and time-limited. Policy space for infant industry protection, strategic trade policy, and food sovereignty systematically eroded.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_governments, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, developing_country_governments, agenda_setter).

% Face immediate import competition from established global producers before achieving scale economies or learning-by-doing. Cannot use tariffs (bound), subsidies (prohibited), local content (TRIMS), or technology transfer requirements (TRIPS/investment rules). No effective exit — domestic market too small, capital markets underdeveloped, political cost of deindustrialization high. Many sectors never launch; others are acquired by foreign firms.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries_developing_countries, payer,
    powerless, biographical, trapped, national).

% Compete against heavily subsidized agricultural exports from developed countries (EU CAP, US Farm Bill) while own governments cannot provide equivalent support (AMS limits, de minimis thresholds). Food import dependence rises; rural livelihoods collapse. No exit — land-bound, capital-constrained, politically marginalized. WTO Agreement on Agriculture locks in asymmetry: developed country subsidies mostly in non-actionable boxes, developing country support constrained.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, smallholder_agricultural_producers_global_south, payer,
    powerless, immediate, trapped, local).

% Adjudicates disputes under DSU; panels and Appellate Body (when functional) interpret covered agreements. Jurisprudence has progressively narrowed policy space: Shrimp-Turtle (PPM distinction), EC-Asbestos (regulatory autonomy), Brazil-Retreaded Tyres (necessity test), India-Solar Cells (local content). Enforcement via retaliation authorization gives teeth. Current crisis: Appellate Body paralyzed since 2019 by US blocking appointments — enforcement mechanism degraded but not dead.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% Advocate for policy space, food sovereignty, access to medicines, climate justice. Formal participation limited to NGO symposium at Ministerial Conferences; no standing in disputes, no voice in negotiations. Green room processes exclude them. Their alternative frameworks (Buenos Aires Plan, Doha Development Agenda original intent) marginalized. Some access via sympathetic developing country delegations but structurally locked out of decision-making.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, civil_society_development_ngos, excluded,
    moderate, biographical, constrained, global).

% Analyze WTO law from legal, economic, political economy perspectives. Debate whether system is net coordination (trade peace, predictability) or net extraction (asymmetric lock-in). Produce evidence on development impacts, dispute settlement jurisprudence, negotiating dynamics. No stake in outcome but frame the epistemic contest between market_access_reading and developmental_reading.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, academic_observers_trade_law, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a multilateral rules-based system for trade: binds tariffs, prohibits discrimination (MFN, national treatment), disciplines subsidies, establishes transparent procedures, and offers binding dispute settlement — replacing power-based bilateralism with law-based predictability.
% TRANSFER_FUNCTION: Moves policy autonomy from developing country governments to multinational capital: tariff bindings transfer revenue from domestic treasury to importers; subsidies disciplines transfer industrial policy tools from state to market; TRIPS transfers monopoly rents to IP holders (mostly Northern); TRIMS transfers technology and local linkage requirements from host state to investor. Gains accrue overwhelmingly to MNCs and developed country exporters.
% ABSENT_VOICES: Smallholder farmers, informal sector workers, indigenous communities, and future generations in developing countries — those most affected by agricultural liberalization, IP enforcement, and policy space compression — have no formal representation in WTO bodies. Their interests are mediated through governments that face power asymmetry in negotiations, or not at all.
% DISAPPEARANCE_RATIONALE: If the market_access_reading's constraints vanished overnight: developing countries would immediately raise tariffs to bound or applied rates, reinstate industrial policies (local content, export subsidies, technology transfer), issue compulsory licenses for medicines, and restructure agricultural support. Global trade flows would shift; MNC profits from IP rents and forced market access would fall; developed country exporters would face new barriers. The world trade system would reorganize around bilateral/regional power dynamics rather than multilateral rules.
% FOUNDING_PROBLEM: Post-WWII: prevent return to 1930s protectionist spiral (Smoot-Hawley, imperial preference) that deepened depression and contributed to war. GATT 1947 created reciprocal tariff reduction framework; WTO 1995 expanded to services, IP, investment measures, binding dispute settlement. The market_access_reading frames the founding problem as 'discrimination and unpredictability in trade relations' — solved by symmetric obligations.
% FOUNDING_PROBLEM_CORROBORATION: Developed country governments and MNCs attest the problem remains live: non-tariff barriers, state-owned enterprises, forced technology transfer, digital trade gaps require new rules. Developing country governments (G90, LDC Group, African Group) and development economists (Chang, Stiglitz, Rodrik) attest the founding problem is substantially solved for industrial goods — the system now solves a different problem: locking in Northern competitive advantage. UNCTAD, South Centre, and academic literature outside beneficiary set corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__market_access_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the single undertaking binds developing countries to agreements (TRIPS, TRIMS, SCM, AoA) that prohibit the very policy tools (infant industry protection, export subsidies, local content, technology transfer requirements) that today's developed countries used historically. Suppression is substantial because the DSU makes these prohibitions enforceable via retaliation — exit is blocked by the single undertaking (accept all or none). Theater is moderate: the coordination function (trade peace, MFN, transparency) is genuine, but a growing share of jurisprudence and negotiating energy defends extraction (IP rents, market access for services/investment) rather than coordination. Measurements show extraction and suppression rising 1995-2008 (post-Uruguay Round implementation, TRIPS transition ends, China accession), then plateauing as Doha Round fails and Appellate Body crisis degrades enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the MNC/developed country seat: the system is a Rope — genuine coordination solving discrimination and unpredictability, net beneficial. From the infant industry/smallholder seat: the system is a Snare — coordination story is cover for locking in asymmetry, exit suppressed, alternatives (industrial policy, food sovereignty) forbidden. From the developing country government seat: Tangled Rope — they get market access (coordination) but pay through policy space compression (extraction). The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   MNCs and developed country exporters are structural beneficiaries (d near 0): they collect IP rents, gain market access, face no binding constraints on their home subsidies. Developing country governments are payers (d ~0.7): bound by all agreements, S&D non-justiciable, policy space compressed. Infant industries and smallholders are full targets (d ~0.9): trapped, no exit, bear immediate competition without protection. WTO DSB is agenda_setter with analytical exit (d ~0.3): administers the system, benefits from its legitimacy, but constrained by member states. Civil society excluded (d not computed — not a seat in the engine). Academics observer (d=0.5 by convention).
 *
 * MANDATROPHY ANALYSIS:
 *   The market_access_reading treats S&D as temporary transitions — implying the mandate (development accommodation) is resolved once countries 'graduate.' But the founding problem (asymmetric starting conditions) is NOT resolved: per capita income gaps persist, industrial capacity gaps persist, technological gaps persist. The arrangement persists because it extracts for beneficiaries, not because the founding problem is solved. Mandatrophy is UNRESOLVED: the constraint's coordination function (trade peace) is real but its extraction function (policy space compression) has become the dominant driver of its persistence. The developmental_reading exists precisely because mandatrophy is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_reading_kernel_committer,
    'Is the market_access_reading a genuine reading of the WTO kernel, or a reinterpretation that serves extractive interests?',
    'Compare the reading''s structural commitments (symmetric obligations, temporary S&D, TRIPS/TRIMS/SCM as core) against the negotiating history (Doha mandate, Hong Kong declaration, Bali/Nairobi/MC12 outcomes) and the text of the Marrakesh Agreement preamble (which references development objectives). If the reading systematically ignores textual commitments to development, it is a reinterpretation, not a reading.',
    'If reinterpretation, the kernel has been captured — the market_access_reading is not a reading but a displacement. The developmental_reading would be the only faithful reading. Classification of this constraint would shift: the coordination function is thinner, extraction is the primary driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_reading_kernel_committer, conceptual, 'Whether this reading faithfully instantiates the kernel or displaces it').

omega_variable(
    s_and_d_temporary_vs_permanent,
    'Are Special and Differential Treatment provisions structurally temporary transitions or permanent accommodations of asymmetry?',
    'Empirical: track S&D utilization rates, graduation outcomes, and whether any developing country has ''graduated'' to symmetric obligations without losing policy space. Conceptual: analyze whether asymmetric starting conditions (colonial history, structural adjustment, climate vulnerability) are time-bound or structural.',
    'If S&D is permanent accommodation, the market_access_reading''s core premise (temporary transition) is false — the constraint''s extractiveness is structural, not transitional. The developmental_reading''s claim gains structural validity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(s_and_d_temporary_vs_permanent, conceptual, 'Whether development asymmetry is transient or structural').

omega_variable(
    coordination_extraction_boundary_wto,
    'How much of the WTO system''s measured extractiveness is the necessary cost of its coordination function (trade peace, MFN, transparency) versus asymmetric rent extraction?',
    'Counterfactual modeling: simulate a WTO with symmetric obligations but robust, justiciable S&D, technology transfer obligations, and policy space for industrial development. Compare welfare distribution. If coordination benefits persist without extraction, the boundary is separable.',
    'If separable, the market_access_reading''s high extractiveness is a design choice, not a coordination necessity — the constraint is a Tangled Rope where extraction could be reduced without losing coordination. If inseparable, the system is fundamentally a Snare with coordination as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_wto, empirical, 'Whether coordination and extraction in the WTO are structurally separable').

omega_variable(
    appellate_body_crisis_nature,
    'Is the Appellate Body paralysis (2019-present) a temporary institutional crisis or a structural feature revealing the market_access_reading''s dependence on uncontrolled adjudication?',
    'Track whether developed countries (especially US) accept dispute outcomes that constrain their subsidies/agriculture/IP enforcement. If the crisis resolves only when adjudication is constrained (e.g., no review of national security exceptions, no review of ''essential security interests''), the market_access_reading requires uncontrolled enforcement for extraction.',
    'If structural, the market_access_reading''s enforcement mechanism is itself extractive — it works only when it favors beneficiaries. The constraint''s suppression is conditional on power, not law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_crisis_nature, empirical, 'Whether WTO enforcement is law-based or power-conditioned').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_tr_t1995, wto_treaty_framework__market_access_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_tr_t2001, wto_treaty_framework__market_access_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_tr_t2008, wto_treaty_framework__market_access_reading, theater_ratio, 2008, 0.27).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_tr_t2013, wto_treaty_framework__market_access_reading, theater_ratio, 2013, 0.3).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_tr_t2017, wto_treaty_framework__market_access_reading, theater_ratio, 2017, 0.31).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_tr_t2024, wto_treaty_framework__market_access_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_be_t1995, wto_treaty_framework__market_access_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_be_t2001, wto_treaty_framework__market_access_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_be_t2008, wto_treaty_framework__market_access_reading, base_extractiveness, 2008, 0.68).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_be_t2013, wto_treaty_framework__market_access_reading, base_extractiveness, 2013, 0.7).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_be_t2017, wto_treaty_framework__market_access_reading, base_extractiveness, 2017, 0.71).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_be_t2024, wto_treaty_framework__market_access_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_su_t1995, wto_treaty_framework__market_access_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_su_t2001, wto_treaty_framework__market_access_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_su_t2008, wto_treaty_framework__market_access_reading, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_su_t2013, wto_treaty_framework__market_access_reading, suppression_requirement, 2013, 0.66).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_su_t2017, wto_treaty_framework__market_access_reading, suppression_requirement, 2017, 0.67).
narrative_ontology:measurement(wto_treaty_framework__market_access_reading_su_t2024, wto_treaty_framework__market_access_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__market_access_reading, 0.12).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, trips_access_to_medicines).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, scm_subsidies_disciplines).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, trims_local_content_prohibition).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, agriculture_agreement_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint and developmental_reading form the wto_treaty_framework constraint family. They share the same kernel (WTO covered agreements) but instantiate different constraints with different ε values, different beneficiary/victim structures, and different claimed types. The market_access_reading has higher ε (0.72 vs ~0.35) because it treats S&D as temporary and development policy tools as prohibited; the developmental_reading has lower ε because it treats policy space as a coordination good. The market_access_reading influences the developmental_reading by shaping jurisprudence and negotiating dynamics that narrow the latter's operational space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, institutional, 0.15).
constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, organized, 0.65).
constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
