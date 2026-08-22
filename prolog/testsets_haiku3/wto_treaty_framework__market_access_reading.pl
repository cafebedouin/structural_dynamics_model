% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Market Access Reading: Trade Liberalization as Symmetric Obligation
 *   domain: international/economic/political
 *
 * SUMMARY:
 *   The WTO treaty framework contains a contested kernel: what is the
 *   treaty's PRIMARY PURPOSE and which obligations are PERMANENT vs.
 *   TEMPORARY? The market-access reading treats trade liberalization as a
 *   symmetric universal obligation, with non-discrimination and binding
 *   market access as the core commitment. Special and Differential (S&D)
 *   provisions for developing countries are read narrowly as temporary
 *   exceptions for adjustment, not as a structural accommodation recognizing
 *   asymmetric capacities. Under this reading, infant-industry protection,
 *   strategic tariffs, and development-oriented subsidies are treaty
 *   violations. Dispute-settlement enforcement pulls the reading into
 *   practice: panels invalidate protectionist policies as inconsistent with
 *   market-access bindings. This reading primarily benefits multinational
 *   corporations and high-capacity exporters; it compresses policy space for
 *   developing-state industrial policy.
 *
 * KEY AGENTS:
 *   - Multinational corporations: primary beneficiary, low exit cost, high power
 *   - Developing state industrial policy: primary victim, constrained exit, institutional
 *   - WTO dispute settlement: agenda-setter, enforces the reading through jurisprudence
 *   - Infant industry sectors: victims, moderate power, constrained exit
 *   - Advanced-economy exporters: beneficiaries, institutional, arbitrage exit
 *   - Least-developed countries: excluded, powerless, trapped
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.78).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.71).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Market Access Reading: Trade Liberalization as Symmetric Obligation").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international/economic/political").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, 'b30dc1ab-977a-4839-88b9-f190ef82db52').
narrative_ontology:cs_kernel_codification('b30dc1ab-977a-4839-88b9-f190ef82db52', formalized).
narrative_ontology:cs_authority_grounding('b30dc1ab-977a-4839-88b9-f190ef82db52', extraction).
narrative_ontology:cs_interpretation_layer_present('b30dc1ab-977a-4839-88b9-f190ef82db52').
narrative_ontology:cs_reading_relation('b30dc1ab-977a-4839-88b9-f190ef82db52', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('b30dc1ab-977a-4839-88b9-f190ef82db52', foundational, trade_liberalization_symmetric_obligation).
narrative_ontology:cs_axiom_status(trade_liberalization_symmetric_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b30dc1ab-977a-4839-88b9-f190ef82db52', trade_liberalization_symmetric_obligation, conventional).
narrative_ontology:cs_axiom('b30dc1ab-977a-4839-88b9-f190ef82db52', foundational, special_differential_temporary_transition).
narrative_ontology:cs_axiom_status(special_differential_temporary_transition, holdable).
narrative_ontology:cs_axiom_grounding('b30dc1ab-977a-4839-88b9-f190ef82db52', special_differential_temporary_transition, conventional).
narrative_ontology:cs_reference_frame('b30dc1ab-977a-4839-88b9-f190ef82db52', reciprocal_tariff_reduction_and_non_discrimination).
narrative_ontology:cs_drift_state('b30dc1ab-977a-4839-88b9-f190ef82db52', post_2000_dispute_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b30dc1ab-977a-4839-88b9-f190ef82db52', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, high_capacity_exporters).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, tariff_revenue_dependent_governments).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industry_sectors).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, commodity_dependent_economies).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_state_industrial_policy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, advanced_economy_labor).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, advanced_economy_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain direct, binding, dispute-enforceable market access to developing economies without tariff barriers or local-content requirements. Can source global supply chains and sell in protected developing markets at scale. Their access is secured by WTO bindings and dispute settlement; their tariff exposure at home is much shallower and covers a narrower range of goods (advanced manufactures are more protected than textiles).
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    powerful, generational, arbitrage, global).

% Advanced-economy governments (US, EU, Japan) and their export champions secure binding tariff commitments from developing countries at a broad scope (textiles, agriculture, simple manufactures), while maintaining their own protective measures (antidumping, safeguards, agricultural support) within carve-out safe havens. They use dispute-settlement machinery to enforce the market-access reading and block developing-country protective policies.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, high_capacity_exporters, beneficiary,
    institutional, generational, arbitrage, global).

% Face binding commitments to tariff reduction on their inputs and outputs. During the critical learning phase (typically 10-20 years for new manufacturing sectors), they must compete immediately with established multinational and advanced-economy competitors. Policy tools they would otherwise use (tariff protection, infant-industry subsidies, local-content mandates) are either bound above baseline rates or prohibited; using them triggers dispute settlement.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industry_sectors, payer,
    moderate, biographical, constrained, regional).

% Have few competitive export manufactures but face broad tariff-reduction commitments covering the sectors where they could develop competitiveness (textiles, apparel, food processing, basic metals). Early reduction schedules tied to S&D provisions mean tariffs fall before domestic capacity to compete is built. They cannot use tariff revenue for development investment.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, commodity_dependent_economies, payer,
    moderate, biographical, constrained, regional).

% A policy space itself — the capacity of developing-country governments to deploy tariffs, production subsidies, local-content requirements, and technology-transfer mandates as instruments of deliberate industrial development. The market-access reading treats this space as a constraint violation. Policies designed to build domestic capability are invalidated by dispute panels as inconsistent with market-access bindings.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_state_industrial_policy, payer,
    institutional, biographical, constrained, national).

% Interprets the treaty through the market-access reading: non-discrimination (MFN, national treatment) and binding enforcement are primary; S&D is construed narrowly as a temporary exception. The Dispute Settlement Body's jurisprudence (India—Patents, India—Automobiles, India—Chemicals) invalidates development-protective policies as trade-restrictive. The DSB's reasoning enshrines the reading in practice, making it the de facto treaty interpretation.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_dispute_settlement, agenda_setter,
    institutional, generational, analytical, global).

% Are nominally granted longer S&D transition periods (e.g., 2020 for apparel, 2030 for other sectors) but are excluded from meaningful dispute defense (limited legal capacity, weak bargaining power). They face informal pressure to accept 'capacity building' and 'implementation assistance' that accelerates their liberalization. The reading treats their longer timeline as a phase-out schedule, not as a recognition that symmetric liberalization may never be optimal for their economic structure.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, least_developed_countries, excluded,
    powerless, generational, trapped, regional).

% Benefits from cheaper imports of textiles, apparel, electronics, and agricultural goods — lower consumer prices. Simultaneously bears employment displacement in manufacturing sectors. The market-access reading prioritizes consumer benefit over employment stability; displaced workers are treated as benefiting from cheaper goods even as their livelihoods vanish. Labor's excluded voice is a demand for longer adjustment timelines or trade adjustment assistance.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, advanced_economy_labor, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, advanced_economy_labor, payer).

% Produce empirical research on trade liberalization's effects on developing-economy welfare, industrial formation, and poverty. Their work contests the reading's assumptions (whether symmetric liberalization generates equal benefits, whether development requires policy space). This seat has no power to enforce alternative interpretation but generates evidence that delegitimizes the reading's empirical premises.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, development_economists_and_ngos, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rules-based, non-discriminatory trade system with binding tariff commitments enforceable through dispute settlement. Solves the collective-action problem of 1930s-style competitive devaluation and bilateral mercantilist bargaining by creating a multilateral frame where all tariff bindings are reciprocal and covered by the same dispute rules.
% TRANSFER_FUNCTION: Moves market access from developing economies to multinational corporations and advanced-economy exporters. Developing countries surrender policy space (tariff autonomy, subsidy capacity, local-content authority) in exchange for market-access concessions that are shallower in scope (advanced-economy tariffs remain higher on protected sectors like agriculture and apparel) and enforcement (developing countries rarely win disputes at the same rate as advanced economies). Tariff revenue is transferred from developing governments to foreign corporations that displace domestic industry.
% ABSENT_VOICES: Domestic manufacturers and industrial workers in developing countries are not parties to treaty negotiations and cannot litigate in dispute settlement. Infant-industry advocates are excluded. Labor unions in both advanced and developing economies are excluded — they would argue for longer adjustment timelines, larger trade adjustment assistance, and exemptions for critical manufacturing sectors. Least-developed countries have minimal voice despite being most affected.
% DISAPPEARANCE_RATIONALE: If the market-access binding and WTO enforcement disappeared overnight, developing economies would immediately restore tariffs and subsidies for protected sectors, multinational corporations would face re-entry barriers and higher landed costs, and industrial-policy space would reopen. Global trade would reorganize around regional integration blocs (RCEP, African Union, ALBA) with their own development-permissive rules. Advanced-economy export markets in developing jurisdictions would contract within 18-24 months; supply chains would reorient toward tariff-managed regional networks.
% FOUNDING_PROBLEM: The post-WWII trade system needed a rule-based alternative to bilateral mercantilist negotiation, competitive devaluation, and tit-for-tat protectionism (the 1930s pathology). GATT solved this by establishing reciprocal, non-discriminatory tariff bindings enforceable through dispute resolution. The challenge when the WTO was created (1995) was extending this system to developing economies that had achieved independence and wanted development-oriented policy autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Advanced economies and the WTO Secretariat attest the founding problem remains live: without binding enforcement, unilateral protectionism and beggar-thy-neighbor trade policy would return (they cite recent US tariff escalations as evidence). Developing economies and development economists attest the original problem (uncertainty and bilateralism) is largely solved through bilateral investment treaties and regional trade agreements, which now carry more transactional weight than WTO dispute litigation. They attest the NEW problem is that the market-access reading has weaponized the non-discrimination rule: dispute settlement now enforces a specific interpretation that transfers development-policy autonomy to wealthy importers. Independent analyses (UNCTAD, academic economists) corroborate that the reading has compressed policy space without generating the promised development outcomes for most developing countries.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__market_access_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness climbs steadily (0.45 → 0.78 over 35 years) as dispute-settlement jurisprudence narrows S&D exemptions and enforces market-access bindings against development-protective measures. Suppression rises in parallel (0.48 → 0.71) — the constraint's persistence depends on active enforcement through dispute panels, not on participant preference. Theater is moderate and rising (0.25 → 0.42), reflecting growing emphasis on 'implementation capacity building' and technical assistance that frames coerced liberalization as development aid. The measurements track one aligned time grid: every metric is authored at every examined point. The temporal trajectory shows classic extraction accumulation: a real coordination function (non-discrimination) layered with asymmetric extraction (market access seized unequally) that intensifies as enforcement machinery matures. This matches the tangled-rope structure: genuine coordination + asymmetric extraction + active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the high-capacity exporter seat, the reading is pure coordination: a system of binding commitments that reduce trade uncertainty and expand market access predictably. From the developing-state industrial-policy seat, the same mechanism operates as enforced extraction: losing policy autonomy to protect infant industries without triggering disputes, facing asymmetric liberalization timelines, and bearing displacement costs while advanced economies retain substantial protective capacity (agriculture, antidumping, safeguards). The engine computes this divergence from the structural data — directionality is opposite at the two seats despite both being institutional. The agenda-setter (WTO DSB) reads the treaty as market-access-centric; developing-state observers read it as a sovereignty compromise that has been reinterpreted asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations are clear beneficiaries: they gain tariff-free market access without corresponding risk (d near 0.0, subsidy-equivalent). High-capacity exporters are beneficiaries: their shallow tariff bindings and protected domestic sectors (agriculture in EU/US) are the cover story; their leverage over developing economies' market access is the real flow (d near 0.1-0.2). Developing-state industrial policy is the target: loses autonomy, faces dispute risk, absorbs displacement costs (d near 0.85-0.95). Infant industries are victims: face import competition they are contractually barred from protecting (d near 0.9). Advanced-economy labor sits at d ≈ 0.5: benefits from cheaper imports, pays the cost of displacement. The directionality derivation from beneficiary/victim + exit options matches the authored base metrics without override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'coordination in trade rules' — solved by the treaty's non-discrimination framework. The reading has been reinterpreted to make market-access enforcement the PRIMARY mechanism, reducing S&D to a temporary phase-out schedule. Under this reinterpretation, the founding problem appears 'live' (uncertainty in trade still exists) but the solution has transmuted: it now enforces asymmetric extraction rather than mutual benefit. Mandatrophy is LIVE — the constraint persists because dispute settlement has made the market-access reading profitable for high-capacity actors and costly to challenge. The reading's own advocates would deny mandatrophy ('the rule of law requires binding enforcement') while victims attest the founding coordination problem is increasingly solved through bilateral/regional deals that bypass the asymmetric multilateral mechanism. Classification as tangled_rope correctly captures this: the coordination is real (risk reduction) AND the extraction is real (market-access asymmetry); both are sustained by the dispute-enforcement machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    s_d_temporality_contest,
    'Are S&D provisions a TEMPORARY adjustment mechanism (phase-out to symmetric obligations) or a PERMANENT structural accommodation recognizing asymmetric development capacities?',
    'Textual examination of treaty language (the Preamble and Part IV of GATT 1994 are the contested passages); panel jurisprudence trend analysis (are S&D exemptions narrowing over time or holding steady); longitudinal study of actual S&D use (do fewer countries claim S&D protection, or do new entrants claim it?)',
    'If temporary (market-access reading): the constraint''s core mandate is universal liberalization; extractiveness remains high and justified by the rule-of-law principle. If permanent (developmental reading): the constraint is a hybrid that enforces market access on beneficiaries while protecting policy space for victims; extractiveness would be reclassified as a false summit (a natural law of reciprocal trade masking an extractive asymmetry). The reading contest determines classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(s_d_temporality_contest, conceptual, 'The fundamental disagreement about what the treaty promises to developing countries').

omega_variable(
    capacity_asymmetry_externality,
    'What share of the measured extraction (0.78) is the COST OF UNEQUAL STARTING CAPACITY vs. DELIBERATE ASYMMETRIC POLICY DESIGN?',
    'Counterfactual simulation: if liberalization timelines were scaled to equalize industrial-capacity development (20-year phase-outs for least-developed countries matching the historical pace of advanced economies), would extractiveness drop to 0.4-0.5 (pure coordination cost) or remain at 0.7+ (deliberate asymmetry)?',
    'If mostly capacity cost: the reading is harsh but not structurally extractive; a softer timeline would restore rope classification. If mostly asymmetric design: the reading is a snare dressed as coordination; the agenda-setter (WTO DSB) is captured by beneficiary interests. The resolution reshapes whether this is a ''tough but fair'' rule or a ''predatory extraction.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_asymmetry_externality, empirical, 'The extent to which extraction tracks developing-economy incapacity vs. deliberate treaty design').

omega_variable(
    dispute_settlement_bias,
    'Does WTO dispute-settlement jurisprudence systematically favor beneficiary interpretations of the treaty (market-access reading) over alternative readings (developmental reading)?',
    'Meta-analysis of dispute outcomes: win rate for advanced-economy complainants vs. developing-country complainants; rate of S&D exemption acceptance in panel reports; trend in panel reasoning (are panels citing non-discrimination more frequently than development-as-purpose over time?)',
    'If biased toward market-access: the agenda-setter (DSB) is not neutral; extractiveness is sustained by procedural asymmetry in interpretation. Reclassifies as snare if the neutrality of law is the primary justification. If neutral (various readings win approximately equally): the reading is just one live interpretation; the divergence in outcomes reflects underlying power, not interpretation bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_settlement_bias, empirical, 'Whether the enforcement machinery favors the market-access reading structurally').

omega_variable(
    infant_industry_learning_necessity,
    'Is tariff protection for infant industries EMPIRICALLY NECESSARY for industrial capacity to develop, or can developing economies achieve manufacturing competitiveness through exposure to competition?',
    'Comparative history of industrial development (East Asia, Latin America, Sub-Saharan Africa); econometric analysis of tariff protection''s relationship to subsequent manufacturing competitiveness; case studies of successful vs. failed industrial sectors in liberalizing economies.',
    'If protection is necessary: the market-access reading systematically forecloses a development strategy that advanced economies used historically; the constraint compresses policy space that is functionally essential. If protection is unnecessary: the reading''s constraints are a cost of adjustment but not a blocker to development. The evidence determines whether victims face genuine harm or just adjustment costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infant_industry_learning_necessity, empirical, 'Whether infant-industry tariff protection is causally necessary for industrial development').

omega_variable(
    kernel_reading_commission,
    'KERNEL CONTEXT: This constraint is ONE READING of the WTO treaty. The sibling developmental_reading treats the same kernel (the treaty text) but instantiates different epsilon and beneficiary/victim structure. Are these readings LOGICALLY FORECLOSING (cannot coexist in the same legal framework) or COEXISTING (held by different parties in the same dispute system)?',
    'Examine the treaty''s actual text (GATT Preamble, Part IV, Article XXXVI-XXXVIII) — does it textually endorse EITHER reading uniquely, or is the text ambiguous enough that both can claim fidelity? Review panel jurisprudence: have panels explicitly rejected the developmental reading as inconsistent with the treaty, or do they simply not apply it?',
    'If foreclosing: one reading is incompatible with the treaty''s actual language; classification requires choosing which reading accurately reflects the legal text. If coexisting: the dispute system contains both readings simultaneously; classification divergence across readings is a feature not a bug, and the corpus documents the interpretive contest itself. The resolution determines whether the engine should produce one classification (the correct reading) or two (two live readings of one kernel).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commission, conceptual, 'Whether the market-access and developmental readings are logically foreclosing or coexisting within the WTO treaty framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_treaty_framework__market_access_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(wto__tr_t0, observed).
narrative_ontology:measurement(wto__tr_t5, wto_treaty_framework__market_access_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(wto__tr_t5, observed).
narrative_ontology:measurement(wto__tr_t10, wto_treaty_framework__market_access_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(wto__tr_t10, observed).
narrative_ontology:measurement(wto__tr_t15, wto_treaty_framework__market_access_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(wto__tr_t15, observed).
narrative_ontology:measurement(wto__tr_t25, wto_treaty_framework__market_access_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(wto__tr_t25, observed).
narrative_ontology:measurement(wto__tr_t35, wto_treaty_framework__market_access_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(wto__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_treaty_framework__market_access_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(wto__be_t0, observed).
narrative_ontology:measurement(wto__be_t5, wto_treaty_framework__market_access_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(wto__be_t5, observed).
narrative_ontology:measurement(wto__be_t10, wto_treaty_framework__market_access_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(wto__be_t10, observed).
narrative_ontology:measurement(wto__be_t15, wto_treaty_framework__market_access_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(wto__be_t15, observed).
narrative_ontology:measurement(wto__be_t25, wto_treaty_framework__market_access_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement_basis(wto__be_t25, observed).
narrative_ontology:measurement(wto__be_t35, wto_treaty_framework__market_access_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(wto__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_treaty_framework__market_access_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(wto__su_t0, observed).
narrative_ontology:measurement(wto__su_t5, wto_treaty_framework__market_access_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(wto__su_t5, observed).
narrative_ontology:measurement(wto__su_t10, wto_treaty_framework__market_access_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(wto__su_t10, observed).
narrative_ontology:measurement(wto__su_t15, wto_treaty_framework__market_access_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(wto__su_t15, observed).
narrative_ontology:measurement(wto__su_t25, wto_treaty_framework__market_access_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement_basis(wto__su_t25, observed).
narrative_ontology:measurement(wto__su_t35, wto_treaty_framework__market_access_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(wto__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__market_access_reading, 0.18).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).

% DUAL FORMULATION NOTE:
% The WTO treaty framework decomposes into two structurally distinct constraint stories corresponding to two readings: (1) MARKET-ACCESS reading (this file) — treats trade liberalization as symmetric obligation, S&D as temporary exceptions, high extractiveness, multinational beneficiaries, infant-industry victims. (2) DEVELOPMENTAL reading (sibling file) — treats policy space for development as permanent commitment, S&D as structural accommodation, lower extractiveness, developing-state beneficiaries, capacity asymmetry victims. The readings share the kernel (the treaty text) but instantiate different epsilon and classification. The difference is not measurement ambiguity — it is a genuine contest about the treaty's PRIMARY PURPOSE and BINDING COMMITMENTS. Both readings are live in the dispute system; the corpus documents the contest, not the resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
