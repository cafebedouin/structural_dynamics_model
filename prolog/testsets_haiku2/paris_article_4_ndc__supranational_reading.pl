% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc_supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris NDCs as Binding Supranational Ratcheting Commitments with International Accountability
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 establishes Nationally Determined
 *   Contributions (NDCs) as the mechanism for climate action. Three distinct
 *   readings contest what NDCs are and what they obligate: (1) the
 *   supranational reading (this constraint) holds that NDCs are binding,
 *   escalating, internationally-accountable commitments that create
 *   supranational climate governance with enforcement teeth; (2) the
 *   sovereigntist reading holds that NDCs are voluntary self-determined
 *   pledges that preserve national energy sovereignty; (3) the equity reading
 *   holds that NDCs must be interpreted through Common But Differentiated
 *   Responsibilities, structurally distinguishing obligations between
 *   developed and developing states. This constraint instantiates the
 *   supranational reading: it models NDCs as a high-extraction,
 *   actively-enforced, ratcheting constraint system where wealthy states and
 *   international climate institutions extract compliance from fossil-fuel
 *   exporters and industrializing nations through binding targets, financial
 *   conditionality, and reputational/regulatory sanctions. The constraint is
 *   CLAIMED as tangled rope because it combines genuine coordination function
 *   (solving the commons tragedy) with asymmetric extraction (wealthy states
 *   set terms; vulnerable states and carbon-dependent economies pay).
 *
 * KEY AGENTS:
 *   - climate_vulnerable_nations — Trapped beneficiaries; they need binding enforcement on emitters to claim adaptation finance, but have no leverage and cannot exit.
 *   - fossil_fuel_exporting_states — Organized payers; they face stranded assets and de-industrialization; their exit options are constrained (can weaken enforcement politically but cannot leave the framework without reputational cost).
 *   - carbon_intensive_industries — Powerful payers; they face regulatory extinction as NDCs translate into domestic emissions limits; constrained to relocation or transition.
 *   - wealthy_developed_states — Institutional agenda-setters; they design the enforcement architecture and compliance standards; their exit options are highest (they can modulate ambition without framework exit).
 *   - renewable_energy_manufacturers — Powerful beneficiaries; they capture market growth from regulatory forcing; strong arbitrage options across jurisdictions.
 *   - international_climate_finance_institutions — Institutional beneficiaries and agenda-setters; they grow in mission and gatekeeper authority as NDCs require climate conditionality on development lending.
 *   - UNFCCC compliance secretariat — Institutional agenda-setter and trapped mechanism; they operate the framework and have no exit.
 *   - analytical observer — carries no policy power but frames what the constraint does.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.81).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.74).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris NDCs as Binding Supranational Ratcheting Commitments with International Accountability").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '73b6873e-c85d-449e-9846-d267ff71ae70').
narrative_ontology:cs_kernel_codification('73b6873e-c85d-449e-9846-d267ff71ae70', formalized).
narrative_ontology:cs_authority_grounding('73b6873e-c85d-449e-9846-d267ff71ae70', extraction).
narrative_ontology:cs_interpretation_layer_present('73b6873e-c85d-449e-9846-d267ff71ae70').
narrative_ontology:cs_reading_relation('73b6873e-c85d-449e-9846-d267ff71ae70', paris_article_4_ndc__paris_article_4_ndc_sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('73b6873e-c85d-449e-9846-d267ff71ae70', paris_article_4_ndc__paris_article_4_ndc_equity_reading, coexists_with).
narrative_ontology:cs_axiom('73b6873e-c85d-449e-9846-d267ff71ae70', foundational, ndc_binding_supranational_enforcement).
narrative_ontology:cs_axiom_status(ndc_binding_supranational_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('73b6873e-c85d-449e-9846-d267ff71ae70', ndc_binding_supranational_enforcement, deontological).
narrative_ontology:cs_axiom('73b6873e-c85d-449e-9846-d267ff71ae70', foundational, commons_tragedy_requires_supranational_coordination).
narrative_ontology:cs_axiom_status(commons_tragedy_requires_supranational_coordination, holdable).
narrative_ontology:cs_axiom_grounding('73b6873e-c85d-449e-9846-d267ff71ae70', commons_tragedy_requires_supranational_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('73b6873e-c85d-449e-9846-d267ff71ae70', paris_binding_accountability_framework).
narrative_ontology:cs_drift_state('73b6873e-c85d-449e-9846-d267ff71ae70', contemporary_enforcement_hardening, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('73b6873e-c85d-449e-9846-d267ff71ae70', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_manufacturers).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_climate_finance_institutions).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, industrializing_developing_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, industrializing_developing_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, wealthy_developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, corporate_transition_losers).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, climate_emergency_supranational_governance).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, emissions_reductions_legally_binding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Small island states and least-developed countries face existential threats from warming; the NDC framework institutionalizes their claims on wealthy emitters for climate finance and technology transfer. They have no exit: they experience climate impacts regardless of their own NDC ambition. Their beneficiary position rests on the constraint's enforceability — without binding accountability on wealthy states, their claims on adaptation finance dissolve.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations, beneficiary,
    powerless, civilizational, trapped, universal).

% States with economies dependent on oil/gas exports face NDC ratcheting as an existential economic threat. They pay through stranded assets, de-industrialization risk, and loss of geopolitical leverage as carbon-intensive sectors face regulatory elimination. Their exit options are exit the Paris framework (political cost + reputational sanctions) or attempt to weaken enforcement (political effort, declining success as consensus hardens).
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_states, payer,
    organized, generational, constrained, universal).

% Coal, oil refining, cement, steel, and petrochemical sectors face regulatory extinction under binding NDC enforcement. The constraint forces asset devaluation, workforce displacement, and loss of market access in jurisdictions with strong climate accountability. Their exit is partial diversification or relocation; staying in high-regulating jurisdictions becomes increasingly costly as enforcement tightens.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, universal).

% Nations in development transition (India, Brazil, Indonesia, Vietnam) face NDCs that constrain energy-intensive industrialization pathways that wealthy nations used historically. They benefit from technology transfer and finance commitments in theory; they pay through growth constraints and industrial policy limitations. Their exit is degraded because development pathways are locked by historical emissions budgets.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, industrializing_developing_nations, payer,
    moderate, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, industrializing_developing_nations, beneficiary).

% EU, UK, US, Canada set the NDC framework's enforcement standards through UNFCCC consensus and bilateral climate diplomacy. They pay through de-carbonization costs, industrial restructuring, and technology-transfer finance obligations. Their exit options are highest: they can modulate ambition, delay enforcement, or invoke climate-finance delays without face-losing framework exit (unlike fossil exporters).
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, wealthy_developed_states, agenda_setter,
    institutional, civilizational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, wealthy_developed_states, payer).

% Solar, wind, battery, hydrogen equipment makers capture market growth driven by NDC compliance mandates. They benefit from regulatory forcing of their technologies into economies. Their exit options are strong: they serve multiple jurisdictions and can relocate supply chains in response to policy shifts. They have incentive to strengthen enforcement.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_manufacturers, beneficiary,
    powerful, biographical, arbitrage, universal).

% The World Bank climate finance arm, UN climate trust funds, and multilateral development banks grow in mission and capital allocation as NDC binding status demands climate conditionality on development lending. They administer the wealth transfer, set its terms, and benefit from expanded institutional authority. Their exit options are institutional repositioning; their primary interest is in NDC enforcement strengthening their gatekeeper role.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_climate_finance_institutions, beneficiary,
    institutional, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, international_climate_finance_institutions, agenda_setter).

% The UNFCCC operates the NDC registry, reviews progress reports, and administers the compliance measurement framework. Their role expands with binding enforcement; they become arbiters of what counts as compliance and interpreters of ambition levels. They have no exit—they are the mechanism itself—and their institutional survival depends on enforcement credibility.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, unfccc_compliance_secretariat, agenda_setter,
    institutional, generational, trapped, universal).

% Labor-intensive manufacturing sectors (automotive, chemicals, construction) in regions without cheap renewable energy face regulatory obsolescence and must transition or exit. Workers in these sectors pay through job loss and wage compression. Individual firms can relocate; workers and regional economies cannot.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, corporate_transition_losers, payer,
    moderate, biographical, constrained, universal).

% Sovereigntist political movements opposing international climate binding commitments as erosions of national energy autonomy are structurally excluded from the framework's authority chain. They argue NDCs represent unaccountable supranational governance; the framework's binding enforcement strengthens their marginalization and delegitimizes their policy voice.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_denying_sovereigntist_coalitions, excluded,
    moderate, biographical, identity_locked, global).

% Climate economists, systems analysts, and international relations scholars assess the constraint's structure, enforcement trajectory, and distributional consequences across seats. They occupy no policy power but carry epistemic authority in framing what the constraint is doing.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__supranational_reading, international_climate_finance_institutions).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__supranational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the atmosphere-as-commons tragedy: uncoordinated national energy policy produces global warming that no nation can solve alone. NDCs as binding commitments with escalating targets create a coordination mechanism that allows each state to commit to reductions credibly (observable, reviewable, sanctionable) rather than free-riding. The supranational reading holds that binding status is essential to this function—voluntary pledges do not overcome the tragedy-of-the-commons incentive structure.
% TRANSFER_FUNCTION: Moves stranded assets from fossil-fuel-dependent economies to renewable-energy supply chains; moves climate-finance capital from wealthy states to vulnerable states and renewable projects; moves labor value from carbon-intensive sectors to clean-energy sectors (with displacement costs); moves regulatory authority upward from national energy policy to international climate-accountability mechanisms.
% ABSENT_VOICES: Fossil-fuel workers whose livelihoods depend on coal and oil extraction; energy-sovereignty advocates who oppose supranational climate governance as unaccountable; subnational jurisdictions whose energy infrastructure sits at regulatory risk from centrally-set NDC targets; indigenous peoples whose land is affected by transition projects (both renewable infrastructure and extraction prevention).
% DISAPPEARANCE_RATIONALE: If binding NDC enforcement disappeared, global emissions trajectory would immediately revert to the country-by-country voluntary-pledge baseline (Paris pre-2018); long-term heating projections would shift upward by 0.5–1.0 degrees Celsius; the renewable-energy industrial base would contract; international climate finance would evaporate; vulnerable nations would lose institutional leverage for loss-and-damage claims. The global energy, finance, and geopolitical order would reorganize around national energy sovereignty rather than binding supranational carbon budgets.
% FOUNDING_PROBLEM: The Paris Framework (2015) was built to solve the acceleration of climate damages from runaway atmospheric CO2, and to establish that national energy policy cannot be treated as purely sovereign when its externalities are planetary. The supranational reading instantiates the founding problem as: 'The tragedy of the commons in the global atmosphere requires binding, escalating, internationally-accountable commitments to overcome free-riding incentives; voluntary pledges have proven inadequate.'
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and the IPCC attest that warming acceleration is unambiguous and that limiting it to 1.5–2.0 C requires near-zero global emissions by 2050 (empirical claim, outside beneficiary frame). Wealthy-state negotiators and climate finance advocates attest binding enforcement is necessary (inside beneficiary frame). Fossil-fuel states and sovereigntist economists argue the founding problem is overestimated and that adaptation, not transformation, is the sustainable path (outside beneficiary frame). The empirical warming record and the emissions-reduction feasibility studies (from multiple research communities) corroborate the problem statement; disagreement centers on the supranational solution, not the problem itself.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The supranational reading scores high on extractiveness (0.81 at interval end) because the constraint's power to ratchet targets and enforce compliance through financial sanctions and regulatory pressure is concentrated in wealthy-state and international-institution hands, while the burden of transition falls on carbon-dependent economies with no equivalent enforcement power over high-emitting wealthy nations. Suppression is high (0.74) because the framework actively suppresses fossil-fuel industry capacity (regulatory bans, stranded-asset forcing) and suppresses the sovereigntist policy voice (by making unilateral energy autonomy increasingly costly). Theater ratio declines over the interval (0.42 to 0.28) because the framework's initially rhetorical commitment to 1.5 C targets is increasingly backed by actual enforcement mechanisms (national-level carbon pricing, trade carbon borders, development-bank climate conditionality), so the gap between declared ambition and functional enforcement narrows. The measurement series run on one shared grid: every metric is measured at times 0, 5, 10, 15, 20, 30 so temporal analysis has consistent sampling. The rising extractiveness trajectory models rent-seeking layered onto coordination: early in the framework (t=0), the coordination story dominates and extraction appears moderate; as enforcement machinery hardens and ratcheting escalates, the extraction component becomes visible and dominates (t=30).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (wealthy developed states, international climate finance institutions), the arrangement is genuine coordination solving the atmosphere commons; their seat-level computation should yield rope or tangled rope with manageable extraction. From the payer seats (fossil-fuel exporters, carbon-intensive industries, industrializing nations), the same structure operates as enforced extraction: their seat-level computation should yield snare or high-extraction tangled rope. The divergence arises because the supranational reading instantiates a constraint whose enforcement asymmetry is structural: those who set the rules are themselves constrained by less extractive alternatives (they can modulate NDC targets, delay enforcement, or invoke technology-transfer delays) while those who bear the costs face regulatory elimination with no modulation option.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality mapping: climate_vulnerable_nations carry d ≈ 0.1–0.2 (they benefit from enforcement without bearing major transition costs; their exit is identity-locked to the constraint because they face climate impacts regardless). Fossil_fuel_exporting_states and carbon_intensive_industries carry d ≈ 0.85–0.95 (they are the primary targets; their exit is severely constrained; extraction falls directly on them). Industrializing_developing_nations carry d ≈ 0.6–0.7 (they face growth constraints and de-industrialization risk; they benefit from finance commitments in theory but pay through industrial-policy limits; their exit is constrained because development pathways are locked by historical emissions budgets). Wealthy_developed_states carry d ≈ 0.45–0.55 (they benefit from framework authority and green-industrial capture; they pay through de-carbonization costs; their exit options are highest, so directionality is near-symmetric). Renewable_energy_manufacturers carry d ≈ 0.15–0.25 (they capture market growth without bearing major costs; their exit options are strong arbitrage). International_climate_finance_institutions carry d ≈ 0.2–0.3 (they benefit from expanded mission and gatekeeper authority; they bear no direct costs; their exit is institutional repositioning). UNFCCC_compliance_secretariat carries d ≈ 0.3–0.4 (they benefit from increased authority and staffing; they pay through institutional burden; their exit is trapped—they are the mechanism).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no sign of mandatrophy: the founding problem (atmosphere-as-commons tragedy, acceleration of warming damages) remains live; the constraint's function (coordinating binding commitments to overcome free-riding) directly addresses that problem; and the constraint persists because beneficiaries (climate-vulnerable nations, renewable manufacturers, wealthy states that capture green-industrial rents) maintain institutional pressure to strengthen enforcement. The theater ratio decline (0.42 → 0.28) might appear to signal mandatrophy, but it signals the opposite: the framework is shifting from rhetorical to enforcement-backed operation. Mandatrophy would manifest as theater ratio rising above 0.5 (compliance becomes performative while real function atrophies); here, enforcement is hardening while rhetoric remains consistent, so the ratio is falling. The constraint is not a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_voluntary_interpretive_gap,
    'Is Article 4''s language establishing ''binding'' commitments (as the supranational reading claims) or ''nationally determined'' voluntary pledges (as the sovereigntist reading claims), or is the binding question itself deferred to state-by-state interpretation?',
    'UNFCCC Conferences of the Parties interpretive rulings (formal decisions) that establish enforcement protocols and sanctions; natural experiments from jurisdictions that treat NDCs as legally binding in domestic law (EU, UK) versus those treating them as aspirational targets. Meta-analysis of state practice in compliance and non-compliance consequences.',
    'If binding interpretation prevails at the interpretive authority level, the supranational reading''s enforcement structure becomes authoritative and the sovereigntist reading is foreclosed within UNFCCC frameworks (though sovereigntist states can withdraw). If voluntary interpretation prevails, extraction appears lower and the constraint reclassifies toward Rope. The binding question is NOT empirical; it is a reading-dependent authority construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_voluntary_interpretive_gap, conceptual, 'Whether Article 4 language instantiates binding or voluntary commitments is an interpretive authority question, not a fact claim.').

omega_variable(
    common_but_differentiated_responsibilities_compatibility,
    'Are binding supranational enforcement targets compatible with Common But Differentiated Responsibilities (CBDR), or do binding ratcheting targets require equal obligations regardless of development level?',
    'UNFCCC rulings on NDC adequacy standards and whether developed-nation targets are held to different criteria than developing-nation targets. Empirical measurement of whether enforcement (sanctions, finance conditionality) treats developed and developing nations symmetrically or asymmetrically.',
    'If CBDR-compatible binding is established, the equity reading and supranational reading coexist (both support binding with differentiated obligations). If binding requires equal ratcheting for all states, the supranational reading forecloses CBDR-strong interpretations of equity reading and creates higher extraction for industrializing nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_but_differentiated_responsibilities_compatibility, conceptual, 'Whether binding supranational commitments can be structured around differentiated obligations or require equal obligations regardless of development.').

omega_variable(
    climate_finance_adequacy_gap,
    'What is the actual cost of NDC compliance for developing nations, and how much of that cost does international climate finance cover? Is the finance-to-cost ratio sufficient to make binding targets non-extractive for developing-nation payers?',
    'Systematic cost-of-compliance studies disaggregated by development level; tracking of actual climate finance disbursements vs. promised commitments; country-level analysis of whether NDC compliance is feasible with committed finance.',
    'If finance-to-cost ratio is high (>80%), extraction for developing-nation payers is lower and the constraint may reclassify toward genuine Rope for those seats. If ratio is low (<50%), extraction remains high and developing nations remain targets. This is the empirical anchor for the ''whether vulnerable nations benefit or pay'' question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_finance_adequacy_gap, empirical, 'Whether international climate finance sufficiently covers developing-nation compliance costs or leaves them as net payers.').

omega_variable(
    enforcement_mechanisms_credibility,
    'Are the enforcement mechanisms (reputational sanctions, trade carbon borders, development-bank climate conditionality, carbon pricing regimes) sufficient to actually constrain emissions from wealthy-nation and industry payers, or do they remain largely performative pressure?',
    'Time-series tracking of emissions reductions in binding-target jurisdictions (EU, UK, wealthy signatories) vs. voluntary-pledge-only jurisdictions; measurement of whether trade carbon borders and development-bank climate conditionality actually shift behavior or are bypassed through political exemptions.',
    'If enforcement mechanisms are credible, the suppression measurement (0.74) is conservative and extraction is real. If enforcement is performative, suppression is lower and the constraint reclassifies toward theater-dominated Piton or high-theater Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanisms_credibility, empirical, 'Whether the supranational reading''s enforcement architecture actually changes behavior or remains largely theatrical.').

omega_variable(
    sovereigntist_political_resilience,
    'Will sovereigntist opposition to supranational climate binding mount a successful political challenge to UNFCCC authority (e.g., through major-state withdrawal, creation of alternative frameworks, or undermining of compliance sanctions), or does supranational binding become increasingly institutionalized?',
    'Political trajectory of sovereigntist movements and their influence on state negotiating positions; measurement of whether UNFCCC authority is strengthened or weakened in successive Conferences of the Parties; natural experiments from major-state Framework withdrawals (e.g., US Paris withdrawal 2017–2021) and their consequences.',
    'If sovereigntist challenges succeed, the supranational reading''s enforcement architecture weakens, suppression declines, and the constraint may revert toward voluntary-pledge coordination (Rope) with lower extraction. If supranational binding becomes entrenched, the reading''s forecast of rising extraction and suppression holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereigntist_political_resilience, preference, 'Political question: whether sovereigntist opposition can successfully challenge the supranational interpretation of Article 4.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(pari_tr_t0, observed).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__supranational_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(pari_tr_t5, observed).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__supranational_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(pari_tr_t10, observed).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__supranational_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(pari_tr_t15, observed).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__supranational_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(pari_tr_t20, projected).
narrative_ontology:measurement(pari_tr_t30, paris_article_4_ndc__supranational_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(pari_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(pari_be_t0, observed).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__supranational_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement_basis(pari_be_t5, observed).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__supranational_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(pari_be_t10, observed).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__supranational_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement_basis(pari_be_t15, observed).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__supranational_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement_basis(pari_be_t20, projected).
narrative_ontology:measurement(pari_be_t30, paris_article_4_ndc__supranational_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(pari_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(pari_su_t0, observed).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__supranational_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(pari_su_t5, observed).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__supranational_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(pari_su_t10, observed).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__supranational_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(pari_su_t15, observed).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__supranational_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(pari_su_t20, projected).
narrative_ontology:measurement(pari_su_t30, paris_article_4_ndc__supranational_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement_basis(pari_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__supranational_reading, 0.18).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc_sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc_equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, unfccc_compliance_measurement_framework).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, carbon_border_adjustment_mechanisms).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, multilateral_development_bank_climate_conditionality).

% DUAL FORMULATION NOTE:
% The paris_article_4_ndc kernel instantiates three distinct constraint stories: (1) supranational_reading (this file) — NDCs as binding, escalating, internationally-accountable commitments with enforcement teeth; (2) sovereigntist_reading — NDCs as voluntary self-determined pledges preserving national energy sovereignty; (3) equity_reading — NDCs structured by Common But Differentiated Responsibilities distinguishing obligations between developed and developing states. All three read the same Article 4 text; they differ in what 'binding' means and how 'nationally determined' interacts with international accountability and differentiation. The supranational reading forecloses the sovereigntist reading (binding enforcement is incompatible with unilateral energy autonomy within a single framework); it coexists with the equity reading (both support binding commitments; they differ on how binding interacts with differentiation). This story's high extraction (0.81) reflects the supranational reading's structure: enforcement asymmetry concentrates power in agenda-setter and international-institution seats while costs fall on carbon-dependent payers. The sovereigntist reading would instantiate lower extraction and higher accessibility to exit (national energy sovereignty preserves more autonomy). The equity reading would maintain high binding but restructure extraction asymmetry to track development level. Sibling stories are organized as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
