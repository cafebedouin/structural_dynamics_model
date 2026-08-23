% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary — Capital Supremacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint story instantiates the capital_supremacy_reading of the
 *   nafta_jurisdictional_boundary kernel. The reading holds that NAFTA's text
 *   (and successor agreements) establishes trade agreement provisions as
 *   supreme law that overrides domestic regulatory standards, with capital
 *   mobility and regulatory harmonization as mandatory treaty obligations.
 *   The structural delta from sibling readings: domestic labor and
 *   environmental standards enter the victim set; subnational regulatory
 *   agencies lose jurisdictional authority; extraction flows upward to
 *   multinational corporations, financial capital, and the ISDS practitioner
 *   class. The claimed type is tangled_rope because the arrangement genuinely
 *   coordinates market access (a real collective-action problem) while
 *   simultaneously extracting through the supremacy mechanism that overrides
 *   non-trade domestic policy. The engine will compute per-seat
 *   classifications from the stakeholder surface; this commentary records the
 *   authoring seat's structural analysis.
 *
 * KEY AGENTS:
 *   - multinational_corporations: Primary beneficiary (institutional/arbitrage) — collect extraction via ISDS and regulatory chill
 *   - financial_capital: Primary beneficiary (institutional/arbitrage) — capital mobility guaranteed as supreme value
 *   - investor_state_arbitration_practitioners: Secondary beneficiary (organized/mobile) — professional rents from dispute infrastructure
 *   - domestic_labor_standards: Primary victim (powerless/trapped) — overridden by supremacy clause
 *   - domestic_environmental_standards: Primary victim (powerless/trapped) — overridden by supremacy clause
 *   - subnational_regulatory_agencies: Victim (organized/constrained) — lose jurisdictional authority, persist as hollowed shells
 *   - worker_collectives: Victim (organized/constrained) — bear wage/protection losses, limited exit
 *   - local_communities_affected_by_deregulation: Victim (powerless/trapped) — bear environmental/health externalities, no exit
 *   - federal_governments: Agenda_setter (institutional/arbitrage) — signed treaty, administer ISDS defense, dual-positioned
 *   - trade_legal_academy: Observer (analytical/analytical) — produces the interpretive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Jurisdictional Boundary — Capital Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '419e0f6f-6dee-4caf-8b37-4c213b7c4d65').
narrative_ontology:cs_kernel_codification('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', formalized).
narrative_ontology:cs_authority_grounding('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', lineage).
narrative_ontology:cs_interpretation_layer_present('419e0f6f-6dee-4caf-8b37-4c213b7c4d65').
narrative_ontology:cs_reading_relation('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', foundational, treaty_text_as_supreme_law_over_domestic_standards).
narrative_ontology:cs_axiom_status(treaty_text_as_supreme_law_over_domestic_standards, holdable).
narrative_ontology:cs_axiom_grounding('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', treaty_text_as_supreme_law_over_domestic_standards, conventional).
narrative_ontology:cs_axiom('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', foundational, capital_mobility_as_non_derogable_treaty_value).
narrative_ontology:cs_axiom_status(capital_mobility_as_non_derogable_treaty_value, holdable).
narrative_ontology:cs_axiom_grounding('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', capital_mobility_as_non_derogable_treaty_value, conventional).
narrative_ontology:cs_reference_frame('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', nafta_original_credible_commitment_framework).
narrative_ontology:cs_drift_state('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', post_cusma_renegotiation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('419e0f6f-6dee-4caf-8b37-4c213b7c4d65', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_capital).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_practitioners).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_standards).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, worker_collectives).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities_affected_by_deregulation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, federal_governments).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, capital_mobility_as_supreme_value).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, regulatory_harmonization_as_treaty_obligation).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_text_as_supreme_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use ISDS and regulatory chill to secure regulatory outcomes favorable to cross-border operations. Capital mobility lets them shift production and investment to jurisdictions with lower standards, while the supremacy clause prevents host states from raising standards. They collect the extraction directly through avoided compliance costs and ISDS awards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Capital mobility guaranteed as supreme treaty value means financial flows cannot be restricted by domestic regulation. The supremacy clause ensures that capital controls, prudential regulation, and financial transaction taxes are treated as treaty violations. Extraction accrues as unregulated rent on cross-border financial intermediation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_capital, beneficiary,
    institutional, generational, arbitrage, global).

% A specialized legal-technical class that administers the ISDS mechanism. They benefit from professional fees, institutional prestige, and the expansion of investment treaty arbitration as a field. Their interest aligns with maintaining and expanding the supremacy clause's reach.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_practitioners, beneficiary,
    organized, biographical, mobile, global).

% Minimum wages, collective bargaining rights, occupational safety rules, and employment protections are vulnerable to ISDS challenge as 'indirect expropriation' or 'fair and equitable treatment' violations. Workers cannot exit the jurisdiction; the standards bear the full cost of regulatory chill and adverse awards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards, payer,
    powerless, biographical, trapped, national).

% Pollution limits, resource extraction restrictions, climate policies, and ecosystem protections face ISDS challenge when they reduce investor returns. Communities bearing environmental harm have no exit; the standards absorb the regulatory chill and the cost of foregone protection.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_standards, payer,
    powerless, generational, trapped, national).

% State/provincial environmental agencies, labor ministries, and health regulators lose jurisdictional authority when federal governments defend ISDS claims by preempting subnational standards. Agencies persist as hollowed shells — formal authority remains but effective regulatory capacity is transferred to treaty compliance offices. They cannot exit the federal system but are constrained by it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_regulatory_agencies, payer,
    organized, biographical, constrained, regional).

% Unions and worker organizations bear wage suppression and protection losses from regulatory chill. They can organize politically but face capital mobility threats (plant closure, relocation) that discipline demands. Their exit is constrained by national labor markets and the treaty's structural power.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, worker_collectives, payer,
    organized, biographical, constrained, national).

% Communities bearing pollution, resource depletion, and health externalities from deregulated investment have no meaningful exit. They are not parties to the treaty and have no standing in ISDS. The constraint's extraction lands on them as diffuse, uncompensated harm.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities_affected_by_deregulation, payer,
    powerless, biographical, trapped, local).

% Signed and ratified the treaty; administer ISDS defense; set federal regulatory policy within treaty constraints. Dual-positioned: they control the treaty machinery (agenda_setter) but bear political costs from victim constituencies and pay ISDS awards/legal costs (payer). Their exit is arbitrage-grade — they could withdraw from the treaty but face massive geopolitical and economic costs.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, federal_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, federal_governments, payer).

% Produces the interpretive framework that legitimizes the supremacy reading. Generates the doctrinal vocabulary (indirect expropriation, fair and equitable treatment, regulatory chill) that operationalizes the constraint. Neither collects nor pays; observes and shapes the epistemic infrastructure.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_legal_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible commitment problem for cross-border investment: investors need assurance that host states will not expropriate or regulate away returns after capital is sunk. The treaty provides a supranational enforcement mechanism (ISDS) that substitutes for domestic judicial credibility.
% TRANSFER_FUNCTION: Moves regulatory value (the right to set labor, environmental, health, and financial standards) from domestic publics and subnational governments to mobile capital and its legal-technical intermediaries, via the supremacy clause and ISDS mechanism. The transfer is upward (subnational → national → supranational) and outward (public → private).
% ABSENT_VOICES: Future generations who bear the environmental and climatic costs of regulatory foreclosure; indigenous communities whose territorial rights are not recognized in treaty text; informal sector workers with no collective bargaining standing; small states pressured into ISDS acceptance via accession conditionalities. These voices are structurally excluded from the treaty negotiation and dispute resolution processes.
% DISAPPEARANCE_RATIONALE: If the supremacy clause and ISDS mechanism vanished overnight, domestic regulatory authority would revert to national and subnational legislatures; capital mobility would remain but without treaty-guaranteed supremacy; ISDS practitioners would lose their jurisdiction; multinational corporations would face domestic courts for regulatory disputes. The global investment regime would reorganize around domestic legal systems and political negotiation rather than supranational arbitration.
% FOUNDING_PROBLEM: In the early 1990s, North American cross-border investment was deterred by perceived judicial bias and regulatory unpredictability in Mexico, and by US/Canadian investor concerns about Mexican policy stability. The treaty provided a credible commitment mechanism: supranational arbitration would discipline state behavior, unlocking capital flows.
% FOUNDING_PROBLEM_CORROBORATION: Capital_supremacy_reading proponents (US Trade Representative, multinational business associations) attest the problem remains live — citing ongoing regulatory risk in emerging markets. Embedded_liberalism_reading proponents (Canadian/EU trade ministries, labor NGOs, development economists) attest the founding problem is substantially solved — modern domestic courts provide credible commitment; the arrangement persists as rent collection. Sovereignty_primacy_reading proponents (global south governments, constitutional scholars) attest the problem was mis-specified — the treaty imposed a northern legal template that never matched southern institutional realities. Corroboration from outside the beneficiary set: legislative-hearing testimony, independent economic analysis (e.g., UNCTAD ISDS reviews), and renegotiation outcomes (CUSMA Chapter 11 narrowing) support the shifted-function reading.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.78) is high because the supremacy clause transfers regulatory value from domestic publics to mobile capital without compensation; the ISDS mechanism monetizes regulatory space. Suppression (0.72) is substantial because the constraint actively prevents regulatory alternatives through treaty text and ISDS threat — not merely lack of alternatives but active foreclosure. Theater ratio (0.41) is moderate-high: the market-access coordination function is real but a declining share of the constraint's operational energy; ISDS defense and regulatory chill management are increasingly performative maintenance of the extraction structure. Accessibility collapse (0.58) is partial: alternatives (non-discriminatory regulation, policy space carve-outs) exist conceptually but are structurally foreclosed by the supremacy reading's interpretive dominance. Resistance (0.67) is significant: legislative pushback, judicial narrowing, and renegotiation (CUSMA) demonstrate active contestation. The measurement series shows monotonic accumulation of extraction and suppression over the 30-year interval, with theater rising as coordination function atrophies — a classic tangled_rope drift toward snare.
 *
 * PERSPECTIVAL GAP:
 *   From the multinational_corporations and financial_capital seats (beneficiaries, d near 0.1): the constraint is genuine coordination — it solves the credible commitment problem for cross-border investment. From the domestic_labor_standards and domestic_environmental_standards seats (victims, d near 0.9): the same structure is enforced extraction — their protective value is transferred upward. From the subnational_regulatory_agencies seat (victim, d ~0.7): institutional capacity is hollowed out while formal authority persists (theater). From the federal_governments seat (agenda_setter, d ~0.4): dual-positioned — they administer the constraint but also bear political costs from victim constituencies. The engine computes these divergences from the structural data; the claimed_type does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: multinational_corporations, financial_capital, investor_state_arbitration_practitioners — these agents collect rents from the supremacy clause (ISDS awards, regulatory freedom, professional fees). Victims declared: domestic_labor_standards, domestic_environmental_standards, subnational_regulatory_agencies, worker_collectives, local_communities_affected_by_deregulation — these agents bear the costs of overridden standards, lost jurisdiction, and externalized harms. Federal_governments are dual-positioned (agenda_setter in stakeholders, not listed in beneficiaries/victims): they signed the treaty and administer ISDS defense but face domestic political costs. The derivation chain assigns low d to beneficiaries (arbitrage-grade exit via capital mobility), high d to victims (trapped or constrained exit), and intermediate d to federal_governments. The override for federal_governments (d=0.4) corrects the derivation which would otherwise place them nearer the beneficiary end due to institutional power and arbitrage exit — the political cost from victim constituencies shifts them toward symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible commitment for cross-border investment in 1990s North America) is contested: capital_supremacy_reading holders say it remains live; embedded_liberalism_reading holders say it is substantially solved and the arrangement persists as rent collection; sovereignty_primacy_reading holders say the founding problem was mis-specified. The constraint shows mandatrophy indicators: theater rising, extraction accumulating, sunset clause absent, enforcement hardening. The classification as tangled_rope (not snare) preserves the coordination function as live — preventing false 'pure extraction' labeling that would miss the genuine market-access problem the treaty solved. But the drift trajectory suggests the coordination function is atrophying; if extraction continues accumulating and theater crosses 0.5, reclassification toward snare or piton becomes warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the nafta_jurisdictional_boundary kernel, or a free-standing constraint?',
    'Compare structural delta: this reading places domestic standards in victim set and capital mobility beneficiaries in beneficiary set; sibling readings do not. If delta holds across all structural atoms, it is a distinct reading.',
    'If distinct reading, cs_structure.reading_relations and axioms apply; if free-standing, kernel context is misattribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment of this constraint to the nafta_jurisdictional_boundary kernel as capital_supremacy_reading').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the treaty''s market-access coordination function genuinely require the supremacy clause that overrides domestic standards, or is the supremacy clause extractive surplus?',
    'Counterfactual: if the supremacy clause were narrowed to non-discrimination only (per embedded_liberalism_reading), would market access coordination fail? Empirical evidence from CUSMA Chapter 19 modifications and CPTPP side letters.',
    'If coordination survives without supremacy, extraction is separable → tangled_rope confirmed. If coordination fails, supremacy is coordination cost → rope-adjacent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the coordination and extraction components of the supremacy obligation are structurally separable').

omega_variable(
    regulatory_chill_measurement,
    'How much of the measured suppression is anticipatory regulatory chill vs. active ISDS enforcement?',
    'Compare pre- and post-ISDS filing regulatory trajectories in affected domains; text-analysis of legislative records for ''treaty compliance'' justifications.',
    'If chill dominates, suppression is internalized and persists after enforcement machinery changes; if enforcement dominates, suppression is structural and removable by treaty amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_measurement, empirical, 'Structural vs. internalized suppression mechanism in the regulatory chill dynamic').

omega_variable(
    victim_set_boundary,
    'Are subnational regulatory agencies victims in their own right, or only as proxies for the standards they administer?',
    'Track institutional survival: do agencies persist with hollowed mandates, or are they restructured/eliminated? Measure budgetary and personnel flows.',
    'If agencies are independent victims, the constraint extracts institutional capacity as well as policy outcomes; if only proxies, victim set collapses to the standards themselves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Whether regulatory agencies or the standards they administer are the primary victim of jurisdictional override').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t0, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t8, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t16, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t24, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t0, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t8, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t16, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t24, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t0, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t8, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t16, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t24, observed).
narrative_ontology:measurement(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(nafta_jurisdictional_boundary__capital_supremacy_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, cisg_harmonization).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, wto_tbt_agreement).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, isds_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the nafta_jurisdictional_boundary kernel. The capital_supremacy_reading declares trade text as supreme law overriding domestic standards (this story). The embedded_liberalism_reading declares trade text as framework balancing market access with domestic policy space. The sovereignty_primacy_reading declares trade text as subordinate coordination mechanism. All three share the kernel_id but instantiate different constraints with different ε, different beneficiary/victim structures, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__capital_supremacy_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
