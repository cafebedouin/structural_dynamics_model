% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Speed-Priority Mitigation Allocation Rule (Opportunity-Cost Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the opportunity_cost_reading — of
 *   the contested kernel climate_mitigation_imperative. The constraint it
 *   authorizes is an allocation norm: mitigation capital, concessional
 *   finance, and policy attention must rank candidate technologies by avoided
 *   carbon per dollar per year, and options whose capital intensity and build
 *   timelines make them slow contributors are treated as net-harmful under a
 *   hard carbon budget. Under this reading nuclear power is the structural
 *   victim — not merely ranked lower but affirmatively labeled harmful, which
 *   closes finance channels — while the renewables-and-storage complex is the
 *   structural beneficiary. The rule solves a genuine problem (finite
 *   budgets, stock-driven warming, early tons worth more) while imposing
 *   concentrated costs on one sector through the same structure, which is why
 *   it is claimed as tangled_rope rather than rope. Sibling readings
 *   (portfolio_optimization_reading, systems_transition_reading) instantiate
 *   different constraints from the same kernel and are authored as separate
 *   files linked through network.affects_constraints; per the
 *   epsilon-invariance principle this file does not average over them.
 *   Epsilon's referent is the standing speed-priority allocation regime as
 *   this reading sees it: the reading regards the rule as justified triage,
 *   so it authors moderate base extraction while openly acknowledging the
 *   concentrated costs the rule imposes on nuclear — those costs are the
 *   reading's own central claim, not a hidden one. The claim and the metrics
 *   are independent authored facts; divergence between them is signal, not
 *   error.
 *
 * KEY AGENTS:
 *   - renewable_energy_industry: primary beneficiary (powerful/arbitrage) — receives the capital, contracts, and priority status the ranking directs
 *   - nuclear_power_sector: primary target (organized/trapped) — bears finance exclusion, capital diversion, and the net-harm label
 *   - green_finance_institutions: agenda setter (institutional/arbitrage) — administers the screens and taxonomies through which the rule binds
 *   - climate_advocacy_coalition: agenda setter (organized/identity_locked) — enforces the norm discursively; parts of the coalition carry a pre-climate anti-nuclear identity
 *   - electricity_consumers: dual-positioned (beneficiary + payer) — rapid cheap abatement, plus integration and reliability costs
 *   - emerging_economy_grid_planners: coordinated participants (moderate/constrained) — conform to access concessional finance
 *   - fossil_gas_producers: incidental beneficiary (powerful/arbitrage) — backfill retired nuclear in several grids
 *   - grid_reliability_engineers: excluded voice (moderate/constrained) — firm-capacity objections fall outside the metric's frame
 *   - energy_systems_analysts: analytical observer — computes portfolio outcomes across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.55).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.56).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Speed-Priority Mitigation Allocation Rule (Opportunity-Cost Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '5cdb4924-047d-42c0-8515-78f55eb64ff8').
narrative_ontology:cs_kernel_codification('5cdb4924-047d-42c0-8515-78f55eb64ff8', distributed).
narrative_ontology:cs_authority_grounding('5cdb4924-047d-42c0-8515-78f55eb64ff8', distributed).
narrative_ontology:cs_reading_relation('5cdb4924-047d-42c0-8515-78f55eb64ff8', climate_mitigation_imperative__portfolio_optimization_reading, influences).
narrative_ontology:cs_reading_relation('5cdb4924-047d-42c0-8515-78f55eb64ff8', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('5cdb4924-047d-42c0-8515-78f55eb64ff8', foundational, fastest_avoided_ton_per_dollar_maximizes_climate_value).
narrative_ontology:cs_axiom_status(fastest_avoided_ton_per_dollar_maximizes_climate_value, holdable).
narrative_ontology:cs_axiom_grounding('5cdb4924-047d-42c0-8515-78f55eb64ff8', fastest_avoided_ton_per_dollar_maximizes_climate_value, empirically_contingent).
narrative_ontology:cs_axiom('5cdb4924-047d-42c0-8515-78f55eb64ff8', foundational, capital_scarcity_makes_opportunity_cost_decisive).
narrative_ontology:cs_axiom_status(capital_scarcity_makes_opportunity_cost_decisive, holdable).
narrative_ontology:cs_axiom_grounding('5cdb4924-047d-42c0-8515-78f55eb64ff8', capital_scarcity_makes_opportunity_cost_decisive, empirically_contingent).
narrative_ontology:cs_reference_frame('5cdb4924-047d-42c0-8515-78f55eb64ff8', scarce_capital_triage_framework).
narrative_ontology:cs_drift_state('5cdb4924-047d-42c0-8515-78f55eb64ff8', post_2021_energy_security_realignment, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5cdb4924-047d-42c0-8515-78f55eb64ff8', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, electricity_consumers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, emerging_economy_grid_planners).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, fossil_gas_producers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_power_sector).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_advocacy_coalition).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, electricity_consumers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, emerging_economy_grid_planners).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, avoided_carbon_per_dollar_per_year_ranking).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, cumulative_carbon_budget_urgency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Development banks, taxonomy bodies, and ESG fund managers apply the avoided-carbon-per-dollar-per-year screen when deciding which projects count as green and which receive concessional terms. They administer the criteria through which the ranking binds borrowers and issuers, and they can revise the screens, though revision carries coalition and reputational costs across their member governments.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, green_finance_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Environmental organizations and speed-focused campaign networks press the rule in media, shareholder forums, and legislative testimony, treating slow-build options as a threat to the carbon budget. Parts of the coalition carry an anti-nuclear commitment that predates climate advocacy, so abandoning the frame would cost members a piece of organizational identity, not just a policy position. The rule's salience also anchors their fundraising and coalition coherence.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_advocacy_coalition, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, climate_advocacy_coalition, beneficiary).

% Solar, wind, storage, and grid-equipment developers and manufacturers receive the capital, contracts, and policy priority that the ranking directs toward the top of the curve. Their short build cycles and modular supply chains let them convert each unit of steered finance into deployed capacity quickly, and they operate across jurisdictions, so they can shift sales toward whichever markets adopt the rule most strongly.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Utilities, reactor vendors, fuel-cycle firms, and their workforces bear the rule's costs: green-finance channels stay closed, capital diversion raises borrowing costs, projects are cancelled on opportunity-cost grounds, and the net-harmful designation attaches to the technology itself. Reactor assets are licensed, site-specific, and multi-decade, so firms cannot redeploy plant, skills, or certifications into another line of business; state-backed programs in a few countries are the main reason the sector persists at scale.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_power_sector, payer,
    organized, generational, trapped, global).

% Households and firms receive rapid, comparatively cheap decarbonization of the generation mix where the rule governs procurement. They also absorb the integration costs of weather-dependent output, network buildout, and the reliability risk of foregone firm capacity, and they cannot exit the grid system their charges finance.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, electricity_consumers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, electricity_consumers, payer).

% National planners in developing economies gain access to concessional climate finance by conforming their buildout plans to the ranking, which favors renewables-first pathways. Conforming also means accepting that nuclear options are effectively off the menu, since lenders will not fund them, so the planners trade an open option set for affordable capital and import-dependence on equipment supply chains.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, emerging_economy_grid_planners, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, emerging_economy_grid_planners, payer).

% Gas producers and turbine suppliers capture business the rule never names them for: where nuclear plants retire under opportunity-cost pressure before renewables and storage fully replace them, gas fills the gap, as several European and North American grids have recorded. Their benefit is incidental to the rule's operation rather than conferred by it, and they compete with the rule's stated aim wherever carbon pricing bites.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fossil_gas_producers, beneficiary,
    powerful, biographical, arbitrage, global).

% Firm-capacity and system-operations specialists argue that a per-dollar-per-year metric omits the value of dispatchable low-carbon output, capacity credit, and locational flexibility, and that portfolios built purely on the ranking under-provide these. Their objections surface in technical proceedings but carry little weight in the finance-allocation bodies where the rule is administered, and their employers depend on those bodies for project approval.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_reliability_engineers, excluded,
    moderate, biographical, constrained, national).

% Integrated-assessment modelers, academic energy-system groups, and independent statistical agencies compute portfolio outcomes under alternative allocation rules, publish cost and deployment comparisons, and take no side in the finance contest. Their datasets are the common evidentiary ground on which all readings of the allocation question argue.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, energy_systems_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__opportunity_cost_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates scarce mitigation capital under a stock-driven deadline: rank abatement options by avoided tonnes per dollar per year and fund the top of the ranking first, so that finite political and financial budgets purchase the largest near-term reduction in cumulative emissions.
% TRANSFER_FUNCTION: Moves investment capital, concessional finance, policy attention, and engineering labor away from slow-build, capital-intensive generation toward fast-deploy modular generation; it also transfers a net-harmful designation onto nuclear projects, raising their cost of capital and closing green-finance channels to them.
% ABSENT_VOICES: Grid reliability engineers would object that the metric omits firm-capacity and system-value contributions; nuclear-adjacent planners in developing economies would object that lender exclusions foreclose options before domestic analysis concludes. Both sit outside the green-finance allocation bodies where the rule is administered, so the unanimity behind the ranking partly reflects who was in the room.
% DISAPPEARANCE_RATIONALE: If the speed-priority rule vanished overnight, green-finance screens would lose their ranking criterion, capital currently steered away from nuclear would renegotiate across the portfolio, renewables would lose their priority claim on concessional funds, and the advocacy coalition enforcing the norm would lose its operative frame. Allocation across the entire mitigation finance complex would reorganize.
% FOUNDING_PROBLEM: Early-2000s climate politics faced limited budgets, weak political will, and a widespread perception that nuclear was unbankable after cost overruns and waste disputes. Advocates needed a decisive allocation heuristic, and the claim that the carbon budget could not wait for nuclear construction crystallized into the speed metric.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports corroborate the urgency premise from outside the renewables industry, and national carbon-budget statutes attest it independently. The exclusion corollary is disputed from outside the benefiting parties: IEA net-zero modeling includes substantial nuclear expansion, and grid-operator filings in several jurisdictions contest the too-slow claim. No outside source attests the full rule as stated; the urgency half is corroborated, the nuclear-is-net-harmful half is not.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   End-state base extraction is 0.55: the rule's costs on nuclear are large and concentrated (closed green-finance channels, elevated cost of capital, cancelled projects) but the reading regards the underlying ranking as tracking real cost differences, so extraction is substantial yet bounded. Suppression is 0.56 as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Suppression here is the active maintenance of exclusion (lending bans, taxonomy conditions, discursive net-harm framing) rather than coercion of persons. Theater is 0.32: the allocation function is real, but a growing share of enforcement activity defends the exclusion itself with evidence-based framing as the costume. Accessibility collapse is 0.38 — deliberately low, because the alternative allocation principles do not collapse when this rule is understood; they persist as the sibling readings of the same kernel, which is precisely why this is a contested-kernel story. Resistance is 0.68: an organized counter-coalition (nuclear states, vendor industries, reliability engineers, parts of the investor community) actively contests the rule. The measurement series run on ONE shared time grid (2005-2025, eight points, all three metrics at every point) and are deliberately non-monotonic: extraction and enforcement ratchet up through the Fukushima-era exclusion spike (2011) and peak around 2017-2020 as development-bank exclusions entrench, then ease after 2021 as energy-security shocks, the EU taxonomy compromise, the COP28 tripling pledge, and datacenter demand create reversal pressure. Theater peaks later (2020) than enforcement, consistent with performative maintenance thickening as the empirical case narrows, then receding as the frame is challenged.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seats the arrangement is a rational triage protocol they built and administer; from the nuclear seat the same structure operates as enforced exclusion of a trapped specialist; from the consumer seat it is cheap decarbonization with a reliability surcharge; from the gas seat it is an unearned windfall. The identity-lock dynamic concentrates in the advocacy coalition: for a segment of its members the anti-nuclear commitment predates the climate frame, so the constraint is held by ideological identity fusion — exit is unthinkable not because the arguments fail but because leaving would dissolve an organizational self-concept formed in an earlier struggle. If that identity frame broke, the coalition could accept a portfolio framing, and the rule's enforcement would decay quickly since the finance institutions' commitment is shallower and already shows arbitrage behavior. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable_energy_industry declares as beneficiary with arbitrage-grade exit, placing it near the full-beneficiary end: the rule subsidizes it and it can relocate if any jurisdiction defects. Nuclear_power_sector declares as victim with trapped exit (site-specific licensed assets, non-transferable workforce), placing it near the full-target end — trapped targets amplify effective extraction. Electricity_consumers and emerging_economy_grid_planners are dual-positioned (beneficiary with payer secondary role): genuine coordination benefit, real diffuse costs, landing near symmetric. Fossil_gas_producers are beneficiaries whose benefit is incidental — the derived directionality will likely understate their true distance from the beneficiary pole, but no override is authored: overrides key on the power atom, and gas shares the powerful/arbitrage profile with the renewables industry, so a power-atom-level correction would distort both seats identically. The discrepancy is documented in the incidental_gas_capture omega instead, where a per-agent resolution is possible. Green_finance_institutions derive low directionality as administrators, which is accurate: they set the rule rather than pay it, though revision costs bind them.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabels. Reading the rule as pure rope would erase the asymmetric extraction the reading itself foregrounds: nuclear pays through the same structure that coordinates everyone else, and enforcement machinery (lending exclusions, taxonomy conditions) actively maintains that asymmetry. Reading it as pure snare would erase the genuine coordination function: the carbon budget is stock-driven, budgets are finite, and speed-weighted ranking is a defensible answer to a real collective-action problem — the founding problem is corroborated from outside the benefiting parties on its urgency half. Piton is excluded because the function has not atrophied: the administrator demonstrably profits from administering, and the ranking still moves real money. The R5 mismatch consumer should watch this story closely: founding_problem_status is contested rather than dead, so no zombie flag fires today, but the trajectory (theater rising 2005-2020, suppression_requirement falling from its 2017 peak, extraction easing after 2020) is the signature of an aging mandate whose exclusion corollary has outrun its evidentiary basis while its urgency core remains live. If the corollary dies and the apparatus persists, the story migrates toward piton; if the corollary is vindicated by system-cost evidence, it stabilizes as tangled_rope with lower theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the opportunity_cost_reading of kernel climate_mitigation_imperative; what structurally changes if the portfolio_optimization_reading governs instead?',
    'Classify the sibling story climate_mitigation_imperative__portfolio_optimization_reading and diff the victim/beneficiary sets: nuclear exits the victim set and re-enters the coordinated set; the governing metric becomes portfolio-reliability-weighted rather than carbon-per-dollar-per-year; the systems_transition_reading keeps nuclear in its victim set but on decentralization grounds rather than speed grounds.',
    'Under the sibling reading, this story''s extraction structure inverts for nuclear (directionality falls from target toward beneficiary) and the speed-priority rule reads as under-building firm capacity — identical finance flows classify oppositely across the two files, which is the corpus''s measure of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: which sibling reading would redistribute the victim set and invert nuclear''s directionality.').

omega_variable(
    metric_completeness_firm_capacity,
    'Does avoided-carbon-per-dollar-per-year fully specify the allocation objective, or does omitting firm-capacity value, flexibility value, and transmission costs systematically misprice dispatchable low-carbon sources?',
    'Like-for-like system-cost modeling: construct portfolios meeting identical reliability standards with and without the excluded option, and compare total system cost per tonne avoided rather than generator-level metrics.',
    'If the metric is incomplete, the rule''s extraction exceeds its coordination value and the story drifts toward snare; if the metric is essentially complete, the rule remains a defensible triage protocol and the tangled_rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_completeness_firm_capacity, empirical, 'Whether the ranking metric is a complete specification of the allocation objective.').

omega_variable(
    endogenous_cost_loop,
    'Is nuclear''s observed capital intensity and build timeline partially produced by the exclusion itself — finance denial breaking supply chains and learning curves, which then raises costs and renews the justification for exclusion?',
    'Compare cost and deployment trajectories across jurisdictions that sustained continuous nuclear programs versus those that exited under opportunity-cost pressure, controlling for labor and commodity inputs.',
    'If the loop is real, the rule manufactures part of its own evidence: the vindicated proposition (cost-based ranking) is partly an artifact of the rule''s operation, deepening measured extraction and strengthening the snare-side hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_cost_loop, empirical, 'Whether the constraint''s evidentiary basis is endogenous to its enforcement.').

omega_variable(
    incidental_gas_capture,
    'Do fossil gas producers capture more of the rule''s operation than the named beneficiaries, via backfilling nuclear capacity that retires under opportunity-cost pressure before clean firm replacements arrive?',
    'Decompose the generation mix replacing retired nuclear capacity in rule-governed jurisdictions year by year, separating gas backfill from renewables-and-storage substitution.',
    'Material gas capture would show the rule functioning partly as fossil protection despite its genuine speed rationale, strengthening the snare-side reading and complicating the beneficiary-set declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_gas_capture, empirical, 'Whether an unnamed party captures the rule''s gains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmi_ocr_tr_t2005, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(cmi_ocr_tr_t2009, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2009, 0.24).
narrative_ontology:measurement(cmi_ocr_tr_t2011, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2011, 0.3).
narrative_ontology:measurement(cmi_ocr_tr_t2014, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2014, 0.33).
narrative_ontology:measurement(cmi_ocr_tr_t2017, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2017, 0.36).
narrative_ontology:measurement(cmi_ocr_tr_t2020, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(cmi_ocr_tr_t2022, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2022, 0.34).
narrative_ontology:measurement(cmi_ocr_tr_t2025, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(cmi_ocr_be_t2005, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(cmi_ocr_be_t2009, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2009, 0.46).
narrative_ontology:measurement(cmi_ocr_be_t2011, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2011, 0.52).
narrative_ontology:measurement(cmi_ocr_be_t2014, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(cmi_ocr_be_t2017, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2017, 0.63).
narrative_ontology:measurement(cmi_ocr_be_t2020, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(cmi_ocr_be_t2022, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2022, 0.59).
narrative_ontology:measurement(cmi_ocr_be_t2025, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(cmi_ocr_su_t2005, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(cmi_ocr_su_t2009, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2009, 0.5).
narrative_ontology:measurement(cmi_ocr_su_t2011, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2011, 0.62).
narrative_ontology:measurement(cmi_ocr_su_t2014, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2014, 0.66).
narrative_ontology:measurement(cmi_ocr_su_t2017, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(cmi_ocr_su_t2020, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2020, 0.66).
narrative_ontology:measurement(cmi_ocr_su_t2022, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2022, 0.6).
narrative_ontology:measurement(cmi_ocr_su_t2025, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2025, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the climate mitigation imperative' covers at least three structurally distinct allocation rules, decomposed per the epsilon-invariance principle. This file authors the opportunity_cost_reading (metric: carbon-per-dollar-per-year; nuclear in the victim set; renewables as beneficiaries). The portfolio_optimization_reading (maximize all low-carbon sources; nuclear necessary for firm capacity) and the systems_transition_reading (decentralized democratic control; nuclear as extractive centralization) instantiate different constraints with different epsilon, beneficiary sets, and failure modes, and are authored separately. Direction of influence: the opportunity-cost reading currently dominates green-finance administration, so it exerts structural pressure on the portfolio reading's operating environment (capital availability for nuclear), while coexisting with the systems reading as an allied-but-distinct position targeting the same technology on different grounds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
