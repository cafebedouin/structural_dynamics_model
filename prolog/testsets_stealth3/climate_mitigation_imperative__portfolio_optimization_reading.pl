% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Portfolio-Optimization Reading of the Mitigation Imperative (All Low-Carbon Sources Maximized; Nuclear Necessary Baseload)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   climate_mitigation_imperative. The portfolio-optimization reading
 *   constrains legitimate mitigation policy to technology-inclusive portfolio
 *   maximization: every low-carbon source must be deployed at scale, and
 *   nuclear specifically is declared necessary for reliable firm supply. The
 *   arrangement under contest is the governance regime built on this claim —
 *   carbon targets implemented through source-agnostic accounting, capacity
 *   mechanisms written around firm-low-carbon necessity, and directed
 *   financial support flowing to nuclear operators and vendors. Time mapping
 *   for measurements: T=0 corresponds to 2000, T=24 to 2024, on a four-year
 *   grid. KEY AGENTS (by structural relationship): -
 *   governments_energy_ministries: Agenda setter (institutional/mobile) —
 *   legislates the mandate, funds nuclear support, signs tripling pledges -
 *   capacity_market_regulators: Agenda setter (institutional/constrained) —
 *   encode firm-low-carbon necessity into procurement rules -
 *   nuclear_plant_operators: Primary beneficiary (institutional/constrained)
 *   — collects operating subsidies and guaranteed revenues -
 *   nuclear_vendor_supply_chain: Secondary beneficiary (organized/mobile) —
 *   captures construction-cycle contracts - fossil_generators: Primary target
 *   (powerful/constrained) — loses dispatch share and forward revenue to
 *   mandated displacement - electricity_ratepayers: Target
 *   (moderate/constrained) — bears strike-price top-ups and overrun
 *   pass-throughs - general_taxpayers: Target (powerless/constrained) — funds
 *   credits, guarantees, and export finance diffusely -
 *   alternative_firm_clean_developers: Dual-positioned (moderate/constrained)
 *   — crowded out of the firm slot, subsidized by the breadth clause -
 *   anti_nuclear_civil_society: Excluded voice (organized/constrained) —
 *   contests the clause outside the technical rooms -
 *   energy_modeling_institutions: Analytical observer
 *   (institutional/analytical) — produces the evidentiary frame Claim/metric
 *   independence: the claimed type (tangled_rope) is my structural belief
 *   that this regime possesses BOTH a genuine coordination function
 *   (portfolio decarbonization with reliability) AND asymmetric extraction
 *   (directed transfers plus mandated displacement); the metrics are my
 *   descriptive account of its actual operation. Where the engine's computed
 *   per-seat types diverge from this claim, that divergence is data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.52).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.48).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Portfolio-Optimization Reading of the Mitigation Imperative (All Low-Carbon Sources Maximized; Nuclear Necessary Baseload)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, 'c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8').
narrative_ontology:cs_kernel_codification('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', formalized).
narrative_ontology:cs_authority_grounding('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', expertise).
narrative_ontology:cs_interpretation_layer_present('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8').
narrative_ontology:cs_reading_relation('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', climate_mitigation_imperative__opportunity_cost_reading, influences).
narrative_ontology:cs_reading_relation('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', foundational, firm_zero_carbon_necessity).
narrative_ontology:cs_axiom_status(firm_zero_carbon_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', firm_zero_carbon_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', secondary, source_agnostic_carbon_metric).
narrative_ontology:cs_axiom_status(source_agnostic_carbon_metric, holdable).
narrative_ontology:cs_axiom_grounding('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', source_agnostic_carbon_metric, conventional).
narrative_ontology:cs_reference_frame('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', technology_inclusive_portfolio_equilibrium).
narrative_ontology:cs_drift_state('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', contemporary_netzero_pledge_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c69c0a91-bd47-4a36-b9fd-ac1fa2dd0dc8', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_plant_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_vendor_supply_chain).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_generators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, general_taxpayers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, alternative_firm_clean_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, alternative_firm_clean_developers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, portfolio_diversification_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, firm_low_carbon_necessity_claim).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, source_agnostic_carbon_accounting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates carbon targets, directs finance ministries to back nuclear construction and life extension (loan guarantees, regulated asset base models, permitting reform such as the US ADVANCE Act, EU sustainable-finance taxonomy inclusion), and signs international tripling pledges. Can shift course between electoral cycles; exit looks like changing policy stance, which carries credibility and industrial-strategy costs.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, governments_energy_ministries, agenda_setter,
    institutional, generational, mobile, national).

% Design capacity mechanisms and clean-firm procurement rules (PJM capacity auctions, UK Capacity Market, state clean-firm standards) that operationalize the claim that firm low-carbon capacity is required, effectively writing the necessity clause into market design. Embedded in agency mandates and stakeholder negotiation; rule redesign is possible but slow.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, capacity_market_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Operate existing reactor fleets and pursue license renewals to sixty and eighty years. Collect production tax credits, contract-for-difference top-ups above market price, and regulated returns on new construction. Fleet capital is sunk and license-bound; revenue continuity depends on the policy framework remaining supportive.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_plant_operators, beneficiary,
    institutional, generational, constrained, national).

% Sell reactor designs, heavy components, fuel fabrication, and maintenance services across jurisdictions. Gains concentrate in construction cycles and fleet service agreements. Least captured seat among the beneficiaries: order books are internationally portable, though the customer base narrows if support regimes lapse.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_vendor_supply_chain, beneficiary,
    organized, biographical, mobile, global).

% Lose dispatch share and forward revenue as carbon-intensity mandates tighten: coal plants retire early, gas plants face shrinking running hours and stranded-asset risk. Respond with lobbying, bridge-fuel framing, and selective adoption of technology-neutral language. Cannot repurpose most site-specific capital; exit is partial fuel-switching at best.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_generators, payer,
    powerful, biographical, constrained, global).

% Pay contract-for-difference levies and construction-overrun pass-throughs on bills (for example Georgia Power Vogtle riders, Hinkley Point C top-ups). Cannot leave the grid; rooftop solar and efficiency provide only partial hedges. Costs arrive as line items with limited visibility into the underlying contracts.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers, payer,
    moderate, biographical, constrained, national).

% Fund production tax credits, federal loan guarantees, and export finance for reactor projects through appropriations and tax expenditure. Per-household amounts are diffuse and unorganized; no constituency forms specifically around these flows.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, general_taxpayers, payer,
    powerless, biographical, constrained, national).

% Advanced geothermal, long-duration storage, and demand-flexibility companies compete for the firm-clean designation that procurement reserves for nuclear under the necessity clause, losing revenue-certainty channels to it. They nonetheless gain incidentally from the breadth clause, which subsidizes all low-carbon deployment including theirs.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, alternative_firm_clean_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, alternative_firm_clean_developers, beneficiary).

% Environmental organizations, decentralization advocates, and reactor-host community groups contest the necessity clause in legislatures, courts, and streets, but hold no seats in treasury modeling exercises, capacity market design processes, or integrated assessment model consortia where the constraint is operationalized.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, anti_nuclear_civil_society, excluded,
    organized, generational, constrained, global).

% IPCC working-group scenario teams, IEA outlook groups, and national laboratory modelers produce the portfolio-optimization evidence base that legitimizes the constraint. They neither collect nor pay under it; their model architectures propagate its logic into official targets and procurement categories.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, energy_modeling_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_plant_operators).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of decarbonizing reliable bulk power: coordinating investment across every low-carbon source simultaneously so that variable renewables, firm nuclear output, and network infrastructure jointly meet demand in every hour of every season while emissions fall, without betting entire grids on any single technology's maturity or failure modes.
% TRANSFER_FUNCTION: Moves guaranteed revenue and public finance (tax credits, contract-for-difference top-ups, regulated returns, loan guarantees, export finance) from taxpayers and ratepayers to nuclear operators and vendors; moves market share and forward revenue away from fossil generators; moves the firm-capacity designation and its revenue certainty toward nuclear and away from competing firm-clean technologies.
% ABSENT_VOICES: Anti-nuclear and decentralization advocates object loudly in public but are absent from the technical rooms where the constraint binds: treasury and ministry modeling exercises, capacity market design consultations, and integrated assessment model consortia. Ratepayer advocates attend proceedings but are resourced far below incumbent utilities in rate-case and contract negotiations.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, ministries would rebalance toward cheapest-per-tonne portfolios, nuclear credit and contract pipelines would lapse, capacity market firm-designations would reprice toward storage and demand response, fossil retirement schedules would slip where no firm-zero substitute stood ready, and the modeling frameworks that encode the necessity clause would be rewritten by the rival readings.
% FOUNDING_PROBLEM: How can electricity systems eliminate carbon while keeping supply firm and affordable during the decades in which storage and demand flexibility remain too expensive or immature to carry grids dominated by variable renewables?
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by continental reliability assessments (NERC, ENTSO-E) and university energy-system modeling groups whose firm-low-carbon cost findings predate and do not depend on industry funding; the necessity clause itself is disputed by opportunity-cost economists, so the founding problem is corroborated as live while its nuclear-specific solution is contested.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the regime moves tens of billions annually (production tax credits, contract-for-difference differentials, regulated returns, guarantee-backed construction) onto captive bill-payers and taxpayers while mandating fossil displacement, against a backdrop of genuinely valuable decarbonization and reliability services. Suppression 0.48: nothing physically blocks solar, wind, or storage — suppression operates discursively and institutionally, through procurement categories, taxonomy gatekeeping, and modeling assumptions that strip rival strategies of legitimacy inside planning processes. Theater ratio 0.30: the engineering substance (fleet operations, safety cases, grid studies) is real and dominant, but a growing share of activity defends the necessity clause rhetorically — technology-neutral language deployed to justify technology-specific support. Accessibility collapse 0.62: once the constraint is accepted inside a planning frame, nuclear-free pathways fail review as illegitimate, though the rival readings keep external alternatives alive. Resistance 0.55: sustained anti-nuclear mobilization, litigation over taxonomy inclusion, siting opposition, and opportunity-cost counteradvocacy meet the constraint continuously. The measurement series run on one shared four-year grid (T=0..24, mapped to 2000..2024) with every tracked metric authored at every point. Trajectory drivers: the subsidy layer thickened after the 2012 Hinkley contract-for-difference and the Vogtle overrun era, then hardened with the 2022 IRA credit, EU taxonomy inclusion, and 2024 permitting reform — visible as rising base_extractiveness; enforcement machinery (taxonomy adjudication, pledge tracking, streamlined licensing) intensified over the same span — rising suppression_requirement; the neutrality rhetoric spread faster than directed support narrowed — slowly rising theater_ratio.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the operator and vendor seats the regime is coordination they help perform: guaranteed revenues reward them for providing firm zero-carbon output the system genuinely needs. From the fossil-generator seat the same regime is expropriation of forward revenue by mandate. From the ratepayer and taxpayer seats it is a transfer they did not contract into, priced above alternatives they are told are insufficient. The modeling institutions see an optimization problem; the excluded civil-society seat sees capture of the technical process. The engine computes these per-seat classifications from the structural data; this commentary does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (nuclear_plant_operators, nuclear_vendor_supply_chain) drive those seats toward the beneficiary end; their differing exits matter — vendors are mobile across jurisdictions, operators are license-bound and constrained. Victim declarations drive fossil_generators, electricity_ratepayers, general_taxpayers, and alternative_firm_clean_developers toward the target end. One override is authored: power_atom 'powerful' to d=0.8. The structural derivation would temper fossil generators' target-position because of their incumbent scale and lobbying power, but the constraint's operative mechanism — mandated displacement of their dispatch share — makes them direct targets regardless of their defensive capacity; the override corrects the power-dampening that would otherwise understate their extraction. Alternative firm-clean developers carry the story's sharpest dual structure: the breadth clause subsidizes them (pulling d down) while the necessity clause forecloses the firm-capacity slot (pushing d up); the derivation nets this from the declared roles rather than an override. Note suppression is authored as a raw structural property and is NOT scaled by directionality or scope — only extractiveness is scaled by the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Reading the regime as pure rope (the consensus 'all of the above' framing) would erase the transfer layer: guaranteed above-market revenues, overrun pass-throughs, and the foreclosure of the firm slot from competitors are real asymmetries riding on real coordination. Reading it as pure snare (nuclear-industry capture) would erase the coordination layer: firm low-carbon capacity measurably lowers total decarbonization cost and risk in the published system literature, and the founding problem — firming a decarbonized grid before storage matures — remains live. Tangled rope holds both halves. Mandatrophy is not resolved here: the founding problem is live (corroborated externally), so no dead-mandate persistence claim is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading of the climate_mitigation_imperative kernel governs a given jurisdiction''s operative mitigation constraint — and therefore which actors sit in the beneficiary versus victim sets?',
    'Cross-file comparison of the three sibling constraint stories plus institutional-uptake tracing: which reading''s axioms actually appear in statute, procurement rules, and finance-ministry guidance in each jurisdiction.',
    'If the opportunity_cost_reading prevails institutionally, this constraint''s beneficiaries (operators and vendors) become its targets and the fossil displacement accelerates on different terms; if the systems_transition_reading prevails, centralized operators broadly become targets regardless of source.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer structure: this constraint is one of three readings of one kernel; classification is valid per reading, and which reading binds is itself a structural variable.').

omega_variable(
    storage_substitution_trajectory,
    'Does the firm-capacity necessity premise survive the observed decline of long-duration storage and demand-flexibility costs through the 2030s?',
    'System-model studies comparing portfolios with and without firm zero-carbon capacity at realized (not projected) storage prices, plus actual procurement outcomes for multi-day storage against new firm-zero contracts.',
    'If substitution proves viable at scale, the necessity clause loses its engineering warrant; the constraint degrades toward pure portfolio-completeness coordination or retains its nuclear-directed transfers without the necessity cover, shifting classification toward snare-flavored extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_substitution_trajectory, empirical, 'Whether the foundational necessity axiom remains empirically warranted.').

omega_variable(
    necessity_vs_preference_conflation,
    'Is ''nuclear is necessary for reliable baseload'' an engineering result about grid physics, or a technology preference encoded as a constraint?',
    'Counterfactual portfolio optimization excluding nuclear across credible cost and performance ranges: a small cost-and-reliability penalty indicates preference; a large one indicates genuine constraint.',
    'If the penalty is small, the necessity clause functions rhetorically and the measured excess extraction rises sharply; if large, the coordination half of the tangled-rope reading is confirmed and part of the transfer is the price of reliability itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_preference_conflation, conceptual, 'Epistemic status of the distinguishing axiom separating this reading from its siblings.').

omega_variable(
    fossil_displacement_welfare_scope,
    'Do displaced fossil-sector workers and host-region economies count among the constraint''s victims, or are their losses simply the constraint''s intended operation?',
    'A policy choice on welfare accounting, evidenced by whether just-transition compensation is designed into the regime: compensated displacement distributes losses deliberately; uncompensated displacement concentrates them.',
    'Counting them enlarges the victim set and raises estimated resistance and coalition potential; excluding confines victimhood to the ratepayer and taxpayer transfer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_displacement_welfare_scope, preference, 'Boundary of the victim class under mandated fossil displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmipor_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(cmipor_tr_t0, observed).
narrative_ontology:measurement(cmipor_tr_t4, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(cmipor_tr_t4, observed).
narrative_ontology:measurement(cmipor_tr_t8, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(cmipor_tr_t8, observed).
narrative_ontology:measurement(cmipor_tr_t12, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(cmipor_tr_t12, observed).
narrative_ontology:measurement(cmipor_tr_t16, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(cmipor_tr_t16, observed).
narrative_ontology:measurement(cmipor_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(cmipor_tr_t20, observed).
narrative_ontology:measurement(cmipor_tr_t24, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(cmipor_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(cmipor_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(cmipor_be_t0, observed).
narrative_ontology:measurement(cmipor_be_t4, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement_basis(cmipor_be_t4, observed).
narrative_ontology:measurement(cmipor_be_t8, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement_basis(cmipor_be_t8, observed).
narrative_ontology:measurement(cmipor_be_t12, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(cmipor_be_t12, observed).
narrative_ontology:measurement(cmipor_be_t16, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement_basis(cmipor_be_t16, observed).
narrative_ontology:measurement(cmipor_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(cmipor_be_t20, observed).
narrative_ontology:measurement(cmipor_be_t24, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(cmipor_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(cmipor_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.33).
narrative_ontology:measurement_basis(cmipor_su_t0, observed).
narrative_ontology:measurement(cmipor_su_t4, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement_basis(cmipor_su_t4, observed).
narrative_ontology:measurement(cmipor_su_t8, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(cmipor_su_t8, observed).
narrative_ontology:measurement(cmipor_su_t12, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(cmipor_su_t12, observed).
narrative_ontology:measurement(cmipor_su_t16, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement_basis(cmipor_su_t16, observed).
narrative_ontology:measurement(cmipor_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(cmipor_su_t20, observed).
narrative_ontology:measurement(cmipor_su_t24, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(cmipor_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the mitigation imperative' decomposes into three structurally distinct constraints, one per reading of the kernel climate_mitigation_imberative: this portfolio-optimization reading (source-agnostic carbon intensity; nuclear in the beneficiary set; fossil generators the primary target); the opportunity_cost_reading (fastest deployment per dollar; nuclear reclassified net-harmful and moved to the victim side); and the systems_transition_reading (democratized decentralization; centralized operators generally targeted). The readings share an upstream physical referent (the carbon-budget constraint) but measure different arrangements, so their epsilons legitimately differ; forcing one story to span all three would violate epsilon-invariance. Every member of the family links to the others via affects_constraints; this file authors ONLY the portfolio-optimization reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
