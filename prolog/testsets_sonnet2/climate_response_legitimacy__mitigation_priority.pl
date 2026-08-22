% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Legitimacy: Decoupling Growth from Emissions via Innovation and Carbon Pricing
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The dominant international and national climate governance frame holds
 *   that legitimate climate response means reducing emissions primarily
 *   through carbon pricing (making emissions costly) and technological
 *   innovation (making low-carbon alternatives cheap and available), such
 *   that economic growth continues but 'decouples' from emissions growth.
 *   This frame coordinates a genuine collective action problem: without some
 *   shared mechanism, no individual jurisdiction or firm can unilaterally
 *   decarbonize without competitive disadvantage. But the same frame
 *   asymmetrically distributes costs — near-term adjustment costs land on
 *   carbon-intensive labor and low-income energy consumers, while the risk
 *   that decoupling fails to occur at sufficient speed and scale is
 *   transferred forward onto future generations and vulnerable ecosystems who
 *   have no voice in current pricing or subsidy design. The frame also
 *   privileges actors (clean-tech capital, carbon-market intermediaries,
 *   transitioning fossil incumbents) who profit from the specific mechanism
 *   chosen (price-and-innovate) over alternative mechanisms (adaptation
 *   reallocation, degrowth) that would distribute costs and benefits very
 *   differently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.44).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Legitimacy: Decoupling Growth from Emissions via Innovation and Carbon Pricing").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '2820a62f-82a5-4631-a4b4-8c74ccb66fae').
narrative_ontology:cs_kernel_codification('2820a62f-82a5-4631-a4b4-8c74ccb66fae', distributed).
narrative_ontology:cs_authority_grounding('2820a62f-82a5-4631-a4b4-8c74ccb66fae', distributed).
narrative_ontology:cs_reading_relation('2820a62f-82a5-4631-a4b4-8c74ccb66fae', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2820a62f-82a5-4631-a4b4-8c74ccb66fae', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('2820a62f-82a5-4631-a4b4-8c74ccb66fae', foundational, growth_emissions_decoupling_is_achievable_and_sufficient).
narrative_ontology:cs_axiom_status(growth_emissions_decoupling_is_achievable_and_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('2820a62f-82a5-4631-a4b4-8c74ccb66fae', growth_emissions_decoupling_is_achievable_and_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('2820a62f-82a5-4631-a4b4-8c74ccb66fae', foundational, economic_growth_preservation_is_a_legitimacy_constraint_on_climate_policy).
narrative_ontology:cs_axiom_status(economic_growth_preservation_is_a_legitimacy_constraint_on_climate_policy, holdable).
narrative_ontology:cs_axiom_grounding('2820a62f-82a5-4631-a4b4-8c74ccb66fae', economic_growth_preservation_is_a_legitimacy_constraint_on_climate_policy, instrumental).
narrative_ontology:cs_reference_frame('2820a62f-82a5-4631-a4b4-8c74ccb66fae', post_paris_agreement_market_based_governance).
narrative_ontology:cs_drift_state('2820a62f-82a5-4631-a4b4-8c74ccb66fae', post_ipcc_ar6_synthesis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2820a62f-82a5-4631-a4b4-8c74ccb66fae', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_fossil_capital_transitioning_to_low_carbon).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, clean_tech_industry).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, high_income_consuming_classes).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, national_governments_of_wealthy_states).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, carbon_intensive_labor_sectors).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, low_income_energy_consumers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, global_south_states_dependent_on_carbon_finance).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, ecosystems_dependent_on_timely_decarbonization).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, incumbent_fossil_capital_transitioning_to_low_carbon).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, green_growth_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, market_based_climate_governance_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce carbon pricing schedules, subsidize technological innovation (CDR, renewables, hydrogen), and set the legitimacy frame in international negotiations that growth need not be sacrificed. They set emissions targets that assume future technological maturation and can revise both the price path and the sunset date for compensatory measures.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, national_governments_of_wealthy_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Receives subsidies, tax credits, and carbon-price-driven demand for renewables, batteries, hydrogen, and carbon capture. Benefits directly from the arrangement's insistence that technology, not consumption reduction, is the answer. Can relocate operations across jurisdictions competing for green industrial policy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, clean_tech_industry, beneficiary,
    organized, biographical, mobile, global).

% Large energy incumbents rebrand as diversified energy companies, capture carbon-pricing revenue recycling and offset markets, and use their financial scale to shape the pace of transition on terms that protect existing capital stock. They pay compliance costs but recoup much of it through pricing power and new low-carbon asset lines.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_fossil_capital_transitioning_to_low_carbon, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, incumbent_fossil_capital_transitioning_to_low_carbon, payer).

% Brokers, verifiers, and exchanges that build fee-generating infrastructure around carbon pricing and offset markets. Their revenue depends on the mitigation-priority frame remaining dominant rather than a degrowth or command-and-control alternative; they can shift business models across jurisdictions as pricing regimes proliferate.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Retain consumption patterns and mobility largely intact because the burden of adjustment is displaced onto carbon prices passed through supply chains and onto future technological delivery rather than onto near-term lifestyle constraint. Can absorb marginal price increases without altering behavior.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, high_income_consuming_classes, beneficiary,
    moderate, biographical, mobile, national).

% Coal, oil, gas, and heavy-industry workers bear job displacement as carbon pricing and innovation-led transition proceed on a timetable set by capital and government, without the compensating structural transformation (job guarantees, ownership stakes) that a degrowth or a more redistributive path would require. Regional economies dependent on these sectors have few alternative employers.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_intensive_labor_sectors, payer,
    powerless, biographical, trapped, regional).

% Carbon pricing raises the cost of energy and transport disproportionately relative to income; compensatory rebates are frequently underfunded or politically contested. They cannot substitute away from carbon-intensive energy because alternatives require capital they do not have.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, low_income_energy_consumers, payer,
    powerless, immediate, trapped, national).

% Depend on carbon markets, green finance, and technology transfer promised under the mitigation-priority frame, but face conditionalities, underfunded pledges, and continued exposure to climate impacts their own emissions did not cause. Their bargaining leverage in negotiations is limited by dependence on external finance.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, global_south_states_dependent_on_carbon_finance, payer,
    moderate, generational, constrained, global).

% Inherit whatever climate trajectory results if technological decoupling and carbon pricing prove insufficient or too slow relative to remaining carbon budgets. They have no representation in current pricing or innovation-timeline decisions and cannot exit a warmed world.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Named for completeness as a non-agent entity bearing the cost of delayed or insufficient decoupling: ecosystems and species with tipping-point vulnerabilities that do not participate in and cannot benefit from carbon markets or innovation subsidies.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, ecosystems_dependent_on_timely_decarbonization, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__mitigation_priority, ecosystems_dependent_on_timely_decarbonization).

% Argue that mitigation-priority framing understates transition risk, over-relies on unproven negative-emissions technology, and forecloses the political space for either adaptation-first resource reallocation or structural degrowth. They participate in UNFCCC side events and academic literature but are structurally marginal to the treaty text and national carbon-pricing design processes, which are dominated by finance ministries and industry.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, adaptation_and_degrowth_advocates, excluded,
    organized, generational, constrained, global).

% Assess remaining carbon budgets, technology readiness levels for carbon dioxide removal, and the plausibility of decoupling pathways. Their scenario modeling is drawn upon selectively by the agenda-setters to legitimate the mitigation-priority frame, sometimes stripped of the caveats attached to high-CDR-reliance pathways.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_scientists_and_ipcc_assessors, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, internationally legible framework — carbon pricing plus innovation subsidy — that allows disparate states and firms to coordinate emissions reduction without requiring any single actor to unilaterally sacrifice growth or competitiveness, avoiding a race-to-the-bottom collective action failure.
% TRANSFER_FUNCTION: Moves near-term adjustment costs (energy price increases, labor displacement in carbon-intensive sectors) onto powerless and trapped populations now, while moving climate risk itself forward onto future generations and vulnerable ecosystems, in exchange for preserving growth and profit continuity for capital-holding and consuming classes today.
% ABSENT_VOICES: Future generations have no seat in carbon-price-setting or innovation-timeline negotiations. Degrowth and adaptation-priority advocates are present in discourse but structurally marginal to the treaty and pricing-design processes, which are dominated by finance ministries, energy incumbents, and green-industrial-policy coalitions.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority legitimacy frame collapsed overnight, carbon-pricing infrastructure, offset markets, and innovation-subsidy programs built around it would lose their justificatory basis; political space would open rapidly for either an adaptation-first reallocation of resources toward vulnerable populations or a degrowth-transformation agenda, both of which entail materially different resource flows, different winners, and different institutional architectures than currently exist.
% FOUNDING_PROBLEM: The founding problem is twofold: rising anthropogenic emissions threatening catastrophic and irreversible climate change, and the political-economic reality that no government would accept an emissions-reduction pathway understood to require abandoning growth, since growth is tied to electoral survival, debt servicing, and geopolitical competitiveness.
% FOUNDING_PROBLEM_CORROBORATION: IPCC working group assessments (an outside scientific body, not itself a beneficiary of carbon markets) attest that the physical emissions problem remains live and that current mitigation-priority pledges are insufficient to hold to stated temperature targets absent substantial reliance on unproven-at-scale carbon dioxide removal. Independent economists studying decoupling data are divided: some find absolute decoupling occurring in select wealthy economies, others find it insufficient in rate or geographically offset by consumption-based emissions displaced to manufacturing regions — corroboration exists but is contested even among non-beneficiary observers.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects substantial but not extreme extraction: the coordination function is real (carbon pricing does reduce emissions at the margin, and innovation subsidies have driven real cost declines in renewables), but the framing's insistence on growth preservation systematically discounts intergenerational risk transfer and displaces adjustment costs onto powerless populations. Suppression (0.44) is moderate — the mitigation-priority frame does not use hard coercion against rival readings so much as structural dominance in treaty architecture, central-bank green-finance taxonomies, and multilateral funding conditionality, which crowds out adaptation-first and degrowth alternatives from serious policy consideration without banning them outright. Theater ratio (0.42) is elevated and rising because a growing share of activity — voluntary carbon offset markets, corporate net-zero pledges reliant on unproven CDR at scale, and finance-sector ESG reporting — has decoupled from verified emissions outcomes even as it grows in volume and prominence. Accessibility collapse (0.4) is moderate: rival readings (adaptation, degrowth) remain articulable and are actively argued in academic and activist spaces, so alternatives have not collapsed as completely as under a mountain-type constraint. Resistance (0.55) is substantial, coming from carbon-intensive labor movements, Global South negotiating blocs demanding loss-and-damage and adaptation finance, and degrowth scholarship increasingly cited in IPCC working group discussions.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy-state governments and clean-tech/carbon-finance industry actors sit near the beneficiary end: they set the frame's terms, capture subsidy flows and market-making fees, and bear the least disruption to underlying growth models. Incumbent fossil capital is dual-positioned — it pays compliance costs but recoups much through pricing power and diversification into low-carbon assets, hence beneficiary-with-payer secondary role. Carbon-intensive labor, low-income energy consumers, and Global South states dependent on carbon finance sit near the target end: they bear concentrated, immediate costs from a transition timetable and pricing structure they did not design and cannot easily exit (trapped or constrained exit options). Future generations and climate-vulnerable ecosystems sit at the extreme target end: full exposure to the risk that decoupling proves insufficient, zero voice in the mechanism's design, and no exit option of any kind — hence directionality is maximal even though the metric is authored, not engine-computed, for these seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — catastrophic emissions trajectory colliding with the political impossibility of abandoning growth — remains substantially live per IPCC assessment, which distinguishes this constraint from a pure mandatrophy case where the founding problem has fully dissolved. What has drifted is the balance between the coordination function (real, and shrinking the emissions-per-dollar-of-GDP ratio in many wealthy economies) and the extraction function (carbon-market theater, offset gaming, and risk transfer onto future generations who cannot object). Classifying this as tangled_rope rather than snare or mountain avoids two errors: treating it as pure extraction would deny the real decoupling occurring in some jurisdictions and the genuine coordination carbon pricing provides against competitive races-to-the-bottom; treating it as a natural, inevitable arrangement (mountain) would launder the choice to preserve growth over more redistributive alternatives as if no choice had been made at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_sufficiency_ambiguity,
    'Will absolute decoupling of GDP growth from emissions occur fast enough, at the scale required, to keep remaining carbon budgets from being exceeded — or does the mitigation-priority frame''s reliance on future technological maturation (especially large-scale carbon dioxide removal) constitute a bet against the interests of future generations that current beneficiaries have no incentive to price correctly?',
    'Longitudinal tracking of territorial AND consumption-based emissions against GDP growth in wealthy economies, cross-referenced against IPCC remaining-carbon-budget estimates and independent (non-industry-funded) assessment of CDR technology readiness and deployment rates versus the rates assumed in national pledges.',
    'If decoupling proves insufficient or too slow, the classification should shift toward snare with future generations as the dominant, uncompensated victim class; if decoupling proves sufficient and CDR scales as projected, the coordination function dominates and a rope classification becomes more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_sufficiency_ambiguity, empirical, 'Whether technological decoupling will occur fast enough to avoid transferring catastrophic risk to future generations.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice of mitigation-priority over adaptation-priority or degrowth-transformation itself a legitimate collective political decision, or is it structurally determined by which actors (finance ministries, industry incumbents, growth-dependent electoral systems) control the venues where climate legitimacy claims are adjudicated?',
    'Comparative institutional analysis of which stakeholder groups have formal standing and agenda-setting power in UNFCCC negotiations, national carbon-pricing design processes, and multilateral climate finance governance, versus which groups (future generations, degrowth advocates, adaptation-first Global South coalitions) are present only as commentary, not as decision architecture.',
    'If the reading is structurally determined by incumbent power rather than a genuinely contested and revisable political choice, the tangled_rope classification understates the extraction component — the ''choice'' of mitigation-priority is closer to captured agenda-setting than open coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the mitigation-priority reading''s dominance reflects genuine political consensus or structural capture of the venues that adjudicate climate legitimacy.').

omega_variable(
    offset_market_integrity_ambiguity,
    'What proportion of carbon offset and net-zero pledge activity represents genuine, additional, permanent emissions reduction versus accounting theater that allows continued high-carbon activity under a compliant-looking wrapper?',
    'Independent third-party audit of offset project additionality and permanence rates across major voluntary and compliance carbon markets, compared against corporate emissions trajectories claimed to be offset.',
    'A high theater share would support reclassifying carbon-market intermediaries from coordination facilitators to primarily extractive rent-collectors riding on the mitigation-priority frame''s legitimacy; a low theater share would support the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offset_market_integrity_ambiguity, empirical, 'Whether carbon offset markets perform genuine additional decarbonization or primarily generate compliance theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__mitigation_priority, theater_ratio, 5, 0.28).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__mitigation_priority, theater_ratio, 10, 0.33).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__mitigation_priority, theater_ratio, 15, 0.37).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__mitigation_priority, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__mitigation_priority, theater_ratio, 25, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__mitigation_priority, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__mitigation_priority, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__mitigation_priority, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__mitigation_priority, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__mitigation_priority, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__mitigation_priority, suppression_requirement, 5, 0.34).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__mitigation_priority, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__mitigation_priority, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__mitigation_priority, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__mitigation_priority, suppression_requirement, 25, 0.43).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_response_legitimacy kernel, each authored as a separate, ε-invariant constraint per the decomposition principle: mitigation_priority (this story, tangled_rope, ε=0.58 — coordination via carbon pricing/innovation genuinely present but growth-preservation premise transfers risk to future generations and immediate cost to powerless populations), adaptation_priority (accepts warming trajectory, reallocates toward resilience — different beneficiary/victim structure, different ε), and degrowth_transformation (rejects growth-preservation entirely, requires structural economic transformation — different ε again, likely lower extraction toward future generations but higher near-term disruption to current high-consumption classes). The three readings compete for legitimacy in the same institutional venues (UNFCCC, national climate policy, IPCC framing choices), so structural or legitimacy shifts in one reading's institutional standing plausibly shift resource and attention allocation available to the others — hence the network edges, not a shared ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
