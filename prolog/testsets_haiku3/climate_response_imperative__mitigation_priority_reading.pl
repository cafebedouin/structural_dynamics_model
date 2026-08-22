% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Climate Mitigation via Market-Driven Innovation (Mitigation-Priority Reading)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates the mitigation-priority reading of the
 *   contested climate-response-imperative kernel: climate response is
 *   primarily emissions reduction via technological innovation and market
 *   mechanisms, with adaptation as residual. The reading is institutionally
 *   dominant in UNFCCC, World Bank, and high-income country climate
 *   governance. It assumes technological decoupling of emissions from growth,
 *   scalability of unproven carbon dioxide removal technologies, and that
 *   adaptation needs will diminish as warming is mitigated. This reading
 *   extracts from vulnerable frontline regions and future generations by
 *   deferring adaptation investment and funding innovation sectors in
 *   high-income countries. Three sibling readings contest this allocation:
 *   the adaptation-priority reading emphasizes resilience and damage
 *   reduction in exposed regions as primary, and the degrowth reading demands
 *   structural economic transformation in the Global North as prerequisite
 *   for both mitigation and adaptation. The claim/metric independence is
 *   deliberate: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination function + asymmetric extraction both present), and the
 *   authored metrics describe substantially extractive, actively suppressed
 *   operation. The engine computes how each seat experiences this structure
 *   differently; this narrative declares the structural reading that sustains
 *   all three.
 *
 * KEY AGENTS:
 *   - global_north_innovation_sectors — institutional beneficiary (agenda-setter), captures investment and carbon credit revenues; powerful globally, mobile exit but arbitrage-positioned
 *   - vulnerable_frontline_regions — powerless victim, trapped geographically and economically; bears immediate climate impacts and deferred adaptation costs
 *   - future_generations — powerless victim, identity-locked (inherits climate system); bears accumulated warming and deferred adaptation damages
 *   - high_income_country_governments — institutional agenda-setter and enforcer; sets international policy frame via UNFCCC dominance and development conditionality
 *   - carbon_markets_and_offset_industry — institutional beneficiary; captures fees from intermediating emissions accounting rather than actual reduction
 *   - low_income_country_governments — constrained participant (role: observer + forced payer); must accept frame to access climate finance
 *   - unproven_cdr_technology_developers — powerful beneficiary; capture rents from assumption that their speculative technologies will scale to necessary levels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.71).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Climate Mitigation via Market-Driven Innovation (Mitigation-Priority Reading)").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '02f59171-af95-4339-bbd5-e4f3a86ceac9').
narrative_ontology:cs_kernel_codification('02f59171-af95-4339-bbd5-e4f3a86ceac9', formalized).
narrative_ontology:cs_authority_grounding('02f59171-af95-4339-bbd5-e4f3a86ceac9', extraction).
narrative_ontology:cs_interpretation_layer_present('02f59171-af95-4339-bbd5-e4f3a86ceac9').
narrative_ontology:cs_reading_relation('02f59171-af95-4339-bbd5-e4f3a86ceac9', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('02f59171-af95-4339-bbd5-e4f3a86ceac9', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('02f59171-af95-4339-bbd5-e4f3a86ceac9', foundational, technological_decoupling_feasible).
narrative_ontology:cs_axiom_status(technological_decoupling_feasible, holdable).
narrative_ontology:cs_axiom_grounding('02f59171-af95-4339-bbd5-e4f3a86ceac9', technological_decoupling_feasible, empirically_contingent).
narrative_ontology:cs_axiom('02f59171-af95-4339-bbd5-e4f3a86ceac9', foundational, market_mechanisms_sufficient_for_allocation).
narrative_ontology:cs_axiom_status(market_mechanisms_sufficient_for_allocation, holdable).
narrative_ontology:cs_axiom_grounding('02f59171-af95-4339-bbd5-e4f3a86ceac9', market_mechanisms_sufficient_for_allocation, instrumental).
narrative_ontology:cs_axiom('02f59171-af95-4339-bbd5-e4f3a86ceac9', secondary, adaptation_residual_to_mitigation).
narrative_ontology:cs_axiom_status(adaptation_residual_to_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('02f59171-af95-4339-bbd5-e4f3a86ceac9', adaptation_residual_to_mitigation, empirically_contingent).
narrative_ontology:cs_reference_frame('02f59171-af95-4339-bbd5-e4f3a86ceac9', efficient_emissions_reduction_via_innovation).
narrative_ontology:cs_drift_state('02f59171-af95-4339-bbd5-e4f3a86ceac9', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('02f59171-af95-4339-bbd5-e4f3a86ceac9', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, fossil_fuel_replacement_industries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_credit_traders).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, vulnerable_frontline_regions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, land_dependent_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_markets_and_offset_industry).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, unproven_cdr_technology_developers).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, low_income_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Technology and energy companies in high-income countries shape climate policy via innovation narratives and market-based mechanisms (carbon pricing, carbon capture, renewable energy infrastructure). They capture investment flows, intellectual property rights, and carbon offset revenues. Their models assume emissions reductions decouple from growth; success is measured in deployment of their technologies and expansion of carbon markets.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary).

% Small island states, sub-Saharan Africa, South Asia, and climate-vulnerable countries experience immediate climate impacts (drought, sea-level rise, extreme weather) while adaptation funding flows primarily toward Global North mitigation technology deployment and development of carbon markets. They bear the costs of delayed mitigation (warming acceleration) while adaptation remains underfunded. Exit is structurally impossible: they cannot leave their territories and have minimal capacity to redirect climate finance.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, vulnerable_frontline_regions, payer,
    powerless, immediate, trapped, regional).

% Inherit a climate system path-dependent on cumulative emissions made under this reading. If mitigation-as-innovation-deployment fails to meet warming targets, they face adaptation costs Global North deferred today. Identity is locked: they cannot choose not to inherit the climate system they are born into. Their interests are represented discursively but not in any institutional seat with enforcement power.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, identity_locked, global).

% Indigenous peoples, pastoral communities, and subsistence farmers depend on ecosystems threatened by residualized adaptation (deferred investment in watershed management, soil restoration, flood protection). They bear costs of both delayed mitigation (warming) and delayed adaptation (ecosystem degradation from competing land-use priorities—carbon sequestration plantations, biofuel production, mining). Their land-use knowledge is subordinated to optimization for carbon storage and technological deployment.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, land_dependent_communities, payer,
    organized, generational, constrained, regional).

% Financial institutions, trading platforms, and carbon offset project developers capture fees and profits by intermediating between emitters seeking offsets and land-use or technology projects selling credits. Their business model depends on mitigation-as-accounting: the continuation of carbon markets as the primary policy mechanism. They benefit from continued reliance on market-based rather than regulatory approaches.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_markets_and_offset_industry, beneficiary,
    institutional, biographical, mobile, global).

% Set the international policy frame through dominant voting positions in UNFCCC, World Bank, IMF. They enforce the mitigation-priority reading via climate finance conditionality (technology transfer over adaptation budgets), trade rules, and intellectual property regimes that shape which solutions are deployed. Exit from this enforcement is costly but possible through policy realignment.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, high_income_country_governments, agenda_setter,
    powerful, biographical, mobile, national).

% Provide epistemic authority for this reading through IPCC assessments, journal publications, and policy briefings. Their modeling and framing choices (discount rates, adaptation feasibility, technology timelines) systematically embed the mitigation-priority assumptions. Constrained by funding structures and policy access that reward alignment with the dominant frame; exit requires career cost.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_scientists_and_advocacy_researchers, observer,
    moderate, generational, constrained, global).

% Must accept the mitigation-priority frame to access climate finance and development resources, even where their immediate climate impacts (adaptation needs) are larger than their mitigation obligations. They have formal representation in UNFCCC but minimal enforcement power over the reading's application. Constrained by external financial dependency and threat of climate impact escalation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, low_income_country_governments, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, low_income_country_governments, observer).

% Renewable energy manufacturers, battery producers, and EV makers capture growth and subsidies from the mitigation-via-innovation frame. They benefit from Global North investment prioritization and intellectual property protection for clean technologies. Exit is available: they could support alternative readings emphasizing degrowth or demand management that threaten their expansion model.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, fossil_fuel_replacement_industries, beneficiary,
    institutional, biographical, mobile, global).

% Direct air capture, enhanced weathering, ocean alkalinity, and other carbon dioxide removal technologies are speculative and largely undeployed at scale. This reading assumes they will achieve necessary deployment and cost reduction; developers capture research funding, carbon credit value, and intellectual property rents predicated on the assumption. Their business model collapses if the reading is displaced by emphasis on near-term mitigation or degrowth.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, unproven_cdr_technology_developers, beneficiary,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a decentralized climate response via price signals and innovation incentives: carbon pricing creates emissions reductions incentives; technology deployment spreads globally via markets; adaptation planning emerges as residual accounting for unavoidable warming. Avoids centralized mandates and distributional conflict by framing climate response as win-win innovation.
% TRANSFER_FUNCTION: Moves climate-mitigation investment, research funding, and carbon credit revenues from high-income countries and emitters toward Global North innovation sectors and carbon markets. Defers adaptation investment and shifts its costs onto vulnerable regions and future generations via path-dependent warming and delayed infrastructure investment.
% ABSENT_VOICES: Land-dependent and Indigenous communities whose adaptation strategies and land-use knowledge are subordinated to optimization for carbon storage and technology deployment. Future generations who cannot participate in present policy-setting but bear the cost of delayed adaptation. Low-income country negotiators who formally participate but hold minimal enforcement power and face conditionality on technology transfer over adaptation budgets.
% DISAPPEARANCE_RATIONALE: If this reading were displaced by the adaptation-priority or degrowth readings, capital flows would realign from innovation to resilience infrastructure, land-use policy would reorient from carbon sequestration toward local food security and water security, and intellectual property regimes would shift from protecting high-income country innovations toward enabling technology access and local adaptation. The global distribution of climate finance and research priority would restructure fundamentally.
% FOUNDING_PROBLEM: Anthropogenic climate change requires reducing emissions to avoid catastrophic warming; existing emissions-reduction technologies are capital-intensive and concentrated in high-income countries; markets and decentralized innovation can mobilize capital and accelerate deployment faster than centralized planning or demand reduction.
% FOUNDING_PROBLEM_CORROBORATION: Global North innovation sectors, carbon markets, and technology-optimist researchers attest the founding problem remains live and this reading solves it efficiently. Low-income countries, climate justice advocates, and adaptation researchers counter that the founding problem's emphasis on technology speed is selective: it prioritizes risk mitigation for high-income countries while deferring tangible adaptation for the most vulnerable. Indigenous and land-dependent communities attest that their adaptation expertise has been sidelined and their territories instrumentalized as carbon storage. Scientific assessments from IPCC acknowledge both mitigation and adaptation are necessary but have been disciplined by policy preferences that treat mitigation as the primary lever.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (0.52→0.70 over 40-year interval) because the constraint transfers research funding, carbon credit revenue, and intellectual property rents from global emitters toward Global North innovation sectors while deferring adaptation investment. The transfer is not driven by real coordination service cost — carbon pricing and market mechanisms are largely accounting systems, not scarcity-driven valuations. Suppression is higher (0.71) because the constraint's persistence requires actively excluding alternative readings (adaptation-priority, degrowth) that would redirect resources; exclusion is enforced through climate finance conditionality, intellectual property regimes, and epistemic authority in research and policy forums. Theater ratio is moderate (0.42, rising to 0.43) because the constraint performs genuine coordination functions (price signals, innovation deployment) but a growing share of activity defends the reading's legitimacy against empirical challenges (adaptation costs mounting, CDR failing to scale) rather than advancing the coordination itself. Measurement trajectory shows creeping extraction and theater over 40 years as initial legitimacy erodes and enforcement must intensify. The acceleration is modest because the reading's institutional power is resilient, but the trend signals mandatrophy: if CDR and decoupling fail to deliver promised outcomes, the constraint shifts from tangled_rope toward snare (extraction persists, coordination function evaporates). Accessibility collapse is relatively low (0.48) because alternatives persist in subaltern forums, Indigenous policy spaces, and academic critique; suppression is required to maintain dominance, not achieved through complete foreclosure of all alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (global_north_innovation_sectors, high_income_country_governments) and the victim seats (vulnerable_frontline_regions, future_generations) compute radically different types from the same structural data. From the beneficiary seat, the constraint is genuine tangled_rope: real coordination problem (climate requires emissions reduction), workable solution (market mechanisms and innovation incentives), proportional cost sharing (actors pay based on emissions), and distributed benefit (emissions reductions benefit everyone). From the powerless victim seats, the same structure is snare: extraction masked as coordination, costs borne by those least responsible and least able to exit, benefits concentrated in high-income sectors, and no consent or negotiation from the most vulnerable. The engine computes this divergence from directionality: beneficiary seats have d near 0.0 (subsidized by the constraint), target seats have d near 1.0 (extracted from), identity-locked seats remain constrained even if barriers lower. The temporal dynamics are asymmetric: from the beneficiary seat, the constraint is working as designed (innovation deploying, markets forming); from the victim seat, the constraint's founding function (enabling emissions reduction) is failing (cumulative emissions rising, adaptation gaps widening) while extraction persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Global_north_innovation_sectors: d ≈ 0.15 (near beneficiary end). They set the agenda, capture rents, have arbitrage-quality exit (can redirect innovation to other domains if climate policy changes), and operate at institutional power with global scope. The constraint subsidizes their capital formation. Low directionality means low effective extraction on them; the engine applies negative χ (subsidy). Vulnerable_frontline_regions: d ≈ 0.92 (near target end). Powerless agents, trapped geographically, face immediate impacts, and constrained to accept whatever finance allocation the system provides. The constraint extracts from them via deferral of adaptation funding. High directionality means high effective extraction; χ is amplified. Future_generations: d ≈ 0.95 (near full target). Powerless by definition (unborn), identity-locked (cannot choose which climate system to inherit), face civilizational time horizon, have zero exit options. The constraint's path-dependent warming extracts maximal cost from them. Identity-lock and powerlessness push d toward 1.0. Carbon_markets_and_offset_industry: d ≈ 0.25 (beneficiary-leaning). They benefit from market mechanisms being the primary policy tool and can exit if regulation (degrowth reading's preferred mode) replaces markets, so exit is mobile. High-income_country_governments: d ≈ 0.40 (near symmetric). They benefit from the reading (avoids redistributive conflict) but also face pressure from advocacy and low-income country delegations; their exit from the reading is costly but possible through policy realignment. Unproven_cdr_technology_developers: d ≈ 0.12 (strong beneficiary). They capture speculative rents; their exit is arbitrage-grade because if the reading fails they redirect to other sectors.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits early mandatrophy signals but has not yet fully crossed the threshold. The founding problem is 'climate change requires emissions reduction; market mechanisms can mobilize innovation to deliver it efficiently.' This founding problem is contested (founding_problem_status: contested) and shows signs of attrition. Empirically, emissions continue to rise despite decades of market-based climate policy; adaptation gaps widen faster than mitigation measures close them; unproven CDR technologies remain far from necessary scale. The policy mandate persists, however, because the constraint is sustained by institutional power and lock-in: high-income country governments benefit from avoiding demand-side transformation; innovation sectors benefit from continued investment prioritization; carbon markets have become financial infrastructure. The theater ratio rising from 0.28 to 0.43 signals that an increasing share of activity defends the reading's legitimacy rather than advancing emissions reduction. If CDR fails to scale or technological decoupling proves insufficient, the constraint will tip into snare-type mandatrophy: extraction continues (Global North innovation sectors remain funded, carbon markets persist) while the coordination function (genuine emissions reduction pathway) evaporates. The dissipation_timeline is 10–20 years: if major CDR deployments do not achieve cost and scale targets by 2035–2045, the reading's epistemic authority collapses and institutional transition becomes unavoidable. The suppression required to maintain the reading will escalate (more resource allocation to legitimacy defense, more exclusion of rival readings, more stringent intellectual property enforcement). This trajectory is consistent with tangled_rope degrading toward snare via extraction accumulation outpacing coordination function delivery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_decoupling_viability,
    'Can emissions reductions via market-driven innovation and technology deployment occur at sufficient scale and speed to meet Paris targets without demand-side transformation in high-income countries?',
    'Empirical tracking: compare deployed renewable capacity, efficiency gains, and actual emissions trajectories against decoupling assumptions embedded in IPCC scenarios. Natural experiment: compare outcomes in regions pursuing pure technology-based mitigation versus those incorporating adaptation and demand management.',
    'If decoupling fails to achieve necessary emission reductions, the mitigation-priority reading collapses into a mandatrophy constraint — a technology-optimist reading that persists despite its founding function being unmet. Victim set expands dramatically; adaptation constraint emerges as live crisis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_decoupling_viability, empirical, 'Whether technological decoupling can deliver sufficient mitigation without demand-side transformation.').

omega_variable(
    cdr_scalability_assumption,
    'Will carbon dioxide removal technologies (direct air capture, enhanced weathering, ocean alkalinity, bioenergy with CCS) achieve the deployment scale, cost reduction, and permanence required to offset residual emissions and meet net-zero targets?',
    'Technology monitoring: cost trajectories, actual pilot deployment, permanence verification, land-use and energy intensity of scaled CDR. Comparison: track whether CDR deployment follows learning curves of prior energy technologies or encounters fundamental thermodynamic or resource constraints.',
    'If CDR fails to scale, the constraint shifts from tangled_rope (coordination + extraction) toward snare: the extraction (deferral of adaptation, prioritization of innovation investment) persists but the coordination function (legitimate technical solution) evaporates. Victims expand; suppression must intensify to maintain the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cdr_scalability_assumption, empirical, 'Whether unproven CDR technologies will achieve necessary scale and cost to enable this reading''s logic.').

omega_variable(
    adaptation_residualism_vs_necessity,
    'Is adaptation truly residual to mitigation, or is immediate adaptation infrastructure investment necessary alongside mitigation to prevent compound risks and avoid locking in future vulnerability?',
    'Longitudinal study: compare warming trajectories, adaptation outcomes, and compounded climate impact in regions that invested in early adaptation versus those that deferred. IPCC assessment of adaptation pathways under different mitigation scenarios.',
    'If immediate adaptation is necessary rather than residual, the constraint''s classification shifts: adaptation becomes a coequal goal, not a secondary effect. This reclassifies mitigation-priority as a contested reading rather than truth, and reframes victims as frontline regions bearing both mitigation delay costs AND adaptation-funding starvation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_residualism_vs_necessity, empirical, 'Whether adaptation can truly be deferred until mitigation reduces warming, or whether early investment is necessary to prevent compounding vulnerability.').

omega_variable(
    reading_kernel_rivalry,
    'Is this reading (mitigation-via-innovation) a live normative claim about how climate response should prioritize resources, or is it primarily a cover story for rent extraction by Global North innovation sectors, with the adaptation-priority and degrowth readings being the structurally truer accounts?',
    'Audit the distribution of climate finance (observed vs. recommended); compare emissions reduction per dollar invested across mitigation innovation, adaptation infrastructure, and demand-side approaches. Examine career incentives and funding patterns in climate research to trace whether the reading persists due to epistemic merit or institutional capture.',
    'If this reading is primarily extractive cover, it should be reclassified as snare from all victim seats. The constraint would then anchor the decomposition of the broader climate-response-imperative kernel into three distinct constraints, each with distinct classification, rather than three coexisting readings of a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_rivalry, conceptual, 'Whether the mitigation-priority reading reflects structural truth about climate response or primarily serves Global North institutional interests.').

omega_variable(
    future_generation_standing,
    'How should the interests of future generations—who cannot negotiate or consent but bear the accumulated consequences of present adaptation deferral—be represented in the classification of this constraint?',
    'Ethical framework comparison: assess whether future-generation victimhood (deferred adaptation costs, inherited warming) is sufficiently modeled in the directionality derivation. Consider whether identity_locked exit for future generations warrants classification-level adjustment relative to present-day payers.',
    'If future generations are underweighted in the victim analysis, extractiveness and suppression are underestimated. The constraint may compute as rope or tangled_rope when it should compute as snare from the civilizational time-horizon perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generation_standing, preference, 'How to epistemically represent the interests of unborn agents in constraint classification.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the hegemony of the mitigation-priority reading in international climate governance actively foreclose the adaptation-priority and degrowth readings from policy consideration, or do all three coexist as live normative positions?',
    'Institutional analysis: examine UNFCCC voting patterns, climate finance allocation, research funding, and policy formation to determine whether the mitigation reading dominates by superior argument or by control of institutional resources. Compare voice given to each reading in official versus subaltern forums.',
    'If the mitigation reading forecloses the others through institutional dominance rather than logical necessity, reading_relations should shift from coexists_with or influences to a form of institutional foreclosure, and the constraint''s suppression metric should be reframed to include suppression of alternative readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, empirical, 'Whether the mitigation-priority reading forecloses or merely competes with adaptation-priority and degrowth readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__mitigation_priority_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__mitigation_priority_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__mitigation_priority_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__mitigation_priority_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__mitigation_priority_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__mitigation_priority_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(clim_tr_t40, climate_response_imperative__mitigation_priority_reading, theater_ratio, 40, 0.43).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_be_t40, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 40, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(clim_su_t40, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__mitigation_priority_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate-response-imperative kernel decomposes into three constraint stories, one per reading: mitigation_priority_reading (this story — technological innovation and markets as primary lever), adaptation_priority_reading (resilience-building and damage reduction in exposed regions as primary), and degrowth_reading (structural economic transformation as prerequisite). Each reading instantiates different beneficiary/victim structures, different ε values, and different classifications emerging from the same kernel. The epsilon-invariance principle requires decomposition: the ε value differs substantially across readings because they assess the standing arrangement (the global climate response system) under different interpretive lights. The mitigation reading sees the arrangement's extractiveness as moderate (0.68) because it assumes coordination benefits are real and broadly shared; the adaptation reading sees extractiveness as higher (estimated 0.75+) because it deems coordination benefits concentrated in high-income countries; the degrowth reading sees extractiveness as severe (estimated 0.82+) because it deems the entire arrangement subordinate to Global North growth imperatives. These are not measurement differences on one constraint; they are structural differences on three distinct constraint stories that all reference the same kernel. Network edges link them: mitigation influences adaptation and degrowth (if markets succeed, alternatives are marginalized); if mitigation fails, adaptation and degrowth become live policy options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__mitigation_priority_reading, powerless, 0.95).
constraint_indexing:directionality_override(climate_response_imperative__mitigation_priority_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
