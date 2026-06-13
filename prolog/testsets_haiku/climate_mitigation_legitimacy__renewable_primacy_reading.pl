% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy Climate Mitigation Claim
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This is ONE READING of a contested kernel (climate_mitigation_legitimacy)
 *   addressing the question: 'What is the optimal pathway to full
 *   decarbonization?' The renewable-primacy reading claims that renewables
 *   plus storage can deliver decarbonization faster and cheaper than nuclear.
 *   This reading structures energy policy globally, directing capital toward
 *   distributed renewable deployment and away from baseload nuclear projects,
 *   declaring nuclear a capital sink that delays decarbonization. The
 *   constraint manifests as a policy framework, a consensus among climate
 *   advocates and renewable investors, and an assumption embedded in climate
 *   models and investment decisions. The constraint is CLAIMED as
 *   tangled_rope (coordination function around decarbonization + asymmetric
 *   extraction from nuclear and baseload operators) while the metrics
 *   describe substantial extraction (0.62), high suppression (0.58), and
 *   rising theater (0.41 at interval end), suggesting the constraint is
 *   increasingly maintained through enforcement rather than voluntary
 *   coordination. The claim/metric divergence is structural: from the
 *   renewable-development and climate-urgency seats, the constraint is
 *   genuine coordination around an urgent collective problem; from the
 *   nuclear-industry and grid-operator seats, the same structure operates as
 *   enforced exclusion and capital redirection.
 *
 * KEY AGENTS:
 *   - distributed_renewable_developers: beneficiary, mobile exit, organized power — capture policy through advocacy and demonstration projects
 *   - battery_storage_manufacturers: beneficiary, arbitrage exit, institutional power — positioned as enabler; benefit from storage-necessity framing
 *   - climate_urgency_advocates: beneficiary, constrained exit, organized power — benefit from techno-optimist narrative avoiding demand-reduction compromise
 *   - nuclear_industry: victim, constrained exit, institutional power — capital diverted; policy legitimacy undermined; trapped in stranded-asset risk
 *   - fossil_baseload_operators: victim, trapped exit, institutional power — delegitimized and abandoned; no residual role in decarbonization
 *   - grid_infrastructure_incumbents: victim, constrained exit, institutional power — forced to retrofit centralized infrastructure for distributed generation under accelerated timelines
 *   - energy_analysts_modeling_community: agenda-setter, analytical power — sets framing through choice of cost assumptions, learning curves, discount rates, and modeling scope
 *   - climate_policy_makers: agenda-setter, institutional power — enforce through mandates, subsidies, grid-access rules, carbon pricing
 *   - baseload_necessity_advocates: excluded, constrained exit — structurally locked out of decarbonization pathway design
 *   - degrowth_theorists: excluded, constrained exit — excluded from growth-compatible decarbonization assumption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy Climate Mitigation Claim").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, 'dc31357b-307e-4a05-9bb0-e03e1cec8503').
narrative_ontology:cs_kernel_codification('dc31357b-307e-4a05-9bb0-e03e1cec8503', distributed).
narrative_ontology:cs_authority_grounding('dc31357b-307e-4a05-9bb0-e03e1cec8503', expertise).
narrative_ontology:cs_interpretation_layer_present('dc31357b-307e-4a05-9bb0-e03e1cec8503').
narrative_ontology:cs_reading_relation('dc31357b-307e-4a05-9bb0-e03e1cec8503', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc31357b-307e-4a05-9bb0-e03e1cec8503', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('dc31357b-307e-4a05-9bb0-e03e1cec8503', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('dc31357b-307e-4a05-9bb0-e03e1cec8503', foundational, storage_adequacy_at_scale).
narrative_ontology:cs_axiom_status(storage_adequacy_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('dc31357b-307e-4a05-9bb0-e03e1cec8503', storage_adequacy_at_scale, empirically_contingent).
narrative_ontology:cs_axiom('dc31357b-307e-4a05-9bb0-e03e1cec8503', foundational, renewable_cost_curve_continuation).
narrative_ontology:cs_axiom_status(renewable_cost_curve_continuation, holdable).
narrative_ontology:cs_axiom_grounding('dc31357b-307e-4a05-9bb0-e03e1cec8503', renewable_cost_curve_continuation, empirically_contingent).
narrative_ontology:cs_axiom('dc31357b-307e-4a05-9bb0-e03e1cec8503', secondary, growth_compatible_decarbonization).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('dc31357b-307e-4a05-9bb0-e03e1cec8503', growth_compatible_decarbonization, instrumental).
narrative_ontology:cs_reference_frame('dc31357b-307e-4a05-9bb0-e03e1cec8503', renewable_sufficiency_paradigm).
narrative_ontology:cs_drift_state('dc31357b-307e-4a05-9bb0-e03e1cec8503', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dc31357b-307e-4a05-9bb0-e03e1cec8503', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_urgency_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_baseload_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_infrastructure_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, grid_modernization_specialists).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_modernization_specialists).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, cost_curve_inversion_hypothesis).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, storage_scalability_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy solar, wind, and other renewable projects globally. The renewable-primacy reading legitimates rapid expansion by declaring renewables sufficient for decarbonization, directs investment subsidies and preferential grid access toward renewable projects, and frames nuclear as an unnecessary competitor. They benefit from accelerated deployment timelines, policy mandates favoring renewables, and reduced regulatory scrutiny for project approval. They can exit by relocating to other jurisdictions with different energy policies, though global climate policy alignment means renewable developers maintain advantageous positioning across most markets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers, beneficiary,
    organized, biographical, mobile, global).

% Manufacture lithium-ion batteries, long-duration storage systems, and other energy storage technologies. The renewable-primacy reading positions storage as the critical enabling technology: by claiming that 'renewables plus storage' can achieve full decarbonization, the reading creates massive addressable market demand for battery systems. Manufacturers benefit from framing storage-cost-reduction as the decarbonization bottleneck and solution. They collect rent from the policy-driven demand and can arbitrage between markets (high-subsidy regions vs. open markets), shifting production and pricing as policy shifts.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Climate NGOs, environmental advocates, and climate-focused researchers who advocate for rapid decarbonization. They benefit from the renewable-primacy reading because it provides a techno-optimist pathway to decarbonization that does not require demand-side behavior change, managed retreat, degrowth, or consumption reduction. The reading allows climate advocates to maintain a growth-compatible narrative around climate action, making climate policy more politically acceptable across diverse constituencies. They face constrained exit: shifting to a degrowth or demand-reduction framing would fragment their coalition and reduce political influence.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_urgency_advocates, beneficiary,
    organized, generational, constrained, global).

% Nuclear utilities, reactor manufacturers, fuel suppliers, and engineering firms that develop nuclear power. The renewable-primacy reading frames nuclear as slower and more expensive than renewables, delegitimizing new nuclear investment even in applications where nuclear would provide dispatchable baseload. Capital intended for new nuclear builds is redirected to renewable and storage projects. Existing nuclear plants continue operating but face accelerated decommissioning timelines. The industry bears stranded-asset risk as replacement capacity becomes politically and financially inaccessible. Engineering talent flows away from nuclear toward renewable and storage sectors. They face constrained exit: they cannot adapt nuclear into the renewable-primary vision without abandoning their core business model of long-lived, capital-intensive, dispatchable generation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% Coal, natural-gas, and other fossil fuel generators that provide dispatchable power. The renewable-primacy reading frames these operators as entirely obsolete—not as transitional baseload pending renewable buildout, but as entirely unnecessary. Fossil operators face acceleration of their displacement from the energy mix, with policy mandating wholesale replacement by renewables plus storage. They have no viable exit: they cannot compete for capital within the renewable-primary framework, and the fossil-fuel-free future leaves no residual market for their services. Their assets become stranded; their political influence in decarbonization planning collapses.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_baseload_operators, payer,
    institutional, generational, trapped, global).

% Transmission and distribution utilities, grid operators, and the technical staff managing electricity systems. The reading's emphasis on distributed renewable generation and rapid deployment creates cascading infrastructure costs: centralized grid architecture designed for large baseload plants must be retrofitted for distributed generation, high-frequency balancing, and intermittency management. Grid operators must redesign dispatch algorithms, frequency response mechanisms, and interoperability standards under accelerated timelines, creating operational risk, project overruns, and technical uncertainty. They face constrained exit: they must operate the grid under the new constraints (distributed, variable renewable generation) or hand authority to decentralized operators, but they cannot maintain the incumbent architecture.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_infrastructure_incumbents, payer,
    powerful, generational, constrained, national).

% Engineering firms, software vendors, and consultants specializing in grid modernization, advanced forecasting, distributed-generation coordination, and microgrid systems. They benefit from the demand for grid services; the renewable-primary transition creates a multi-decade market for modernization projects. They pay through operational uncertainty and pressure to develop solutions faster than infrastructure and regulatory frameworks can support, creating technical risk and project cost overruns. They can exit by shifting focus to different regions or technologies if a particular market cools.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_modernization_specialists, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, grid_modernization_specialists, payer).

% Academic researchers, national laboratory analysts, and consulting firms that model energy pathways and cost curves. They set the framing by choosing which models are treated as authoritative. The renewable-primacy reading depends on specific analytical choices: learning-curve projections for battery costs, assumptions about storage roundtrip efficiency improvements, discount rates applied to long-duration-storage requirements, capacity-factor assumptions for wind and solar, and timelines for grid-integration challenges. Analysts choosing parameters that validate rapid renewables deployment enable the renewable-primacy reading; analysts choosing different parameters (higher storage costs, longer timelines, larger grid infrastructure expenses) enable competing readings. Their authority derives from perceived technical neutrality, but their parameter choices are consequential.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_analysts_modeling_community, agenda_setter,
    analytical, biographical, analytical, global).

% National and international climate negotiators, energy ministers, and central-bank governors who set climate targets and decarbonization policies. They enforce the renewable-primacy reading through carbon pricing, renewable-energy mandates, investment subsidies, grid-access rules that privilege distributed generation, and regulatory approval processes biased toward renewable projects. They set boundary conditions: which technologies receive support, which face carbon pricing, how rapidly deployment timelines are compressed. They constrain their own exit by committing to net-zero targets that depend on the reading's speed and cost projections being accurate; if the renewable pathway fails to deliver, they face political backlash and treaty violations.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Represent the competing baseload-necessity reading of the climate-mitigation kernel and are structurally excluded from decarbonization pathway design. This group includes nuclear advocates, grid-reliability engineers in cold climates, and energy analysts modeling long-duration storage challenges. They would argue that renewables alone cannot meet decarbonization targets, that storage solutions are overstated in cost and capability, and that nuclear and other firm generation remain necessary for reliable grids. They face constrained exit: they can publish analyses, testify at regulatory proceedings, and maintain research communities, but policy design and capital allocation proceed under the renewable-primacy framing. Their exclusion is active suppression: their arguments are delegitimized as 'pro-nuclear ideology' regardless of technical merit.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_necessity_advocates, excluded,
    organized, generational, constrained, global).

% Environmental economists, ecological thinkers, and systems analysts who argue that full decarbonization without demand reduction is physically impossible. Degrowth theorists represent a competing reading of the climate-mitigation kernel. They argue that the renewable-primacy assumption of growth-compatible decarbonization is false; the kernel's true solution is to stabilize and reduce energy consumption. They are excluded from mainstream climate policy design, which assumes decarbonization is compatible with maintained or increased energy services. Their position is marginalized as 'anti-growth ideology,' even when grounded in energy accounting or material flows. They face constrained exit: their analyses exist in academic literature and activist networks, but do not shape policy or investment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, degrowth_theorists, excluded,
    moderate, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared technical consensus on the optimal pathway to full decarbonization: coordinates investment, policy, and R&D around rapid renewable plus storage deployment. Solves the collective-action problem of technology selection and capital sequencing—different parties would make different investment choices without coordination; the renewable-primacy framing directs capital toward distributed generation and away from competing alternatives.
% TRANSFER_FUNCTION: Diverts capital investment from nuclear plants, natural-gas peaker plants, and other dispatchable baseload toward solar, wind, and battery storage. Transfers policy authority from incumbent baseload operators and centralized grid operators toward distributed-generation developers and rapid-deployment coordinators. Moves political legitimacy from baseload-necessity framing toward storage-abundance framing; declares nuclear-sector expertise as outdated; raises the status of renewable-sector researchers and developers.
% ABSENT_VOICES: Nuclear industry and baseload-necessity advocates are excluded from decarbonization pathway design—they have no seat at the policy table where technology choices are made. Fossil operators are delegitimized rather than excluded; they are acknowledged but marked for obsolescence. Degrowth advocates are excluded from the foundational assumption that decarbonization can occur without demand reduction. Rival payment networks (from the example) do not apply here; the energy sector has no direct 'excluded competing processors,' but regional grid operators in weak wind/solar regions face exclusion if their case for baseload dependence is suppressed.
% DISAPPEARANCE_RATIONALE: If the renewable-primacy constraint and its policy enforcement vanished overnight, capital investment would flow toward nuclear and dispatchable-generation projects; climate policy would revert to technology-neutral frameworks or portfolio-based approaches; grid architecture would evolve around firm generation rather than intermittent distributed sources; the energy sector would reorganize around different dispatch models, investment timelines (longer), industrial winners (nuclear, grid infrastructure), and coal/gas phase-out timelines (slower). The absence would require renegotiating climate targets or accepting longer decarbonization timelines.
% FOUNDING_PROBLEM: Global decarbonization toward net-zero requires rapid displacement of fossil fuels from electricity systems. Climate targets (net-zero by 2050) demand near-complete decarbonization of the electricity sector within 25–30 years. Early analyses in the 2000s suggested nuclear was the only firm, scalable zero-carbon source, but nuclear cost overruns and construction delays lengthened timelines. Renewable costs dropped faster than projected (learning curves exceeded expectations), creating an opening for an alternative pathway: could renewables plus storage deliver decarbonization faster and more cheaply than nuclear?
% FOUNDING_PROBLEM_CORROBORATION: The cost-decline component is corroborated by independent data: IEA, BNEF, and NREL reports confirm that solar and wind costs have fallen faster than projected. Climate urgency is corroborated by IPCC assessments and independent climate science. However, whether renewables-plus-storage CAN achieve full decarbonization at scale without dispatchable baseload is NOT corroborated by independent voices outside the renewable industry. Corroboration gaps: (1) The nuclear industry disputes the 'faster and cheaper' claim, citing learning-curve improvements in advanced reactors and long-duration-storage challenges. (2) Grid operators in cold climates (Scandinavia, Canada, northern US) dispute seasonal storage feasibility and cite ongoing dependence on hydropower and nuclear for winter baseload. (3) Independent energy analysts (MIT, IVL, DNV) publish mixed results: some models validate renewable-only pathways with aggressive storage assumptions; others identify bottlenecks requiring dispatchable generation or demand reduction. (4) No jurisdiction has yet operated a full decarbonized grid on a renewable-only pathway—all existing high-renewable grids (Denmark, Costa Rica) maintain hydro or nuclear backup or fossil reserve capacity. Corroboration is strongest from renewable-industry analyses (IRENA, BNEF) and weakest from incumbent baseload sectors. The founding-problem dispute is technical (can storage + renewable generation meet winter peak demand in high-latitude grids?) but shaped by structural interests: renewable developers benefit from claiming sufficiency; nuclear operators benefit from claiming necessity.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (2015) to 0.62 (2050), peaking around 2040 (0.68), then declining slightly as nuclear plants are decommissioned and the extraction target (nuclear capital allocation) is eliminated. Suppression rises from 0.32 to 0.58 as policy must actively exclude competing pathways (nuclear, dispatchable generation) and suppress baseload-necessity arguments to maintain the reading's authority. Theater_ratio rises from 0.18 to 0.44 (by 2030) then declines to 0.41, suggesting that early-stage promotion (conferences, modeling papers, policy white papers) has high theater content, but as deployment proceeds and real grid-integration challenges emerge, the performance-to-function ratio drops slightly—the constraint's real function is increasingly visible alongside its rhetorical function. The coercion_grid shows structural-level suppression (0.62 at 2050) driven by policy exclusion of competing technologies; organizational-level resistance is highest (0.75 at 2050) from incumbent baseload operators and grid operators resisting the forced transition; individual-level resistance is lower (0.62 at 2050) as households benefit from cheaper renewable electricity. Accessibility_collapse is moderate (0.52 structural by 2050) because nuclear and baseload projects remain legally possible and technically feasible—they are not impossible, only delegitimized and defunded. Time points use one shared grid so every metric is authored at every examined moment, avoiding misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (policy-makers and energy analysts) and beneficiary seats (renewable developers, storage manufacturers) experience the constraint as genuine coordination around decarbonization urgency; they compute it as solving a real collective-action problem (which technologies to back, how to sequence capital, how to align climate targets with technological feasibility). The victim seats (nuclear industry, grid operators) experience the same structure as enforced extraction: their capital, policy influence, and operational authority are being transferred to renewable developers and storage manufacturers through policy mandates and subsidy programs, independent of technical merit or cost-competitiveness in specific applications. From the grid-operator seat, the constraint imposes infrastructure costs and technical risks (balancing a 70%+ renewable grid requires capabilities not yet fully deployed). From the nuclear-industry seat, the constraint eliminates the possibility of competing on technical grounds; their exit option is abandonment of the decarbonization space, not adaptation within it. The engine computes these divergences from the structural data: power atoms differ (policy-makers are institutional, nuclear operators are institutional but with declining legitimacy in policy-making); exit_options differ (policy-makers have analytical/constrained exit; nuclear operators have only trapped/constrained); beneficiary/victim declarations establish the directionality. The measurement series show the constraint tightening: suppression_requirement rises as policy must work harder to suppress baseload alternatives; theater_ratio is already significant and rises to 0.44 by 2030, suggesting the constraint relies increasingly on persuasion and narrative enforcement rather than technical demonstration.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are explicitly declared: distributed renewable developers (organized power, mobile exit—they can relocate to other jurisdictions; the constraint raises their investment returns and policy priority, d ≈ 0.15, near-beneficiary end), battery manufacturers (institutional power, arbitrage exit—they benefit from the storage-necessity framing and can shift production between markets, d ≈ 0.20), climate advocates (organized power, constrained exit within climate activism—they benefit from a techno-optimist framing, d ≈ 0.25). Victims are: nuclear industry (institutional power, constrained exit—capital diverted, legitimacy lost, no alternative market, d ≈ 0.85, near-target end), fossil baseload (institutional power, trapped exit—abandoned entirely, d ≈ 0.90), grid operators (institutional power, constrained exit—forced to absorb infrastructure costs, d ≈ 0.75). The analytical observer seat (researchers, analysts) has power=analytical, exit=analytical, so d is not computed from beneficiary/victim derivation; they sit outside the extraction mechanism. No directionality overrides are needed: the derivation chain (beneficiary/victim + exit + power → d) produces accurate directionality for each seat without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (global decarbonization requires rapid fossil displacement) is LIVE but CONTESTED: climate urgency is affirmed by IPCC and independent climate science; however, whether renewables-plus-storage are SUFFICIENT to achieve full decarbonization WITHOUT dispatchable baseload is actively disputed. The reading's claim that nuclear is unnecessary, slower, and more expensive is contested by the nuclear industry, by grid operators in regions with weak wind/solar resources, and by energy analysts modeling long-duration storage challenges. The constraint avoids pure mandatrophy (where the founding problem is universally agreed to be solved) by relying on contestation: the policy-making consensus suppresses dissenting voices on the technical question, treating the founding problem as 'solved if you accept our answer.' This is tangled_rope characteristic: genuine coordination (decarbonization) + asymmetric extraction (nuclear capital diverted) + active enforcement (suppression of competing pathways) + contested mandate (is renewables-only truly sufficient?). If the founding problem's contestation is not resolved (if 30 years of operation prove that long-duration storage cannot be scaled economically, or that grid stability requires dispatchable baseload even with storage), the constraint may shift from tangled_rope toward snare (pure extraction hiding behind a failed coordination claim). Currently, mandatrophy is held in check by the projected timeline (2050) being far enough that validation can be deferred; as actual deployment data accumulates, the constraint will either validate (renewables reach the projected cost and performance targets) or invalidate (the founding assumption about storage adequacy or cost curves proves wrong).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    storage_duration_adequacy,
    'Can battery and other storage technologies provide reliable backup across seasonal and multi-day weather patterns at cost curves projections assume, or does long-duration storage create a permanent bottleneck that requires dispatchable baseload (nuclear or fossil)?',
    'Real-world grid operations data from high-renewable-penetration systems (Denmark, South Australia, California) showing whether seasonal storage needs are met, at what cost, and with what dispatchability gaps. Modeling of 100% renewable grids under extreme-weather scenarios.',
    'If long-duration storage remains uneconomical or technically limited, the renewable-primacy reading collapses into baseload_necessity_reading, and the victim set (nuclear industry) shifts to beneficiary. If storage scales as projected, the reading holds and nuclear remains victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_duration_adequacy, empirical, 'Whether storage can substitute for dispatchable baseload at grid scale.').

omega_variable(
    cost_curve_inversion_timing,
    'Are the learning-curve assumptions for battery and renewable costs accurate enough to justify the timeline? Do cost declines continue as projected, or do they plateau due to materials constraints, supply-chain bottlenecks, or manufacturing limits?',
    'Continued tracking of actual cost data (BNEF, IEA, NREL) against published learning-curve models. Early signals of cost deceleration or supply constraints.',
    'If cost curves decelerate, the ''faster and cheaper than nuclear'' claim weakens; the timeline extends and nuclear may become competitive again, reframing the constraint as portfolio_pragmatism_reading instead of renewable primacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_curve_inversion_timing, empirical, 'Whether projected cost declines sustain through decarbonization endpoint.').

omega_variable(
    modeling_parameter_contestation,
    'Which modeling parameters (discount rates, storage efficiency assumptions, transmission expansion costs, grid balancing requirements) drive the conclusion that renewables are cheaper and faster? Are these parameters transparent and independently auditable, or embedded in proprietary models?',
    'Publication of full modeling assumptions and source code. Independent replication of headline cost and speed claims. Sensitivity analysis showing which parameter choices flip the verdict.',
    'High dependence on opaque or contested parameters undermines the reading''s claim to neutrality and exposes it as advocacy framing rather than empirical consensus. Increases legitimacy of competing readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modeling_parameter_contestation, conceptual, 'Whether the renewable-primacy claim rests on transparent analysis or hidden assumptions.').

omega_variable(
    grid_stability_under_intermittency,
    'At high renewable penetration (80%+), what is the frequency response capability, voltage stability margin, and inertia requirement? Can grid-following inverters and fast-response controls substitute for synchronous generation, or is some spinning reserve required?',
    'Grid simulation and laboratory testing of high-renewable systems. Operational experience from grids exceeding 70% renewable penetration.',
    'If substantial synchronous reserve is required, firm generation (nuclear or otherwise) becomes necessary, moving the constraint closer to portfolio_pragmatism_reading or baseload_necessity_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_under_intermittency, empirical, 'Whether 100% renewable grids are technically feasible without additional firm generation.').

omega_variable(
    reading_incommensurability_with_baseload,
    'Is the renewable-primacy reading logically incompatible with the baseload-necessity reading, or do they describe different deployment scenarios (regions with high solar/wind resources vs. those with poor intermittency characteristics)?',
    'Geography-specific analysis: can renewable-plus-storage deliver full decarbonization in ALL climate zones, or only in wind/solar-abundant regions? If only abundant regions, the readings coexist (different strategies for different places); if all regions, one forecloses the other.',
    'If geography-specific, both readings remain live (influences or coexists_with). If renewable-primacy claims global universality, it forecloses baseload-necessity at the global level, but not regionally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incommensurability_with_baseload, conceptual, 'Whether the renewable-primacy and baseload-necessity readings are mutually exclusive globally or regionally differentiated.').

omega_variable(
    actor_alignment_artifact,
    'How much of the renewable-primacy claim''s persuasiveness comes from alignment with renewable industry interests and climate advocacy coalitions, vs. independent technical analysis? Is the reading more ''consensus opinion of beneficiaries'' than ''objective truth''?',
    'Cross-check against independent energy analyses (national labs, IEA, academic research without funding from renewable companies). Examine disagreement patterns: who disputes the claim, and what are their structural interests?',
    'High alignment with beneficiary interests suggests the reading is partially captured analysis, increasing suppression (enforcement requires delegitimizing competing views) and raising the extraction signal. If confirmed, the constraint shifts toward snare characteristics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actor_alignment_artifact, empirical, 'Whether the renewable-primacy reading reflects independent consensus or beneficiary-aligned advocacy.').

omega_variable(
    material_supply_constraints,
    'Does the projected deployment rate require more lithium, cobalt, rare earths, silicon, and land area than global supply chains can deliver? Or do physical constraints emerge that force slower deployment than the reading assumes?',
    'Materials accounting: supply audits (USGS, IVL), mining capacity forecasts, recycling infrastructure development.',
    'If material constraints emerge, the ''faster'' claim fails; the timeline extends, potentially to the point where nuclear becomes competitive again on deployment speed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_supply_constraints, empirical, 'Whether material supply chains can sustain projected renewable and storage deployment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 2015, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2024, 0.35).
narrative_ontology:measurement_basis(clim_tr_t2024, observed).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2030, 0.44).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2040, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2050, 0.41).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2024, 0.55).
narrative_ontology:measurement_basis(clim_be_t2024, observed).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2050, 0.62).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2024, 0.48).
narrative_ontology:measurement_basis(clim_su_t2024, observed).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2030, 0.62).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2040, 0.64).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2050, 0.58).
narrative_ontology:measurement_basis(clim_su_t2050, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=2015, tn=2050
narrative_ontology:measurement(clim_grid_01, climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse(class), 2015, 0.22).
narrative_ontology:measurement(clim_grid_02, climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse(class), 2050, 0.48).
narrative_ontology:measurement(clim_grid_03, climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse(individual), 2015, 0.18).
narrative_ontology:measurement(clim_grid_04, climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse(individual), 2050, 0.42).
narrative_ontology:measurement(clim_grid_05, climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse(organizational), 2015, 0.28).
narrative_ontology:measurement(clim_grid_06, climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse(organizational), 2050, 0.58).
narrative_ontology:measurement(clim_grid_07, climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse(structural), 2015, 0.35).
narrative_ontology:measurement(clim_grid_08, climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse(structural), 2050, 0.52).
narrative_ontology:measurement(clim_grid_09, climate_mitigation_legitimacy__renewable_primacy_reading, resistance(class), 2015, 0.65).
narrative_ontology:measurement(clim_grid_10, climate_mitigation_legitimacy__renewable_primacy_reading, resistance(class), 2050, 0.72).
narrative_ontology:measurement(clim_grid_11, climate_mitigation_legitimacy__renewable_primacy_reading, resistance(individual), 2015, 0.48).
narrative_ontology:measurement(clim_grid_12, climate_mitigation_legitimacy__renewable_primacy_reading, resistance(individual), 2050, 0.62).
narrative_ontology:measurement(clim_grid_13, climate_mitigation_legitimacy__renewable_primacy_reading, resistance(organizational), 2015, 0.72).
narrative_ontology:measurement(clim_grid_14, climate_mitigation_legitimacy__renewable_primacy_reading, resistance(organizational), 2050, 0.75).
narrative_ontology:measurement(clim_grid_15, climate_mitigation_legitimacy__renewable_primacy_reading, resistance(structural), 2015, 0.58).
narrative_ontology:measurement(clim_grid_16, climate_mitigation_legitimacy__renewable_primacy_reading, resistance(structural), 2050, 0.68).
narrative_ontology:measurement(clim_grid_17, climate_mitigation_legitimacy__renewable_primacy_reading, stakes_inflation(class), 2015, 0.28).
narrative_ontology:measurement(clim_grid_18, climate_mitigation_legitimacy__renewable_primacy_reading, stakes_inflation(class), 2050, 0.55).
narrative_ontology:measurement(clim_grid_19, climate_mitigation_legitimacy__renewable_primacy_reading, stakes_inflation(individual), 2015, 0.22).
narrative_ontology:measurement(clim_grid_20, climate_mitigation_legitimacy__renewable_primacy_reading, stakes_inflation(individual), 2050, 0.48).
narrative_ontology:measurement(clim_grid_21, climate_mitigation_legitimacy__renewable_primacy_reading, stakes_inflation(organizational), 2015, 0.35).
narrative_ontology:measurement(clim_grid_22, climate_mitigation_legitimacy__renewable_primacy_reading, stakes_inflation(organizational), 2050, 0.64).
narrative_ontology:measurement(clim_grid_23, climate_mitigation_legitimacy__renewable_primacy_reading, stakes_inflation(structural), 2015, 0.42).
narrative_ontology:measurement(clim_grid_24, climate_mitigation_legitimacy__renewable_primacy_reading, stakes_inflation(structural), 2050, 0.68).
narrative_ontology:measurement(clim_grid_25, climate_mitigation_legitimacy__renewable_primacy_reading, suppression(class), 2015, 0.35).
narrative_ontology:measurement(clim_grid_26, climate_mitigation_legitimacy__renewable_primacy_reading, suppression(class), 2050, 0.54).
narrative_ontology:measurement(clim_grid_27, climate_mitigation_legitimacy__renewable_primacy_reading, suppression(individual), 2015, 0.32).
narrative_ontology:measurement(clim_grid_28, climate_mitigation_legitimacy__renewable_primacy_reading, suppression(individual), 2050, 0.52).
narrative_ontology:measurement(clim_grid_29, climate_mitigation_legitimacy__renewable_primacy_reading, suppression(organizational), 2015, 0.32).
narrative_ontology:measurement(clim_grid_30, climate_mitigation_legitimacy__renewable_primacy_reading, suppression(organizational), 2050, 0.58).
narrative_ontology:measurement(clim_grid_31, climate_mitigation_legitimacy__renewable_primacy_reading, suppression(structural), 2015, 0.28).
narrative_ontology:measurement(clim_grid_32, climate_mitigation_legitimacy__renewable_primacy_reading, suppression(structural), 2050, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__renewable_primacy_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_capital_allocation_decision).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, grid_infrastructure_modernization_mandate).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, battery_supply_chain_constraint).

% DUAL FORMULATION NOTE:
% The climate_mitigation_legitimacy kernel has four readings structured around competing claims about optimal decarbonization strategy. This reading (renewable-primacy) claims renewables plus storage are sufficient, faster, and cheaper than nuclear. The baseload-necessity reading claims dispatchable baseload (including nuclear) is required. The portfolio-pragmatism reading claims both are needed. The degrowth reading claims demand reduction is the real solution. Each reading is a separate constraint with its own ε-invariance, beneficiary/victim structure, and type classification. They affect each other through causality (if renewable-primacy successfully redirects capital away from nuclear, it influences baseload-necessity reading's viability) and through legitimacy contestation (the readings compete for policy authority). All four readings share the upstream founding problem (climate change requires decarbonization) but diverge on solution strategy. Network edges point from upstream (founding-problem-level) to downstream (solution-strategy) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
