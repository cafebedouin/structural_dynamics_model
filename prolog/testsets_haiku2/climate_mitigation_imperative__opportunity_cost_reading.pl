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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Imperative: Opportunity Cost Reading (Nuclear as Capital Diversion)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested
 *   climate_mitigation_imperative kernel: the opportunity-cost reading. The
 *   climate mitigation kernel is a stabilized commitment to rapid carbon
 *   reduction, but different parties read what counts as responsible
 *   mitigation differently. The opportunity-cost reading asserts that
 *   mitigation requires the fastest carbon reduction per dollar per year, and
 *   therefore that nuclear's capital intensity and long timelines make it
 *   net-harmful—capital deployed to nuclear is unavailable for renewables and
 *   storage that deploy faster. This reading benefits renewable energy
 *   sectors and rapid-deployment actors, and extracts from the nuclear
 *   industry and baseload-reliability advocates by reframing nuclear as
 *   climate-misaligned. The constraint is Tangled Rope: it coordinates
 *   climate action around a measurable metric (carbon per dollar per year),
 *   but asymmetrically extracts from nuclear through regulatory framework
 *   shifts that suppress alternative design priorities and technical
 *   objections. The other readings of this
 *   kernel—portfolio_optimization_reading and systems_transition_reading—are
 *   separate constraint stories with their own ε values and stakeholder
 *   asymmetries; they are linked via network.affects_constraints but not
 *   folded into this story.
 *
 * KEY AGENTS:
 *   - renewable_energy_sector: beneficiary, organized power — benefits from capital redirection and permitting acceleration
 *   - nuclear_industry: payer, organized power — bears the extraction through project deferrals and market devaluation
 *   - rapid_deployment_actors (climate agencies, policymakers): agenda-setter, institutional power — set and enforce the opportunity-cost metric
 *   - baseload_reliability_advocates: payer, powerful but suppressed — their technical objections are reframed as status-quo defense
 *   - climate_scientists_and_agencies: observer/analyst, institutional power — generate data that grounds the reading's legitimacy
 *   - centralized_grid_operators: payer + beneficiary, institutional power — face disruption but eventual modernization benefit
 *   - low_income_energy_consumers: nominally beneficiary but trapped — bear transition costs, capture marginal benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.78).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.72).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Imperative: Opportunity Cost Reading (Nuclear as Capital Diversion)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, 'ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093').
narrative_ontology:cs_kernel_codification('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', formalized).
narrative_ontology:cs_authority_grounding('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', expertise).
narrative_ontology:cs_interpretation_layer_present('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093').
narrative_ontology:cs_reading_relation('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', climate_mitigation_imperative__systems_transition_reading, influences).
narrative_ontology:cs_axiom('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', foundational, deployment_speed_is_primary_criterion).
narrative_ontology:cs_axiom_status(deployment_speed_is_primary_criterion, holdable).
narrative_ontology:cs_axiom_grounding('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', deployment_speed_is_primary_criterion, empirically_contingent).
narrative_ontology:cs_axiom('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', foundational, carbon_per_dollar_per_year_metric_is_sufficient).
narrative_ontology:cs_axiom_status(carbon_per_dollar_per_year_metric_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', carbon_per_dollar_per_year_metric_is_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', fastest_achievable_decarbonization).
narrative_ontology:cs_drift_state('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', contemporary_policy_era_2020_2030, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee79c66c-8fd9-41d1-bb9e-7a4b3b91e093', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, distributed_generation_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, rapid_deployment_actors).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_claims).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, centralized_grid_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, centralized_grid_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, low_income_energy_consumers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, low_income_energy_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar, wind, and battery manufacturers and developers benefit from the opportunity-cost frame: capital that would flow to nuclear is redirected to faster-deploying renewables and storage. Manufacturing scale increases, costs decline through learning curves, market adoption accelerates. The constraint's operation vindicates their business model as climate-aligned and economically superior per unit of carbon averted per year.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_sector, beneficiary,
    organized, generational, mobile, global).

% Bears the extraction: capital funding is redirected away from nuclear projects; regulatory frameworks increasingly treat nuclear as capital-inefficient in the mitigation context; new plant starts face stricter cost-per-carbon-avoided scrutiny. Existing plants compete against renewables on marginal cost. The constraint operates by reframing what counts as responsible climate action: nuclear's contributions are discounted as too slow and expensive per ton of CO2 per dollar per year.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    organized, generational, constrained, global).

% Policymakers, climate agencies, and project developers committed to meeting carbon reduction targets on aggressive timelines benefit from the opportunity-cost frame. It justifies fast-track permitting, capital mobilization, and regulatory streamlining for renewables and storage. The constraint operates as an enforcement tool: it legitimates treating deployment speed as a primary design criterion.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, rapid_deployment_actors, beneficiary,
    institutional, biographical, arbitrage, global).

% Face pressure to retire nuclear plants or abandon expansion plans under the opportunity-cost logic, yet also depend on reliable baseload supply. Their cost structure is built around large plants; distributed renewables and storage require grid modernization investment they may not be funded to perform. Simultaneously benefit from reduced capital requirements and modernization pressure (eventual transition advantage), but pay immediate disruption costs.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, centralized_grid_operators, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, centralized_grid_operators, beneficiary).

% Engineers, grid operators, and traditionalist energy analysts who prioritize constant supply argue that opportunity-cost framing undervalues nuclear's reliability contribution and oversimplifies the real-world constraints on renewable integration. The constraint suppresses this analysis: their technical objections are reframed as defending the status quo rather than describing physical constraints. They bear the suppression of their alternative design priorities.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates, payer,
    powerful, generational, trapped, global).

% Set the frame: carbon budget urgency justifies prioritizing fastest deployment per dollar. They generate and disseminate the cost-per-ton and deployment timeline data that the constraint operationalizes. Their authority grounds the legitimacy of the opportunity-cost reading. They have stakes in how their research is translated into policy—some attest the reading simplifies their actual conclusions; others endorse it as pedagogically appropriate for mobilizing action.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_scientists_and_agencies, agenda_setter,
    institutional, generational, analytical, global).

% Would benefit from continued or expanded nuclear demand; currently excluded from the climate-mitigation policy conversation that could preserve their market. Their interests in maintaining uranium demand and geopolitical leverage through fuel supply are structurally kept off the table by the opportunity-cost frame.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, geopolitical_uranium_suppliers, excluded,
    powerful, generational, trapped, global).

% Benefit from lower marginal costs as renewables deployment accelerates and market prices fall. They also bear the upfront transition costs: grid modernization, storage investment, and interim price volatility from legacy plant retirements and intermittency management. Their voice is nominally centered in the constraint (affordability of rapid transition) but they have no control over implementation or benefit capture.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, low_income_energy_consumers, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, low_income_energy_consumers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates climate mitigation around an empirical metric: carbon reduction per dollar per year. Solves the allocation problem by establishing a single, measurable priority that cuts across competing low-carbon technologies, avoiding the deadlock of trying to justify every approach simultaneously.
% TRANSFER_FUNCTION: Moves capital allocation away from nuclear projects toward renewable and storage deployment. Moves regulatory approval speed away from large, lengthy projects toward modular, fast-deploying technologies. Transfers permitting legitimacy from nuclear advocates to renewable developers. Transfers the burden of justification from renewable deployment (why not wait for cheaper nuclear?) to nuclear projects (how do you compete with deployment speed?).
% ABSENT_VOICES: Geopolitical uranium suppliers and baseload-reliability engineers are structurally excluded from the decision-making conversation. They would argue that the constraint oversimplifies by ignoring grid stability complexity, geopolitical supply security, and the physical limits of renewable integration. Low-income energy consumers are nominally included (affordability is mentioned) but have no decisional voice in implementation pace or mechanism.
% DISAPPEARANCE_RATIONALE: If the opportunity-cost constraint vanished, capital would flow back to nuclear projects suspended on opportunity-cost grounds; permitting timelines would lengthen as regulatory urgency around renewable deployment eased; grid operating philosophy would pivot back toward baseload stability as a primary concern; the climate mitigation strategy would revert to portfolio optimization (trying to justify all low-carbon sources) or would reset around the systems-transition reading (democratic decentralization). Energy sector investment patterns, regulatory frameworks, and technology deployment would substantially reorganize.
% FOUNDING_PROBLEM: Carbon reduction must happen at scale and speed compatible with climate budgets. Current climate pledges require cutting emissions by 50% by 2030 and reaching net-zero by 2050. Nuclear plants take 10+ years to build and cost $10–20 billion each. In a carbon-budget-limited world, capital deployed to nuclear is unavailable for renewables and storage that can deploy in 2–3 years at lower cost. The opportunity cost—the carbon NOT reduced because capital was slow-deployed—is larger than the carbon reduced by nuclear itself.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and energy modelers (IPCC, NREL, independent academic analyses) attest that deployment speed is material to feasibility within carbon budgets and that renewable deployment has lower per-dollar-per-year carbon impact. Nuclear industry and reliability engineers attest that the founding problem is misconceived: the real problem is achieving full decarbonization (deep emissions cuts in hard-to-abate sectors like aviation and heat), where nuclear's high-temperature and baseload capabilities are essential. Some corroboration exists from outside the immediate contest: peer-reviewed analyses of grid integration costs note the problem is more complex than the opportunity-cost metric captures; neither side has achieved corroboration that is purely outside its beneficiary set.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint operates by changing what counts as responsible climate action: nuclear's technical and strategic contributions are discounted through a metric (per-dollar-per-year) that advantages renewables and storage over large plants. This is not a natural law or coordination mechanism—it is a framing choice that benefits some actors at the expense of others. Suppression is high (0.72) because the constraint actively suppresses alternative framings: baseload-reliability engineers and nuclear designers have technical objections to renewable-only pathways, but the opportunity-cost frame treats these objections as defending the status quo rather than raising valid physical constraints. Theater ratio is moderate (0.41) because the constraint performs a real coordination function (prioritizing deployment speed) but increasingly serves extractive purposes (excluding baseload alternatives from the policy conversation). The measurement series shows extractiveness rising from 0.58 to 0.78 over the interval as the constraint becomes institutionalized in permitting and financing decisions, suppression requirement rising as more vocal nuclear advocates must be countered, and theater ratio stabilizing as the constraint matures into a normalized policy frame. The time grid is shared across all three metrics; every measurement is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable sector and rapid-deployment actors' seats, this constraint is a clear, scientifically grounded priority that aligns climate action with feasibility. From the nuclear industry and baseload-reliability engineers' seats, it is an arbitrary devaluation of proven low-carbon technology driven by renewable industry interests and a misunderstanding of what full decarbonization requires. The engine computes this divergence from the stakeholder power atoms and exit options: the renewable sector has arbitrage-grade exit (can pivot to other low-carbon technologies or geographies), while the nuclear industry has constrained exit (capital is sunk, skilled workforce is stranded, supply chains are specialized). From the climate scientists' seat, the opportunity-cost frame captures the essence of the problem; some scientists also note it simplifies by ignoring grid stability complexity. The constraint's actual operation depends on suppressing the reliability-engineer perspective, which is where the extraction lives.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are needed; the derivation from beneficiary/victim declarations and exit_options captures the structural asymmetry accurately. The renewable sector's arbitrage-grade exit keeps their d low; the nuclear industry's constrained exit and victim status keep theirs high.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is the carbon budget urgency under climate change. The status is contested: climate scientists attest the urgency is live and that deployment speed is material to feasibility. Nuclear industry and some reliability engineers attest the problem is misconceived—the real constraint is decarbonization depth, not speed, and nuclear is necessary for it. The Tangled Rope classification resolves the mandatrophy because the constraint genuinely coordinates around a measurable metric (carbon per dollar per year) while also asymmetrically extracting from nuclear through regulatory frame shifts. If the extraction vanished but the coordination remained (pure Rope: renewable and nuclear both funded fairly per their marginal carbon impact), the constraint would morph into portfolio_optimization_reading. The Tangled Rope classification preserves both facts: the constraint does coordinate mitigation action AND it does suppress alternative technology pathways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opportunity_cost_vs_portfolio_sufficiency,
    'Can rapid decarbonization be achieved with renewables and storage alone, or is nuclear''s baseload contribution structurally necessary to meet reliability and coverage requirements?',
    'Large-scale grid simulation and real-world deployment in high-renewable-penetration systems (Denmark, Costa Rica, California at various penetration levels) showing whether reliability and cost targets are met, plus peer-reviewed studies on feasibility of 100% renewable energy systems.',
    'If baseload is necessary, the opportunity-cost frame is fundamentally misconceived and nuclear re-enters as beneficiary rather than victim. If 100% renewable grids are feasible, the opportunity-cost frame is structurally sound and nuclear''s extraction is confirmed. If feasibility depends on geographic and temporal factors, the readings coexist (portfolio makes sense in some systems, opportunity-cost in others), but this nuance is suppressed by the current constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_vs_portfolio_sufficiency, empirical, 'Whether baseload electricity is structurally necessary for reliable decarbonization.').

omega_variable(
    metric_distortion_and_system_boundaries,
    'Does the per-dollar-per-year metric capture the full cost of integrating variable renewables (grid modernization, transmission, storage infrastructure, curtailment), or does it systematically undercount the true capital requirements of a renewable-dominated system?',
    'Comprehensive lifecycle cost analysis comparing total system cost (plant plus integration infrastructure) for renewable vs. nuclear pathways, controlling for reliability levels and grid topology.',
    'If integration costs are substantial and underrepresented in the metric, the opportunity-cost frame is Goodhardt drift—the metric has become decoupled from the true optimization problem, and nuclear''s suppression is performative rather than economically grounded. If integration costs are minor, the metric is sound and the constraint''s extraction is legitimate extraction of economically inferior technology from the climate-action budget.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_distortion_and_system_boundaries, empirical, 'Whether the opportunity-cost metric captures true system costs or systematically distorts them.').

omega_variable(
    regulatory_suppression_of_baseload_analysis,
    'Is the suppression of baseload-reliability objections structural (the metric genuinely makes them irrelevant) or performative (the objections are silenced to prevent reopening the nuclear question despite their technical validity)?',
    'Comparison of suppression before and after a jurisdictional shift that deprioritizes the opportunity-cost frame; tracking whether baseload engineers'' objections resurface and whether system failures occur that had been predicted under the suppressed analysis.',
    'If purely performative, the constraint''s Tangled Rope classification stands: genuine coordination with asymmetric extraction. If structural, the suppression is justified by the metric and the constraint tilts toward pure Rope. The distinction determines whether the constraint is sustainable or requires enforcement escalation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_suppression_of_baseload_analysis, empirical, 'Whether suppression of baseload analysis is justified by the metric or is coercive overlay.').

omega_variable(
    contesting_kernel_framing,
    'Is the climate_mitigation_imperative kernel genuinely a unitary commitment to carbon reduction, or does it conceal multiple distinct commitments (urgency vs. systems stability vs. democratic control) that the three readings are actually disputing at the kernel level?',
    'Analysis of how climate scientists, engineers, and political actors articulate the founding problem and their implicit priorities. If the kernel is actually multiple commitments, the three readings are not alternative framings of one commitment—they are enforcing different commitments against each other.',
    'If the kernel is unitary, the readings coexist and influence each other per the schema. If the kernel is multiply constituted, the readings foreclose each other more sharply (opportunity-cost and systems-transition do foreclose on autonomy grounds), and the constraint''s persistence depends on institutional suppression of the latent kernel dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contesting_kernel_framing, conceptual, 'Whether the climate mitigation kernel is a single commitment or multiple distinct commitments in disguise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(clim_su_t40, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(clim_grid_01, climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse(class), 0, 0.52).
narrative_ontology:measurement(clim_grid_02, climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse(class), 40, 0.58).
narrative_ontology:measurement(clim_grid_03, climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_04, climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse(individual), 40, 0.52).
narrative_ontology:measurement(clim_grid_05, climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(clim_grid_06, climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse(organizational), 40, 0.62).
narrative_ontology:measurement(clim_grid_07, climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(clim_grid_08, climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse(structural), 40, 0.72).
narrative_ontology:measurement(clim_grid_09, climate_mitigation_imperative__opportunity_cost_reading, resistance(class), 0, 0.55).
narrative_ontology:measurement(clim_grid_10, climate_mitigation_imperative__opportunity_cost_reading, resistance(class), 40, 0.51).
narrative_ontology:measurement(clim_grid_11, climate_mitigation_imperative__opportunity_cost_reading, resistance(individual), 0, 0.61).
narrative_ontology:measurement(clim_grid_12, climate_mitigation_imperative__opportunity_cost_reading, resistance(individual), 40, 0.58).
narrative_ontology:measurement(clim_grid_13, climate_mitigation_imperative__opportunity_cost_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(clim_grid_14, climate_mitigation_imperative__opportunity_cost_reading, resistance(organizational), 40, 0.62).
narrative_ontology:measurement(clim_grid_15, climate_mitigation_imperative__opportunity_cost_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(clim_grid_16, climate_mitigation_imperative__opportunity_cost_reading, resistance(structural), 40, 0.48).
narrative_ontology:measurement(clim_grid_17, climate_mitigation_imperative__opportunity_cost_reading, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(clim_grid_18, climate_mitigation_imperative__opportunity_cost_reading, stakes_inflation(class), 40, 0.65).
narrative_ontology:measurement(clim_grid_19, climate_mitigation_imperative__opportunity_cost_reading, stakes_inflation(individual), 0, 0.51).
narrative_ontology:measurement(clim_grid_20, climate_mitigation_imperative__opportunity_cost_reading, stakes_inflation(individual), 40, 0.58).
narrative_ontology:measurement(clim_grid_21, climate_mitigation_imperative__opportunity_cost_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(clim_grid_22, climate_mitigation_imperative__opportunity_cost_reading, stakes_inflation(organizational), 40, 0.71).
narrative_ontology:measurement(clim_grid_23, climate_mitigation_imperative__opportunity_cost_reading, stakes_inflation(structural), 0, 0.74).
narrative_ontology:measurement(clim_grid_24, climate_mitigation_imperative__opportunity_cost_reading, stakes_inflation(structural), 40, 0.81).
narrative_ontology:measurement(clim_grid_25, climate_mitigation_imperative__opportunity_cost_reading, suppression(class), 0, 0.61).
narrative_ontology:measurement(clim_grid_26, climate_mitigation_imperative__opportunity_cost_reading, suppression(class), 40, 0.68).
narrative_ontology:measurement(clim_grid_27, climate_mitigation_imperative__opportunity_cost_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_28, climate_mitigation_imperative__opportunity_cost_reading, suppression(individual), 40, 0.54).
narrative_ontology:measurement(clim_grid_29, climate_mitigation_imperative__opportunity_cost_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(clim_grid_30, climate_mitigation_imperative__opportunity_cost_reading, suppression(organizational), 40, 0.73).
narrative_ontology:measurement(clim_grid_31, climate_mitigation_imperative__opportunity_cost_reading, suppression(structural), 0, 0.71).
narrative_ontology:measurement(clim_grid_32, climate_mitigation_imperative__opportunity_cost_reading, suppression(structural), 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the climate_mitigation_imperative kernel. The kernel is a stabilized commitment to rapid carbon reduction formalized in climate policy and international agreements, but different parties read what counts as responsible mitigation differently. The opportunity_cost_reading asserts fastest deployment per dollar as the primary metric, making nuclear capital-inefficient. The portfolio_optimization_reading asserts maximizing all low-carbon sources including nuclear for reliability. The systems_transition_reading asserts democratic decentralization as the criterion, making nuclear's centralization extractive. Each reading instantiates a distinct constraint with its own ε (epistemic referent: the standing arrangement each reading interprets), beneficiary/victim structure, and type. The three are linked via network.affects_constraints: the opportunity-cost reading influences the other two by establishing deployment speed as a normative criterion, which the portfolio reading must defend against and the transition reading reframes as false urgency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__opportunity_cost_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
