% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response via Structural Economic Transformation (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth reading of the contested
 *   climate response kernel: climate stabilization requires structural
 *   economic transformation in wealthy nations—material consumption
 *   contraction, working-time reduction, institutional redesign away from
 *   growth dependence, and North-to-South redistribution. The reading asserts
 *   that technological mitigation alone cannot achieve 1.5°C targets and that
 *   carbon budgets are too small for global convergence to Northern
 *   consumption levels. Present-day Global North consumers and
 *   growth-dependent workers enter the victim set by bearing consumption loss
 *   and forced economic restructuring; future generations and Global South
 *   populations benefit from climate stabilization and redistribution. The
 *   constraint is claimed as tangled_rope (genuine coordination function in
 *   climate stabilization + asymmetric extraction from Northern actors) and
 *   measured accordingly.
 *
 * KEY AGENTS:
 *   - global_north_consumers: structural targets of consumption reduction (moderate power, constrained exit, biographical horizon)
 *   - future_generations: voiceless beneficiaries of climate stability and post-growth institutions (powerless, trapped exit, civilizational horizon)
 *   - global_south_populations: beneficiaries of redistribution and reduced climate impacts (organized power, constrained exit, generational horizon)
 *   - high_consumption_workers: payers whose professional identity is fused to high-material sectors (moderate power, identity-locked exit, biographical horizon)
 *   - post_growth_institutional_builders: agenda-setters designing and enforcing the transition (institutional power, analytical perspective)
 *   - carbon_intensive_industries: structurally excluded advocates for alternative (mitigation-priority) framing (powerful, mobile exit)
 *   - technological_mitigation_advocates: excluded representatives of the mitigation_priority_reading sibling (institutional power, analytical perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.62).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response via Structural Economic Transformation (Degrowth Reading)").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '48f92ccd-6eb5-404e-b657-aeb7fe9ff192').
narrative_ontology:cs_kernel_codification('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', distributed).
narrative_ontology:cs_authority_grounding('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', expertise).
narrative_ontology:cs_interpretation_layer_present('48f92ccd-6eb5-404e-b657-aeb7fe9ff192').
narrative_ontology:cs_reading_relation('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', climate_response_imperative__mitigation_priority_reading, forecloses).
narrative_ontology:cs_reading_relation('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', foundational, carbon_budgets_binding_constraint).
narrative_ontology:cs_axiom_status(carbon_budgets_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', carbon_budgets_binding_constraint, empirically_contingent).
narrative_ontology:cs_axiom('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', foundational, technology_decoupling_insufficient).
narrative_ontology:cs_axiom_status(technology_decoupling_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', technology_decoupling_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', foundational, consumption_contraction_necessary).
narrative_ontology:cs_axiom_status(consumption_contraction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', consumption_contraction_necessary, instrumental).
narrative_ontology:cs_axiom('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', foundational, historical_emissions_create_redistribution_obligation).
narrative_ontology:cs_axiom_status(historical_emissions_create_redistribution_obligation, holdable).
narrative_ontology:cs_axiom_grounding('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', historical_emissions_create_redistribution_obligation, deontological).
narrative_ontology:cs_reference_frame('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', planetary_boundaries_binding_stability).
narrative_ontology:cs_drift_state('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', contemporary_climate_emergency, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('48f92ccd-6eb5-404e-b657-aeb7fe9ff192', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, high_consumption_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, high_consumption_workers).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, planetary_boundaries_doctrine).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, intergenerational_equity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of the degrowth transition: reduced consumption of material goods, energy rationing, lower per-capita resource availability, and lifestyle restructuring. Their exit options are constrained by the fact that climate transformation is global — leaving the jurisdiction does not escape the physical constraint, and capital mobility cannot escape the resource redistribution logic. They experience this reading as imposing sacrifice in the present for benefits accruing primarily to future people and distant populations.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_consumers, payer,
    moderate, biographical, constrained, continental).

% Inherit a stable climate system and post-growth institutions that do not demand perpetual growth to maintain solvency. They cannot voice preferences in the present constraint negotiation; their position is represented by advocates and inferred from the degrowth reading's own logic. They are locked into the consequences of present-day choices — exit is impossible.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Benefit from North-to-South resource redistribution (both current reparations and reduced future climate impacts) that the degrowth reading treats as structurally necessary. Their exit options are constrained by global climate physics and by asymmetric carbon-debt structures that the reading seeks to reverse. They have voice in climate negotiations but limited implementation power in Northern jurisdictions.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% Professional identity and career path are bound to high-consumption sectors (petrochemical, aviation, luxury goods, fast fashion manufacturing). The degrowth transition demands contraction of these sectors and retraining for lower-material-intensity work. Exit from the constraint would require either repudiating professional identity or attempting to leave the jurisdiction — both are high-friction. They also benefit from the post-growth institutional stability future generations inherit, but that benefit is diffuse and temporal.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, high_consumption_workers, payer,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, high_consumption_workers, beneficiary).

% Design and enforce the transition: carbon budgets, working-time reduction, redistribution mechanisms, and institutional redesign away from GDP growth dependence. This includes climate policy makers, central banks reorienting monetary frameworks, labor regulators, and international climate governance bodies. They set the terms of the constraint and bear the political cost of enforcement against organized resistance from consumption-dependent constituencies.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, post_growth_institutional_builders, agenda_setter,
    institutional, generational, analytical, continental).

% Would object to the degrowth reading's contraction logic and argue for technological mitigation, carbon capture, and managed adaptation instead. They are structurally excluded by the reading's own framing — the degrowth approach treats their business model as incompatible with climate stability. Their exit option is regulatory arbitrage (relocating to less stringent jurisdictions) or deep structural opposition to the reading itself.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, carbon_intensive_industries, excluded,
    powerful, biographical, mobile, global).

% Represent the sibling mitigation_priority_reading: they claim technological innovation and market mechanisms (renewable energy scaling, carbon capture and removal, efficiency gains) can decouple emissions from growth and deliver climate stability without the consumption sacrifice the degrowth reading demands. They are excluded from the present constraint negotiation by the reading's epistemic claim that unproven CDR cannot anchor climate strategy.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, technological_mitigation_advocates, excluded,
    institutional, biographical, analytical, global).

% Already experiencing climate impacts (sea-level rise, drought, extreme weather): flooding communities, agricultural collapse zones, climate-displaced populations. They observe the constraint debate from positions of acute vulnerability. Their voice carries moral weight in the climate reading contest, but implementation power concentrates in Northern institutional actors and global governance bodies.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_affected_communities, observer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, post_growth_institutional_builders).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the climate stabilization problem by reducing aggregate material throughput and redistributing resources to align incentives: reduced consumption in high-emission regions enables mitigation; redistribution to vulnerable populations enables adaptation; post-growth institutions eliminate the growth-or-collapse dynamic that drives extraction. The coordination problem is global carbon-budget scarcity and asymmetric historical liability.
% TRANSFER_FUNCTION: Transfers material consumption from present-day Global North populations to future generations and Global South populations. Moves working-time productivity gains from output expansion to leisure, care work, and local provisioning. Redirects carbon accounting from territorial (where a nation produces) to consumption-based (where goods are consumed) and implements reparations for unequal historical emissions. Redistributes institutional design from growth-dependent to steady-state frameworks.
% ABSENT_VOICES: Carbon-intensive industries and technological-mitigation advocates are structurally excluded from the degrowth reading's negotiation space. Communities locked in fossil-fuel-dependent infrastructure (coal-mining regions, oil-extraction economies, auto-manufacturing centers) have partial representation through labor unions but their preference for managed transition rather than degrowth is present but subordinated. Global South workers in export-manufacturing depend on Northern consumption and would experience transition disruption; their consent is inferred from climate-impact logic rather than directly solicited.
% DISAPPEARANCE_RATIONALE: If the degrowth reading and its enforcement infrastructure vanished overnight, Global North consumption would revert to expansion until carbon budgets become physically impossible to overshoot; redistribution mechanisms would collapse; post-growth institutions would be dismantled or subordinated to growth logic; climate adaptation in vulnerable regions would remain severely under-resourced; future climate stability would rest on unproven and slow-to-deploy technological mitigation, leaving intergenerational and inter-regional inequity embedded in the physical climate outcome. The world does not rearrange itself back — it bifurcates into climate havens and uninhabitable zones absent the structural constraint.
% FOUNDING_PROBLEM: 1970s onward: planetary boundaries are being exceeded by aggregate consumption in wealthy nations while climate impacts fall asymmetrically on populations with minimal historical responsibility. Post-1990s acceleration: IPCC science establishes that technological mitigation alone cannot achieve climate stability if demand growth continues; emissions must fall faster than historical decarbonization rates. 2015 onward: Paris climate economics and climate justice scholarship establish that the global carbon budget is too small for all populations to consume at Northern levels — redistribution and contraction are structurally necessary, not optional.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Assessment Reports (AR5, AR6) establish that 1.5°C mitigation requires global emissions cuts of 45% by 2030 — faster than any non-crisis reduction on record. Climate justice scholarship (Hickel, D'Alisa, Demaria, Kallis) and systems-analysis modeling (HANDY, Limits to Growth) from outside the degrowth-reading community document that growth-on-current-trajectories is incompatible with climate stability. UN Climate Change High-Level Champions and Global South climate negotiators have articulated redistribution as a precondition for their participation in climate agreements. The founding problem is live and corroborated by analysts with no inherent stake in the degrowth reading itself.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint imposes material costs on current Northern populations without contemporaneous compensation; the asymmetry is necessary from a climate-stabilization perspective but constitutes extraction from the payers' position. Suppression is elevated (0.62) because the constraint depends on: (1) carbon budgeting mechanisms that override market signals, (2) labor regulations that compress working time and output expectations, (3) consumption restrictions that conflict with established preferences, and (4) active exclusion of high-carbon industries and technological-mitigation rhetoric. Theater is low-to-moderate (0.28) because the constraint's functional core—material reduction and redistribution—is difficult to perform rhetorically; the measured ratio rises slightly at interval end as green-growth rhetoric and ESG frameworks create performative alternatives to genuine degrowth, suggesting potential for theater substitution over time. Accessibility_collapse is high (0.71) because once the degrowth reading's premises are understood (planetary boundaries binding, redistribution necessary, growth incompatible with stability), alternative exit routes (technological substitution, managed adaptation, growth-continuation) appear structurally infeasible rather than merely undesirable. Resistance is high (0.74) because the constraint encounters organized opposition from carbon-intensive sectors, growth-dependent constituencies, and advocates of alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   The seat-divergence is sharp and structural. From the post_growth_institutional_builders' seat (agenda-setter, institutional power, analytical perspective), the constraint is a genuine coordination mechanism solving the collective-action problem of planetary carbon-budget governance and enabling intergenerational stability. From the global_north_consumers' seat (payer, moderate power, constrained exit), the constraint appears as uncompensated sacrifice imposed from above, with benefits accruing to distant future actors and foreign populations who bear no responsibility for the transition. From the high_consumption_workers' seat (identity-locked exit), the constraint is experienced as forced occupational death and identity loss. From the global_south_populations' seat (beneficiary but with constrained exit and no implementation power in Northern institutions), the constraint's benefits are promised but not yet delivered, while they observe Northern actors deploying moral language about 'their' benefits. The engine will compute these as different type classifications: the institutional-seat reading may compute as rope (genuine coordination), the payer seats may compute as snare (extraction under coordination cover), and the global-south seat may compute as piton (promised benefits maintained theatrically while extraction of manufacturing labor continues). The claim/metric independence rule applies sharply here: the constraint is claimed as tangled_rope from the authoring seat's perspective; the engine's per-seat computations will reveal the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and global_south_populations are declared beneficiaries (d approaches 0.0 for benefit-collecting agents with no capacity to exit). Global_north_consumers and high_consumption_workers are declared victims (d approaches 1.0 for agents bearing costs with constrained exit). The directionality derivation flows from: (1) explicit beneficiary/victim declarations, (2) power atoms (global_north_consumers and workers are moderate power, not trapped-powerless; this reflects their capacity for organized resistance and political voice), (3) exit_options differentiation: global_north_consumers are constrained (cannot exit the constraint by leaving the jurisdiction—climate is global, consumption is enforced, redistribution is enforced); high_consumption_workers are identity-locked (they can geographically leave, but their professional identity is fused to high-carbon sectors, making exit psychologically costly and practically difficult). The identity_locked exit modulation raises the effective extraction experienced by this agent despite moderate power. Future_generations are trapped (no exit; commitment is binding across generations). Global_south_populations are constrained (climate physics is global, and the constraint's enforcement mechanisms are designed to prevent escape via manufacturing-export intensification). No directionality_overrides are needed; the declared structure produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (planetary boundaries exceeded, historical emissions inequality, inadequacy of technological mitigation alone) is live and corroborated by IPCC science and climate-justice scholarship. The disappearance_verdict is world_rearranges: if the degrowth constraint vanished, Northern consumption would revert to expansion until physical limits became impossible; redistribution mechanisms would collapse; future climate stability would rest on unproven technological substitutes; intergenerational and inter-regional equity would be embedded in a locked-in climate outcome. The constraint's mandate has NOT become mandatrophic (dead founding problem + persisting constraint); rather, the constraint is in active phase of enforcement buildup (measurements show extractiveness rising, theater_ratio declining, suggesting functional enforcement rather than theatrical maintenance). The constraint is classified as tangled_rope rather than piton precisely because the founding problem remains live and the extraction is structurally necessary to solve it, not residual institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_vs_technological_substitution,
    'Can technological mitigation (renewable energy deployment, carbon capture, efficiency gains) stabilize climate without the consumption contraction the degrowth reading requires?',
    'Engineering-scale deployment of carbon capture and renewable infrastructure in the 2030s–2050s will establish empirically whether emissions can decouple from material throughput. Rate of actual decarbonization relative to IPCC 1.5°C pathway will resolve the claim.',
    'If technological substitution proves sufficient, the degrowth reading collapses to a false urgency and the sibling mitigation_priority_reading forecloses it. If substitution proves insufficient or dangerously slow, the degrowth reading''s framing becomes structurally necessary and the mitigation reading''s core promise becomes indefensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_vs_technological_substitution, empirical, 'Sufficiency of technological decoupling to avoid consumption contraction').

omega_variable(
    intergenerational_consent_fiction,
    'Can future generations whose welfare anchors the degrowth reading be said to consent to or endorse the constraints imposed on present actors in their name?',
    'Not empirically resolvable within the constraint''s operative timeframe. Unresolved through the interval. Requires either explicit intergenerational covenant language in post-growth institutions or acceptance that representation is always contestable and future-focused reading rests on fiduciary theory, not consent.',
    'If intergenerational consent is required for legitimacy, the degrowth reading''s entire beneficiary structure becomes suspect — it imposes costs on present actors for benefits to voiceless third parties. If fiduciary obligation suffices without consent, the reading''s legitimacy rests on trust in institutional design and expert judgment about future welfare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_consent_fiction, conceptual, 'Whether voiceless future generations can be beneficiaries of present constraint without presuming consent').

omega_variable(
    global_south_compliance_under_transition,
    'Will Global South populations ratify the degrowth reading''s redistribution logic and accept reduced manufacturing-export opportunities during the Northern contraction, or will transition pressures drive them toward carbon-intensive development to escape vulnerability?',
    'International climate negotiations and trade agreements over the next decade will signal whether Global South actors accept the degrowth framing. Alternative: Global South countries initiate fossil-fuel investment and manufacturing expansion in response to Northern contraction and redistribution shortfalls.',
    'If Global South compliance fails, the redistribution flow reverses or is simply not forwarded; the constraint becomes unilateral Northern sacrifice without the offsetting benefit structure. The degrowth reading becomes tangled_rope from the Global North perspective but pure snare from the Global South perspective — not a unified constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_compliance_under_transition, empirical, 'Whether Global South populations consent to the degrowth reading''s beneficiary framing and redistribution promises').

omega_variable(
    structural_vs_rhetorical_degrowth,
    'Is the degrowth reading''s constraint genuinely about structural economic transformation (post-growth institutions, working-time reduction, consumption contraction), or primarily about moral reframing of growth-dependent economies (green growth rhetoric, ''sustainable development'', ESG capture)?',
    'Institutional analysis: do post-growth alternatives (commons-based resource management, care-work prioritization, energy descent) actually displace growth-dependent institutions, or do they remain aesthetic overlays on unchanged profit-maximization logic?',
    'If rhetorical only, the measured extractiveness and theater_ratio understate the constraint''s incoherence — actors face no real structural change, only narrative performance. If genuinely structural, the high suppression and resistance scores reflect real material conflict over who bears transition costs. Determines whether the constraint is a tangled_rope (real coordination + real extraction) or a snare (extraction masked by green rhetoric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_rhetorical_degrowth, empirical, 'Depth of institutional transformation: structural replacement of growth-dependence versus rhetorical green-growth repackaging').

omega_variable(
    kernel_reading_contest__degrowth_sibling_relations,
    'How does this degrowth reading relate logically and institutionally to its sibling readings (mitigation_priority_reading and adaptation_priority_reading)? Does one reading foreclose another, or do they coexist as live competing frameworks?',
    'Examine the core axiom differences and strategic implications: can an actor hold both degrowth AND mitigation-priority frames simultaneously, or do they require incompatible institutional commitments and empirical assumptions?',
    'If forecloses relation: the readings are rivals for institutional hegemony; only one can be the binding constraint. If coexists_with relation: multiple readings are simultaneously operative in different jurisdictions and constituencies, creating coordination problems across the climate regime. If influences relation: one reading shifts the resource/legitimacy conditions the others operate in without logically ruling them out.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest__degrowth_sibling_relations, conceptual, 'Logical and institutional relationship between degrowth and sibling readings of climate response').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__degrowth_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__degrowth_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(clim_tr_t15, projected).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__degrowth_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__degrowth_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__degrowth_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(clim_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__degrowth_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__degrowth_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(clim_be_t15, projected).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__degrowth_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__degrowth_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__degrowth_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__degrowth_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__degrowth_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(clim_su_t15, projected).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__degrowth_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__degrowth_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__degrowth_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(clim_su_t30, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=30
narrative_ontology:measurement(clim_grid_01, climate_response_imperative__degrowth_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(clim_grid_02, climate_response_imperative__degrowth_reading, accessibility_collapse(class), 30, 0.75).
narrative_ontology:measurement(clim_grid_03, climate_response_imperative__degrowth_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(clim_grid_04, climate_response_imperative__degrowth_reading, accessibility_collapse(individual), 30, 0.78).
narrative_ontology:measurement(clim_grid_05, climate_response_imperative__degrowth_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(clim_grid_06, climate_response_imperative__degrowth_reading, accessibility_collapse(organizational), 30, 0.82).
narrative_ontology:measurement(clim_grid_07, climate_response_imperative__degrowth_reading, accessibility_collapse(structural), 0, 0.65).
narrative_ontology:measurement(clim_grid_08, climate_response_imperative__degrowth_reading, accessibility_collapse(structural), 30, 0.7).
narrative_ontology:measurement(clim_grid_09, climate_response_imperative__degrowth_reading, resistance(class), 0, 0.78).
narrative_ontology:measurement(clim_grid_10, climate_response_imperative__degrowth_reading, resistance(class), 30, 0.74).
narrative_ontology:measurement(clim_grid_11, climate_response_imperative__degrowth_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(clim_grid_12, climate_response_imperative__degrowth_reading, resistance(individual), 30, 0.72).
narrative_ontology:measurement(clim_grid_13, climate_response_imperative__degrowth_reading, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(clim_grid_14, climate_response_imperative__degrowth_reading, resistance(organizational), 30, 0.76).
narrative_ontology:measurement(clim_grid_15, climate_response_imperative__degrowth_reading, resistance(structural), 0, 0.65).
narrative_ontology:measurement(clim_grid_16, climate_response_imperative__degrowth_reading, resistance(structural), 30, 0.7).
narrative_ontology:measurement(clim_grid_17, climate_response_imperative__degrowth_reading, stakes_inflation(class), 0, 0.7).
narrative_ontology:measurement(clim_grid_18, climate_response_imperative__degrowth_reading, stakes_inflation(class), 30, 0.76).
narrative_ontology:measurement(clim_grid_19, climate_response_imperative__degrowth_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(clim_grid_20, climate_response_imperative__degrowth_reading, stakes_inflation(individual), 30, 0.74).
narrative_ontology:measurement(clim_grid_21, climate_response_imperative__degrowth_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(clim_grid_22, climate_response_imperative__degrowth_reading, stakes_inflation(organizational), 30, 0.78).
narrative_ontology:measurement(clim_grid_23, climate_response_imperative__degrowth_reading, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(clim_grid_24, climate_response_imperative__degrowth_reading, stakes_inflation(structural), 30, 0.72).
narrative_ontology:measurement(clim_grid_25, climate_response_imperative__degrowth_reading, suppression(class), 0, 0.55).
narrative_ontology:measurement(clim_grid_26, climate_response_imperative__degrowth_reading, suppression(class), 30, 0.62).
narrative_ontology:measurement(clim_grid_27, climate_response_imperative__degrowth_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(clim_grid_28, climate_response_imperative__degrowth_reading, suppression(individual), 30, 0.68).
narrative_ontology:measurement(clim_grid_29, climate_response_imperative__degrowth_reading, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(clim_grid_30, climate_response_imperative__degrowth_reading, suppression(organizational), 30, 0.64).
narrative_ontology:measurement(clim_grid_31, climate_response_imperative__degrowth_reading, suppression(structural), 0, 0.45).
narrative_ontology:measurement(clim_grid_32, climate_response_imperative__degrowth_reading, suppression(structural), 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, planetary_boundaries_doctrine__hard_constraint).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, carbon_debt_reparations__north_to_south).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, working_time_reduction__labor_restructuring).

% DUAL FORMULATION NOTE:
% This constraint is one reading (degrowth_reading) of the climate_response_imperative kernel. The sibling readings (mitigation_priority_reading and adaptation_priority_reading) instantiate the same kernel through different strategic framings. They are separate constraint stories with distinct beneficiary structures, ε values, and classifications. Decomposition is necessary because the ε referent is the same (climate response arrangements in the standing global economy), but the readings prescribe incompatible institutional arrangements and have fundamentally different victim sets. A unified story would fabricate neutrality across three structurally distinct constraint architectures. See the respective sibling stories for comparative analysis of ε, suppression, and seat divergence across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
