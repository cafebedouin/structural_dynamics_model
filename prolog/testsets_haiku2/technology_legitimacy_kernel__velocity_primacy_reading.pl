% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Technology Legitimacy via Velocity-Primacy: Deployment Timeline as Arbiter
 *   domain: energy_policy/climate_mitigation
 *
 * SUMMARY:
 *   The velocity-primacy reading legitimizes climate mitigation technologies
 *   solely on their ability to scale within 2030/2050 carbon budget
 *   timelines. This reading operationalizes the urgency framing: faster
 *   deployment = more legitimate for decarbonization. The constraint benefits
 *   rapid-deployment renewable vendors and climate urgency advocates (who
 *   gain policy authority and capital flow), while imposing grid integration
 *   costs on grid operators and delegitimizing longer-lead-time technologies
 *   (nuclear, advanced geothermal, carbon capture infrastructure). The
 *   constraint's core extraction mechanism is the transfer of technology
 *   legitimacy authority from multi-metric technical assessment (reliability,
 *   lifecycle impact, grid integration feasibility) to a single metric
 *   (deployment velocity). Grid operators and nuclear stakeholders bear costs
 *   (integration burden, investment/development freeze) while the criterion's
 *   beneficiaries capture legitimacy rents. This is a tangled rope: it
 *   coordinates technology strategy around a temporal bottleneck AND extracts
 *   through metric reduction and cost-shifting.
 *
 * KEY AGENTS:
 *   - rapid_deployment_renewable_vendors: institutional actor benefiting from fast-deployment criterion (solar/wind manufacturers)
 *   - climate_urgency_advocates: organized coalition (IPCC, green NGOs) enforcing velocity as binding
 *   - grid_operators: institutional actors bearing integration cost, structurally constrained from exit
 *   - nuclear_industry: powerful but constrained, excluded from legitimacy definition
 *   - carbon_budget_authority: agenda-setter (UN/IPCC climate authorities) defining timelines and enforcement
 *   - lifecycle_impact_researchers: excluded from legitimacy conversation despite technical relevance
 *   - developing_nation_grids: powerless, trapped between rapid deployment mandate and infrastructure capacity constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.72).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Technology Legitimacy via Velocity-Primacy: Deployment Timeline as Arbiter").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'febed296-892f-4263-bd2b-ba20b6c9c73f').
narrative_ontology:cs_kernel_codification('febed296-892f-4263-bd2b-ba20b6c9c73f', fixed_text).
narrative_ontology:cs_authority_grounding('febed296-892f-4263-bd2b-ba20b6c9c73f', extraction).
narrative_ontology:cs_interpretation_layer_present('febed296-892f-4263-bd2b-ba20b6c9c73f').
narrative_ontology:cs_reading_relation('febed296-892f-4263-bd2b-ba20b6c9c73f', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('febed296-892f-4263-bd2b-ba20b6c9c73f', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('febed296-892f-4263-bd2b-ba20b6c9c73f', foundational, deployment_velocity_primary_legitimacy_criterion).
narrative_ontology:cs_axiom_status(deployment_velocity_primary_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('febed296-892f-4263-bd2b-ba20b6c9c73f', deployment_velocity_primary_legitimacy_criterion, empirically_contingent).
narrative_ontology:cs_axiom('febed296-892f-4263-bd2b-ba20b6c9c73f', foundational, carbon_budget_timeline_binding_constraint).
narrative_ontology:cs_axiom_status(carbon_budget_timeline_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('febed296-892f-4263-bd2b-ba20b6c9c73f', carbon_budget_timeline_binding_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('febed296-892f-4263-bd2b-ba20b6c9c73f', climate_urgency_decarbonization_primacy).
narrative_ontology:cs_drift_state('febed296-892f-4263-bd2b-ba20b6c9c73f', grid_integration_saturation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('febed296-892f-4263-bd2b-ba20b6c9c73f', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, rapid_deployment_renewable_vendors).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_urgency_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_managing_intermittency).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_industry_stakeholders).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, long_lead_time_technology_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, developing_nation_grid_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar and wind manufacturers, project developers, and installation companies benefit directly from velocity-primacy legitimacy. Their technologies (2-5 year deployment cycles) pass the criterion by construction. Capital investment, regulatory fast-tracking, and long-term contracts flow to them. They control the narrative that urgency demands rapid scaling. They collect rents on supply growth and market certainty without bearing the downstream cost of grid integration.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, rapid_deployment_renewable_vendors, beneficiary,
    institutional, biographical, arbitrage, global).

% Environmental organizations, climate scientists in the net-zero-by-2050 coalition, and progressive policy advocates use velocity-primacy to operationalize their framing that climate change is an existential emergency requiring maximum speed. The criterion vindicates their narrative authority. They gain policy influence when urgent timelines become binding, and they lose influence if reliability or precautionary framings take precedence. Their exit is constrained by ideological commitment to urgency primacy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_urgency_advocates, beneficiary,
    organized, generational, constrained, global).

% Independent system operators and utility grid planners bear the operational and capital cost of integrating high-penetration variable renewable generation. They invest in frequency support, grid-scale battery storage, demand response infrastructure, advanced forecasting, and rotating reserves. The velocity criterion does not measure these costs; they are externalized as 'grid operator responsibility.' They have no formal seat at the technology-legitimacy table. They cannot refuse to integrate admitted technologies. Their exit is structurally impossible: they must operate a reliable grid while integrating whatever fast technologies the velocity criterion admits.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_managing_intermittency, payer,
    institutional, biographical, constrained, regional).

% Nuclear reactor manufacturers, utilities operating nuclear plants, and developers of small modular reactors are structurally excluded from the legitimacy category by velocity-primacy. Even if nuclear provides dispatchable low-carbon generation, its 10-15 year development and licensing timelines (per large reactors) or 7-10 years (per SMRs) fail the 2030/2050 carbon budget criterion. Investment capital migrates to renewables. Existing reactors are treated as legacy carbon (not new decarbonization). Development programs face funding collapse. Their only exit is to lobby for a different legitimacy criterion (reliability primacy) or to prove acceleration is possible, both constrained by technology and regulatory realities.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_industry_stakeholders, payer,
    powerful, generational, constrained, national).

% Researchers and companies developing advanced geothermal, fusion energy, next-generation carbon capture, and long-duration energy storage face delegitimization in real time. Their technologies have intrinsic development timelines they cannot arbitrarily shorten without compromising feasibility or safety. Capital dries up. Regulatory support shifts to fast renewables. They face a choice between abandoning their work, accelerating under constraints they did not design, or reframing around a different criterion that values long-term impact over deployment speed.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, long_lead_time_technology_developers, payer,
    moderate, generational, constrained, global).

% The IPCC climate science working groups, the UN climate process (UNFCCC), and national climate ministries set the carbon budget (remaining emissions permitted under 1.5°C scenarios) and enforce the velocity criterion as its primary operationalization. They define 'remaining timeline' (2030/2050 targets), 'scale' (deployment capacity), and 'feasibility' (whether a technology can meet deadlines). Their authority persists as long as the carbon budget frame remains credible and velocity remains the primary policy mechanism.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_authority, agenda_setter,
    institutional, generational, analytical, global).

% Grid engineers, utility operators, and reliability-focused policymakers who argue that grid stability is a prerequisite for any technology deployment are structurally excluded from the technology-legitimacy conversation. They would prioritize dispatchability, frequency support, and integration feasibility over deployment velocity. Their exclusion is not coercive but definitional: the velocity criterion pre-empts the reliability criterion at the kernel level. Their arguments are classified as 'operations detail' rather than 'legitimacy question.'
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_reliability_advocates, excluded,
    organized, biographical, trapped, regional).

% Scientists studying full-cycle carbon (extraction, manufacturing, transportation, installation, decommissioning), material requirements, land use, water use, and long-term waste of different technologies are sidelined when velocity becomes the primary legitimacy metric. Their research on lifecycle emissions, embodied carbon, and material constraints is reframed as 'implementation detail' rather than 'carbon legitimacy.' Their exclusion is institutional: the criterion pre-empts detailed assessment at the definitional layer.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, lifecycle_impact_researchers, excluded,
    moderate, generational, constrained, global).

% Countries building electrification and grid infrastructure for the first time face a double mandate: deploy fast to meet global carbon budget targets AND maintain reliability to serve their populations. They lack the capital, technical expertise, and infrastructure (balancing reserves, interconnection, frequency support, storage) that developed nations use to manage high-penetration renewables. They inherit both the velocity mandate and the grid stability liability, but with powerless agency in either. They cannot exit: they face pressure from above (global decarbonization targets) and pressure from below (population demand for reliable electricity). They pay twice: once for rapid renewable deployment and once for grid infrastructure to manage it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, developing_nation_grid_operators, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, developing_nation_grid_operators, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, rapid_deployment_renewable_vendors).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operationalizes the global carbon budget constraint (remaining emissions permitted to limit warming to 1.5–2°C) as a binding deployment timeline. Solves the coordination problem: how to align technology investment and policy around a shared temporal bottleneck. Without the velocity criterion, governments and investors would lack a common metric for prioritizing between competing climate mitigation pathways.
% TRANSFER_FUNCTION: Transfers legitimacy authority and investment capital from technologies with longer development timelines to those with faster deployment timelines, regardless of their dispatchability, lifecycle impact, or grid integration requirements. Transfers grid integration cost from renewable technology developers to grid operators and electricity ratepayers. Transfers authority over technology legitimacy from technical performance metrics (reliability, efficiency, lifecycle carbon) to the single metric of deployment velocity.
% ABSENT_VOICES: Grid reliability engineers, lifecycle carbon researchers, nuclear industry representatives, and developing-nation grid operators would contest the criterion if seated at the legitimacy-definition table. They argue for multi-metric technology assessment (reliability, lifecycle legality, grid integration feasibility) rather than velocity-only framing. They are excluded because the criterion pre-empts the conversation at the kernel level — it defines what counts as legitimate evidence before technical assessment happens.
% DISAPPEARANCE_RATIONALE: If the velocity-primacy criterion vanished and were replaced by multi-metric legitimacy assessment, investment capital and regulatory priority would redistribute across the technology portfolio. Nuclear and advanced geothermal projects would regain investment and development momentum. Grid operators would gain authority in technology-integration decisions. Renewable deployment would slow where grid integration was not yet solved, but the overall carbon mitigation pathway would incorporate dispatchable and reliable technologies earlier. The global technology strategy would restructure around a different optimization surface.
% FOUNDING_PROBLEM: The remaining carbon budget (per IPCC 1.5°C scenarios) exhausts in 20–30 years. Decarbonization requires massive capacity deployment within a tight timeline. Earlier analyses suggested any technology that reduced emissions mattered; the founding problem became: how to operationalize 'enough speed' to stay within budget?
% FOUNDING_PROBLEM_CORROBORATION: The IPCC and climate urgency advocates (outside the beneficiary set) attest the founding problem is live and velocity is the binding constraint. Grid operators and nuclear engineers (outside the beneficiary set) attest the founding problem is real but velocity is a misspecified solution — the constraint is actually 'dispatchable decarbonization at pace,' not 'any fast deployment.' Independent energy modeling (Bloomberg NEF, IEA net-zero pathways) shows successful mitigation pathways include both fast renewables AND longer-lead nuclear/geothermal, suggesting the single-metric framing simplifies beyond reality.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68) because the constraint transfers investment capital and regulatory authority via a single metric, creating winners (fast renewables) and losers (long-timeline technologies). It is not maximally extractive because the underlying carbon budget problem is real and the velocity constraint addresses a genuine temporal constraint; the extraction lies in how the constraint is specified (velocity only, not velocity+reliability+integration feasibility). Suppression is high (0.72) because the constraint operates through definitional pre-emption: technologies that cannot meet the velocity criterion are delegitimized before technical assessment happens, and there is no formal process for re-litigating what counts as 'legitimate technology.' Grid operators and nuclear stakeholders face suppression through regulatory mechanism, not direct coercion, but the effect is the same (their objections are ruled out-of-order at the criterion level). Theater ratio is moderate (0.42): the velocity criterion was born from real carbon budget analysis, so the justification is genuine; but as the constraint persists and integration problems accumulate, an increasing share of enforcement activity focuses on defending the criterion's binding status rather than assessing whether deployment velocity actually optimizes carbon outcomes. The measurements show extractiveness plateauing around year 20 (the 2040 midpoint of the 2020-2050 interval), suggesting the constraint's primary extraction happens early (technology choice lock-in) and then stabilizes as capital and policy gravitate to renewable energy regardless of criterion renewal. Suppression requirement also plateaus, indicating grid operators adapt to managing high-penetration renewables and stop formally challenging the velocity criterion.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (renewable vendors, climate advocates), the velocity criterion is rational coordination: urgency is real, faster deployment saves carbon, capital should flow to speed. From the grid operator seat, the same constraint is extractive: they inherit both a mandate to integrate fast renewables AND liability for grid stability, but no authority to shape the technology portfolio around reliability needs. From the nuclear stakeholder seat, the constraint is definitional exclusion masquerading as technical assessment. The engine should compute these as different types: beneficiary seats may compute rope (coordination with side payments), grid operator seats as snare (extraction through regulatory mechanism), nuclear seats as piton (atrophied by delegitimation, persisting through institutional inertia). The claimed type is tangled_rope because the constraint's structure contains both genuine coordination (meeting carbon budget via rapid deployment) and asymmetric extraction (velocity-only legitimacy, cost-shifting to grid operators, exclusion of long-timeline technologies).
 *
 * DIRECTIONALITY LOGIC:
 *   Rapid-deployment renewable vendors: d approaches 0.1 (full beneficiaries — they collect legitimacy rents, face low suppression, have exit via market growth). Climate urgency advocates: d approaches 0.15 (beneficiaries — they gain policy authority, face minimal suppression, exit via narrative frame persistence). Grid operators: d approaches 0.85 (targets — they pay in integration cost, face suppression through definitional pre-emption, constrained exit). Nuclear stakeholders: d approaches 0.9 (strong targets — they pay through development freeze, are structurally excluded, face suppression at the definitional layer). The directionality divergence is extreme: the same constraint produces d ∈ [0.1, 0.15] for beneficiaries and d ∈ [0.85, 0.9] for targets. No overrides needed; the structural derivation captures this correctly from beneficiary/victim declarations and exit analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (remaining carbon budget exhausts in ~25 years, rapid decarbonization needed) remains live and credible. The velocity criterion operationalizes this legitimately. Mandatrophy does not apply: the constraint's persistence tracks the binding status of the carbon budget itself. However, there is a secondary degradation risk: as grid integration costs mount and renewable deployment accelerates, the criterion's framing may shift from 'meet carbon budget by deploying fast' to 'any renewable deployment is good because we said so.' If this occurs, the constraint decays from tangled_rope (real coordination + extraction) toward piton (performance maintained for legitimacy theater, not because velocity still optimizes outcomes). The theater_ratio plateau at 0.42 suggests early-stage theater accumulation but not yet piton-level (piton theaters run 0.6+). Monitor whether future measurements show theater rising while extractiveness plateaus — that would signal mandatrophy onset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    velocity_vs_integration_tradeoff,
    'Can rapid renewable deployment scale without proportional grid integration investment, or does velocity-only legitimacy hide a cost-shifting mechanism?',
    'Grid modeling and load-flow analysis comparing actual vs. modeled integration costs under different technology portfolios. Post-deployment grid reliability and stability metrics (frequency nadir, ramp rates, forced outages).',
    'If integration costs are controllable with deployed technologies, the constraint is genuine coordination (faster deployment needed, grid can handle it). If integration costs scale unpredictably with renewable penetration, the constraint becomes extractive cost-shifting: velocity beneficiaries collect legitimacy without bearing integration burden. This would suggest reclassification from tangled_rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(velocity_vs_integration_tradeoff, empirical, 'Whether velocity-primacy legitimacy hides unmodeled grid integration costs').

omega_variable(
    carbon_budget_specification_ambiguity,
    'Is the remaining carbon budget (per IPCC 1.5°C) a real physical constraint or a policy normalization that could be revised as climate response deepens?',
    'Climate physics (can the 1.5°C target be refined/redefined with new climate data?) and political economy (have carbon budget targets been revised before, and under what conditions?).',
    'If the budget is a normative target rather than a physical floor, the velocity criterion''s binding force rests on political consensus, not carbon physics. This would suggest the constraint is a collective commitment device (rope, or tangled_rope with weaker extraction). If the budget is a physical constraint that degrades as delay accumulates, velocity becomes unavoidably binding (mountain-like naturalness). This affects whether the criterion''s persistence is coordinate or coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_budget_specification_ambiguity, conceptual, 'Whether the carbon budget is a physical necessity or a policy target').

omega_variable(
    technology_capability_vs_deployment_timeline,
    'Do nuclear and advanced geothermal technologies actually require the stated 10-15 year timelines, or are shorter timelines technically feasible but suppressed by regulatory/licensing constraints?',
    'Comparative case analysis of jurisdictions with streamlined licensing (e.g., China''s nuclear fleet construction rates) vs. those with extended regulatory review. Engineering analysis of modular reactor and direct geothermal timelines vs. conventional deployment.',
    'If longer timelines are engineering-intrinsic, the velocity criterion correctly excludes slow technologies. If timelines are policy-modifiable, the criterion is enforcing a regulatory choice (not deploy nuclear) by disguising it as a technical bottleneck. This would strengthen the snare classification (definitional exclusion masquerading as constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_capability_vs_deployment_timeline, empirical, 'Whether longer technology timelines are intrinsic or policy-contingent').

omega_variable(
    reading_committer_ambiguity,
    'Does the velocity-primacy reading''s framing originate from genuine climate urgency analysis, or does it systematize ex-post facto the preferences of actors already invested in fast-deployment technologies?',
    'Historical document analysis of climate modeling evolution (when was velocity made primary vs. other metrics?). Funding source tracing for IPCC working groups and climate policy organizations.',
    'If velocity-primacy emerged from independent climate analysis, it reflects real constraint (carbon budget + deployment dynamics). If it co-evolved with renewable industry growth, the reading may be a post-hoc justification for technology choices already made for other reasons (cost competitiveness, supply chain momentum). This affects whether the constraint''s legitimacy authority is epistemic or captured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Whether velocity-primacy originates from independent climate analysis or reflects pre-existing technology preferences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement_basis(tech_tr_t4, observed).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(tech_tr_t8, observed).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(tech_tr_t12, observed).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(tech_tr_t16, observed).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(tech_tr_t20, observed).
narrative_ontology:measurement(tech_tr_t28, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement_basis(tech_tr_t28, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 4, 0.59).
narrative_ontology:measurement_basis(tech_be_t4, observed).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement_basis(tech_be_t8, observed).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(tech_be_t12, observed).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(tech_be_t16, observed).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(tech_be_t20, observed).
narrative_ontology:measurement(tech_be_t28, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 28, 0.68).
narrative_ontology:measurement_basis(tech_be_t28, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement_basis(tech_su_t4, observed).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement_basis(tech_su_t8, observed).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(tech_su_t12, observed).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(tech_su_t16, observed).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(tech_su_t20, observed).
narrative_ontology:measurement(tech_su_t28, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 28, 0.72).
narrative_ontology:measurement_basis(tech_su_t28, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__velocity_primacy_reading, 0.18).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__precautionary_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, grid_integration_cost_distribution).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_implementation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_legitimacy_kernel. All three readings (velocity-primacy, reliability-primacy, precautionary) are linked via network.affects_constraints and represent decomposition per ε-invariance: each reading has a different ε, different beneficiary/victim structure, and different persistence mechanism. The velocity-primacy reading (this file) examines the constraint as operationalized through carbon budget timelines; the reliability-primacy reading examines the same kernel (technology legitimacy) operationalized through dispatchability and grid stability; the precautionary reading operationalizes it through failure-mode reversibility. The readings coexist as live policy positions and influence each other's feasibility and authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__velocity_primacy_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
