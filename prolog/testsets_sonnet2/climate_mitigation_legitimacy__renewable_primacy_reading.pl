% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable-Primacy Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint is the renewable-primacy reading of a contested kernel:
 *   what counts as legitimate decarbonization policy. This reading holds that
 *   renewables-plus-storage can achieve full decarbonization faster and
 *   cheaper than nuclear, and it operates through cost-curve modeling (LCOE
 *   comparisons), subsidy design, and permitting prioritization to route
 *   capital, workforce attention, and political legitimacy toward
 *   distributed, short-capital-cycle generation and away from
 *   long-capital-cycle nuclear programs. The reading has a genuine
 *   coordination function — it lets policymakers move quickly on falling-cost
 *   technology rather than betting decades of capital on projects with
 *   chronic overrun risk — but it also structurally disadvantages nuclear
 *   operators and the industrial users who depend on firm baseload, recasting
 *   nuclear capital as a 'sink' that delays the 'real' transition. This is
 *   ONE of four sibling readings of the same kernel
 *   (baseload_necessity_reading, degrowth_sufficiency_reading,
 *   portfolio_pragmatism_reading); each is authored as its own constraint
 *   with its own epsilon, beneficiaries, and victims — this file does not
 *   average across them or hedge its extraction estimate to accommodate the
 *   others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.42).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable-Primacy Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '14d962fe-7c4b-4be5-8d42-e518bcf0aa5e').
narrative_ontology:cs_kernel_codification('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', distributed).
narrative_ontology:cs_authority_grounding('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', distributed).
narrative_ontology:cs_reading_relation('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', foundational, cost_curve_trajectory_determines_optimal_pathway).
narrative_ontology:cs_axiom_status(cost_curve_trajectory_determines_optimal_pathway, holdable).
narrative_ontology:cs_axiom_grounding('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', cost_curve_trajectory_determines_optimal_pathway, empirically_contingent).
narrative_ontology:cs_axiom('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', secondary, modular_short_cycle_capital_preferred_over_long_horizon_commitment).
narrative_ontology:cs_axiom_status(modular_short_cycle_capital_preferred_over_long_horizon_commitment, holdable).
narrative_ontology:cs_axiom_grounding('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', modular_short_cycle_capital_preferred_over_long_horizon_commitment, instrumental).
narrative_ontology:cs_reference_frame('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', declining_cost_curve_technology_primacy).
narrative_ontology:cs_drift_state('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', post_2020s_grid_reliability_scrutiny, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('14d962fe-7c4b-4be5-8d42-e518bcf0aa5e', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, utility_scale_solar_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_project_financiers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, grid_scale_software_platforms).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_construction_workforce).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_dependent_industrial_users).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capture subsidy streams, favorable interconnection queues, and procurement mandates that this reading's legitimacy claim helps secure in policy debates. Can redeploy capital across jurisdictions if one market's incentives fade, so their exposure to any single policy fight is limited.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, utility_scale_solar_developers, beneficiary,
    organized, biographical, mobile, national).

% Benefit directly from the claim that storage substitutes for baseload; every megawatt of storage mandated in place of dispatchable generation is direct revenue. Operate across multiple national markets and are not tied to any one country's grid architecture.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Underwrite the short capital-cycle renewable buildout this reading privileges; also sit on advisory panels and rating frameworks that classify nuclear projects as high-risk relative to renewables, shaping which projects get financed at all.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_project_financiers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_project_financiers, agenda_setter).

% Sell forecasting, demand-response, and virtual power plant software premised on a distributed, variable-generation grid; their market only exists if the renewable-primacy account of decarbonization wins the policy argument.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_scale_software_platforms, beneficiary,
    organized, biographical, mobile, national).

% Operate plants with multi-decade capital horizons and sunk construction costs; under this reading's framing, nuclear is recast as a capital sink that delays the 'real' transition, which erodes political support for life-extension, cost-recovery, and new-build financing. Cannot redeploy a nuclear plant's capital elsewhere once built.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_operators, payer,
    powerful, civilizational, trapped, national).

% Skilled trades and engineers whose employment depends on continued nuclear construction and refurbishment programs. When this reading dominates procurement and permitting priorities, project pipelines shrink and specialized skills atrophy or require costly retraining.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_construction_workforce, payer,
    moderate, biographical, constrained, regional).

% Heavy industry (steel, chemicals, aluminum smelting) requiring firm round-the-clock power. When grid planning is reoriented around variable renewables plus storage rather than dispatchable baseload, they bear higher costs or curtailment risk and cannot easily relocate operations.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_dependent_industrial_users, payer,
    moderate, biographical, constrained, regional).

% System operators responsible for frequency stability and resource adequacy raise technical objections about inertia, seasonal storage gaps, and firm capacity margins, but their engineering caveats are frequently treated as secondary to the cost-curve narrative in public and legislative debate.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_planners, excluded,
    institutional, generational, analytical, national).

% Climate advocacy organizations, think tanks, and policy coalitions that actively promote the LCOE-based case for renewables-plus-storage over nuclear in legislative testimony, model assumptions, and public messaging, shaping which technologies receive mandates and subsidies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_primacy_advocates, agenda_setter,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital, permitting priority, and grid planning around the fastest-deployable, most modular decarbonization technologies, allowing rapid iteration and falling costs through manufacturing scale rather than large, slow, one-off construction projects.
% TRANSFER_FUNCTION: Moves policy attention, subsidy allocation, interconnection priority, and public legitimacy away from nuclear capital programs and toward renewable-plus-storage capital programs, shifting employment and capital-recovery risk from one industrial constituency to another.
% ABSENT_VOICES: Long-duration seasonal storage researchers and grid engineers focused on multi-day and multi-week reliability gaps are rarely centered in the cost-curve debate; nuclear waste management specialists and communities near existing nuclear sites who would bear stranded-asset costs are also largely outside the room when this reading's legitimacy claim is adjudicated in legislatures.
% DISAPPEARANCE_RATIONALE: If this reading's legitimacy claim lost its grip on policy discourse, nuclear life-extension and new-build financing would become politically easier, renewable subsidy design would face more technology-neutral competition, and grid planning bodies would weight firm capacity requirements more heavily against cost-per-megawatt-hour comparisons — procurement rules, subsidy allocation, and permitting queues would all shift.
% FOUNDING_PROBLEM: Early 2000s-2010s climate policy needed a persuasive, politically viable argument for why decarbonization investment should flow toward renewables rather than stalling on decades-long nuclear construction timelines and cost overruns, at a moment when solar and wind costs were falling rapidly and storage was becoming commercially viable.
% FOUNDING_PROBLEM_CORROBORATION: Independent grid reliability studies (e.g. system operator resource adequacy reports) and IPCC scenario modeling from outside the renewable industry attest that the underlying decarbonization urgency remains live, but dispute whether renewables-plus-storage alone resolves it at the pace and reliability required, versus requiring a technology-neutral portfolio; grid engineers outside both the renewable and nuclear industries corroborate that firm capacity and seasonal storage gaps remain empirically unresolved.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that the coordination function (fast, cost-effective capital deployment) is real but travels alongside a genuine transfer: nuclear operators and dependent workforces bear delayed financing, stranded-asset risk, and skill atrophy that flow directly from this reading's success in policy debates. Suppression (0.42) is moderate rather than severe — grid reliability planners and portfolio advocates can and do contest the reading in technical and legislative venues, so alternatives are not fully foreclosed, only structurally disadvantaged in funding and permitting priority. Theater ratio (0.28) is modest: most of the activity (subsidy allocation, interconnection studies, storage deployment) is functionally real, though a growing share of public advocacy work performs urgency rather than resolving the seasonal-storage and firm-capacity questions grid engineers raise. Accessibility collapse (0.35) is low-moderate because the portfolio-pragmatism and baseload-necessity alternatives remain live and funded in many jurisdictions; this reading has not achieved anything like a natural-law monopoly on the decarbonization debate. Resistance (0.55) is substantial, coming from nuclear operators, industrial baseload users, and system reliability engineers who actively contest the cost-curve framing on technical grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers, storage manufacturers, project financiers, and grid-software platforms are declared beneficiaries because the reading's legitimacy claim directly shapes which projects get subsidized, financed, and prioritized in interconnection queues — they collect from the reading's success in policy discourse. Nuclear operators and their construction workforce are declared victims because the same reading recasts their multi-decade capital commitments as an obstacle to be routed around, eroding political and financial support for life-extension and new-build programs; their exit options are trapped or constrained because sunk nuclear capital and specialized labor cannot be redeployed quickly. Grid reliability planners are excluded rather than victimized outright — their technical objections are structurally present but underweighted relative to the cost-curve narrative, which is why they carry the 'excluded' role and feed the absent_voices answer rather than being named a direct payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — persuading policymakers to back rapidly-falling-cost technology over historically overrun-prone nuclear construction — remains partially live (renewable costs continue falling, nuclear construction still overruns in many markets), which is why founding_problem_status is authored as contested rather than dead. This prevents mislabeling the reading as pure legitimacy-theater: it retains a real coordination function (faster capital deployment, modular scaling) even as its persistence increasingly serves the concentrated interests of renewable capital and software platforms whose business models depend on the framing winning. The tangled_rope classification captures exactly this: coordination (real, falling-cost technology deployment) plus asymmetric extraction (nuclear capital and workforce bear the downside) sustained by active enforcement (subsidy design, permitting priority, rating-agency risk classifications).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_primacy_reading_identity,
    'Is the renewable-primacy account of decarbonization legitimacy a genuinely superior engineering and cost pathway, or a reading whose persuasive force derives disproportionately from the short capital cycles and rapid political feedback loops that favor its own beneficiaries over nuclear''s multi-decade horizon?',
    'Independent (non-industry-funded) whole-system cost studies that price in firm capacity, seasonal storage, and grid stability requirements at full decarbonization scale, compared across jurisdictions that pursued renewable-primacy versus portfolio-pragmatist strategies over a full multi-decade buildout.',
    'If whole-system studies vindicate the renewable-primacy cost advantage even under full reliability constraints, the reading''s extraction on nuclear stakeholders reflects an efficient reallocation rather than a captured legitimacy claim. If whole-system costs are comparable or favor a portfolio approach once reliability is fully priced, the reading''s dominance in policy discourse looks more like beneficiary capture of the legitimacy narrative than an efficient outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_primacy_reading_identity, empirical, 'Whether renewable primacy is a genuine engineering/cost verdict or a captured framing favoring its own short-capital-cycle beneficiaries.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the four kernel readings (renewable_primacy, baseload_necessity, portfolio_pragmatism, degrowth_sufficiency) disagree — is it primarily an empirical dispute over cost curves and grid engineering, or a values dispute over risk tolerance, centralization, and intergenerational capital commitment?',
    'Structured elicitation of each reading''s core axioms (as declared in cs_structure.axioms across the four sibling files) to identify whether disagreements are resolvable by better data (empirically_contingent axioms) or are irreducibly normative (deontological/conventional axioms about acceptable risk and centralization).',
    'If the disagreement is primarily empirical, convergence on a shared framework is possible as cost and reliability data accumulate. If primarily normative, the readings will coexist indefinitely as competing legitimate positions, and policy will oscillate with political coalitions rather than converge on a single ''correct'' technology mix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the renewable-primacy vs sibling-reading disagreement is empirically resolvable or normatively irreducible.').

omega_variable(
    nuclear_capital_sink_framing_accuracy,
    'Is characterizing nuclear construction as a ''capital sink delaying renewable deployment'' an accurate structural description, or does it understate nuclear''s marginal contribution to decarbonization speed in grids where renewable interconnection queues are themselves the binding constraint?',
    'Comparative analysis of interconnection queue delays versus nuclear construction delays in specific grid regions, to determine which factor is actually rate-limiting for decarbonization pace in practice.',
    'If interconnection queues (not nuclear construction) are the binding constraint in most grids, the victim framing of nuclear as a delaying capital sink is overstated relative to the reading''s own logic, weakening the tangled_rope classification''s extraction claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_capital_sink_framing_accuracy, empirical, 'Whether nuclear construction is actually the rate-limiting factor this reading claims it is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__renewable_primacy_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the climate_mitigation_legitimacy kernel. Each reading is authored as an independent constraint story with its own epsilon, beneficiary/victim structure, and claimed type, per the epsilon-invariance principle. This file (renewable_primacy_reading) authors nuclear operators and dependent workforce as victims and renewable/storage capital as beneficiaries; the baseload_necessity_reading sibling is expected to invert much of this structure (renewables as unreliable/subsidy-dependent, nuclear as the coordination backbone); portfolio_pragmatism_reading is expected to show lower extraction on either technology camp since it declines to privilege one path; degrowth_sufficiency_reading is expected to reframe the entire generation-expansion question as the wrong problem. All four should be linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__renewable_primacy_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
