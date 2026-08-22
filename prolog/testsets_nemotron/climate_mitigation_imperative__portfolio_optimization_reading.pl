% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Portfolio Optimization Reading of Climate Mitigation: Nuclear as Necessary Baslowd
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This reading frames climate mitigation as a portfolio optimization
 *   problem: minimize cumulative emissions subject to reliability
 *   constraints. Nuclear enters as a firm low-carbon resource that reduces
 *   the overbuild/storage needed for high-renewable systems. The constraint
 *   is technology-neutral on carbon intensity but not on capital intensity,
 *   timeline, or institutional form. Beneficiaries (nuclear industry, grid
 *   operators, energy-intensive users) gain subsidies, market access, and
 *   planning certainty. Victims (fossil sector, ratepayers, renewable
 *   developers in nuclear-priority markets) bear stranding risk, cost
 *   overruns, and market crowding. The reading coexists with two siblings:
 *   opportunity_cost (nuclear too slow/expensive per dollar) and
 *   systems_transition (nuclear perpetuates centralized control).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Portfolio Optimization Reading of Climate Mitigation: Nuclear as Necessary Baslowd").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '49c4553b-19d7-4a80-aae2-c800e8ef287a').
narrative_ontology:cs_kernel_codification('49c4553b-19d7-4a80-aae2-c800e8ef287a', formalized).
narrative_ontology:cs_authority_grounding('49c4553b-19d7-4a80-aae2-c800e8ef287a', lineage).
narrative_ontology:cs_interpretation_layer_present('49c4553b-19d7-4a80-aae2-c800e8ef287a').
narrative_ontology:cs_reading_relation('49c4553b-19d7-4a80-aae2-c800e8ef287a', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('49c4553b-19d7-4a80-aae2-c800e8ef287a', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('49c4553b-19d7-4a80-aae2-c800e8ef287a', foundational, reliability_constraint_binds_portfolio).
narrative_ontology:cs_axiom_status(reliability_constraint_binds_portfolio, holdable).
narrative_ontology:cs_axiom_grounding('49c4553b-19d7-4a80-aae2-c800e8ef287a', reliability_constraint_binds_portfolio, empirically_contingent).
narrative_ontology:cs_axiom('49c4553b-19d7-4a80-aae2-c800e8ef287a', foundational, technology_neutrality_on_carbon_only).
narrative_ontology:cs_axiom_status(technology_neutrality_on_carbon_only, holdable).
narrative_ontology:cs_axiom_grounding('49c4553b-19d7-4a80-aae2-c800e8ef287a', technology_neutrality_on_carbon_only, conventional).
narrative_ontology:cs_reference_frame('49c4553b-19d7-4a80-aae2-c800e8ef287a', ipcc_ar5_mitigation_pathways).
narrative_ontology:cs_drift_state('49c4553b-19d7-4a80-aae2-c800e8ef287a', post_ar6_storage_cost_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('49c4553b-19d7-4a80-aae2-c800e8ef287a', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, energy_intensive_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_sector).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers_in_nuclear_regions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, renewable_developers_in_constrained_markets).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, carbon_intensity_neutrality_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, baseload_reliability_requirement).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, technology_agnostic_mitigation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives direct subsidies (production tax credits, loan guarantees), liability caps (Price-Anderson), and regulatory streamlining under this reading. Depends on policy classification as 'clean' for market access. Exit options limited by massive sunk capital and regulatory lock-in.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    institutional, generational, constrained, global).

% Gain planning certainty and reliability services from nuclear baseload. Their operational mandates (reliability, resource adequacy) align with nuclear's firm capacity. They administer capacity markets that value firm low-carbon resources, giving them agenda-setting influence over portfolio composition.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, agenda_setter).

% Benefit from stable, low-carbon electricity prices enabled by nuclear in the mix. Can relocate production to jurisdictions with favorable nuclear policy (mobile exit), but face high switching costs for specialized facilities.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, energy_intensive_industry, beneficiary,
    organized, biographical, mobile, national).

% Primary victim: coal and gas generation displaced by nuclear + renewables portfolios. Face stranded asset risk and carbon pricing. Exit constrained by existing infrastructure and political influence, but structurally losing market share under any serious mitigation scenario.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_sector, payer,
    institutional, generational, constrained, global).

% Bear cost overruns and rate base increases from new nuclear construction (e.g., Vogtle, Flamanville). Captive customers of regulated utilities; exit requires moving or distributed generation adoption (constrained by housing, capital, interconnection).
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers_in_nuclear_regions, payer,
    moderate, biographical, constrained, regional).

% Face market crowding when nuclear receives priority in capacity markets or clean energy standards. Can deploy in other regions (mobile), but lose scale advantages in markets where nuclear dominates policy support.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_developers_in_constrained_markets, payer,
    organized, biographical, mobile, national).

% Evaluate portfolio compositions against carbon budgets, reliability metrics, and cost curves. Their models (IPCC, IEA, national grid studies) provide the analytical substrate for this reading. No direct stake in outcomes.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_policy_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a technology-neutral low-carbon portfolio that meets reliability constraints (firm capacity, inertia, frequency response) while minimizing cumulative emissions. Solves the intermittency-reliability trade-off by including nuclear as a firm low-carbon resource.
% TRANSFER_FUNCTION: Moves public subsidy, liability protection, and market preference from fossil generation and (at the margin) renewable developers toward nuclear operators and their supply chains. Ratepayers in nuclear regions absorb construction risk; fossil asset holders absorb stranding risk.
% ABSENT_VOICES: Communities hosting nuclear waste (intergenerational, no political voice); future ratepayers locked into long-term cost recovery; Global South nations excluded from nuclear technology transfer regimes. These voices would object to waste burden, intergenerational cost transfer, and technology access asymmetry.
% DISAPPEARANCE_RATIONALE: If the portfolio optimization reading vanished, clean energy standards would likely become renewables-only, capacity markets would lose firm low-carbon valuation, and nuclear new-build would stall in liberalized markets. Emissions trajectories would shift toward higher reliance on gas firming or storage breakthroughs. The world rearranges because infrastructure plans, subsidy regimes, and regulatory frameworks are built around this reading.
% FOUNDING_PROBLEM: Early climate policy recognized that variable renewables alone could not guarantee reliability at decarbonization scale without massive overbuild or unproven storage. The portfolio reading was built to keep nuclear as a hedge against renewables integration limits.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators (PJM, ERCOT, RTE) and IPCC WGIII attest the reliability problem remains live. Renewable advocates and storage developers attest the problem is substantially solved by falling storage costs and grid expansion. No neutral arbiter; the corroboration split mirrors the kernel contest.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).
:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real but bounded transfer: nuclear receives meaningful policy preference but not monopoly rents; fossil displacement is the primary extraction vector. Suppression (0.38) captures active exclusion of fossil generation and market design that disadvantages non-firm resources — but alternatives (renewables+storage) remain legally and technically viable. Theater (0.22) is moderate: reliability services are genuine, but 'baseload necessity' framing increasingly diverges from grid operations reality (inverter-based resources providing grid-forming services). Metrics measured at 2025 (interval midpoint) as the reading's peak policy influence.
 *
 * PERSPECTIVAL GAP:
 *   From nuclear industry seat: genuine coordination solving reliability at lowest system cost. From fossil sector seat: targeted extraction via policy preference for a competitor. From ratepayer seat: imposed cost for a reliability service they may not value (increasingly provided by alternatives). From renewable developer seat: unfair market distortion. The engine computes these divergences from power/exit/role declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry and grid operators sit at beneficiary end (d ~0.2-0.3): they collect subsidies and control planning. Fossil sector sits at target end (d ~0.8-0.9): structural displacement is the mechanism. Ratepayers are constrained payers (d ~0.6): captive to regulated cost recovery. Renewable developers are mobile payers (d ~0.4): can exit markets but lose scale. Analytical observers sit at d=0.5 (symmetric). Exit options differentiate same-power actors: energy-intensive industry (mobile) vs ratepayers (constrained) both moderate power but different d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliability hedge) is contested — storage and grid integration may have solved it. If the problem is dead but the reading persists, mandatrophy applies. Current status 'contested' means the engine should flag for review but not auto-reclassify. The reading's continuation is actively maintained by institutional beneficiaries (nuclear industry, grid operators) rather than passively drifting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliability_necessity_boundary,
    'Is nuclear''s firm capacity structurally necessary for grid reliability at >90% decarbonization, or is it one option among many (long-duration storage, transmission, demand response, grid-forming inverters)?',
    'Empirical: grid operator studies (NERC, ENTSO-E, ISO-NE) modeling high-renewable systems with and without nuclear. Track whether reliability metrics (LOLE, SAIDI) diverge.',
    'If nuclear is not necessary, the coordination function collapses — the reading becomes pure extraction (snare) for nuclear beneficiaries. If necessary, the coordination function holds and tangled_rope classification is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_necessity_boundary, empirical, 'Whether the coordination function (reliability) genuinely requires nuclear or merely prefers it.').

omega_variable(
    cost_trajectory_crossover,
    'At what renewable+storage penetration does the marginal cost of firm low-carbon capacity favor storage/transmission over new nuclear?',
    'Track LCOE and LCOS (levelized cost of storage) trajectories from Lazard, BNEF, IEA. Identify crossover points in major markets.',
    'If crossover has occurred or is imminent, the reading''s extractiveness is higher than measured — subsidies support a dominated technology. If crossover is distant, coordination function remains cost-justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_trajectory_crossover, empirical, 'Economic boundary where nuclear shifts from coordination to extraction.').

omega_variable(
    reading_framing_underdetermination,
    'Does ''technology-neutral carbon intensity'' framing genuinely coordinate a portfolio, or does it structurally privilege nuclear by externalizing waste, decommissioning, and proliferation costs that renewables do not carry?',
    'Full lifecycle cost internalization comparison: nuclear (waste fund adequacy, decommissioning reserves, liability caps) vs renewables (recycling, land remediation). Regulatory accounting audit.',
    'If nuclear''s apparent cost advantage depends on uninternalized externalities, the ''neutral'' framing is a cover story — extraction is higher, classification shifts toward snare. If internalized costs still favor nuclear in some portfolios, coordination holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the technology-neutral framing is structurally honest or a subsidy vehicle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmi_por_tr_t2015, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(cmi_por_tr_t2020, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(cmi_por_tr_t2025, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement(cmi_por_tr_t2030, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2030, 0.25).
narrative_ontology:measurement(cmi_por_tr_t2035, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 2035, 0.22).

% Extraction over time
narrative_ontology:measurement(cmi_por_be_t2015, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(cmi_por_be_t2020, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(cmi_por_be_t2025, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement(cmi_por_be_t2030, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2030, 0.45).
narrative_ontology:measurement(cmi_por_be_t2035, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 2035, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cmi_por_su_t2015, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(cmi_por_su_t2020, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement(cmi_por_su_t2025, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement(cmi_por_su_t2030, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2030, 0.35).
narrative_ontology:measurement(cmi_por_su_t2035, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 2035, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, clean_energy_standard_design).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, capacity_market_reform).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_waste_policy).

% DUAL FORMULATION NOTE:
% Part of the climate_mitigation_imperative constraint family (3 readings). This reading (portfolio_optimization) treats mitigation as portfolio optimization with reliability constraint; opportunity_cost treats it as fastest-deployment-per-dollar; systems_transition treats it as structural transformation toward decentralization. All three share the kernel 'climate mitigation is necessary' but differ on technology eligibility, beneficiary sets, and extraction vectors. ε values differ: portfolio (0.42), opportunity_cost (0.58), systems_transition (0.51) — different constraints, not measurement differences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, institutional, 0.25).
constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
