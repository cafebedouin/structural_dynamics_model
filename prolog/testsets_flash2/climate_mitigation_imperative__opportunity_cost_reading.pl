% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Climate Mitigation Imperative (Opportunity Cost Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'opportunity cost' reading of the climate
 *   mitigation imperative, which prioritizes energy technologies based on
 *   their ability to deliver the fastest and most cost-effective carbon
 *   reductions. Under this reading, nuclear power, despite being low-carbon,
 *   is seen as a net-harmful diversion of capital and time due to its high
 *   upfront costs and long deployment timelines, which could otherwise be
 *   invested in faster-deploying renewables. The constraint is claimed as a
 *   Tangled Rope because it genuinely coordinates investment towards a
 *   collective good (climate mitigation) but does so by extracting from and
 *   suppressing alternatives (nuclear) that are also low-carbon, creating an
 *   asymmetric burden.
 *
 * KEY AGENTS:
 *   - renewable_energy_advocates: Primary beneficiary (organized/mobile) — promotes rapid, cost-effective decarbonization.
 *   - climate_vulnerable_communities: Ultimate beneficiary (powerless/trapped) — benefits from faster mitigation.
 *   - nuclear_industry: Primary target (institutional/constrained) — bears extraction from diverted capital.
 *   - pro_nuclear_policymakers: Secondary target (powerful/constrained) — faces pressure to de-prioritize nuclear.
 *   - fossil_fuel_industry: Excluded (institutional/constrained) — antithetical to the imperative.
 *   - climate_scientists: Analytical observer (analytical/analytical) — provides scientific basis for urgency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.4).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Imperative (Opportunity Cost Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf').
narrative_ontology:cs_kernel_codification('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', implicit).
narrative_ontology:cs_authority_grounding('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', expertise).
narrative_ontology:cs_reading_relation('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', foundational, carbon_reduction_per_dollar_per_year_maximization).
narrative_ontology:cs_axiom_status(carbon_reduction_per_dollar_per_year_maximization, holdable).
narrative_ontology:cs_axiom_grounding('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', carbon_reduction_per_dollar_per_year_maximization, empirically_contingent).
narrative_ontology:cs_axiom('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', foundational, nuclear_capital_intensity_and_timeline_are_net_harmful).
narrative_ontology:cs_axiom_status(nuclear_capital_intensity_and_timeline_are_net_harmful, holdable).
narrative_ontology:cs_axiom_grounding('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', nuclear_capital_intensity_and_timeline_are_net_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', urgent_cost_effective_decarbonization).
narrative_ontology:cs_drift_state('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('527bb8f1-8f19-48e6-abb2-7b35f5bcb7cf', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_vulnerable_communities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, pro_nuclear_policymakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the prioritization of faster, cheaper renewable deployments, aligning with their advocacy for rapid decarbonization. They actively promote policies that redirect investment away from nuclear.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates, beneficiary,
    organized, generational, mobile, global).

% Are the ultimate beneficiaries of rapid, cost-effective climate mitigation, as they face the most immediate and severe impacts of climate change. Their situation is improved by policies that maximize carbon reduction per dollar per year.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_vulnerable_communities, beneficiary,
    powerless, immediate, trapped, local).

% Bears the cost of this constraint as it diverts capital and policy support away from nuclear projects due to their high capital intensity and long deployment timelines. They advocate for nuclear as a necessary part of a low-carbon portfolio.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% Face political and budgetary pressure to prioritize investments that yield the fastest climate impact, making it difficult to fund new nuclear projects. Their policy agenda is constrained by the imperative for rapid, cost-effective mitigation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, pro_nuclear_policymakers, payer,
    powerful, biographical, constrained, national).

% Is excluded from the conversation about low-carbon energy choices, as the constraint focuses on the most effective mitigation strategies. Their business model is directly challenged by any effective climate mitigation policy.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fossil_fuel_industry, excluded,
    institutional, generational, constrained, global).

% Provide the scientific basis for the urgency of climate action and the need for effective mitigation strategies. They observe and analyze the outcomes of different energy policies without directly benefiting or paying.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global and national investment decisions towards climate mitigation strategies that deliver the fastest and most cost-effective carbon reductions, ensuring resources are allocated efficiently to meet urgent climate targets.
% TRANSFER_FUNCTION: Transfers capital and policy support from high-capital-intensity, long-timeline energy projects (like nuclear) to faster-deploying, lower-cost alternatives (like renewables), aiming to maximize carbon reduction per dollar per year.
% ABSENT_VOICES: The fossil fuel industry is entirely absent from this framing, as their interests are antithetical to the core imperative. Communities reliant on nuclear power for employment or energy security might also be marginalized, as the focus shifts to aggregate carbon reduction metrics.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, investment in climate mitigation would become less focused on speed and cost-effectiveness. Capital would likely flow to a wider array of technologies, including those with longer timelines and higher upfront costs, potentially slowing overall decarbonization efforts and shifting the global energy transition.
% FOUNDING_PROBLEM: The urgent need to reduce greenhouse gas emissions rapidly and cost-effectively to avert catastrophic climate change, given finite financial resources and limited time.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) and numerous national scientific academies consistently corroborate the urgency and the need for rapid, cost-effective mitigation. Independent economic analyses of energy systems also support the focus on deployment speed and capital efficiency for climate impact.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the significant capital and policy support diverted from nuclear projects. Suppression (0.4) is moderate, as nuclear is not outright banned but faces significant headwinds in policy and funding. Theater ratio (0.2) is low, as the imperative is genuinely focused on mitigation, though some 'greenwashing' of less effective projects may occur. Accessibility collapse (0.3) is moderate, as nuclear alternatives are not entirely eliminated but are made less viable. Resistance (0.7) is high, primarily from the nuclear industry and its advocates, who contest this framing.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable energy advocates and climate-vulnerable communities, this constraint is a necessary coordination mechanism for survival. From the nuclear industry and its proponents, it is an extractive snare that unfairly targets a viable low-carbon solution, driven by a narrow interpretation of 'fastest deployment per dollar' that ignores other benefits like baseload reliability or energy security.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy advocates and climate-vulnerable communities are beneficiaries (low d) as the constraint aligns with their interests in rapid, cost-effective decarbonization. The nuclear industry and pro-nuclear policymakers are victims (high d) as their preferred technology is disfavored. The fossil fuel industry is excluded, as their business model is fundamentally incompatible with the constraint's goal. Climate scientists are observers, providing data without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare by acknowledging its genuine coordination function (rapid climate mitigation). However, it also prevents mislabeling it as a pure Rope by highlighting the asymmetric extraction from the nuclear sector, which is a low-carbon alternative, and the active enforcement required to maintain this specific prioritization. The contest over 'fastest deployment per dollar' versus 'all low-carbon sources' is central to its Tangled Rope nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_benefit_analysis_scope,
    'Does the ''fastest deployment per dollar'' metric adequately capture all relevant costs and benefits, including grid stability, energy security, and long-term waste management for nuclear?',
    'Comprehensive, independent lifecycle assessment and integrated energy system modeling that includes externalities and system-level costs beyond direct capital expenditure and carbon reduction.',
    'If the metric is too narrow, the perceived ''extraction'' from nuclear might be overstated, potentially reclassifying the constraint closer to a Rope or even a Scaffold if nuclear''s systemic benefits are found to outweigh its opportunity costs. If the metric is robust, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_analysis_scope, empirical, 'Scope of cost-benefit analysis for energy technologies.').

omega_variable(
    nuclear_deployment_timeline_reduction,
    'Can modular reactor designs or streamlined regulatory processes significantly reduce nuclear''s capital intensity and deployment timelines, altering its opportunity cost profile?',
    'Empirical data from pilot projects and regulatory reforms demonstrating substantial reductions in nuclear project costs and construction times.',
    'If nuclear''s deployment profile improves dramatically, its position as a ''victim'' of this constraint would weaken, potentially shifting the constraint''s extractiveness downward and moving it closer to a Rope or even a Piton if the original justification for exclusion atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_deployment_timeline_reduction, empirical, 'Impact of technological and regulatory innovation on nuclear''s competitiveness.').

omega_variable(
    framing_of_mitigation_goal,
    'Is the primary goal of climate mitigation solely ''fastest carbon reduction per dollar,'' or does it encompass broader objectives like energy system resilience, equity, and long-term sustainability?',
    'Deliberative democratic processes and policy consensus-building that explicitly define the multi-dimensional goals of climate mitigation beyond a single metric.',
    'If the goal is broadened, this ''opportunity_cost_reading'' would be seen as a partial, rather than comprehensive, framing, potentially weakening its legitimacy and reducing its suppressive force on alternative low-carbon technologies. This would shift the constraint''s classification towards a more contested Tangled Rope or even a conceptual Snare if the narrow framing is seen as serving specific interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_mitigation_goal, conceptual, 'Conceptual framing of climate mitigation objectives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2030, 0.22).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2040, 0.23).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2050, 0.24).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2050, 0.7).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2030, 0.45).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2040, 0.48).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2050, 0.5).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_investment_prioritization).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, nuclear_power_regulatory_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_mitigation_imperative' kernel. This 'opportunity_cost_reading' focuses on maximizing carbon reduction per dollar per year, leading to the exclusion of nuclear power. The 'portfolio_optimization_reading' (a sibling constraint) views nuclear as a necessary baseload component, while the 'systems_transition_reading' (another sibling) critiques nuclear for perpetuating centralized control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
