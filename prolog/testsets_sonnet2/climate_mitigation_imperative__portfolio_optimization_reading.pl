% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Portfolio-Optimization Reading of the Climate Mitigation Imperative (Technology-Neutral Carbon Intensity Standard)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the portfolio-optimization reading of the
 *   contested climate mitigation imperative kernel: mitigation requires
 *   maximizing deployment of every low-carbon source, with nuclear
 *   specifically necessary because only it can supply firm, dispatchable,
 *   weather-independent baseload at grid scale. Under this reading, a
 *   technology-neutral carbon-intensity standard is built into capacity
 *   markets and loan-guarantee programs so that nuclear, wind, solar, hydro,
 *   and storage compete on a common decarbonization metric rather than being
 *   singled out or excluded by name. The structural delta from the sibling
 *   readings is specific: nuclear enters the beneficiary set (receiving
 *   capacity payments, loan guarantees, and regulatory carve-outs justified
 *   by baseload necessity), fossil fuel extraction workers and dependent
 *   regions become the primary payers (accelerated retirement without
 *   proportional transition investment), and distributed renewable developers
 *   are structurally disadvantaged relative to centralized dispatchable
 *   assets even though the standard claims neutrality. This is a different
 *   constraint from the opportunity-cost reading (which treats nuclear's
 *   capital intensity and timeline as net-harmful to fastest-dollar
 *   decarbonization) and from the systems-transition reading (which treats
 *   nuclear as perpetuating extractive centralization). Each reading has its
 *   own epsilon, its own beneficiary/victim structure, and its own
 *   classification; they are linked as siblings in the same kernel contest,
 *   not merged into one story.
 *
 * KEY AGENTS:
 *   - nuclear_generation_operators: primary beneficiary (organized/constrained) — collects subsidy and capacity-market preference under the baseload-necessity premise
 *   - grid_reliability_authorities: agenda_setter (institutional/analytical) — designs and enforces the technology-neutral carbon-intensity standard
 *   - incumbent_utility_holding_companies: secondary beneficiary (powerful/arbitrage) — preserves centralized rate base against distributed displacement
 *   - fossil_fuel_extraction_workers and fossil_fuel_dependent_regions: primary payers (powerless/trapped) — bear accelerated retirement costs without proportional transition support
 *   - distributed_renewable_developers: secondary payer/excluded (moderate/constrained) — structurally disadvantaged by reliability-credit weighting despite technology-neutral framing
 *   - systems_transition_advocates: excluded (moderate/constrained) — present procedurally but discounted substantively by a metric with no scoring for ownership structure
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
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Portfolio-Optimization Reading of the Climate Mitigation Imperative (Technology-Neutral Carbon Intensity Standard)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '00553908-d720-473b-8c6a-298810131b5b').
narrative_ontology:cs_kernel_codification('00553908-d720-473b-8c6a-298810131b5b', distributed).
narrative_ontology:cs_authority_grounding('00553908-d720-473b-8c6a-298810131b5b', distributed).
narrative_ontology:cs_reading_relation('00553908-d720-473b-8c6a-298810131b5b', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('00553908-d720-473b-8c6a-298810131b5b', climate_mitigation_imperative__systems_transition_reading, influences).
narrative_ontology:cs_axiom('00553908-d720-473b-8c6a-298810131b5b', foundational, dispatchable_firm_capacity_is_necessary_condition).
narrative_ontology:cs_axiom_status(dispatchable_firm_capacity_is_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('00553908-d720-473b-8c6a-298810131b5b', dispatchable_firm_capacity_is_necessary_condition, empirically_contingent).
narrative_ontology:cs_axiom('00553908-d720-473b-8c6a-298810131b5b', secondary, technology_neutral_carbon_metric_is_sufficient_fairness_standard).
narrative_ontology:cs_axiom_status(technology_neutral_carbon_metric_is_sufficient_fairness_standard, holdable).
narrative_ontology:cs_axiom_grounding('00553908-d720-473b-8c6a-298810131b5b', technology_neutral_carbon_metric_is_sufficient_fairness_standard, conventional).
narrative_ontology:cs_reference_frame('00553908-d720-473b-8c6a-298810131b5b', post_2011_baseload_reliability_consensus).
narrative_ontology:cs_drift_state('00553908-d720-473b-8c6a-298810131b5b', post_storage_cost_collapse_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('00553908-d720-473b-8c6a-298810131b5b', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_generation_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_authorities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, incumbent_utility_holding_companies).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_extraction_workers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_dependent_regions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, distributed_renewable_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers_and_taxpayers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers_and_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate large capital-intensive plants that qualify as low-carbon baseload under this reading's carbon-intensity metric. Receive loan guarantees, capacity payments, and regulatory carve-outs justified by the necessity-of-baseload framing. Their long asset lifetimes make them dependent on the policy regime remaining favorable for decades; they lobby heavily to keep the technology-neutral standard in place because it is the mechanism through which they receive support without being singled out as a subsidized technology.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_generation_operators, beneficiary,
    organized, generational, constrained, national).

% Design and enforce the technology-neutral carbon-intensity standard that counts nuclear, wind, solar, hydro, and geothermal identically as 'low-carbon.' They administer capacity auctions and reliability mandates that structurally favor dispatchable baseload sources, embedding the necessity claim into procurement rules. They gain institutional authority and continuity of mandate from being the arbiter of what counts as adequate reliability.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_authorities, beneficiary).

% Own or co-invest in existing nuclear fleets and gain from a policy architecture that preserves centralized generation and prevents distributed alternatives from displacing their transmission-and-distribution rate base. They can shift capital between technologies as the standard evolves, giving them far more room to maneuver than either fossil workers or independent renewable developers.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, incumbent_utility_holding_companies, beneficiary,
    powerful, generational, arbitrage, national).

% Employed in coal, oil, and gas extraction and generation communities that are the primary targets of decarbonization under any reading of the mitigation imperative, but under this reading specifically bear costs without receiving the just-transition emphasis that a systems-transition reading would prioritize. Their exit from the affected industry requires retraining, relocation, or absorbing wage and pension losses; geographic and skill lock-in leaves most with no meaningful alternative inside the region.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_extraction_workers, payer,
    powerless, biographical, trapped, regional).

% Local tax bases, school funding, and municipal budgets are structured around fossil extraction and generation revenue. The portfolio-optimization reading's technology-neutral standard accelerates fossil retirement without dedicating comparable capital to regional economic replacement, since compliance capital flows preferentially to nuclear and utility-scale renewables sited elsewhere. These regions cannot relocate; their tax and employment base erodes over the interval covered by the standard's enforcement.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_dependent_regions, payer,
    powerless, generational, trapped, regional).

% Develop rooftop solar, community wind, and distributed storage projects that compete for the same decarbonization budget and grid interconnection queue slots as nuclear and utility-scale projects. Under a technology-neutral standard weighted toward dispatchable baseload, their smaller, intermittent, distributed assets are treated as structurally less valuable for reliability credit, crowding them out of capacity markets even where they are cheaper and faster to deploy per ton abated.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, distributed_renewable_developers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, distributed_renewable_developers, excluded).

% Fund nuclear capital costs, loan guarantees, and capacity payments through electricity rates and taxes, and in return receive the benefit of a more reliable, lower-emission grid than would exist absent any climate mitigation constraint. Their exit is limited to reducing consumption or, for the wealthiest, self-generation — most cannot opt out of the rate base that funds the arrangement.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers_and_taxpayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers_and_taxpayers, beneficiary).

% Argue that centralized nuclear procurement under this reading entrenches utility control and forecloses the democratized, decentralized grid architecture they consider the actual mitigation goal. They participate in regulatory dockets and public comment but the portfolio-optimization standard's technology-neutral, reliability-first framing structurally discounts their governance-and-ownership arguments as outside the scope of a carbon-intensity metric.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, systems_transition_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_generation_operators).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation across all low-carbon generation technologies against a shared, auditable metric (carbon intensity per unit delivered, adjusted for dispatchability) so that grid planners, investors, and regulators can compare nuclear, renewables, and storage on common terms rather than technology-specific mandates.
% TRANSFER_FUNCTION: Moves capital, loan guarantees, and capacity-market revenue toward nuclear operators and incumbent utilities (via the baseload-necessity premise embedded in procurement rules) and away from fossil fuel regions (via accelerated retirement without proportional transition funding) and from distributed renewable developers (via reliability-credit discounting).
% ABSENT_VOICES: Fossil-dependent regional governments and distributed renewable cooperatives raise concerns in regulatory dockets but have no seat in the reliability-standard-setting process itself, which is dominated by grid engineers and incumbent generation interests who define what counts as 'reliable.' Systems-transition advocates are procedurally present but substantively discounted because the standard's carbon-intensity metric has no mechanism for scoring ownership structure or decentralization.
% DISAPPEARANCE_RATIONALE: If the technology-neutral, baseload-necessity standard were repealed, nuclear operators would lose capacity-market preference and loan guarantee eligibility, prompting several marginal plants toward early retirement; capital would reallocate faster toward the cheapest deployable-per-dollar low-carbon assets (favoring the opportunity-cost reading's preferred mix), and distributed renewable developers would gain interconnection queue priority. Utility holding companies would face pressure to restructure around distributed assets. The grid's institutional architecture — who plans it and on what metric — would visibly change.
% FOUNDING_PROBLEM: Intermittent renewable deployment alone cannot guarantee grid reliability during multi-day low-wind, low-solar periods, and rapid fossil retirement without a dispatchable low-carbon substitute risks reliability crises that would politically discredit decarbonization entirely.
% FOUNDING_PROBLEM_CORROBORATION: Grid reliability engineers outside the nuclear industry (independent system operators, national labs) corroborate that dispatchable capacity is a genuine technical requirement at high renewable penetration, supporting the founding problem as still live. However, storage-technology researchers and systems-transition advocates — also outside the nuclear beneficiary set — contest that the specific solution (large centralized nuclear baseload) is still the only or best answer given falling storage costs, meaning the founding problem's diagnosis is corroborated but the prescribed solution is independently disputed.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) reflects genuine coordination — the technology-neutral carbon-intensity metric does solve a real comparability problem across generation types — combined with a real asymmetric transfer toward nuclear/incumbent-utility capital and away from fossil regions and distributed developers. This sits meaningfully below what a pure extraction reading would show, because the coordination function (comparable decarbonization accounting) is genuine and would persist even if the specific beneficiary allocation changed. Suppression (0.38) and its rising trajectory reflect hardening enforcement: as capacity-market rules and loan-guarantee criteria formalize the baseload-necessity premise over the interval, the standard increasingly forecloses procurement pathways that don't credit dispatchable generation, which is why distributed developers and systems-transition advocates experience escalating exclusion even without any change in the metric's stated neutrality. Theater ratio (0.22) is modest: most of the standard's activity is genuine technical accounting rather than performance, though a growing share of nuclear-specific carve-out justification increasingly recites baseload necessity as doctrine rather than re-deriving it from current storage-cost data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators and incumbent utilities sit near the beneficiary end: they receive capital and regulatory support through a mechanism (technology-neutral scoring) whose neutrality is precisely what channels the benefit to them, since only currently-deployed dispatchable technology qualifies for full reliability credit. Fossil fuel workers and dependent regions sit near the full-target end: trapped exit options, biographical-to-generational time horizons for community effects, and no proportional transition capital under this reading's allocation logic. Distributed renewable developers sit closer to target than the 'technology-neutral' label would suggest — their constrained exit and moderate power mean the standard's dispatchability weighting extracts opportunity from them even though they are nominally included as a qualifying low-carbon technology. Ratepayers sit near symmetric: real reliability and emissions benefit, real cost pass-through.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (a common carbon-intensity metric enabling technology comparison) remains genuinely live — intermittency-driven reliability risk is a real physical constraint, not a fabricated one. What prevents this from being read as pure extraction is that the standard would still need to solve a real problem (dispatchable low-carbon supply) even if reformed; what prevents it from being read as pure coordination is that the specific mechanism chosen (baseload-necessity-weighted capacity credit) systematically channels benefit to incumbent centralized generation over cheaper distributed alternatives, and does so with escalating enforcement rigidity over time. Tangled Rope, not Rope: the coordination is real, the extraction is real, and the same mechanism carries both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity_empirical_status,
    'Is large-scale nuclear baseload structurally necessary for grid reliability at high renewable penetration, or has falling storage-cost and demand-response technology made the necessity claim obsolete for new capacity decisions (even if legacy plants remain useful)?',
    'Comparative grid-reliability studies across jurisdictions with different generation mixes (e.g., high-storage vs. high-nuclear grids) tracking loss-of-load-expectation outcomes over multi-year periods, cross-checked against independent system operator technical assessments rather than incumbent-funded modeling.',
    'If storage/demand-response substitutes are empirically adequate, the reading''s foundational premise weakens substantially, shifting classification toward the opportunity-cost reading''s structure (nuclear as net-harmful rather than necessary) and reducing the justification for nuclear''s beneficiary position in procurement rules. If genuinely necessary at current technology levels, the coordination function is stronger than the extraction reading of this story suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_status, empirical, 'Whether nuclear baseload is technically necessary or a technology-specific policy preference presented as physical necessity.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three sibling readings of the climate mitigation imperative (portfolio-optimization, opportunity-cost, systems-transition) genuinely reconcilable through better empirical data, or do they rest on different underlying values (risk tolerance for reliability, discount rates on capital deployment speed, preference for centralized vs. distributed governance) that no amount of evidence resolves?',
    'Track whether experts converge on a single reading as storage costs, deployment data, and grid operation experience accumulate, versus whether disagreement persists even among parties with access to the same evidence — the latter would indicate a genuine values conflict rather than an empirical one.',
    'If reconcilable, later evidence should collapse the kernel to a single dominant reading and this story''s classification would become provisional. If genuinely incommensurable, all three readings persist indefinitely as parallel, non-converging constraints — none is more ''true'' than the others, and the classification differences reflect real, permanent structural differences in whose interests each reading''s mechanism serves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel contest is empirically resolvable or reflects a permanent values disagreement about how to weigh reliability, speed, and governance structure.').

omega_variable(
    transition_capital_allocation_intent,
    'Is the underinvestment in fossil-dependent-region transition support a deliberate feature of how this reading''s coalition allocates capital (nuclear and utility interests capturing transition funding streams), or an unaddressed gap that could be fixed without altering the reading''s core technology-neutral premise?',
    'Track legislative and regulatory proposals: if transition-funding amendments are repeatedly defeated by the same coalition that benefits from nuclear/utility capacity payments, that supports deliberate capture; if transition funding is simply an omitted afterthought, correctable within the existing framework.',
    'If deliberate capture, the tangled_rope classification is robust and the victim-side extraction is structural, not incidental. If a correctable gap, the constraint could shift toward a rope classification with an added transition-funding component, reducing measured extraction without changing the baseload-necessity premise itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_capital_allocation_intent, conceptual, 'Whether transition-funding neglect is structural to this reading''s coalition or an addressable policy gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings decomposed from the single natural-language claim 'the climate mitigation imperative requires nuclear.' Each reading has a distinct epsilon and beneficiary/victim structure: portfolio_optimization_reading (this story, epsilon=0.42, nuclear as beneficiary, fossil workers/regions and distributed developers as payers, tangled_rope); opportunity_cost_reading (nuclear as target/net-harmful, fastest-deployment technologies as beneficiary); systems_transition_reading (nuclear as perpetuator of centralized extraction, decentralized/community-owned generation as beneficiary). The three are linked via affects_constraints because policy resource allocation is zero-sum across the readings at the level of capital and regulatory attention — a jurisdiction adopting this reading's standard structurally reduces resources available under the sibling readings' preferred allocations, which is the mechanism of the 'influences' relation declared in cs_structure below.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
