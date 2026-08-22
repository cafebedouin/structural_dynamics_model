% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Portfolio Doctrine for Decarbonization Policy
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates the portfolio-pragmatism reading of the contested
 *   climate mitigation legitimacy kernel: the claim that optimal
 *   decarbonization requires a technology-neutral mix of nuclear and
 *   renewables rather than privileging either. Under this reading, the
 *   standing arrangement under contest is technology-neutral procurement
 *   policy as currently practiced — not an idealized least-cost optimization,
 *   but the actual doctrine as it operates in most OECD grid planning
 *   regimes, where genuine hedging value coexists with incumbent-protecting
 *   redundancy. The reading treats neither technology as a priori superior
 *   and expects the optimal mix to vary by region and by the specific
 *   cost/reliability data available to that region's planners; it explicitly
 *   rejects both the baseload-necessity reading's nuclear primacy and the
 *   renewable-primacy reading's renewables-only sufficiency claim as
 *   premature technology commitments given real uncertainty.
 *
 * KEY AGENTS:
 *   - grid_planning_agencies: agenda_setter (institutional/analytical) — mandates and administers the technology-neutral procurement doctrine
 *   - diversified_utility_holding_companies: beneficiary (powerful/arbitrage) — hedges across both technology classes, captures optionality value
 *   - single_technology_developers_excluded_from_subsidy: payer (moderate/constrained) — loses share of subsidy pool and interconnection priority to the mandated split
 *   - ratepayers_funding_redundant_capacity: payer (powerless/trapped) — bears cost of hedge-driven redundancy where local optimization would be cheaper
 *   - communities_hosting_both_nuclear_and_renewable_siting_burdens: payer (powerless/trapped) — hosts dual siting burden under a mix rather than a concentrated single-technology compensation regime
 *   - nuclear_and_renewable_equipment_manufacturers_jointly: beneficiary (organized/mobile) — captures durable procurement share regardless of which technology wins any tender
 *   - climate_scientists_and_grid_reliability_engineers: observer (analytical/analytical) — evaluates regional optimality independent of institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Portfolio Doctrine for Decarbonization Policy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '0520c7c6-f32b-4bf0-b6e8-07cc810b0c46').
narrative_ontology:cs_kernel_codification('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', distributed).
narrative_ontology:cs_authority_grounding('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', distributed).
narrative_ontology:cs_reading_relation('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', foundational, no_technology_privileged_a_priori).
narrative_ontology:cs_axiom_status(no_technology_privileged_a_priori, holdable).
narrative_ontology:cs_axiom_grounding('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', no_technology_privileged_a_priori, empirically_contingent).
narrative_ontology:cs_axiom('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', foundational, optimal_mix_is_regionally_determined).
narrative_ontology:cs_axiom_status(optimal_mix_is_regionally_determined, holdable).
narrative_ontology:cs_axiom_grounding('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', optimal_mix_is_regionally_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', technology_uncertainty_hedging_framework).
narrative_ontology:cs_drift_state('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', post_cost_curve_convergence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0520c7c6-f32b-4bf0-b6e8-07cc810b0c46', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_holding_companies).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_planning_agencies).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_and_renewable_equipment_manufacturers_jointly).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_developers_excluded_from_subsidy).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers_funding_redundant_capacity).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, communities_hosting_both_nuclear_and_renewable_siting_burdens).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, technology_neutrality_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, least_regrets_portfolio_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design integrated resource plans that formally mandate consideration of both nuclear and renewable options, set procurement targets accordingly, and justify the mixed portfolio as risk-hedging against technology-specific failure modes (renewable intermittency, nuclear cost overruns and construction delay). Their authority and budget expand under a technology-neutral mandate rather than a single-technology one.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_planning_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Hold assets across nuclear, wind, solar, and gas-backup portfolios. A technology-neutral policy lets them rate-base whichever asset class is politically favored in a given cycle, hedge stranded-asset risk across their portfolio, and avoid being forced to divest from any single technology. They can shift capital allocation faster than regulators can re-litigate the doctrine.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_holding_companies, beneficiary,
    powerful, generational, arbitrage, national).

% Firms that specialize purely in utility-scale solar, offshore wind, or advanced nuclear and have no diversified hedge. Technology-neutral procurement rules split subsidy pools and grid interconnection queues across competing technologies, slowing any single developer's buildout relative to a technology-committed regime, and forcing them to compete for capital against a doctrine that treats their specialization as a bet rather than a strategy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_developers_excluded_from_subsidy, payer,
    moderate, biographical, constrained, regional).

% Pay through electricity bills for a portfolio that, in some regions, builds parallel nuclear and renewable capacity beyond what either alone would require for reliability, because the doctrine's hedge-against-uncertainty logic favors redundancy over least-cost optimization for their specific grid. They have no vote on the resource mix and cannot choose a cheaper single-technology rate class.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers_funding_redundant_capacity, payer,
    powerless, biographical, trapped, regional).

% Regions selected as national energy hubs may host both nuclear waste storage and dense transmission/renewable buildout because the neutral portfolio spreads siting need across multiple project types rather than concentrating expertise and compensation around one technology's established community-benefit framework. Exit means relocation; voice is diluted across two separate permitting and compensation regimes.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, communities_hosting_both_nuclear_and_renewable_siting_burdens, payer,
    powerless, generational, trapped, local).

% Industry associations spanning both reactor vendors and renewable OEMs jointly lobby for technology-neutral framing because it guarantees each a durable share of public procurement rather than an all-or-nothing outcome from a single-technology mandate. They shape the doctrine's legislative language and benefit from its persistence regardless of which technology wins any given tender.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_and_renewable_equipment_manufacturers_jointly, beneficiary,
    organized, generational, mobile, global).

% Assess decarbonization pathways on physical and economic merits without institutional stake in either technology's market share; can evaluate whether a given jurisdiction's optimal mix genuinely requires both nuclear and renewables or whether the neutrality doctrine is being used to justify politically convenient redundancy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists_and_grid_reliability_engineers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decarbonization investment across a genuinely uncertain technology landscape: neither nuclear nor renewables dominates on cost, reliability, and buildability across all regions and time horizons, so a portfolio approach hedges against any single technology's failure to scale, cost-overrun, or site-availability constraint.
% TRANSFER_FUNCTION: Moves procurement guarantees, subsidy allocation, and grid-interconnection priority from any single winning technology's developers toward a split allocation across both nuclear and renewable developers, and moves the cost of that hedge from technology-specific risk-bearers onto ratepayers and host communities generally.
% ABSENT_VOICES: Single-technology developers who could demonstrate a cheaper least-cost path for a specific region are structurally deprioritized by a doctrine that treats neutrality as a virtue independent of regional cost data; ratepayers in regions where one technology is clearly superior have no seat in the national-level portfolio framing that overrides local optimization.
% DISAPPEARANCE_RATIONALE: Grid planners and diversified utilities argue that abandoning technology neutrality would mean picking losers prematurely and stranding capital when either technology underperforms; single-technology developers and cost-conscious regulators argue the doctrine mainly protects incumbent diversified portfolios from having to compete on regional merit, and that many regions would rearrange toward a genuinely cheaper single-dominant-technology mix if the neutrality mandate were removed.
% FOUNDING_PROBLEM: Early 2000s-2010s decarbonization planning oscillated between renewable-only and nuclear-only advocacy coalitions, each claiming their technology alone could achieve full decarbonization; the portfolio doctrine emerged to prevent premature technology lock-in given genuine uncertainty about future cost curves, storage breakthroughs, and nuclear construction learning curves.
% FOUNDING_PROBLEM_CORROBORATION: IPCC mitigation pathway modeling and multiple national grid operators' capacity-adequacy studies corroborate that technology diversification reduces tail-risk in decarbonization pathways under genuine cost uncertainty — this is attestation from outside the beneficiary set. However, independent cost-of-electricity analyses in several specific regions (e.g., strong-wind or strong-solar-resource jurisdictions) find single-technology paths would have been cheaper than the mandated portfolio mix, suggesting the doctrine is applied uniformly even where the founding uncertainty rationale does not hold locally.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the portfolio doctrine has real hedging value under genuine cost-curve and buildability uncertainty, but a nontrivial share of its persistence traces to diversified incumbents' interest in avoiding technology-specific competition on the merits. Suppression (0.38) reflects that single-technology developers with strong regional cost cases are not blocked outright but face a structurally tilted procurement and subsidy allocation process that treats neutrality as the default even where local data would justify concentration. Theater ratio (0.30) captures that some 'technology-neutral' planning exercises function primarily to legitimize decisions already made on political-coalition grounds rather than genuine least-cost analysis. Resistance (0.55) is comparatively high because single-technology advocacy coalitions (pure-renewable and pure-nuclear camps) both actively contest the doctrine's application in specific dockets. Accessibility collapse (0.35) is moderate-low: alternative single-technology pathways remain visible and are actively argued for in regulatory proceedings, they are just structurally disadvantaged rather than foreclosed.
 *
 * DIRECTIONALITY LOGIC:
 *   Diversified utilities and joint industry associations sit near the beneficiary end: the neutral doctrine converts what would be a winner-take-most competition into a guaranteed-share arrangement for any actor with a diversified or mixed portfolio, and their exit options (arbitrage, mobile) let them capture that optionality value across jurisdictions. Grid planning agencies are agenda-setters whose institutional mandate and budget expand under the neutrality framework. Single-technology developers, ratepayers, and host communities carry the transfer function: developers lose competitive share regardless of merit, ratepayers fund redundancy where a single-technology path would be cheaper, and host communities absorb dual siting burdens. The powerless/trapped exit options for ratepayers and host communities reflect that neither group can select a different jurisdiction's resource mix or exit the rate base.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine technological uncertainty in the 2000s-2010s about which decarbonization pathway would prove cheapest and most buildable — was real and is only partially resolved: cost curves have converged in some respects (both technologies now compete on cost in many markets) but remain genuinely uncertain in others (nuclear construction risk, storage-cost trajectories at very high renewable penetrations). This story treats the mandate as contested rather than dead: unlike a pure mandatrophy case, there IS a live coordination function in many regions. The engine's classification as tangled_rope (rather than a clean rope) is intended to reflect that the SAME structure that provides genuine uncertainty-hedging in ambiguous-cost regions also protects diversified incumbents from having to compete on regional merit in unambiguous-cost regions — the coordination function does not disappear, but it is not the whole story either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_hedge_vs_incumbent_protection,
    'In any given regional application, is the technology-neutral portfolio mandate primarily solving genuine cost/reliability uncertainty, or is it primarily protecting diversified incumbents from having to compete on regional merit?',
    'Region-by-region counterfactual cost analysis: compare the mandated portfolio''s total system cost against the best available single-technology alternative using the region''s actual wind/solar resource quality, nuclear siting feasibility, and grid topology. A persistent, large cost gap in one direction across many regions would indicate the doctrine functions more as incumbent protection than genuine hedging in those regions.',
    'If the hedge-value interpretation dominates, this reading''s tangled_rope classification would move toward rope (predominantly coordination); if the incumbent-protection interpretation dominates in most jurisdictions, it would move toward snare (predominantly extraction dressed as risk management).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_hedge_vs_incumbent_protection, empirical, 'Whether portfolio neutrality reflects genuine uncertainty-hedging or incumbent-protecting redundancy in practice.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the four kernel readings disagree — is it a factual dispute about cost curves and buildability, or a normative dispute about which risks are acceptable to hedge against?',
    'Decompose each reading''s core claim into (a) empirical sub-claims about cost, buildability, and reliability that could in principle be resolved by data, and (b) normative sub-claims about acceptable risk tolerance, distributional fairness, and intergenerational obligation that data alone cannot resolve.',
    'If the disagreement is mostly empirical, convergence toward one reading over the interval as cost/reliability data accumulates is plausible, and this reading''s ''neither privileged a priori'' stance may itself become empirically falsifiable in either direction. If mostly normative, the readings will coexist indefinitely as genuinely different value commitments about acceptable risk, and no amount of cost data will resolve which reading is ''correct.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel contest is empirically resolvable or reflects irreducible value disagreement about risk tolerance.').

omega_variable(
    regional_variation_measurement_gap,
    'Does the doctrine actually implement regional variation in optimal mix as this reading claims, or does it in practice apply a uniform national/international template regardless of regional cost data?',
    'Audit actual integrated resource plans across multiple jurisdictions for whether the specific nuclear/renewable ratio mandated tracks documented regional cost and resource data, or is a fixed ratio applied irrespective of regional variation.',
    'If regional variation is genuinely implemented, this reading''s structural delta claim holds and extraction is concentrated in the residual incumbent-protection component; if a uniform ratio is applied regardless of region, the ''technology-neutral, regionally optimal'' framing is itself largely theatrical, and theater_ratio should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variation_measurement_gap, empirical, 'Whether claimed regional optimization is actually practiced or is a uniform template dressed as regional analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.15).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the natural-language label 'optimal decarbonization technology mix' per the ε-invariance principle. Each reading of the climate_mitigation_legitimacy kernel — baseload_necessity, renewable_primacy, degrowth_sufficiency, and this portfolio_pragmatism reading — has its own ε, beneficiary/victim structure, and classification, because measuring 'the correct decarbonization pathway' by different lights (baseload reliability, cost-and-speed, demand reduction, or portfolio hedging) yields structurally different extraction profiles and different victim sets. They are linked via network edges rather than merged into one constraint with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
