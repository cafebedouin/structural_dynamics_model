% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Portfolio Decarbonization Legitimacy Claim
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the 'portfolio pragmatism' reading of the
 *   contested climate_mitigation_legitimacy kernel: the claim that optimal
 *   decarbonization requires a technology-neutral mix of nuclear and
 *   renewables, with no a priori privileging of either, and regional
 *   variation in the optimal blend. This reading is distinct from, and
 *   structurally different in ε from, the baseload_necessity_reading (which
 *   privileges dispatchable nuclear/gas-with-CCS as structurally necessary),
 *   the renewable_primacy_reading (which claims renewables-plus-storage
 *   dominates on cost and speed), and the degrowth_sufficiency_reading (which
 *   rejects the generation-expansion framing entirely). Those are separate
 *   constraints, not alternate measurements of this one. The pragmatism
 *   reading is authored here as a genuine coordination mechanism (portfolio
 *   theory applied to infrastructure risk) that has, over the observed
 *   interval, accumulated moderate extractive overhead as nuclear vendor
 *   consortiums and diversified incumbents use the 'balanced portfolio'
 *   framing to preserve procurement guarantees that would not survive pure
 *   least-cost competition in many regions.
 *
 * KEY AGENTS:
 *   - grid_planning_agencies: agenda_setter (institutional/analytical) — codifies portfolio mandates into procurement rules
 *   - diversified_utility_incumbents: beneficiary (institutional/arbitrage) — profits regardless of which technology wins
 *   - nuclear_vendor_consortiums: beneficiary (organized/constrained) — depends on mandate to remain competitive against cost trends
 *   - single_technology_developers: payer (moderate/constrained) — capped below what unconstrained competition would award
 *   - ratepayers_in_stranded_asset_regions: payer (powerless/trapped) — bears cost overruns from mandated diversity
 *   - climate_scientists_and_systems_modelers: observer (analytical/analytical) — supplies contested evidence base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Portfolio Decarbonization Legitimacy Claim").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'fd87d9cd-9641-403d-a4a8-33df71da77d8').
narrative_ontology:cs_kernel_codification('fd87d9cd-9641-403d-a4a8-33df71da77d8', distributed).
narrative_ontology:cs_authority_grounding('fd87d9cd-9641-403d-a4a8-33df71da77d8', expertise).
narrative_ontology:cs_interpretation_layer_present('fd87d9cd-9641-403d-a4a8-33df71da77d8').
narrative_ontology:cs_reading_relation('fd87d9cd-9641-403d-a4a8-33df71da77d8', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd87d9cd-9641-403d-a4a8-33df71da77d8', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd87d9cd-9641-403d-a4a8-33df71da77d8', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('fd87d9cd-9641-403d-a4a8-33df71da77d8', foundational, no_technology_privileged_a_priori).
narrative_ontology:cs_axiom_status(no_technology_privileged_a_priori, holdable).
narrative_ontology:cs_axiom_grounding('fd87d9cd-9641-403d-a4a8-33df71da77d8', no_technology_privileged_a_priori, instrumental).
narrative_ontology:cs_axiom('fd87d9cd-9641-403d-a4a8-33df71da77d8', foundational, optimal_mix_is_regionally_contingent_not_universal).
narrative_ontology:cs_axiom_status(optimal_mix_is_regionally_contingent_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('fd87d9cd-9641-403d-a4a8-33df71da77d8', optimal_mix_is_regionally_contingent_not_universal, empirically_contingent).
narrative_ontology:cs_axiom('fd87d9cd-9641-403d-a4a8-33df71da77d8', secondary, moderate_capital_diversification_reduces_systemic_risk).
narrative_ontology:cs_axiom_status(moderate_capital_diversification_reduces_systemic_risk, holdable).
narrative_ontology:cs_axiom_grounding('fd87d9cd-9641-403d-a4a8-33df71da77d8', moderate_capital_diversification_reduces_systemic_risk, instrumental).
narrative_ontology:cs_reference_frame('fd87d9cd-9641-403d-a4a8-33df71da77d8', least_cost_technology_agnostic_planning).
narrative_ontology:cs_drift_state('fd87d9cd-9641-403d-a4a8-33df71da77d8', post_storage_cost_decline_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd87d9cd-9641-403d-a4a8-33df71da77d8', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_incumbents).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_vendor_consortiums).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_planning_agencies).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, capital_markets_intermediaries).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers_in_stranded_asset_regions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fast_deployment_advocates).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, technology_neutrality_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, portfolio_diversification_reduces_systemic_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets integrated resource plans that mandate a mix of nuclear, wind, solar, and storage rather than optimizing for a single technology's cost curve. Justifies the mandate as risk management against any single technology's supply chain, financing, or performance failure. Administers procurement rules and interconnection queues that operationalize the mix.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_planning_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Hold generation assets across nuclear, gas, and renewables and profit from rate structures that guarantee returns on a diversified capital stack. A technology-neutral mandate locks in demand for whichever assets they already own or plan to build, insulating them from a scenario where regulators or markets pick one winning technology that they don't hold.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_incumbents, beneficiary,
    institutional, generational, arbitrage, national).

% Depend on the portfolio framing to keep nuclear procurement politically viable despite higher levelized costs and long construction timelines relative to renewables. Without a technology-neutral mandate, competitive-bid processes optimizing purely on near-term cost would likely exclude them. The 'balanced portfolio' framing guarantees them a seat regardless of comparative cost performance.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_vendor_consortiums, beneficiary,
    organized, civilizational, constrained, global).

% Structure financing vehicles across multiple technology classes and collect fees on complexity. A mandated multi-technology portfolio produces more distinct financial products, more advisory mandates, and more risk-tranching opportunities than a single-technology build-out would.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, capital_markets_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Firms specializing purely in wind, solar, or storage face procurement caps and set-aside quotas reserved for nuclear or 'firm' capacity regardless of their own technology's demonstrated cost and deployment speed advantage in a given region. They can lobby against the caps or relocate to jurisdictions without technology-neutral mandates, but within the mandated market their addressable demand is structurally capped below what unconstrained competition would award them.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_technology_developers, payer,
    moderate, biographical, constrained, national).

% Pay for nuclear cost overruns and construction delays embedded in the portfolio mandate's requirement that a jurisdiction maintain 'baseload diversity' even where local renewable-plus-storage costs have fallen well below nuclear's levelized cost. They cannot choose a different regional grid operator and bear the rate impact of assets whose necessity is asserted rather than locally demonstrated.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers_in_stranded_asset_regions, payer,
    powerless, biographical, trapped, regional).

% Argue that near-term emissions reduction is time-critical and that mandating slow-to-build nuclear alongside renewables delays overall decarbonization by tying planning and interconnection capacity to the slowest-maturing technology in the portfolio. Their objection is raised in public comment processes but rarely changes the procurement mix once codified in integrated resource plans.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fast_deployment_advocates, excluded,
    moderate, biographical, mobile, national).

% Model decarbonization pathways under varying regional resource endowments, grid topologies, and cost trajectories. Some modeling supports technology-neutral portfolios as genuinely robust to uncertainty; other modeling shows renewable-dominant pathways achieving equivalent or faster decarbonization at lower cost in most but not all regions. They supply the evidence base that both the pragmatism reading and its rival readings selectively cite.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists_and_systems_modelers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_incumbents).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation and grid planning across multiple generation technologies so that decarbonization is not derailed by a single technology's supply chain shock, financing collapse, siting failure, or performance shortfall — genuine portfolio-theory logic applied to physical infrastructure under long time horizons.
% TRANSFER_FUNCTION: Moves procurement guarantees and rate-recovery certainty from whichever technology would win under unconstrained cost competition to a fixed multi-technology allocation; in practice this transfers ratepayer money toward higher-cost, slower-to-deploy nuclear capacity in jurisdictions where renewables-plus-storage would otherwise have captured a larger share, and transfers market access away from single-technology developers regardless of their local cost advantage.
% ABSENT_VOICES: Fast deployment advocates and pure least-cost-per-ton analysts are present in comment proceedings but structurally outweighed once integrated resource plans encode fixed technology set-asides; ratepayers in specific stranded-asset regions have essentially no voice in the initial portfolio design decision, which is typically made at a state or national planning level distant from local cost realities.
% DISAPPEARANCE_RATIONALE: Proponents (grid agencies, nuclear vendors, diversified utilities) argue the world would rearrange badly — supply chain shocks or storage-cost stagnation would leave decarbonization exposed to a single point of failure. Opponents (single-technology developers, some ratepayer advocates, fast-deployment advocates) argue that regional least-cost competition would simply reallocate capital toward whichever technologies are actually winning in that region, and that decarbonization would proceed at least as fast, at lower cost, without the mandate. Both positions are held by parties with a stake in the answer, and the underlying empirical question (how much diversification value is real vs. rent-preserving) remains unresolved by the modeling literature.
% FOUNDING_PROBLEM: In the mid-2010s to early-2020s, several jurisdictions that bet heavily on a single decarbonization technology encountered problems: renewable-heavy grids without adequate storage or transmission faced reliability events, and nuclear-heavy jurisdictions faced construction cost overruns and public opposition. The portfolio approach was built to hedge against correlated technology-specific failure modes.
% FOUNDING_PROBLEM_CORROBORATION: Independent systems-reliability researchers and some grid operators outside the nuclear and utility beneficiary set corroborate that genuine diversification value exists under specific conditions (limited transmission buildout, immature storage economics). However, independent techno-economic analyses from academic energy-systems groups not aligned with either nuclear or renewables industry associations increasingly find that in many regions the marginal diversification benefit is smaller than the cost premium the mandate imposes, suggesting the founding problem has partially resolved via storage cost declines and transmission expansion in ways the mandate has not been re-tested against.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 by interval end) — this reading genuinely coordinates against correlated-failure risk, but the diversification requirement has been used to preserve nuclear's market share against cost trends more than pure risk-hedging would justify, and this has grown over time as storage costs fell without proportional adjustment to the mandated mix. Suppression is moderate (0.38): single-technology developers are not blocked outright but face structural set-asides and interconnection queue rules that function as soft exclusion. Theater ratio (0.31) reflects that some 'technology-neutral' planning language increasingly substitutes for the harder regional-specific optimization the pragmatism reading actually calls for — agencies invoke 'balance' as a planning shortcut rather than doing the region-specific analysis the reading's own logic requires. Accessibility collapse is moderate (0.40): alternative pure-least-cost planning approaches exist and are used in some jurisdictions, so the mandate has not fully foreclosed alternatives. Resistance is elevated (0.55) because single-technology developers and fast-deployment advocates actively contest the set-asides in regulatory proceedings.
 *
 * PERSPECTIVAL GAP:
 *   From the grid planning agency's seat, the portfolio mandate is prudent risk management — a rope solving a genuine coordination problem under deep technological uncertainty. From the single-technology developer's seat with constrained exit, the same rule computes as extraction: a guaranteed allocation to a higher-cost incumbent technology that a fair competitive process would not sustain. The engine's per-seat computation should surface this divergence rather than resolve it in either direction — that divergence is the diagnostic signal this reading exists to make visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid planning agencies sit as agenda-setters with analytical exit — they administer the rule but bear no direct extraction themselves. Diversified utility incumbents and nuclear vendor consortiums are structural beneficiaries: the mandate guarantees them market share independent of comparative technology performance, pushing their derived directionality toward the beneficiary end. Single-technology developers and ratepayers in stranded-asset regions are structural targets: developers are capped below their competitive potential, and ratepayers are trapped bearing costs of a diversity requirement they did not choose and cannot exit regionally. Capital markets intermediaries are beneficiaries through fee complexity rather than through the underlying technology outcome — their directionality reflects capture of process, not capture of physical infrastructure returns.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (correlated technology-specific failure risk) was real circa the mid-2010s when storage economics and transmission buildout were both immature. Storage costs have since fallen substantially and transmission expansion has partially addressed the intermittency concern in several regions, which weakens the original justification for fixed technology set-asides in those regions specifically — while leaving it intact in regions where storage/transmission remain undeveloped. This is a genealogy-status of 'contested' rather than 'dead': the mandate's justification has eroded unevenly by region, and re-litigating the mix region-by-region (rather than treating the national portfolio mandate as settled) is exactly what the pragmatism reading's own logic calls for but institutionally resists doing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversification_value_vs_rent_preservation,
    'How much of the observed extraction (0.42) reflects genuine portfolio-diversification value against correlated technology failure, versus rent preservation for incumbent nuclear and diversified-utility capital that would not survive unconstrained least-cost competition?',
    'Region-specific counterfactual modeling: compare realized system cost and reliability outcomes in jurisdictions with technology-neutral mandates against comparable jurisdictions using pure least-cost competitive procurement, controlling for resource endowment and starting grid topology.',
    'If diversification value dominates, this reading is closer to a genuine Rope with modest coordination overhead; if rent preservation dominates, the tangled_rope classification understates the extractive share and the constraint drifts toward snare-like dynamics in regions where the cost gap is largest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversification_value_vs_rent_preservation, empirical, 'Whether measured extraction reflects real hedging value or captured incumbency.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the portfolio_pragmatism_reading''s disagreement with baseload_necessity_reading and renewable_primacy_reading actually sit — is it a genuine empirical disagreement about cost/reliability trajectories, or a disagreement about which uncertainty (technology risk vs. climate timeline risk) should be weighted more heavily in the planning objective function?',
    'Decompose each reading''s underlying planning model to isolate whether the divergence traces to different empirical cost/performance forecasts (resolvable by better data) or to different risk-weighting axioms (a values disagreement not resolvable by data alone).',
    'If the disagreement is empirical, converging cost data over time should collapse the readings toward one another; if it is a risk-weighting/values disagreement, the readings will persist as coexisting positions indefinitely regardless of data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel contest is empirically resolvable or a persistent values disagreement.').

omega_variable(
    regional_variation_authenticity,
    'Is the reading''s claim of ''regional variation in optimal mix'' being genuinely operationalized region-by-region, or is it a rhetorical concession used to justify a de facto fixed national mandate that is never actually re-optimized per region?',
    'Audit a sample of jurisdictions'' integrated resource plans over a decade: track whether the nuclear/renewable allocation ratio actually shifts as regional cost and resource data update, or remains static despite updated inputs.',
    'If allocations are static despite updated regional data, the theater_ratio is understated and the ''technology-neutral'' framing functions more as institutional cover than as the adaptive optimization the reading claims to perform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variation_authenticity, empirical, 'Whether claimed regional adaptivity is real or performative.').


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
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.31).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the climate_mitigation_legitimacy kernel, decomposed per the ε-invariance principle: the colloquial label 'optimal decarbonization strategy' conflates at least four structurally distinct claims with different beneficiary/victim structures and different ε values. portfolio_pragmatism_reading occupies the structural middle: unlike baseload_necessity_reading (which forecloses pure-renewable pathways as insufficient) or renewable_primacy_reading (which forecloses nuclear as unnecessary), this reading coexists with both by design — it is definitionally the position that refuses to foreclose either technology. It influences degrowth_sufficiency_reading by keeping the debate anchored in a generation-expansion framing that degrowth_sufficiency_reading rejects outright.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
