% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel over what
 *   decarbonization legitimately requires: the claim that reliable, deep
 *   decarbonization is structurally impossible without dispatchable baseload
 *   capacity that variable renewables cannot supply at the necessary scale
 *   and cost. Under this reading, nuclear power (and to a lesser degree
 *   gas-with-carbon-capture and other firm thermal sources) enters as
 *   necessary infrastructure rather than one option among several,
 *   renewable-only pathways are classified as technically inadequate rather
 *   than merely different, and capital is directed toward long-lived,
 *   capital-intensive assets whose multi-decade payback periods and
 *   stranded-asset risk are treated as acceptable costs of reliability. As
 *   with the BGS decomposition pattern, this is not 'the truth about
 *   decarbonization' assessed from a neutral standpoint — it is the
 *   constraint that exists when this specific reading of the underlying
 *   contest is operationalized into capacity-market rules, loan guarantee
 *   programs, and reliability standards. The sibling readings
 *   (renewable-primacy, portfolio-pragmatism, degrowth-sufficiency) are
 *   separate constraints with their own ε, beneficiary sets, and structural
 *   profiles, linked here only by network reference — this file does not
 *   average across them or hedge the extraction level to split the
 *   difference.
 *
 * KEY AGENTS:
 *   - nuclear_utility_operators: institutional beneficiary and agenda-setter — collects capacity payments and subsidy eligibility premised on the necessity claim
 *   - reactor_vendors_and_engineering_firms: institutional beneficiary — order books depend on baseload framing hardening into policy
 *   - grid_reliability_authorities: institutional agenda-setter — certifies what counts as adequate capacity, the actual enforcement mechanism
 *   - distributed_solar_and_wind_developers: moderate-power payer — bears curtailment and market-disqualification costs
 *   - ratepayers_in_high_capex_nuclear_regions: powerless, trapped payer — absorbs cost overruns with no exit
 *   - next_generation_storage_and_demand_response_firms: excluded payer — technologies structurally locked out of reliability-standard definitions
 *   - climate_policy_analysts: analytical observer — sees the full contested structure across all four kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '97329bce-0469-4690-b5e6-8b107b25e697').
narrative_ontology:cs_kernel_codification('97329bce-0469-4690-b5e6-8b107b25e697', distributed).
narrative_ontology:cs_authority_grounding('97329bce-0469-4690-b5e6-8b107b25e697', distributed).
narrative_ontology:cs_reading_relation('97329bce-0469-4690-b5e6-8b107b25e697', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('97329bce-0469-4690-b5e6-8b107b25e697', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('97329bce-0469-4690-b5e6-8b107b25e697', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('97329bce-0469-4690-b5e6-8b107b25e697', foundational, dispatchable_firm_capacity_is_technically_irreplaceable).
narrative_ontology:cs_axiom_status(dispatchable_firm_capacity_is_technically_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('97329bce-0469-4690-b5e6-8b107b25e697', dispatchable_firm_capacity_is_technically_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('97329bce-0469-4690-b5e6-8b107b25e697', secondary, long_lived_capital_intensive_assets_justify_present_cost_for_future_reliability).
narrative_ontology:cs_axiom_status(long_lived_capital_intensive_assets_justify_present_cost_for_future_reliability, holdable).
narrative_ontology:cs_axiom_grounding('97329bce-0469-4690-b5e6-8b107b25e697', long_lived_capital_intensive_assets_justify_present_cost_for_future_reliability, instrumental).
narrative_ontology:cs_reference_frame('97329bce-0469-4690-b5e6-8b107b25e697', thermal_plant_grid_reliability_paradigm).
narrative_ontology:cs_drift_state('97329bce-0469-4690-b5e6-8b107b25e697', post_storage_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97329bce-0469-4690-b5e6-8b107b25e697', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_utility_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, reactor_vendors_and_engineering_firms).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_authorities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, capital_intensive_incumbent_generators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_solar_and_wind_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_in_high_capex_nuclear_regions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, communities_near_proposed_reactor_sites).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, next_generation_storage_and_demand_response_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing reactor fleets and lobby regulators and legislators to classify baseload dispatchability as an indispensable requirement of any credible decarbonization pathway. Collect long-term capacity payments, favorable rate-basing, and public loan guarantees premised on this framing. Positioned to expand fleet life and new-build subsidy eligibility as the framing hardens into policy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_utility_operators, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_utility_operators, agenda_setter).

% Sell reactor designs, construction contracts, and multi-decade service agreements. Benefit directly from any policy environment where baseload necessity is treated as settled fact rather than contested technical claim, since this channels public capital and loan guarantees toward their order books.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, reactor_vendors_and_engineering_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Set interconnection standards, capacity market rules, and reliability criteria that embed dispatchability assumptions derived from thermal-plant-era grid modeling. Their authority to certify what counts as 'reliable' capacity is the enforcement mechanism that makes the baseload framing operative in procurement and permitting decisions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Own coal and gas plants being repositioned as 'bridge' or 'firming' capacity within a baseload-necessity framework, extending their operating licenses and cost recovery under the umbrella of reliability arguments that also favor nuclear.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, capital_intensive_incumbent_generators, beneficiary,
    organized, generational, constrained, national).

% Face curtailment rules, interconnection queues, and capacity-market disqualification premised on the claim that variable generation cannot substitute for baseload. Absorb the cost of grid studies and firming requirements justified by dispatchability doctrine, even where storage and demand-response alternatives are commercially available.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_solar_and_wind_developers, payer,
    moderate, biographical, constrained, national).

% Bear cost overruns and rate increases from new nuclear construction justified by baseload-necessity arguments, with no ability to opt out of the regulated utility's capital plan or to reallocate that capital toward cheaper alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_in_high_capex_nuclear_regions, payer,
    powerless, biographical, trapped, regional).

% Live with siting decisions, waste storage, and long construction timelines imposed because the baseload framing forecloses siting debate about whether the capacity is needed at all. Limited standing in reliability-authority proceedings that already assume the necessity premise.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, communities_near_proposed_reactor_sites, payer,
    powerless, generational, trapped, local).

% Offer technologies (long-duration storage, demand response, transmission expansion) that could substitute for dispatchable baseload but are structurally excluded from capacity-market and reliability-standard definitions built around thermal plant characteristics, limiting their addressable market regardless of cost competitiveness.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, next_generation_storage_and_demand_response_firms, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, next_generation_storage_and_demand_response_firms, excluded).

% Model decarbonization pathways under varying assumptions about storage cost curves, demand flexibility, and nuclear build timelines. Their findings feed all four kernel readings without adjudicating among them, since the underlying disagreement is partly about contested empirical trajectories (future storage costs, reactor construction learning curves) and partly about risk tolerance for stranded-asset or blackout scenarios.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-horizon capital allocation and grid-reliability planning around a shared technical premise: that firm, dispatchable generation must anchor the grid because variable renewable output cannot be fully substituted by storage and demand flexibility at the scale and cost needed within relevant decarbonization timelines. This genuinely solves a real planning problem — utilities, regulators, and financiers need a stable premise to commit multi-decade capital.
% TRANSFER_FUNCTION: Moves public loan guarantees, rate-based capital recovery, and capacity-market revenue toward nuclear operators, reactor vendors, and incumbent thermal generators; moves interconnection costs, curtailment risk, and market disqualification onto variable-renewable developers and emerging storage/demand-response firms; moves cost-overrun risk and siting burden onto ratepayers and host communities.
% ABSENT_VOICES: Storage and demand-response firms whose commercial trajectories directly contradict the necessity premise are structurally excluded from the reliability-standard-setting processes that define what counts as adequate capacity; host communities near proposed reactor sites have limited standing once regulators treat the necessity question as resolved rather than open for siting-stage debate.
% DISAPPEARANCE_RATIONALE: If the baseload-necessity premise were abandoned in policy and capital-markets contexts, capacity-market rules would need to be rewritten to credit storage and demand response on equal footing, nuclear subsidy programs would lose their primary justification, incumbent thermal 'bridge' framing would collapse, and billions in planned nuclear capital commitments would face immediate scrutiny — the reallocation of capital and regulatory standing would be substantial and fast.
% FOUNDING_PROBLEM: Electricity grids historically required continuously available generation matched to demand because storage was prohibitively expensive and renewable output was intermittent and largely unpredictable; without dispatchable plants, blackouts and grid instability were genuine risks.
% FOUNDING_PROBLEM_CORROBORATION: Grid engineers and reliability authorities (including entities with no nuclear financial stake, such as some independent system operators) attest the underlying reliability problem remains partially live under current storage costs and penetration levels. However, renewable-industry analysts, several national grid studies (e.g. jurisdictions running >60% variable renewables with adequate reliability), and storage-cost trend data from outside the nuclear industry corroborate that the specific claim — that only nuclear-style dispatchable baseload can solve it — is empirically contested rather than settled; the necessity framing as currently operationalized in capacity markets outpaces what the corroborating technical evidence supports.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the necessity premise, once operationalized into capacity-market and reliability-standard rules, channels substantial public capital and cost-recovery guarantees toward incumbent nuclear and thermal generators while imposing real costs (curtailment, market exclusion, siting burden) on renewable developers, storage firms, and host communities — costs that a portfolio-pragmatism or renewable-primacy reading would not impose at this level, because the same underlying grid would be planned differently. Suppression (0.52) is moderate: the framing is enforced through standard-setting authority and subsidy eligibility criteria rather than direct coercion, but it does foreclose genuine alternatives from formal consideration in reliability proceedings. Theater ratio (0.28) is present but not dominant — real reliability engineering work occurs alongside the doctrinal use of 'baseload necessity' to justify capital allocation decisions that outrun the technical consensus. Accessibility collapse (0.45) is moderate-low because alternative pathways (storage, demand response, transmission) remain commercially visible and increasingly cost-competitive even as they are excluded from formal standards. Resistance (0.62) is substantial: renewable developers, storage firms, and increasingly some grid operators actively contest the necessity claim with competing technical evidence, which is precisely why this is authored as a contested kernel rather than settled fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear utility operators, reactor vendors, and incumbent thermal generators sit near the full-beneficiary end: the necessity framing directly produces their revenue streams and regulatory standing, and their exit options (arbitrage across jurisdictions and subsidy programs) reinforce this. Grid reliability authorities are agenda-setters whose institutional exit is constrained — they are structurally bound to the standards they administer even if they did not originate the necessity claim. Distributed renewable and storage developers are targets: the same standard-setting apparatus that pays incumbents excludes or disqualifies them, and their exit options are constrained by dependence on the same grid and market rules. Ratepayers and host communities sit at the extreme target end: trapped, powerless, bearing costs from decisions made under a premise they cannot contest in the relevant proceedings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — grids historically needed continuously available generation because storage was prohibitively expensive — was genuinely live for most of the 20th century. Whether it remains live today is exactly the contested question this reading answers in the affirmative while the renewable-primacy reading answers in the negative, based on differing empirical weight given to recent storage-cost declines and grid operation data from high-renewable-penetration jurisdictions. Classifying this as tangled_rope rather than snare or mountain acknowledges that a genuine coordination problem (grid reliability planning requires SOME stable technical premise) persists even as the specific operationalized premise (only nuclear-style dispatchable baseload solves it) has hardened past what the corroborating evidence uniformly supports, and is actively defended by parties who profit from that specific resolution of the underlying technical uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    storage_cost_trajectory_uncertainty,
    'Will long-duration storage and demand-response technologies reach cost and scale parity with dispatchable thermal generation within the relevant decarbonization timeline (roughly 2030-2050), such that the baseload-necessity premise becomes empirically false even by its own terms?',
    'Track realized (not projected) cost curves for grid-scale long-duration storage, and observe reliability outcomes in jurisdictions operating at sustained high (>70%) variable renewable penetration over multi-year periods.',
    'If storage/demand-response costs converge with or undercut new nuclear build costs before large nuclear fleets are financed, the necessity premise loses its empirical grounding and the beneficiary structure built on it becomes harder to justify as coordination rather than pure extraction — pushing this reading''s classification toward snare. If the convergence does not occur, the coordination function is better supported and the tangled_rope classification (genuine problem plus asymmetric capture) remains apt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_cost_trajectory_uncertainty, empirical, 'Whether the technical premise underlying this reading will hold or collapse under future cost/performance data.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice to operationalize the baseload-necessity reading into binding capacity-market and reliability standards (rather than the portfolio-pragmatism or renewable-primacy readings) driven by genuine technical assessment, or by the structural power of incumbent nuclear and thermal generators to shape the standard-setting bodies that adjudicate the underlying contest?',
    'Compare regulatory outcomes across jurisdictions with differing incumbent generation mixes and lobbying capacity, controlling for underlying grid characteristics (renewable resource quality, existing transmission capacity, storage deployment rates), to isolate whether reading-selection correlates with incumbent political power rather than technical grid need.',
    'If reading-selection tracks incumbent lobbying capacity more than technical necessity, this constraint''s coordination story is substantially cover for capture, supporting reclassification toward snare in jurisdictions where the pattern is strongest; if reading-selection tracks independently-verified grid technical studies, the tangled_rope classification (real coordination need, asymmetric benefit distribution) is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this kernel reading''s dominance in a given jurisdiction''s policy is technically or politically determined.').

omega_variable(
    stranded_asset_risk_allocation,
    'If the baseload-necessity premise is later abandoned or substantially revised (e.g., due to storage cost declines), who bears the cost of stranded nuclear and thermal assets built under this reading''s justification — ratepayers, taxpayers, or the incumbent operators who benefited from the initial framing?',
    'Track cost-recovery mechanisms and loan-guarantee terms in currently financed nuclear projects to determine whether risk is structured to fall on public balance sheets or on private equity/operator balance sheets.',
    'If stranded-asset risk is structured to fall on ratepayers/taxpayers regardless of outcome, this substantially strengthens the extraction reading (beneficiaries capture upside, victims bear downside risk symmetrically); if risk is genuinely shared or borne by operators, the coordination framing is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stranded_asset_risk_allocation, empirical, 'Whether downside risk from this reading''s capital allocation choices is symmetrically or asymmetrically distributed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__baseload_necessity_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the climate_mitigation_legitimacy kernel, decomposed per the ε-invariance principle because the underlying natural-language claim ('what decarbonization requires') covers structurally distinct positions with different ε, different beneficiary/victim sets, and different classifications. baseload_necessity_reading (this file, tangled_rope, ε=0.58) authors nuclear and incumbent thermal generators as necessary-infrastructure beneficiaries and renewable/storage developers as structurally disadvantaged payers. renewable_primacy_reading is expected to invert much of this beneficiary/victim structure. portfolio_pragmatism_reading is expected to show lower extraction (broader beneficiary distribution, less concentrated capture) as a genuine hedge-based coordination mechanism. degrowth_sufficiency_reading is expected to reject the capital-intensive-generation-expansion premise entirely, producing a very different constraint shape (targeting consumption/production systems rather than generation technology choice). All four are linked bidirectionally via affects_constraints because policy resources, political coalitions, and capital markets are shared and mutually constraining across the readings — a jurisdiction's commitment to one reading directly reduces resources and legitimacy available to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
