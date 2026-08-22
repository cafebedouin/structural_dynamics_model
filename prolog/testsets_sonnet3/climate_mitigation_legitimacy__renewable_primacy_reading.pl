% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable-Primacy Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested
 *   climate_mitigation_legitimacy kernel: the claim that renewables plus
 *   storage can achieve full decarbonization faster and cheaper than nuclear,
 *   and that this comparative advantage should govern capital allocation,
 *   permitting priority, and public legitimacy. As policy discourse and
 *   financing structures adopt this framing, it functions as tangled rope: it
 *   coordinates a genuine and urgent decarbonization deployment pathway (real
 *   coordination function — gigawatt-scale renewable buildout within relevant
 *   climate timeframes) while simultaneously and systematically starving
 *   nuclear capital projects of financing and legitimacy, treating them as
 *   capital sinks. The coordination is real; so is the asymmetric cost borne
 *   by nuclear-dependent workforces, communities, and utilities. Sibling
 *   readings (baseload_necessity_reading, portfolio_pragmatism_reading,
 *   degrowth_sufficiency_reading) are NOT part of this story — each is a
 *   separate constraint with its own ε and stakeholder structure, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - renewable_developers: primary beneficiary (organized/arbitrage) — captures financing and permitting priority
 *   - incumbent_nuclear_utilities: primary target (institutional/trapped) — capital projects delegitimized as slow and costly
 *   - nuclear_construction_workforce: secondary target (powerless/constrained) — bears employment discontinuity
 *   - climate_advocacy_organizations_aligned_with_speed_framing: agenda_setter — sets terms of the legitimacy contest
 *   - energy_systems_analysts: analytical observer — sees full-system cost structure across pathways
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
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable-Primacy Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, 'c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f').
narrative_ontology:cs_kernel_codification('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', distributed).
narrative_ontology:cs_authority_grounding('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', distributed).
narrative_ontology:cs_reading_relation('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', foundational, deployment_speed_dominates_legitimacy_criterion).
narrative_ontology:cs_axiom_status(deployment_speed_dominates_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', deployment_speed_dominates_legitimacy_criterion, empirically_contingent).
narrative_ontology:cs_axiom('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', secondary, capital_cycle_length_determines_technology_priority).
narrative_ontology:cs_axiom_status(capital_cycle_length_determines_technology_priority, holdable).
narrative_ontology:cs_axiom_grounding('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', capital_cycle_length_determines_technology_priority, instrumental).
narrative_ontology:cs_reference_frame('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', pre_paris_agreement_diversified_generation_planning).
narrative_ontology:cs_drift_state('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', post_2020_cost_curve_divergence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c0c62660-6e18-4f0e-89b3-7bafdf9b2c4f', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, grid_scale_solar_wind_financiers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations_aligned_with_speed_framing).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, incumbent_nuclear_utilities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_construction_workforce).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, communities_hosting_stranded_nuclear_projects).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_facing_intermittency_costs).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, levelized_cost_of_energy_renewable_advantage).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, capital_cycle_speed_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build wind, solar, and storage projects that benefit directly from policy frameworks, subsidies, and interconnection priority premised on the renewable-primacy reading. Their financing model depends on short construction timelines and modular capital cycles that this reading privileges over nuclear's long-horizon capital commitments.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_developers, beneficiary,
    organized, biographical, arbitrage, national).

% Supply the storage component that makes the renewable-primacy claim technically plausible. Demand for their product scales directly with policy adoption of this reading; they have strong incentive to fund advocacy reinforcing it.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, arbitrage, global).

% Deploy capital into renewable projects at scale, benefiting from planning and regulatory environments that treat renewables-plus-storage as the presumptively correct decarbonization pathway. Capital is portable across jurisdictions that adopt this framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_scale_solar_wind_financiers, beneficiary,
    institutional, biographical, mobile, global).

% Campaign for policy and public opinion frameworks that treat speed-to-deployment as the dominant decarbonization criterion, which structurally favors renewables over nuclear's longer lead times. They set the terms of the legitimacy contest in media and legislative fora, and some receive funding tied to renewable-sector success.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations_aligned_with_speed_framing, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations_aligned_with_speed_framing, beneficiary).

% Operate or seek to build nuclear capacity with multi-decade capital cycles. Under the renewable-primacy framing, their projects are recast as capital sinks that delay decarbonization, undermining access to financing, licensing priority, and public legitimacy regardless of individual project merit. Sunk capital cannot be redeployed quickly.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, incumbent_nuclear_utilities, payer,
    institutional, civilizational, trapped, national).

% Skilled tradespeople and engineers whose careers depend on nuclear construction pipelines. As the renewable-primacy reading gains policy traction and nuclear projects are cancelled or delayed, this workforce faces employment discontinuity with limited transferability of specialized skills to renewable construction at equivalent wages.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_construction_workforce, payer,
    powerless, biographical, constrained, regional).

% Towns that planned tax bases, schools, and local economies around long-term nuclear plant operation. When projects are cancelled or plants prematurely retired under renewable-primacy-influenced policy, these communities absorb stranded infrastructure costs and lost economic anchors with no comparable replacement offered.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, communities_hosting_stranded_nuclear_projects, payer,
    powerless, generational, trapped, local).

% Manage real-time balancing of variable renewable generation, absorbing costs of curtailment, reserve capacity, and transmission buildout that are often excluded from renewable levelized-cost comparisons used to justify the renewable-primacy framing. They bear operational costs the advocacy framing does not fully price.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_facing_intermittency_costs, payer,
    institutional, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_facing_intermittency_costs, observer).

% Engineers and policy analysts arguing dispatchable baseload is structurally required at scale. Within jurisdictions where the renewable-primacy reading dominates planning discourse, their technical objections are treated as either bad-faith incumbency defense or resolved by future storage breakthroughs not yet demonstrated at grid scale.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_necessity_advocates, excluded,
    organized, generational, constrained, national).

% Model full-system costs including firming, transmission, storage duration limits, and land use across competing decarbonization pathways. Their findings are cited selectively by all sides of the kernel contest depending on which variables are foregrounded.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_systems_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation, permitting priority, and public narrative around a decarbonization pathway that can plausibly deploy gigawatts of clean capacity within single-digit-year timeframes, addressing the genuine urgency of near-term emissions reduction targets.
% TRANSFER_FUNCTION: Moves financing access, regulatory priority, and public legitimacy away from nuclear capital projects and toward renewable-plus-storage capital projects, on the premise that speed and levelized cost favor the latter; also moves employment and regional economic stability away from nuclear-hosting communities and workforces.
% ABSENT_VOICES: Baseload-necessity engineers and grid reliability specialists who argue firm capacity is being systematically underweighted in current levelized-cost comparisons are present in technical literature but structurally excluded from the political and media framing that adopts speed-to-deployment as the dominant legitimacy criterion.
% DISAPPEARANCE_RATIONALE: If the renewable-primacy framing lost its grip on policy and financing decisions overnight, nuclear projects currently starved of capital and licensing priority would become financeable again, some cancelled projects could be revived, and the capital-cycle-speed argument would lose its power to redirect subsidy and permitting resources — utility investment portfolios and interconnection queues would visibly reorganize.
% FOUNDING_PROBLEM: The founding problem was the observed slowness and high capital cost of new nuclear construction relative to the shrinking window for meeting emissions targets — advocates needed a legitimacy framework that could justify prioritizing technologies deployable within the relevant political and climate timeframe.
% FOUNDING_PROBLEM_CORROBORATION: Independent energy-systems analysts (a seat with no direct stake in either renewable or nuclear capital flows) corroborate that renewable deployment speed and falling storage costs are real and significant, supporting part of the founding problem's continued relevance; the same analysts, however, note that full-system firming costs are frequently excluded from the comparisons used to declare the problem solved, which is contested by grid operators and baseload-necessity advocates from outside the renewable-development beneficiary set.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that the renewable-primacy framing, once embedded in financing and permitting decisions, redirects real capital and legitimacy away from nuclear projects regardless of individual project merit — this is asymmetric cost imposed through the same structure that coordinates genuine renewable deployment. Suppression (0.42) is moderate: baseload-necessity technical objections are not banned, but are structurally marginalized in policy and media framing that has adopted speed-to-deployment as the dominant criterion. Theater ratio (0.28) is modest-low because the coordination function (fast, real gigawatt deployment) is substantially genuine, not primarily performative. Accessibility collapse (0.35) is moderate — alternative technology pathways remain technically and legally available, just financially and politically disadvantaged. Resistance (0.62) is substantial: nuclear industry, baseload engineers, and some grid operators actively contest the framing in technical and policy fora.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable-developer and advocacy seats, this reading is a straightforward coordination success: capital and policy converging on the fastest deployable decarbonization pathway. From the nuclear-utility and host-community seats, the identical structure operates as enforced delegitimization — their projects are recast as illegitimate capital sinks by the same narrative and financing apparatus that claims to be solving a shared climate problem. The engine computes this divergence from the beneficiary/victim structure; the claimed_type does not resolve it in advance.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers, storage manufacturers, and aligned financiers sit near the beneficiary end of directionality — the framing directly subsidizes their access to capital and regulatory priority. Incumbent nuclear utilities, construction workforces, and host communities sit near the target end — the framing extracts legitimacy and financing access from them through the same policy and narrative structures that coordinate renewable deployment. Grid operators occupy an intermediate position: they benefit from decarbonization progress but absorb underpriced intermittency costs the framing's cost comparisons often exclude.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — nuclear's slow, capital-intensive construction relative to a shrinking emissions-reduction window — remains partially live (renewable deployment speed is empirically real), which is why founding_problem_status is 'contested' rather than 'dead.' This prevents mislabeling the constraint as pure extraction: there is a genuine coordination function being solved. But the framing has also hardened into a legitimacy gate that forecloses nuclear financing even in contexts where full-system firming costs would favor a mixed portfolio — this is where the tangled-rope classification, rather than a clean rope classification, is structurally required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    full_system_cost_comparison_scope,
    'Do levelized-cost comparisons underpinning the renewable-primacy claim adequately price firming, transmission buildout, and storage-duration limits, or do they systematically exclude costs that would favor a mixed portfolio?',
    'Independent full-system cost modeling (not LCOE alone) comparing decarbonization pathways at matched reliability standards across multiple grid geographies and climates.',
    'If full-system costs favor renewables-plus-storage even after firming costs are included, the coordination function dominates and extraction is lower than authored; if firming costs substantially close or reverse the gap, the extraction from nuclear delegitimization is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(full_system_cost_comparison_scope, empirical, 'Whether renewable-primacy cost claims are apples-to-apples with firm-capacity alternatives.').

omega_variable(
    capital_cycle_speed_as_legitimacy_criterion,
    'Is capital-cycle speed a legitimate primary criterion for decarbonization pathway selection, or does privileging speed structurally bias against any technology with long lead times regardless of eventual system value?',
    'Comparative policy analysis of jurisdictions weighting speed heavily versus those using multi-criteria portfolio frameworks, tracking long-run system cost and reliability outcomes.',
    'If speed-primacy is shown to systematically underweight long-run value, the renewable-primacy reading''s legitimacy claim weakens relative to portfolio_pragmatism_reading; if speed genuinely dominates under realistic climate deadlines, the reading''s coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_cycle_speed_as_legitimacy_criterion, conceptual, 'Whether prioritizing deployment speed as the legitimacy criterion is itself defensible or a framing choice favoring one technology class.').

omega_variable(
    storage_scaling_uncertainty,
    'Will grid-scale, long-duration storage technology mature quickly enough and cheaply enough to fully substitute for dispatchable baseload at the scale this reading requires?',
    'Track storage cost curves, duration capability, and deployment rates against the multi-decade timelines the reading implicitly assumes; compare to historical technology learning-curve precedents.',
    'If storage scaling stalls short of full substitution, the reading''s central technical premise weakens and the victim classification of nuclear utilities as unnecessary capital sinks becomes harder to sustain; if storage scales as projected, the reading''s technical premise strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(storage_scaling_uncertainty, empirical, 'Whether the technological premise underlying renewable-primacy''s cost and speed claims will hold over the relevant timeframe.').


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
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 4, 0.31).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the climate_mitigation_legitimacy kernel, each authored as a separate constraint with its own ε, stakeholders, and classification per the ε-invariance principle. The renewable_primacy_reading treats nuclear capital projects as delaying decarbonization (victim set includes incumbent nuclear utilities and their host communities/workforce). The baseload_necessity_reading inverts this, treating renewable intermittency without adequate firming as the coordination failure and nuclear exclusion as the extraction. The portfolio_pragmatism_reading treats technology-exclusive framing itself (in either direction) as the extractive move, favoring both. The degrowth_sufficiency_reading treats the entire generation-expansion race as a category error, positioning both renewable-primacy and nuclear-necessity advocates as beneficiaries of an unnecessary capital arms race. All four should be read as siblings, not as convergent measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
