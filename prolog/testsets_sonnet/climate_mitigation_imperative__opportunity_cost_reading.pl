% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Mitigation-Speed Imperative: Carbon-per-Dollar-per-Year Opportunity-Cost Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'climate mitigation
 *   imperative' kernel: the opportunity-cost reading, which holds that
 *   because emissions accumulate and warming thresholds are time-sensitive,
 *   the correct allocation criterion for climate capital is carbon avoided
 *   per dollar per year, not total lifetime carbon avoided or system-level
 *   reliability. Under this metric, nuclear's long construction timelines and
 *   high capital intensity make it a comparatively poor allocation target
 *   relative to fast-deploying wind, solar, and storage — nuclear becomes a
 *   victim of the allocation rule even though it is not itself a
 *   climate-harming technology. This is distinct from the
 *   portfolio_optimization_reading (which treats nuclear as necessary
 *   baseload alongside renewables) and the systems_transition_reading (which
 *   treats nuclear as illegitimate for centralization reasons unrelated to
 *   deployment speed). Each reading produces a different victim/beneficiary
 *   structure and a different epsilon; they are not merged here.
 *
 * KEY AGENTS:
 *   - utility_scale_solar_developers: primary beneficiary (organized/arbitrage) — captures reallocated capital
 *   - nuclear_developers: primary target (powerful/trapped) — capital and financing systematically deprioritized
 *   - climate_finance_analysts_favoring_fast_deployment: agenda-setter (institutional/analytical) — administers the allocation metric
 *   - nuclear_supply_chain_workers and host communities: powerless payers bearing concentrated local costs
 *   - future_generations_2060_grid: excluded — bears long-horizon reliability risk with no current seat
 *   - climate_modelers_and_iam_researchers: analytical observers who can show the metric's sensitivity to modeling choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.42).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Mitigation-Speed Imperative: Carbon-per-Dollar-per-Year Opportunity-Cost Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '78477cad-25ef-4304-960b-804d96a34413').
narrative_ontology:cs_kernel_codification('78477cad-25ef-4304-960b-804d96a34413', distributed).
narrative_ontology:cs_authority_grounding('78477cad-25ef-4304-960b-804d96a34413', distributed).
narrative_ontology:cs_reading_relation('78477cad-25ef-4304-960b-804d96a34413', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('78477cad-25ef-4304-960b-804d96a34413', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('78477cad-25ef-4304-960b-804d96a34413', foundational, near_term_deployment_speed_dominates_allocation).
narrative_ontology:cs_axiom_status(near_term_deployment_speed_dominates_allocation, holdable).
narrative_ontology:cs_axiom_grounding('78477cad-25ef-4304-960b-804d96a34413', near_term_deployment_speed_dominates_allocation, empirically_contingent).
narrative_ontology:cs_axiom('78477cad-25ef-4304-960b-804d96a34413', secondary, capital_intensive_long_timeline_projects_are_net_harmful_under_carbon_budget_constraint).
narrative_ontology:cs_axiom_status(capital_intensive_long_timeline_projects_are_net_harmful_under_carbon_budget_constraint, holdable).
narrative_ontology:cs_axiom_grounding('78477cad-25ef-4304-960b-804d96a34413', capital_intensive_long_timeline_projects_are_net_harmful_under_carbon_budget_constraint, instrumental).
narrative_ontology:cs_reference_frame('78477cad-25ef-4304-960b-804d96a34413', carbon_budget_urgency_baseline).
narrative_ontology:cs_drift_state('78477cad-25ef-4304-960b-804d96a34413', post_2020s_ira_and_green_taxonomy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('78477cad-25ef-4304-960b-804d96a34413', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, utility_scale_solar_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, onshore_wind_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, grid_flexibility_service_providers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_finance_analysts_favoring_fast_deployment).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_developers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_supply_chain_workers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, communities_hosting_planned_reactors).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_dependent_grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, advanced_reactor_startups).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, levelized_avoided_carbon_per_dollar_per_year_metric).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, deployment_speed_as_primary_mitigation_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build projects that can be permitted, financed, and interconnected in 1-3 years, so under a carbon-per-dollar-per-year allocation rule they capture a growing share of climate finance and policy support that would otherwise be split with nuclear. Their argument that speed dominates lifetime output is the operative allocation criterion, and it directs capital toward them by construction.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, utility_scale_solar_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Similarly favored by the fast-deployment metric; can site and commission projects far faster than reactors, so the opportunity-cost framing routes subsidy, tax credit, and PPA volume toward them ahead of nuclear proposals competing for the same decarbonization budget.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, onshore_wind_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Benefit indirectly: as the metric pushes capital toward variable renewables, storage becomes the complementary investment needed to firm the grid, expanding their addressable market at nuclear's expense in the same capital pool.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Demand-response and flexibility markets grow as the grid is built around fast, variable renewables rather than nuclear baseload, creating new revenue streams contingent on the allocation rule persisting.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_flexibility_service_providers, beneficiary,
    moderate, biographical, mobile, regional).

% Set and defend the carbon-per-dollar-per-year metric in IPCC-adjacent modeling, philanthropic funding criteria, and multilateral climate finance screens. They administer which projects qualify as 'climate-aligned,' and could revise the metric to weight long-lived firm capacity, but doing so would require reworking models and funding criteria they have already built institutional careers around defending.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_finance_analysts_favoring_fast_deployment, agenda_setter,
    institutional, generational, analytical, global).

% Propose reactors with 6-15 year construction timelines and large upfront capital requirements; under the opportunity-cost allocation rule, their projects are systematically deprioritized or excluded from climate finance vehicles regardless of eventual lifetime carbon-free output, foreclosing projects already underway or shelving new ones. They cannot compress construction timelines to compete on the metric's terms and cannot exit the framing since it now governs sovereign, multilateral, and private climate capital simultaneously.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_developers, payer,
    powerful, civilizational, trapped, national).

% Skilled tradespeople, engineers, and fabricators whose employment depends on a pipeline of reactor construction. As the metric redirects capital away from nuclear, project cancellations and non-starts eliminate multi-year employment horizons with no comparable retraining pathway into renewables construction at the same skill/wage tier.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_supply_chain_workers, payer,
    powerless, biographical, trapped, regional).

% Regions that planned economic development, tax base, and long-term high-wage employment around a reactor project see it cancelled or indefinitely deferred when financing dries up under the opportunity-cost screen, with no equivalent replacement industrial anchor offered.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, communities_hosting_planned_reactors, payer,
    powerless, generational, trapped, local).

% Grid operators in regions with limited hydro or gas backup that were counting on new nuclear for firm decarbonized capacity must now solve reliability with faster-deploying but variable resources plus storage, absorbing higher integration costs and reliability risk that the metric does not price.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_dependent_grid_operators, payer,
    moderate, generational, constrained, regional).

% Small modular and advanced reactor firms betting on shorter future build times face investor skepticism because the current allocation metric penalizes any nuclear-adjacent bet before their claimed timeline compression is demonstrated at scale, starving them of the capital needed to prove the compression.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, advanced_reactor_startups, payer,
    moderate, biographical, constrained, national).

% Would inherit whatever generation mix results from today's allocation choices decades hence; if the fast-deployment bias under-builds firm zero-carbon capacity, they bear the reliability and decarbonization-completion costs, but have no seat in current capital allocation debates.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, future_generations_2060_grid, excluded,
    powerless, civilizational, trapped, global).

% Run integrated assessment models comparing deployment-speed-weighted versus total-capacity-weighted decarbonization pathways; can show scenario sensitivity to the discount rate and time horizon chosen, revealing that the metric's dominance is itself a modeling choice rather than a physical necessity.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_modelers_and_iam_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce climate finance and policy attention toward the projects that remove the most carbon per dollar per year, under real urgency: emissions locked in during the 2020s-2030s compound and cannot be un-emitted later, so speed of avoided-carbon delivery is a genuine physical constraint on cumulative warming, not merely a preference.
% TRANSFER_FUNCTION: Moves capital, subsidy eligibility, and favorable financing terms away from capital-intensive, long-timeline nuclear projects and toward fast-deploying wind, solar, and storage — redirecting billions in public and private climate finance flows and, with them, construction employment, regional economic development, and long-term generation capacity commitments.
% ABSENT_VOICES: Nuclear supply chain workers and host communities bear concentrated local costs from project cancellation but have no representation in the multilateral finance committees and philanthropic funding boards that set eligibility criteria. Future grid operators and consumers of the 2050s-2060s who may face reliability gaps from under-built firm capacity are not present in any current allocation decision.
% DISAPPEARANCE_RATIONALE: Renewables developers and the finance institutions that built portfolios and models around the fast-deployment criterion would see it as a rearrangement — reallocated capital, revised model assumptions, disrupted competitive advantage. Nuclear developers and advocates would say the world 'unchanged' is closer to true in the sense that underlying physics and grid reliability needs don't change; only the allocation framework governing who gets funded would shift, restoring nuclear's eligibility for the same capital pool.
% FOUNDING_PROBLEM: Climate finance and policy needed some allocation rule to prioritize among competing decarbonization technologies under real capital scarcity and a hard emissions-budget deadline; the opportunity-cost/deployment-speed framing was adopted to prevent capital from being tied up for a decade-plus in projects that might not deliver avoided carbon before critical warming thresholds are crossed.
% FOUNDING_PROBLEM_CORROBORATION: Independent energy-systems modelers outside both the renewables and nuclear industries (e.g., IEA and IPCC working-group contributors who model multiple pathways) attest that near-term deployment speed genuinely matters for cumulative emissions, corroborating that the founding problem remains partly live; but the same independent modelers note the metric, once adopted, is now defended by finance institutions whose portfolios and reputations are built on it, and by nuclear-industry critics who argue the framing has hardened past what the physical urgency case alone would justify — corroboration is split, not unanimous, and no source entirely outside all interested camps has adjudicated it.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58 at interval end) reflects real diversion of capital from nuclear toward renewables under a metric that, while grounded in genuine physical urgency, has hardened into an institutional screening criterion defended independent of case-by-case reassessment. Suppression (0.42) is moderate: nuclear projects are not banned, but face systematically unfavorable financing terms, exclusion from green-taxonomy classifications in some jurisdictions, and reputational headwinds that function as soft coercion within capital markets. Theater ratio (0.28) is present but not dominant — the underlying deployment-speed concern is empirically real (near-term emissions do compound), so this is not pure performance, but a growing share of the metric's defense has become institutional self-justification rather than fresh physical analysis. Resistance (0.68) is high because nuclear industry actors, some grid operators, and portfolio_optimization-reading advocates actively contest the metric's dominance in policy and finance venues.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (solar, wind, storage), the constraint reads as pure Rope: legitimate coordination directing scarce capital to where it saves the most carbon fastest, with no injustice since nuclear remains free to compete by demonstrating faster builds. From the payer seats (nuclear developers, supply chain workers, host communities), the same structure reads as Tangled Rope shading toward Snare: a genuine-sounding coordination rationale (speed matters!) that in practice forecloses a technology needed for long-term grid decarbonization, enforced through financing gatekeeping that leaves little room for case-by-case reassessment of specific projects with credible faster timelines (e.g., advanced reactors). The engine's per-seat computation should reflect that gap without this story adjudicating which seat's perception is 'true' — that adjudication is exactly what the omega variables below are for.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewables developers and storage manufacturers are structural beneficiaries: the allocation rule is defined in terms that favor their deployment profile, so declaring them beneficiaries directly derives a low directionality (subsidized position). Nuclear developers, their supply chains, and host communities are structural victims: capital that would have flowed to them under a different metric (e.g., total-lifetime-carbon-avoided) is redirected, and their trapped exit options (sunk site investments, long permitting histories, specialized workforce) push their derived directionality toward the full-target end. The agenda-setting climate finance institutions occupy an unusual seat: institutional power with analytical exit options, administering a rule they could revise but have entrenched through model dependencies and existing portfolio commitments — this is closer to Piton-adjacent inertia within an otherwise tangled-rope structure, which the seat-divergence analysis below addresses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine capital scarcity plus a hard, time-sensitive emissions budget — remains partly live (independent modelers corroborate that near-term deployment speed matters physically). But institutional entrenchment (climate finance analysts whose models, funding criteria, and professional reputations are built on the metric) risks converting a once-appropriate emergency allocation heuristic into a persistent structural bias against a technology class regardless of whether specific projects (e.g., demonstrated fast-build SMRs) later invalidate the premise that nuclear cannot deploy quickly. Classifying this as Tangled Rope rather than pure Rope or pure Snare captures both halves: real coordination value in directing capital toward proven-fast decarbonization, and real asymmetric extraction from a capital-intensive technology class that cannot escape the allocation rule's terms even where individual projects might merit exception.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deployment_speed_versus_lifetime_output_tradeoff,
    'Does prioritizing carbon-per-dollar-per-year systematically under-value total lifetime avoided carbon from long-lived, high-capacity-factor assets like nuclear, or does near-term emissions urgency genuinely dominate any lifetime-output consideration given physical warming thresholds?',
    'Integrated assessment model runs comparing cumulative 2025-2100 emissions and peak warming under a deployment-speed-weighted allocation versus a total-lifetime-avoided-carbon-weighted allocation, holding total capital constant, to see whether the ranking of nuclear versus renewables reverses under different discount rates and time horizons.',
    'If lifetime output dominates under most credible discount rates, the opportunity-cost reading''s exclusion of nuclear is a modeling artifact rather than a physical necessity, weakening its claim to be the mandatory reading of the kernel. If deployment speed dominates robustly, the reading is closer to physically grounded and the extraction reading of nuclear''s exclusion is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployment_speed_versus_lifetime_output_tradeoff, empirical, 'Whether the deployment-speed metric''s ranking of technologies is robust to reasonable modeling choices or is itself a contestable framing choice.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the opportunity_cost_reading the objectively correct instantiation of the climate_mitigation_imperative kernel, or is the choice among opportunity_cost_reading, portfolio_optimization_reading, and systems_transition_reading itself a value-laden framing decision made by whoever controls climate finance criteria?',
    'Track whether climate finance institutions that adopt this reading can articulate a principled, technology-neutral justification for weighting near-term deployment speed above total decarbonization capacity and grid reliability, independent of which technologies happen to score well under each metric.',
    'If the reading selection is principled and technology-neutral, the resulting nuclear exclusion is a legitimate consequence of physical urgency. If the reading selection tracks which technologies the selecting institutions already favor (e.g., renewables-focused philanthropies and funds), the reading itself is evidence of a captured framing rather than a physically necessitated one — this would support reclassifying toward snare rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the choice of kernel reading is itself independent of the interests of the parties advocating for it.').

omega_variable(
    advanced_reactor_timeline_compression_uncertainty,
    'Can advanced/modular reactor designs credibly compress construction timelines enough to compete on the opportunity-cost metric''s own terms within the 2025-2035 window that matters most for near-term emissions?',
    'Track actual construction timelines of first-of-a-kind and Nth-of-a-kind SMR and advanced reactor projects currently under construction against their announced schedules.',
    'If timelines compress as claimed, the opportunity-cost reading''s exclusion of nuclear becomes self-correcting as project-level evidence accumulates, and the current victim classification of nuclear developers would need revision for the specific subset achieving fast builds. If timelines do not compress, the opportunity-cost reading''s treatment of nuclear as systematically slow is empirically vindicated for at least the current generation of projects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advanced_reactor_timeline_compression_uncertainty, empirical, 'Whether nuclear''s capital-intensity/timeline disadvantage is a permanent technology feature or a current, correctable state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'climate mitigation imperative' concept per the epsilon-invariance principle. Each reading assigns a different victim/beneficiary structure to nuclear power and different epsilon values: opportunity_cost_reading (this story) treats nuclear as a victim of a deployment-speed allocation rule; portfolio_optimization_reading treats nuclear as a beneficiary necessary for reliable baseload; systems_transition_reading treats nuclear as a victim for centralization/extraction reasons unrelated to deployment speed. All three are linked via affects_constraints because they compete for the same climate finance capital pool and each reading's institutional dominance structurally affects the others' available resources and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
