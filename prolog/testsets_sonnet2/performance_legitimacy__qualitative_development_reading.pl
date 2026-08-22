% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy: 'High-Quality Development' Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint models one reading of the contested 'performance
 *   legitimacy' kernel — the standard by which a state's economic governance
 *   is judged legitimate. In this reading, legitimacy is grounded in
 *   demonstrated structural transformation toward 'high-quality development':
 *   innovation intensity, sustainability, and efficiency gains, explicitly
 *   tolerating slower headline GDP growth as the price of the transition.
 *   This reading reallocates credit, land-use priority, and cadre-evaluation
 *   criteria away from legacy manufacturing and property-linked local finance
 *   toward high-tech firms and state-backed innovation vehicles, with venture
 *   capital and M&A exit infrastructure built out to support the latter. The
 *   coordination function is real — addressing genuine overcapacity,
 *   environmental, and debt-fragility problems from the prior growth model —
 *   but the transition costs land disproportionately on traditional
 *   manufacturing labor, construction workers, and property-dependent local
 *   governments, none of whom have a formal channel to contest the pace or
 *   terms of the reallocation.
 *
 * KEY AGENTS:
 *   - central_planning_apparatus: agenda_setter — defines and enforces the qualitative-development performance criteria
 *   - high_tech_sector_firms: primary beneficiary — receives preferential credit, tax, and procurement treatment
 *   - traditional_manufacturing_employment: primary payer — bears deprioritization, layoffs, deindustrialization
 *   - property_dependent_local_governments: secondary payer — loses land-finance revenue model without adequate replacement
 *   - national_statistics_bureau: analytical observer — operationalizes the indicators that make the standard measurable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.58).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy: 'High-Quality Development' Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '1eead9c0-9d75-4571-a8e0-8dcd38e43a68').
narrative_ontology:cs_kernel_codification('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', distributed).
narrative_ontology:cs_authority_grounding('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', extraction).
narrative_ontology:cs_interpretation_layer_present('1eead9c0-9d75-4571-a8e0-8dcd38e43a68').
narrative_ontology:cs_reading_relation('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', performance_legitimacy__quantitative_growth_reading, forecloses).
narrative_ontology:cs_reading_relation('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', foundational, structural_upgrading_supersedes_volume_growth).
narrative_ontology:cs_axiom_status(structural_upgrading_supersedes_volume_growth, holdable).
narrative_ontology:cs_axiom_grounding('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', structural_upgrading_supersedes_volume_growth, instrumental).
narrative_ontology:cs_axiom('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', foundational, slower_growth_is_acceptable_transition_cost).
narrative_ontology:cs_axiom_status(slower_growth_is_acceptable_transition_cost, holdable).
narrative_ontology:cs_axiom_grounding('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', slower_growth_is_acceptable_transition_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', investment_led_volume_growth_legitimacy).
narrative_ontology:cs_drift_state('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', post_overcapacity_deleveraging_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('1eead9c0-9d75-4571-a8e0-8dcd38e43a68', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sector_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, venture_capital_and_pe_intermediaries).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, central_planning_apparatus).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_employment).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, construction_and_real_estate_workers).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, smaller_industrial_cities).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, innovation_driven_growth_superiority_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__qualitative_development_reading, efficiency_over_volume_development_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the criteria by which economic performance is judged legitimate, shifting evaluation from GDP growth rate to indicators like R&D intensity, patent output, and industrial upgrading. Directs credit allocation, cadre promotion criteria, and industrial policy toward this standard. Bears no direct personal cost from the reallocation and gains a durable legitimacy narrative that survives a lower headline growth number.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_planning_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Receive preferential credit, tax treatment, procurement access, and regulatory tolerance because their activity is classified as 'high-quality.' Can relocate operations across favored innovation zones and access state venture funds; some can list overseas, giving them exit leverage most domestic firms lack.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sector_firms, beneficiary,
    powerful, biographical, arbitrage, national).

% State guidance funds, national labs, and industrial policy vehicles are staffed, funded, and expanded under this legitimacy standard. Their institutional survival and budget growth are directly tied to the qualitative-development framing continuing to define success.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    organized, generational, mobile, national).

% Benefit from the buildout of exit infrastructure (IPO channels, M&A markets, state co-investment funds) prioritized to support innovation-sector capital formation. Can shift capital across sectors and geographies faster than the industrial workers whose sectors lose favor.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, venture_capital_and_pe_intermediaries, beneficiary,
    organized, biographical, arbitrage, national).

% Works in labor-intensive or resource-intensive manufacturing sectors that are deprioritized, denied credit, or actively phased out (overcapacity cuts, environmental closures) to make room for the new standard. Faces layoffs, wage stagnation, and retraining mandates with no equivalent state investment behind them; relocation to innovation-favored regions is not realistic given skills, housing costs, and hukou-linked constraints.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_employment, payer,
    powerless, biographical, trapped, regional).

% Historically financed public services through land sales and construction-linked revenue. Under the qualitative-development standard this fiscal model is treated as low-quality growth to be wound down, but no adequate replacement revenue stream has been substituted, leaving these governments carrying legacy debt while losing their primary financing tool.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    moderate, biographical, trapped, regional).

% Employment collapses as property-sector deleveraging and construction slowdown are treated as necessary corrections toward higher-quality growth. Have few transferable skills into the favored high-tech sectors and little geographic or social mobility to relocate into innovation hubs.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, construction_and_real_estate_workers, payer,
    powerless, immediate, trapped, regional).

% Built their entire local economy around a single legacy industry now classified as low-quality growth. Lack the university systems, talent pools, or infrastructure to attract innovation-sector investment, and are structurally excluded from the resources being redirected toward flagship tech corridors.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, smaller_industrial_cities, payer,
    powerless, generational, trapped, regional).

% Compiles and reports the indicators (patent counts, R&D-to-GDP ratio, industrial upgrading indices) that operationalize the qualitative-development standard, shaping which activity counts as evidence of successful transformation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, national_statistics_bureau, observer,
    institutional, immediate, analytical, national).

% Has no formal channel to contest the reclassification of their sector as obsolete or to demand transitional support proportional to the reform's benefits elsewhere; labor organizing outside state-sanctioned unions is restricted, so objection has no institutional pathway.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, displaced_industrial_workers_coalition, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, high_tech_sector_firms).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Redirects national investment, credit, and policy attention toward higher-value-added, more innovation-intensive, and more environmentally sustainable economic activity, addressing genuine problems of overcapacity, environmental degradation, and diminishing returns from investment-and-construction-led growth.
% TRANSFER_FUNCTION: Moves credit access, land-use approval priority, tax preference, and cadre-promotion weight away from labor-intensive manufacturing and property-linked local fiscal activity toward high-tech firms, state innovation vehicles, and the capital-market infrastructure that serves them.
% ABSENT_VOICES: Displaced manufacturing and construction workers, and the fiscal officials of smaller property-dependent municipalities, have no institutionalized channel to contest the pace or distribution of the transition; their objections surface as social stability incidents rather than as formal input into planning criteria.
% DISAPPEARANCE_RATIONALE: If this legitimacy standard were abandoned overnight, credit and policy priority would likely revert toward volume-based growth metrics and property-sector stabilization, industrial policy funding for frontier tech would face renewed competition from stimulus-oriented infrastructure and construction spending, and local governments currently being weaned off land finance would face pressure to restart it — the resource allocation underlying regional development would visibly shift.
% FOUNDING_PROBLEM: Investment- and export-led growth built on debt-financed construction and low-value manufacturing was hitting diminishing returns, generating overcapacity, environmental damage, and financial fragility (local government debt, property-sector leverage) — the qualitative-development standard was built to redirect the growth model before these accumulating fragilities produced a crisis.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists and multilateral institutions (e.g., IMF Article IV consultations, World Bank growth-model analyses) corroborate that the underlying overcapacity and property-leverage problems were real and that a growth-model shift was structurally necessary. However, displaced-region economists and labor researchers outside the beneficiary set argue the standard has been implemented in a way that concentrates gains in favored sectors and regions while externalizing transition costs onto manufacturing labor and property-dependent localities faster than replacement livelihoods or fiscal revenue have materialized — corroboration for the founding problem's reality is stronger than corroboration that the current implementation resolves it equitably.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects a substantial but not extreme transfer: the reallocation genuinely addresses a real structural problem (overcapacity, property-sector leverage, environmental cost), which caps how purely extractive the arrangement can be judged, but the concentration of gains in high-tech firms and innovation intermediaries alongside uncompensated costs to manufacturing labor and local fiscal capacity pushes it well above a pure-coordination reading. Suppression (0.58) captures that dissent from displaced sectors has limited institutional channels — labor organizing outside sanctioned unions is restricted, and local government fiscal complaints are managed administratively rather than through open contestation. Theater ratio (0.44) is elevated because a meaningful share of 'high-quality development' activity — headline patent counts, symbolic innovation-zone announcements, cadre performance narratives — outpaces actual productivity or sustainability gains, though real R&D investment and industrial upgrading are also genuinely occurring. Accessibility collapse (0.5) is moderate: firms and localities that can pivot into favored sectors retain real alternatives, but those locked into legacy industry or land-finance dependency have few realistic paths. Resistance (0.61) is substantial because the costs are concentrated and visible even where voice channels are constrained — social stability incidents, informal protest, and elite pushback from provincial officials losing fiscal tools are all observable.
 *
 * DIRECTIONALITY LOGIC:
 *   High-tech firms, the state-backed innovation ecosystem, and VC/PE intermediaries are beneficiaries with mobile-to-arbitrage exit — they can shift capital and operations toward whatever the standard currently rewards, giving them low derived directionality (subsidized position). Traditional manufacturing labor, construction workers, and smaller industrial cities are trapped, powerless payers with high derived directionality — the standard's costs land on them with no meaningful exit. Property-dependent local governments occupy a moderate-power but still trapped position: they administer real fiscal authority but cannot exit the constraint any more than they can invent a replacement revenue base overnight, so their directionality sits closer to the target end than their formal power level would suggest on its own.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — overcapacity, environmental strain, and property-sector leverage from the prior growth model — was genuinely live at founding and is corroborated as real by independent multilateral analysis, which argues against treating this as pure mandatrophy. But the founding_problem_status is authored as contested rather than resolved: the standard's persistence as the dominant legitimacy criterion, even as implementation externalizes costs faster than compensating mechanisms materialize, is exactly the tangled-rope signature — a genuine coordination function riding alongside asymmetric extraction that requires active enforcement (credit gatekeeping, promotion criteria, statistical operationalization) to hold. Classifying this as a clean rope would erase the traditional-manufacturing and property-dependent-government victim set; classifying it as a pure snare would erase the real environmental and overcapacity problem the standard responds to. Tangled rope is the only classification that holds both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_transition_vs_growth_model_narrative,
    'Is the qualitative-development standard a genuine structural response to real overcapacity and leverage problems, or primarily a legitimacy narrative constructed to justify a resource reallocation toward politically favored sectors and regions?',
    'Compare realized productivity and total-factor-productivity gains in favored high-tech sectors against the scale of preferential credit and subsidy directed toward them; compare against counterfactual growth-model trajectories in similarly-positioned economies that did not adopt the standard.',
    'If productivity gains are commensurate with the resources redirected, the tangled-rope reading is well-supported (real coordination function plus real extraction). If gains are substantially smaller than the redirected resources, the constraint looks more like a snare wearing a development-economics justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_transition_vs_growth_model_narrative, empirical, 'Whether measured productivity gains justify the scale of resource reallocation under this reading.').

omega_variable(
    transition_cost_compensation_gap,
    'Is the absence of adequate compensation or retraining investment for displaced manufacturing and construction workers a temporary lag in an otherwise well-intentioned transition, or a structural feature of how the standard is being implemented?',
    'Track state investment in worker retraining, relocation subsidy, and local-government fiscal-replacement mechanisms relative to the scale of sectoral job losses and land-finance revenue lost, over a multi-year window.',
    'A rising compensation gap over time would support reclassifying the extraction as intensifying rather than transitional, weakening any scaffold-adjacent reading and reinforcing tangled_rope or a drift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_compensation_gap, empirical, 'Whether transition costs to displaced sectors are being compensated over time or left uncompensated.').

omega_variable(
    kernel_framing_indeterminacy,
    'Is the choice to read this constraint through the qualitative_development_reading (rather than quantitative_growth_reading or techno_nationalist_reading) itself a contested political act, and does that framing choice change which victim set is visible?',
    'Compare official planning documents and cadre-evaluation criteria across time periods and provinces to see which reading''s language dominates the operative legitimacy standard at each point, and whether the shift in dominant framing correlates with shifts in which sectors bear costs.',
    'If the dominant reading shifts opportunistically depending on which framing best justifies current resource allocation, that supports treating performance_legitimacy as a kernel genuinely under contest among elites rather than a settled doctrine — reinforcing the need to keep these readings as separate constraint stories rather than merging them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_indeterminacy, conceptual, 'Whether the selection among sibling readings is itself an object of elite contestation with material stakes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__qualitative_development_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__qualitative_development_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__qualitative_development_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__qualitative_development_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__qualitative_development_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__qualitative_development_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__qualitative_development_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__qualitative_development_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__qualitative_development_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__qualitative_development_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__qualitative_development_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__qualitative_development_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__qualitative_development_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__qualitative_development_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__qualitative_development_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the performance_legitimacy kernel. It forecloses quantitative_growth_reading at the level of legitimacy criteria (a GDP-rate-centric legitimacy standard cannot simultaneously be held alongside a standard that treats slower growth as success within one coherent evaluative framework, though different factions may hold each). It coexists_with techno_nationalist_reading because innovation-driven high-quality development and technological self-sufficiency point toward overlapping but not identical investment priorities (civilian efficiency gains vs. strategic-sector security), and different factions can hold both simultaneously without contradiction. It influences livelihood_security_reading because the credit and land-use reallocation this reading drives changes the resource environment in which livelihood-security commitments (employment, elderly care, healthcare) must be funded, without logically foreclosing the livelihood standard. Each reading carries its own ε, beneficiary/victim set, and classification; do not average or merge them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
