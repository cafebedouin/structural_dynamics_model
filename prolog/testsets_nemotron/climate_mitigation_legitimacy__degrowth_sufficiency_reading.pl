% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth Sufficiency Framing of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth_sufficiency_reading of
 *   the contested kernel 'climate_mitigation_legitimacy'. The reading asserts
 *   that legitimate climate mitigation requires absolute reductions in energy
 *   demand from high-consuming populations, making large-scale generation
 *   expansion (both nuclear and renewable) unnecessary and indeed
 *   counterproductive. The constraint operates by redefining the mitigation
 *   problem: not 'how to supply clean energy' but 'how to live well within a
 *   shrinking energy budget'. This redefinition extracts legitimacy and
 *   resources from the entire supply-side industrial complex while
 *   coordinating a demand-side policy agenda. The claimed type is
 *   tangled_rope because the constraint performs genuine coordination
 *   (unifying fragmented demand-side policies under a single carbon-budget
 *   logic) while simultaneously extracting from supply-side industries whose
 *   growth models depend on the opposite framing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth Sufficiency Framing of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '27127ff1-1951-47ac-959d-da9fb6aefb20').
narrative_ontology:cs_kernel_codification('27127ff1-1951-47ac-959d-da9fb6aefb20', distributed).
narrative_ontology:cs_authority_grounding('27127ff1-1951-47ac-959d-da9fb6aefb20', diffuse_epistemic).
narrative_ontology:cs_reading_relation('27127ff1-1951-47ac-959d-da9fb6aefb20', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('27127ff1-1951-47ac-959d-da9fb6aefb20', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('27127ff1-1951-47ac-959d-da9fb6aefb20', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_axiom('27127ff1-1951-47ac-959d-da9fb6aefb20', foundational, absolute_energy_demand_reduction_necessary_for_mitigation).
narrative_ontology:cs_axiom_status(absolute_energy_demand_reduction_necessary_for_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('27127ff1-1951-47ac-959d-da9fb6aefb20', absolute_energy_demand_reduction_necessary_for_mitigation, empirically_contingent).
narrative_ontology:cs_axiom('27127ff1-1951-47ac-959d-da9fb6aefb20', secondary, supply_expansion_legitimacy_contingent_on_sufficiency_exhaustion).
narrative_ontology:cs_axiom_status(supply_expansion_legitimacy_contingent_on_sufficiency_exhaustion, holdable).
narrative_ontology:cs_axiom_grounding('27127ff1-1951-47ac-959d-da9fb6aefb20', supply_expansion_legitimacy_contingent_on_sufficiency_exhaustion, conventional).
narrative_ontology:cs_reference_frame('27127ff1-1951-47ac-959d-da9fb6aefb20', post_paris_equity_budget_framing).
narrative_ontology:cs_drift_state('27127ff1-1951-47ac-959d-da9fb6aefb20', ar6_low_energy_demand_inclusion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('27127ff1-1951-47ac-959d-da9fb6aefb20', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_networks).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_justice_movements).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, low_consumption_communities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, post_growth_economics_institutes).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry_coalition).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_infrastructure_investors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industry_lobby).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_dependent_labor_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coordinate transnational campaigns for demand-side climate policy; set research agendas around sufficiency metrics; publish scenario models that privilege demand reduction over supply expansion. Their influence depends on academic credibility and NGO funding streams that could shift toward techno-optimist framings.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_networks, agenda_setter,
    organized, generational, constrained, global).

% Gain moral authority and policy leverage from the claim that energy poverty is solved by redistribution not expansion. Their constituencies are locked into the framing because it validates their core demand for energy democracy. Exit would mean abandoning the justice framework that gives them political coherence.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_justice_movements, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_justice_movements, agenda_setter).

% Materially benefit from policies that cap per-capita energy use and redirect investment to efficiency and public provision. Have no organized voice in energy governance; their situation improves only if the sufficiency framing displaces growth-dependent supply-side logic. Cannot exit the constraint — they live it.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, low_consumption_communities, beneficiary,
    powerless, immediate, trapped, local).

% Receive research funding, academic appointments, and policy consultancy contracts contingent on the sufficiency paradigm remaining credible. Their professional identity is fused with the paradigm; exit means career restructuring. Constrained by the small size of the post-growth funding ecosystem.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, post_growth_economics_institutes, beneficiary,
    moderate, generational, constrained, global).

% Face existential threat to their business model if demand reduction makes new baseload capacity unnecessary. Deploy massive lobbying, regulatory capture, and narrative campaigns to maintain 'baseload necessity' as a policy axiom. Their exit options are constrained by sunk capital in reactor fleets and supply chains — they cannot pivot to demand reduction without cannibalizing their core asset base.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry_coalition, payer,
    institutional, civilizational, constrained, global).

% Lose the 'unlimited clean growth' narrative that justifies massive deployment subsidies and land-use privileges. However, they retain mobility — many firms already pivot between solar, wind, storage, and efficiency services. The constraint extracts from their growth-dependent valuation models but not their operational capacity.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers, payer,
    institutional, biographical, mobile, global).

% Transmission and distribution asset values depend on throughput growth. A sufficiency pathway that flattens or reduces peak demand strands regulated assets. Their exit is constrained by regulatory compacts that guarantee returns on capital deployed for growth scenarios that may not materialize.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_infrastructure_investors, payer,
    powerful, generational, constrained, continental).

% Aluminum, steel, cement, chemicals — their competitiveness model requires cheap, abundant, growing energy supply. They can arbitrage across jurisdictions (carbon leakage) and have done so historically. The constraint threatens their cost structure but their exit option is the strongest among victims: they move production, not capital structure.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industry_lobby, payer,
    powerful, biographical, arbitrage, global).

% Building trades, energy sector unions, manufacturing unions — their membership model, pension funds, and political identity are built on energy-intensive growth. They cannot exit the growth paradigm without dissolving the institutional identity that gives them power. Identity-locked: the union *is* the growth coalition.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_dependent_labor_unions, payer,
    organized, biographical, identity_locked, national).

% Produce the integrated assessment models that structure global climate policy discourse. Their models have historically marginalized demand-side scenarios; recent AR6 inclusion of 'low energy demand' pathways reflects pressure from the sufficiency network. They observe the contest but their analytical authority is itself a stake in the outcome.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, ipcc_wg3_scenario_architects, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global mitigation strategy around a single binding metric (total energy throughput) rather than technology-specific deployment targets. Solves the collective action problem of how to allocate the remaining carbon budget when supply-side technologies have conflicting land, mineral, and temporal requirements.
% TRANSFER_FUNCTION: Moves political legitimacy, research funding, and regulatory favor from supply-expansion industries (nuclear, renewables, grid build-out) toward demand-reduction policies (efficiency standards, sufficiency caps, behavioral interventions, circular economy mandates). Transfers the burden of proof: supply-side must justify each marginal unit of capacity against a sufficiency baseline.
% ABSENT_VOICES: Global South industrialization advocates who argue that per-capita energy convergence requires massive supply expansion, not demand contraction. They are excluded from the sufficiency framing's core constituency because their development model contradicts its central premise. Their absence is structural — the framing cannot accommodate their claim without dissolving.
% DISAPPEARANCE_RATIONALE: If the sufficiency framing vanished overnight, IPCC scenarios would revert to supply-side dominance; nuclear and renewable deployment targets would be re-legitimized as primary mitigation levers; energy justice movements would lose their central policy lever; post-growth institutes would face funding collapse. The global mitigation architecture would reorganize around technology-neutral portfolio expansion.
% FOUNDING_PROBLEM: The founding problem was the empirical and ethical recognition that (a) supply-side decarbonization at 2-3% annual growth rates requires physically implausible deployment speeds and material extraction, and (b) high-consuming populations' energy use exceeds sustainable and just shares of the carbon budget. The arrangement was built to make 'less energy' a legitimate policy objective rather than a failure of supply.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's empirical premise (implausibility of supply-only decarbonization at growth rates) is corroborated by independent material flow analyses (Haberl et al., 2020; UN IRP Global Resources Outlook) and IEA's own 'Net Zero by 2050' scenario requiring unprecedented deployment rates. The ethical premise (energy justice requires demand reduction in the Global North) is corroborated by climate justice networks outside the sufficiency advocacy core. However, the nuclear and renewable industries contest both premises, citing advancing capacity factors, declining costs, and emerging storage technologies.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the systematic redirection of policy legitimacy, capital allocation, and regulatory attention from supply expansion to demand reduction. The extraction is not monetary but epistemic and institutional: the constraint makes supply-side growth narratives illegitimate in serious policy discourse. Suppression (0.52) is moderate — the constraint does not ban nuclear or renewables but makes their expansionary logic politically costly to articulate. Theater ratio (0.38) captures the performative maintenance of 'sufficiency scenarios' in IPCC reports that remain marginal to actual policy while the real extraction occurs in the redefinition of what counts as legitimate mitigation. Accessibility collapse (0.45) is moderate because supply-side alternatives remain technically and economically visible; they are suppressed politically, not rendered unthinkable. Resistance (0.71) is high because the victim coalition commands massive material resources and institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the sufficiency network seat, the constraint is a rope — it coordinates a coherent mitigation strategy that would otherwise fragment into competing techno-fixes. From the nuclear industry seat, it is a snare — the coordination story is cover for an ideological project that strands their assets. From the renewable developer seat, it is a tangled rope — they accept the coordination function (rapid decarbonization) but experience extraction from their growth narrative. From the Global South industrialization advocate seat (absent voice), it is a snare — the constraint imposes a Northern consumption ceiling as a global norm. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The sufficiency networks and justice movements are structural beneficiaries (d near 0.1-0.2) — they gain policy coherence, moral authority, and funding from the constraint. Low-consumption communities are deep beneficiaries (d ~ 0.0) but powerless to enforce it. Nuclear and renewable industries are primary targets (d ~ 0.8-0.9) — their growth models are directly invalidated. Grid investors and energy-intensive industry face high but not total extraction (d ~ 0.6-0.7) because they retain some adaptive capacity. Growth-dependent unions are identity-locked targets (d ~ 0.9) — the constraint threatens their institutional self-conception. The IPCC architects are analytical observers (d = 0.5) whose scenario authority is contested by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (supply-side implausibility at growth rates) remains empirically live — material constraints on deployment have not relaxed. However, the constraint has accumulated extraction beyond its coordination function: it now serves as a boundary marker excluding not just 'infinite growth' but any supply expansion, including that needed for Global South convergence. The mandatrophy risk is that the constraint becomes a vehicle for Northern consumption politics masquerading as biophysical necessity. The theater ratio rise (0.25 to 0.38) tracks this: more energy goes into maintaining the sufficiency scenario in models than into implementing demand reduction policies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_coordination_vs_extraction_boundary,
    'Where does the genuine coordination function of the sufficiency framing end and the ideological extraction from supply-side industries begin?',
    'Counterfactual policy simulation: if a supply-side technology (e.g., advanced nuclear, perovskite solar) achieved deployment characteristics that satisfied the material plausibility criteria, would the sufficiency network accept it as complementary, or would the framing shift to exclude it on new grounds?',
    'If the boundary is fixed by biophysical criteria, the constraint is a rope with incidental extraction. If the boundary shifts to maintain the victim set, the constraint is a snare using biophysics as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_coordination_vs_extraction_boundary, conceptual, 'Whether the constraint''s extraction is structurally necessary to its coordination function or ideologically motivated').

omega_variable(
    global_south_exclusion_mechanism,
    'Is the exclusion of Global South industrialization needs from the sufficiency framing a structural necessity of the carbon budget logic, or a political choice that could be resolved by differentiated convergence pathways?',
    'Scenario analysis of contracted-and-converge models with explicit per-capita equity floors and differentiated demand reduction schedules for high vs. low consuming populations.',
    'If exclusion is structurally necessary, the constraint is a snare for Global South development. If resolvable, the constraint is a tangled rope with a design flaw in its current institutionalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_exclusion_mechanism, preference, 'Whether the constraint''s justice claim is universalizable or Northern-particular').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does this reading''s core premise (absolute demand reduction makes large-scale generation unnecessary) logically foreclose the sibling readings, or do they coexist as competing policy options?',
    'Formal logic analysis of the premises: if ''sufficiency makes expansion unnecessary'' is true, does it entail ''baseload is unnecessary'' and ''renewables cannot achieve full decarbonization''? Or can all four readings be simultaneously true under different parameter assumptions?',
    'If forecloses, the kernel is a zero-sum legitimacy contest. If coexists, the kernel is a pluralistic policy space where the constraint story''s extraction is from the *dominance* of other readings, not their existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between this reading and its siblings in the climate_mitigation_legitimacy kernel').

omega_variable(
    theater_ratio_driver_identity,
    'Is the rising theater ratio driven by performative maintenance of the sufficiency scenario in IPCC models (intermittent reinforcement), or by genuine implementation friction in demand-side policies?',
    'Track the ratio of (a) peer-reviewed sufficiency scenario publications to (b) enacted demand-reduction policies with measurable throughput effects, over the measurement interval.',
    'If publications grow faster than policies, theater is the extraction mechanism. If policies track scenarios, theater reflects implementation lag.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_driver_identity, empirical, 'Whether the constraint''s performative component is an extraction mechanism or a side effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(degrowth_sufficiency_tr_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(degrowth_sufficiency_tr_t2018, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(degrowth_sufficiency_tr_t2021, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2021, 0.31).
narrative_ontology:measurement(degrowth_sufficiency_tr_t2024, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2024, 0.34).
narrative_ontology:measurement(degrowth_sufficiency_tr_t2027, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2027, 0.36).
narrative_ontology:measurement(degrowth_sufficiency_tr_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2030, 0.38).
narrative_ontology:measurement(degrowth_sufficiency_tr_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2035, 0.38).

% Extraction over time
narrative_ontology:measurement(degrowth_sufficiency_be_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(degrowth_sufficiency_be_t2018, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(degrowth_sufficiency_be_t2021, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(degrowth_sufficiency_be_t2024, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement(degrowth_sufficiency_be_t2027, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2027, 0.65).
narrative_ontology:measurement(degrowth_sufficiency_be_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(degrowth_sufficiency_be_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(degrowth_sufficiency_su_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(degrowth_sufficiency_su_t2018, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement(degrowth_sufficiency_su_t2021, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2021, 0.47).
narrative_ontology:measurement(degrowth_sufficiency_su_t2024, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2024, 0.51).
narrative_ontology:measurement(degrowth_sufficiency_su_t2027, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2027, 0.52).
narrative_ontology:measurement(degrowth_sufficiency_su_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2030, 0.52).
narrative_ontology:measurement(degrowth_sufficiency_su_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2035, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_demand_reduction_policy_suite).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, carbon_budget_allocation_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the climate_mitigation_legitimacy kernel. The kernel contest structures the entire mitigation policy field: each reading defines a different legitimate solution space, victim set, and coordination logic. This reading's extraction targets the growth-dependency of both nuclear and renewable industries simultaneously — a structural position unique among the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, organized, 0.15).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, powerless, 0.05).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, institutional, 0.85).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
