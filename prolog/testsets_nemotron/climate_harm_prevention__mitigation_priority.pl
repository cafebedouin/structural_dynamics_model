% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation Priority Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation priority reading frames legitimate climate response as
 *   aggressive emissions reduction achieved through technological transition
 *   within continued economic growth. This reading has dominated
 *   international climate architecture since the UNFCCC (1992) and structures
 *   the Paris Agreement. It positions future generations as primary
 *   beneficiaries and present carbon-intensive sectors as primary
 *   cost-bearers, assuming growth-compatible decarbonization is feasible. The
 *   constraint operates as a tangled rope: it provides genuine coordination
 *   (shared targets, technology transfer, carbon markets) while extracting
 *   transition costs from structurally disadvantaged groups (fossil workers,
 *   petrostates) who lack exit. The coordination function is real — without
 *   it, collective action on a global commons problem would be far weaker —
 *   but the extraction is asymmetric and actively enforced through trade
 *   rules, finance conditionality, and scenario gatekeeping.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.32).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.41).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.32).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation Priority Climate Response").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'c9d329e5-b536-459c-a378-00f859d982af').
narrative_ontology:cs_kernel_codification('c9d329e5-b536-459c-a378-00f859d982af', formalized).
narrative_ontology:cs_authority_grounding('c9d329e5-b536-459c-a378-00f859d982af', lineage).
narrative_ontology:cs_interpretation_layer_present('c9d329e5-b536-459c-a378-00f859d982af').
narrative_ontology:cs_reading_relation('c9d329e5-b536-459c-a378-00f859d982af', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('c9d329e5-b536-459c-a378-00f859d982af', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('c9d329e5-b536-459c-a378-00f859d982af', foundational, growth_compatible_decarbonization_feasible).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization_feasible, holdable).
narrative_ontology:cs_axiom_grounding('c9d329e5-b536-459c-a378-00f859d982af', growth_compatible_decarbonization_feasible, empirically_contingent).
narrative_ontology:cs_axiom('c9d329e5-b536-459c-a378-00f859d982af', foundational, intergenerational_harm_prevention_obligatory).
narrative_ontology:cs_axiom_status(intergenerational_harm_prevention_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('c9d329e5-b536-459c-a378-00f859d982af', intergenerational_harm_prevention_obligatory, deontological).
narrative_ontology:cs_reference_frame('c9d329e5-b536-459c-a378-00f859d982af', pre_industrial_climate_stability).
narrative_ontology:cs_drift_state('c9d329e5-b536-459c-a378-00f859d982af', contemporary_overshoot_trajectory, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c9d329e5-b536-459c-a378-00f859d982af', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations_global).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, clean_energy_sectors).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, developing_economies_dependent_on_extraction).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, intergenerational_justice_principle).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, technological_optimism_thesis).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, growth_compatible_decarbonization_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Future generations are the primary beneficiaries of avoided catastrophic warming but have no voice in present decisions and cannot exit the atmospheric commons. Their interests are represented only through proxy advocacy and legal standing innovations.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations_global, beneficiary,
    powerless, civilizational, trapped, universal).

% Renewable energy, electrification, and efficiency industries receive massive policy support, subsidies, and market creation from mitigation policy. They actively lobby for stronger targets but can pivot to other markets if policy shifts.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, clean_energy_sectors, beneficiary,
    organized, biographical, mobile, global).

% Populations in low-lying islands, arid regions, and extreme heat zones benefit directly from successful mitigation. They have limited political power and constrained exit options (migration barriers, loss of homeland). Some are also victims of poorly designed transition policies.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_vulnerable_populations, beneficiary,
    powerless, biographical, constrained, global).

% Fossil fuel extraction, heavy industry, and internal combustion transport bear the direct costs of transition (stranded assets, carbon pricing, regulation). They retain enormous agenda-setting power through lobbying, regulatory capture, and narrative control, delaying and shaping policy to manage decline.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors, agenda_setter).

% Workers in coal, oil, gas, and associated industries face job loss, community collapse, and identity dislocation. Retraining is often inadequate, geographic mobility limited, and the promised 'just transition' remains largely rhetorical. They bear concentrated costs while benefits are diffuse.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_workers, payer,
    moderate, biographical, constrained, national).

% Nations whose fiscal stability depends on fossil fuel exports (e.g., petrostates, coal exporters) face revenue collapse without comparable alternative development pathways. They have constrained exit due to debt, infrastructure lock-in, and limited diversification capacity. Climate finance promises remain unfulfilled at scale.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, developing_economies_dependent_on_extraction, payer,
    moderate, generational, constrained, regional).

% International negotiators, national policymakers, and technical bodies (IPCC, UNFCCC) design and administer the mitigation framework. They benefit professionally and institutionally from the regime's continuation but can rotate across climate, energy, and development portfolios.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_policy_architects, agenda_setter,
    institutional, generational, arbitrage, global).

% Advocates for planned economic contraction argue mitigation within growth is physically impossible. They are structurally excluded from mainstream policy forums (IPCC scenarios, UNFCCC negotiations, major climate finance) because their framing contradicts the growth-compatible premise. Their exclusion is maintained by institutional gatekeeping, not lack of evidence.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    moderate, generational, identity_locked, global).

% Actors who argue for prioritizing near-term resilience (infrastructure, migration planning, heat adaptation) over deep mitigation. They include some development banks, disaster agencies, and Global South negotiators. They are not fully excluded but their framing is subordinated in the dominant architecture.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_prioritizers, excluded,
    organized, biographical, mobile, global).

% Philosophers, economists, and legal scholars analyzing the ethical structure of climate policy. They have no material stake but frame the legitimacy of the constraint. Their analyses inform but do not determine policy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, intergenerational_ethics_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global emissions reduction across sovereign states to avoid catastrophic climate change, providing a shared target architecture (NDCs, carbon budgets, net-zero pledges) that aligns disparate national policies toward a collective atmospheric goal.
% TRANSFER_FUNCTION: Transfers transition costs (capital reallocation, stranded assets, workforce dislocation, higher near-term energy costs) from future generations (who avoid climate damages) to present carbon-intensive sectors and their workers, mediated by carbon pricing, regulation, and subsidy regimes.
% ABSENT_VOICES: Future generations (by definition), fossil fuel workers in informal sectors, subsistence communities affected by both climate impacts and transition minerals extraction, degrowth advocates excluded from scenario modeling, and Global South voices demanding climate finance as prerequisite for mitigation ambition.
% DISAPPEARANCE_RATIONALE: If the mitigation priority framework vanished overnight, the Paris Architecture would collapse, carbon pricing would evaporate, clean energy deployment would lose policy tailwinds, and emissions would follow baseline growth trajectories. The world would rearrange around unmanaged warming and reactive adaptation.
% FOUNDING_PROBLEM: How to prevent catastrophic climate change while maintaining economic growth and political stability, given that unrestricted emissions threaten civilizational stability but rapid decarbonization threatens incumbent power and development aspirations.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group III reports corroborate the technical feasibility of mitigation pathways. Climate-vulnerable nation coalitions (AOSIS, LDCs) corroborate the urgency. Fossil fuel interests and some development economists contest whether growth-compatible decarbonization is physically achievable at required speed. No independent body has validated the core feasibility claim at scale.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).
:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) reflects real but moderate transfer: carbon pricing and regulation impose costs on incumbents, but these are partly offset by subsidies, free allocation, and delayed timelines. Suppression (0.41) is moderate: alternatives (degrowth, radical adaptation) are marginalized in official discourse and scenario modeling, but not banned. Theater ratio (0.28) is significant: net-zero pledges, distant targets, and 'just transition' rhetoric often substitute for near-term action. Accessibility collapse (0.58) is moderate: the carbon budget framing narrows the imaginable policy space, but alternative framings persist in civil society. Resistance (0.52) is substantial: incumbent industries, petrostates, and political movements actively contest the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are full beneficiaries (d ≈ 0.0) — they receive avoided damages with zero cost. Clean energy sectors are beneficiaries with arbitrage exit (d ≈ 0.15) — they gain policy rents but can pivot. Climate-vulnerable populations are beneficiaries with constrained exit (d ≈ 0.3) — they gain from mitigation but bear adaptation costs and transition harms. Carbon-intensive sectors are payers with agenda-setting power (d ≈ 0.55) — they bear costs but shape the constraint. Fossil fuel workers are payers with constrained exit (d ≈ 0.75) — concentrated costs, limited mobility. Developing extraction economies are payers with constrained exit (d ≈ 0.7) — fiscal exposure without alternatives. Policy architects are agenda-setters with arbitrage exit (d ≈ 0.2). Degrowth advocates are excluded and identity-locked (d ≈ 0.85) — their framing is structurally incompatible with the constraint. Adaptation prioritizers are excluded but mobile (d ≈ 0.5). Ethics scholars are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing catastrophic warming while preserving growth) remains contested — not dead, not universally accepted as live. The coordination function (global emissions alignment) is real and would cause world rearrangement if removed. But extraction has accumulated (rising extractiveness, theater) as deadlines approach and easy reductions are exhausted. The mandatrophy risk is that the growth-compatibility axiom becomes a cover for inadequate mitigation, transforming the tangled rope toward snare for vulnerable populations while piton-like performance (net-zero theater) expands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_feasibility,
    'Is deep decarbonization compatible with continued economic growth at the speed required by carbon budgets, or does the growth premise constrain mitigation ambition to infeasible pathways?',
    'Empirical test: compare actual decarbonization rates under growth-targeting policies vs. required rates for 1.5°C/2°C. Monitor whether integrated assessment models systematically overestimate feasibility by embedding growth assumptions.',
    'If growth-compatible decarbonization is physically infeasible at required speed, the constraint''s coordination function is partially illusory — it coordinates around a target that cannot be hit without either breaking the growth premise or accepting catastrophic warming. This would shift classification toward snare for vulnerable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_feasibility, empirical, 'Whether the core feasibility assumption of the mitigation priority reading holds.').

omega_variable(
    transition_cost_distribution,
    'Are transition costs (stranded assets, job losses, energy poverty) distributed in a way that the coordination function justifies, or do they fall disproportionately on those with least responsibility and least capacity to absorb them?',
    'Longitudinal tracking of distributional impacts of carbon pricing, subsidy reform, and transition policies across income deciles and geographies. Compare with counterfactual climate damage distribution.',
    'If costs fall disproportionately on the global poor and fossil-dependent workers while benefits accrue to clean energy capital and future middle classes, the extraction asymmetry exceeds what the coordination function can justify. This would shift the constraint toward snare for specific victim groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_distribution, empirical, 'Whether the extraction asymmetry is procedurally and substantively fair.').

omega_variable(
    kernel_reading_boundary,
    'Does the mitigation_priority reading structurally foreclose the degrowth_reading, or do they coexist as competing but logically compatible framings within different institutional frameworks?',
    'Analyze whether any institutional framework (national policy, international treaty, financial architecture) could simultaneously adopt both readings as legitimate. Track whether degrowth scenarios are modeled in IPCC assessments and UNFCCC processes.',
    'If forecloses: the kernel admits only one legitimate reading at a time, making the reading choice a structural commitment. If coexists_with: the kernel supports pluralism, and the dominance of mitigation_priority is political, not logical. If influences: mitigation_priority''s institutional dominance creates resource/legitimacy pressure on degrowth_reading without logical foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between this reading and the degrowth_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__mitigation_priority, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__mitigation_priority, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(clim_tr_t2009, climate_harm_prevention__mitigation_priority, theater_ratio, 2009, 0.22).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__mitigation_priority, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(clim_tr_t2024, climate_harm_prevention__mitigation_priority, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__mitigation_priority, base_extractiveness, 1992, 0.18).
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__mitigation_priority, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(clim_be_t2009, climate_harm_prevention__mitigation_priority, base_extractiveness, 2009, 0.26).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__mitigation_priority, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.3).
narrative_ontology:measurement(clim_be_t2024, climate_harm_prevention__mitigation_priority, base_extractiveness, 2024, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__mitigation_priority, suppression_requirement, 1992, 0.28).
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__mitigation_priority, suppression_requirement, 2000, 0.33).
narrative_ontology:measurement(clim_su_t2009, climate_harm_prevention__mitigation_priority, suppression_requirement, 2009, 0.37).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__mitigation_priority, suppression_requirement, 2015, 0.39).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(clim_su_t2024, climate_harm_prevention__mitigation_priority, suppression_requirement, 2024, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, global_carbon_pricing_architecture).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, clean_energy_subsidy_regimes).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, just_transition_policy_frameworks).

% DUAL FORMULATION NOTE:
% Part of the climate_harm_prevention kernel family. This reading (mitigation_priority) assumes growth-compatible decarbonization feasibility and prioritizes emissions reduction. The adaptation_priority reading accepts higher warming and prioritizes resilience. The degrowth_reading rejects growth-compatibility and prioritizes planned contraction. The three readings share the kernel's harm prevention aim but differ on feasibility premises, cost-bearer identification, and legitimate policy space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, institutional, 0.2).
constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, organized, 0.15).
constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, moderate, 0.7).
constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
