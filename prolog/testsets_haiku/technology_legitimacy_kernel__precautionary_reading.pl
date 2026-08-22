% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Technology Legitimacy Standard for Climate Mitigation
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The precautionary reading of technology legitimacy in climate mitigation
 *   codifies reversibility within a generation as the non-negotiable boundary
 *   for acceptable climate solutions. Renewable technologies (solar, wind,
 *   battery storage) benefit because their failure modes are reversible and
 *   decommissioning manageable. Nuclear technology, despite its carbon
 *   intensity and operational safety record, is structurally excluded because
 *   high-level radioactive waste legacies and catastrophic accident scenarios
 *   impose irreversible harms spanning 10^5+ years. This reading
 *   operationalizes the precautionary principle and intergenerational justice
 *   doctrine in energy-infrastructure governance. The constraint extracts
 *   real costs from grid-reliability planners and nuclear-dependent energy
 *   systems while channeling capital and legitimacy toward renewables. The
 *   claim/metric divergence is intentional: the reading is CLAIMED as
 *   tangled_rope (genuine coordination problem: solving climate without
 *   exporting waste crises) while the authored metrics describe substantially
 *   asymmetric extraction — the engine computes that divergence from the
 *   structural data. This is one of three sibling readings of the same
 *   kernel; the siblings are recorded in cs_structure.reading_relations and
 *   the axioms are distinguished in cs_structure.axioms.
 *
 * KEY AGENTS:
 *   - renewable_technology_developers: beneficiary — gain market legitimacy and policy capital
 *   - nuclear_technology_incumbents: target/payer — face delegitimization and constrained deployment
 *   - fossil_fuel_transition_planners: target/payer — absorb grid-stability costs during transition
 *   - grid_reliability_advocates: target/payer — subordinated optimization objective to precautionary constraint
 *   - future_generations: nominal beneficiary, structurally excluded — protected paternalistically
 *   - climate_scientists: observer/analytical seat — provide empirical grounding for 'bounded' and 'reversible'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.72).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Technology Legitimacy Standard for Climate Mitigation").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '8b00fae4-c5d7-41f4-9445-0b2d45d3fa16').
narrative_ontology:cs_kernel_codification('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', distributed).
narrative_ontology:cs_authority_grounding('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', distributed).
narrative_ontology:cs_reading_relation('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', foundational, irreversible_harm_inadmissible).
narrative_ontology:cs_axiom_status(irreversible_harm_inadmissible, holdable).
narrative_ontology:cs_axiom_grounding('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', irreversible_harm_inadmissible, deontological).
narrative_ontology:cs_axiom('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', foundational, intergenerational_justice_priority).
narrative_ontology:cs_axiom_status(intergenerational_justice_priority, holdable).
narrative_ontology:cs_axiom_grounding('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', intergenerational_justice_priority, deontological).
narrative_ontology:cs_axiom('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', secondary, one_generation_reversibility_threshold).
narrative_ontology:cs_axiom_status(one_generation_reversibility_threshold, holdable).
narrative_ontology:cs_axiom_grounding('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', one_generation_reversibility_threshold, conventional).
narrative_ontology:cs_reference_frame('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', intergenerational_stewardship_framework).
narrative_ontology:cs_drift_state('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', contemporary_carbon_budget_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8b00fae4-c5d7-41f4-9445-0b2d45d3fa16', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_technology_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_technology_incumbents).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, fossil_fuel_transition_planners).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, grid_reliability_advocates).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, intergenerational_justice_doctrine).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, precautionary_principle_in_environmental_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar, wind, and battery-storage manufacturers gain market legitimacy and policy support under the precautionary reading. Their decommissioning pathways are reversible — panels and turbines can be recycled, storage systems have clear end-of-life protocols — and they face no century-scale waste or accident liabilities. The constraint codifies their technologies as legitimate while competitors face exclusion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_technology_developers, beneficiary,
    organized, generational, mobile, global).

% Nuclear operators and fuel-cycle industries face delegitimization under the precautionary standard. High-level radioactive waste remains hazardous for 100,000+ years — far beyond the one-generation reversibility threshold. Catastrophic accident scenarios (core melt, release) impose irreversible contamination. The constraint's adoption forecloses their expansion and channels policy toward competitors, regardless of their operational safety record or carbon-intensity advantages.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_technology_incumbents, payer,
    institutional, civilizational, constrained, global).

% Grid operators and energy ministers responsible for ensuring reliable electricity supply face a conflict: the precautionary standard prioritizes reversibility over dispatchability, which constrains their ability to use existing nuclear baseload while building renewable capacity. They absorb the cost of managing intermittency and grid stability during the transition — a constraint imposed by the legitimacy criterion, not by technical necessity.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, fossil_fuel_transition_planners, payer,
    institutional, biographical, trapped, national).

% Structurally absent from policy-making. The constraint nominally protects them by forbidding irreversible legacy costs — no nuclear waste inheritances, no contaminated sites — but they have no voice in negotiating the legitimacy standard itself, no seat at the table, and cannot exit. Their protection is imposed paternalistically, not negotiated.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, future_generations, excluded).
narrative_ontology:stakeholder_non_agent(technology_legitimacy_kernel__precautionary_reading, future_generations).

% Provide empirical grounding for the constraint: model worst-case climate impacts, assess carbon budgets, evaluate technology failure modes and mitigation timelines. Their evidence informs what counts as 'bounded' and 'reversible,' but they do not set the threshold — policy-makers translate the evidence into the legitimacy criterion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, climate_scientists, observer,
    analytical, generational, analytical, global).

% Engineers, reliability organizations, and blackout-risk analysts argue that reversibility within a generation is incompatible with zero-carbon baseload; the constraint subordinates their optimization objective (stable, dispatchable supply) to the precautionary objective (bounded, reversible risk). They absorb the cost of solving intermittency and grid stability under tighter constraints than a reliability-primary standard would impose.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, grid_reliability_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, grid_reliability_advocates, observer).

% The normative doctrine — enshrined in treaties, national constitutions, and environmental legislation — that when an activity raises threats of harm to the environment, precautionary measures should be taken even if cause-and-effect relationships are not fully established. The constraint operationalizes this doctrine in climate-technology governance by making reversibility non-negotiable.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, precautionary_principle_advocacy, agenda_setter,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(technology_legitimacy_kernel__precautionary_reading, precautionary_principle_advocacy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the technology-selection problem in climate mitigation by establishing a single non-negotiable criterion (reversibility within one generation) that applies globally, preventing jurisdictions from choosing technologies that solve today's climate crisis at the cost of creating permanent waste or accident legacies. Coordinates expectations across technology sectors, investment communities, and policy regimes on which technologies are legitimate.
% TRANSFER_FUNCTION: Transfers policy legitimacy, regulatory approval, and investment capital from long-lived, centralized energy technologies (nuclear, coal-with-capture) to renewable and reversible technologies (solar, wind, batteries). Also transfers the burden of intergenerational stewardship forward: instead of future generations managing waste or contamination legacies, present-day decision-makers absorb the cost of solving intermittency and grid stability without long-lived technologies.
% ABSENT_VOICES: Future generations have no voice in the process, though the constraint nominally protects them. Additionally, workers in nuclear-fuel cycles, nuclear-dependent communities, and nations whose energy security depends on nuclear baseload are excluded from the standard-setting process. Grid-reliability engineers have limited influence — their optimization objectives are subordinated to the precautionary criterion, not negotiated as coequal constraints.
% DISAPPEARANCE_RATIONALE: If the precautionary legitimacy standard disappeared, nuclear capacity would expand rapidly as a carbon-free baseload option; investment portfolios would rebalance away from renewable-only strategies; grid-reliability optimization would not be subordinated to reversibility criteria; and the century-scale risk calculus would shift toward letting future generations solve their own problems rather than present-day decision-makers preventing long-term legacies. Trillions in global energy-infrastructure plans, carbon budgets, and technology R&D pipelines are calibrated to this constraint.
% FOUNDING_PROBLEM: Climate mitigation requires rapid decarbonization, but if the solution technologies create permanent waste or irreversible contamination legacies, the net effect is to trade the climate crisis for the waste-management crisis — solving today's emergency by ensuring tomorrow's permanence. The precautionary reading was constructed to prevent this inversion: ensure that technologies chosen to solve climate do not impose irreversible harms on the people who inherit them.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and waste-management experts corroborate that nuclear-waste legacies and accident scenarios would persist far beyond present-generation decision-making horizons, and that this represents a real choice point: are present-day societies willing to impose permanent liabilities to speed decarbonization? Reliability engineers and nuclear-technology advocates contest the framing, arguing that the founding problem misconstrues the actual risk hierarchy (climate impacts are certain and catastrophic; nuclear accidents are rare and engineerable) and that the one-generation reversibility threshold is arbitrary, not derived from the problem itself.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over 40 years, tracking the constraint's widening application as renewable capacity grows and nuclear retirements accelerate. Early (t=0) the precautionary standard is aspirational; by t=20-30 it becomes operationalized in major jurisdictions and investment frameworks, raising the effective extraction cost for non-compliant technologies. Theater ratio (0.41 at t=40) reflects the constraint's genuine coordination function — it does solve the legacy-cost problem — but a growing share of enforcement activity is defending renewable primacy against reliability challenges (intermittency, grid stability) rather than preventing waste crises (which the constraint makes moot by excluding the worst offenders). Suppression is high and stable (0.72) because the constraint's persistence depends on actively excluding nuclear deployment, not on voluntary coordination — the suppression machinery is needed to maintain the boundary against a powerful incumbent lobby. Accessibility collapse (0.62) and resistance (0.71) reflect a contested constraint: alternatives (reliability-primary, velocity-primary readings) remain live, grid engineers mount real resistance, and the one-generation boundary is openly disputed in policy circles. The coercion grid shows stakes inflation rising steeply at individual and class levels (from 0.70/0.64 to 0.82/0.78) because the constraint's impact on household energy prices and energy-sector employment concentrates on those least able to absorb volatility. Suppression at the organizational level (0.56→0.70) tracks the hardening of investment screens and regulatory barriers against nuclear. The grid pattern shows the precautionary standard is not a natural law — it is enforced, and enforcement intensity rises as the constraint becomes operationalized.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable-developer seat and the climate-advocate seat, the constraint is pure coordination — preventing climate suicide through waste-legacy reversal. From the nuclear-incumbent and grid-reliability seats, it is pure extraction — a regulatory weapon deployed to eliminate a competitor and to subordinate technical optimization to political risk aversion. These divergent framings arise from structural directionality: renewables have low d (they benefit without bearing the suppression cost); nuclear incumbents have high d (they absorb the extraction and the active enforcement machinery). The engine computes this divergence from beneficiary/victim declarations and exit-option asymmetry. The precautionary principle itself is neither beneficiary nor victim — it is a vindicated proposition; the readings of it differ on whether it should govern technology selection at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers (organized, mobile, global scope) sit near d=0.15: they benefit from legitimacy and capital flows, face minimal suppression cost, and can relocate if a reading shifts. Nuclear incumbents (institutional, constrained, civilizational horizon) sit near d=0.85: they absorb the constraint's full extraction (delegitimization, deployment barriers, capital flight), cannot exit the waste problem, and face active suppression. Grid reliability advocates (moderate power, constrained, national scope) sit near d=0.68: they absorb higher-cost grid management as the precautionary standard subordinates their optimization objective. Future generations nominally benefit (protected from waste legacies) but are structurally powerless and excluded — their d is undefined in the stakeholder model because they have no current agency; their protection is paternalistic, not negotiated. No directionality overrides are needed: the structural derivation produces the correct d values from beneficiary/victim + exit + power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ensure climate solutions do not create permanent waste crises — is live and actively contested. The constraint is NOT mandatrophic. The precautionary standard operationalizes a real coordination problem: selecting technologies that solve today's crisis without creating tomorrow's. The counter-reading (reliability-primary) disputes WHETHER the founding problem warrants the precautionary solution, not WHETHER the problem exists. This is a live dispute among active parties, not a dead mandate. The theater ratio (0.41) is moderate, reflecting the constraint's genuine function rather than pure performance. However, the suppression machinery (0.72) is substantial and actively maintained — the high suppression without parallel high theater would be a mandatrophy flag if the constraint did not carry a genuine coordination function. Because it does, the suppression is the enforced boundary, not theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_boundary_arbitrariness,
    'Is one generation (20-30 years) a principled threshold for ''reversibility,'' or an arbitrary political choice that reflects precautionary-principle advocacy rather than risk science?',
    'Intergenerational ethics literature and climate-risk analysis: what does science say about the timescale at which irreversibility becomes ethically decisive? Does the boundary track a genuine discontinuity in harm (e.g., species extinction thresholds, climate tipping points), or is it a normative choice about acceptable intergenerational burden-shifting?',
    'If the boundary is arbitrary, the precautionary reading''s legitimacy derives from political advocacy, not principled ethics or science — the constraint becomes a cover for renewable-technology preference, not a neutral coordination mechanism. If principled, the reading grounds itself in genuine irreversibility thresholds. Either way, the reliability-primary and velocity-primary readings can claim the boundary is miscalibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_boundary_arbitrariness, conceptual, 'Whether the one-generation reversibility threshold is grounded in ethical principle or political choice.').

omega_variable(
    future_generation_voice_deficit,
    'Can a legitimate constraint that nominally protects future generations proceed without their voice in negotiating the protection mechanism?',
    'Representational adequacy review: do climate-justice advocates, indigenous-rights advocates, or other proxy agents for long-term interest adequately represent future-generation perspectives, or is the constraint paternalistic? Can the constraint survive a counterfactual where future generations had a voting seat?',
    'If the constraint is indefensibly paternalistic, it loses legitimacy grounding despite achieving a socially desirable outcome (no nuclear waste legacies). If proxy representation is adequate, the paternalism is justified as unavoidable (future generations cannot consent). This affects whether the constraint is truly consensual (required for rope classification) or enforced (tangled_rope or snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_generation_voice_deficit, preference, 'Whether protecting future generations without their input invalidates the constraint''s legitimacy.').

omega_variable(
    worst_case_vs_expected_case_asymmetry,
    'Does the precautionary reading''s focus on worst-case failure modes (rare but severe) systematically bias it against technologies with low expected harm but high tail risk?',
    'Probabilistic risk analysis: compare the expected-harm integrals (probability × magnitude) of nuclear (low-probability, high-magnitude accidents) vs. renewable (high-probability, low-magnitude diffuse harms from manufacturing and e-waste). Does focusing on worst-case asymmetrically exclude nuclear while accepting renewable diffuse harms?',
    'If worst-case bias is systematic, the precautionary reading''s beneficiary set (renewables) rests on an asymmetric application of risk criteria — solar/wind avoid the precautionary screen because their harms are diffuse, not catastrophic, even if expected harms are comparable. This would lower the reading''s philosophical integrity but not change its structural classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worst_case_vs_expected_case_asymmetry, empirical, 'Whether worst-case focus asymmetrically favors diffuse over concentrated risks.').

omega_variable(
    committer_frame_kernel_identity,
    'Is the technology_legitimacy_kernel a single, durable commitment that three readings dispute, or are the three readings sufficiently different that they constitute separate kernels?',
    'Examine whether the three readings share a common text, doctrine, or authority that they interpret differently (same kernel, different readings), or whether they invoke entirely separate legitimacy traditions (separate kernels). Do nuclear advocates and renewable advocates debate the same criterion, or do they talk past each other because they invoke incommensurable standards?',
    'If separate kernels, the decomposition is wrong — these are three unrelated constraints, not three readings of one. If a single kernel with three readings, the network.affects_constraints edges and reading_relations correctly model the constraint family. This affects the corpus meta-analysis: are we measuring three political positions on one issue, or three separate issues that co-occur in energy discourse?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_identity, conceptual, 'Whether the three readings constitute interpretations of one kernel or separate constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(tech_tr_t5, observed).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(tech_tr_t10, observed).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(tech_tr_t15, observed).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tech_tr_t20, observed).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(tech_tr_t25, projected).
narrative_ontology:measurement(tech_tr_t30, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(tech_tr_t30, projected).
narrative_ontology:measurement(tech_tr_t40, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(tech_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(tech_be_t5, observed).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(tech_be_t10, observed).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(tech_be_t15, observed).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(tech_be_t20, observed).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(tech_be_t25, projected).
narrative_ontology:measurement(tech_be_t30, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(tech_be_t30, projected).
narrative_ontology:measurement(tech_be_t40, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(tech_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(tech_su_t5, observed).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(tech_su_t10, observed).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(tech_su_t15, observed).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(tech_su_t20, observed).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(tech_su_t25, projected).
narrative_ontology:measurement(tech_su_t30, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(tech_su_t30, projected).
narrative_ontology:measurement(tech_su_t40, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(tech_su_t40, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(tech_grid_01, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(tech_grid_02, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(class), 40, 0.72).
narrative_ontology:measurement(tech_grid_03, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(tech_grid_04, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(individual), 40, 0.75).
narrative_ontology:measurement(tech_grid_05, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(tech_grid_06, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(tech_grid_07, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(tech_grid_08, technology_legitimacy_kernel__precautionary_reading, accessibility_collapse(structural), 40, 0.58).
narrative_ontology:measurement(tech_grid_09, technology_legitimacy_kernel__precautionary_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(tech_grid_10, technology_legitimacy_kernel__precautionary_reading, resistance(class), 40, 0.78).
narrative_ontology:measurement(tech_grid_11, technology_legitimacy_kernel__precautionary_reading, resistance(individual), 0, 0.76).
narrative_ontology:measurement(tech_grid_12, technology_legitimacy_kernel__precautionary_reading, resistance(individual), 40, 0.8).
narrative_ontology:measurement(tech_grid_13, technology_legitimacy_kernel__precautionary_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(tech_grid_14, technology_legitimacy_kernel__precautionary_reading, resistance(organizational), 40, 0.75).
narrative_ontology:measurement(tech_grid_15, technology_legitimacy_kernel__precautionary_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(tech_grid_16, technology_legitimacy_kernel__precautionary_reading, resistance(structural), 40, 0.64).
narrative_ontology:measurement(tech_grid_17, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(class), 0, 0.64).
narrative_ontology:measurement(tech_grid_18, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(class), 40, 0.78).
narrative_ontology:measurement(tech_grid_19, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(individual), 0, 0.7).
narrative_ontology:measurement(tech_grid_20, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(individual), 40, 0.82).
narrative_ontology:measurement(tech_grid_21, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(tech_grid_22, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(organizational), 40, 0.72).
narrative_ontology:measurement(tech_grid_23, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(tech_grid_24, technology_legitimacy_kernel__precautionary_reading, stakes_inflation(structural), 40, 0.65).
narrative_ontology:measurement(tech_grid_25, technology_legitimacy_kernel__precautionary_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement(tech_grid_26, technology_legitimacy_kernel__precautionary_reading, suppression(class), 40, 0.75).
narrative_ontology:measurement(tech_grid_27, technology_legitimacy_kernel__precautionary_reading, suppression(individual), 0, 0.68).
narrative_ontology:measurement(tech_grid_28, technology_legitimacy_kernel__precautionary_reading, suppression(individual), 40, 0.8).
narrative_ontology:measurement(tech_grid_29, technology_legitimacy_kernel__precautionary_reading, suppression(organizational), 0, 0.56).
narrative_ontology:measurement(tech_grid_30, technology_legitimacy_kernel__precautionary_reading, suppression(organizational), 40, 0.7).
narrative_ontology:measurement(tech_grid_31, technology_legitimacy_kernel__precautionary_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(tech_grid_32, technology_legitimacy_kernel__precautionary_reading, suppression(structural), 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__precautionary_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% The technology_legitimacy_kernel decomposes into three structurally distinct constraints, each instantiating a different reading of how climate-mitigation technologies should be selected. The precautionary_reading prioritizes reversibility and intergenerational protection; the reliability_primacy_reading prioritizes grid stability and dispatchability; the velocity_primacy_reading prioritizes deployment speed and carbon-budget alignment. These readings are held simultaneously by different policy factions (environmental advocates vs. grid operators vs. climate-emergency advocates), neither logically forecloses the others (they coexist), and each produces different beneficiary/victim sets and different ε values. The decomposition is necessary per ε-invariance: a single constraint cannot carry multiple ε values. Each reading is authored separately, and all three are linked via network.affects_constraints to show the constraint family structure. The sibling readings are documented in cs_structure.reading_relations as 'coexists_with' — no single framework resolves the dispute, and different parties hold different readings as live positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
