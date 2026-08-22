% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Velocity-Primacy Legitimacy Criterion for Climate Mitigation Technology
 *   domain: energy_policy/climate_governance
 *
 * SUMMARY:
 *   This constraint is the velocity_primacy_reading of the
 *   technology_legitimacy_kernel. It governs which technologies are treated
 *   as legitimate instruments of climate mitigation by making deployability
 *   at scale within the carbon budget timeline (2030/2050 targets) the
 *   necessary and sufficient condition for legitimacy. Under this reading,
 *   renewables enter the beneficiary set because their deployment timelines
 *   align with NDC cycles, while nuclear is structurally excluded or
 *   marginalized due to assumed construction durations. Grid operators
 *   managing intermittency are treated as invisible cost-bearers. The
 *   constraint operates through green taxonomies, subsidy eligibility, and
 *   international climate finance criteria, actively enforced by national
 *   regulators and velocity-aligned advocacy coalitions.
 *
 * KEY AGENTS:
 *   - Renewable energy developers (organized/global) â primary beneficiaries of legitimacy and finance flows
 *   - Climate velocity advocates (organized/global) â beneficiaries of framing dominance and institutional influence
 *   - National climate regulators (institutional/national) â agenda-setters administering taxonomy and subsidy gates
 *   - Nuclear energy sector (powerful/global) â primary target, excluded from transition finance by timeline assumptions
 *   - Electric grid operators (powerful/national) â secondary target, bearing unfunded intermittency integration costs
 *   - Reliability-centered planners (moderate/national) â excluded voice, treated as obstructive to speed mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.62).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.55).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Velocity-Primacy Legitimacy Criterion for Climate Mitigation Technology").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'c734b45c-3c87-4b35-9b9a-de1a8d6d9656').
narrative_ontology:cs_kernel_codification('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', formalized).
narrative_ontology:cs_authority_grounding('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', lineage).
narrative_ontology:cs_interpretation_layer_present('c734b45c-3c87-4b35-9b9a-de1a8d6d9656').
narrative_ontology:cs_reading_relation('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', foundational, deployment_velocity_paramount).
narrative_ontology:cs_axiom_status(deployment_velocity_paramount, holdable).
narrative_ontology:cs_axiom_grounding('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', deployment_velocity_paramount, instrumental).
narrative_ontology:cs_reference_frame('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', timeline_bound_technology_legitimacy).
narrative_ontology:cs_drift_state('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', contemporary_policy_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c734b45c-3c87-4b35-9b9a-de1a8d6d9656', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_velocity_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_energy_sector).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, electric_grid_operators).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_imperative).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, rapid_energy_transition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive preferential access to climate finance, green taxonomy certification, and grid interconnection priority by virtue of technologies that can be deployed rapidly within NDC timelines. Their market expansion is structurally tied to the velocity criterion's dominance.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers, beneficiary,
    organized, biographical, constrained, global).

% Gain institutional influence, funding, and policy access by centering deployment speed as the exclusive metric of climate legitimacy. Their organizational success is coupled to the dominance of the velocity framing in national and international climate governance.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_velocity_advocates, beneficiary,
    organized, biographical, mobile, global).

% Codify the velocity criterion into green taxonomies, subsidy eligibility, and NDC implementation rules. They administer the boundary between legitimate and illegitimate climate technologies, determining which projects qualify for public and private transition finance.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, national_climate_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Excluded from green taxonomies and climate-transition financing in multiple jurisdictions because reactor construction timelines are assumed to exceed 2030 horizons, despite delivering near-zero carbon baseload generation. Capital and engineering talent are diverted to faster-deploying alternatives.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_energy_sector, payer,
    powerful, generational, constrained, global).

% Required to integrate high penetrations of variable renewable generation on timelines driven by policy velocity rather than grid physics, bearing the costs of balancing, storage, and transmission upgrades that the legitimacy criterion does not explicitly fund or acknowledge.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, electric_grid_operators, payer,
    powerful, biographical, constrained, national).

% Argue that grid stability and dispatchable capacity are prerequisites for legitimate climate technology, but their framework is treated as obstructive or outdated under velocity-primacy decision-making and is rarely admitted to policy hearings.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, reliability_centered_planners, excluded,
    moderate, generational, constrained, national).

% Provide the carbon budget assessments that the velocity framing cites as its empirical foundation, without themselves adjudicating which specific technologies satisfy the timeline criterion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, ipcc_climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligning global technology deployment decisions with the finite remaining carbon budget to avoid exceeding temperature thresholds, solving the coordination problem of fragmented national investment across incompatible technology pathways.
% TRANSFER_FUNCTION: Moves policy legitimacy, subsidies, and grid access priority from technologies with long deployment horizons to those with short deployment horizons, while transferring intermittency integration costs to grid operators.
% ABSENT_VOICES: Reliability-centered grid planners emphasizing dispatchability and baseload; precautionary analysts concerned with waste streams and catastrophic failure modes; nuclear engineering experts noting the mismatch between carbon urgency and technology-neutral decarbonization.
% DISAPPEARANCE_RATIONALE: If the velocity-primacy criterion vanished, national energy mixes would shift toward nuclear and long-duration storage, renewable subsidy structures would flatten toward technology-neutrality, and grid planning would re-center on reliability rather than speed of deployment.
% FOUNDING_PROBLEM: The climate clock is finite and carbon budgets are being exhausted; without speed-based prioritization, slow-footed technology choices risk overshooting temperature targets before deployment can alter emissions trajectories.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists attest to the carbon budget constraint from outside the renewable industry. However, the velocity-primacy framing itself is championed primarily by renewable advocacy coalitions and allied policy elites, while grid operators and nuclear engineers contest whether speed alone is the correct operational variable.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the criterion systematically transfers policy legitimacy and capital from slow-deploying to fast-deploying technologies, while imposing unfunded integration obligations on grid operators. Suppression (0.55) reflects active enforcement through taxonomy exclusion and subsidy denial rather than physical coercion. Theater ratio (0.40) acknowledges that urgency rhetoric has performative dimensions, even though the underlying carbon budget is empirically grounded. Accessibility collapse (0.70) is high because once 2030 targets are institutionalized, nuclear appears temporally impossible and alternatives to the velocity frame become cognitively unavailable in policy discourse. Resistance (0.50) is moderate, driven by nuclear industry litigation, grid operator warnings, and some industrial user lobbying.
 *
 * PERSPECTIVAL GAP:
 *   From the velocity-advocate seat, the constraint is survival-coordination: without speed, the carbon budget is breached and all other values become moot. From the nuclear sector seat, it is arbitrary exclusion that discards a proven decarbonization tool on the basis of administrative timeline rather than carbon efficacy. From the grid operator seat, it is an unfunded mandate that pairs legitimacy with speed while leaving system stability costs unaccounted. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy developers and climate velocity advocates are structural beneficiaries: the constraint subsidizes their market expansion and organizational influence (low d). National climate regulators enforce the boundary and accrue institutional mandate, sitting toward the beneficiary side. The nuclear energy sector is a declared victim: it bears the cost of capital exclusion and reputational delegitimation (high d). Electric grid operators are also victims: they absorb the physical and economic costs of velocity-driven intermittency without corresponding legitimacy or compensation (high d). The engine will compute divergent seat types from these directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled rope prevents mislabeling the constraint as pure extraction (snare), because the carbon budget coordination problem is genuine: fragmented national investment does need alignment with finite atmospheric capacity. It also prevents mislabeling as pure coordination (rope), because the asymmetry against nuclear and the cost-shifting to grid operators constitute structurally identifiable extraction that requires active enforcement to hold. If the founding problem were deadâif carbon budgets were no longer bindingâthe constraint would likely decay into a piton or snare; currently the founding problem remains contested, supporting the tangled rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Which reading of the technology_legitimacy_kernelâvelocity primacy, reliability primacy, or precautionary boundednessâcorrectly describes the structural constraint governing technology legitimacy?',
    'Comparative policy analysis tracking which criterion actually predicts technology approval and funding outcomes across jurisdictions over a full planning cycle.',
    'If reliability primacy is the true structure, beneficiaries become baseload providers and extractiveness drops; if precautionary, waste-stream analysts enter the victim set and extraction shifts to long-term legacy costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Structural ambiguity between competing readings of the same climate technology legitimacy kernel.').

omega_variable(
    nuclear_deployment_timeline_naturality,
    'Are nuclear construction timelines an immutable physical and regulatory constant, or are they artificially inflated by permitting and financing structures that the velocity criterion treats as given?',
    'Comparative build-time data from jurisdictions with streamlined regulatory frameworks and factory-built reactor programs.',
    'If timelines are artificially inflated, the velocity criterion functions as a constructed extraction mechanism rather than a neutral temporal filter, raising effective extraction and shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_deployment_timeline_naturality, empirical, 'Whether nuclear exclusion is natural or constructed by the velocity framing.').

omega_variable(
    intermittency_cost_accounting,
    'Are the grid integration and balancing costs imposed by rapid variable renewable deployment fully internalized in the velocity-primacy legitimacy framework, or borne as externalities by grid operators?',
    'Full-cost accounting audits comparing system-level LCOE with integration, balancing, and transmission-upgrade costs across high-penetration renewable grids.',
    'If externalized, grid operators are confirmed as victims and the coordination story is partially cover for cost-shifting; if internalized, the extraction is lower and the rope component stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermittency_cost_accounting, empirical, 'Cost allocation ambiguity for renewable integration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(techleg_vel_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(techleg_vel_tr_t7, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(techleg_vel_tr_t14, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement(techleg_vel_tr_t21, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 21, 0.42).
narrative_ontology:measurement(techleg_vel_tr_t28, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 28, 0.45).
narrative_ontology:measurement(techleg_vel_tr_t35, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 35, 0.4).

% Extraction over time
narrative_ontology:measurement(techleg_vel_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(techleg_vel_be_t7, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 7, 0.4).
narrative_ontology:measurement(techleg_vel_be_t14, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 14, 0.48).
narrative_ontology:measurement(techleg_vel_be_t21, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 21, 0.55).
narrative_ontology:measurement(techleg_vel_be_t28, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement(techleg_vel_be_t35, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(techleg_vel_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(techleg_vel_su_t7, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 7, 0.45).
narrative_ontology:measurement(techleg_vel_su_t14, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(techleg_vel_su_t21, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 21, 0.58).
narrative_ontology:measurement(techleg_vel_su_t28, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 28, 0.6).
narrative_ontology:measurement(techleg_vel_su_t35, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 35, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is the velocity_primacy_reading of the technology_legitimacy_kernel. The colloquial label 'legitimate climate mitigation technology' conflates three structurally distinct claims (velocity, reliability, precaution). Per the Îµ-invariance principle, each reading is modeled as a separate constraint with its own Îµ, stakeholders, and classification. This story links to its sibling readings in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
