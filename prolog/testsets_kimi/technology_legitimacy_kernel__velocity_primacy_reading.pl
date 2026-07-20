% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Velocity-Primacy Technology Legitimacy Rule for Climate Mitigation
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint is the velocity_primacy_reading of the
 *   technology_legitimacy_kernel: a contested commitment-system kernel that
 *   defines whether a technology is legitimate for climate mitigation. Under
 *   this reading, legitimacy is granted if and only if a technology can be
 *   deployed at scale within the remaining carbon budget timeline (2030/2050
 *   targets). The reading structurally benefits fast-deploying renewables,
 *   marginalizes nuclear due to construction duration, and imposes
 *   intermittency-management costs on grid operators. It is claimed here as
 *   tangled_rope because it coordinates a genuine collective-action problem
 *   (scarce capital under a carbon budget) while simultaneously extracting
 *   from excluded technologies and grid stability budgets.
 *
 * KEY AGENTS:
 *   - renewables_developers (beneficiary/powerful/constrained) â capture policy priority and finance
 *   - nuclear_industry (payer/powerful/constrained) â excluded by timeline criterion
 *   - grid_operators (payer/institutional/constrained) â bear integration and balancing costs
 *   - climate_policy_regime (agenda_setter/institutional/analytical) â sets legitimacy rules
 *   - climate_science_community (observer/analytical/analytical) â supplies carbon budget analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.7).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.7).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Velocity-Primacy Technology Legitimacy Rule for Climate Mitigation").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '5b8b5512-e178-4a65-b5aa-e1ea40a6836f').
narrative_ontology:cs_kernel_codification('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', formalized).
narrative_ontology:cs_authority_grounding('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', expertise).
narrative_ontology:cs_interpretation_layer_present('5b8b5512-e178-4a65-b5aa-e1ea40a6836f').
narrative_ontology:cs_reading_relation('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', foundational, deployment_velocity_is_sole_legitimacy_gate).
narrative_ontology:cs_axiom_status(deployment_velocity_is_sole_legitimacy_gate, holdable).
narrative_ontology:cs_axiom_grounding('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', deployment_velocity_is_sole_legitimacy_gate, instrumental).
narrative_ontology:cs_axiom('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', foundational, carbon_budget_timeline_trumps_all_other_selection_criteria).
narrative_ontology:cs_axiom_status(carbon_budget_timeline_trumps_all_other_selection_criteria, holdable).
narrative_ontology:cs_axiom_grounding('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', carbon_budget_timeline_trumps_all_other_selection_criteria, empirically_contingent).
narrative_ontology:cs_reference_frame('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', carbon_budget_velocity_framework).
narrative_ontology:cs_drift_state('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', contemporary_policy_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b8b5512-e178-4a65-b5aa-e1ea40a6836f', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewables_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive priority access to climate finance, green taxonomy recognition, and streamlined permitting under velocity-centered policy frameworks. Their business model and valuation depend on the legitimacy rule that equates fast deployment with climate virtue. Exit is constrained because their cost structures and market access are built around subsidy-dependent deployment schedules and renewable portfolio standards.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewables_developers, beneficiary,
    powerful, biographical, constrained, global).

% Bears the cost of exclusion from green taxonomies, sustainable finance classifications, and climate procurement rules because reactor construction timelines exceed 2030 thresholds. Low-carbon baseload generation is structurally de-legitimized by the velocity criterion despite near-zero operational emissions. Options are limited to lobbying for rule changes or accepting market erosion in jurisdictions adopting the velocity frame.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_industry, payer,
    powerful, generational, constrained, global).

% Must integrate rapidly increasing shares of variable renewable generation to meet velocity-target timelines, bearing the capital and operational costs of flexibility services, grid reinforcement, storage procurement, and balancing reserves. They are obligated to maintain reliability regardless of the generation mix imposed by climate targets and cannot opt out of the integration burden.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, biographical, constrained, national).

% Sets the technology legitimacy criteria through NDCs, green taxonomies, and IPCC mitigation pathway translations. Defines deployability within carbon budget timelines as the necessary and sufficient condition for climate finance and regulatory approval. Administers the constraint by converting carbon budget science into procurement mandates, subsidy eligibility rules, and exclusion criteria for slower technologies.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_policy_regime, agenda_setter,
    institutional, generational, analytical, global).

% Provides the carbon budget analysis that underpins the velocity-primacy framework. Observes whether technology deployment rates align with emissions pathways and remaining budgets, but does not directly capture financial gains or bear costs from the technology selection rule itself.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_science_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, renewables_developers).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how to allocate scarce policy attention, capital, and grid access when emissions must fall rapidly within a finite carbon budget by prioritizing technologies that can achieve scale before 2030 or 2050 thresholds.
% TRANSFER_FUNCTION: Moves policy legitimacy, subsidized capital, and guaranteed grid connection priority from slower-to-deploy low-carbon technologies and baseline grid stability budgets toward fast-deploying variable renewables; transfers intermittency integration and balancing costs to grid operators.
% ABSENT_VOICES: Nuclear engineers and energy systems analysts who argue that construction timelines are policy-driven rather than physically fixed; dispatchable generator operators who note that velocity targets ignore synchronous inertia and reliability constraints; future ratepayers who may inherit grids optimized for 2030 deployment speed rather than multi-decade resilience.
% DISAPPEARANCE_RATIONALE: If the velocity-primacy legitimacy rule vanished, capital would reallocate toward nuclear and other slow-build low-carbon baseload, green taxonomies would reopen to technologies with longer construction timelines, grid planning would shift toward reliability-centered procurement, and the 2030/2050 target architecture would lose its technology-filtering function.
% FOUNDING_PROBLEM: Climate change requires rapid emissions reduction; without a prioritization rule, investment and policy attention scatter across technologies with incompatible deployment timelines, risking exceedance of carbon budget limits.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and IPCC working groups attest to carbon budget mathematics from outside the direct beneficiary set. However, the claim that deployment velocity must be the sole or dominant technology selection criterion is contested by energy systems engineers, nuclear advocates, and grid reliability authorities who argue that reliability, material limits, and legacy-cost constraints are equally binding.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.7, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.70) is high because the constraint's core mechanismâlegitimacy gating by speedâsystematically transfers policy support and finance away from nuclear toward renewables while externalizing grid integration costs. Suppression (0.70) reflects active enforcement through green taxonomy exclusions, sustainable finance conditionalities, and procurement rules that render nuclear illegitimate regardless of emissions performance. Theater ratio (0.55) captures the growing performative gap between announced 2030 deployment targets and realized grid integration planning. Accessibility collapse (0.60) indicates that nuclear and dispatchable alternatives are increasingly excluded from climate legitimacy discourse, though not fully erased. Resistance (0.50) reflects organized pushback from nuclear vendors, reliability-focused engineers, and some grid operators. The founding problem remains live but contested, preventing mandatrophy while permitting extraction accumulation.
 *
 * PERSPECTIVAL GAP:
 *   The renewables_developer seat experiences the constraint as coordination: a necessary prioritization rule that directs capital toward scalable climate solutions within a limited carbon budget. The nuclear_industry and grid_operator seats experience the same structure as extraction: a legitimacy rule that denies them standing, finance, or cost recovery based on a single criterion (speed) that does not map to their contribution to decarbonization or reliability. The engine computes this divergence from the structural dataâbeneficiary versus victim declarations plus constrained exitâwithout the author reconciling the claim to any single seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   renewables_developers are declared beneficiaries and have constrained exit; structural derivation pushes d toward the beneficiary end (low d, damped effective extraction). nuclear_industry and grid_operators are declared victims with constrained exit; derivation pushes d toward the target end (high d, amplified effective extraction). The climate_policy_regime has analytical exit and is agenda_setter; it sits near the center but slightly toward beneficiary because it draws institutional authority from the constraint's operation. climate_science_community is observer/analytical and is neither beneficiary nor victim; d remains near neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this constraint as either pure coordination (Rope) or pure extraction (Snare). The founding problemârapid emissions reduction under a finite carbon budgetâis genuinely live, which would resist Piton classification. However, the velocity-primacy axiom operates as asymmetric extraction because it uses the urgency frame to exclude nuclear (a competing low-carbon solution) and to socialize intermittency costs onto grid operators. If the founding problem were dead, the high theater ratio and extraction would signal Piton or Snare. Because the problem is contested but live, the classification remains Tangled Rope: coordination with embedded extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    velocity_vs_reliability_framing,
    'Does the velocity-primacy reading structurally exclude reliability-centered technologies, or merely deprioritize them relative to speed?',
    'Comparative policy analysis of jurisdictions with strict velocity-primacy targets versus reliability-centered frameworks; measure whether nuclear and dispatchable capacity are legislatively excluded from green taxonomies or merely unsubsidized.',
    'If structurally excluded, the constraint operates as stronger asymmetric extraction; if merely deprioritized, it may function as weaker coordination with incidental displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(velocity_vs_reliability_framing, conceptual, 'Whether velocity framing actively excludes reliability technologies or merely ranks them lower.').

omega_variable(
    carbon_budget_bindingness,
    'Is the remaining carbon budget genuinely the binding constraint on technology selection, or are material, labor, and grid-integration limits equally or more constraining?',
    'Empirical assessment of actual deployment bottlenecks in high-renewable grids versus carbon budget trajectories; compare whether integration costs or carbon limits bind first.',
    'If carbon budget is not the sole binding constraint, the velocity axiom rests on an empirically contingent premise vulnerable to axiom_overriding drift, potentially reclassifying the constraint toward practice drift or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_budget_bindingness, empirical, 'Whether carbon budget timeline is the actual limiting factor.').

omega_variable(
    kernel_reading_alternatives,
    'How would the classification change if the reliability_primacy_reading or precautionary_reading were adopted as the governing framework?',
    'Construct parallel constraint stories for sibling readings and compare computed seat classifications and beneficiary/victim structures.',
    'The velocity reading produces renewables beneficiaries and grid-operator victims; reliability reading would invert victim/beneficiary structures for nuclear and baseload; precautionary reading would introduce material-footprint victims and exclude mining-intensive technologies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternatives, conceptual, 'Structural differences between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(velocity_primacy_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(velocity_primacy_tr_t5, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(velocity_primacy_tr_t10, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(velocity_primacy_tr_t15, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(velocity_primacy_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(velocity_primacy_tr_t25, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(velocity_primacy_tr_t30, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(velocity_primacy_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(velocity_primacy_be_t5, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(velocity_primacy_be_t10, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(velocity_primacy_be_t15, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(velocity_primacy_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(velocity_primacy_be_t25, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(velocity_primacy_be_t30, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(velocity_primacy_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(velocity_primacy_su_t5, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(velocity_primacy_su_t10, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(velocity_primacy_su_t15, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(velocity_primacy_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(velocity_primacy_su_t25, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(velocity_primacy_su_t30, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
