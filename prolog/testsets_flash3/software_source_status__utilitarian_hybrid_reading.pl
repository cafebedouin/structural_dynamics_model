% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Reading of Software Source Status
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents a utilitarian hybrid reading of software
 *   source status, asserting that licensing decisions should maximize
 *   aggregate welfare, acknowledging that both open and proprietary models
 *   serve different contexts. It rejects categorical claims in favor of
 *   context-dependent optimization. This is one reading of the
 *   'software_source_status' kernel, which also includes
 *   'freedom_imperative_reading', 'pragmatic_development_reading', and
 *   'property_rights_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.3).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.2).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Reading of Software Source Status").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '42697f6b-baf4-45a8-840c-2a429c02d6eb').
narrative_ontology:cs_kernel_codification('42697f6b-baf4-45a8-840c-2a429c02d6eb', distributed).
narrative_ontology:cs_authority_grounding('42697f6b-baf4-45a8-840c-2a429c02d6eb', diffuse_epistemic).
narrative_ontology:cs_reading_relation('42697f6b-baf4-45a8-840c-2a429c02d6eb', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('42697f6b-baf4-45a8-840c-2a429c02d6eb', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('42697f6b-baf4-45a8-840c-2a429c02d6eb', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('42697f6b-baf4-45a8-840c-2a429c02d6eb', foundational, maximize_aggregate_welfare).
narrative_ontology:cs_axiom_status(maximize_aggregate_welfare, holdable).
narrative_ontology:cs_axiom_grounding('42697f6b-baf4-45a8-840c-2a429c02d6eb', maximize_aggregate_welfare, instrumental).
narrative_ontology:cs_axiom('42697f6b-baf4-45a8-840c-2a429c02d6eb', foundational, context_determines_optimal_licensing).
narrative_ontology:cs_axiom_status(context_determines_optimal_licensing, holdable).
narrative_ontology:cs_axiom_grounding('42697f6b-baf4-45a8-840c-2a429c02d6eb', context_determines_optimal_licensing, empirically_contingent).
narrative_ontology:cs_reference_frame('42697f6b-baf4-45a8-840c-2a429c02d6eb', optimal_software_ecosystem).
narrative_ontology:cs_drift_state('42697f6b-baf4-45a8-840c-2a429c02d6eb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('42697f6b-baf4-45a8-840c-2a429c02d6eb', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, economic_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from access to diverse software solutions, both open and proprietary, chosen for their fitness to purpose and overall societal value. Their welfare is the primary metric for licensing decisions.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Benefit from a flexible licensing environment that allows them to choose the model (open or proprietary) that best serves the aggregate welfare for their specific project. They may pay costs in terms of foregone proprietary revenue or increased competition.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_developers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, software_developers, payer).

% Benefit from an optimal mix of open and proprietary software that drives innovation, efficiency, and economic growth across various industries. They are concerned with the overall productivity and competitiveness of the economy.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, economic_sectors, beneficiary,
    institutional, generational, constrained, national).

% Would argue for the categorical superiority of open source on ethical grounds, viewing proprietary software as inherently unjust. Their perspective is not central to this utilitarian calculus, which prioritizes aggregate welfare over categorical freedom.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_advocates, excluded,
    organized, generational, identity_locked, global).

% Would argue for strong intellectual property rights as a fundamental entitlement, regardless of aggregate welfare outcomes. This reading would challenge their absolute claim to control and profit, subjecting it to a broader societal benefit test.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_software_firms, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates software development and deployment decisions towards maximizing overall societal benefit, allowing for a flexible mix of open and proprietary models based on context.
% TRANSFER_FUNCTION: Optimizes the flow of value (innovation, access, economic efficiency) across different software models, potentially transferring some control from individual creators to collective welfare considerations.
% ABSENT_VOICES: Categorical advocates for either pure open source (freedom imperative) or pure proprietary rights (property rights) are excluded, as their foundational axioms are not centered on aggregate welfare maximization.
% DISAPPEARANCE_RATIONALE: If this utilitarian framework for software licensing disappeared, the debate would likely revert to more absolutist positions (pure freedom vs. pure property rights), leading to less optimal and more fragmented software ecosystems, with less consideration for overall societal benefit.
% FOUNDING_PROBLEM: The problem of how to license software to best serve the public good, balancing innovation incentives with access and collaboration, recognizing that no single model is universally optimal.
% FOUNDING_PROBLEM_CORROBORATION: Economists, public policy researchers, and interdisciplinary technology ethicists outside of specific software camps corroborate the ongoing challenge of optimizing software licensing for societal welfare, citing diverse contexts where different models excel.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) and suppression (0.2) are relatively low because this reading aims for an optimal balance, not a categorical imposition. Any 'extraction' is a necessary trade-off for aggregate welfare, and 'suppression' is minimal, primarily involving the rejection of absolutist claims. The 'claimed_type' is 'rope' because it genuinely seeks to coordinate diverse interests towards a common good (aggregate welfare) with minimal coercive overhead, allowing for flexible solutions.
 *
 * PERSPECTIVAL GAP:
 *   This reading inherently creates a perspectival gap with those holding more absolutist views. While this reading sees itself as a flexible, welfare-maximizing coordination mechanism, those with strong ideological commitments to either open source freedom or proprietary rights would perceive it as either compromising fundamental principles or undermining legitimate property claims. The engine would compute different classifications for those seats based on their declared axioms.
 *
 * DIRECTIONALITY LOGIC:
 *   Software users, developers, and economic sectors are all beneficiaries, as the framework aims to optimize outcomes for them. There is no categorical victim set, as the goal is to find contexts where each model is beneficial. Those advocating for absolutist positions (e.g., pure open source or pure proprietary rights) are 'excluded' in the sense that their foundational axioms are not the primary drivers of this reading's decisions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_welfare_measurement,
    'How is ''aggregate welfare'' precisely defined and measured in the context of software licensing, and who adjudicates this measurement?',
    'Development of standardized, multi-stakeholder impact assessment frameworks for software licensing models, with independent auditing bodies.',
    'Ambiguity in welfare measurement could allow powerful actors to define ''welfare'' in self-serving ways, increasing effective extraction and shifting the classification towards a Tangled Rope or Snare. Clear, independent measurement would reinforce its Rope-like coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_welfare_measurement, conceptual, 'The definition and measurement of ''aggregate welfare'' are crucial for this reading''s legitimacy and classification.').

omega_variable(
    context_dependent_optimization_bias,
    'Is the ''context-dependent optimization'' genuinely neutral, or does it implicitly favor certain economic models or power structures?',
    'Longitudinal studies of licensing outcomes across diverse economic sectors and developer scales, disaggregated by power and market concentration.',
    'If the ''optimization'' consistently favors dominant proprietary models or large corporations, the constraint''s effective extraction would be higher for smaller developers and open-source projects, pushing it towards a Tangled Rope. If truly neutral, its Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_dependent_optimization_bias, empirical, 'Potential for bias in applying context-dependent optimization.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, distinct reading of the ''software_source_status'' kernel, or is it a strategic framing to mediate between more fundamental positions?',
    'Analysis of the philosophical coherence and independent grounding of the ''aggregate welfare'' axiom, distinct from instrumental arguments for other positions.',
    'If it''s a distinct reading, its classification as a Rope is robust. If it''s merely a strategic compromise, its stability and classification might be more dependent on the relative power of the underlying ''freedom'' and ''property'' readings, potentially making it a more fragile Scaffold or even a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies whether this reading is a fundamental position or a pragmatic bridge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(soft_su_t5, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(soft_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(soft_su_t15, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
