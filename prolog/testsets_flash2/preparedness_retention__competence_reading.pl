% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: disaster_preparedness/governance/institutional_memory
 *
 * SUMMARY:
 *   This constraint describes preparedness as a continuous, active process of
 *   maintaining operational capacity through drills, inspections, and
 *   training. It emphasizes that true preparedness is 'live exercised
 *   knowledge' rather than static plans or symbolic gestures. This reading
 *   prioritizes genuine skill retention and adaptive capacity, with resource
 *   allocation optimized towards these goals. The primary beneficiary is
 *   population safety, and any 'victim' is typically limited to fiscal
 *   efficiency if over-invested, rather than direct extraction from a
 *   specific group. This is one reading of the 'preparedness_retention'
 *   kernel, contrasting with readings that emphasize ceremonial aspects or
 *   stratified competence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.05).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/governance/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '8c0f5318-f08c-435b-b6ad-da05c30dd3d7').
narrative_ontology:cs_kernel_codification('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', formalized).
narrative_ontology:cs_authority_grounding('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', expertise).
narrative_ontology:cs_interpretation_layer_present('8c0f5318-f08c-435b-b6ad-da05c30dd3d7').
narrative_ontology:cs_reading_relation('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', foundational, competence_is_exercised_knowledge).
narrative_ontology:cs_axiom_status(competence_is_exercised_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', competence_is_exercised_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', secondary, adaptive_capacity_is_paramount).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', adaptive_capacity_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', continuous_learning_system).
narrative_ontology:cs_drift_state('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8c0f5318-f08c-435b-b6ad-da05c30dd3d7', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_safety).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, public_health_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, general_public).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, fiscal_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate recipient of effective preparedness, benefiting from reduced harm and faster recovery during crises. This is an abstract good, not an active agent.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population_safety, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(preparedness_retention__competence_reading, population_safety).

% Actively participate in and lead drills, inspections, and training. Their competence is directly maintained by these practices, and they advocate for resource allocation to support them. They are both beneficiaries of the system's effectiveness and administrators of its practices.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_responders, agenda_setter,
    organized, biographical, constrained, local).

% Develop and implement preparedness protocols, often based on scientific evidence and best practices. They benefit from a system that genuinely retains operational capacity and are key drivers of its design and evaluation.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Allocate budgets for preparedness activities. From their perspective, over-investment in drills and inspections beyond a cost-effective threshold could be seen as a 'victim' of fiscal inefficiency, though the primary benefit is public safety.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, fiscal_authorities, payer,
    institutional, immediate, mobile, national).

% Benefits from the safety and resilience provided by a genuinely prepared system, but typically does not directly participate in or fund the specific competence-preserving practices.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, general_public, beneficiary,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions and knowledge of diverse emergency and public safety organizations, ensuring they can effectively respond to disasters through shared, exercised operational capacity.
% TRANSFER_FUNCTION: Transfers resources (time, funding, personnel) into training, drills, and inspections, which in turn generate and retain operational competence and institutional memory, ultimately transferring safety and resilience to the population.
% ABSENT_VOICES: Those who would prioritize short-term fiscal savings over long-term preparedness, or those who believe preparedness is a 'set-and-forget' task, are often excluded from the operational planning and resource allocation discussions, where the value of continuous exercise is understood.
% DISAPPEARANCE_RATIONALE: If the practices of live exercised knowledge vanished, operational capacity would rapidly degrade, institutional memory would atrophy, and the ability to respond effectively to disasters would collapse, leading to significantly higher human and economic costs during crises. The entire disaster response ecosystem would need to be rebuilt from scratch.
% FOUNDING_PROBLEM: The historical problem of inadequate response to recurring disasters due to lack of practiced coordination, outdated knowledge, and insufficient operational skill among responders.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster reviews, post-incident analyses, and academic studies consistently corroborate that effective preparedness, rooted in live exercised knowledge, remains a critical and ongoing challenge, especially in the face of evolving threats (e.g., climate change, novel pandemics). This is attested by disaster researchers and international aid organizations, not just the direct beneficiaries.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the primary 'cost' is the investment in training and exercises, which directly translates to public benefit (safety). There is minimal extraction from any specific group beyond the necessary allocation of public funds. Suppression is low (0.05) as the constraint is largely self-enforcing through professional standards and the clear benefits of effective response. Theater ratio is low (0.1) because the focus is on functional competence, not symbolic performance. Accessibility collapse is high (0.8) because neglecting live exercise fundamentally collapses the ability to respond effectively. Resistance is low (0.1) as the value of genuine competence is widely accepted by practitioners.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of emergency responders and public health agencies, this constraint is a clear Rope, providing essential coordination and capacity. From the perspective of fiscal authorities, it might be viewed as a necessary but potentially 'expensive' Rope, where the 'victim' is fiscal efficiency if the investment is perceived as excessive, though this is a minor concern compared to the public safety benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Population safety, emergency responders, and public health agencies are beneficiaries, as they directly gain from or administer the effective functioning of the system. Fiscal authorities are payers, bearing the cost of investment, but are not 'victims' in an extractive sense, as the investment yields public good. The general public is a diffuse beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently resists mandatrophy by defining preparedness as 'live exercised knowledge.' If the knowledge ceases to be live or exercised, it ceases to be preparedness under this definition. The classification as a Rope (or a very low-extraction Tangled Rope if fiscal efficiency is strongly considered a victim) prevents mislabeling genuine, functional coordination as mere extraction or inert performance, as long as the practices genuinely maintain competence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_validity,
    'Are the drills and inspections genuinely measuring and preserving operational competence, or are they susceptible to ''teaching to the test'' and superficial compliance?',
    'Independent, unannounced, and adaptive stress tests that simulate novel scenarios, combined with post-incident analysis that correlates drill performance with real-world outcomes.',
    'If competence measurement is found to be superficial, the constraint''s true theater_ratio would be higher, and its extractiveness might increase if resources are being consumed for performative rather than functional ends, potentially shifting it towards a Piton or even a Snare if the performance actively misleads.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_measurement_validity, empirical, 'Assesses the fidelity of competence-preserving practices.').

omega_variable(
    resource_allocation_efficiency,
    'Is the allocation of resources for preparedness optimized for skill retention and adaptive capacity, or are there significant inefficiencies that divert funds from effective practice?',
    'Detailed cost-benefit analysis of different preparedness strategies, benchmarking against international best practices, and auditing resource utilization in exercises.',
    'Significant inefficiencies would increase the effective extractiveness of the constraint, as more resources are consumed for less actual competence, potentially shifting it towards a Tangled Rope where fiscal authorities are clearer victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Evaluates whether preparedness funding is effectively translated into competence.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''competence_reading'' truly distinct from the ''husk_reading'' and ''hybrid_reading'' in practice, or do elements of ceremonial performance or stratified competence inevitably creep into even well-intentioned preparedness systems?',
    'Longitudinal ethnographic studies of preparedness organizations, comparing declared intent with observed practice, and analyzing the ''ceremony-to-competence'' ratio in different contexts. This would also involve analyzing the structural conditions that lead to the ''husk'' or ''hybrid'' states.',
    'If the competence reading is found to be consistently undermined by ceremonialism or stratification in practice, the classification of this specific constraint might need to be re-evaluated towards a higher theater_ratio or a more complex, multi-seat classification reflecting the hybrid reality, even if the ideal remains a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Examines the practical boundaries between the different readings of preparedness retention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_retention__competence_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(prep_tr_t1998, preparedness_retention__competence_reading, theater_ratio, 1998, 0.09).
narrative_ontology:measurement(prep_tr_t2006, preparedness_retention__competence_reading, theater_ratio, 2006, 0.09).
narrative_ontology:measurement(prep_tr_t2014, preparedness_retention__competence_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(prep_tr_t2024, preparedness_retention__competence_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_retention__competence_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(prep_be_t1998, preparedness_retention__competence_reading, base_extractiveness, 1998, 0.12).
narrative_ontology:measurement(prep_be_t2006, preparedness_retention__competence_reading, base_extractiveness, 2006, 0.13).
narrative_ontology:measurement(prep_be_t2014, preparedness_retention__competence_reading, base_extractiveness, 2014, 0.14).
narrative_ontology:measurement(prep_be_t2024, preparedness_retention__competence_reading, base_extractiveness, 2024, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
