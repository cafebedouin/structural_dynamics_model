% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission (Hybrid Reading)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint describes a stratified state of disaster preparedness
 *   transmission, where competence in physical infrastructure (e.g.,
 *   engineering of bridges, power grids) remains high and actively
 *   maintained, but the knowledge and capacity for civilian coordination
 *   (e.g., effective evacuation, community self-organization) has
 *   significantly decayed. The system appears robust on the surface due to
 *   infrastructure, but is brittle in its social dimension. This is one
 *   reading of the 'preparedness_transmission' kernel, focusing on the hybrid
 *   nature of competence and decay.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.4).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.6).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7').
narrative_ontology:cs_kernel_codification('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', formalized).
narrative_ontology:cs_authority_grounding('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', lineage).
narrative_ontology:cs_interpretation_layer_present('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7').
narrative_ontology:cs_reading_relation('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', foundational, competence_is_stratified).
narrative_ontology:cs_axiom_status(competence_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', competence_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', foundational, social_competence_is_decayed).
narrative_ontology:cs_axiom_status(social_competence_is_decayed, holdable).
narrative_ontology:cs_axiom_grounding('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', social_competence_is_decayed, empirically_contingent).
narrative_ontology:cs_reference_frame('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', comprehensive_resilience_mandate).
narrative_ontology:cs_drift_state('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a28a6f01-4e4d-4ee0-9e32-67ef0137d3f7', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_engineers).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_populations).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, local_emergency_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining preparedness, they focus on infrastructure and formal plans, often overlooking the decay in civilian coordination capacity. They benefit from appearing competent in engineering, but bear the costs of coordination failures.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Maintain high competence in physical infrastructure design, construction, and maintenance. Their knowledge is actively transmitted and applied, leading to robust physical systems. They benefit from clear mandates and demonstrable success in their domain.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_engineers, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of coordination failures during disasters, experiencing confusion, delayed evacuation, and inadequate response, despite robust physical infrastructure. Their knowledge of emergency procedures and coordination mechanisms has atrophied.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_populations, payer,
    powerless, immediate, trapped, local).

% Are on the front lines during disasters and directly experience the gap between well-engineered infrastructure and poor civilian coordination. They pay in increased workload, stress, and compromised effectiveness due to the decayed knowledge base of the population they serve.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, local_emergency_responders, payer,
    moderate, biographical, constrained, local).

% Study the effectiveness of disaster preparedness systems, identifying the stratification between physical and social competence. They can propose reforms but are external to the operational enforcement of the constraint.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, policy_analysts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate national disaster response by ensuring robust physical infrastructure and a prepared civilian population, enabling efficient evacuation and aid distribution.
% TRANSFER_FUNCTION: Transfers resources and attention to maintaining physical infrastructure and formal plans, while implicitly transferring the burden of decayed civilian coordination knowledge onto local responders and the affected population.
% ABSENT_VOICES: Past generations who experienced large-scale civil defense drills and community-level preparedness would highlight the lost knowledge and the shift from active civilian participation to passive reliance on institutional response.
% DISAPPEARANCE_RATIONALE: If the stratified preparedness transmission vanished, the underlying decay in civilian coordination would become immediately apparent, leading to catastrophic failures in disaster response, forcing a complete re-evaluation and rebuilding of civil defense strategies from the ground up.
% FOUNDING_PROBLEM: To ensure national resilience against large-scale disasters and external threats by maintaining a high state of readiness across physical and social dimensions.
% FOUNDING_PROBLEM_CORROBORATION: Civil defense agencies attest the problem is live, focusing on infrastructure threats. Policy analysts and local responders corroborate the live status of the problem but highlight the failure in the civilian coordination aspect, pointing to post-disaster reports and sociological studies.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).
:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.4) is moderate, reflecting the implicit cost borne by civilian populations and local responders due to coordination failures, despite the benefits of robust infrastructure. Suppression (0.6) is higher, as the institutional focus on physical infrastructure implicitly suppresses the development and transmission of civilian coordination knowledge, making alternatives less accessible. Theater ratio (0.5) is significant because drills and formal plans often focus on the visible, engineered aspects, creating an illusion of comprehensive readiness while the social coordination aspect atrophies. The claimed type is Tangled Rope because there's a genuine coordination function (infrastructure) but also asymmetric extraction (decayed civilian capacity).
 *
 * PERSPECTIVAL GAP:
 *   Civil defense agencies may perceive the system as a Rope, focusing on the successful maintenance of physical infrastructure. Civilian populations and local responders, however, experience it as a Snare or Tangled Rope, due to the high costs of coordination failures and the lack of accessible alternatives for self-organization. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies and infrastructure engineers are beneficiaries, as their domains of competence are maintained and funded. Civilian populations and local emergency responders are payers, bearing the costs of coordination gaps. The constraint's structure implicitly directs resources and attention away from the social coordination aspect, creating a burden on those who need it most.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_of_civilian_coordination_decay,
    'How accurately can the decay in civilian coordination knowledge be measured, given its diffuse and tacit nature?',
    'Longitudinal sociological studies, post-disaster ethnographies, and comparative analysis of community resilience metrics across different generations and regions.',
    'If decay is underestimated, the constraint is more extractive and suppressive than currently assessed; if overestimated, the coordination function is stronger. This impacts the classification''s severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_civilian_coordination_decay, empirical, 'Uncertainty in quantifying the decay of tacit civilian coordination knowledge.').

omega_variable(
    institutional_incentives_for_stratification,
    'Are there institutional incentives that actively promote the focus on physical infrastructure over civilian coordination, beyond mere oversight?',
    'Analysis of funding allocations, career progression paths within civil defense agencies, and public relations strategies that emphasize visible infrastructure projects.',
    'If active incentives exist, the constraint leans more towards a Snare, as the stratification is a deliberate outcome of institutional self-interest rather than an accidental decay. This would increase the effective extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentives_for_stratification, conceptual, 'Whether the stratification is an unintended consequence or an actively incentivized outcome.').

omega_variable(
    hybrid_vs_husk_distinction,
    'Is the distinction between high physical competence and decayed social competence a genuine stratification, or is the entire system (including physical infrastructure maintenance) merely a ''husk'' of ritualistic performance?',
    'Empirical testing of physical infrastructure under extreme stress (e.g., earthquake simulations, cyberattack resilience tests) to verify actual operational competence, alongside detailed analysis of civilian response in real disasters.',
    'If physical competence is also found to be a ''husk'', this reading collapses into the ''husk_reading'', implying a much higher theater ratio and overall extractiveness, as the entire system is performative. If physical competence is robust, the hybrid reading is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_vs_husk_distinction, empirical, 'Distinguishing genuine stratified competence from overall ritualistic performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__hybrid_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__hybrid_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__hybrid_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(prep_tr_t50, preparedness_transmission__hybrid_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__hybrid_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__hybrid_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__hybrid_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(prep_be_t50, preparedness_transmission__hybrid_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__hybrid_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__hybrid_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__hybrid_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(prep_su_t50, preparedness_transmission__hybrid_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_transmission' kernel, focusing on the hybrid nature of competence and decay. It is linked to the 'competence_reading' and 'husk_reading' which represent alternative interpretations of the same underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
