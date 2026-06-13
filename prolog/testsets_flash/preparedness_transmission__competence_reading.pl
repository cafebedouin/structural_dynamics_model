% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission (Competence Reading): Drills and Inspections as Live Exercised Knowledge
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint describes the 'competence reading' of preparedness
 *   transmission, where drills and inspections are genuinely effective
 *   mechanisms for re-validating and transmitting operational capability. It
 *   assumes high adaptive capacity, where inspectors recognize novel failure
 *   signatures and drill participants improvise effectively under scenario
 *   variation. This reading emphasizes the continuous, live exercise of
 *   knowledge as essential for effective disaster risk management and
 *   institutional memory.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.2).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission (Competence Reading): Drills and Inspections as Live Exercised Knowledge").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '2c6bf060-6f28-4b07-8679-a1c082e64e68').
narrative_ontology:cs_kernel_codification('2c6bf060-6f28-4b07-8679-a1c082e64e68', formalized).
narrative_ontology:cs_authority_grounding('2c6bf060-6f28-4b07-8679-a1c082e64e68', expertise).
narrative_ontology:cs_interpretation_layer_present('2c6bf060-6f28-4b07-8679-a1c082e64e68').
narrative_ontology:cs_reading_relation('2c6bf060-6f28-4b07-8679-a1c082e64e68', preparedness_transmission__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('2c6bf060-6f28-4b07-8679-a1c082e64e68', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('2c6bf060-6f28-4b07-8679-a1c082e64e68', foundational, operational_knowledge_is_exercised).
narrative_ontology:cs_axiom_status(operational_knowledge_is_exercised, holdable).
narrative_ontology:cs_axiom_grounding('2c6bf060-6f28-4b07-8679-a1c082e64e68', operational_knowledge_is_exercised, empirically_contingent).
narrative_ontology:cs_axiom('2c6bf060-6f28-4b07-8679-a1c082e64e68', foundational, adaptive_capacity_is_measurable).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_measurable, holdable).
narrative_ontology:cs_axiom_grounding('2c6bf060-6f28-4b07-8679-a1c082e64e68', adaptive_capacity_is_measurable, empirically_contingent).
narrative_ontology:cs_reference_frame('2c6bf060-6f28-4b07-8679-a1c082e64e68', continuous_learning_system).
narrative_ontology:cs_drift_state('2c6bf060-6f28-4b07-8679-a1c082e64e68', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2c6bf060-6f28-4b07-8679-a1c082e64e68', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, public_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, drill_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, mandates, and evaluates drills and inspections. Benefits from a competent, responsive system. Bears the cost of continuous training and infrastructure maintenance. Their legitimacy depends on effective preparedness.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Participate in drills, gaining practical experience and validating their skills. Benefit from clear protocols and effective coordination. Their professional identity is tied to their competence in crisis response.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, first_responders, beneficiary,
    organized, biographical, constrained, local).

% Are the ultimate beneficiaries of effective disaster preparedness, relying on the system to protect them during emergencies. They bear indirect costs through taxes and occasional disruption from drills.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, public_citizens, beneficiary,
    powerless, immediate, trapped, local).

% Are responsible for identifying novel failure signatures and ensuring adaptive capacity. Their expertise is critical to maintaining the 'live' aspect of the knowledge. They benefit from the system's continuous improvement.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inspectors_and_evaluators, agenda_setter,
    institutional, biographical, mobile, national).

% Invest time and effort in participating in drills, which can be disruptive to routine operations. They benefit from improved safety and coordination, but bear the direct costs of participation.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, drill_participants, payer,
    moderate, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that diverse agencies, first responders, and civilian populations can coordinate effectively and adaptively during a disaster, by regularly exercising and validating their collective capabilities.
% TRANSFER_FUNCTION: Transfers practical, exercised knowledge and adaptive capacity across generations of personnel and evolving threat landscapes, from experienced practitioners to new recruits, and from planning to execution.
% ABSENT_VOICES: Those who would argue for a purely theoretical or 'paper' preparedness system, or those who believe that real-world experience is sufficient without formal drills, are absent. Their perspective would challenge the necessity of continuous, live exercise.
% DISAPPEARANCE_RATIONALE: If drills and inspections as live exercised knowledge vanished, the system would rapidly degrade into a 'husk' of formal procedures without actual competence. Coordination would fail in real crises, leading to increased casualties and economic damage, forcing a complete reorganization of disaster response.
% FOUNDING_PROBLEM: The problem of maintaining operational competence and adaptive capacity in complex disaster response systems across personnel turnover and evolving threats, where theoretical knowledge alone is insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management experts, military strategists, and public safety officials consistently corroborate that the problem of maintaining live, exercised competence is ongoing and critical. Historical analysis of disaster responses where preparedness failed also supports this, from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary function is coordination and public benefit, with costs primarily being the necessary overhead of training and maintenance. Suppression is also low (0.2) as participation is largely driven by professional duty and public safety, rather than coercion. Theater ratio is very low (0.05) because the activities are genuinely functional and aimed at real-world competence, not mere performance. The metrics reflect a system that is largely effective and efficient in its stated purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil defense agencies and first responders, this is a clear Rope, a necessary and beneficial coordination mechanism. From the perspective of public citizens, it is also a Rope, providing essential safety. The costs borne by drill participants are seen as a fair exchange for the benefits of competence and safety.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies and first responders are direct beneficiaries, gaining competence and legitimacy. Public citizens are the ultimate beneficiaries of a safe and prepared society. Drill participants are payers in terms of time and effort, but also beneficiaries of improved skills and safety. Inspectors are agenda-setters and beneficiaries of a well-functioning system. All agents are oriented towards the constraint's success, reflecting its coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly counters mandatrophy by asserting that the founding problem (maintaining live competence) is still 'live' and that the constraint's activities are genuinely functional, not merely inertial. The low theater ratio and sustained low extractiveness indicate that the mandate has not atrophied; the system continues to deliver its intended coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_ambiguity,
    'Is this system truly maintaining live exercised knowledge (competence reading), or has it degraded into a ritualistic performance where operational knowledge has hollowed out (husk reading)?',
    'Empirical analysis of drill outcomes under novel, high-stress scenarios; assessment of improvisation capacity and recognition of emergent failure modes by inspectors. Comparison of actual crisis response effectiveness with drill performance.',
    'If resolved towards the ''husk reading'', the constraint''s extractiveness and theater ratio would be significantly higher, and its classification would shift towards a Piton or Snare, as resources are consumed for performative rather than functional ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_ambiguity, empirical, 'Distinguishing genuine competence from ritualistic performance in preparedness systems.').

omega_variable(
    competence_vs_hybrid_ambiguity,
    'Is competence uniformly high across all domains (competence reading), or is it stratified, with high physical infrastructure competence but decayed civilian coordination knowledge (hybrid reading)?',
    'Granular assessment of competence across different sub-domains (e.g., engineering vs. public communication, logistics, and social coordination). Analysis of inter-agency and civilian-military coordination during drills and actual events.',
    'If resolved towards the ''hybrid reading'', the constraint would decompose into multiple linked constraints, some potentially retaining a Rope classification (e.g., engineering competence) and others shifting towards Piton or Snare (e.g., decayed civilian coordination), reflecting differential extraction and theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_hybrid_ambiguity, empirical, 'Assessing the uniformity of competence across different aspects of preparedness transmission.').

omega_variable(
    kernel_reading_identification,
    'This constraint is a ''competence_reading'' of the ''preparedness_transmission'' kernel. What structural elements would change if a ''husk_reading'' or ''hybrid_reading'' were adopted?',
    'Conceptual analysis of the core premises of each reading and their implications for metrics like extractiveness, suppression, and theater ratio, as well as stakeholder roles and exit options.',
    'A ''husk_reading'' would imply higher theater and lower genuine coordination, shifting classification towards Piton. A ''hybrid_reading'' would imply differential competence and extraction across sub-domains, leading to a decomposition into multiple linked constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documenting the structural differences between the ''competence_reading'' and its sibling readings of the ''preparedness_transmission'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1950, preparedness_transmission__competence_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(prep_tr_t1965, preparedness_transmission__competence_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(prep_tr_t1980, preparedness_transmission__competence_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(prep_tr_t1995, preparedness_transmission__competence_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(prep_tr_t2010, preparedness_transmission__competence_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(prep_tr_t2024, preparedness_transmission__competence_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(prep_be_t1950, preparedness_transmission__competence_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(prep_be_t1965, preparedness_transmission__competence_reading, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(prep_be_t1980, preparedness_transmission__competence_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(prep_be_t1995, preparedness_transmission__competence_reading, base_extractiveness, 1995, 0.14).
narrative_ontology:measurement(prep_be_t2010, preparedness_transmission__competence_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(prep_be_t2024, preparedness_transmission__competence_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1950, preparedness_transmission__competence_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(prep_su_t1965, preparedness_transmission__competence_reading, suppression_requirement, 1965, 0.17).
narrative_ontology:measurement(prep_su_t1980, preparedness_transmission__competence_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(prep_su_t1995, preparedness_transmission__competence_reading, suppression_requirement, 1995, 0.19).
narrative_ontology:measurement(prep_su_t2010, preparedness_transmission__competence_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(prep_su_t2024, preparedness_transmission__competence_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('competence_reading') of the 'preparedness_transmission' kernel. It describes drills and inspections as genuinely effective for transmitting live, exercised knowledge. Sibling readings ('husk_reading' and 'hybrid_reading') describe scenarios where competence has atrophied or become stratified, leading to different structural classifications and metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
