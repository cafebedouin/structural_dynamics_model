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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Drills and Inspections as Live Exercised Competence
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint describes drills and inspections as a mechanism for
 *   continuously re-validating and transmitting operational competence in
 *   disaster risk management. It focuses on the 'competence_reading' of the
 *   'preparedness_transmission' kernel, where the primary function is to
 *   build adaptive capacity and ensure that institutional memory is live,
 *   exercised knowledge, not merely ritual. Key agents include civil defense
 *   agencies, emergency responders, and the public, all of whom benefit from
 *   enhanced preparedness, while taxpayers and drill participants bear the
 *   costs. The metrics reflect a functional coordination mechanism with low
 *   extraction and theater, and moderate, consistent enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.45).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Drills and Inspections as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, 'cc211627-5628-4b0e-be71-d2c6b22f120f').
narrative_ontology:cs_kernel_codification('cc211627-5628-4b0e-be71-d2c6b22f120f', formalized).
narrative_ontology:cs_authority_grounding('cc211627-5628-4b0e-be71-d2c6b22f120f', expertise).
narrative_ontology:cs_interpretation_layer_present('cc211627-5628-4b0e-be71-d2c6b22f120f').
narrative_ontology:cs_reading_relation('cc211627-5628-4b0e-be71-d2c6b22f120f', preparedness_transmission__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('cc211627-5628-4b0e-be71-d2c6b22f120f', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('cc211627-5628-4b0e-be71-d2c6b22f120f', foundational, operational_knowledge_is_exercised).
narrative_ontology:cs_axiom_status(operational_knowledge_is_exercised, holdable).
narrative_ontology:cs_axiom_grounding('cc211627-5628-4b0e-be71-d2c6b22f120f', operational_knowledge_is_exercised, empirically_contingent).
narrative_ontology:cs_axiom('cc211627-5628-4b0e-be71-d2c6b22f120f', foundational, adaptive_capacity_is_measurable).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_measurable, holdable).
narrative_ontology:cs_axiom_grounding('cc211627-5628-4b0e-be71-d2c6b22f120f', adaptive_capacity_is_measurable, empirically_contingent).
narrative_ontology:cs_reference_frame('cc211627-5628-4b0e-be71-d2c6b22f120f', dynamic_adaptive_preparedness).
narrative_ontology:cs_drift_state('cc211627-5628-4b0e-be71-d2c6b22f120f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cc211627-5628-4b0e-be71-d2c6b22f120f', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, public_citizens).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, taxpayers).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, drill_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, emergency_responders).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, adaptive_capacity_doctrine).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, institutional_learning_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates, designs, and oversees drills and inspections. Benefits from a prepared populace and validated response capabilities. Bears the administrative and coordination costs.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Directly participate in drills, gaining critical experience and validating protocols. Benefit from enhanced coordination and personal competence. Pay in time, effort, and exposure to simulated stress.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_responders, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, emergency_responders, payer).

% Benefit from a more resilient community and effective disaster response, reducing risk to life and property. Their participation in drills is often voluntary but encouraged.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, public_citizens, beneficiary,
    moderate, biographical, mobile, local).

% Fund the entire system of drills, inspections, and preparedness infrastructure through taxes. Bear the financial cost of maintaining this competence.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Individuals (e.g., employees, students) whose participation in drills is mandatory due to institutional policy or legal requirements. Pay in time, disruption, and mental effort.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, drill_participants, payer,
    moderate, immediate, constrained, local).

% Evaluate the effectiveness of preparedness systems, seeking evidence of genuine adaptive capacity and identifying gaps between reported compliance and actual operational knowledge. Their analysis can influence policy and funding.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, skeptical_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of diverse emergency services, government agencies, and the public to ensure a coherent and effective response to disaster scenarios, building shared operational knowledge and adaptive capacity.
% TRANSFER_FUNCTION: Transfers operational knowledge, practical skills, and validated protocols from planning stages into live, exercised competence across all levels of disaster response, from agencies to individual citizens.
% ABSENT_VOICES: Marginalized communities or individuals who distrust authorities may be less engaged in drills, leading to gaps in preparedness that are not identified by standard inspection regimes. Their absence means their specific vulnerabilities and needs are not fully integrated into the competence framework.
% DISAPPEARANCE_RATIONALE: If drills and inspections vanished, the collective operational knowledge and adaptive capacity for disaster response would rapidly decay. Protocols would become theoretical, coordination would fragment, and real-world responses would be significantly less effective, leading to greater loss of life and property.
% FOUNDING_PROBLEM: Fragmented, uncoordinated, and inexperienced responses to natural and man-made disasters, leading to avoidable casualties and prolonged recovery times.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster studies, historical records of past disaster responses, and ongoing risk assessments from outside the civil defense agencies consistently corroborate that the threat of uncoordinated disaster response remains live, necessitating continuous preparedness efforts.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.18) because the primary goal is public safety and competence, not rent collection, with costs mainly administrative overhead. Suppression is moderate (0.45) as participation in drills and adherence to inspection standards are often mandatory, but not coercively enforced beyond compliance. Theater ratio is low (0.12) because the focus is on genuine learning and validation, with performance serving functional ends. Accessibility collapse is high (0.8) because there are few effective alternatives to live, exercised drills for building adaptive capacity. Resistance is low (0.3) as the value of preparedness is widely recognized, though some friction exists due to time and effort commitments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil defense agencies and emergency responders, this constraint is a vital coordination mechanism that builds essential competence. From the perspective of taxpayers and drill participants, it represents a necessary cost and obligation. The engine will compute these different experiences based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies, emergency responders, and public citizens are beneficiaries, gaining safety and validated competence. Taxpayers and drill participants are payers, bearing the financial and time costs. The moderate suppression ensures participation, but the overall structure is designed for collective benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ritual_ambiguity,
    'Is the observed performance of drills and inspections genuinely building adaptive capacity and live knowledge, or has it degraded into a memorial ritual (husk_reading) where operational knowledge has hollowed out?',
    'Evaluation of drill outcomes against novel, unscripted scenarios; post-incident analysis of improvisation and adaptation; independent audits of learning transfer and retention.',
    'If found to be a husk_reading, the constraint''s effective extractiveness would be higher (as resources are consumed for non-functional performance) and its classification would shift towards Piton or Snare, reflecting a loss of genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ritual_ambiguity, empirical, 'Distinguishing genuine competence building from performative ritual.').

omega_variable(
    uniform_vs_stratified_competence,
    'Is the competence transmission uniform across all sectors and levels, or is it stratified (hybrid_reading), with high competence in some areas (e.g., engineering infrastructure) but decay in others (e.g., civilian coordination)?',
    'Granular, sector-specific assessments of preparedness, comparing technical infrastructure readiness with community-level coordination and public engagement metrics.',
    'If stratified, the overall ''competence_reading'' might still hold, but the analysis would need to decompose into sub-constraints reflecting varying levels of effectiveness and extraction across different domains, potentially revealing localized Snare or Piton dynamics within the broader Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_vs_stratified_competence, empirical, 'Assessing the uniformity of competence across different domains of preparedness.').

omega_variable(
    adaptive_capacity_measurement_validity,
    'Are the metrics used to assess adaptive capacity and novel failure signature recognition truly valid, or do they primarily measure compliance with pre-defined procedures?',
    'Development and validation of new assessment tools focused on emergent problem-solving, cross-domain improvisation, and learning from unexpected events, rather than adherence to checklists.',
    'If current metrics are found to primarily measure compliance, the actual adaptive capacity might be lower than reported, pushing the constraint''s effective theater_ratio upward and potentially shifting its classification towards Piton, as the claimed function is not fully realized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_capacity_measurement_validity, conceptual, 'Validity of adaptive capacity measurement in drills and inspections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t2000, preparedness_transmission__competence_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(prep_tr_t2004, preparedness_transmission__competence_reading, theater_ratio, 2004, 0.11).
narrative_ontology:measurement(prep_tr_t2008, preparedness_transmission__competence_reading, theater_ratio, 2008, 0.11).
narrative_ontology:measurement(prep_tr_t2012, preparedness_transmission__competence_reading, theater_ratio, 2012, 0.12).
narrative_ontology:measurement(prep_tr_t2016, preparedness_transmission__competence_reading, theater_ratio, 2016, 0.12).
narrative_ontology:measurement(prep_tr_t2020, preparedness_transmission__competence_reading, theater_ratio, 2020, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t2000, preparedness_transmission__competence_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(prep_be_t2004, preparedness_transmission__competence_reading, base_extractiveness, 2004, 0.16).
narrative_ontology:measurement(prep_be_t2008, preparedness_transmission__competence_reading, base_extractiveness, 2008, 0.17).
narrative_ontology:measurement(prep_be_t2012, preparedness_transmission__competence_reading, base_extractiveness, 2012, 0.17).
narrative_ontology:measurement(prep_be_t2016, preparedness_transmission__competence_reading, base_extractiveness, 2016, 0.18).
narrative_ontology:measurement(prep_be_t2020, preparedness_transmission__competence_reading, base_extractiveness, 2020, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t2000, preparedness_transmission__competence_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(prep_su_t2004, preparedness_transmission__competence_reading, suppression_requirement, 2004, 0.42).
narrative_ontology:measurement(prep_su_t2008, preparedness_transmission__competence_reading, suppression_requirement, 2008, 0.43).
narrative_ontology:measurement(prep_su_t2012, preparedness_transmission__competence_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(prep_su_t2016, preparedness_transmission__competence_reading, suppression_requirement, 2016, 0.45).
narrative_ontology:measurement(prep_su_t2020, preparedness_transmission__competence_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
