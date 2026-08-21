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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission (Competence Reading)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint describes drills and inspections as a mechanism for
 *   transmitting live, exercised competence in disaster risk management. It
 *   emphasizes the continuous re-validation of capability through practice,
 *   where each generation of participants and inspectors actively contributes
 *   to and benefits from a high adaptive capacity. This reading posits that
 *   the system is genuinely effective, with low extraction and minimal
 *   theatricality, focused on solving a critical coordination problem for
 *   public safety.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.1).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.05).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission (Competence Reading)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '1f8d97f5-b740-4f86-9461-c01d38c9387b').
narrative_ontology:cs_kernel_codification('1f8d97f5-b740-4f86-9461-c01d38c9387b', formalized).
narrative_ontology:cs_authority_grounding('1f8d97f5-b740-4f86-9461-c01d38c9387b', expertise).
narrative_ontology:cs_interpretation_layer_present('1f8d97f5-b740-4f86-9461-c01d38c9387b').
narrative_ontology:cs_reading_relation('1f8d97f5-b740-4f86-9461-c01d38c9387b', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f8d97f5-b740-4f86-9461-c01d38c9387b', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1f8d97f5-b740-4f86-9461-c01d38c9387b', foundational, operational_knowledge_is_exercised_knowledge).
narrative_ontology:cs_axiom_status(operational_knowledge_is_exercised_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('1f8d97f5-b740-4f86-9461-c01d38c9387b', operational_knowledge_is_exercised_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('1f8d97f5-b740-4f86-9461-c01d38c9387b', foundational, adaptive_capacity_is_continuously_revalidated).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_continuously_revalidated, holdable).
narrative_ontology:cs_axiom_grounding('1f8d97f5-b740-4f86-9461-c01d38c9387b', adaptive_capacity_is_continuously_revalidated, empirically_contingent).
narrative_ontology:cs_reference_frame('1f8d97f5-b740-4f86-9461-c01d38c9387b', continuous_competence_validation).
narrative_ontology:cs_drift_state('1f8d97f5-b740-4f86-9461-c01d38c9387b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1f8d97f5-b740-4f86-9461-c01d38c9387b', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, first_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, public_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, budget_allocators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, mandates, and oversees drills and inspections. Benefits from a competent, responsive system. Bears the cost of planning and executing these exercises, but also gains legitimacy and funding from demonstrated capability.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Participates in drills, gaining practical experience and validating their skills. Benefits from clear protocols and effective inter-agency coordination. Their professional identity is tied to their competence in crisis situations.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, first_responders, beneficiary,
    organized, biographical, constrained, local).

% Benefits from a well-prepared civil defense system that can effectively respond to disasters, ensuring safety and continuity. Their participation in drills is often passive, but their safety is the ultimate goal.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, public_citizens, beneficiary,
    powerless, immediate, trapped, local).

% Conducts inspections and evaluates drill performance, identifying gaps and ensuring standards are met. Their expertise is critical for maintaining high adaptive capacity and recognizing novel failure signatures.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inspectors, agenda_setter,
    organized, biographical, constrained, regional).

% Provides funding for civil defense agencies, drills, and training. Bears the financial cost of maintaining preparedness. Their decisions reflect political priorities and perceived threats.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, budget_allocators, payer,
    institutional, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that diverse agencies, personnel, and infrastructure can effectively coordinate and respond to novel disaster scenarios, transmitting critical operational knowledge across generations of practitioners.
% TRANSFER_FUNCTION: Transfers operational knowledge, validated procedures, and adaptive capacity from experienced personnel to new generations, and from planning to practice, ensuring a collective capability to respond to crises.
% ABSENT_VOICES: Those who would argue for a purely theoretical or 'paper' preparedness, without the cost and disruption of live drills, are often marginalized by the demonstrated efficacy of exercised competence. Also, future generations who would suffer from decayed competence are not present to advocate for robust transmission.
% DISAPPEARANCE_RATIONALE: If the practice of live drills and inspections vanished, the system would rapidly lose its adaptive capacity. Institutional memory would decay into inert documents, and the ability to respond effectively to novel threats would collapse, leading to catastrophic failures in real disaster scenarios.
% FOUNDING_PROBLEM: The challenge of maintaining and transmitting complex, context-dependent operational knowledge for disaster response across changing personnel and evolving threats, ensuring that theoretical plans translate into effective action.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management professionals, disaster historians, and public safety advocates consistently corroborate that the problem of maintaining live competence is ongoing and critical. Post-disaster analyses frequently highlight the importance of exercised knowledge versus theoretical understanding, from outside the direct beneficiaries of the system.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.1) because the primary function is genuine coordination and knowledge transmission, with costs largely aligned with benefits. Suppression is low (0.05) as participation is driven by professional duty and the clear benefits of preparedness, rather than coercion. Theater ratio is low (0.05) because the exercises are designed for genuine learning and validation, not merely symbolic performance. Accessibility collapse is low (0.1) because alternatives (e.g., relying solely on written plans) are recognized as insufficient, and resistance is low (0.05) due to the clear value proposition of effective preparedness.
 *
 * PERSPECTIVAL GAP:
 *   In this competence reading, all stakeholders largely share a common understanding of the constraint's value and function. The primary 'gap' is between this reading and alternative readings (husk, hybrid) that posit a decay or hollowing out of competence, which would lead to different classifications for the same underlying activities.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies and inspectors are agenda-setters and beneficiaries, directly involved in maintaining and benefiting from the system's competence. First responders are primary beneficiaries, gaining essential skills. Public citizens are ultimate beneficiaries of a safe, prepared society. Budget allocators are payers, bearing the financial costs, but also benefiting from public trust and safety. All seats are net beneficiaries in this reading, as the system genuinely delivers its promised coordination function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validation_of_competence,
    'To what extent do post-disaster analyses consistently validate the high adaptive capacity and effective improvisation claimed by this reading, across diverse and novel scenarios?',
    'Systematic, independent meta-analysis of disaster response outcomes, comparing performance against pre-event drill metrics and identifying instances of genuine improvisation versus rote execution.',
    'If empirical validation is weak or inconsistent, it would suggest that the ''competence_reading'' overstates the actual adaptive capacity, potentially shifting the constraint towards a ''husk_reading'' (higher theater, lower genuine coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_validation_of_competence, empirical, 'Verifying the actual adaptive capacity and improvisation in real-world events.').

omega_variable(
    distinguishing_competence_from_ritual,
    'Is the observed low theater ratio a true reflection of functional activity, or does it mask a subtle shift towards ritualistic performance that maintains appearances without transmitting deep competence?',
    'Qualitative ethnographic studies of drill participants and inspectors, focusing on their subjective experience of learning, problem-solving, and adaptation versus compliance and performance for external observers.',
    'If a significant ritualistic component is uncovered, the theater ratio would need to be adjusted upward, pushing the constraint towards a ''piton'' or ''husk_reading'' classification, indicating a decay of its primary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinguishing_competence_from_ritual, conceptual, 'Differentiating genuine competence transmission from performative ritual.').

omega_variable(
    competence_reading_vs_husk_reading,
    'Is this constraint a genuine mechanism for transmitting live competence, or has it largely hollowed out into a memorial ritual, as suggested by the ''husk_reading''?',
    'Longitudinal studies tracking the actual operational effectiveness of civil defense systems over multiple generations of personnel, specifically looking for evidence of knowledge decay or loss of adaptive capacity in novel situations.',
    'If the ''husk_reading'' is found to be more accurate, the extractiveness and theater_ratio would be significantly higher, and the claimed_type would shift towards ''piton'' or ''snare'', as the coordination function would be largely performative cover for institutional inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_reading_vs_husk_reading, empirical, 'Distinguishing between genuine competence and ritualistic performance.').

omega_variable(
    competence_reading_vs_hybrid_reading,
    'Is competence uniformly high across all domains (e.g., physical infrastructure, civilian coordination), or is it stratified, as suggested by the ''hybrid_reading''?',
    'Comparative analysis of drill outcomes and inspection reports across different sub-domains of disaster response (e.g., engineering vs. public communication), looking for significant disparities in adaptive capacity and knowledge transmission.',
    'If the ''hybrid_reading'' is more accurate, this single ''competence_reading'' constraint would need to be decomposed into multiple, domain-specific constraints, each with its own metrics and classification, reflecting the stratified nature of preparedness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_reading_vs_hybrid_reading, empirical, 'Assessing the uniformity of competence across different domains of preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__competence_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__competence_reading, theater_ratio, 30, 0.04).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__competence_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(prep_tr_t50, preparedness_transmission__competence_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__competence_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__competence_reading, base_extractiveness, 30, 0.09).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__competence_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(prep_be_t50, preparedness_transmission__competence_reading, base_extractiveness, 50, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__competence_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__competence_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__competence_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__competence_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(prep_su_t50, preparedness_transmission__competence_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_transmission' kernel. The 'competence_reading' emphasizes live, exercised knowledge and high adaptive capacity, contrasting with the 'husk_reading' (ritualistic performance) and 'hybrid_reading' (stratified competence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
