% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Catastrophe Memory Transmission: Hybrid Embedded Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid_embedded_reading' of catastrophe
 *   memory transmission, where survival competence is understood as
 *   intrinsically encoded within symbolic ritual form. Ritual fidelity is not
 *   merely cultural performance but the active transmission mechanism for
 *   non-propositional operational capacity. The constraint functions as a
 *   Rope, coordinating collective survival through shared, embodied practice,
 *   with a strong underlying sense of natural necessity due to the
 *   inseparability of form and function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.07).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.07).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe Memory Transmission: Hybrid Embedded Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'aa22cc5b-6afc-4373-9155-c694c8851b02').
narrative_ontology:cs_kernel_codification('aa22cc5b-6afc-4373-9155-c694c8851b02', implicit).
narrative_ontology:cs_authority_grounding('aa22cc5b-6afc-4373-9155-c694c8851b02', practice).
narrative_ontology:cs_interpretation_layer_present('aa22cc5b-6afc-4373-9155-c694c8851b02').
narrative_ontology:cs_reading_relation('aa22cc5b-6afc-4373-9155-c694c8851b02', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa22cc5b-6afc-4373-9155-c694c8851b02', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_axiom('aa22cc5b-6afc-4373-9155-c694c8851b02', foundational, form_and_function_inseparable).
narrative_ontology:cs_axiom_status(form_and_function_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('aa22cc5b-6afc-4373-9155-c694c8851b02', form_and_function_inseparable, deontological).
narrative_ontology:cs_axiom('aa22cc5b-6afc-4373-9155-c694c8851b02', secondary, non_propositional_transmission_efficacy).
narrative_ontology:cs_axiom_status(non_propositional_transmission_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('aa22cc5b-6afc-4373-9155-c694c8851b02', non_propositional_transmission_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('aa22cc5b-6afc-4373-9155-c694c8851b02', ancestral_fidelity_framework).
narrative_ontology:cs_drift_state('aa22cc5b-6afc-4373-9155-c694c8851b02', contemporary_secularization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa22cc5b-6afc-4373-9155-c694c8851b02', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in and perpetuate the ritual practices, benefiting directly from the transmitted survival competence and the cohesion it fosters. Their identity is often intertwined with the fidelity to these traditions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, community_members, agenda_setter,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, community_members, beneficiary).

% Are the ultimate beneficiaries, inheriting the non-propositional survival knowledge and operational capacity embedded in the faithfully transmitted rituals. Their well-being depends on the current generation's adherence to the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, local).

% Are responsible for maintaining the fidelity of ritual forms and ensuring the accurate transmission of embedded knowledge. They benefit from the status and purpose derived from this role, and their identity is deeply tied to the practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_specialists, beneficiary).

% Study the mechanisms of cultural transmission and collective memory, analyzing how ritual forms encode and transmit survival competence. They do not directly participate in or benefit from the constraint's operation, but seek to understand its structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, anthropological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action and knowledge transmission for survival in the face of recurring threats by embedding operational competence within symbolic ritual forms, ensuring shared understanding and response patterns.
% TRANSFER_FUNCTION: Transfers non-propositional survival knowledge, operational capacity, and collective resilience across generations through faithful ritual enactment, from past and present community members to future ones.
% ABSENT_VOICES: Those who prioritize individual autonomy over communal fidelity, or those who believe knowledge can be transmitted purely propositionally (e.g., through written manuals or scientific instruction), might object to the constraint of ritual form and its perceived 'irrationality'. They are often outside the community's epistemic framework.
% DISAPPEARANCE_RATIONALE: If the constraint of inseparable form and function vanished, the ritual practices would likely degrade, leading to a loss of embedded survival competence and increased vulnerability for the community. The collective memory of catastrophe and the learned responses would dissipate, forcing a reorganization of survival strategies.
% FOUNDING_PROBLEM: How to transmit critical survival knowledge and operational capacity across generations, especially in contexts of recurring catastrophe, when explicit propositional instruction is insufficient, unreliable, or easily lost.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic studies of communities facing recurring environmental or social threats, and historical accounts of successful long-term survival strategies, corroborate the ongoing relevance of this problem and the efficacy of ritual transmission. Independent scholars and community elders attest to the problem's persistence.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness and suppression are low because it primarily serves a coordination function for collective survival, and adherence is largely driven by internal community norms and perceived necessity rather than external coercion. The theater ratio is minimal as the rituals are understood to be functionally vital. Accessibility collapse is high because alternatives for transmitting this specific type of embedded, non-propositional knowledge are limited. Resistance is low due to the community's shared belief in the practice's efficacy.
 *
 * PERSPECTIVAL GAP:
 *   While the constraint is claimed as a Rope, the 'mountain substrate' aspect (the perceived natural inseparability of form and function) could lead an external observer to classify it as a Mountain, overlooking the active coordination and fidelity required. Conversely, an overly instrumental reading might miss the deep, non-propositional embedding of competence.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and ritual specialists are both beneficiaries and agenda-setters, actively participating in and maintaining the constraint while reaping its benefits. Future generations are pure beneficiaries, dependent on the constraint's persistence. There are no direct victims, as the constraint's primary function is collective well-being, and its costs are diffuse and accepted as necessary for survival.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_inseparability,
    'Is the inseparability of symbolic form and survival competence a genuine ''mountain substrate'' (a natural, irreducible fact of embodied cognition) or a socially constructed, albeit deeply ingrained, belief that could be re-engineered?',
    'Cross-cultural comparative studies of similar catastrophe-prone communities that have successfully adapted or re-engineered their knowledge transmission methods, or neuroscientific research into the mechanisms of non-propositional knowledge encoding.',
    'If genuinely natural, the constraint''s ''rope'' classification has a strong mountain-like foundation, making its persistence highly robust. If constructed, the constraint is more amenable to intentional modification or replacement, potentially shifting its classification towards a more flexible ''scaffold'' or even a ''snare'' if the ''inseparability'' claim is used to suppress alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_inseparability, conceptual, 'Ambiguity regarding the naturalness of the form-function inseparability.').

omega_variable(
    empirical_manifestation_of_non_propositional_knowledge,
    'How does the ''non-propositional knowledge embedded in practice'' empirically manifest, and how can its transmission efficacy be measured independently of the ritual''s symbolic continuity?',
    'Detailed ethnographic observation combined with cognitive science experiments that isolate and measure specific operational capacities transmitted through ritual, comparing outcomes in communities with varying degrees of ritual fidelity.',
    'Clear empirical evidence would strengthen the ''rope'' classification by validating its coordination function. Lack of measurable operational capacity, or evidence that efficacy is primarily symbolic/social, would shift the constraint towards a ''piton'' (if function atrophied) or ''symbol_continuity_reading'' (if the primary function is identity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_manifestation_of_non_propositional_knowledge, empirical, 'Empirical ambiguity of non-propositional knowledge and its measurement.').

omega_variable(
    reading_contest_resolution,
    'Which of the competing readings of catastrophe memory transmission (hybrid_embedded, symbol_continuity, operational_competence) best captures the structural reality of the constraint''s operation?',
    'Longitudinal studies tracking community resilience and survival outcomes under varying conditions of ritual fidelity and explicit operational training, combined with detailed analysis of community narratives and practices.',
    'Resolution would clarify the primary function and beneficiary structure, potentially leading to a reclassification of this constraint or its siblings. For example, if operational competence is found to be primary and separable, this reading''s claim of inseparability would be challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_resolution, conceptual, 'The core contest between the different readings of catastrophe memory transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 60, 0.06).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 80, 0.07).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.07).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 40, 0.11).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 80, 0.12).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
