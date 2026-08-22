% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual as Intergenerational Trauma Encoding for Threat Vigilance
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a reading of a collective memory kernel where
 *   ritual practice functions as a mechanism for encoding and transmitting
 *   intergenerational trauma. The purpose is to create a perpetual warning
 *   system, ensuring future generations maintain vigilance against past
 *   catastrophes. However, this comes at the cost of imposing a significant
 *   psychological burden on descendants. The constraint is claimed as a
 *   Tangled Rope because it genuinely coordinates collective threat-detection
 *   but does so through an asymmetric extraction of psychological well-being
 *   from later generations, requiring active enforcement of ritual adherence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual as Intergenerational Trauma Encoding for Threat Vigilance").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '952059f0-a428-439b-8ffb-23354bc66e0b').
narrative_ontology:cs_kernel_codification('952059f0-a428-439b-8ffb-23354bc66e0b', implicit).
narrative_ontology:cs_authority_grounding('952059f0-a428-439b-8ffb-23354bc66e0b', practice).
narrative_ontology:cs_interpretation_layer_present('952059f0-a428-439b-8ffb-23354bc66e0b').
narrative_ontology:cs_reading_relation('952059f0-a428-439b-8ffb-23354bc66e0b', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('952059f0-a428-439b-8ffb-23354bc66e0b', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('952059f0-a428-439b-8ffb-23354bc66e0b', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('952059f0-a428-439b-8ffb-23354bc66e0b', foundational, trauma_as_essential_warning).
narrative_ontology:cs_axiom_status(trauma_as_essential_warning, holdable).
narrative_ontology:cs_axiom_grounding('952059f0-a428-439b-8ffb-23354bc66e0b', trauma_as_essential_warning, instrumental).
narrative_ontology:cs_axiom('952059f0-a428-439b-8ffb-23354bc66e0b', secondary, intergenerational_transmission_of_affect).
narrative_ontology:cs_axiom_status(intergenerational_transmission_of_affect, holdable).
narrative_ontology:cs_axiom_grounding('952059f0-a428-439b-8ffb-23354bc66e0b', intergenerational_transmission_of_affect, empirically_contingent).
narrative_ontology:cs_reference_frame('952059f0-a428-439b-8ffb-23354bc66e0b', perpetual_vigilance_through_suffering).
narrative_ontology:cs_drift_state('952059f0-a428-439b-8ffb-23354bc66e0b', contemporary_psychological_awareness, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('952059f0-a428-439b-8ffb-23354bc66e0b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, future_generations_collective_vigilance).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendants_psychological_burden).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original group that experienced the catastrophe and instituted the ritual. They encode their trauma into the practice, intending to transmit a warning system. Their identity is fused with the survival narrative.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, founding_generation_survivors, agenda_setter,
    institutional, generational, identity_locked, local).

% Later generations who inherit the ritual and, through it, the psychological burden of the original trauma. They experience anxiety, hyper-vigilance, and a sense of impending doom, even without direct experience of the catastrophe. Their identity is often tied to the group's history of suffering.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendants_psychological_burden, payer,
    powerless, biographical, identity_locked, local).

% The collective capacity of the group to detect and respond to future threats, derived from the trauma-encoded warning system. This vigilance is a diffuse benefit, but it is directly linked to the psychological costs borne by individuals.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, future_generations_collective_vigilance, beneficiary,
    organized, generational, constrained, local).

% Those who maintain and interpret the ritual across generations. They enforce adherence to the practices, believing in their vital role for group survival. They benefit from the authority derived from their role as custodians of collective memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_leaders_interpreters, agenda_setter,
    powerful, biographical, constrained, local).

% Researchers and scholars who study the ritual's effects on collective memory and psychological well-being. They analyze the mechanisms of trauma transmission and the efficacy of the warning system.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, external_observers_academics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and threat-detection by transmitting the emotional and psychological imprint of a past catastrophe, ensuring future generations remain vigilant against similar threats.
% TRANSFER_FUNCTION: Transfers a psychological burden (anxiety, hyper-vigilance) from the founding generation to descendants, in exchange for a diffuse benefit of collective threat-vigilance and perceived survival capacity.
% ABSENT_VOICES: Descendants who might wish to process and release the trauma rather than perpetually re-enact it are often silenced by the narrative of collective survival and loyalty to ancestral suffering. Their voices are excluded by the very identity-fusing mechanisms of the ritual.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the group's collective memory of the catastrophe would fade, and with it, the specific, trauma-encoded warning system. While other forms of memory might persist, the direct, visceral transmission of threat vigilance would cease, potentially altering the group's adaptive strategies and psychological landscape.
% FOUNDING_PROBLEM: The problem of ensuring the group's survival after a catastrophic event by preventing future generations from forgetting the danger and becoming complacent.
% FOUNDING_PROBLEM_CORROBORATION: The founding generation's accounts and the ongoing vigilance of the community attest to the problem's initial and continued salience. External anthropological and psychological studies corroborate the persistence of both the trauma and the vigilance, even if they question the efficacy or cost-benefit of the encoding mechanism.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.65) because the psychological cost to descendants is substantial and ongoing, often manifesting as anxiety or hyper-vigilance. Suppression is high (0.70) due to the identity-locked nature of participation; challenging the ritual is often seen as a betrayal of the group's history and survival. The theater ratio is low (0.10) as the ritual is genuinely functional in its intent to transmit a warning, even if the cost-benefit is contested. The measurements show a relatively stable, high level of extractiveness and suppression over time, reflecting the enduring nature of the trauma transmission.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the founding generation and ritual leaders, the constraint is a necessary, albeit difficult, coordination mechanism for survival. From the perspective of individual descendants, it can feel like an inescapable burden, a form of extraction that perpetuates suffering. The engine's per-seat classification should reflect this divergence, with agenda-setters experiencing it closer to a Rope and payers closer to a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The founding generation and ritual leaders act as agenda-setters, benefiting from the perceived security and authority derived from maintaining the warning system. Future generations, particularly individual descendants, are the primary payers, bearing the psychological costs. The collective vigilance of future generations is a diffuse beneficiary, but this benefit is directly tied to the individual psychological burden. The identity-locked exit option for descendants amplifies their directionality towards being targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring survival through vigilance) is still considered 'live' by the community, preventing a clear mandatrophy resolution. However, the high extractiveness and suppression suggest that the mechanism for fulfilling this mandate has become overly burdensome, potentially indicating a 'zombie' function where the cost outweighs the adaptive benefit. The classification as Tangled Rope captures this hybrid nature, preventing it from being mislabeled as pure coordination or pure extraction without acknowledging its dual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_vs_cost_of_vigilance,
    'Is the level of psychological burden imposed on descendants proportional to the actual threat-detection efficacy provided by the ritual?',
    'Longitudinal psychological studies comparing groups with similar historical trauma but different ritual practices, or empirical analysis of actual threat-response outcomes versus psychological distress levels.',
    'If the burden significantly outweighs efficacy, the constraint''s extractiveness is higher than justified by its coordination function, pushing it closer to a Snare. If efficacy is high, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_vs_cost_of_vigilance, empirical, 'Assesses the cost-benefit ratio of trauma-encoded vigilance.').

omega_variable(
    trauma_processing_alternatives,
    'Are there alternative, less psychologically burdensome methods for transmitting threat vigilance and collective memory that would achieve similar or better outcomes?',
    'Comparative studies of communities that have adopted therapeutic or educational approaches to trauma memory versus those relying solely on ritual transmission.',
    'If effective alternatives exist, the suppression of those alternatives by the ritual''s enforcement mechanism would increase, pushing the constraint towards a Snare by highlighting the unnecessary nature of the current extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trauma_processing_alternatives, preference, 'Examines the availability and impact of less extractive memory transmission methods.').

omega_variable(
    structural_vs_internalized_suppression,
    'What proportion of the measured suppression is structural (e.g., social ostracism for non-adherence) versus internalized (e.g., guilt or identity fusion making exit unthinkable)?',
    'Post-exit suppression trajectory: if psychological distress and identity-based adherence persist after structural enforcement is removed, reclassify as partially internalized.',
    'If internalized suppression is high, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit, making the constraint more resilient and harder to dismantle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Structural vs. internalized suppression mechanism in ritual adherence.').

omega_variable(
    kernel_reading_divergence,
    'Is this constraint a genuine mechanism for trauma encoding and vigilance, or is it primarily a mechanism for symbolic continuity, survival competence, or boundary maintenance, with trauma encoding as a secondary effect?',
    'Detailed ethnographic and psychological studies focusing on the primary intent and observed effects of the ritual, distinguishing between explicit and latent functions.',
    'If the primary function is found to be one of the sibling readings, the classification of this specific ''trauma_encoding_reading'' constraint would shift, potentially altering its extractiveness and suppression metrics based on the new primary function''s characteristics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Distinguishes the primary function of the ritual among competing kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'catastrophe_memory_kernel', focusing on trauma encoding. It influences and is influenced by other readings that emphasize symbolic continuity, survival competence, and boundary maintenance, as these functions are often intertwined in ritual practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
