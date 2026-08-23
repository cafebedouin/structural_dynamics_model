% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Hybrid Embedded Reading of Catastrophe Memory Transmission
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes the hybrid embedded reading of catastrophe
 *   memory transmission: survival competence is encoded within symbolic form,
 *   and the two are inseparable. Ritual fidelity transmits operational
 *   capacity through non-propositional knowledge embedded in practice. The
 *   constraint functions as a rope (coordination through shared practice)
 *   with a mountain substrate (embodied knowledge as a physical/cognitive
 *   constraint). There is no clear victim unless the practice is
 *   discontinued, at which point the community becomes a victim of its own
 *   lost transmission mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Hybrid Embedded Reading of Catastrophe Memory Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'fe2bb66c-98f4-4f9d-9876-3a4b45389368').
narrative_ontology:cs_kernel_codification('fe2bb66c-98f4-4f9d-9876-3a4b45389368', distributed).
narrative_ontology:cs_authority_grounding('fe2bb66c-98f4-4f9d-9876-3a4b45389368', practice).
narrative_ontology:cs_interpretation_layer_present('fe2bb66c-98f4-4f9d-9876-3a4b45389368').
narrative_ontology:cs_reading_relation('fe2bb66c-98f4-4f9d-9876-3a4b45389368', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe2bb66c-98f4-4f9d-9876-3a4b45389368', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('fe2bb66c-98f4-4f9d-9876-3a4b45389368', foundational, ritual_form_and_function_co_constitutive).
narrative_ontology:cs_axiom_status(ritual_form_and_function_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('fe2bb66c-98f4-4f9d-9876-3a4b45389368', ritual_form_and_function_co_constitutive, conventional).
narrative_ontology:cs_axiom('fe2bb66c-98f4-4f9d-9876-3a4b45389368', foundational, embodied_knowledge_irreducible_to_propositional).
narrative_ontology:cs_axiom_status(embodied_knowledge_irreducible_to_propositional, holdable).
narrative_ontology:cs_axiom_grounding('fe2bb66c-98f4-4f9d-9876-3a4b45389368', embodied_knowledge_irreducible_to_propositional, empirically_contingent).
narrative_ontology:cs_reference_frame('fe2bb66c-98f4-4f9d-9876-3a4b45389368', co_constitutive_ritual_practice).
narrative_ontology:cs_drift_state('fe2bb66c-98f4-4f9d-9876-3a4b45389368', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe2bb66c-98f4-4f9d-9876-3a4b45389368', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practitioner_community).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_knowledge_irreducible_to_propositional).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_fidelity_as_survival_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community enacts the ritual as a living practice; their survival competence is encoded in the symbolic form and transmitted through faithful repetition. Leaving the practice means losing access to the non-propositional knowledge that sustains them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practitioner_community, beneficiary,
    organized, generational, constrained, regional).

% Elders maintain the ritual fidelity, adjudicate correct performance, and transmit the embedded operational capacity. Their authority derives from mastery of the practice, not from external mandate.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_elders, agenda_setter,
    organized, generational, constrained, local).

% Anthropologists, historians, and cognitive scientists study the ritual as a case of non-propositional knowledge transmission. They do not participate in the practice but analyze its structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, external_observers, observer,
    analytical, immediate, analytical, global).

% Individuals who would be excluded from the community's survival competence if the ritual were altered or lost. They are not currently in the conversation because the practice continues, but their future exclusion is structurally implied.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, potential_displaced_members, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, practitioner_community).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual coordinates the community's survival competence by embedding it in a shared symbolic form that can be faithfully reproduced across generations without propositional reduction.
% TRANSFER_FUNCTION: Transfers operational capacity (pattern recognition, resource coordination, threat assessment) from elders to practitioners through enacted ritual fidelity, moving non-propositional knowledge from one generation to the next.
% ABSENT_VOICES: Potential displaced members who would lose access to the embedded survival competence if the ritual form were altered or discontinued. They are not present because the practice currently persists, but their structural exclusion is latent.
% DISAPPEARANCE_RATIONALE: If the ritual and its fidelity requirement disappeared overnight, the community's survival competence — encoded in the symbolic form — would degrade or be lost, forcing a reorganization of how they transmit operational capacity. The world rearranges because the constraint is the transmission mechanism itself.
% FOUNDING_PROBLEM: How to transmit complex survival competence (pattern recognition, resource coordination, threat assessment) across generations in a pre-literate or orally dominated context without losing the non-propositional nuances that propositional instruction cannot capture.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by cognitive anthropology (e.g., work on embodied cognition and ritual transmission) and by the practitioner community's own testimony that the ritual 'works' only when performed exactly. No single benefiting party monopolizes this attestation.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because the ritual primarily coordinates and transmits; it does not extract resources from participants for an external beneficiary. Suppression is low (0.1) because participation is voluntary and the practice persists through value, not coercion. Theater ratio is low (0.1) because the ritual's performative aspect is its functional core. Accessibility collapse is high (0.8) because the non-propositional knowledge cannot be easily replaced by alternative transmission methods. Resistance is low (0.1) because the community values the practice. The claimed type is rope, reflecting the coordination function, though the mountain substrate (embodied knowledge as irreducible constraint) is noted.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat, the constraint is a life-sustaining coordination mechanism (rope). From the external observer seat, it appears as a cultural adaptation with high fidelity (mountain-like regularity). From the potential displaced member seat, it is a latent snare if the practice fails. The engine computes these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The practitioner community and ritual elders are beneficiaries (they gain survival competence). There are no current victims; potential displaced members are excluded but not yet harmed. Directionality for beneficiaries is low (d near 0.0), for excluded is high (d near 1.0) if the practice ends. The engine will compute per-seat effective extraction from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting non-propositional survival competence) remains live. The ritual has not outlived its function; it continues to solve the coordination problem. No mandatrophy is present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_inseparability,
    'Is the inseparability of ritual form and operational function a genuine structural fact (mountain substrate) or a contingent feature of this particular transmission system (rope)?',
    'Cross-cultural comparison of catastrophe memory rituals: if all such rituals show that altering form degrades function, the mountain substrate claim gains support; if some rituals allow form variation without functional loss, the rope characterization dominates.',
    'If mountain substrate, the constraint has a natural-law core that cannot be redesigned without functional loss. If rope, the coordination function could potentially be achieved by alternative symbolic forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_inseparability, empirical, 'Whether the form-function coupling is universal or contingent.').

omega_variable(
    suppression_mechanism_if_discontinued,
    'If the ritual practice is discontinued, is the resulting loss of survival competence a structural suppression (loss of transmission mechanism) or an internalized suppression (community believes it cannot survive without the ritual)?',
    'Longitudinal study of communities that have lost or transformed their catastrophe rituals: measure actual survival outcomes vs. perceived capability.',
    'If structural, the constraint''s effective suppression is higher than measured — the community genuinely loses capacity. If internalized, the suppression is partially self-imposed and potentially reversible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_if_discontinued, conceptual, 'Structural vs. internalized suppression in the event of practice discontinuation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 80, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__hybrid_embedded_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_memory_transmission kernel. The hybrid_embedded_reading posits co-constitution of form and function; the operational_competence_reading reduces to functional components; the symbol_continuity_reading elevates symbolic form as the survival mechanism. All three share the kernel but differ in ε and beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
