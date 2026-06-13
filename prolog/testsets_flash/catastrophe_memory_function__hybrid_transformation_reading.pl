% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function: Hybrid Transformation Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes a ritual's function in encoding both the memory
 *   of catastrophe (mourning practice) and the transmission of survival
 *   competence (adaptive mechanisms). It is a 'hybrid transformation'
 *   reading, exemplified by rituals like Passover, which combine elements of
 *   grief (bitter herbs) with a rehearsal of resilience and continuity (Seder
 *   performance). The constraint ensures that the community processes loss
 *   while simultaneously building capacity for future challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.2).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '9f0347b4-198a-4e8d-b0f6-67e232650c69').
narrative_ontology:cs_kernel_codification('9f0347b4-198a-4e8d-b0f6-67e232650c69', formalized).
narrative_ontology:cs_authority_grounding('9f0347b4-198a-4e8d-b0f6-67e232650c69', lineage).
narrative_ontology:cs_interpretation_layer_present('9f0347b4-198a-4e8d-b0f6-67e232650c69').
narrative_ontology:cs_reading_relation('9f0347b4-198a-4e8d-b0f6-67e232650c69', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f0347b4-198a-4e8d-b0f6-67e232650c69', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('9f0347b4-198a-4e8d-b0f6-67e232650c69', foundational, memory_requires_dual_processing).
narrative_ontology:cs_axiom_status(memory_requires_dual_processing, holdable).
narrative_ontology:cs_axiom_grounding('9f0347b4-198a-4e8d-b0f6-67e232650c69', memory_requires_dual_processing, empirically_contingent).
narrative_ontology:cs_axiom('9f0347b4-198a-4e8d-b0f6-67e232650c69', foundational, ritual_as_transformative_mechanism).
narrative_ontology:cs_axiom_status(ritual_as_transformative_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('9f0347b4-198a-4e8d-b0f6-67e232650c69', ritual_as_transformative_mechanism, conventional).
narrative_ontology:cs_reference_frame('9f0347b4-198a-4e8d-b0f6-67e232650c69', integrated_commemorative_practice).
narrative_ontology:cs_drift_state('9f0347b4-198a-4e8d-b0f6-67e232650c69', contemporary_secularization_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('9f0347b4-198a-4e8d-b0f6-67e232650c69', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that provide both a framework for processing collective trauma and a means of transmitting adaptive strategies. The ritual reinforces group identity and resilience, but requires active participation and emotional engagement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, community_members, beneficiary,
    organized, biographical, identity_locked, local).

% Administer and interpret the ritual, ensuring its continuity and fidelity to tradition. They bear the responsibility of maintaining the balance between commemorating loss and emphasizing adaptive lessons, guiding the community through its dual function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Inherit the collective memory of catastrophe and the adaptive mechanisms encoded in the ritual. They benefit from the resilience and identity fostered by the practice, without having directly experienced the founding trauma or chosen the ritual's form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Analyze the evolution and function of such rituals, documenting how they serve both commemorative and adaptive roles. Their work provides external corroboration and critical perspective on the ritual's efficacy and meaning.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, historical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory of a catastrophic event, ensuring that both the trauma of loss and the lessons of survival are transmitted across generations, fostering group cohesion and resilience.
% TRANSFER_FUNCTION: Transfers collective memory, emotional processing frameworks, and adaptive cultural mechanisms from past generations to present and future community members, reinforcing group identity and continuity.
% ABSENT_VOICES: Those who reject the dual function, either emphasizing pure mourning without adaptive lessons, or focusing solely on survival without acknowledging the depth of loss. Their voices are often marginalized in communities that prioritize a balanced approach to memory.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a vital mechanism for collective memory, trauma processing, and cultural transmission. This would likely lead to fragmentation, a diminished sense of shared identity, and a reduced capacity for adaptive response to future challenges.
% FOUNDING_PROBLEM: How to collectively remember a catastrophic event in a way that honors the loss while simultaneously transmitting the knowledge and resilience necessary for the community's continued survival and flourishing.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of ritual, sociological analyses of collective memory, and historical accounts of communities enduring and recovering from catastrophe all corroborate the ongoing need for such mechanisms. Ritual leaders and community elders also attest to its live status, citing its continued relevance in maintaining group identity and adaptive capacity.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) as the ritual primarily serves a coordination function for collective memory and resilience, with minimal direct costs beyond participation. Suppression is also low (0.15) as adherence is largely voluntary and identity-driven, rather than coercively enforced. The theater ratio is low (0.1) because the ritual's functions are genuinely active and meaningful to participants, with little performative maintenance for its own sake. The cyclical nature of the measurements reflects the ebb and flow of communal engagement and the periodic re-emphasis of different aspects of the ritual over long historical periods.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a beneficial practice that provides meaning and continuity. From an analytical observer's perspective, it is a complex social technology that efficiently solves a dual problem of collective memory and adaptive transmission. There is little divergence in perceived type, as the benefits are widely distributed and the costs are low.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are clear beneficiaries, receiving the psychological and social benefits of collective memory and resilience. Ritual leaders act as agenda-setters, guiding the practice and ensuring its fidelity, but do not extract disproportionate gains. There are no identifiable victims, as the constraint's primary function is to serve the collective good.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to preserve memory and transmit competence) remains live. The low extractiveness and high accessibility collapse suggest it is a genuine Rope, effectively coordinating a complex social function without significant rent-seeking or coercive overhead. The hybrid nature prevents mislabeling it as purely extractive (if only the 'cost' of mourning were seen) or purely benign (if only the 'benefit' of survival were seen), by acknowledging both essential components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mourning_vs_survival_emphasis,
    'Does the ritual''s emphasis shift over time between mourning practice and survival competence, and if so, what drives these shifts?',
    'Longitudinal ethnographic studies and historical analysis of ritual performance and interpretation across different eras and contexts.',
    'If the emphasis shifts significantly, the constraint''s effective extractiveness or suppression might fluctuate, as one aspect (e.g., rigid mourning) could become more burdensome than the other (e.g., flexible adaptation). This could lead to temporary reclassification towards a Tangled Rope if the burden becomes asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_vs_survival_emphasis, empirical, 'Ambiguity in the dynamic balance between the two core functions of the ritual.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''hybrid transformation'' reading truly distinct from a simple combination of the ''mourning practice'' and ''survival competence'' readings, or is it merely their sum?',
    'Conceptual analysis demonstrating emergent properties or synergistic effects in the hybrid reading that are not present in either sibling reading alone, or in their simple aggregation.',
    'If it''s merely a sum, the hybrid reading might be redundant, and the kernel should be decomposed into two separate, linked constraints. If it''s truly distinct, it validates the hybrid as a unique and irreducible constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether the hybrid reading represents an emergent structural function or a mere aggregation of sibling functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 75, 0.07).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 75, 0.17).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 100, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 75, 0.12).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_function' kernel, which also includes 'mourning_practice_reading' and 'survival_competence_reading'. This hybrid reading integrates both aspects into a single, transformative ritual structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
