% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Memory Preservation: Mourning Practice Reading
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in preserving the memory
 *   of past catastrophes, specifically focusing on its role in maintaining
 *   symbolic continuity and collective identity through mourning practices.
 *   It is a reading of the broader 'catastrophe_memory_preservation' kernel,
 *   which also includes readings focused on survival competence or atrophy.
 *   This reading emphasizes the non-operational, identity-forming aspect of
 *   ritual, with low extractiveness and suppression due to its voluntary
 *   nature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.05).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Memory Preservation: Mourning Practice Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'b1b18d4c-7b69-401e-9ee4-a200d7d1539d').
narrative_ontology:cs_kernel_codification('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', implicit).
narrative_ontology:cs_authority_grounding('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', practice).
narrative_ontology:cs_interpretation_layer_present('b1b18d4c-7b69-401e-9ee4-a200d7d1539d').
narrative_ontology:cs_reading_relation('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', foundational, symbolic_continuity_is_identity).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', symbolic_continuity_is_identity, conventional).
narrative_ontology:cs_axiom('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', secondary, ritual_as_emotional_processing).
narrative_ontology:cs_axiom_status(ritual_as_emotional_processing, holdable).
narrative_ontology:cs_axiom_grounding('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', ritual_as_emotional_processing, empirically_contingent).
narrative_ontology:cs_reference_frame('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', ritual_as_identity_marker).
narrative_ontology:cs_drift_state('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b1b18d4c-7b69-401e-9ee4-a200d7d1539d', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate voluntarily in rituals that commemorate past catastrophes, reinforcing shared identity and emotional bonds. They benefit from a sense of belonging and continuity, with low cost of participation and easy exit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, community_members, beneficiary,
    organized, generational, mobile, local).

% Organize and guide the commemorative rituals, ensuring their proper execution and transmission across generations. They derive status and purpose from this role, but are constrained by the community's expectations and traditions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_leaders, agenda_setter,
    moderate, biographical, constrained, local).

% Study the ritual practices for their sociological, anthropological, or historical significance. They analyze the constraint's function in collective memory and identity formation without direct participation or benefit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, external_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective mourning and remembrance, providing a shared framework for processing historical trauma and reinforcing group identity through symbolic action.
% TRANSFER_FUNCTION: Transfers symbolic meaning, emotional solidarity, and a sense of shared history across generations within the community. No material or operational transfer is involved.
% ABSENT_VOICES: Those who have left the community or reject the historical narrative might object to the perpetuation of certain memories or interpretations, but their voices are typically outside the ritual's immediate sphere of influence.
% DISAPPEARANCE_RATIONALE: If the ritual practices vanished, the community would lose a primary mechanism for collective memory and identity maintenance. While the historical facts would remain, their emotional and symbolic resonance, and thus the community's cohesion, would significantly diminish, leading to a gradual rearrangement of social bonds and self-understanding.
% FOUNDING_PROBLEM: The problem of preserving the memory of a catastrophic event and its impact on the community, ensuring that future generations understand their shared history and identity.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and historians attest to the ongoing need for these practices to maintain cultural continuity and prevent the erosion of collective memory, corroborated by sociological studies on the role of ritual in identity formation.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because participation is voluntary and the 'cost' is primarily time and emotional engagement, which is reciprocated by a sense of belonging. Suppression is very low (0.05) as there is no active coercion to participate; individuals can opt out without significant penalty. Theater ratio is low (0.2) because the primary function of symbolic continuity is genuinely performed, though some elements might become more performative over time as direct memory fades. The constraint is classified as a Rope because it facilitates genuine coordination (collective identity, shared memory) with minimal extraction and no active suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a beneficial practice for collective identity. From an external observer's perspective, it is a cultural mechanism for memory preservation. Both views align on the low extractiveness and voluntary nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are beneficiaries (d near 0.0) as they gain a sense of identity and belonging without significant cost. Ritual leaders are agenda-setters (d near 0.5) as they facilitate the ritual and gain status, but also bear the responsibility of its maintenance. External observers are analytical (d near 0.5) and are not directly affected by the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_symbolic_function,
    'Is the ritual purely symbolic, or does it retain any latent operational function for survival competence?',
    'Empirical study of community responses to new threats: if ritual practices correlate with effective operational responses, a latent function exists.',
    'If a latent operational function is found, the constraint''s extractiveness might be re-evaluated as a cost for a tangible benefit, potentially shifting its classification towards a Tangled Rope if participation becomes less voluntary due to perceived necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_symbolic_function, empirical, 'Distinguishing purely symbolic ritual from one with hidden operational utility.').

omega_variable(
    voluntary_vs_social_pressure,
    'To what extent is participation truly voluntary, versus influenced by subtle social pressure or identity-lock mechanisms?',
    'Sociological surveys and ethnographic studies on community members'' perceived freedom to exit or abstain from ritual participation without social cost.',
    'If significant social pressure or identity-lock is identified, the suppression metric would increase, potentially reclassifying the constraint towards a Tangled Rope or even Snare, as the ''voluntary'' aspect diminishes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_social_pressure, empirical, 'Assessing the true voluntariness of ritual participation.').

omega_variable(
    reading_framing_choice,
    'Is this ''mourning_practice_reading'' the most appropriate framing, or does the ''hybrid_atrophy_reading'' better capture the constraint''s historical evolution?',
    'Historical analysis of ritual evolution and community narratives: if evidence strongly suggests a prior operational function that has since diminished, the atrophy reading might be more accurate.',
    'Adopting the ''hybrid_atrophy_reading'' would imply a higher historical extractiveness and a shift in the constraint''s function over time, potentially reclassifying it as a Piton if the original mandate has atrophied but the practice persists by inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Ambiguity in framing the ritual''s primary function and historical trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_preservation' kernel. It focuses on the symbolic and identity-preserving function of ritual, distinct from readings emphasizing operational competence or historical atrophy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
