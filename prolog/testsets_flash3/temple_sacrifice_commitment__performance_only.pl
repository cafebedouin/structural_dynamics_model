% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment: Performance Only Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'performance only' reading of temple
 *   sacrifice law, which asserts that the commitment to sacrifice is dormant
 *   and non-binding in the absence of the physical Temple and its material
 *   conditions for performance. Study of these laws is considered archival
 *   preservation of a defunct practice, not an active occupation of the
 *   commitment. This reading provides clarity and removes the burden of
 *   ritual obligation, but it also means the commitment is not actively
 *   'lived' in the present. The constraint is claimed as a Mountain due to
 *   its perceived natural emergence from the material conditions of the law,
 *   but with beneficiaries who gain from this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.02).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment: Performance Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '8b6a209c-201c-4045-b89c-a3f9657e8e74').
narrative_ontology:cs_kernel_codification('8b6a209c-201c-4045-b89c-a3f9657e8e74', fixed_text).
narrative_ontology:cs_authority_grounding('8b6a209c-201c-4045-b89c-a3f9657e8e74', lineage).
narrative_ontology:cs_interpretation_layer_present('8b6a209c-201c-4045-b89c-a3f9657e8e74').
narrative_ontology:cs_reading_relation('8b6a209c-201c-4045-b89c-a3f9657e8e74', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('8b6a209c-201c-4045-b89c-a3f9657e8e74', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('8b6a209c-201c-4045-b89c-a3f9657e8e74', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('8b6a209c-201c-4045-b89c-a3f9657e8e74', foundational, material_performance_is_sine_qua_non).
narrative_ontology:cs_axiom_status(material_performance_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('8b6a209c-201c-4045-b89c-a3f9657e8e74', material_performance_is_sine_qua_non, deontological).
narrative_ontology:cs_reference_frame('8b6a209c-201c-4045-b89c-a3f9657e8e74', pre_destruction_material_performance).
narrative_ontology:cs_drift_state('8b6a209c-201c-4045-b89c-a3f9657e8e74', post_temple_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8b6a209c-201c-4045-b89c-a3f9657e8e74', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, historical_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, archival_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the clarity that the commitment is not currently active, allowing for objective historical and textual study without the burden of present-day ritual obligation. Their work is seen as preservation, not performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, historical_scholars, beneficiary,
    moderate, generational, mobile, global).

% Benefit from the classification of sacrifice law as a defunct practice, justifying its preservation in archives and academic curricula as historical artifacts rather than living religious obligations. They manage the textual and material remnants.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, archival_institutions, beneficiary,
    organized, civilizational, mobile, global).

% Hold the belief that the commitment is dormant without material performance. They are not victims, as they are not compelled to perform, nor are they beneficiaries in an extractive sense. Their identity is tied to this specific interpretation of religious law.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, adherents_of_performance_only_reading, observer,
    powerless, biographical, identity_locked, local).

% Would advocate for the physical restoration of sacrifice, but this reading considers their efforts premature or misdirected without the necessary material and political conditions. They are excluded from the current interpretive consensus of this reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, future_restoration_advocates, excluded,
    powerless, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous framework for understanding the current status of temple sacrifice law: it is not currently performable, and therefore not an active religious obligation. This coordinates scholarly and communal understanding.
% TRANSFER_FUNCTION: No active transfer of resources or obligations occurs under this reading, as the practice is considered defunct. It transfers the burden of active ritual performance from adherents to a future, hypothetical state.
% ABSENT_VOICES: Advocates for immediate or symbolic restoration of sacrifice are absent from the interpretive consensus of this reading, as their views are considered inconsistent with the material requirements of the law.
% DISAPPEARANCE_RATIONALE: If this constraint (the 'performance only' reading) vanished, the physical world and the historical texts would remain unchanged. The only change would be in the interpretive framework of some adherents, potentially leading to increased internal debate or shifts towards other readings.
% FOUNDING_PROBLEM: The historical destruction of the Temple and the cessation of material sacrifice created a dilemma regarding the ongoing status and obligation of sacrifice law.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and theological debates across centuries corroborate the enduring problem of how to relate to sacrifice law in the absence of the Temple. This is attested by a wide range of religious scholars and historians, not just adherents of this specific reading.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because this reading imposes no active obligations or costs; rather, it removes them. Suppression is negligible (0.02) as there's no active enforcement against alternative interpretations, only a structural assertion of material necessity. Theater ratio is minimal (0.01) as the reading is straightforward and not performative. Accessibility collapse is high (0.95) because, from this perspective, there are no viable alternatives to material performance for occupying the commitment. Resistance is low (0.01) because this reading is largely accepted by those who hold it, and those who resist it are simply holding different readings of the same kernel.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between this reading and those that assert an active, albeit non-material, occupation of the sacrifice commitment (e.g., 'study as exercise' or 'symbolic transformation'). This reading sees the others as misinterpreting the fundamental material requirements of the law, while the other readings see this one as overly literal or failing to adapt to changed circumstances.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are primarily those who gain clarity and freedom from ritual obligation, such as historical scholars and archival institutions, who can study the laws without the burden of present-day performance. There are no direct victims, as the constraint's primary effect is to declare a practice non-binding. Adherents of this reading are identity-locked into this interpretation, but not in an extractive sense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    materiality_of_commitment,
    'Is the ''material instantiation'' requirement for sacrifice law an irreducible physical/theological limit, or a historically contingent interpretation?',
    'Theological consensus shift or new archaeological/textual discoveries that reframe the historical understanding of sacrifice requirements.',
    'If historically contingent, the ''performance only'' reading''s claim to be a Mountain would weaken, potentially reclassifying it as a Snare (if it benefits those who wish to avoid ritual obligation) or a Rope (if it coordinates a new, non-material practice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(materiality_of_commitment, conceptual, 'Ambiguity regarding the fundamental nature of the ''materiality'' requirement for sacrifice.').

omega_variable(
    future_victim_potential,
    'If a restoration of material sacrifice were attempted under this reading, would it create a new class of victims due to ethical evolution or changed social norms?',
    'Hypothetical scenario analysis and ethical deliberation on the implications of restoring ancient practices in a modern context.',
    'If restoration would create victims, the ''performance only'' reading, by preserving the dormant commitment, implicitly carries a future Snare potential, even if it is not extractive now.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_victim_potential, preference, 'Potential for future victimhood if the dormant commitment were to be reactivated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.01).
narrative_ontology:measurement(temp_tr_t25, temple_sacrifice_commitment__performance_only, theater_ratio, 25, 0.01).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_commitment__performance_only, theater_ratio, 50, 0.01).
narrative_ontology:measurement(temp_tr_t75, temple_sacrifice_commitment__performance_only, theater_ratio, 75, 0.01).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__performance_only, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t25, temple_sacrifice_commitment__performance_only, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_commitment__performance_only, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(temp_be_t75, temple_sacrifice_commitment__performance_only, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__performance_only, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(temp_su_t25, temple_sacrifice_commitment__performance_only, suppression_requirement, 25, 0.02).
narrative_ontology:measurement(temp_su_t50, temple_sacrifice_commitment__performance_only, suppression_requirement, 50, 0.02).
narrative_ontology:measurement(temp_su_t75, temple_sacrifice_commitment__performance_only, suppression_requirement, 75, 0.02).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__performance_only, suppression_requirement, 100, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, information_standard).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'temple_sacrifice_commitment' kernel. Its 'performance only' interpretation stands in contrast to other readings that assert active or symbolic occupation of the commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
