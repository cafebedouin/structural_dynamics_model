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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Temple Sacrifice Law: Performance Only Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Temple sacrifice
 *   commitment within Halakhic tradition: that sacrifice law fundamentally
 *   requires material instantiation. From this perspective, the current
 *   absence of the Temple means the commitment is a 'dormant husk.' Study of
 *   these laws is considered archival preservation of a defunct practice, not
 *   an active occupation or performance of the commitment itself. This
 *   reading asserts a fixed, unchangeable truth about the nature of the law,
 *   rather than a currently active or extractive practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Law: Performance Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '9cf79730-d51d-478e-bdd6-aca9613778af').
narrative_ontology:cs_kernel_codification('9cf79730-d51d-478e-bdd6-aca9613778af', fixed_text).
narrative_ontology:cs_authority_grounding('9cf79730-d51d-478e-bdd6-aca9613778af', lineage).
narrative_ontology:cs_interpretation_layer_present('9cf79730-d51d-478e-bdd6-aca9613778af').
narrative_ontology:cs_reading_relation('9cf79730-d51d-478e-bdd6-aca9613778af', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_reading_relation('9cf79730-d51d-478e-bdd6-aca9613778af', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('9cf79730-d51d-478e-bdd6-aca9613778af', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('9cf79730-d51d-478e-bdd6-aca9613778af', foundational, material_instantiation_is_essential).
narrative_ontology:cs_axiom_status(material_instantiation_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('9cf79730-d51d-478e-bdd6-aca9613778af', material_instantiation_is_essential, deontological).
narrative_ontology:cs_axiom('9cf79730-d51d-478e-bdd6-aca9613778af', foundational, study_is_archival_not_performance).
narrative_ontology:cs_axiom_status(study_is_archival_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('9cf79730-d51d-478e-bdd6-aca9613778af', study_is_archival_not_performance, conventional).
narrative_ontology:cs_reference_frame('9cf79730-d51d-478e-bdd6-aca9613778af', material_performance_standard).
narrative_ontology:cs_drift_state('9cf79730-d51d-478e-bdd6-aca9613778af', contemporary_diaspora_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9cf79730-d51d-478e-bdd6-aca9613778af', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the fundamental, unchangeable nature of the divine command regarding Temple sacrifice, providing a clear (if currently inactive) standard for what constitutes a valid sacrifice.
% TRANSFER_FUNCTION: None currently, as the commitment is dormant and material sacrifices are not being performed.
% ABSENT_VOICES: Those who believe that study of sacrifice law is itself a form of divine worship or performance, or that the commitment has undergone a symbolic transformation, would object to this reading's assertion of dormancy and archival status.
% DISAPPEARANCE_RATIONALE: If this specific interpretation of sacrifice law vanished overnight, the world would not immediately rearrange itself, as the practice of material sacrifice is already suspended. The broader religious tradition would continue, but this particular understanding of the law's inherent requirements would be lost.
% FOUNDING_PROBLEM: To establish the proper and divinely ordained form of worship, atonement, and covenantal relationship through material sacrifice in the ancient Israelite Temple.
% FOUNDING_PROBLEM_CORROBORATION: Historical and archaeological records attest to the ancient practice of Temple sacrifice. Rabbinic tradition universally acknowledges the cessation of material sacrifice due to the destruction of the Temple and the absence of the necessary conditions for performance. This corroborates that the original problem (how to perform sacrifice) is currently unaddressable.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The constraint is classified as a Mountain because it describes an unchangeable, irreducible truth about the nature of sacrifice law from this reading's perspective. Its extractiveness, suppression, and theater ratio are all very low (0.05) because the constraint is currently dormant; it is not actively demanding performance, enforcing rules, or engaging in theatrical maintenance. Accessibility collapse is very high (0.95) because, for this reading, there are no alternatives to material instantiation for actual sacrifice. Resistance is very low (0.05) as the dormant nature of the law means it is not actively resisted. The measurement series reflect this stable, dormant state over time.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of this kernel (e.g., 'study_as_exercise' or 'symbolic_transformation') would experience the commitment very differently, seeing it as actively engaged through non-material means. This reading, however, maintains that the fundamental requirement for material performance remains, rendering the commitment inactive in its absence.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain with no active beneficiaries or victims, there are no parties from whom extraction is currently occurring or to whom benefits are flowing. The constraint simply describes a fundamental truth about the law's requirements, which is not currently being met.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_performance_ambiguity,
    'Is the study of sacrifice law a form of occupying the commitment, or merely archival preservation?',
    'Theological consensus shift or a new authoritative interpretive ruling that redefines ''performance'' in a non-material sense.',
    'If study is reclassified as performance, the constraint''s current dormant status would shift, potentially altering its classification from Mountain to a more active type (e.g., Rope for coordination of study).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_performance_ambiguity, conceptual, 'Ambiguity regarding the nature of engagement with dormant religious law.').

omega_variable(
    dormant_husk_implications,
    'Does the ''dormant husk'' status of the commitment imply a future obligation for material restoration, or is it truly defunct until external conditions change?',
    'Messianic era or a new Temple construction, which would force a re-evaluation of the commitment''s active status and ethical implications.',
    'If it implies a future obligation, the constraint carries latent extractiveness and potential victim sets that are currently unmanifested. If truly defunct, its Mountain status is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormant_husk_implications, empirical, 'The nature of a suspended religious obligation.').

omega_variable(
    ethical_evolution_of_sacrifice,
    'If material sacrifice were to be restored, would the ethical framework surrounding it need to evolve to prevent the emergence of new victim sets?',
    'Hypothetical future scenario analysis by ethicists and theologians, or actual attempts at restoration and their societal impact.',
    'If ethical evolution is deemed necessary, the current ''dormant'' Mountain status implicitly carries a future Scaffold-like function (transitional support for ethical re-framing) rather than a pure Mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_evolution_of_sacrifice, preference, 'Ethical considerations for the potential restoration of ancient religious practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.05).
narrative_ontology:measurement(temp_tr_t10, temple_sacrifice_commitment__performance_only, theater_ratio, 10, 0.05).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__performance_only, theater_ratio, 20, 0.05).
narrative_ontology:measurement(temp_tr_t30, temple_sacrifice_commitment__performance_only, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t10, temple_sacrifice_commitment__performance_only, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__performance_only, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(temp_be_t30, temple_sacrifice_commitment__performance_only, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t10, temple_sacrifice_commitment__performance_only, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__performance_only, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(temp_su_t30, temple_sacrifice_commitment__performance_only, suppression_requirement, 30, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
