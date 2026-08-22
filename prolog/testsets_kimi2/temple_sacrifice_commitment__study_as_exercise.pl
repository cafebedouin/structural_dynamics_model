% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Performance of Command
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   This constraint story instantiates the study_as_exercise reading of the
 *   temple_sacrifice_commitment kernel. In this reading, intellectual
 *   engagement with sacrificial law is not a placeholder or archive but the
 *   actual performance of the divine command in an era without material
 *   Temple conditions. The constraint coordinates a global community around
 *   study-as-worship, with zero extractive asymmetry: the studying community
 *   is both agent and beneficiary, maintaining covenant fidelity. The story
 *   is authored as a rope: pure coordination with negligible extraction,
 *   though the kernel contest (performance_only, hybrid, symbolic readings)
 *   introduces conceptual omegas.
 *
 * KEY AGENTS:
 *   - covenant_study_community (beneficiary/organized/identity_locked)
 *   - temple_restoration_advocates (excluded/moderate/constrained)
 *   - academic_religious_studies (observer/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.02).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.1).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study of Sacrifice Law as Performance of Command").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '7ad831d9-bd3c-44a6-b04b-524ebba22fc9').
narrative_ontology:cs_kernel_codification('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', fixed_text).
narrative_ontology:cs_authority_grounding('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', lineage).
narrative_ontology:cs_interpretation_layer_present('7ad831d9-bd3c-44a6-b04b-524ebba22fc9').
narrative_ontology:cs_reading_relation('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_reading_relation('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', foundational, study_occupies_divine_command).
narrative_ontology:cs_axiom_status(study_occupies_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', study_occupies_divine_command, theological).
narrative_ontology:cs_axiom('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', foundational, material_conditions_non_essential).
narrative_ontology:cs_axiom_status(material_conditions_non_essential, holdable).
narrative_ontology:cs_axiom_grounding('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', material_conditions_non_essential, theological).
narrative_ontology:cs_reference_frame('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', study_occupies_command_frame).
narrative_ontology:cs_drift_state('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7ad831d9-bd3c-44a6-b04b-524ebba22fc9', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, covenant_study_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in daily Talmudic study of sacrificial law as an act of divine service; maintains covenant fidelity and communal continuity in the absence of the Temple; exit would require abandoning the normative framework of covenant obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, covenant_study_community, beneficiary,
    organized, generational, identity_locked, global).

% Advocate for material sacrificial worship in a rebuilt Temple; hold that study without altar service is insufficient for covenant fulfillment; excluded from the normative consensus that validates study-as-performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, temple_restoration_advocates, excluded,
    moderate, generational, constrained, national).

% Analyzes the halakhic mechanism by which study substitutes for sacrifice; neither benefits from nor is bound by the covenantal commitment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, academic_religious_studies, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves covenant continuity and collective divine service after the loss of the Temple by coordinating the community around intellectual engagement with sacrificial law as a replacement modality.
% TRANSFER_FUNCTION: Moves individual cognitive and temporal effort into the collective maintenance of a suspended sacrificial tradition; transfers the site of divine service from priest to scholar.
% ABSENT_VOICES: Temple restoration advocates and holders of the performance_only reading are structurally absent from the normative consensus; they would assert that study is archival or suspended, not performative, but are not the dominant voices in the post-Temple interpretive tradition.
% DISAPPEARANCE_RATIONALE: The daily practice of thousands of study-community members and their theological self-understanding depend on this reading; without it, the rationale for intensive study of defunct sacrificial law collapses, and the community would need to adopt hybrid or transformed justifications.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the material and institutional conditions for biblical sacrificial worship, threatening the continuity of a central covenantal practice.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and archaeologists corroborate the Temple's destruction as a historical rupture; the performance_only reading attests the problem from outside the study-community beneficiary group, though it rejects the study-as-exercise solution.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.02 â functionally zero, consistent with the reading's claim that study is intrinsically valuable and non-extractive. Suppression is low (0.10) because there is no coercive enforcement; adherence is identity-driven. Theater ratio is low-moderate (0.15) because the study practice is functionally central to the community, though some performative display of piety may occur. Accessibility_collapse is low (0.20) because alternatives (hybrid, symbolic, performance) remain intellectually available, though historically marginalized. Resistance is negligible (0.05) because the constraint does not extract from any party; resistance comes from outside the beneficiary community (performance_only holders), not from victims within.
 *
 * PERSPECTIVAL GAP:
 *   From the covenant_study_community seat, the constraint is experienced as liberating coordination â a way to continue divine service. From the excluded performance_only seat, the same structure is a failure to perform the command, a suspended or collapsed practice. The analytical seat sees a commitment system maintaining continuity through interpretive innovation. The engine will compute near-zero directionality for the beneficiary community and higher directionality for excluded advocates if modeled, but the structural asymmetry is minimal because no party is trapped in a payer role.
 *
 * DIRECTIONALITY LOGIC:
 *   The covenant_study_community is the sole beneficiary (d near 0.0). No victim group is declared. Temple_restoration_advocates are excluded, not payers; their cost is opportunity cost of a non-realized practice, not extraction by this constraint. Directionality is therefore uniformly low within the constraint's operating community.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not triggered: the founding problem (Temple destruction) remains live, and the coordination function (study maintaining covenant) is still actively performed. There is no decay into theatrical maintenance. The reading's own theology prevents piton classification because the practice is claimed as fully functional divine service, not inertial residue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_continuity_or_transformation,
    'Is study-as-exercise a genuine continuity of the original sacrificial command, or an authorized transformation masked as continuity?',
    'Source-critical and redaction analysis of Talmudic passages framing study as sacrifice.',
    'If transformation, the constraint may be a symbolic_transformation reading misclassified as continuity, raising extractiveness if the continuity claim suppresses alternative legitimacies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_continuity_or_transformation, conceptual, 'Whether study substitutes for sacrifice or transforms the command.').

omega_variable(
    suppression_of_alternative_readings,
    'Does the dominance of the study_as_exercise reading within rabbinic tradition suppress the performance_only and hybrid readings by institutional exclusion?',
    'Examination of canonical inclusion patterns and hermeneutic marginalization of priestly and restoration voices.',
    'If suppression is structural, the rope classification weakens toward tangled_rope; if the readings merely coexist, rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_readings, empirical, 'Institutional suppression of competing kernel readings.').

omega_variable(
    exit_options_for_identity_locked,
    'Are members of the covenant study community identity_locked into the constraint, or do they retain genuine exit mobility?',
    'Sociological study of exit patterns from traditional Torah-study communities.',
    'If identity_locked, effective extraction is higher than the zero base because identity fusion constitutes a non-monetary cost; this would raise directionality for community members.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_options_for_identity_locked, empirical, 'Identity lock vs genuine exit in the study community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.15).
narrative_ontology:measurement(temp_tr_t10, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 10, 0.15).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 20, 0.15).
narrative_ontology:measurement(temp_tr_t30, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 30, 0.15).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 40, 0.15).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(temp_be_t10, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 10, 0.02).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 20, 0.02).
narrative_ontology:measurement(temp_be_t30, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 30, 0.02).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 40, 0.02).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 50, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_commitment kernel; sibling constraints instantiate competing readings. Decomposition follows the Îµ-invariance principle: each reading has distinct structural data and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
