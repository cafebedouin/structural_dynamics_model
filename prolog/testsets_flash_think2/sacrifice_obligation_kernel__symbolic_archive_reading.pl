% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive (Study Reading)
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'symbolic_archive_reading' of the
 *   'sacrifice_obligation_kernel'. It frames the study of ancient Jewish
 *   sacrifice law as a voluntary cultural practice aimed at preserving
 *   collective memory and identity, explicitly making no binding halakhic
 *   claim. This reading emerged as a way to maintain engagement with these
 *   texts after the destruction of the Temple rendered physical sacrifices
 *   impossible, without imposing an unfulfillable religious obligation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic Archive (Study Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '69142927-3073-48b4-b451-0d36e00fd63c').
narrative_ontology:cs_kernel_codification('69142927-3073-48b4-b451-0d36e00fd63c', fixed_text).
narrative_ontology:cs_authority_grounding('69142927-3073-48b4-b451-0d36e00fd63c', lineage).
narrative_ontology:cs_interpretation_layer_present('69142927-3073-48b4-b451-0d36e00fd63c').
narrative_ontology:cs_reading_relation('69142927-3073-48b4-b451-0d36e00fd63c', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('69142927-3073-48b4-b451-0d36e00fd63c', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('69142927-3073-48b4-b451-0d36e00fd63c', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_axiom('69142927-3073-48b4-b451-0d36e00fd63c', foundational, sacrifice_law_is_historical_archive).
narrative_ontology:cs_axiom_status(sacrifice_law_is_historical_archive, holdable).
narrative_ontology:cs_axiom_grounding('69142927-3073-48b4-b451-0d36e00fd63c', sacrifice_law_is_historical_archive, conventional).
narrative_ontology:cs_axiom('69142927-3073-48b4-b451-0d36e00fd63c', foundational, study_preserves_identity_not_fulfills_mitzvah).
narrative_ontology:cs_axiom_status(study_preserves_identity_not_fulfills_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('69142927-3073-48b4-b451-0d36e00fd63c', study_preserves_identity_not_fulfills_mitzvah, conventional).
narrative_ontology:cs_reference_frame('69142927-3073-48b4-b451-0d36e00fd63c', post_temple_diaspora_cultural_preservation).
narrative_ontology:cs_drift_state('69142927-3073-48b4-b451-0d36e00fd63c', contemporary_jewish_studies_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('69142927-3073-48b4-b451-0d36e00fd63c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, students_of_torah).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, rabbis_and_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, secular_jewish_cultural_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__symbolic_archive_reading, students_of_torah).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of ancient texts and traditions, ensuring historical continuity and access to foundational narratives for future generations.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    institutional, civilizational, analytical, universal).

% Is reinforced and sustained by engagement with the historical archive of sacrifice law, providing a sense of belonging and connection to a shared past.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity, beneficiary,
    institutional, generational, identity_locked, universal).

% Invest time and intellectual effort in studying sacrifice law, gaining knowledge and contributing to its preservation. They benefit from the intellectual and spiritual enrichment, but bear the cost of study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, students_of_torah, payer,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, students_of_torah, beneficiary).

% Guide the study of sacrifice law, interpret its historical and cultural significance, and transmit this knowledge. They benefit from their role in preserving and shaping Jewish cultural heritage.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, rabbis_and_scholars, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, rabbis_and_scholars, beneficiary).

% Utilize the study of sacrifice law as a resource for educational programs, exhibitions, and cultural events, reinforcing Jewish heritage for a broader audience without religious obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, secular_jewish_cultural_institutions, beneficiary,
    organized, generational, mobile, national).

% Represent alternative interpretations where sacrifice law carries a binding halakhic obligation (e.g., requiring performance or fulfillment through study). They are excluded from this reading's purely archival framing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authorities_of_other_readings, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the voluntary preservation and transmission of ancient Jewish legal texts and cultural memory related to sacrificial practices, ensuring continuity of heritage across generations.
% TRANSFER_FUNCTION: Transfers historical knowledge, cultural narratives, and a sense of collective identity from past generations to present and future ones, through the act of study and interpretation.
% ABSENT_VOICES: Those who adhere to readings where sacrifice law carries a binding halakhic obligation (e.g., 'performance_only_reading' or 'study_as_exercise_reading') would object, arguing that this reading diminishes the religious imperative and reduces sacred text to mere history.
% DISAPPEARANCE_RATIONALE: If this reading vanished, a significant framework for understanding and engaging with ancient Jewish legal texts would be lost. The cultural and historical continuity it provides would be severed, impacting collective Jewish identity and the transmission of heritage, potentially leading to a decline in the study of these texts.
% FOUNDING_PROBLEM: The problem of maintaining the relevance, memory, and cultural significance of ancient sacrificial practices and their associated laws after the destruction of the Second Temple and the cessation of physical sacrifices, in a way that did not impose an unfulfillable religious obligation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion, cultural anthropologists, and sociologists of Jewish identity widely corroborate the role of post-Temple study in cultural and identity preservation, independent of specific halakhic claims. Academic scholarship outside traditional religious institutions supports this function.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because study is voluntary and imposes no coercive costs; it explicitly disavows any binding halakhic claim. Suppression is negligible (0.02) as there are no active enforcement mechanisms to compel this form of study, nor are alternatives for cultural engagement suppressed. Theater ratio is low (0.1) because the stated purpose of cultural preservation is genuinely fulfilled through study. Accessibility collapse is moderate (0.6) as while other forms of cultural preservation exist, this specific engagement with sacrifice law as an archive is a unique and valued pathway. Resistance is minimal (0.01) as it is a widely accepted and non-coercive practice.
 *
 * PERSPECTIVAL GAP:
 *   Adherents of other readings (e.g., 'performance_only_reading' or 'study_as_exercise_reading') would view this 'symbolic_archive_reading' as a significant departure from, or even an evasion of, a binding religious obligation. They would argue that reducing sacrifice law to a purely historical archive diminishes its sacred status and the imperative for its fulfillment, whether through physical performance or intellectual engagement. This reading, however, sees itself as a necessary and legitimate adaptation for cultural survival.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective memory and identity are direct beneficiaries, as the practice ensures their continuity. Students, rabbis, and cultural institutions also benefit from their engagement and role in this preservation, though students bear the cost of their effort. There are no victims, as no party is coerced or extracted from. Halakhic authorities of other readings are 'excluded' as their interpretive framework is not acknowledged by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent reading of the ''sacrifice_obligation_kernel'', or a strategic re-framing to avoid unfulfillable obligations?',
    'Historical and theological analysis of the development of post-Temple Jewish thought, examining whether this reading emerged organically as a theological position or as a pragmatic response to historical conditions.',
    'If a strategic re-framing, its ''rope'' classification might mask a deeper ''snare'' of unfulfillable obligation that this reading attempts to mitigate, shifting the burden of ''unfulfillability'' rather than resolving it. If organic, the ''rope'' classification stands as a genuine coordination of cultural memory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the origin and nature of this kernel reading.').

omega_variable(
    subtle_obligation_ambiguity,
    'Does the act of studying sacrifice law, even when framed as purely cultural preservation, implicitly carry a subtle, non-halakhic form of identity-based obligation or social pressure within the Jewish community?',
    'Sociological studies of Jewish educational practices and community norms, examining motivations for study and perceived social consequences of non-participation.',
    'If a subtle obligation exists, the ''extractiveness'' and ''suppression'' metrics might be slightly higher than currently assessed, reflecting a diffuse, internalized pressure to participate in cultural transmission, even if not halakhically binding. This would shift the classification slightly towards a ''tangled_rope'' for individual students.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subtle_obligation_ambiguity, empirical, 'Whether cultural study implies a subtle, non-halakhic obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 70, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 70, 0.05).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 500, 0.04).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 70, 0.02).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 500, 0.02).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 1000, 0.02).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 2000, 0.02).
narrative_ontology:measurement(sacr_su_t2024, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'sacrifice_obligation_kernel', each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
