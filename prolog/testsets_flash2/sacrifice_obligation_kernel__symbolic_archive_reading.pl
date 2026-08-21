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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive (No Halakhic Claim)
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents a reading of Jewish sacrifice law that views
 *   it primarily as a cultural and historical archive. Study of these laws is
 *   understood as a means of preserving collective memory and identity,
 *   rather than fulfilling a binding halakhic (religious legal) obligation.
 *   This reading explicitly makes no halakhic claim, positioning the
 *   constraint as a 'mountain' of cultural fact and continuity, with
 *   negligible extraction or suppression. The beneficiaries are abstract
 *   concepts of Jewish memory and identity, which are reinforced by this
 *   non-coercive engagement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.01).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.01).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.01).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic Archive (No Halakhic Claim)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__symbolic_archive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, 'cb389f40-cda3-4124-98e7-3d47cda99b81').
narrative_ontology:cs_kernel_codification('cb389f40-cda3-4124-98e7-3d47cda99b81', fixed_text).
narrative_ontology:cs_authority_grounding('cb389f40-cda3-4124-98e7-3d47cda99b81', lineage).
narrative_ontology:cs_interpretation_layer_present('cb389f40-cda3-4124-98e7-3d47cda99b81').
narrative_ontology:cs_reading_relation('cb389f40-cda3-4124-98e7-3d47cda99b81', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb389f40-cda3-4124-98e7-3d47cda99b81', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb389f40-cda3-4124-98e7-3d47cda99b81', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('cb389f40-cda3-4124-98e7-3d47cda99b81', foundational, no_halakhic_obligation_without_temple).
narrative_ontology:cs_axiom_status(no_halakhic_obligation_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('cb389f40-cda3-4124-98e7-3d47cda99b81', no_halakhic_obligation_without_temple, conventional).
narrative_ontology:cs_axiom('cb389f40-cda3-4124-98e7-3d47cda99b81', foundational, study_as_cultural_preservation).
narrative_ontology:cs_axiom_status(study_as_cultural_preservation, holdable).
narrative_ontology:cs_axiom_grounding('cb389f40-cda3-4124-98e7-3d47cda99b81', study_as_cultural_preservation, instrumental).
narrative_ontology:cs_reference_frame('cb389f40-cda3-4124-98e7-3d47cda99b81', post_temple_cultural_continuity).
narrative_ontology:cs_drift_state('cb389f40-cda3-4124-98e7-3d47cda99b81', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cb389f40-cda3-4124-98e7-3d47cda99b81', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of historical and cultural continuity through the study of sacrifice law, ensuring that the past remains accessible and meaningful for future generations. This is a non-coercive, identity-affirming benefit.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    institutional, civilizational, identity_locked, global).

% Is reinforced and sustained by engagement with the historical archive of sacrifice law, providing a sense of shared heritage and belonging without imposing any active ritual obligation. The constraint is a source of cultural cohesion.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity, beneficiary,
    institutional, generational, identity_locked, global).

% Engage in the academic and cultural study of sacrifice law for intellectual enrichment and historical understanding. They are not bound by any halakhic obligation related to the sacrifices themselves, viewing the texts as historical artifacts.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, individual_scholars, observer,
    moderate, biographical, mobile, local).

% Are structurally excluded from asserting halakhic claims or obligations within this reading, as the constraint explicitly denies any binding legal force to the sacrifice laws. They would typically advocate for a reading that maintains halakhic relevance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authorities, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation and transmission of a significant body of historical and cultural texts, ensuring that knowledge of ancient Jewish practices and their textual foundations is maintained across generations.
% TRANSFER_FUNCTION: Transfers cultural knowledge and historical continuity from past generations to present and future ones, fostering a shared identity and collective memory without imposing ritual or financial burdens.
% ABSENT_VOICES: Halakhic authorities and those who believe in a contemporary, binding obligation for sacrifice would object, arguing that this reading strips the texts of their religious imperative. They are absent because this reading explicitly frames the laws as non-halakhic.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the physical texts of sacrifice law would still exist, and their historical study might continue, but the explicit framing of them as a non-halakhic cultural archive would be lost. The world would not rearrange itself in a material sense, but a specific interpretive lens for cultural continuity would be gone.
% FOUNDING_PROBLEM: The problem of maintaining Jewish identity and cultural continuity in the absence of the Temple and active sacrificial practice, ensuring that the rich textual tradition remains relevant.
% FOUNDING_PROBLEM_CORROBORATION: Historians, cultural anthropologists, and secular Jewish educators corroborate that maintaining cultural continuity and identity through historical study is an ongoing and live problem, independent of any halakhic claims.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.01, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__symbolic_archive_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness and suppression are near zero because this reading asserts no binding obligation; participation in study is voluntary and driven by cultural or intellectual interest. The 'mountain' classification reflects the idea that the historical existence and cultural significance of these texts are unchangeable facts, and their study for preservation is a natural outcome of a community's desire for continuity. Accessibility collapse is high because, within this frame, there are few 'alternatives' to the historical record itself. Resistance is low because no one is coerced.
 *
 * PERSPECTIVAL GAP:
 *   There is a significant perspectival gap between this reading and those that assert a binding halakhic obligation. While this reading sees the constraint as a non-extractive cultural mountain, other readings would experience it as a source of significant obligation or a contested domain of religious practice. The engine's classification of this specific reading as a mountain highlights its internal coherence as a non-binding cultural artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective memory and identity are beneficiaries, as their continuity is directly served by this interpretive framework. Individual scholars are observers, engaging with the material without being subject to its 'rules' in a halakhic sense. Halakhic authorities are 'excluded' because their primary function (adjudicating religious law) is explicitly denied relevance by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently resolves mandatrophy by denying any active mandate for sacrifice. The 'function' of the law is reinterpreted from ritual obligation to cultural preservation, thus preventing the constraint from becoming a 'piton' (a degraded ritual) or a 'snare' (an extractive obligation). The mandate is transformed, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    halakhic_vs_cultural_framing,
    'Is the framing of sacrifice law as a purely cultural-historical archive, devoid of halakhic claim, a defensible interpretation within traditional Jewish thought, or a modern re-framing?',
    'Analysis of historical rabbinic responsa and philosophical texts regarding the purpose of studying sacrifice law in the absence of the Temple. If traditional sources consistently deny halakhic relevance, the framing is defensible.',
    'If defensible, this reading''s ''mountain'' classification is robust. If it''s a modern re-framing, it might be seen as ''suppressing'' alternative halakhic readings, potentially shifting its classification towards a ''tangled_rope'' or ''snare'' from the perspective of those who maintain halakhic obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(halakhic_vs_cultural_framing, conceptual, 'Ambiguity regarding the historical and theological legitimacy of a purely cultural reading of sacrifice law.').

omega_variable(
    identity_coercion_ambiguity,
    'Does the ''beneficiary'' status of ''Jewish collective memory'' and ''Jewish identity'' imply a subtle, identity-based coercion to engage with the archive, even without explicit halakhic claim?',
    'Sociological studies of engagement patterns among non-observant Jews: if disengagement from the archive leads to no perceived loss of identity or social standing, then coercion is absent. If disengagement carries social costs, then a subtle ''identity_locked'' mechanism might be present.',
    'If subtle coercion is present, the ''extractiveness'' and ''suppression'' metrics, though low, might be understated, and the ''beneficiary'' status might mask a ''tangled_rope'' dynamic where identity is ''coordinated'' at a low, diffuse cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coercion_ambiguity, empirical, 'Whether cultural identity, even without halakhic claim, can exert a coercive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(sacr_be_t25, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 25, 0.01).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 50, 0.01).
narrative_ontology:measurement(sacr_be_t75, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 75, 0.01).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 100, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0, 0.01).
narrative_ontology:measurement(sacr_su_t25, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 25, 0.01).
narrative_ontology:measurement(sacr_su_t50, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 50, 0.01).
narrative_ontology:measurement(sacr_su_t75, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 75, 0.01).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 100, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
