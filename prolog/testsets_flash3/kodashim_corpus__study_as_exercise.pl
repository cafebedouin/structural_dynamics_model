% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Corpus: Study as Mitzvah Performance
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'study as exercise' reading of the
 *   Kodashim corpus, a central body of Jewish law concerning Temple
 *   sacrifices. In this reading, the intellectual and spiritual engagement
 *   with the laws of sacrifice is itself considered a fulfillment of the
 *   mitzvah (divine commandment), particularly in the absence of the Temple.
 *   This reading posits zero extraction, as study is a complete and
 *   beneficial act for all participants, coordinating continuous engagement
 *   with the tradition. This is one reading of the 'kodashim_corpus' kernel,
 *   alongside 'performance_only' and 'substitution_archive'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.01).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.01).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Corpus: Study as Mitzvah Performance").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '10f1c7b4-8c9f-4cbf-9325-4bbc4acca198').
narrative_ontology:cs_kernel_codification('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', fixed_text).
narrative_ontology:cs_authority_grounding('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', lineage).
narrative_ontology:cs_interpretation_layer_present('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198').
narrative_ontology:cs_reading_relation('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', foundational, study_is_equivalent_to_action).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_action, holdable).
narrative_ontology:cs_axiom_grounding('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', study_is_equivalent_to_action, theological).
narrative_ontology:cs_axiom('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', secondary, divine_will_requires_continuous_engagement).
narrative_ontology:cs_axiom_status(divine_will_requires_continuous_engagement, holdable).
narrative_ontology:cs_axiom_grounding('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', divine_will_requires_continuous_engagement, deontological).
narrative_ontology:cs_reference_frame('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', rabbinic_halakhic_continuity).
narrative_ontology:cs_drift_state('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('10f1c7b4-8c9f-4cbf-9325-4bbc4acca198', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kodashim_corpus__study_as_exercise, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars are the primary agents of engagement with the Kodashim corpus. Their continuous intellectual-spiritual engagement is understood as the fulfillment of the mitzvah, maintaining cosmic order and spiritual connection. Their identity is deeply intertwined with this practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% The community benefits from the spiritual merit and cosmic order maintained by the scholars' study. It provides a continuous connection to the tradition and a sense of active participation in divine commandments, even in the absence of a Temple.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, jewish_community, beneficiary,
    organized, generational, identity_locked, global).

% While not directly studying the corpus, lay adherents support the institutions and scholars who do, through donations and communal participation. They 'pay' by deferring to scholarly authority and participating in a system where their direct engagement with sacrifice law is mediated.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, lay_adherents, payer,
    moderate, biographical, constrained, local).

% This group believes that only physical sacrifice can fulfill the mitzvah, viewing study as a temporary substitute rather than a complete fulfillment. They are excluded from the 'study as fulfillment' discourse, as their core premise is that the mitzvah is currently unfulfilled.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, messianic_restorationists, excluded,
    organized, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous spiritual and intellectual engagement of the Jewish people with the divine commandment of sacrifice, ensuring its ongoing relevance and fulfillment through study in the absence of a Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit and cosmic order from the act of scholarly study to the entire Jewish community, maintaining a continuous connection to the divine commandments.
% ABSENT_VOICES: Messianic restorationists, who believe only physical sacrifice fulfills the mitzvah, are excluded. They would argue that study is an incomplete substitute, not a full performance, and that the true mitzvah remains unfulfilled until the Temple is rebuilt.
% DISAPPEARANCE_RATIONALE: If the understanding of 'study as mitzvah' vanished, a core mechanism for Jewish spiritual continuity and engagement with a central commandment would be lost. The community would face a profound spiritual void, and the role of rabbinic scholarship would be fundamentally altered, leading to a significant reorganization of religious practice and identity.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the cessation of physical sacrifices left a void in Jewish religious practice, particularly concerning the central commandment of Kodashim (sacrifices).
% FOUNDING_PROBLEM_CORROBORATION: The ongoing absence of the Temple and the continued centrality of Kodashim in Jewish liturgy and thought, attested by centuries of rabbinic commentary and communal practice, corroborate that the problem remains live. This is widely accepted across diverse Jewish movements, not just by the direct beneficiaries of this reading.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.01, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero (0.01) because the act of study is considered a complete fulfillment, not a substitute or a burden. There are no identifiable victims, as no one is deprived or coerced. Suppression is minimal (0.05) as participation is voluntary and driven by spiritual commitment, not external enforcement. Accessibility collapse is high (0.95) because for adherents of this reading, there are no 'alternatives' to this form of engagement that would offer a more complete fulfillment of the mitzvah in the current era. Resistance is negligible (0.01) as this reading is widely accepted within mainstream Rabbinic Judaism. Theater ratio is zero (0.0) as the study is considered genuinely functional and spiritually efficacious.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a pure coordination mechanism, a 'rope' that binds the community to its tradition in a meaningful way. Other readings, such as 'performance_only', would view the current state as one of unfulfilled commandment, leading to a different classification (e.g., a 'snare' of unfulfilled obligation or a 'piton' of theatrical longing). The engine's computation of per-seat classification would reflect this divergence if other readings were modeled as distinct seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars are direct beneficiaries, as their life's work is framed as a direct fulfillment of divine will, granting them spiritual and communal status. The broader Jewish community also benefits from the spiritual continuity and cosmic order maintained by this practice. Lay adherents are 'payers' in a diffuse sense, supporting the system, but are also beneficiaries of the spiritual outcomes. Messianic restorationists are excluded, as their core premise (only physical sacrifice) fundamentally differs.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a genuine spiritual coordination as extraction. The 'study as exercise' reading directly addresses the potential mandatrophy of sacrifice law in the absence of the Temple, by re-mandating its function through intellectual engagement. It resolves the problem of an 'atrophied' commandment by providing a live, active mode of fulfillment, thus avoiding a 'piton' classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fulfillment_completeness_ambiguity,
    'Is intellectual study a complete fulfillment of the mitzvah of Kodashim, or a partial/temporary substitute?',
    'Theological consensus shift or a messianic event leading to Temple rebuilding and resumption of physical sacrifices. If physical sacrifices resume and are universally accepted as the *only* true fulfillment, this reading would be superseded.',
    'If study is deemed a partial substitute, the extractiveness would rise (as the community is ''deprived'' of full fulfillment), and the classification might shift towards a ''tangled_rope'' or ''snare'' of unfulfilled obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fulfillment_completeness_ambiguity, conceptual, 'Ambiguity regarding the completeness of study as fulfillment.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is the ''identity_locked'' exit option for scholars and community a genuine spiritual commitment versus a form of internalized suppression against alternative interpretations?',
    'Sociological study of exit pathways for individuals who reject this reading, examining the social and psychological costs. If costs are primarily external and coercive, reclassify as structural suppression.',
    'If internalized suppression is a significant factor, the effective suppression metric for these seats would be higher, potentially shifting the overall classification towards a ''tangled_rope'' by revealing hidden coercive elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Distinguishing genuine identity-lock from internalized suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.0).
narrative_ontology:measurement(koda_tr_t25, kodashim_corpus__study_as_exercise, theater_ratio, 25, 0.0).
narrative_ontology:measurement(koda_tr_t50, kodashim_corpus__study_as_exercise, theater_ratio, 50, 0.0).
narrative_ontology:measurement(koda_tr_t75, kodashim_corpus__study_as_exercise, theater_ratio, 75, 0.0).
narrative_ontology:measurement(koda_tr_t100, kodashim_corpus__study_as_exercise, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(koda_be_t25, kodashim_corpus__study_as_exercise, base_extractiveness, 25, 0.01).
narrative_ontology:measurement(koda_be_t50, kodashim_corpus__study_as_exercise, base_extractiveness, 50, 0.01).
narrative_ontology:measurement(koda_be_t75, kodashim_corpus__study_as_exercise, base_extractiveness, 75, 0.01).
narrative_ontology:measurement(koda_be_t100, kodashim_corpus__study_as_exercise, base_extractiveness, 100, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__study_as_exercise, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(koda_su_t25, kodashim_corpus__study_as_exercise, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(koda_su_t50, kodashim_corpus__study_as_exercise, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(koda_su_t75, kodashim_corpus__study_as_exercise, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(koda_su_t100, kodashim_corpus__study_as_exercise, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_corpus' kernel. This 'study_as_exercise' reading emphasizes continuous intellectual engagement as fulfillment, contrasting with 'performance_only' (awaiting physical restoration) and 'substitution_archive' (Kodashim as superseded history). Each reading represents a distinct structural claim about the kernel's current function and status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
