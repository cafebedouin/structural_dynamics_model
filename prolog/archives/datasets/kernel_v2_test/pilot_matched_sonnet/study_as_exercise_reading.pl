% ============================================================================
% CONSTRAINT STORY: study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_exercise_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: study_as_exercise_reading
 *   human_readable: Study as Exercise of Sacrifice Obligation (Rabbinic Reading)
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The study-as-exercise reading of the sacrifice obligation kernel holds
 *   that intellectual engagement with sacrifice law constitutes genuine
 *   performance of the mitzvah under post-Temple conditions. This reading is
 *   grounded in Talmudic authorization (Menachot 110a: 'whoever engages in
 *   the study of the sacrifice laws, it is as if they offered the sacrifice')
 *   and represents the dominant rabbinic position across Ashkenazi and
 *   Sephardi traditions. The constraint coordinates observance of an
 *   obligation whose original performance mode (physical sacrifice in the
 *   Jerusalem Temple) has been structurally unavailable since 70 CE. The
 *   transformation from physical to intellectual performance preserves the
 *   mitzvah's binding force while adapting to historical constraint.
 *   Extractiveness is minimal (0.08) because the reading solves a genuine
 *   coordination problem: how to maintain obligation without performance
 *   infrastructure. Rabbinic authority benefits from interpretive monopoly on
 *   what counts as fulfillment, but this benefit is secondary to the
 *   coordination function and constrained by textual-fidelity requirements.
 *   Theater ratio is low (0.15) because study is not performative — it
 *   requires substantive engagement with complex legal material (Talmudic
 *   tractates Zevachim, Menachot, Kodashim). Suppression is moderate (0.25)
 *   because the reading is identity-locked for observant Jews (exit requires
 *   abandoning halakhic framework entirely) and because alternative
 *   fulfillment interpretations must demonstrate textual warrant to gain
 *   legitimacy.
 *
 * KEY AGENTS:
 *   - Individual Observant Jew: Primary coordinated party (powerless/identity_locked) — maintains mitzvah observance through study; identity-locked within halakhic framework but experiences low extraction
 *   - Rabbinic Interpretive Authority: Primary beneficiary (institutional/constrained) — holds interpretive monopoly on fulfillment criteria; constrained by textual tradition
 *   - Yeshiva System: Secondary beneficiary (organized/constrained) — coordinates transmission of sacrifice law study; benefits from institutional centrality
 *   - Analytical Observer: External perspective (analytical/analytical) — sees authorized transformation solving genuine coordination problem with minimal extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_exercise_reading, 0.08).
domain_priors:suppression_score(study_as_exercise_reading, 0.25).
domain_priors:theater_ratio(study_as_exercise_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_exercise_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(study_as_exercise_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(study_as_exercise_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(study_as_exercise_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(study_as_exercise_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_exercise_reading, rope).
narrative_ontology:human_readable(study_as_exercise_reading, "Study as Exercise of Sacrifice Obligation (Rabbinic Reading)").
narrative_ontology:topic_domain(study_as_exercise_reading, "religious_law/halakhic_authority/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(study_as_exercise_reading, '2667d8e7-23ed-423a-a8df-971bfa1a54cd').
narrative_ontology:cs_kernel_codification('2667d8e7-23ed-423a-a8df-971bfa1a54cd', fixed_text).
narrative_ontology:cs_authority_grounding('2667d8e7-23ed-423a-a8df-971bfa1a54cd', lineage).
narrative_ontology:cs_interpretation_layer_present('2667d8e7-23ed-423a-a8df-971bfa1a54cd').
narrative_ontology:cs_reading_relation('2667d8e7-23ed-423a-a8df-971bfa1a54cd', study_as_exercise_reading__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('2667d8e7-23ed-423a-a8df-971bfa1a54cd', study_as_exercise_reading__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('2667d8e7-23ed-423a-a8df-971bfa1a54cd', study_as_exercise_reading__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('2667d8e7-23ed-423a-a8df-971bfa1a54cd', foundational, intellectual_engagement_constitutes_performance).
narrative_ontology:cs_axiom_status(intellectual_engagement_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('2667d8e7-23ed-423a-a8df-971bfa1a54cd', intellectual_engagement_constitutes_performance, conventional).
narrative_ontology:cs_axiom('2667d8e7-23ed-423a-a8df-971bfa1a54cd', secondary, talmudic_equivalence_doctrine).
narrative_ontology:cs_axiom_status(talmudic_equivalence_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('2667d8e7-23ed-423a-a8df-971bfa1a54cd', talmudic_equivalence_doctrine, conventional).
narrative_ontology:cs_reference_frame('2667d8e7-23ed-423a-a8df-971bfa1a54cd', talmudic_authorization_framework).
narrative_ontology:cs_drift_state('2667d8e7-23ed-423a-a8df-971bfa1a54cd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2667d8e7-23ed-423a-a8df-971bfa1a54cd', '').
narrative_ontology:cs_kernel_id(study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_exercise_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(study_as_exercise_reading, contemporary_observant_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(study_as_exercise_reading, yeshiva_system).
narrative_ontology:constraint_victim(study_as_exercise_reading, individual_observant_jew).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the obligation to study sacrifice law as fulfillment of the mitzvah. Identity-locked within halakhic framework — exit requires abandoning religious identity entirely. Experiences study as accessible and meaningful fulfillment path, not as extraction. The 'cost' is time and intellectual effort, but this is experienced as legitimate religious practice rather than imposed burden.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, individual_observant_jew, payer,
    powerless, biographical, identity_locked, local).

% Sets the criteria for what counts as valid fulfillment of the sacrifice obligation. Benefits from interpretive monopoly — authority to determine that study constitutes performance. Constrained by need to demonstrate textual warrant in Talmudic sources. Dual-positioned: coordinates the transformation (agenda_setter) while also collecting institutional authority from it (beneficiary). The benefit is real but secondary to coordination function.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(study_as_exercise_reading, rabbinic_interpretive_authority, beneficiary).

% Organized institutional structure that transmits sacrifice law study. Benefits from centrality in mitzvah observance — yeshivot are the primary sites where this obligation is fulfilled. Constrained by textual-fidelity requirements and by competition with other study priorities. Collects institutional resources (students, funding, prestige) from its role in coordinating this mitzvah.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, yeshiva_system, beneficiary,
    organized, generational, constrained, regional).

% The collective body of Jews who maintain halakhic observance. Benefits from the reading because it preserves mitzvah accessibility — the obligation remains fulfillable despite Temple absence. Identity-locked as a community (exit requires leaving the tradition). Experiences the transformation as successful adaptation rather than extraction.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, contemporary_observant_community, beneficiary,
    organized, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains observance of sacrifice mitzvot under post-Temple conditions by transforming physical performance into intellectual engagement. Solves the problem: how to preserve binding obligation when original performance mode is structurally unavailable.
% TRANSFER_FUNCTION: Transfers interpretive authority over fulfillment criteria from Temple priesthood (no longer extant) to rabbinic scholars. Transfers time and intellectual effort from individual observants into study of sacrifice law. No monetary transfer — study is accessible without payment.
% ABSENT_VOICES: Karaite Jews and some Reconstructionist Jews reject rabbinic interpretive authority and would dispute that study constitutes fulfillment. They are not in the conversation because this reading is internal to rabbinic Judaism. Their absence does not indicate extraction — they hold alternative readings of the kernel (symbolic_archive or performance_only) rather than being excluded from this reading's benefits.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, observant Jews would face an unfulfillable obligation (sacrifice mitzvot remain binding but Temple is absent). The community would need to adopt an alternative reading: messianic_suspension (obligation suspended until Temple restoration), performance_only (obligation binding but unfulfillable, creating guilt/extraction), or symbolic_archive (obligation dissolved). Current practice (yeshiva study of Kodashim, individual study of sacrifice law) would lose its religious meaning and become either academic interest or memorial ritual. The world rearranges because arrangements (study curricula, yeshiva priorities, individual practice) depend on this reading's legitimacy.
% FOUNDING_PROBLEM: Temple destruction in 70 CE made physical sacrifice performance impossible. The founding problem was: how to maintain sacrifice mitzvot as binding obligations when their original performance mode is structurally unavailable? The problem was theological (preserving covenant obligations) and communal (maintaining Jewish identity and practice continuity).
% FOUNDING_PROBLEM_CORROBORATION: The Temple remains absent (corroborated by historical fact). The theological problem persists for observant Jews who maintain that mitzvot are binding (corroborated by continued halakhic observance across Orthodox, Conservative, and some Reform communities). The problem's liveness is attested by: (1) continued study of sacrifice law in yeshivot (observable institutional practice), (2) inclusion of sacrifice-related prayers in liturgy (observable ritual practice), (3) halakhic discussions of sacrifice law in contemporary responsa (observable legal discourse). Corroboration comes from both internal sources (rabbinic authorities across denominations) and external observers (academic scholars of Judaism document the practice).
narrative_ontology:disappearance_verdict(study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(study_as_exercise_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL OBSERVANT (ROPE) — Identity-locked within halakhic framework but experiences study-as-fulfillment as genuine coordination: the mitzvah remains accessible despite Temple absence. Low extraction — the transformation preserves obligation's meaning while adapting to historical constraint.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RABBINIC AUTHORITY (ROPE) — Benefits from interpretive monopoly on what counts as fulfillment, but experiences this as legitimate coordination function: preserving mitzvah observance under changed conditions. Constrained by need to maintain continuity with textual tradition.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: YESHIVA SYSTEM (ROPE) — Organized institutional structure that coordinates transmission of sacrifice law study. Benefits from centrality in observance but constrained by textual fidelity requirements. Experiences as coordination of educational resources toward mitzvah fulfillment.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From civilizational perspective, this reading solves genuine coordination problem: how to maintain obligation's binding force when its original performance mode is structurally unavailable. Low extraction — the transformation is authorized by the tradition's own interpretive principles (Torah study as equivalent to performance, Menachot 110a). Rabbinic benefit is real but secondary to coordination function.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_exercise_reading_tests).
:- end_tests(study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The reading solves a genuine coordination problem (maintaining obligation without Temple) through authorized transformation grounded in Talmudic text. Rabbinic benefit from interpretive monopoly is real but constrained by textual-fidelity requirements — novel interpretations must demonstrate warrant in authoritative sources. The slight extraction (not zero) reflects that gatekeeping function has some institutional overhead and that alternative readings (performance_only, messianic_suspension) are marginalized through interpretive authority rather than purely textual necessity. Suppression (0.25): Low-moderate. The reading is identity-locked for observant Jews (exit requires abandoning halakhic framework), and alternative fulfillment interpretations face epistemic barriers (must demonstrate textual grounding). But suppression is not high because the transformation is widely accepted within the tradition and because study is genuinely accessible (no resource barriers beyond literacy and time). Theater ratio (0.15): Very low. Study of sacrifice law requires substantive engagement with complex Talmudic material — it is not performative ritual. The slight theater component reflects that some study may be motivated by obligation-fulfillment rather than intrinsic interest, but the intellectual content is real. Measurements show modest increase over time as institutional structures (yeshiva system) mature and as distance from Temple increases, but trajectory remains low.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as rope. The individual observant Jew, rabbinic authority, yeshiva system, and analytical observer all experience the constraint as coordination solving a genuine problem (maintaining mitzvah observance without Temple). The uniformity reflects that the reading is structurally successful: it preserves obligation's binding force through authorized transformation with minimal extraction. The slight variation in experienced extraction (individual < yeshiva < rabbinic < analytical) reflects different positions relative to interpretive authority, but all remain well below the tangled_rope threshold. This uniform-type pattern is diagnostic: when a commitment-system reading produces rope from all perspectives, the transformation has succeeded in its coordination function without embedding significant extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives experience low directionality (d near 0.0-0.2) because all agents are net beneficiaries or symmetric participants in the coordination. Individual observant Jews benefit from accessible fulfillment path. Rabbinic authority benefits from interpretive centrality but is constrained by textual tradition. Yeshiva system benefits from institutional role. No victim set exists because the transformation is authorized by the tradition's own interpretive principles — this is not extraction masked as coordination, but genuine adaptation to structural constraint (Temple absence). The identity_locked exit option for individual observant Jews reflects that exit requires abandoning the halakhic framework entirely, not that the constraint itself is extractive. The analytical observer sees the rabbinic benefit as legitimate coordination overhead rather than extraction because alternative interpretations are filtered through textual-fidelity criteria (epistemic gate) rather than blocked institutionally (coercive gate).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that authorized transformation can preserve obligation without extraction. The original mandate (sacrifice performance) is structurally unavailable, but the transformed mandate (sacrifice study) maintains binding force through textual authorization (Menachot 110a). The reading is not mandatrophy because the function has not outlived its purpose — the purpose (mitzvah observance) persists, and the transformation enables it. The low theater_ratio confirms that study is not performative maintenance of a dead function but substantive engagement with living obligation. The constraint would become mandatrophy only if binding force degraded (omega: accessibility_vs_dilution) such that study became academic exercise maintained through institutional inertia rather than felt obligation. Current measurements show no such degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the study_as_exercise_reading of the sacrifice_obligation_kernel. Sibling readings: performance_only_reading (physical performance is the only valid fulfillment; study is preparation, not exercise), messianic_suspension_reading (obligation is suspended until Temple restoration; study is memorial, not fulfillment), symbolic_archive_reading (sacrifice law is historical record with no binding obligation; study is academic, not religious). What structural element distinguishes this reading from its siblings?',
    'The distinguishing element is the ontological status of intellectual engagement: this reading holds that study CONSTITUTES performance of the mitzvah under current conditions (authorized transformation via Menachot 110a: ''whoever engages in the study of the sacrifice laws, it is as if they offered the sacrifice''). Performance_only denies this equivalence. Messianic_suspension accepts the equivalence but frames it as temporary placeholder. Symbolic_archive denies binding obligation entirely.',
    'If study is genuine exercise: zero extractiveness, pure coordination (this reading). If study is placeholder: moderate extraction, scaffold classification (messianic_suspension). If study is preparation only: high extraction, snare classification (performance_only locks obligation without fulfillment path). If no binding obligation: constraint dissolves (symbolic_archive).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame structural delta: ontological status of study as mitzvah fulfillment').

omega_variable(
    interpretive_monopoly_extraction,
    'Does rabbinic authority''s benefit from interpretive monopoly on what counts as fulfillment constitute extraction, or is it legitimate coordination overhead?',
    'Measure whether alternative fulfillment interpretations are suppressed coercively or filtered through textual-fidelity criteria. If suppression is primarily epistemic (must demonstrate grounding in authoritative texts) rather than institutional (must have rabbinic approval regardless of textual warrant), the monopoly is coordination. If novel interpretations with textual warrant are blocked institutionally, extraction is present.',
    'If coordination: extractiveness remains low (0.08), rope classification holds. If extraction: extractiveness rises to 0.35-0.45, reclassifies as tangled_rope (genuine coordination function with embedded extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_extraction, empirical, 'Whether rabbinic interpretive monopoly is coordination overhead or extractive gatekeeping').

omega_variable(
    accessibility_vs_dilution,
    'Does transforming physical performance into intellectual engagement preserve the mitzvah''s binding force, or dilute it into academic exercise?',
    'Longitudinal measurement of observance rates and phenomenological reports of obligation''s subjective binding force. If study-as-exercise maintains comparable binding force to other active mitzvot (Shabbat, kashrut), transformation is successful. If binding force degrades toward academic interest, transformation failed and the reading is aspirational rather than descriptive.',
    'If binding force preserved: rope classification confirmed, low theater_ratio justified. If binding force degraded: theater_ratio rises above 0.5, reclassifies as piton (maintained through institutional inertia despite functional atrophy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_vs_dilution, empirical, 'Whether intellectual transformation preserves or dilutes obligation''s binding force').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_exercise_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(study_ex_theater_talmudic, study_as_exercise_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(study_ex_theater_medieval, study_as_exercise_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement(study_ex_theater_modern, study_as_exercise_reading, theater_ratio, 1000, 0.15).

% Extraction over time
narrative_ontology:measurement(study_ex_extract_talmudic, study_as_exercise_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(study_ex_extract_medieval, study_as_exercise_reading, base_extractiveness, 500, 0.07).
narrative_ontology:measurement(study_ex_extract_modern, study_as_exercise_reading, base_extractiveness, 1000, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(study_ex_suppress_talmudic, study_as_exercise_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(study_ex_suppress_medieval, study_as_exercise_reading, suppression_requirement, 500, 0.23).
narrative_ontology:measurement(study_ex_suppress_modern, study_as_exercise_reading, suppression_requirement, 1000, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_exercise_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_kernel. Other readings (performance_only, messianic_suspension, symbolic_archive) are separate constraint stories with different extractiveness values and victim sets. They are linked through the kernel structure, not through affects_constraints edges, because they are alternative framings of the same commitment rather than causally dependent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
