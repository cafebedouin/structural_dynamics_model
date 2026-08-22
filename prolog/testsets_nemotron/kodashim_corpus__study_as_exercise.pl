% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-23
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Complete Mitzvah Performance (Rabbinic Judaism)
 *   domain: religious/rabbinic_judaism/commitment_system
 *
 * SUMMARY:
 *   In Rabbinic Judaism, the Kodashim corpus (sacrificial law) presents a
 *   unique case: the Temple was destroyed in 70 CE, ending physical
 *   sacrifice, yet the legal corpus remains central to study. The
 *   study_as_exercise reading holds that intellectual engagement with
 *   sacrificial law IS the performance of the mitzvah — not a substitute, not
 *   a memorial, but the thing itself. The kernel (Kodashim corpus) is
 *   occupied through continuous study; the arrangement coordinates scholars
 *   around a shared interpretive practice that maintains cosmic order
 *   (Menachot 110a: 'Anyone who engages in the laws of sacrifice is as if he
 *   offered them'). This reading claims zero extractiveness — no one is
 *   deprived, no rents collected — and operates as pure coordination (rope)
 *   around the vindicated proposition that Torah study sustains the world.
 *
 * KEY AGENTS:
 *   - scholars_maintaining_cosmic_order: Primary beneficiary (institutional/analytical) — engages in study as divine service, maintains cosmic order through intellectual-spiritual labor
 *   - performance_only_adherents: Excluded voice (organized) — holds the kernel as archived blueprint awaiting messianic restoration; would object to study-as-fulfillment claim
 *   - substitution_archive_proponents: Excluded voice (organized) — holds prayer/study as functional replacement for sacrifice; would object to ontological identity claim
 *   - analytical_observer: Observer (analytical) — sees full structural field of the kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.02).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.02).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Study of Sacrifice Law as Complete Mitzvah Performance (Rabbinic Judaism)").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/rabbinic_judaism/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '738091e9-02b3-45cc-89bb-7bc9980a4abd').
narrative_ontology:cs_kernel_codification('738091e9-02b3-45cc-89bb-7bc9980a4abd', formalized).
narrative_ontology:cs_authority_grounding('738091e9-02b3-45cc-89bb-7bc9980a4abd', lineage).
narrative_ontology:cs_interpretation_layer_present('738091e9-02b3-45cc-89bb-7bc9980a4abd').
narrative_ontology:cs_reading_relation('738091e9-02b3-45cc-89bb-7bc9980a4abd', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('738091e9-02b3-45cc-89bb-7bc9980a4abd', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('738091e9-02b3-45cc-89bb-7bc9980a4abd', foundational, study_is_sacrifice_ontological_identity).
narrative_ontology:cs_axiom_status(study_is_sacrifice_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('738091e9-02b3-45cc-89bb-7bc9980a4abd', study_is_sacrifice_ontological_identity, deontological).
narrative_ontology:cs_axiom('738091e9-02b3-45cc-89bb-7bc9980a4abd', foundational, torah_study_sustains_cosmic_order).
narrative_ontology:cs_axiom_status(torah_study_sustains_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('738091e9-02b3-45cc-89bb-7bc9980a4abd', torah_study_sustains_cosmic_order, deontological).
narrative_ontology:cs_reference_frame('738091e9-02b3-45cc-89bb-7bc9980a4abd', talmudic_occupation_framework).
narrative_ontology:cs_drift_state('738091e9-02b3-45cc-89bb-7bc9980a4abd', contemporary_yeshiva_world, gap(stable, minor, true)).
narrative_ontology:cs_created_at('738091e9-02b3-45cc-89bb-7bc9980a4abd', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, scholars_maintaining_cosmic_order).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_study_sustains_world).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, intellectual_engagement_as_divine_service).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, kodashim_corpus_permanent_occupation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in Kodashim study as the primary mode of divine service in the post-Temple era. Their intellectual-spiritual labor is understood to maintain cosmic order (Menachot 110a). They occupy the kernel through continuous engagement. Exit would mean abandoning their vocational identity as Torah scholars and the communal authority that derives from it — identity_locked because the self-concept is constituted through this practice. They receive metaphysical benefit (cosmic order maintenance) and communal status; no material rents are collected from the constraint itself.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, scholars_maintaining_cosmic_order, beneficiary,
    institutional, generational, identity_locked, global).

% Hold that the Kodashim corpus is an archived blueprint — the real mitzvah is physical sacrifice, which awaits messianic restoration. Study is preparation, not fulfillment. They are excluded from the dominant yeshiva curriculum which centers study_as_exercise. Their position is structurally marginalized (not coercively suppressed) — trapped because their reading requires a future state (Third Temple) that the current arrangement treats as irrelevant to present obligation.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, performance_only_adherents, excluded,
    organized, generational, trapped, global).

% Hold that prayer (tefillah) and Torah study functionally replaced sacrifice — the kernel is a memorial archive documenting what was superseded. They are constrained rather than trapped: their reading is respected in academic and liberal religious circles but lacks authority in traditional halakhic decision-making. Exit options include moving to communities where this reading is dominant (Conservative/Reform movements, academic Jewish studies).
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, substitution_archive_proponents, excluded,
    organized, biographical, constrained, global).

% Observes the full structural field of the kernel contest without participating in any reading. Sees the coordination function (shared interpretive practice), the vindicated propositions (Torah study sustains world), and the zero-extractive structure. Provides the classification seat.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(kodashim_corpus__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a trans-generational community of scholars around the continuous intellectual-spiritual occupation of the Kodashim corpus, maintaining the vindicated proposition that Torah study sustains cosmic order in the absence of physical sacrifice.
% TRANSFER_FUNCTION: Moves no material resources. The arrangement transfers interpretive authority and communal status to those who maintain the study practice, but this is endogenous to the coordination (status follows engagement), not an external extraction. Cosmic order maintenance is a metaphysical benefit, not a transferable good.
% ABSENT_VOICES: Performance-only adherents (who would insist the kernel is a blueprint, not occupied) and substitution-archive proponents (who would insist study is functional replacement, not ontological identity) are present in the tradition but structurally excluded from halakhic authority in traditional yeshiva worlds. They are not silenced — their texts exist, their arguments are known — but they do not set the agenda for how the kernel is occupied.
% DISAPPEARANCE_RATIONALE: If the study_as_exercise constraint vanished overnight, the yeshiva world would lose its primary justification for Kodashim centrality. The curriculum would collapse or shift to performance_only or substitution_archive framings. The cosmic-order-maintenance framework would dissolve, redistributing interpretive authority and communal resources. The world of traditional Torah study would rearrange fundamentally.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), how to maintain the sacrificial order and cosmic stability that the Temple service provided? The founding problem was the catastrophic loss of the central divine-service institution and the need to occupy the kernel (sacrificial law) without its physical instantiation.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by the continuous expansion of Kodashim study in yeshiva curricula (Rambam, Vilna Gaon, Rav Chaim Brisker, contemporary rosh yeshivas all treat Kodashim as occupied kernel). No external corroboration needed — the founding problem's liveness is evidenced by the constraint's own persistence and growth. The performance_only reading corroborates that the physical Temple is not restored; the substitution_archive reading corroborates that functional replacement occurred but disputes the ontological claim.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero (0.02) because the constraint extracts nothing from anyone — study is voluntary, open, and the 'benefit' (cosmic order maintenance) is metaphysical and non-rival. Suppression is minimal (0.05) because alternative readings (performance_only, substitution_archive) coexist openly in the tradition without structural exclusion — they are marginalized through interpretive preference, not coercion. Theater ratio is low (0.08) because the study practice is functionally genuine, not performative; the engagement is the thing itself. Accessibility collapse is low (0.15) because alternatives remain conceptually available and structurally unblocked. Resistance is low (0.12) because the constraint meets little active opposition — it is the dominant reading in traditional yeshiva worlds. The claim/metric independence holds: claimed_type=rope, metrics describe a genuine coordination structure with negligible extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the scholar's seat (institutional/identity_locked), the constraint is pure coordination — study IS the mitzvah, cosmic order is maintained, no extraction occurs. From the performance_only seat (organized/trapped), the same structure appears as a displacement — the kernel is treated as occupied when it should be preserved as blueprint. From the substitution_archive seat (organized/constrained), it appears as a category error — study replaced sacrifice functionally but is claimed as ontologically identical. The engine computes these divergences from power/exit/role data; the constraint itself has no extraction to distribute.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: scholars_maintaining_cosmic_order — they receive the metaphysical benefit (cosmic order maintenance) and the communal status of occupying the kernel. No victim set exists — no one is deprived, no rents extracted. Directionality for scholars is near-beneficiary (d ≈ 0.1) because the constraint subsidizes their practice (provides the framework for divine service). For excluded voices, directionality is near-symmetric (d ≈ 0.5) — they are not targeted by the constraint, they simply hold a different reading. The analytical observer sits at d=0.5 (pure analysis).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (how to maintain sacrificial order without Temple) remains live — the Temple is not rebuilt, the cosmic order still requires maintenance. The arrangement has not atrophied; it has expanded (Kodashim study intensified post-destruction). No mandatrophy: the coordination function (shared interpretive practice maintaining cosmic order) is the steady state, not a transition. The mandate is resolved continuously through engagement, not frozen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is study_as_exercise a distinct reading of the kodashim_corpus kernel, or does it collapse into substitution_archive under historical analysis?',
    'Comparative textual analysis of classical sources (Menachot 110a, Rambam Hilchot Tefillah 1:1, Maharal Tiferet Yisrael Ch. 6) to determine whether study_as_exercise claims ontological identity with sacrifice or functional substitution.',
    'If study_as_exercise is ontological identity, it is a rope with zero extractiveness. If functional substitution, it shares structure with substitution_archive and may carry latent extractiveness toward scholars who maintain the substitution framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ontological status of study-as-performance claim within the kernel').

omega_variable(
    cosmic_order_beneficiary_verification,
    'Do scholars maintaining cosmic order through Kodashim study constitute genuine beneficiaries of a coordination function, or is ''cosmic order maintenance'' a vindicated proposition with no human rent-collector?',
    'Institutional analysis of yeshiva funding structures, rabbinic authority hierarchies, and communal resource allocation to trace whether material benefits flow to identifiable agents from the study-as-performance framework.',
    'If material benefits flow to identifiable scholars/institutions, the constraint may be a tangled_rope (coordination + extraction). If benefits are purely metaphysical and non-rent-collecting, it remains a pure rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_order_beneficiary_verification, empirical, 'Whether the declared beneficiary group captures material rents or only metaphysical status').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low suppression (0.05) structural (no barriers to alternative readings) or internalized (alternative readings exist but are marginalized through socialization)?',
    'Sociological study of yeshiva curricula, rabbinic ordination requirements, and communal boundary-maintenance to assess whether performance_only and substitution_archive proponents face structural exclusion or internalized devaluation.',
    'If internalized, effective suppression is higher than structural measure; the constraint operates as soft coordination with identity-locked exit for dissenters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in interpretive community').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_exercise_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(kodashim_study_exercise_tr_t0, observed).
narrative_ontology:measurement(kodashim_study_exercise_tr_t500, kodashim_corpus__study_as_exercise, theater_ratio, 500, 0.06).
narrative_ontology:measurement_basis(kodashim_study_exercise_tr_t500, observed).
narrative_ontology:measurement(kodashim_study_exercise_tr_t1000, kodashim_corpus__study_as_exercise, theater_ratio, 1000, 0.07).
narrative_ontology:measurement_basis(kodashim_study_exercise_tr_t1000, observed).
narrative_ontology:measurement(kodashim_study_exercise_tr_t1500, kodashim_corpus__study_as_exercise, theater_ratio, 1500, 0.08).
narrative_ontology:measurement_basis(kodashim_study_exercise_tr_t1500, observed).
narrative_ontology:measurement(kodashim_study_exercise_tr_t2000, kodashim_corpus__study_as_exercise, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(kodashim_study_exercise_tr_t2000, observed).
narrative_ontology:measurement(kodashim_study_exercise_tr_t2500, kodashim_corpus__study_as_exercise, theater_ratio, 2500, 0.08).
narrative_ontology:measurement_basis(kodashim_study_exercise_tr_t2500, observed).

% Extraction over time
narrative_ontology:measurement(kodashim_study_exercise_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement_basis(kodashim_study_exercise_be_t0, observed).
narrative_ontology:measurement(kodashim_study_exercise_be_t500, kodashim_corpus__study_as_exercise, base_extractiveness, 500, 0.02).
narrative_ontology:measurement_basis(kodashim_study_exercise_be_t500, observed).
narrative_ontology:measurement(kodashim_study_exercise_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement_basis(kodashim_study_exercise_be_t1000, observed).
narrative_ontology:measurement(kodashim_study_exercise_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement_basis(kodashim_study_exercise_be_t1500, observed).
narrative_ontology:measurement(kodashim_study_exercise_be_t2000, kodashim_corpus__study_as_exercise, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement_basis(kodashim_study_exercise_be_t2000, observed).
narrative_ontology:measurement(kodashim_study_exercise_be_t2500, kodashim_corpus__study_as_exercise, base_extractiveness, 2500, 0.02).
narrative_ontology:measurement_basis(kodashim_study_exercise_be_t2500, observed).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_study_exercise_su_t0, kodashim_corpus__study_as_exercise, suppression_requirement, 0, 0.03).
narrative_ontology:measurement_basis(kodashim_study_exercise_su_t0, observed).
narrative_ontology:measurement(kodashim_study_exercise_su_t500, kodashim_corpus__study_as_exercise, suppression_requirement, 500, 0.04).
narrative_ontology:measurement_basis(kodashim_study_exercise_su_t500, observed).
narrative_ontology:measurement(kodashim_study_exercise_su_t1000, kodashim_corpus__study_as_exercise, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement_basis(kodashim_study_exercise_su_t1000, observed).
narrative_ontology:measurement(kodashim_study_exercise_su_t1500, kodashim_corpus__study_as_exercise, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement_basis(kodashim_study_exercise_su_t1500, observed).
narrative_ontology:measurement(kodashim_study_exercise_su_t2000, kodashim_corpus__study_as_exercise, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement_basis(kodashim_study_exercise_su_t2000, observed).
narrative_ontology:measurement(kodashim_study_exercise_su_t2500, kodashim_corpus__study_as_exercise, suppression_requirement, 2500, 0.05).
narrative_ontology:measurement_basis(kodashim_study_exercise_su_t2500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__study_as_exercise, 0.08).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% Part of the kodashim_corpus constraint family. This reading (study_as_exercise) claims ontological identity between study and sacrifice; performance_only claims the kernel is a blueprint awaiting physical restoration; substitution_archive claims functional replacement. The three readings differ in ε (0.02 vs. 0.15 vs. 0.08) and victim structure (none vs. scholars_displaced vs. none). Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
