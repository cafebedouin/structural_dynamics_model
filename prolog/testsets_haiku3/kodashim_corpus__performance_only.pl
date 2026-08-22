% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Performance-Only Husk
 *   domain: religious/commitment-system
 *
 * SUMMARY:
 *   The Kodashim corpus (the six orders of Mishnah dealing with sacrificial
 *   law and temple service) is a unified, authoritative body of Jewish law
 *   that has been inaccessible to physical performance since the destruction
 *   of the Second Temple in 70 CE. Under the performance-only reading
 *   instantiated here, this corpus is understood as an archived blueprint
 *   awaiting messianic restoration—a latent law code that remains binding and
 *   occupied but currently unfulfilled. The reading extracts legitimacy from
 *   a future physical performance it cannot deliver. Practitioners invest
 *   devotional and intellectual labor under the belief they are preparing for
 *   a restorable future state; institutions benefit from maintaining this
 *   reading because it justifies their authority position and the centrality
 *   of Kodashim study in the curriculum. The constraint is CLAIMED as snare
 *   (extraction from unrealizable future legitimacy) and the metrics reflect
 *   substantial extraction (0.82), moderate theater (0.68—the performative
 *   maintenance of study as preparation), and active suppression
 *   (0.71—enforcing the reading against competing interpretations). The
 *   measurement series tracks rising extractiveness and theater over 40 time
 *   units (centuries of rabbinic history, stylized as a continuous interval),
 *   reflecting institutional solidification of the performance-only reading
 *   and increasing reliance on theatrical preparation rather than functional
 *   mastery.
 *
 * KEY AGENTS:
 *   - Messianic-preparation institutions: the institutional authority structure (yeshivas, rabbinic councils) that administers the Kodashim corpus and frames it as blueprints awaiting restoration. Their authority position depends on the corpus remaining occupied but unfulfilled.
 *   - Devotional practitioners: believers and students who commit to mastering Kodashim law under the belief they are preparing for future physical performance. They bear the cost of studying law that cannot be performed and may never be performable (if messianic restoration does not occur or is perpetually deferred).
 *   - Competing theological readings: scholars and communities articulating alternative framings (study-as-exercise, where the mastery itself constitutes the fulfillment; substitution-archive, where sacrifice was replaced by prayer and study is memorial not occupation). These readings are excluded from institutional authority structures.
 *   - Analytical observer: sees the structural extraction mechanism—how a future-conditional reading sustains institutional authority while extracting current devotional resources from practitioners.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.82).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.71).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Performance-Only Husk").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/commitment-system").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '80f6e32e-7541-40a4-9b2d-eb69d58f2e0e').
narrative_ontology:cs_kernel_codification('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', fixed_text).
narrative_ontology:cs_authority_grounding('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', extraction).
narrative_ontology:cs_interpretation_layer_present('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e').
narrative_ontology:cs_reading_relation('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', foundational, corpus_awaits_restoration).
narrative_ontology:cs_axiom_status(corpus_awaits_restoration, holdable).
narrative_ontology:cs_axiom_grounding('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', corpus_awaits_restoration, deontological).
narrative_ontology:cs_axiom('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', secondary, current_study_preparatory_not_occupational).
narrative_ontology:cs_axiom_status(current_study_preparatory_not_occupational, holdable).
narrative_ontology:cs_axiom_grounding('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', current_study_preparatory_not_occupational, deontological).
narrative_ontology:cs_reference_frame('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', corpus_as_dormant_blueprint).
narrative_ontology:cs_drift_state('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', contemporary_indefinite_deferral, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80f6e32e-7541-40a4-9b2d-eb69d58f2e0e', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, yeshiva_establishment).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devotional_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manages and interprets the Kodashim corpus (six orders of Mishnah and Talmud detailing sacrificial law and temple service). Frames the corpus as an archived blueprint awaiting physical restoration in a messianic future. Controls the narrative that study is preparation, not fulfillment. Institutional authority derives from lineage (rabbinic succession) and extraction (the institutional position depends on the corpus remaining occupied but unfulfilled—fulfillment would demote institutions to historical memory).
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Benefits from the performance-only framing because it sustains the legitimacy of intensive study as preparation. The reading justifies yeshiva-centered practice and justifies allocating massive intellectual and devotional resources to mastering laws that cannot currently be performed. Yeshiva authority depends on Kodashim study remaining central to the curriculum.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, yeshiva_establishment, beneficiary,
    organized, generational, constrained, global).

% Invest devotional and intellectual effort in mastering the Kodashim corpus under the belief that this study constitutes preparation for future physical fulfillment. They bear the cost of treating an archived blueprint as a living practice. Their identity as observant Jews is fused with the commitment to prepare, making exit from the framework nearly impossible without internal identity dissolution. They experience the constraint as occupying their devotional life while delivering no realized performance.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devotional_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Scholars and communities advancing alternative readings (study-as-exercise, substitution-archive) are structurally excluded from the institutional framework's authority structure. Their readings would redistribute the meaning and value of Kodashim study in ways that undermine the performance-only framing's institutional position. They contest the constraint but lack enforcement power.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, competing_theological_readings, excluded,
    moderate, generational, constrained, global).

% Observes the structural mechanics of how a reading's institutional position depends on unfulfillable future conditions, and how that dependence creates extraction dynamics from those who adopt the reading as lived practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The performance-only reading coordinates messianic hope and textual interpretation: it holds that the Kodashim corpus represents a unified, authoritative, salvageable body of law that will resume physical instantiation at the eschaton. This provides theological coherence to the corpus and justifies its transmission and intensive study as preparation rather than antiquarian archive.
% TRANSFER_FUNCTION: Transfers devotional energy, intellectual labor, and legitimacy from practitioners (who invest in mastering laws they cannot perform) to messianic-preparation institutions (whose authority position depends on treating the corpus as occupied but unfulfilled). Practitioners allocate life hours to mastering sacrificial law; institutions collect the institutional legitimacy that sustained study of the corpus provides.
% ABSENT_VOICES: Scholars and practitioners holding alternative readings (study-as-exercise framing, substitution-archive framing) are structurally excluded from the institutional authority structures that enforce the performance-only reading. They would argue that the corpus's meaning is independent of future physical performance, or that it has been superseded; they cannot easily voice this within the authority structure because the structure's legitimacy depends on foreclosing it.
% DISAPPEARANCE_RATIONALE: If the performance-only reading and its institutional enforcement disappeared, the Kodashim corpus would be recontextualized: it would be studied as intellectual-spiritual exercise (strengthening the study-as-exercise reading), or frankly as memorial archive of superseded practice (the substitution-archive reading), or dropped from the core curriculum. Institutional positions that depend on Kodashim as unfulfilled blueprint would be reorganized. Devotional practitioners would redistribute their effort toward areas of law and practice that deliver realized performance.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), sacrifice ceased. The rabbinic tradition faced a choice: treat the corpus of sacrifice law as a sealed historical record, or maintain it as an occupied, though unfulfilled, framework awaiting restoration. The performance-only reading chose the latter: the corpus is not dead, but dormant—awaiting the messianic restoration of the Temple and the resumption of sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: The messianic-preparation institutions attest that the founding problem is live: the corpus remains a binding framework awaiting future performance because the messianic age is still expected. Competing theological readings (study-as-exercise, substitution-archive scholars) attest that the founding problem was solved in antiquity (either through intellectual-spiritual substitution or through formal supersession of the practice); they argue the performance-only reading mistakes institutional position for theological truth. Non-benefiting scholars of rabbinic Judaism and historians of Jewish practice from outside the institutional establishment provide testimony that the performance-only reading serves institutional interests rather than solving a present theological necessity.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.82, rising from 0.65 over the interval) reflects the core mechanism: practitioners are told their current study is preparation for a future state that cannot be objectively verified or scheduled. This creates a permanent legitimacy gap—the reading can never be falsified by absence of performance because non-performance is explained as deferred-to-the-eschaton. Suppression (0.71) is substantial because institutional authority structures actively enforce the performance-only reading against competing interpretations; scholars advancing study-as-exercise or substitution-archive framings are marginalized or excluded from mainstream yeshiva curricula. Theater ratio (0.68, rising) is high because increasing portions of Kodashim study are now performatively elaborated—commentarial traditions, hypothetical reconstructions, detailed mastery of inaccessible minutiae—rather than functional mastery for use. The rising trajectory on all three metrics over the interval models institutional solidification and increasing reliance on textual and ceremonial elaboration to sustain the reading's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (messianic-preparation institutions) and the payer seat (devotional practitioners) should compute differently under the engine's per-seat analysis. From the institutional seat, the arrangement genuinely coordinates hope, textual interpretation, and messianic preparation—a real theological function. From the practitioner seat, the same structure operates as enforced extraction: devotion and labor devoted to mastering laws that deliver no realized performance, whose future performance is indefinitely deferred and may never occur. The institutional seat has exit options (the institution could reframe the corpus as study-exercise or memorial archive, though doing so would demote institutional authority); the practitioner seat is trapped by identity fusion (to exit the performance-only reading often means exiting Jewish observance itself or undergoing internal theological crisis). The engine computes these seat-specific directionalities from the structural data; the authored metrics reflect the payer's seat perspective (high extraction, high suppression).
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions: d near 0.0 (full beneficiary). They set the reading, control the authority structure, collect institutional legitimacy and resource allocation from the corpus's centrality. They experience low effective extraction because the reading serves their interests. Devotional practitioners: d near 1.0 (full target). They bear the cost of preparation without realized performance, are trapped by identity fusion, experience the constraint as extractive because their devotion is allocated to a future state they cannot verify will arrive. Yeshiva establishment: d near 0.2 (beneficiary, but with some cost). They benefit from curriculum centrality and justification for intensive study, but also bear the cost of maintaining the reading in the face of competing interpretations. Competing theological readings: structurally excluded, so d would be computed as if they were trapped payers (d near 1.0) if they were seated, but they are excluded from the institutional conversation itself. The analytical observer (d=0.5, analytical) sees the structure but does not participate in the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The performance-only reading is neither a mountain nor a living coordination mechanism. It is a snare because its legitimacy depends on a future physical performance that is structurally unrealizable under the reading's own terms (the destruction of the Temple, the dependence on messianic restoration, and the institutional incentive to defer and elaborate rather than achieve closure all conspire to make performance perpetually unreachable). The reading extracts from practitioners by promising a preparation that can never be completed. The institutional position depends on this incompletion—if the reading were falsified or the future state were realized, institutional authority would be reorganized. The constraint persists because the institutional structures that benefit from it are powerful enough to suppress competing readings and because practitioners' identities are fused with the reading, making exit costly. The mandatrophy is live: the founding problem (how to maintain the corpus of sacrifice law after the Temple's destruction) was originally a genuine theological question. The performance-only answer (the corpus is blueprints awaiting restoration) is now sustained by institutional interests that conflict with practitioners' interests, making it extractive rather than coordinative. The reading extracts legitimate authority from a future state that institutions have every incentive to keep perpetually distant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_performance_realizability,
    'Is messianic restoration of physical sacrifice a realizable future state, or is it perpetually deferred within the reading''s own logic?',
    'Analysis of how the reading''s internal theology handles deferred or perpetually-postponed fulfillment. Examination of whether institutional actors have ever endorsed or accelerated closure toward physical performance, or whether they have consistently elaborated textual and hypothetical dimensions instead. Historical survey of eschatological timeline claims and their repeated postponement.',
    'If restoration is perpetually deferred by the reading''s internal logic (e.g., ''only the Messiah can initiate it, and we cannot force the Messiah''s arrival''), then the performance-only reading creates a structural condition where the future state is approached but never reached—a setup for infinite extraction. If restoration is theoretically realizable but currently inaccessible, the extraction is conditional and potentially temporary. The former supports strong snare classification; the latter qualifies it toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_performance_realizability, empirical, 'Whether the unfulfilled future state is structurally unreachable or merely deferred-to-messianic-timing.').

omega_variable(
    institutional_incentive_alignment,
    'Do messianic-preparation institutions have structural incentives to accelerate, maintain, or indefinitely defer the conditions for physical performance?',
    'Institutional budget analysis: do institutions invest in preparing for imminent restoration (building replicas, training priests, organizing logistics) or in elaborating textual mastery (commentaries, hypotheticals, theoretical refinements)? Historical survey of institutional positions on messianic timing and restoration preconditions.',
    'If institutions systematically defer or complicate the preconditions for restoration, that is evidence of extraction: institutional authority depends on the corpus remaining unperformed. If institutions accelerate preparation for imminent restoration, the reading is more consistent with genuine coordination. The measurement series'' rising theater_ratio (0.55 to 0.68) is consistent with institutional deferral through elaboration rather than preparation-for-performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_alignment, empirical, 'Whether institutional incentives align with restoration (coordination) or with perpetuation of the unfulfilled state (extraction).').

omega_variable(
    identity_lock_mechanism_in_practitioners,
    'Is the measured suppression (0.71) structural (enforcement of the reading against exit) or internalized (practitioners have fused their identity with the reading such that exit feels like self-dissolution)?',
    'Post-exit trajectory study: practitioners who leave the performance-only reading or leave the yeshiva structure entirely—do they maintain suppression of the alternative readings, or do they adopt them? Do they carry guilt, or do they experience liberation? Analysis of explicit teaching on the centrality of Kodashim study to Jewish identity and observance.',
    'If suppression is primarily structural (institutional barriers, curriculum control), then reducing institutional enforcement might allow practitioners to adopt alternative readings. If suppression is internalized (practitioners believe preparation is essential to their identity), then exit would require identity reconstruction, and the effective suppression is higher than the institutional enforcement alone. Identity-locked practitioners are deeper-trapped, supporting strong snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_practitioners, empirical, 'Whether practitioners'' lock to the performance-only reading is structural or internalized.').

omega_variable(
    study_as_exercise_boundary,
    'Is the performance-only reading logically incompatible with the study-as-exercise reading, or could both coexist as two valid ways of relating to the corpus?',
    'Theological analysis: can a practitioner consistently hold both ''this study prepares for future restoration'' and ''this study itself constitutes the performance'' without internal contradiction? Does the reading''s core premise require the exclusion of the study-as-exercise reading, or only assert its superiority?',
    'If performance-only FORECLOSES study-as-exercise (the readings are logically incompatible within a single framework), the suppression is defending a zero-sum boundary. If performance-only merely COEXISTS_WITH study-as-exercise (both valid, different parties hold both, no logical mutual exclusion), then the suppression is defending institutional preference rather than theological necessity, supporting stronger snare classification. The schema reflects coexistence, not foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_exercise_boundary, conceptual, 'Whether the performance-only reading''s core premise logically forecloses the study-as-exercise reading or merely competes with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t5, kodashim_corpus__performance_only, theater_ratio, 5, 0.58).
narrative_ontology:measurement_basis(koda_tr_t5, observed).
narrative_ontology:measurement(koda_tr_t10, kodashim_corpus__performance_only, theater_ratio, 10, 0.61).
narrative_ontology:measurement_basis(koda_tr_t10, observed).
narrative_ontology:measurement(koda_tr_t15, kodashim_corpus__performance_only, theater_ratio, 15, 0.63).
narrative_ontology:measurement_basis(koda_tr_t15, observed).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.65).
narrative_ontology:measurement_basis(koda_tr_t20, observed).
narrative_ontology:measurement(koda_tr_t25, kodashim_corpus__performance_only, theater_ratio, 25, 0.67).
narrative_ontology:measurement_basis(koda_tr_t25, observed).
narrative_ontology:measurement(koda_tr_t30, kodashim_corpus__performance_only, theater_ratio, 30, 0.68).
narrative_ontology:measurement_basis(koda_tr_t30, observed).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.68).
narrative_ontology:measurement_basis(koda_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t5, kodashim_corpus__performance_only, base_extractiveness, 5, 0.69).
narrative_ontology:measurement_basis(koda_be_t5, observed).
narrative_ontology:measurement(koda_be_t10, kodashim_corpus__performance_only, base_extractiveness, 10, 0.73).
narrative_ontology:measurement_basis(koda_be_t10, observed).
narrative_ontology:measurement(koda_be_t15, kodashim_corpus__performance_only, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(koda_be_t15, observed).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(koda_be_t20, observed).
narrative_ontology:measurement(koda_be_t25, kodashim_corpus__performance_only, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(koda_be_t25, observed).
narrative_ontology:measurement(koda_be_t30, kodashim_corpus__performance_only, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(koda_be_t30, observed).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(koda_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t5, kodashim_corpus__performance_only, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(koda_su_t5, observed).
narrative_ontology:measurement(koda_su_t10, kodashim_corpus__performance_only, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(koda_su_t10, observed).
narrative_ontology:measurement(koda_su_t15, kodashim_corpus__performance_only, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(koda_su_t15, observed).
narrative_ontology:measurement(koda_su_t20, kodashim_corpus__performance_only, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(koda_su_t20, observed).
narrative_ontology:measurement(koda_su_t25, kodashim_corpus__performance_only, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(koda_su_t25, observed).
narrative_ontology:measurement(koda_su_t30, kodashim_corpus__performance_only, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(koda_su_t30, observed).
narrative_ontology:measurement(koda_su_t40, kodashim_corpus__performance_only, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(koda_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__performance_only, 0.25).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The Kodashim kernel (the textual corpus of sacrificial law) is contested across three readings, each instantiating a different constraint with different ε values and types. This story (performance_only, snare, ε=0.82) frames Kodashim as unfulfilled blueprint awaiting restoration—high extractiveness. The study_as_exercise reading (ε=lower, type=rope) frames the same corpus as self-fulfilling through intellectual engagement—lower extraction, genuine coordination. The substitution_archive reading (ε≈0, type=mountain) frames Kodashim as memorial of superseded practice—negligible extraction, no coordination function. The three readings compete to interpret the same kernel; they have different ε values because they assess the corpus's occupation and fulfillment differently. Each story carries the relationship to its siblings via reading_relations in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__performance_only, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
