% ============================================================================
% CONSTRAINT STORY: study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_performance, []).

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
 *   constraint_id: study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfilled Obligation
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   The study-as-performance reading instantiates one resolution of a
 *   contested kernel: the sacrifice obligation's continuity after the
 *   destruction of the Temple. In this reading, rabbinic Judaism establishes
 *   that study of sacrifice law — textual engagement with the legal and
 *   theological dimensions of the obligation — constitutes the legitimate and
 *   permanent fulfillment of the obligation itself. Study is not a substitute
 *   awaiting restoration of the Temple; it is the substitution mechanism that
 *   makes the obligation persistent and accessible across a diaspora
 *   community without centralized ritual infrastructure. This reading
 *   presents a pure coordination mechanism: the studying community solves the
 *   collective problem of preserving the obligation's binding force without
 *   Temple access. Low extractiveness (0.15) reflects that study is an
 *   accessible fulfillment form with minimal barriers and no asymmetric
 *   distribution of benefits. No victims are produced because the obligation
 *   is satisfied through participation in study, and study itself is
 *   inclusive and non-coercive.
 *
 * KEY AGENTS:
 *   - Studying Community: Primary beneficiary (powerless/identity_locked) — constitutes their identity through scholarship; the obligation is fulfilled through their participation in textual engagement
 *   - Yeshiva Institution: Secondary beneficiary (organized/constrained) — maintains tradition through institutional transmission; constrained by tradition but benefits from clear obligation structure
 *   - Rabbinic Textual Tradition: Tertiary beneficiary (institutional/mobile) — authority structure that interprets the reading; benefits from study sustaining the canon
 *   - Analytical Observer: Fourth perspective (analytical/analytical) — sees the pure coordination structure: diaspora community coordinates on shared obligation that substitutes for centralized ritual
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_performance, 0.15).
domain_priors:suppression_score(study_as_performance, 0.08).
domain_priors:theater_ratio(study_as_performance, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(study_as_performance, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_performance, rope).
narrative_ontology:human_readable(study_as_performance, "Study of Sacrifice Law as Fulfilled Obligation").
narrative_ontology:topic_domain(study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(study_as_performance, '5a83760e-db9b-4f44-8135-85b767680158').
narrative_ontology:cs_kernel_codification('5a83760e-db9b-4f44-8135-85b767680158', fixed_text).
narrative_ontology:cs_authority_grounding('5a83760e-db9b-4f44-8135-85b767680158', lineage).
narrative_ontology:cs_interpretation_layer_present('5a83760e-db9b-4f44-8135-85b767680158').
narrative_ontology:cs_reading_relation('5a83760e-db9b-4f44-8135-85b767680158', study_as_performance__sacrifice_obligation_continuity_performance_only, coexists_with).
narrative_ontology:cs_reading_relation('5a83760e-db9b-4f44-8135-85b767680158', study_as_performance__sacrifice_obligation_continuity_messianic_suspension, influences).
narrative_ontology:cs_reading_relation('5a83760e-db9b-4f44-8135-85b767680158', study_as_performance__sacrifice_obligation_continuity_archival_preservation, coexists_with).
narrative_ontology:cs_axiom('5a83760e-db9b-4f44-8135-85b767680158', foundational, study_constitutes_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('5a83760e-db9b-4f44-8135-85b767680158', study_constitutes_fulfillment, theological).
narrative_ontology:cs_axiom('5a83760e-db9b-4f44-8135-85b767680158', foundational, obligation_substantively_continuous).
narrative_ontology:cs_axiom_status(obligation_substantively_continuous, holdable).
narrative_ontology:cs_axiom_grounding('5a83760e-db9b-4f44-8135-85b767680158', obligation_substantively_continuous, deontological).
narrative_ontology:cs_reference_frame('5a83760e-db9b-4f44-8135-85b767680158', sacrifice_obligation_post_temple_destruction).
narrative_ontology:cs_drift_state('5a83760e-db9b-4f44-8135-85b767680158', contemporary_diaspora_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5a83760e-db9b-4f44-8135-85b767680158', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_performance, studying_community).
narrative_ontology:constraint_beneficiary(study_as_performance, textual_tradition).
narrative_ontology:constraint_beneficiary(study_as_performance, ritual_continuity_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT IN THE STUDY HOUSE (ROPE) — The individual learner experiences study as genuine coordination: learning the law with peers solves the collective problem of preserving knowledge and fulfilling the obligation through textual engagement. The study practice itself IS the performance. No extraction perceived because identity is constituted through the role of scholar. Low extraction (0.08) reflects accessibility of study as a barrier-free fulfillment mechanism compared to animal sacrifice.
constraint_indexing:constraint_classification(study_as_performance, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: STUDY COMMUNITY (ROPE) — Organized as a transmission institution, the yeshiva experiences study as pure coordination of a fundamental collective obligation. The constraint solves the problem: 'How does the obligation persist without a Temple and without animal sacrifice?' The answer is structural — study IS the substitution mechanism. Constraining exits (tradition requires institutional transmission) but low extraction because participation benefits the community directly.
constraint_indexing:constraint_classification(study_as_performance, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINIC TEXTUAL TRADITION (ROPE) — The interpreting authority (the canon of Talmudic reasoning, the lineage of rabbinic jurisprudence) experiences study as coordination that sustains its own authority. Study keeps the tradition alive; the tradition structures study. This is mutual benefit without asymmetric extraction. Mobile exit options for the authority itself (texts can be abandoned, traditions can shift) but institutional incentive to maintain the framework. Low extraction.
constraint_indexing:constraint_classification(study_as_performance, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a structural analysis, the study-as-performance reading instantiates a pure coordination mechanism: a scattered diaspora community without a central temple coordinates on a shared obligation (study) that substitutes for a centralized ritual (sacrifice). The constraint is low-extraction, high-accessibility coordination. All parties benefit from the clarity of the obligation's continued applicability.
constraint_indexing:constraint_classification(study_as_performance, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_performance_tests).
:- end_tests(study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Very low. This reading locates the obligation's fulfillment in textual study, which is highly accessible — no special materials required, no priestly hierarchy, no centralized infrastructure. Barriers to participation are minimal (literacy, time, community context). The distribution of benefits from study is nearly universal within the studying community: all who participate fulfill the obligation equally. Measurement trajectory shows slight upward drift (0.12 → 0.15) reflecting modest institutionalization of study (yeshiva structures, canonical texts) that creates minor access barriers over time, but the trend remains shallow. Suppression (0.08): Very low. Study is volitional, non-coercive, and constitutive of identity rather than imposed externally. No enforcement machinery is required because participation is intrinsically motivated (identity fusion). Suppression remains flat across the interval — the constraint does not require active coercion to maintain. Theater ratio (0.35): Moderate-low. Study has genuine functional content — learning the law requires actual textual engagement, interpretive work, and knowledge transmission. The performative component is modest: study does include ritual performativity (formal study in pairs, communal recitation), but the core function is substantive. The slight increase over the interval (0.30 → 0.35) reflects gradual shift toward more formalized study patterns and institutional liturgicization of learning, but study remains primarily functional rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The study-as-performance reading eliminates perspectival gaps present in competing readings: all agents experience the constraint similarly as coordination. The powerless student, the organized yeshiva, the institutional tradition, and the analytical observer all classify this as Rope because the obstacle (obligation continuity without Temple) is genuinely solved by the coordination mechanism (study). Unlike the performance-only reading (which would be Snare from the powerless perspective — obligation cannot be fulfilled without Temple access), or the messianic-suspension reading (which would be Scaffold — obligation deferred), the study-as-performance reading produces uniform low-extraction classification across all perspectives. The absence of a perspectival gap is diagnostic: it indicates the reading has successfully identified a coordination mechanism with no hidden extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the beneficiary/victim structure. All listed agents are beneficiaries: studying community (low d from identity fusion + accessibility), yeshiva institution (low-moderate d from mutual benefit + constrained exit), rabbinic tradition (low d from institutional coordination), analytical observer (neutral d from analytical position). No victims exist because the obligation is genuinely satisfied through study participation. The absence of victims is the key structural difference from competing readings: in the performance-only reading, the community would be a victim (obligation unmet); in the archival-preservation reading, future generations might be victims (obligation deferred); in the messianic-suspension reading, the obligation itself might be a victim (deferred fulfillment). The study-as-performance reading defines a constraint with positive or neutral directionality throughout, yielding consistently low effective extraction (χ) because f(d) ≈ 0 for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The study-as-performance reading resolves potential mandatrophy by defining the obligation's mandate as continuous rather than obsolete. Under the performance-only reading, mandatrophy would be critical: the mandate requires Temple and animal sacrifice, but these are no longer available, so the obligation persists without object (zombie constraint). This reading eliminates that mandatrophy by redefining what satisfies the mandate: textual engagement becomes the mandate's legitimate object. The authority structure (rabbinic tradition) that produced this reading explicitly solved the mandatrophy problem by making study the functional substitute. No further mandatrophy resolution is needed for this constraint — the reading has already incorporated the solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the sacrifice obligation continuity kernel best matches the actual authority structure and practice norms of diaspora rabbinic Judaism?',
    'Historical-textual analysis: examination of Talmudic passages on the relationship between study and sacrifice (e.g., Avot 1:2, Menachot 110a); comparison of study''s institutional prioritization vs other substitute mechanisms; authority statements from authoritative rabbinic sources; ethnographic documentation of contemporary practice.',
    'Different sibling readings produce different extraction profiles and different victim/beneficiary sets. Study-as-performance yields low extractiveness and no victims. Performance-only reading yields higher extraction (sacrifice obligation unmet without physical performance). Messianic-suspension reading yields deferred obligation (affects classification at generational timescale). Archival-preservation reading reframes obligation as memory work rather than legal duty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which sibling reading matches the historical authority structure').

omega_variable(
    study_accessibility_vs_performance_sufficiency,
    'Does study of sacrifice law constitute genuine fulfillment of the obligation, or merely commemoration/remembrance that substitutes for genuine fulfillment only under duress (absence of temple)?',
    'Textual warrant analysis: close reading of Talmudic justifications for study as substitute (does the text treat this as permanent solution or as emergency interim measure?); theological comparison: does study carry the same metaphysical weight as animal sacrifice in the framework''s own metaphysics? Can the obligation genuinely be SATISFIED by study, or only ACKNOWLEDGED?',
    'If study constitutes genuine fulfillment: rope classification confirmed, low extractiveness, no victim set (obligation is satisfied). If study is only acknowledgment: classification may shift toward piton (performative acknowledgment of an unsatisfied obligation) or tangled_rope (obligation is acknowledged but not met, creating tension). Extractiveness would rise because there is a structural gap between the obligation and its discharge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_accessibility_vs_performance_sufficiency, conceptual, 'Whether study constitutes fulfillment or only commemorates').

omega_variable(
    reading_interdependence_with_performance,
    'Does the study-as-performance reading logically foreclose the performance-only reading, or do they coexist as parallel traditions held by different communities?',
    'Historical documentation of communal divisions: did performance-focused communities (Karaites, Samaritan communities, groups with access to substitute Temple models) explicitly reject the study-as-fulfillment reading? Did study-focused communities (Rabbinic Diaspora) explicitly reject performance-only positions? Or did both readings coexist as legitimate alternatives within the interpretive tradition?',
    'If forecloses: the study reading rules out performance-only as incoherent within a single Jewish framework — choosing study means rejecting performance as the obligation''s proper object. If coexists_with: both readings remain live, suggesting the kernel allows multiple resolutions. If influences: study-as-performance creates structural pressure on performance-only (reduces its plausibility for diaspora communities without temple access) without logically eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_interdependence_with_performance, empirical, 'Logical relationship between study and performance readings of the kernel').

omega_variable(
    substitution_mechanism_reversibility,
    'If a functioning Temple were reconstructed tomorrow, would the obligation revert to animal sacrifice, or has study become the permanent legitimate fulfillment?',
    'Analysis of source material on the logical structure of substitution: is the substitution presented as temporary (pending Temple restoration) or permanent (substitution is the new object of the obligation)? Comparison with other halakhic substitution mechanisms (e.g., prayer as substitute for Temple service) to establish whether substitutions are reversible.',
    'If reversible: study is a temporary filling mechanism (increases extractiveness relative to the ''true'' obligation). If permanent: study-as-performance reading is confirmed as establishing study as the new binding obligation (low extractiveness). If contested: the answer reveals the precise point of disagreement between this reading and the performance-only sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_mechanism_reversibility, empirical, 'Whether substitution of study for sacrifice is temporary or permanent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_performance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sap_tr_t0, study_as_performance, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sap_tr_t3, study_as_performance, theater_ratio, 3, 0.32).
narrative_ontology:measurement(sap_tr_t6, study_as_performance, theater_ratio, 6, 0.35).
narrative_ontology:measurement(sap_tr_t10, study_as_performance, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(sap_be_t0, study_as_performance, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sap_be_t3, study_as_performance, base_extractiveness, 3, 0.14).
narrative_ontology:measurement(sap_be_t6, study_as_performance, base_extractiveness, 6, 0.15).
narrative_ontology:measurement(sap_be_t10, study_as_performance, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sap_su_t0, study_as_performance, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(sap_su_t10, study_as_performance, suppression_requirement, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(study_as_performance, sacrifice_obligation_continuity_performance_only).
narrative_ontology:affects_constraint(study_as_performance, sacrifice_obligation_continuity_messianic_suspension).
narrative_ontology:affects_constraint(study_as_performance, sacrifice_obligation_continuity_archival_preservation).

% DUAL FORMULATION NOTE:
% The sacrifice obligation continuity constraint family decomposes into four structurally distinct constraints corresponding to four readings of the contested kernel. Each reading defines a different ε value, different beneficiary/victim structure, and different classification type. All four are network-linked as siblings (affects_constraints includes all three sibling readings). The study-as-performance reading is distinguished by low extractiveness (study is accessible) and absence of victims (obligation is satisfied). Siblings have higher extractiveness or victim sets reflecting the unmet or deferred obligation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
