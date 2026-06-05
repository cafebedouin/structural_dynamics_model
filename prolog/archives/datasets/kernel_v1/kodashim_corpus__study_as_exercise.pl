% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Mitzvah Performance (Kodashim)
 *   domain: religious_studies/rabbinic_judaism/commitment_systems
 *
 * SUMMARY:
 *   The study of Kodashim (sacrifice laws) in rabbinic Judaism instantiates a
 *   contested kernel about how a mitzvah persists after the material
 *   conditions of its performance become impossible. The destruction of the
 *   Second Temple in 70 CE rendered the central acts of sacrifice (the
 *   korbanot) physically impossible. Three distinct readings emerged and
 *   persist in contemporary Jewish thought: (1) the 'performance_only'
 *   reading holds that Kodashim is an archived blueprint awaiting messianic
 *   restoration; (2) the 'study_as_exercise' reading (the present constraint)
 *   holds that intellectual engagement with sacrifice law IS the performance
 *   of the mitzvah, maintaining cosmic order through continuous hermeneutical
 *   work; (3) the 'substitution_archive' reading holds that prayer and Torah
 *   study have formally replaced sacrifice as the primary mitzvah, relegating
 *   Kodashim to historical memory. This story instantiates the
 *   'study_as_exercise' reading, which claims zero extractiveness and no
 *   victim set — study fulfills the mitzvah completely through engagement
 *   itself. The constraint coordinates scholarly community practice around
 *   shared interpretive tradition without asymmetric extraction. No one is
 *   deprived; beneficiaries are those engaged in study and those who benefit
 *   from cosmic order maintenance.
 *
 * KEY AGENTS:
 *   - Talmudic Scholars: Primary beneficiary (organized/constrained/generational) — engage in continuous hermeneutical work that (under this reading) constitutes mitzvah performance and cosmic maintenance
 *   - Jewish Interpretive Community: Primary beneficiary (organized/constrained/generational) — sustained by coordinated scholarly practice around shared Kodashim texts
 *   - Cosmic Order: Abstract beneficiary (analytical/analytical/civilizational) — maintained through scholarly engagement with sacrifice law (theological claim)
 *   - Tradition Authority: Institutional actor (institutional/arbitrage/immediate) — legitimates the claim that study is performance through textual interpretation and communal endorsement
 *   - Secular Higher Education: Competing institutional actor (institutional/arbitrage/generational) — attracts scholars away from yeshiva-based Kodashim engagement; creates exit option for students who might otherwise be constrained by tradition
 *   - Messianic Expectation (Performance-Only Sibling): Counter-reading (abstract/analytical/civilizational) — the alternate frame that sees Kodashim as latent rather than active
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.08).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Study of Sacrifice Law as Mitzvah Performance (Kodashim)").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '76f56362-2377-4d68-939c-4b67b6219fe2').
narrative_ontology:cs_kernel_codification('76f56362-2377-4d68-939c-4b67b6219fe2', fixed_text).
narrative_ontology:cs_authority_grounding('76f56362-2377-4d68-939c-4b67b6219fe2', lineage).
narrative_ontology:cs_interpretation_layer_present('76f56362-2377-4d68-939c-4b67b6219fe2').
narrative_ontology:cs_reading_relation('76f56362-2377-4d68-939c-4b67b6219fe2', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('76f56362-2377-4d68-939c-4b67b6219fe2', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('76f56362-2377-4d68-939c-4b67b6219fe2', foundational, cosmic_participation_through_study).
narrative_ontology:cs_axiom_status(cosmic_participation_through_study, holdable).
narrative_ontology:cs_axiom_grounding('76f56362-2377-4d68-939c-4b67b6219fe2', cosmic_participation_through_study, theological).
narrative_ontology:cs_axiom('76f56362-2377-4d68-939c-4b67b6219fe2', foundational, intellectual_engagement_as_mitzvah_fulfillment).
narrative_ontology:cs_axiom_status(intellectual_engagement_as_mitzvah_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('76f56362-2377-4d68-939c-4b67b6219fe2', intellectual_engagement_as_mitzvah_fulfillment, deontological).
narrative_ontology:cs_reference_frame('76f56362-2377-4d68-939c-4b67b6219fe2', active_kernel_engagement).
narrative_ontology:cs_drift_state('76f56362-2377-4d68-939c-4b67b6219fe2', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('76f56362-2377-4d68-939c-4b67b6219fe2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, talmudic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_interpretive_community).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, cosmic_order_maintenance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENGAGED TALMUDIC SCHOLAR (ROPE) — For the scholar committed to this reading, study of Kodashim is genuine coordination around shared interpretive practice. The constraint coordinates intellectual labor toward spiritual-cosmic ends. No extraction experienced — the scholar perceives the mitzvah as fulfilled through engagement itself. The coordination function is primary; no subordinate extraction mechanism.
constraint_indexing:constraint_classification(kodashim_corpus__study_as_exercise, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: YESHIVA STUDENT (ROPE) — Constrained by commitment to the tradition and social embeddedness in the learning community, but perceives study as fulfillment rather than burden. The constraint coordinates their intellectual effort toward cosmic participation. Exit is possible (one can leave the yeshiva) but costly in relational and identity terms. Still experiences the core mechanism as coordination, not extraction.
constraint_indexing:constraint_classification(kodashim_corpus__study_as_exercise, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER — STUDY-AS-EXERCISE READING (ROPE) — From the civilizational frame, this reading constitutes a pure coordination mechanism: Kodashim study coordinates the scholarly community around shared hermeneutical practice and cosmic maintenance. Zero extractiveness. No victims (no one is deprived of anything by the existence of this constraint). The beneficiary set is those engaged in study who maintain cosmic order through intellectual-spiritual work. The theater ratio is minimal (0.15) — the actual practice of study and interpretation is the function, not performance of study.
constraint_indexing:constraint_classification(kodashim_corpus__study_as_exercise, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.05): Minimal. This reading claims that study fully satisfies the mitzvah—no extraction occurs because the scholar is not deprived of anything; the engagement itself is the fulfillment. No one is being kept from performing the 'real' mitzvah elsewhere; the constraint contains the complete mitzvah. The low value reflects that this reading asserts coordination without subordinate extraction. Suppression (0.08): Minimal. Under this reading, scholars have high motivation to engage and perceive the study as fulfilling. Suppression—lack of alternatives or coerced participation—is not a binding mechanism. The scholar could exit (pursue secular learning, abandon the tradition), but exit carries relational and identity costs that flow from identity commitment, not from suppression built into the constraint itself. Theater ratio (0.15): Very low. The constraint's core mechanism is hermeneutical—actual textual interpretation, conceptual synthesis, reasoning through cases. These are functionally genuine, not performative. The small theater ratio accounts only for the framing acts (declaration that study is mitzvah, ritual opening of texts, communal affirmation) that constitute the uptake of the claim within the community. The actual intellectual work is not theater.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives (engaged scholar, yeshiva student, analytical observer) arrive at the same classification (rope) from the same base properties because the reading's axioms eliminate the perspectival gap typical of mixed constraints. The engaged scholar experiences no extraction because study fulfills the mitzvah. The yeshiva student experiences constrained exit (relational/identity costs) but no extraction mechanism that uses suppression to extract value. The analytical observer sees pure coordination. The gap that would exist with the 'performance_only' or 'substitution_archive' readings is absent here: all perspectives perceive the same coordination function without subordinate extraction. This uniformity is diagnostically significant—it marks the reading as internally coherent and aligned across its interpretive community. The absence of perspectival gap suggests either (a) the reading is genuinely a coordination mechanism without hidden extraction, or (b) the reading's axioms are so deeply internalized by all perspectives that extraction, if present, is rendered invisible to all participants.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for this constraint is uniformly low across all perspectives because the reading contains zero extractiveness. Beneficiaries (scholars, community, cosmic order) all experience the constraint as coordination toward shared ends, not as extraction flowing away from them. A beneficiary with arbitrage exit options (institutional power) derives d ≈ 0.15 (low targeting). A moderate scholar with constrained exit derives d ≈ 0.35 (still beneficiary, still low targeting). The analytical observer at the civilizational scope derives d ≈ 0.72 (observer position), but applies this to a rope constraint, not to an extraction mechanism. The derived chi values across all perspectives are proportionally low because ε itself is minimal. The reading's axiom (cosmic_participation_through_study) means that participation IS the benefit, so the beneficiary/victim distinction does not create the asymmetric directionality that would produce high d values. This contrasts sharply with the 'performance_only' sibling reading, which would show high d toward those waiting for restoration (victims of latency) and concentrated benefit flowing to textual archivists.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy (the puzzle of how a mitzvah can be performed after its material conditions become impossible) by claiming that the mitzvah's essence is intellectual-spiritual engagement, not material action. The performance IS the study; the kernel IS occupied through continuous hermeneutical work. This resolves the tension between the sibling readings: 'performance_only' leaves the mitzvah dormant (latent rope, high theater, piton-adjacent); 'substitution_archive' displaces Kodashim from its mitzvah status (archive piton, low function, high theater). The 'study_as_exercise' reading sustains the mitzvah as active and binding—a genuine rope coordination mechanism. The mandatrophy resolution hinges on the axiom that cosmic_participation_through_study is real, not metaphorical. If this axiom is challenged (omega: study_as_actual_performance), the reading may collapse toward piton or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_actual_performance,
    'Is intellectual engagement with sacrifice law genuinely performative (the same ontological category as performing the sacrifice itself) or metaphorically fulfilling (a substitute that coordinates practice without matching the original)?',
    'Textual analysis of Maimonides, Sforno, and contemporary thinkers on whether study achieves literal parity with sacrifice or symbolic/substitute status; phenomenological investigation of how practicing scholars experience the mitzvah claim',
    'If literal parity: the reading sustains rope classification with zero extractiveness. If metaphorical/substitute: the reading may reclassify toward piton (performative study of archive) or scaffold (temporary substitution pending restoration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_actual_performance, conceptual, 'Whether study performs sacrifice or substitutes for it').

omega_variable(
    cosmic_order_maintenance_mechanism,
    'What is the causal mechanism by which study of sacrifice law maintains cosmic order? Is this a literal theological claim (study sustains divine-human relationship and cosmic harmony) or a heuristic framing for hermeneutical discipline?',
    'Examination of theological grounding in Kabbalah, Hasidic thought, and contemporary Jewish philosophy; comparison with similar claims in other religious traditions; empirical assessment of whether believers report observable effects from study practice',
    'If literal causal mechanism: the reading''s axiom (cosmic_participation_through_study) is grounded in theological claim and resistant to empirical drift. If heuristic framing: the axiom is grounded in conventional/institutional transmission and more vulnerable to erosion as secular higher education competes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_order_maintenance_mechanism, conceptual, 'Mechanism of cosmic order maintenance through study').

omega_variable(
    performative_language_boundary,
    'Does the reading''s claim that ''study is the performance of the mitzvah'' constitute an actual ontological equivalence or a performative utterance that constitutes participation within the framework?',
    'Speech-act analysis of how contemporary scholars frame and enact this claim; distinction between ''study performs the mitzvah'' (ontological) vs ''claiming that study performs the mitzvah enacts one''s participation'' (performative); examination of what counts as falsification within each frame',
    'If ontological: the reading asserts parity and is vulnerable to external critique (sacrifice has material causality that study may lack). If performative: the reading is self-validating within the framework and externally critique-resistant, suggesting stronger epistemic closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_language_boundary, conceptual, 'Ontological vs performative status of study-as-performance claim').

omega_variable(
    kernel_occupied_vs_archived,
    'Is the Kodashim corpus an actively occupied kernel (continuously reinterpreted, alive with meaning, generative of new applications) or a preserved archive that reference shows its historicity but may lack living application?',
    'Corpus analysis of Kodashim scholarship over past 50 years: percentage that are pure commentarial (exegesis of existing texts) vs generative (extending logic to new cases, deriving new halakhic conclusions); institutional analysis of whether Kodashim study is integrated into contemporary halakhic decision-making or cordoned off as historical/theoretical',
    'If occupied: the reading sustains its rope classification as a living coordination mechanism. If archived: the reading may shift toward piton (performative maintenance of a kernel the tradition knows is superseded) or toward a new category (historical preservation constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_occupied_vs_archived, empirical, 'Whether Kodashim is actively occupied kernel or preserved archive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.12).
narrative_ontology:measurement(kodashim_study_tr_t5, kodashim_corpus__study_as_exercise, theater_ratio, 5, 0.14).
narrative_ontology:measurement(kodashim_study_tr_t10, kodashim_corpus__study_as_exercise, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(kodashim_study_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(kodashim_study_be_t5, kodashim_corpus__study_as_exercise, base_extractiveness, 5, 0.04).
narrative_ontology:measurement(kodashim_study_be_t10, kodashim_corpus__study_as_exercise, base_extractiveness, 10, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The Kodashim constraint family consists of three separate constraint stories representing three structurally distinct readings of the same kernel (the persistence of sacrifice law after the Temple's destruction). Each reading has different ε, different victim/beneficiary structures, and different type classifications. The 'study_as_exercise' reading (this story) claims zero extractiveness and pure coordination (rope). The 'performance_only' sibling claims moderate extractiveness (waiting for restoration introduces latency cost) and rope with significant theater. The 'substitution_archive' sibling claims low function masked by high theater (piton). The three readings coexist in contemporary Jewish thought, held by different communities and traditions, and each story must be authored separately with its own ε-invariant metrics. Network links enable the engine to recognize that these are siblings in a kernel dispute, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
