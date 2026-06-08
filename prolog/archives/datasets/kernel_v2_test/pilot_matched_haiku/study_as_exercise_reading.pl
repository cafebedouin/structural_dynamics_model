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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Study of Sacrifice Law as Legitimate Exercise of the Mitzvah
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The study-as-exercise reading of the sacrifice obligation kernel
 *   represents one coherent interpretation of how the mitzvah persists after
 *   the Temple's destruction. Under this reading, intellectual engagement
 *   with sacrifice law constitutes genuine fulfillment of the obligation —
 *   the obligation is not suspended or abandoned, but transformed into a form
 *   that can be practiced in the diaspora. This reading coordinates the
 *   observant community around a shared understanding of obligation
 *   fulfillment and preserves the sacrificial obligation's force across
 *   generations. The constraint exhibits low extractiveness (0.15) because
 *   the reading solves a genuine coordination problem: how to maintain the
 *   obligation's meaning without Temple, priesthood, or physical sacrifice.
 *   Rabbinic authority benefits from this reading by maintaining interpretive
 *   monopoly over what counts as legitimate fulfillment, but the benefit is
 *   coupled to the community's benefit — the authority's legitimacy depends
 *   on the community's acceptance of the interpretive framework. The theater
 *   ratio (0.25) is low because the reading is grounded in sustained textual
 *   engagement and interpretive practice, not performative ritual. However,
 *   the reading is one option among several sibling readings
 *   (performance_only, messianic_suspension, symbolic_archive), and the
 *   analytical observer risks naturalizing this contingent institutional
 *   arrangement as hermeneutic necessity.
 *
 * KEY AGENTS:
 *   - Observant Jews (Powerless/Constrained): Participants in study of sacrifice law; experience the constraint as legitimate fulfillment pathway; benefit from meaningful engagement with the obligation
 *   - Rabbinic Interpretive Authority (Institutional/Arbitrage): Maintains monopoly on defining legitimate fulfillment; benefits from interpretive legitimacy; coordinates community around shared understanding
 *   - Observant Community (Organized/Constrained): Yeshiva networks, study circles, halakhic councils; organized agents who participate in defining legitimate study; benefit from community cohesion around shared practice
 *   - Analytical Observer (Analytical/Analytical): Civilizational perspective; risks naturalizing contingent institutional arrangement as hermeneutic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_exercise_reading, 0.15).
domain_priors:suppression_score(study_as_exercise_reading, 0.2).
domain_priors:theater_ratio(study_as_exercise_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_exercise_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(study_as_exercise_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(study_as_exercise_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_exercise_reading, rope).
narrative_ontology:human_readable(study_as_exercise_reading, "Study of Sacrifice Law as Legitimate Exercise of the Mitzvah").
narrative_ontology:topic_domain(study_as_exercise_reading, "religious_law/halakhic_authority/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(study_as_exercise_reading, '3c25b5f7-affe-4aba-b899-93e33f713918').
narrative_ontology:cs_kernel_codification('3c25b5f7-affe-4aba-b899-93e33f713918', fixed_text).
narrative_ontology:cs_authority_grounding('3c25b5f7-affe-4aba-b899-93e33f713918', lineage).
narrative_ontology:cs_interpretation_layer_present('3c25b5f7-affe-4aba-b899-93e33f713918').
narrative_ontology:cs_reading_relation('3c25b5f7-affe-4aba-b899-93e33f713918', study_as_exercise_reading__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c25b5f7-affe-4aba-b899-93e33f713918', study_as_exercise_reading__messianic_suspension_reading, forecloses).
narrative_ontology:cs_reading_relation('3c25b5f7-affe-4aba-b899-93e33f713918', study_as_exercise_reading__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('3c25b5f7-affe-4aba-b899-93e33f713918', foundational, intellectual_engagement_fulfills_obligation).
narrative_ontology:cs_axiom_status(intellectual_engagement_fulfills_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3c25b5f7-affe-4aba-b899-93e33f713918', intellectual_engagement_fulfills_obligation, deontological).
narrative_ontology:cs_axiom('3c25b5f7-affe-4aba-b899-93e33f713918', foundational, suspension_is_authorized_transformation).
narrative_ontology:cs_axiom_status(suspension_is_authorized_transformation, holdable).
narrative_ontology:cs_axiom_grounding('3c25b5f7-affe-4aba-b899-93e33f713918', suspension_is_authorized_transformation, conventional).
narrative_ontology:cs_reference_frame('3c25b5f7-affe-4aba-b899-93e33f713918', continuous_obligation_fulfillment_through_study).
narrative_ontology:cs_drift_state('3c25b5f7-affe-4aba-b899-93e33f713918', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3c25b5f7-affe-4aba-b899-93e33f713918', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_exercise_reading, rabbinic_interpretive_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(study_as_exercise_reading, observant_jews).
narrative_ontology:constraint_beneficiary(study_as_exercise_reading, observant_community_organized).
narrative_ontology:constraint_vindicates(study_as_exercise_reading, intellectual_engagement_as_valid_mitzvah_fulfillment).
narrative_ontology:constraint_vindicates(study_as_exercise_reading, suspension_of_sacrificial_practice_as_authorized_transformation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participants in study of sacrifice law; experience the constraint as legitimate fulfillment pathway for the sacrificial obligation. They engage with the texts, interpretations, and halakhic discussions as genuine religious practice. The study provides meaningful engagement with the obligation without requiring Temple, priesthood, or physical sacrifice. Exit would require abandoning the interpretive framework and the identity constituted through it.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, observant_jews, beneficiary,
    powerless, biographical, constrained, global).

% Maintains interpretive monopoly on what counts as legitimate fulfillment of the sacrificial obligation. Sets the agenda for how the obligation is understood and practiced. Benefits from the study-as-exercise reading because it preserves rabbinic authority as the adjudicator of halakhic meaning. The authority's legitimacy depends on the community's acceptance of the interpretive framework. Can exit by adopting alternative readings, but doing so would diminish interpretive authority.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(study_as_exercise_reading, rabbinic_interpretive_authority, beneficiary).

% Yeshiva networks, study circles, halakhic councils, and organized agents who participate in defining and practicing legitimate study of sacrifice law. Experience the constraint as a coordination mechanism that preserves the sacrificial obligation's force across generations. Benefit from community cohesion around shared interpretive practice. Exit is constrained by institutional identity and social bonds, but the community participates in defining what counts as legitimate study.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, observant_community_organized, beneficiary,
    organized, generational, constrained, global).

% The corpus of foundational texts (Torah, Talmud, Maimonides, later authorities) that grounds the study-as-exercise reading. Not an agent but a non-agent entity kept for narrative completeness. The reading's legitimacy depends on its grounding in the textual tradition's own interpretive logic. Excluded from beneficiary/victim derivation and directionality computation.
narrative_ontology:constraint_stakeholder(study_as_exercise_reading, textual_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(study_as_exercise_reading, textual_tradition).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: How to preserve the force and meaning of the sacrificial obligation in the absence of the Temple, priesthood, and physical sacrifice. The study-as-exercise reading solves this by treating intellectual engagement with sacrifice law as legitimate fulfillment.
% TRANSFER_FUNCTION: The constraint transfers interpretive authority from Temple priests (who performed sacrifice) to rabbinic scholars (who interpret the obligation). It also transfers the obligation's fulfillment from physical action (sacrifice) to intellectual action (study). The transfer moves the locus of obligation from the Temple to the study hall, from the priest to the scholar, from the body to the mind.
% ABSENT_VOICES: Alternative readings that would object to the study-as-exercise framing: those who hold that study is performative substitute (performance_only_reading), those who believe the obligation is suspended until Temple reconstruction (messianic_suspension_reading), those who see study as symbolic preservation rather than fulfillment (symbolic_archive_reading). These readings are not absent from the tradition but are marginalized by the dominance of the study-as-exercise reading in mainstream rabbinic discourse.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise reading disappeared, the world would rearrange itself: the observant community would need to adopt an alternative reading (performance_only, messianic_suspension, or symbolic_archive) or abandon the sacrificial obligation entirely. The constraint's disappearance would force a reckoning with the obligation's status in the diaspora. However, some would argue that the constraint is so deeply embedded in rabbinic practice that its disappearance would be unthinkable — the world would remain unchanged because the reading has become naturalized as the only coherent interpretation.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the material conditions for sacrificial practice. The sacrificial obligation, grounded in Torah, persisted as a binding commandment, but the Temple, priesthood, and sacrificial apparatus no longer existed. The founding problem was: how to maintain the obligation's force and meaning in the diaspora without Temple, priesthood, or physical sacrifice?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Talmud itself (Menachot 110a, Zevachim 2a), which discusses how the obligation persists after the Temple's destruction. Maimonides (Mishneh Torah, Hilkhot Korbanot 3:1) explicitly addresses the problem and endorses the study-as-exercise reading. Contemporary halakhic authorities (Rav Soloveitchik, Rav Lichtenstein) corroborate that the founding problem remains live — the obligation persists and must be fulfilled through study in the absence of Temple. The problem is not solved but managed through the study-as-exercise reading.
narrative_ontology:disappearance_verdict(study_as_exercise_reading, contested).
narrative_ontology:founding_problem_status(study_as_exercise_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVANT JEW ENGAGED IN STUDY (ROPE) — Experiences study of sacrifice law as genuine fulfillment of the mitzvah obligation. The constraint solves a real coordination problem: how to maintain the sacrificial obligation's force and meaning in the absence of the Temple. Study provides a legitimate pathway that requires no physical sacrifice, no Temple, no priestly apparatus. The participant benefits from this coordination — they can fulfill the obligation through intellectual engagement. Suppression is low because the participant accepts the interpretive framework; exit is constrained but not trapped (one could reject the framework, but at identity cost).
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: RABBINIC AUTHORITY (ROPE) — Maintains interpretive monopoly on what counts as legitimate fulfillment of the mitzvah. Benefits from the study-as-exercise reading because it preserves rabbinic authority as the adjudicator of halakhic meaning. The constraint coordinates the community around a shared understanding of obligation fulfillment. Extraction is minimal because the authority's benefit (interpretive legitimacy) is coupled to the community's benefit (meaningful fulfillment pathway). This is genuine coordination, not extraction — the authority's power derives from the community's acceptance of the interpretive framework.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: OBSERVANT COMMUNITY (ROPE) — Organized agents (yeshiva networks, study circles, halakhic councils) experience the study-as-exercise reading as a coordination mechanism that preserves the sacrificial obligation's force across generations without requiring Temple reconstruction. The constraint enables community cohesion around shared interpretive practice. Extraction is low because the community participates in defining what counts as legitimate study. Exit is constrained by identity and social bonds, but not trapped — the community could adopt alternative readings.
constraint_indexing:constraint_classification(study_as_exercise_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the study-as-exercise reading appears as a hermeneutic necessity: any living tradition must find ways to preserve the force of its foundational obligations when material conditions change. The reading emerges naturally from the interpretive logic of the tradition itself — it is not imposed but discovered through sustained engagement with the textual corpus. However, this mountain classification is a false summit candidate: the reading's persistence depends on rabbinic authority's continued acceptance and the community's continued participation. The 'hermeneutic necessity' framing naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(study_as_exercise_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_exercise_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(study_as_exercise_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(study_as_exercise_reading, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The study-as-exercise reading solves a genuine coordination problem — how to preserve the sacrificial obligation's force in the absence of Temple. The reading benefits both the community (meaningful fulfillment pathway) and rabbinic authority (interpretive legitimacy). The coupling of benefits suggests genuine coordination rather than extraction. The low value reflects that no identifiable victim set exists — the suspension of sacrificial practice is treated as an authorized transformation, not an extraction. Suppression (0.20): Low. The reading is grounded in the tradition's own interpretive logic and accepted by the observant community. Suppression is minimal because participants accept the framework; exit is constrained by identity and social bonds, but not trapped. Theater ratio (0.25): Low. The reading is grounded in sustained textual engagement and interpretive practice. The theater is minimal because the study is genuine intellectual work, not performative ritual. The slight increase over the interval (0.15 → 0.25) reflects modest drift toward more formalized study structures and institutionalized yeshiva practice, but the core remains substantive engagement with the texts.
 *
 * PERSPECTIVAL GAP:
 *   The study-as-exercise reading produces consistent rope classifications across the powerless, institutional, and organized perspectives — all experience the constraint as genuine coordination. The analytical observer's mountain classification represents a perspectival gap: the civilizational view risks naturalizing the reading as hermeneutic necessity, when it is actually one option among several sibling readings. The gap reveals the false summit risk: the reading's persistence depends on rabbinic authority's continued acceptance and the community's continued participation, not on logical inevitability. The engine's false summit detector should identify this gap and reclassify the mountain perspective to tangled_rope or snare, revealing the contingent institutional arrangement beneath the naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural position relative to the constraint. The observant Jew (powerless/constrained) experiences low d because they are a beneficiary of the coordination mechanism — the reading provides a legitimate fulfillment pathway. Rabbinic authority (institutional/arbitrage) experiences low d because they are a beneficiary of the interpretive monopoly, but the benefit is coupled to the community's benefit. The organized community (organized/constrained) experiences low d because they participate in defining legitimate study and benefit from community cohesion. The analytical observer (analytical/analytical) experiences the constraint as mountain — no directionality applies because the observer sees the constraint as a hermeneutic necessity, not an institutional arrangement. However, the false summit detector should reveal that the mountain classification naturalizes a contingent institutional arrangement, and the observer's position should be reclassified to show the extractive dimension of rabbinic authority's interpretive monopoly.
 *
 * MANDATROPHY ANALYSIS:
 *   The study-as-exercise reading resolves the mandatrophy by transforming the sacrificial obligation into a form that can be practiced in the diaspora. The mandate (fulfill the sacrificial obligation) is not abandoned but reinterpreted — the obligation is occupied through intellectual engagement with sacrifice law. This is an authorized transformation, not an extraction. The reading's legitimacy depends on its grounding in the tradition's own interpretive logic and the community's acceptance of the framework. The mandatrophy is resolved when the reading is recognized as a coherent interpretation of the tradition, not a performative substitute for Temple sacrifice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is study of sacrifice law a legitimate exercise of the mitzvah obligation, or is it a performative substitute that naturalizes the loss of Temple practice?',
    'Textual analysis of foundational sources (Talmud, Maimonides, later authorities) to determine whether the study-as-exercise reading is grounded in the tradition''s own logic or imposed by post-Temple institutional needs. Cross-reading comparison: does the performance_only_reading or symbolic_archive_reading better account for the textual evidence?',
    'If study-as-exercise is textually grounded: rope classification confirmed, low extractiveness justified. If imposed by institutional need: reclassifies toward tangled_rope or snare, extractiveness rises, beneficiary (rabbinic authority) becomes more visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether study-as-exercise is textually grounded or institutionally imposed').

omega_variable(
    suspension_authorization_ambiguity,
    'Does the suspension of sacrificial practice constitute an authorized transformation of the obligation, or a de facto abandonment that the study-as-exercise reading conceals?',
    'Examination of halakhic sources on suspension (pikuach nefesh, force majeure, temporary vs permanent suspension). Determination of whether the Temple''s destruction is treated as a temporary condition (suspension) or permanent transformation (obligation metamorphosis). Analysis of whether alternative readings (messianic_suspension_reading) better capture the tradition''s own uncertainty about the suspension''s status.',
    'If suspension is authorized: study-as-exercise is legitimate coordination. If suspension is de facto abandonment: study becomes performative theater masking the obligation''s loss, extractiveness rises, theater_ratio rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_authorization_ambiguity, conceptual, 'Whether suspension of sacrifice is authorized transformation or de facto abandonment').

omega_variable(
    interpretive_monopoly_extraction,
    'Does rabbinic authority''s monopoly on defining what counts as legitimate study constitute extractive gatekeeping, or genuine coordination of shared meaning?',
    'Historical analysis of alternative interpretations that were suppressed or marginalized. Examination of whether non-rabbinic forms of engagement with sacrifice law (mystical, philosophical, lay study) are recognized as legitimate fulfillment or excluded. Measurement of interpretive pluralism: do multiple readings coexist, or does one reading dominate?',
    'If monopoly is extractive: reclassifies toward tangled_rope, extractiveness rises, suppression rises. If coordination is genuine: rope classification confirmed, low extractiveness justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_extraction, empirical, 'Whether rabbinic interpretive monopoly is extractive gatekeeping or genuine coordination').

omega_variable(
    false_summit_naturalization,
    'Does the analytical observer''s mountain classification naturalize a contingent institutional arrangement as hermeneutic necessity?',
    'Comparison with sibling readings: if performance_only_reading or symbolic_archive_reading provide equally coherent accounts of the tradition''s logic, then the study-as-exercise reading is one option among several, not a hermeneutic necessity. The mountain classification would be a false summit — the ''necessity'' is institutional, not logical.',
    'If false summit confirmed: engine reclassifies to tangled_rope or snare via FSM signature. Reveals that the reading''s persistence depends on rabbinic authority''s continued acceptance, not on logical inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether hermeneutic necessity is genuine or naturalized institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_exercise_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(study_ex_tr_t0, study_as_exercise_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(study_ex_tr_t500, study_as_exercise_reading, theater_ratio, 500, 0.22).
narrative_ontology:measurement(study_ex_tr_t1000, study_as_exercise_reading, theater_ratio, 1000, 0.25).

% Extraction over time
narrative_ontology:measurement(study_ex_be_t0, study_as_exercise_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(study_ex_be_t500, study_as_exercise_reading, base_extractiveness, 500, 0.13).
narrative_ontology:measurement(study_ex_be_t1000, study_as_exercise_reading, base_extractiveness, 1000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_exercise_reading, identity_coordination).
narrative_ontology:affects_constraint(study_as_exercise_reading, performance_only_reading).
narrative_ontology:affects_constraint(study_as_exercise_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(study_as_exercise_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel decomposes into four structurally distinct constraint stories, one for each reading. Each reading has its own ε value, beneficiary/victim structure, and classification. The study-as-exercise reading (this story) has low extractiveness (0.15) and rope classification because it solves a genuine coordination problem. The performance_only reading would have higher extractiveness and tangled_rope or snare classification because it treats study as performative substitute. The messianic_suspension reading would have different beneficiary structure (messianic hope rather than rabbinic authority). The symbolic_archive reading would have different extractiveness reflecting the symbolic rather than fulfillment framing. All four stories are linked via network.affects_constraints because they are readings of the same kernel and compete for interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
