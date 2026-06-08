% ============================================================================
% CONSTRAINT STORY: liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liturgical_reading, []).

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
 *   constraint_id: liturgical_reading
 *   human_readable: Hebrew Language Preservation Through Liturgical Ritual Reading
 *   domain: sociolinguistics/language_revitalization/religious_practice
 *
 * SUMMARY:
 *   The liturgical reading constraint instantiates one specific reading of
 *   the contested kernel 'Hebrew remains living language.' This reading
 *   asserts that continuous ritual use of Hebrew in prayer, Torah study, and
 *   religious practice constitutes the mechanism through which the language
 *   persists as a living rather than dead or liturgically-ossified system.
 *   The constraint operates primarily in diaspora religious communities where
 *   Hebrew is not a vernacular language of daily commerce but is actively
 *   maintained through structured religious practice. The reading positions
 *   rabbinic authority as the custodian and interpreter of the liturgical
 *   texts that carry the language forward, and characterizes the constraint
 *   as primarily coordinative — solving the collective-action problem of
 *   language transmission across generations in the absence of native-speaker
 *   environments. The structural data shows this reading as genuinely
 *   low-extraction (theater_ratio 0.35, extractiveness 0.22) because
 *   participation in liturgical reading is largely voluntary, benefits accrue
 *   to practitioners themselves (language competence, cultural continuity),
 *   and no victim population is systematically disadvantaged by the
 *   mechanism. However, a competing reading (native_daily_reading) asserts
 *   that Hebrew becomes living only through native speaker acquisition and
 *   daily vernacular use, not through ritual recitation that may preserve
 *   textual form while losing authentic speech patterns. The sibling
 *   continuity_narrative_reading grounds Hebrew's persistence in explicit
 *   institutional and textual transmission narratives rather than in the
 *   functional mechanism of ritual practice. This reading differs from both
 *   in what it takes as constitutive evidence for linguistic life.
 *
 * KEY AGENTS:
 *   - Practitioner Community (moderate/constrained): Participants in daily prayer, Shabbat services, Torah study; experience the constraint as enabling coordination without systemic extraction; voluntary participation with cultural/social costs to exit
 *   - Rabbinic Authority (organized/arbitrage): Institutional class maintaining interpretive control over liturgical texts; benefits from defined role (prestige, textual authority) but frames this as coordination function; high-exit options but choose to remain invested
 *   - Language Preservation Network (powerful/mobile): Linguists, Hebraists, language planners who actively choose to support liturgical reading as a coordination mechanism; external alternatives available; participation marks genuine belief in mechanism effectiveness
 *   - Secular Jewish Identity Seeker (powerless/identity_locked): Diaspora agent seeking Hebrew competence and Jewish cultural continuity without religious commitment; faces pressure to participate in religious frameworks; identity fused with both Hebrew and Jewish identity but not necessarily religious practice; constrained by diaspora resource scarcity for non-liturgical Hebrew instruction
 *   - Analytical Observer (analytical/analytical): Civilizational view risking naturalization of institutional choice as linguistic law; sees ritual practice as immutable requirement for language continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_reading, 0.22).
domain_priors:suppression_score(liturgical_reading, 0.18).
domain_priors:theater_ratio(liturgical_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(liturgical_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(liturgical_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_reading, rope).
narrative_ontology:human_readable(liturgical_reading, "Hebrew Language Preservation Through Liturgical Ritual Reading").
narrative_ontology:topic_domain(liturgical_reading, "sociolinguistics/language_revitalization/religious_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liturgical_reading, '2c5a19df-9e5f-4456-9e85-4c855d0ba8a6').
narrative_ontology:cs_kernel_codification('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', fixed_text).
narrative_ontology:cs_authority_grounding('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', lineage).
narrative_ontology:cs_interpretation_layer_present('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6').
narrative_ontology:cs_reading_relation('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', liturgical_reading__hebrew_native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', liturgical_reading__hebrew_continuity_narrative_reading, influences).
narrative_ontology:cs_axiom('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', foundational, ritual_practice_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(ritual_practice_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', ritual_practice_constitutes_linguistic_life, empirically_contingent).
narrative_ontology:cs_axiom('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', foundational, rabbinic_textual_authority_necessary_for_transmission).
narrative_ontology:cs_axiom_status(rabbinic_textual_authority_necessary_for_transmission, holdable).
narrative_ontology:cs_axiom_grounding('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', rabbinic_textual_authority_necessary_for_transmission, conventional).
narrative_ontology:cs_reference_frame('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', hebrew_continuous_through_rabbinic_tradition).
narrative_ontology:cs_drift_state('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', contemporary_diaspora_pluralism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2c5a19df-9e5f-4456-9e85-4c855d0ba8a6', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(liturgical_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(liturgical_reading, jewish_religious_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTITIONER COMMUNITY (ROPE) — Participants in daily prayer and study (morning minyan, Shabbat services, Torah study groups) experience liturgical reading as genuine coordination: the shared practice solves the collective problem of maintaining linguistic competence across generations without classroom instruction. The constraint enables rather than extracts — participation is voluntary and most practitioners see it as intrinsically meaningful. Constrained exit reflects social and cultural costs of abandonment, not coercion.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: RABBINIC AUTHORITY (ROPE) — The interpretive class that maintains and transmits liturgical practices sees the constraint as coordination of knowledge transmission. The system benefits them (they are authorities; interpretive control is institutionally protected) but the benefit is structured as legitimate function, not hidden extraction. Exit options are high (they could teach Hebrew through secular methods) but arbitrage opportunities (prestige, institutional continuity, textual authority) keep them invested. Experiences the constraint as pure coordination.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LANGUAGE PRESERVATION NETWORK (ROPE) — Linguists, language planners, and Hebraist scholars who recognize liturgical reading as a major vector for language persistence. They have external alternatives (sociolinguistic methods, computational corpus linguistics) but actively choose liturgical participation as a coordination mechanism superior to classroom-only approaches. Mobile exit reflects professional autonomy; choice to participate marks the constraint as coordination rather than extraction.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: SECULAR JEWISH IDENTITY SEEKER (TANGLED ROPE) — An agent seeking Jewish cultural continuity without religious commitment faces structural pressure: liturgical reading is the primary mechanism for Hebrew acquisition in diaspora contexts, but accessing it requires participation in religious frameworks that may contradict secular identity commitments. The constraint carries both genuine coordination (learning Hebrew) and embedded extraction (mandatory religious framing). Identity-locked exit reflects the binding: Hebrew competence is fused with Jewish identity, but the path to that competence runs through religious practice. Moderate extraction because some benefit accrues (language learning) despite the identity conflict.
constraint_indexing:constraint_classification(liturgical_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / LINGUISTIC IMMUTABILITY (MOUNTAIN) — From a civilizational/universal view, the constraint appears to be immutable linguistic law: living languages require active use; discontinuity in use leads to attrition or death; ritual practice is simply a mechanism through which active use occurs naturally. Extraction and suppression negligible — this is how languages work. However, this perspective risks naturalizing a historically contingent institutional arrangement (the choice to preserve Hebrew through rabbinically-mediated liturgy rather than secular daily speech or classroom acquisition). The false-summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(liturgical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liturgical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liturgical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. The constraint coordinates genuine value — language transmission across generations — without systematic asymmetric extraction. Rabbinic authority benefits from their role, but this is framed as coordination (knowledge transmission) rather than hidden extraction. Practitioners gain language competence, cultural continuity, and often intrinsic meaning from participation. No clear victim population bears systematic costs. The rising trajectory (0.15 → 0.22 over 60 years) reflects modest increase as non-liturgical alternatives became available (secular schools, modern media) and some practitioners began to perceive the constraint as less purely coordinative and more as institutional gatekeeping. But the overall level remains low because participation rates remain high and no significant coerced constituency exists. Suppression (0.18): Low. Barriers to exit include social/cultural costs (community relations, identity questions) and diaspora resource scarcity for non-liturgical Hebrew instruction, but these are not systematic coercion. Religious freedom permits exit; practitioners choose to remain. Theater ratio (0.35): Moderate-low. Some performative elements exist (the ritual form of prayer itself has theatrical dimensions, Torah reading follows prescribed cantillation patterns, interpretive commentary has ceremonial elements), but these are not decoupled from function — the ritual form carries linguistic content that is simultaneously performative and functionally necessary for transmission. The rising trajectory (0.25 → 0.35) reflects modest increase as secular alternatives made the choice to conduct religious practice more obviously volitional and thus more marked as ritual choice rather than default behavior.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal for this reading because most observers who engage with the constraint experience it as coordination rather than extraction. The primary gap is between the practitioner/rabbinic/preservation-network perspectives (all Rope — coordination experience) and the secular identity-seeker perspective (Tangled Rope — embedded identity conflict) and the analytical observer perspective (Mountain — naturalized as linguistic law). The gap between Rope and Tangled Rope is not about disagreement on whether the constraint is extractive (both agree extraction is low to moderate) but about whether the coordination mechanism is experienced as accessible or as requiring identity negotiation. The gap between Rope and Mountain is about whether the constraint is contingent institutional arrangement or natural law. The false-summit detector will flag the mountain classification because the analytical observer naturalizes what is actually a choice between coordination mechanisms (liturgical, secular classroom, informal community use, computational corpus methods) rather than an immutable property of language.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from beneficiary/victim declarations and exit options. The rabbinic authority (organized, arbitrage exit) serves as beneficiary: d ≈ 0.15–0.25 (benefits but with high exit autonomy; benefits are institutional/prestige not material extraction). The practitioner community (moderate, constrained exit) is a secondary beneficiary: d ≈ 0.35–0.40 (gains language and cultural continuity; constrained by social costs, not material barriers). The secular identity seeker (powerless, identity_locked exit) experiences mixed directionality: d ≈ 0.55–0.65 (seeks language benefit but pays identity-conflict cost; cannot exit because exit means identity fragmentation). The language preservation network (powerful, mobile exit) is a beneficiary with maximum autonomy: d ≈ 0.10 (gains institutional credibility but could exit costlessly; chooses to remain). The analytical observer computes d from the mountain perspective (analytical, analytical exit), where d is undefined or neutral because the constraint is seen as natural law rather than a structure that extracts from or benefits particular agents. Effective extraction χ is dampened for high-exit agents (rabbinic authority sees low χ despite being named beneficiary) and modulated upward for identity-locked agents (secular seeker experiences higher χ than raw d would suggest because exit is neurologically/identity-wise costly).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The founding mandate of the liturgical reading constraint is linguistic preservation — maintaining Hebrew as a living language. The function has NOT atrophied: Hebrew has demonstrably persisted as an active language in diaspora religious communities and has revived as a vernacular in Israel. However, the question of whether liturgical reading remains the primary mechanism for this preservation has become contested. The secular Hebrew revival (1880-present) and modern Israeli Hebrew development suggest that liturgical reading was historically necessary but is no longer the primary mechanism in contexts where vernacular use and classroom instruction are available. In diaspora secular Jewish contexts, liturgical reading remains a significant but not exclusive mechanism — secular Hebrew classes, online resources, and cultural engagement now provide alternative pathways. The constraint exhibits LOW theater (0.35): it is not primarily maintained through performative enforcement or theatrical necessity. Practitioners genuinely engage with the content, not merely the form. But the rising trajectory and the emergence of secular alternatives suggest the founding mandate (sole mechanism for linguistic preservation) has been partially superseded. This is not mandatrophy in the sense of complete institutional inertia, but rather the constraint has become optional coordination rather than necessary function in contexts where alternatives exist. The constraint is correctly classified as Rope (pure coordination) rather than Scaffold (with sunset) because there is no declared end point — the religious community intends to continue indefinitely, whether or not liturgical reading remains the primary mechanism for broader linguistic preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_kernel_ambiguity,
    'Is the binding mechanism ''Hebrew remains living through continuous ritual use'' rooted in linguistic necessity or in rabbinic institutional control of the definition of ''living''?',
    'Historical comparison: do languages in other religious traditions (Classical Arabic via Quranic reading, Sanskrit via Vedic mantras, Latin via Catholic liturgy) persist through equivalent mechanisms without comparable institutional authority claims? Cross-case analysis of whether ''living'' status requires liturgical validation or whether secular functional use achieves the same outcome.',
    'If linguistic: the constraint is Mountain (natural language law). If institutional: the constraint is Tangled Rope (coordination + extraction of definitional authority). If both: the reading is correctly Rope (coordination with embedded authority benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_kernel_ambiguity, conceptual, 'Whether linguistic life requires ritual validation or institutional authority claim').

omega_variable(
    secular_hebrew_viability,
    'Does secular daily-use Hebrew (not liturgically-mediated) sustain linguistic continuity as effectively as liturgical practice? And if so, why did liturgical reading become the primary institutional vector?',
    'Historical documentation of Modern Hebrew development (1880-present): role of secular press, literature, schools vs. synagogue reading; temporal analysis of when liturgical reading''s functional role (sole carrier of language continuity) was empirically true vs. when alternatives became viable; interview data from secular Hebrew speakers on language acquisition pathways.',
    'If secular Hebrew is viable: the liturgical reading constraint is historically contingent institutional arrangement (Tangled Rope, not Rope). If secular Hebrew requires liturgical substrate: the Rope classification is correct and the constraint is genuinely coordination. If both are viable: the institutional choice to privilege liturgical reading over secular transmission carries extraction elements (prestige, authority control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_hebrew_viability, empirical, 'Whether secular daily-use Hebrew sustains linguistic life without liturgical mediation').

omega_variable(
    identity_lock_vs_constrained_exit,
    'For diaspora secular Jews seeking Hebrew competence, does the barrier to non-liturgical acquisition (classroom cost, diaspora resource scarcity) explain the identity-locked perception, or does the binding mechanism derive from identity fusion with religious practice itself?',
    'Comparative ethnography: (1) secular diaspora contexts where non-liturgical Hebrew instruction is resource-rich and accessible (Israeli secular schools, university programs, apps) — do identity-locked agents still perceive liturgy as mandatory? (2) Religious diaspora contexts with resource-poor non-liturgical options — do constrained agents report the same language outcome as identity-locked agents? Measure: exit reasoning (structural barrier vs. identity incompatibility) in post-exit interviews.',
    'If barrier-driven: reclassify tangled_rope perspective exit from identity_locked to constrained; extract mechanism is resource scarcity, not identity fusion. If identity-driven: the reading''s extraction mechanism (religious framework requirement) is real and identity_locked is correct. If mixed: the reading contains both mechanisms and the exit_options value is a weighted composite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether exit lock is structural (resource barriers) or internalized (identity fusion)').

omega_variable(
    reading_sibling_continuity_claim,
    'This reading asserts Hebrew remains living through ritual use. The sibling ''native_daily_reading'' asserts Hebrew remains living through daily vernacular use. Can both readings hold Hebrew as ''living'' while grounding ''living'' in mutually exclusive evidence bases, or does one reading logically foreclose the other?',
    'Linguistic definition analysis: operationalize ''living language'' — what counts as evidence? (a) active first-language acquisition in children, (b) vernacular daily use by native speakers, (c) continuous textual transmission, (d) institutional recognition and prestige. Map which reading requires which evidence, and identify any logical contradiction at the definitional level vs. merely different emphasis.',
    'If readings define ''living'' differently but both are valid under different criteria: coexists_with relation is correct. If one reading''s definition logically excludes the other''s empirical claims: forecloses relation applies. If definitional difference masks institutional competition for authority over ''living'' status: influences relation with extraction elements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_continuity_claim, conceptual, 'Whether sibling readings have logically contradictory definitions of ''living language''').

omega_variable(
    rabbinic_authority_beneficiary_status,
    'Does rabbinic authority genuinely benefit from the liturgical reading constraint, or does the classification of rabbis as beneficiaries incorrectly attribute institutional privilege to a coordination role that has eroded in diaspora modernity?',
    'Structural analysis: (1) does rabbinic authority over Hebrew interpretation remain materially consequential (access to resources, institutional power, status hierarchy), or is it largely symbolic in secular diaspora contexts? (2) are rabbis defending the liturgical constraint because it preserves their interpretive authority, or because they believe it is the most effective language preservation mechanism? (3) would rabbis accept secular non-liturgical Hebrew transmission if it achieved equivalent language outcomes? Interview rabbis and scholars on motivations; assess whether authority preservation is causal driver or epiphenomenon.',
    'If authority is still material: beneficiary classification is correct; constraint carries extraction elements. If authority is largely symbolic: constraint is pure coordination; beneficiary classification overestimates institutional power. If mixed: the constraint is correctly Rope (coordination with authority benefit as secondary feature).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_beneficiary_status, empirical, 'Whether rabbinic authority preserves material institutional benefits from liturgical control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liturg_tr_t0, liturgical_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(liturg_tr_t30, liturgical_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(liturg_tr_t60, liturgical_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(liturg_be_t0, liturgical_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(liturg_be_t30, liturgical_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(liturg_be_t60, liturgical_reading, base_extractiveness, 60, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(liturg_su_t0, liturgical_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(liturg_su_t30, liturgical_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(liturg_su_t60, liturgical_reading, suppression_requirement, 60, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_reading, attachment_coordination).
narrative_ontology:affects_constraint(liturgical_reading, hebrew_native_daily_reading).
narrative_ontology:affects_constraint(liturgical_reading, hebrew_continuity_narrative_reading).
narrative_ontology:affects_constraint(liturgical_reading, diaspora_language_transmission).
narrative_ontology:affects_constraint(liturgical_reading, rabbinic_interpretive_authority).

% DUAL FORMULATION NOTE:
% The contested kernel hebrew_living_language decomposes into at least three structurally distinct constraints with different ε values: (1) liturgical_reading (this story) — coordination mechanism through ritual practice; low extraction; ε=0.22. (2) hebrew_native_daily_reading — native speaker acquisition; empirically distinct from (1) on whether linguistic life requires vernacular use; likely ε=0.15 (pure coordination, no extraction). (3) hebrew_continuity_narrative_reading — institutional narrative of transmission; more extractive if narrative is used to justify gatekeeping; likely ε=0.35-0.45 (tangled rope with authority extraction). These stories are linked by network.affects_constraints because the institutional authority to define which mechanism constitutes 'living Hebrew' is contested across all three. Each reading has its own ε because the observable-dependent question 'what constitutes linguistic life?' yields different answers for each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
