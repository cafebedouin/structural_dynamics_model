% ============================================================================
% CONSTRAINT STORY: native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_native_daily_reading, []).

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
 *   constraint_id: native_daily_reading
 *   human_readable: Native Daily Use Reading of Hebrew Vitality
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   The native-daily reading of Hebrew vitality holds that only vernacular,
 *   everyday use constitutes a 'living' language; liturgical recitation, no
 *   matter how competent or widespread, is categorized as 'preservation' of a
 *   dead language. This reading emerged during late 19th and early 20th
 *   century Zionist language revival efforts and became institutionally
 *   dominant in the State of Israel. The constraint required substantial
 *   institutional enforcement: lexical expansion through neologism
 *   committees, suppression of Yiddish and Judeo-Arabic in favor of Hebrew,
 *   mandatory Hebrew education in secular contexts, and systematic
 *   devaluation of liturgical-only competence. The native-daily criterion
 *   created asymmetric costs: liturgical tradition bearers (rabbis, cantors,
 *   traditional scholars) saw their sacred register reframed as inadequate;
 *   diasporic multilingual communities had to adopt Israeli Hebrew norms to
 *   participate in Hebrew-language institutions. The Zionist state-building
 *   project benefited from this reading by establishing Hebrew as the
 *   administrative and civic language of a modern nation-state,
 *   differentiating Israeli identity from diasporic Jewish identity. This is
 *   one of three sibling readings of the Hebrew vitality kernel; the others
 *   (liturgical reading and hybrid continuity reading) frame the relationship
 *   between sacred and vernacular registers differently.
 *
 * KEY AGENTS:
 *   - Liturgical Tradition Bearers: Primary victim (powerless/identity_locked) — religious scholars and cantors whose liturgical Hebrew competence is reframed as preservation rather than living use; identity constituted through sacred register
 *   - Diasporic Multilingual Communities: Secondary victim (moderate/constrained) — bear costs of register shift and institutional pressure to adopt Israeli norms; also gain vernacular access
 *   - Zionist State-Building Project: Primary beneficiary (institutional/arbitrage) — captures state legitimacy and cultural distinctiveness through indigenous language revival narrative
 *   - Vernacular Speakers: Secondary beneficiary (moderate/mobile) — native Israeli Hebrew speakers who benefit from institutional support for vernacular Hebrew
 *   - Secular Hebrew Institutions: Secondary beneficiary (institutional/arbitrage) — schools, media, government apparatus operating in vernacular Hebrew
 *   - Vernacular Reconstruction Coalition: Organized agents (organized/mobile) — linguists and educators building vernacular infrastructure; see constraint as scaffold with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(native_daily_reading, 0.48).
domain_priors:suppression_score(native_daily_reading, 0.62).
domain_priors:theater_ratio(native_daily_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(native_daily_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(native_daily_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(native_daily_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(native_daily_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(native_daily_reading, tangled_rope).
narrative_ontology:human_readable(native_daily_reading, "Native Daily Use Reading of Hebrew Vitality").
narrative_ontology:topic_domain(native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(native_daily_reading, 'd0a500ec-5fbc-4ac1-a9a9-a24b12d2464a').
narrative_ontology:cs_kernel_codification('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', distributed).
narrative_ontology:cs_authority_grounding('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', extraction).
narrative_ontology:cs_interpretation_layer_present('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a').
narrative_ontology:cs_reading_relation('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', native_daily_reading__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', native_daily_reading__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', foundational, vernacular_use_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(vernacular_use_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', vernacular_use_constitutes_linguistic_life, empirically_contingent).
narrative_ontology:cs_axiom('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', foundational, liturgical_use_is_preservation_not_vitality).
narrative_ontology:cs_axiom_status(liturgical_use_is_preservation_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', liturgical_use_is_preservation_not_vitality, conventional).
narrative_ontology:cs_axiom('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', secondary, desacralization_enables_modernization).
narrative_ontology:cs_axiom_status(desacralization_enables_modernization, holdable).
narrative_ontology:cs_axiom_grounding('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', desacralization_enables_modernization, instrumental).
narrative_ontology:cs_reference_frame('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', pre_vernacular_liturgical_equilibrium).
narrative_ontology:cs_drift_state('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', post_statehood_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d0a500ec-5fbc-4ac1-a9a9-a24b12d2464a', '').
narrative_ontology:cs_kernel_id(native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(native_daily_reading, vernacular_speakers).
narrative_ontology:constraint_beneficiary(native_daily_reading, secular_hebrew_institutions).
narrative_ontology:constraint_victim(native_daily_reading, liturgical_tradition_bearers).
narrative_ontology:constraint_victim(native_daily_reading, diasporic_multilingual_communities).
narrative_ontology:constraint_victim(native_daily_reading, sacred_register_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(native_daily_reading, diasporic_multilingual_communities).
narrative_ontology:constraint_vindicates(native_daily_reading, language_revival_through_vernacularization).
narrative_ontology:constraint_vindicates(native_daily_reading, desacralization_as_modernization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious scholars, rabbis, and cantors whose professional and spiritual identity is constituted through competence in sacred Hebrew. The native-daily criterion reframes their liturgical expertise as 'preservation' of a dead language rather than living use. Cannot exit without abandoning religious vocation and identity. Experience institutional devaluation: liturgical-only competence is insufficient for citizenship, employment, or secular education participation in Israel.
narrative_ontology:constraint_stakeholder(native_daily_reading, liturgical_tradition_bearers, payer,
    powerless, biographical, identity_locked, national).

% Jewish communities outside Israel maintaining Hebrew as one register among several (Yiddish, Judeo-Arabic, local vernaculars, plus liturgical Hebrew). Face institutional pressure to adopt Israeli Hebrew norms to access Hebrew-language media, literature, and Israeli institutions. Bear costs of register shift (devaluation of alternative Hebrew registers, pressure to suppress non-Hebrew Jewish languages) while gaining vernacular Hebrew competence and access to Israeli cultural production. Mixed position: constrained by institutional dominance of Israeli norms but also benefit from expanded Hebrew linguistic infrastructure.
narrative_ontology:constraint_stakeholder(native_daily_reading, diasporic_multilingual_communities, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(native_daily_reading, diasporic_multilingual_communities, beneficiary).

% The Israeli state apparatus and associated institutions (Hebrew Language Academy, Ministry of Education, national media) that established Hebrew as the administrative and civic language of Israel. Sets the agenda by defining what counts as Hebrew vitality (native daily use) and controls institutional recognition of Hebrew competence. Primary beneficiary: captures state legitimacy through indigenous language revival narrative, establishes cultural distinctiveness from diaspora Jewish identity, consolidates secular nationalist identity against religious authority.
narrative_ontology:constraint_stakeholder(native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(native_daily_reading, zionist_state_building_project, beneficiary).

% Native Israeli Hebrew speakers who use Hebrew as their primary daily language. Benefit from institutional support (Hebrew-language schools, media, government services) and from the cultural capital of speaking the 'living' language. Experience the constraint as seamless coordination: Hebrew functions as their vernacular without requiring liturgical competence or multilingual navigation.
narrative_ontology:constraint_stakeholder(native_daily_reading, vernacular_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Schools, universities, media organizations, government ministries operating in vernacular Hebrew. Benefit from the native-daily criterion by capturing institutional authority over Hebrew-language production and education. The criterion privileges secular over religious institutions by making liturgical-only competence insufficient for participation in secular institutional contexts.
narrative_ontology:constraint_stakeholder(native_daily_reading, secular_hebrew_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Linguists, educators, and cultural planners who built the vernacular Hebrew infrastructure (neologism committees, Hebrew Language Academy, secular education curricula). See the native-daily criterion as a transitional mechanism with sunset logic: needed to bootstrap vernacular competence across a generation but dissolving once Hebrew is established as a living vernacular. Organized and mobile: can shift to other language planning projects if Hebrew vernacularization succeeds or fails.
narrative_ontology:constraint_stakeholder(native_daily_reading, vernacular_reconstruction_coalition, agenda_setter,
    organized, generational, mobile, global).

% Scholarly observer analyzing Hebrew language vitality from outside the immediate institutional and identity stakes. Sees the native-daily criterion as a constructed category serving nationalist goals rather than a linguistic natural law. Recognizes genuine coordination function (solving language revival collective action problem) alongside substantial extraction (asymmetric costs on liturgical tradition and diasporic multilingualism). Can analyze the constraint without being bound by its categories.
narrative_ontology:constraint_stakeholder(native_daily_reading, analytical_sociolinguist, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a shared vernacular language for administrative communication, secular education, and civic participation in a multilingual immigrant society undergoing nation-state formation.
% TRANSFER_FUNCTION: Institutional recognition, cultural capital, and state resources flow from the apparatus controlling vernacular Hebrew norms (Israeli state institutions, Hebrew Language Academy, secular education system) to vernacular speakers and secular institutions. Costs (loss of register status, institutional access barriers, pressure to suppress alternative registers) flow from liturgical tradition bearers and diasporic multilingual communities toward the vernacular-privileging apparatus.
% ABSENT_VOICES: Ultra-Orthodox communities maintaining Yiddish and liturgical Hebrew without vernacular Israeli Hebrew competence; Mizrahi communities whose Judeo-Arabic registers were systematically suppressed; diasporic communities for whom Hebrew is one register among several rather than a primary vernacular. These voices are structurally excluded from defining what counts as Hebrew vitality because the native-daily criterion frames their multilingual or liturgical-only Hebrew use as inadequate.
% DISAPPEARANCE_RATIONALE: If the native-daily criterion disappeared, multiple institutional arrangements would rearrange: (1) liturgical-only competence would gain institutional recognition in citizenship, education, and employment contexts; (2) diasporic Hebrew registers (Judeo-Arabic influences, Yiddish-inflected Hebrew, alternative pronunciation traditions) would have equal status to Israeli Hebrew; (3) the Hebrew Language Academy's authority to control neologisms and suppress alternative registers would lose legitimacy; (4) religious institutions could claim equal authority over Hebrew language norms. The constraint's disappearance would not eliminate Hebrew use but would restructure who controls Hebrew's institutional recognition and what forms of Hebrew count as legitimate. Verdict is uncontested among stakeholders (even beneficiaries recognize the criterion structures institutional access) but the DESIRABILITY of the rearrangement is contested.
% FOUNDING_PROBLEM: Late 19th and early 20th century Zionist language planners faced a genuine collective action problem: establishing a shared linguistic infrastructure for a multilingual immigrant population with no common vernacular. Diasporic Jewish communities used Hebrew primarily as a liturgical language alongside various vernaculars (Yiddish, Judeo-Arabic, Ladino, etc.). Creating a modern nation-state required a shared administrative language for government, education, and civic life. The native-daily criterion was the mechanism chosen to solve this problem: establish Hebrew as the primary vernacular by institutional mandate rather than relying on organic multilingualism.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is disputed between two framings: (1) State-building perspective (Israeli Ministry of Education, Hebrew Language Academy): the founding problem is DEAD — Hebrew is successfully established as a living vernacular with native speakers, making continued enforcement unnecessary but maintained for cultural consolidation. This framing is self-interested (the institutions benefit from continued dominance). (2) Sociolinguistic observer perspective (academic language planning scholarship, e.g., Bernard Spolsky's work on Hebrew language policy): the founding problem was SOLVED by mid-20th century but the criterion persists beyond its functional necessity, now serving identity-boundary maintenance rather than communication needs. (3) Religious Zionist perspective (orthodox educators like Eliezer Schweid): the founding problem was MIS-SPECIFIED — the real problem was maintaining Hebrew continuity across sacred and vernacular registers, not replacing liturgical with vernacular use; the native-daily solution created new problems (desacralization, diaspora-Israel linguistic divide) while solving the original one. Corroboration from outside beneficiary set: sociolinguistic scholarship broadly agrees the language-revival coordination problem was solved by mid-20th century but debates whether continued enforcement serves communication or identity-political goals.
narrative_ontology:disappearance_verdict(native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(native_daily_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL TRADITION BEARER (SNARE) — Identity-locked through religious identity constituted by sacred Hebrew. The vernacular revival desacralizes the language they have spent a lifetime learning to read and chant. Cannot exit without abandoning identity as liturgical expert. Experiences the constraint as pure extraction: their sacred register is reframed as 'dead' preservation while secular daily use is valorized as 'living' language.
constraint_indexing:constraint_classification(native_daily_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: DIASPORIC MULTILINGUAL COMMUNITY (TANGLED ROPE) — Constrained by institutional pressure to adopt Israeli Hebrew norms but also benefits from increased access to Hebrew media, literature, and communication networks. Bears costs of register shift (liturgical competence devalued) while gaining vernacular competence. Mixed experience: genuine coordination (shared linguistic infrastructure) alongside asymmetric extraction (must adopt secular Israeli norms to participate).
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ZIONIST STATE-BUILDING PROJECT (ROPE) — Primary beneficiary experiencing the constraint as coordination. Vernacular Hebrew solves genuine nation-building problems: administrative communication, secular education, civic participation. Captures institutional benefits (state legitimacy through indigenous language revival, cultural distinctiveness from diaspora). Arbitrage-level exit: the state apparatus can shift language policy at will.
constraint_indexing:constraint_classification(native_daily_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: VERNACULAR RECONSTRUCTION COALITION (SCAFFOLD) — Organized linguists, educators, and cultural planners see this as a transitional coordination mechanism. The native-daily criterion serves to bootstrap vernacular competence across a generation; once Hebrew is established as a living vernacular, the dichotomy between 'native daily use' and 'ritual recitation' loses salience. Sunset logic: the constraint is needed to establish the vernacular but dissolves once speakers have native competence in both registers.
constraint_indexing:constraint_classification(native_daily_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SECULAR HEBREW EDUCATOR (TANGLED ROPE) — Benefits from institutional support for vernacular Hebrew education but also constrained by the native-daily criterion's requirement to suppress liturgical Hebrew in secular contexts. Genuine coordination function (teaching a living language) alongside extraction (must actively devalue students' liturgical background to maintain the native-daily distinction). Mixed beneficiary-victim position.
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical perspective, this constraint exhibits genuine coordination (solving the collective action problem of language revival) alongside substantial extraction (liturgical tradition bearers bear asymmetric costs of desacralization; diasporic communities must adopt Israeli norms). The native-daily criterion required active institutional enforcement (lexical expansion, neologism committees, suppression of Yiddish and Judeo-Arabic) and created identifiable victims. Not a natural law of language vitality — a constructed criterion serving specific nationalist goals.
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(native_daily_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(native_daily_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(native_daily_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The native-daily criterion required active institutional enforcement and created identifiable victims. Liturgical tradition bearers lost status and institutional recognition; diasporic communities faced pressure to abandon alternative registers. The extraction is substantial but not maximal because genuine coordination benefits exist (shared linguistic infrastructure, increased access to Hebrew media and literature). The value reflects that vernacular revival solved real collective action problems (administrative communication, secular education) while imposing asymmetric costs on non-vernacular Hebrew users. Suppression (0.62): Moderate-high. Significant institutional enforcement: Yiddish and Judeo-Arabic were actively suppressed in Israeli schools; liturgical-only competence was systematically devalued in employment and citizenship contexts; neologism committees controlled lexical expansion to privilege secular over sacred registers. But suppression was not total — liturgical Hebrew continued in religious contexts, and some diasporic communities maintained alternative registers. Theater ratio (0.35): Moderate-low. The native-daily criterion is functional, not primarily performative. Vernacular Hebrew genuinely serves as the administrative and civic language of Israel; it is not merely symbolic. However, some theatrical content exists in the rhetorical framing of liturgical use as 'dead' — this framing serves ideological goals (establishing secular nationalist identity) beyond the functional requirement of vernacular communication.
 *
 * PERSPECTIVAL GAP:
 *   The Zionist state-building project experiences rope — vernacular revival solves genuine nation-building coordination problems. Liturgical tradition bearers experience snare — their sacred register is delegitimized with no exit path that preserves their religious identity. Diasporic multilingual communities and secular Hebrew educators experience tangled rope — they face mixed coordination and extraction, benefiting from some aspects of vernacular infrastructure while bearing costs of register shift and institutional pressure. The vernacular reconstruction coalition sees scaffold — the native-daily criterion is a transitional mechanism with sunset logic, needed to bootstrap vernacular competence but dissolving once Hebrew is established as a living vernacular. The analytical observer identifies this as tangled rope at the civilizational scale: genuine coordination (solving language revival collective action problem) inseparable from substantial extraction (asymmetric costs imposed on liturgical tradition and diasporic multilingualism). The perspectival gap reveals that what counts as 'language vitality' is not a natural category but a constructed criterion serving specific institutional goals.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Zionist state-building project, vernacular speakers, secular institutions) experience low or negative effective extraction — the constraint subsidizes their position by providing institutional support for vernacular Hebrew and cultural legitimacy through indigenous language revival. Victims (liturgical tradition bearers, diasporic multilingual communities) experience high effective extraction — they bear costs of desacralization and register shift without proportional institutional benefits. Liturgical tradition bearers are identity-locked: their professional and religious identity is constituted through sacred Hebrew competence, so the reframing of their register as 'preservation' rather than 'life' is experienced as delegitimization of their core identity. Diasporic communities are constrained: they face institutional pressure to adopt Israeli Hebrew norms but retain some mobility (can maintain multilingual practices in diaspora contexts). The analytical observer, despite having analytical exit and power, experiences the constraint as tangled rope because the coordination function (language revival) and extraction mechanism (desacralization, register suppression) are structurally inseparable in this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy risk because the native-daily criterion could outlive its coordination function. If the original mandate was to establish Hebrew as a living vernacular (already accomplished by mid-20th century), the continued suppression of liturgical-only competence and devaluation of non-Israeli Hebrew registers may represent mandate drift — the criterion persisting beyond its functional justification. However, mandatrophy is not yet resolved: the constraint continues to serve state-building goals (maintaining cultural distinctiveness from diaspora, privileging secular over religious identity) even if the language-revival goal is complete. The scaffold perspective suggests a sunset: once vernacular competence is universal, the dichotomy between native-daily and liturgical use loses salience. But the constraint's extractiveness has stabilized rather than declined (measurements show extraction peak at t=40, then stabilize rather than sunset), suggesting the criterion may be maintained for ideological reasons beyond its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_competence_survival,
    'Does liturgical Hebrew competence persist as a distinct register among native vernacular speakers, or does vernacularization erode liturgical competence over generations?',
    'Longitudinal study of liturgical Hebrew competence across generations of native Israeli Hebrew speakers; comparison of liturgical reading fluency in vernacular vs. non-vernacular Hebrew-speaking Jewish communities.',
    'If liturgical competence persists: the native-daily vs. ritual dichotomy is false — both registers coexist in living use. If liturgical competence erodes: the extraction from liturgical tradition bearers is permanent — desacralization was structural, not just rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_competence_survival, empirical, 'Whether vernacularization erodes liturgical register competence').

omega_variable(
    register_continuity_definition,
    'Is liturgical Hebrew use genuinely ''preservation'' rather than ''life,'' or does the native-daily criterion artificially devalue liturgical competence to privilege vernacular use?',
    'Conceptual analysis of what constitutes ''living'' language use; cross-linguistic comparison of liturgical vs. vernacular register vitality in other revitalized languages (Irish, Maori, Hawaiian).',
    'If liturgical use constitutes vitality: this reading''s core premise (native daily use = life, ritual = preservation) is a constructed criterion serving nationalist goals, not a linguistic natural law. If liturgical use is genuinely preservation: the criterion reflects real structural difference between registers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(register_continuity_definition, conceptual, 'Whether the native-daily vs. liturgical distinction reflects natural vitality difference or constructed criterion').

omega_variable(
    kernel_framing_arbitrariness,
    'Is ''Hebrew vitality'' the kernel this constraint addresses, or does this reading construct the kernel to justify vernacularization?',
    'Historical analysis of when ''Hebrew vitality'' discourse emerged; identification of whether vitality-framing preceded or followed vernacular revival efforts; analysis of alternative framings (continuity, transmission, sacred preservation) suppressed by the vitality frame.',
    'If the kernel predates this reading: the reading is one interpretation of a stable question. If this reading constructed the kernel: the constraint is self-justifying — it defines ''vitality'' such that only vernacular use counts, then measures vitality by that definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_arbitrariness, conceptual, 'Whether Hebrew vitality is a stable kernel or a frame constructed by this reading').

omega_variable(
    sibling_reading_foreclosure,
    'Does the institutional success of native-daily vernacularization structurally foreclose the liturgical reading, or do they genuinely coexist?',
    'Analysis of whether Israeli institutional Hebrew allows liturgical-only competence to count as Hebrew fluency in citizenship, education, or employment contexts; tracking of liturgical Hebrew speakers'' access to Hebrew-language institutions without vernacular competence.',
    'If liturgical-only competence is institutionally recognized: the readings coexist. If institutional participation requires vernacular competence: the native-daily reading has structurally foreclosed the liturgical reading despite claiming they coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, empirical, 'Whether institutional vernacularization forecloses liturgical-only participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_daily_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(native_daily_theater_1880, native_daily_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(native_daily_theater_1900, native_daily_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(native_daily_theater_1920, native_daily_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(native_daily_theater_1940, native_daily_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(native_daily_theater_1960, native_daily_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement(native_daily_theater_1980, native_daily_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(native_daily_extraction_1880, native_daily_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(native_daily_extraction_1900, native_daily_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(native_daily_extraction_1920, native_daily_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(native_daily_extraction_1940, native_daily_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(native_daily_extraction_1960, native_daily_reading, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(native_daily_extraction_1980, native_daily_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(native_daily_suppression_1880, native_daily_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(native_daily_suppression_1900, native_daily_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(native_daily_suppression_1920, native_daily_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(native_daily_suppression_1940, native_daily_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(native_daily_suppression_1960, native_daily_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(native_daily_suppression_1980, native_daily_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(native_daily_reading, liturgical_reading).
narrative_ontology:affects_constraint(native_daily_reading, hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The native_daily_reading is one of three constraint stories decomposed from the colloquial label 'Hebrew language vitality.' The ε values differ substantially: native_daily_reading has moderate extraction (0.48) due to institutional enforcement and asymmetric costs; liturgical_reading has lower extraction (sacred register maintained without state apparatus); hybrid_continuity_reading has lowest extraction (both registers recognized as vital). The readings are linked via network.affects_constraints because the institutional dominance of native_daily_reading structurally constrains the viability of the liturgical_reading in civic contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
