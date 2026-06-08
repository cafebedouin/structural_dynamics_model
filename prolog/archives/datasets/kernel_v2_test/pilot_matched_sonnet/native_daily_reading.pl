% ============================================================================
% CONSTRAINT STORY: native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
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
 *   human_readable: Native Daily Use as Criterion for Linguistic Life (Hebrew Revival Reading)
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The native-daily-use criterion for linguistic life is one reading of the
 *   contested kernel 'what makes Hebrew a living language.' This reading
 *   holds that Hebrew was dormant or dead until its reconstruction as a
 *   native vernacular in late 19th/early 20th century Palestine, and that
 *   only generative daily use by native speakers constitutes genuine
 *   linguistic life. The criterion served a real coordination function for
 *   the Zionist state-building project: creating linguistic unity among
 *   immigrants from dozens of language backgrounds required a shared
 *   vernacular, and the 'native daily' frame provided legitimacy for
 *   institutional policies (Hebrew-only education, media, administration)
 *   that displaced existing linguistic practices (Yiddish, Ladino, Arabic,
 *   liturgical Hebrew). The constraint exhibits tangled-rope structure: it
 *   coordinates (solves the genuine problem of linguistic fragmentation in
 *   state-building) and extracts (imposes costs on Yiddish speakers,
 *   delegitimizes liturgical tradition, naturalizes a political choice as a
 *   linguistic universal). The extraction peaked during the mandate period
 *   (1920s-1940s) when institutional enforcement was most intense, then
 *   declined as Hebrew achieved native-speaker critical mass and the
 *   criterion shifted from prescriptive gate to descriptive fact. Theater
 *   ratio reflects the gap between the criterion's claimed universality (a
 *   linguistic fact about what constitutes 'life') and its actual function (a
 *   political tool for state-building). The constraint is structurally
 *   distinct from its sibling readings: the liturgical reading holds that
 *   continuous liturgical transmission constitutes linguistic life without
 *   requiring native speakers; the continuity-narrative reading holds that
 *   Hebrew was never dead because the tradition of study and transmission was
 *   unbroken. Each reading instantiates a different constraint with different
 *   beneficiary/victim structures and different ε values.
 *
 * KEY AGENTS:
 *   - Yiddish-Speaking Communities: Primary victim (powerless/identity_locked) — linguistic identity constituted through Yiddish; criterion delegitimizes their practice and imposes institutional costs
 *   - Liturgical Hebrew Practitioners: Secondary victim (moderate/constrained) — centuries-old tradition reclassified as 'dead'; mixed experience because vernacular shift both validates Hebrew and delegitimizes their practice
 *   - State-Building Project: Primary beneficiary (institutional/arbitrage) — criterion solves genuine coordination problem of creating linguistic unity for state institutions
 *   - Hebrew Educational Institutions: Secondary beneficiary (organized/mobile) — build vernacular infrastructure; see criterion as transitional with structural sunset
 *   - Multilingual Diasporic Communities: Secondary victim (moderate/constrained) — multilingual practice delegitimized by binary living/dead frame
 *   - Analytical Observer: Sees tangled rope (analytical/analytical) — both coordination and extraction structurally present; criterion is not a linguistic universal but served a real historical function
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
narrative_ontology:human_readable(native_daily_reading, "Native Daily Use as Criterion for Linguistic Life (Hebrew Revival Reading)").
narrative_ontology:topic_domain(native_daily_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(native_daily_reading, 'e4bbe7e7-ef16-4df5-91fe-b263b040efdd').
narrative_ontology:cs_kernel_codification('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', formalized).
narrative_ontology:cs_authority_grounding('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', extraction).
narrative_ontology:cs_reading_relation('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', native_daily_reading__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', native_daily_reading__continuity_narrative_reading, forecloses).
narrative_ontology:cs_axiom('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', foundational, vernacular_primacy).
narrative_ontology:cs_axiom_status(vernacular_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', vernacular_primacy, empirically_contingent).
narrative_ontology:cs_axiom('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', secondary, native_speaker_requirement).
narrative_ontology:cs_axiom_status(native_speaker_requirement, holdable).
narrative_ontology:cs_axiom_grounding('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', native_speaker_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', vernacular_nationalist_linguistics).
narrative_ontology:cs_drift_state('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', post_sociolinguistic_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e4bbe7e7-ef16-4df5-91fe-b263b040efdd', '').
narrative_ontology:cs_kernel_id(native_daily_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_daily_reading, state_building_project).
narrative_ontology:constraint_beneficiary(native_daily_reading, hebrew_educational_institutions).
narrative_ontology:constraint_beneficiary(native_daily_reading, zionist_cultural_apparatus).
narrative_ontology:constraint_victim(native_daily_reading, yiddish_speaking_communities).
narrative_ontology:constraint_victim(native_daily_reading, liturgical_hebrew_tradition).
narrative_ontology:constraint_victim(native_daily_reading, multilingual_diasporic_identity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(native_daily_reading, liturgical_hebrew_practitioners).
narrative_ontology:constraint_beneficiary(native_daily_reading, multilingual_diasporic_communities).
narrative_ontology:constraint_victim(native_daily_reading, liturgical_hebrew_practitioners).
narrative_ontology:constraint_victim(native_daily_reading, multilingual_diasporic_communities).
narrative_ontology:constraint_vindicates(native_daily_reading, vernacular_primacy_doctrine).
narrative_ontology:constraint_vindicates(native_daily_reading, linguistic_nationalism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yiddish speakers in early 20th century Palestine faced institutional pressure to abandon their native language in favor of Hebrew. Educational policy, media access, and cultural prestige all flowed toward Hebrew. Exit would require abandoning linguistic identity constituted through Yiddish — not merely learning a new language but dissolving the cultural continuity that Yiddish carried. The native-daily criterion delegitimized their existing linguistic practice by classifying Yiddish as a 'diaspora language' inferior to the 'revived' Hebrew.
narrative_ontology:constraint_stakeholder(native_daily_reading, yiddish_speaking_communities, payer,
    powerless, biographical, identity_locked, regional).

% Practitioners of liturgical Hebrew (rabbis, scholars, religious communities) maintained continuous transmission of Hebrew through study and prayer for centuries. The native-daily criterion reclassified their practice as 'dead language' use, delegitimizing their tradition. But they also benefited from the vernacular revival: increased Hebrew literacy, institutional support for Hebrew study, and cultural validation of Hebrew's importance. Mixed position: the criterion both validates and delegitimizes their practice.
narrative_ontology:constraint_stakeholder(native_daily_reading, liturgical_hebrew_practitioners, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(native_daily_reading, liturgical_hebrew_practitioners, beneficiary).

% The Zionist state-building project required linguistic unity to function: education, administration, military, media all needed a shared language. Hebrew was chosen over Yiddish, Ladino, Arabic, or other candidates. The native-daily criterion provided legitimacy for institutional policies that enforced Hebrew and displaced alternatives. The project captured the coordination benefit (linguistic unity) and could exit the constraint costlessly if coordination costs changed (could adopt multilingual policy if needed, though this was never seriously considered).
narrative_ontology:constraint_stakeholder(native_daily_reading, state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(native_daily_reading, state_building_project, beneficiary).

% Hebrew schools, teacher training programs, and language committees built the vernacular infrastructure. They saw the native-daily criterion as transitional: once Hebrew achieved native-speaker critical mass, the criterion would stop being a prescriptive gate and become a descriptive fact. They benefited from institutional support and cultural prestige but also bore the coordination costs of creating a modern vernacular from a liturgical language (lexical gaps, standardization disputes, pedagogical challenges).
narrative_ontology:constraint_stakeholder(native_daily_reading, hebrew_educational_institutions, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(native_daily_reading, hebrew_educational_institutions, beneficiary).

% Jewish communities maintaining multilingual practices (Hebrew for liturgy, Yiddish or Ladino for daily life, local languages for commerce) found their linguistic ecology delegitimized by the binary living/dead frame. The native-daily criterion treated their multilingualism as deficient rather than as a different mode of linguistic life. But they also benefited from Hebrew's revitalization: access to Hebrew literature, cultural resources, and institutional support for Hebrew study.
narrative_ontology:constraint_stakeholder(native_daily_reading, multilingual_diasporic_communities, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(native_daily_reading, multilingual_diasporic_communities, beneficiary).

% Sociolinguists studying language revitalization, liturgical languages, and linguistic vitality observe that the native-daily criterion is not a linguistic universal but a political choice that served a specific historical function. Research since the 1960s has documented linguistic vitality in non-vernacular contexts (liturgical languages, signed languages, creoles), challenging the vernacular-primacy axiom. The analytical observer sees both the coordination function (linguistic unity for state-building) and the extraction function (displacement of Yiddish, delegitimization of liturgical tradition) as structurally present.
narrative_ontology:constraint_stakeholder(native_daily_reading, sociolinguistic_research_community, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creating linguistic unity for a state-building project drawing immigrants from dozens of language backgrounds. The native-daily criterion solved the genuine problem of enabling institutional function (education, administration, military, media) that would be impossible with linguistic fragmentation.
% TRANSFER_FUNCTION: Cultural prestige, institutional access, and educational resources flowed from Yiddish-speaking communities and liturgical Hebrew practitioners toward the state-building project and Hebrew educational institutions. The transfer was linguistic capital: the legitimacy and resources that had been invested in Yiddish and liturgical Hebrew were redirected toward vernacular Hebrew.
% ABSENT_VOICES: Yiddish cultural organizations and liturgical scholars who would have contested the 'dormant/dead' framing were marginalized in the institutional decision-making. The criterion was set by the state-building project and Hebrew educational institutions; Yiddish speakers and liturgical practitioners were not in the room when the 'native daily use' frame was established as the standard for linguistic life. Their absence enabled the unanimity around the criterion within the institutional beneficiary set.
% DISAPPEARANCE_RATIONALE: If the native-daily criterion disappeared, the institutional arrangements would rearrange substantially. Yiddish would regain cultural prestige and institutional access. Liturgical Hebrew practice would be reclassified as a legitimate mode of linguistic life rather than 'dead language' use. Multilingual diasporic practices would be validated rather than delegitimized. The state-building project's linguistic unity would be contested, and alternative coordination mechanisms (multilingual policy, federated linguistic regions) would become viable. The criterion's disappearance would not return Hebrew to its pre-revival state (Hebrew is now genuinely a native vernacular for millions), but it would change how linguistic life is defined and which practices are legitimated.
% FOUNDING_PROBLEM: Linguistic fragmentation among Jewish immigrants to Palestine in the late 19th and early 20th centuries. Immigrants came from dozens of language backgrounds (Yiddish, Ladino, Arabic, Russian, German, Polish, etc.), and no shared vernacular existed. The state-building project required linguistic unity for institutional function: education, administration, military, media. The founding problem was genuine: how to create a shared language for a population with no common vernacular.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (linguistic fragmentation) is dead: Hebrew is now the native language of the majority of Israeli Jews, and linguistic unity has been achieved. This status is corroborated by demographic data (Israeli Central Bureau of Statistics language surveys), sociolinguistic research (Hebrew as L1 for Israeli-born population), and institutional practice (Hebrew-only education and administration function without controversy). The corroboration comes from outside the beneficiary set: sociolinguists and demographers who study language shift document that the coordination problem the criterion was built to solve no longer exists. However, the criterion persists: 'native daily use' is still invoked to delegitimize diasporic Hebrew practices and to naturalize the vernacular-primacy frame as a linguistic universal rather than a historical contingency.
narrative_ontology:disappearance_verdict(native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(native_daily_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH-SPEAKING COMMUNITIES (SNARE) — Identity-locked because linguistic identity is constituted through Yiddish; exit would require abandoning cultural continuity. The native-daily criterion delegitimizes their existing linguistic practice and imposes costs (educational policy, institutional access, cultural prestige) without providing coordination benefit. Maximum extraction: the criterion exists to displace their language, not to coordinate with it.
constraint_indexing:constraint_classification(native_daily_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: LITURGICAL HEBREW PRACTITIONERS (TANGLED ROPE) — Constrained by institutional pressure and educational policy shifts, but also benefit from increased Hebrew literacy and institutional support for Hebrew study. Mixed experience: the vernacular shift both validates Hebrew's importance (coordination) and delegitimizes their non-native liturgical practice (extraction). The criterion creates a hierarchy where their centuries-old tradition is classified as 'dead' despite continuous transmission.
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE-BUILDING PROJECT (ROPE) — Primary beneficiary. The native-daily criterion solves a genuine coordination problem: creating linguistic unity for a state-building project drawing immigrants from dozens of language backgrounds. Experiences the constraint as pure coordination — the criterion enables institutional function (education, administration, military, media) that would be impossible with linguistic fragmentation. Net beneficiary: extraction runs toward this agent.
constraint_indexing:constraint_classification(native_daily_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEBREW EDUCATIONAL INSTITUTIONS (SCAFFOLD) — Organized agents building the vernacular infrastructure see the native-daily criterion as transitional: once Hebrew achieves native-speaker critical mass, the criterion's enforcement function becomes unnecessary. The constraint has a structural sunset: when Hebrew is genuinely the native language of the majority, the 'native daily use' criterion stops being a gate and becomes a description. Estimated sunset: 2-3 generations (achieved by 1970s-1980s for Israeli-born population).
constraint_indexing:constraint_classification(native_daily_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MULTILINGUAL DIASPORIC COMMUNITIES (TANGLED ROPE) — Constrained by the binary frame (living vs dead) that erases their multilingual practice. Benefit from Hebrew's revitalization (access to Hebrew literature, cultural resources, institutional support) but bear the cost of having their diasporic multilingualism delegitimized. The criterion treats their linguistic ecology as deficient rather than as a different mode of linguistic life.
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The native-daily criterion coordinates (solves the genuine problem of creating a shared vernacular for a state-building project) and extracts (imposes costs on existing linguistic communities, delegitimizes liturgical and diasporic practices, naturalizes a contingent political choice as a linguistic universal). The criterion is not a natural law of linguistics — languages can be 'alive' in multiple modes (liturgical, literary, diasporic) — but it served a real coordination function for a specific historical project. Tangled rope: both functions are structurally present.
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
 *   Extractiveness (0.48): Moderate-high. The native-daily criterion imposed substantial costs on Yiddish-speaking communities (educational policy, institutional access, cultural prestige loss) and delegitimized liturgical Hebrew tradition. But the extraction was not maximal because the criterion also solved a genuine coordination problem: linguistic unity for state-building. The value reflects that the coordination function was real, not merely a cover story, even though the extraction was also real. Suppression (0.62): Moderate-high. Significant institutional enforcement: Hebrew-only education policies, media restrictions, social pressure, career barriers for non-Hebrew speakers. But suppression was not total — Yiddish continued in private domains, and liturgical Hebrew was never fully suppressed. The peak (0.72 in 1940s) reflects maximum institutional enforcement during state formation; decline reflects normalization as Hebrew became genuinely native. Theater ratio (0.35): Moderate. The gap between the criterion's claimed status (a linguistic universal about what constitutes 'life') and its actual function (a political tool for state-building). The theater is lower than typical political constraints because the coordination function was genuine — the criterion was not purely performative. Peak theater (0.42 in 1940s) reflects the period when the 'linguistic life' framing was most detached from the underlying state-building function; decline reflects the criterion becoming descriptively accurate as Hebrew achieved native-speaker status.
 *
 * PERSPECTIVAL GAP:
 *   The native-daily criterion produces a six-way perspectival split. Yiddish speakers see pure extraction (snare): the criterion exists to displace their language. Liturgical practitioners see mixed coordination and extraction (tangled rope): their tradition is delegitimized but Hebrew's importance is validated. The state-building project sees pure coordination (rope): the criterion solves the genuine problem of linguistic unity. Hebrew educational institutions see a transitional coordination mechanism with a sunset (scaffold): once Hebrew is genuinely native, the criterion stops being a gate. Multilingual diasporic communities see mixed coordination and extraction (tangled rope): they benefit from Hebrew's revitalization but their multilingualism is delegitimized. The analytical observer sees tangled rope: both coordination and extraction are structurally present, and the criterion is not a linguistic universal but a political tool that served a real historical function. The gap reveals that 'linguistic life' is not a natural category but a contested frame, and the native-daily reading naturalizes one political outcome as a linguistic fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Yiddish-speaking communities are identity_locked victims: their linguistic identity is constituted through Yiddish, so exit would require abandoning cultural continuity. The engine derives high d (target position) from victim status + identity_locked exit, producing high effective extraction. Liturgical practitioners are constrained victims with mixed beneficiary status: they bear costs (delegitimization) but also gain benefits (increased Hebrew literacy, institutional support). The engine derives moderate d from the mixed position. The state-building project is an institutional beneficiary with arbitrage exit: it captures the coordination benefit and can exit the constraint costlessly (could adopt multilingual policy if coordination costs were lower). The engine derives low d (beneficiary position) from beneficiary status + arbitrage exit, producing low or negative effective extraction. Hebrew educational institutions are organized beneficiaries with mobile exit: they build the infrastructure and benefit from it, but see the constraint as transitional (scaffold perspective). Multilingual diasporic communities are constrained victims: their practice is delegitimized but they also benefit from Hebrew's revitalization. The analytical observer sees the structural mix: coordination and extraction are both present, and the constraint's classification depends on which function is foregrounded.
 *
 * MANDATROPHY ANALYSIS:
 *   The native-daily criterion resolves mandatrophy by showing that the coordination function (linguistic unity for state-building) and the extraction function (displacement of Yiddish, delegitimization of liturgical tradition) are both structurally present and irreducible. The constraint is not mislabeled coordination (it genuinely solved a coordination problem) and not mislabeled extraction (it genuinely imposed costs on identifiable victims). The tangled-rope classification captures the structural mix. The scaffold perspective (Hebrew educational institutions) adds a temporal dimension: the constraint had a sunset, and the extraction declined as the coordination function was achieved. The measurements show this trajectory: extraction peaked during institutional enforcement (1920s-1940s) and declined as Hebrew became genuinely native (1960s-1980s). The theater ratio reflects the gap between the criterion's claimed universality and its actual political function, but the theater is moderate because the coordination function was real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vitality_threshold,
    'Does continuous liturgical transmission without native speakers constitute linguistic life, or does ''life'' require generative daily vernacular use?',
    'Cross-linguistic comparison: Latin, Sanskrit, Classical Arabic, Ge''ez. Operationalize ''life'' via generativity metrics (novel utterance production, semantic extension, syntactic innovation) in liturgical vs vernacular contexts.',
    'If liturgical transmission counts as life: the ''dormant/dead'' framing is false, and Hebrew was continuously alive in a different mode. If only vernacular counts: the native-daily reading is vindicated as a linguistic universal rather than a political choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vitality_threshold, conceptual, 'Whether liturgical transmission constitutes linguistic life').

omega_variable(
    yiddish_displacement_counterfactual,
    'Could the state-building coordination function have been achieved without displacing Yiddish, or was linguistic unity a structural requirement?',
    'Comparative analysis: multilingual state-building projects (Switzerland, Belgium, India, Singapore). Identify coordination costs of multilingualism vs extraction costs of linguistic displacement.',
    'If multilingual coordination was feasible: the Yiddish displacement was extractive rather than necessary. If linguistic unity was structurally required: the extraction was a coordination cost rather than avoidable harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yiddish_displacement_counterfactual, empirical, 'Whether Yiddish displacement was structurally necessary').

omega_variable(
    committer_frame_ambiguity,
    'Is this constraint one reading of the contested kernel ''what makes Hebrew a living language,'' or is the kernel itself a retrospective construction that naturalizes a political outcome?',
    'Historical discourse analysis: when did ''living language'' become a contested category for Hebrew? Was the debate contemporaneous with the revival, or did it emerge later as a legitimation narrative? Trace the genealogy of the ''dormant/dead'' framing.',
    'If the kernel is contemporaneous: the readings represent genuine structural alternatives. If the kernel is retrospective: the entire committer frame may be a post-hoc rationalization, and the ''readings'' are not competing interpretations but successive legitimation strategies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Whether the kernel is a genuine historical dispute or a retrospective construction').

omega_variable(
    vernacular_primacy_grounding,
    'Is the vernacular-primacy axiom grounded in empirical linguistics (languages without native speakers exhibit reduced generativity) or in nationalist ideology (states require linguistic unity)?',
    'Separate the empirical claim (generativity metrics for liturgical vs vernacular languages) from the normative claim (only vernacular use constitutes legitimate linguistic life). Test whether the empirical claim, if true, entails the normative claim.',
    'If empirically grounded: the axiom is falsifiable and could be overridden by counterevidence. If ideologically grounded: the axiom is a political commitment masquerading as a linguistic fact, and the ''native daily'' criterion is a cover story for state-building extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vernacular_primacy_grounding, conceptual, 'Whether vernacular primacy is empirical or ideological').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_daily_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1880s, native_daily_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(theater_1900s, native_daily_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(theater_1920s, native_daily_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(theater_1940s, native_daily_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(theater_1960s, native_daily_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(theater_1980s, native_daily_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(extraction_1880s, native_daily_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(extraction_1900s, native_daily_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(extraction_1920s, native_daily_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(extraction_1940s, native_daily_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(extraction_1960s, native_daily_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(extraction_1980s, native_daily_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1880s, native_daily_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(suppression_1900s, native_daily_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(suppression_1920s, native_daily_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(suppression_1940s, native_daily_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(suppression_1960s, native_daily_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(suppression_1980s, native_daily_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(native_daily_reading, liturgical_reading).
narrative_ontology:affects_constraint(native_daily_reading, continuity_narrative_reading).

% DUAL FORMULATION NOTE:
% The native_daily_reading is one of three constraint stories in the hebrew_living_language family. Each reading has its own ε value reflecting different beneficiary/victim structures. The native_daily_reading has moderate-high extraction (0.48) because it imposes costs on Yiddish speakers and liturgical practitioners. The liturgical_reading (not yet authored) would have lower extraction because it does not require vernacular displacement. The continuity_narrative_reading (not yet authored) would have the lowest extraction because it does not delegitimize any existing practice. The readings are linked via network.affects_constraints because they compete for legitimacy: adopting one reading changes the structural position of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
