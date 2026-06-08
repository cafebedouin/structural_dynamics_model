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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Hebrew as Living Language: Native Daily Vernacular Reading
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The native-daily reading of Hebrew-as-living-language instantiates one
 *   normative stance within a contested kernel: what makes a language
 *   'living'? This reading claims that Hebrew is living only when native
 *   speakers generate novel utterances in daily vernacular contexts—a
 *   criterion that simultaneously coordinates a multilingual immigrant
 *   society around shared linguistic substrate and extracts from competing
 *   diaspora traditions (Yiddish, Ladino), liturgical Hebrew, and
 *   multilingual community practices. The reading emerged as institutional
 *   policy during the Yishuv period and solidified post-1948 as part of
 *   nation-state consolidation. It produces genuine coordination
 *   benefits—establishing a common vernacular among diverse immigrant
 *   populations facing governance and cultural integration challenges—but
 *   bundled with these benefits is asymmetric institutional resource
 *   allocation, suppression of competing language transmission, and
 *   delegitimization of non-native fluency pathways. The constraint operates
 *   through education policy, cultural institutional funding, media
 *   normativity, and social prestige mechanisms rather than through legal
 *   prohibition. The theater ratio (0.35) is moderate: the reading is
 *   grounded in genuine linguistic science (living languages do require
 *   native transmission and daily use), but institutional enforcement deploys
 *   this science selectively to suppress alternatives rather than to measure
 *   all candidates by the same standard. This story instantiates the
 *   tangled_rope type because it contains both a real coordination function
 *   (multilingual population needs shared language) and real asymmetric
 *   extraction (resources and cultural legitimacy flow toward Hebrew at the
 *   expense of competing traditions).
 *
 * KEY AGENTS:
 *   - Hebrew Educational Institutions: Institutional beneficiary (institutional/arbitrage) — captures educational resource concentration and cultural legitimacy from native-daily reading; can arbitrage into alternative language policies if incentives shift
 *   - Yiddish/Ladino Communities: Primary victim (powerless/identity_locked) — identity fusion with diaspora linguistic inheritance; structurally mobile but cannot exercise exit because identity frame makes competing language essential to self-concept
 *   - Multilingual Jewish Diaspora: Secondary victim (moderate/constrained) — face resource barriers to maintaining competing traditions and social costs of valuing multilingual competence; some benefit from Hebrew access
 *   - Nation-State Hebrew Identity (Israeli Political Authority): Institutional beneficiary (institutional/mobile) — extracts linguistic unity and cultural identity consolidation from the reading; could adopt alternative readings but benefits from monolinguality thesis
 *   - Liturgical Hebrew Custodians: Marginalized actor (institutional/constrained) — maintain classical Hebrew through religious obligation but de facto delegitimized by authenticity criterion; constrained by institutional narrative pressure
 *   - Language Preservation Movements: Organized competitor (organized/constrained) — organized agents working to maintain competing traditions; compete for same institutional resources; structurally constrained by budget and cultural prestige asymmetries
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing the native-daily criterion as linguistic truth rather than political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(native_daily_reading, 0.52).
domain_priors:suppression_score(native_daily_reading, 0.48).
domain_priors:theater_ratio(native_daily_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(native_daily_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(native_daily_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(native_daily_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(native_daily_reading, tangled_rope).
narrative_ontology:human_readable(native_daily_reading, "Hebrew as Living Language: Native Daily Vernacular Reading").
narrative_ontology:topic_domain(native_daily_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(native_daily_reading, '15d9eeba-4eb2-4c84-9c89-bb22b3b289c9').
narrative_ontology:cs_kernel_codification('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', formalized).
narrative_ontology:cs_authority_grounding('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', lineage).
narrative_ontology:cs_interpretation_layer_present('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9').
narrative_ontology:cs_reading_relation('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', native_daily_reading__hebrew_liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', native_daily_reading__hebrew_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', foundational, native_speaker_authenticity_requirement).
narrative_ontology:cs_axiom_status(native_speaker_authenticity_requirement, holdable).
narrative_ontology:cs_axiom_grounding('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', native_speaker_authenticity_requirement, empirically_contingent).
narrative_ontology:cs_axiom('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', foundational, daily_vernacular_necessity).
narrative_ontology:cs_axiom_status(daily_vernacular_necessity, holdable).
narrative_ontology:cs_axiom_grounding('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', daily_vernacular_necessity, empirically_contingent).
narrative_ontology:cs_axiom('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', secondary, nation_building_linguistic_consolidation).
narrative_ontology:cs_axiom_status(nation_building_linguistic_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', nation_building_linguistic_consolidation, instrumental).
narrative_ontology:cs_reference_frame('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', hebrew_native_vernacular_primacy).
narrative_ontology:cs_drift_state('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', contemporary_diaspora_persistence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15d9eeba-4eb2-4c84-9c89-bb22b3b289c9', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(native_daily_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_daily_reading, hebrew_speaking_institutional_bodies).
narrative_ontology:constraint_beneficiary(native_daily_reading, nation_state_hebrew_identity).
narrative_ontology:constraint_victim(native_daily_reading, competing_diaspora_vernaculars).
narrative_ontology:constraint_victim(native_daily_reading, liturgical_hebrew_tradition).
narrative_ontology:constraint_victim(native_daily_reading, multilingual_jewish_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA VERNACULAR COMMUNITIES (SNARE) — Identity-locked to their linguistic inheritance; structurally mobile (could migrate, shift languages, participate in Hebrew education) but identity fusion with Yiddish/Ladino makes exit literally unthinkable from within the community frame. The reading's core premise ('living Hebrew = native daily utterance') explicitly forecloses the legitimacy of liturgical or diaspora-vernacular fluency. These communities bear extraction through institutional defunding of competing language transmission and cultural delegitimization. No organized exit; trapped by cognitive/identity frame.
constraint_indexing:constraint_classification(native_daily_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: MULTILINGUAL DIASPORA COMMUNITIES (TANGLED ROPE) — Experience coordination function (access to Hebrew culture, connection to Israel, educational opportunity) alongside extraction (pressure to abandon ancestral languages, value diminishment of multilingual competence). Constrained exit: maintaining competing vernacular involves social cost, institutional pressure, educational bifurcation. Some benefit from the Hebrew revitalization ecosystem but also pay through linguistic devaluation.
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEBREW INSTITUTIONAL BENEFICIARIES (ROPE) — Ministry of Education, Hebrew language academies, Ulpan systems, cultural institutions. Net beneficiaries from the native-daily reading. Experiences the constraint as legitimate coordination: establishing Hebrew as living vernacular solves genuine collective-action problem of creating shared linguistic substrate for diverse immigrant populations. Arbitrage options: can pursue competing language projects (minority-language preservation, bilingual education) if incentive structures shift, but institutional position favors Hebrew primacy.
constraint_indexing:constraint_classification(native_daily_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED PRESERVATION MOVEMENTS (TANGLED ROPE) — Organized agents (Yiddish cultural councils, Ladino studies programs, multilingual Jewish education initiatives) see both coordination and extraction. Coordination: preservation work benefits from broader language infrastructure, documentation methods, institutional frameworks. Extraction: competing for same institutional resources; delegitimized by the 'native daily' authenticity criterion. Constrained exit: preservation is resource-intensive; diversion of state funding toward Hebrew monolinguality increases costs. Some agency but significant structural pressure.
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LITURGICAL HEBREW GUARDIANS (PITON) — Religious institutions, rabbinical seminaries, traditional communities that maintain classical Hebrew. The native-daily reading largely marginalizes this perspective by treating liturgical Hebrew as inauthentic or 'not living.' Liturgical custody persists through institutional inertia and religious obligation but is de facto delegitimized by the reading's authenticity standard. Theater ratio reflects the performative maintenance of classical Hebrew for religious purposes alongside dismissal of it as 'not actually living.' Arbitrage options exist but are suppressed through narrative framing rather than legal prohibition.
constraint_indexing:constraint_classification(native_daily_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATION-STATE HEBREW IDENTITY (TANGLED ROPE) — Central authority claiming Hebrew native-daily fluency as identity substrate for national cohesion. Genuine coordination function: linguistic commonality solves real problem of governance and cultural integration for diverse immigrant populations. Simultaneous extraction: the native-daily reading legitimizes resource concentration toward Hebrew education and suppresses funding for multilingual or diaspora-tradition maintenance. Mobile exit: alternative readings (liturgical, hybrid, multilingual) could be institutionalized, but political-structural incentives favor monolinguality. Beneficiary position (cohesion extraction) and coordinator position (language-as-unity-mechanism) bundled.
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LINGUISTIC NATURALISM (MOUNTAIN) — From a civilizational/universal analytical seat, one might view living language as a natural-law phenomenon: languages persist only through daily native use; any language supported primarily by institutional mandate or liturgical tradition naturally decays and ceases to be 'living.' This perspective treats the native-daily criterion as discovered fact about language vitality rather than normative choice. However, the structural data contradicts the mountain classification: the reading has identifiable beneficiaries (Hebrew institutions), enforced suppression of alternatives, and institutional resource flows. Engine false-summit detection will identify this as naturalization of a political-institutional arrangement.
constraint_indexing:constraint_classification(native_daily_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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

test(piton_threshold) :-
    domain_priors:theater_ratio(native_daily_reading, TR),
    TR >= 0.70.

:- end_tests(native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The native-daily reading creates genuine asymmetric benefits and costs. Hebrew institutional actors (Ministries of Education, cultural bodies) benefit from resource concentration and prestige elevation. Competing vernacular communities bear costs through educational resource diversion, cultural delegitimization, and pressure to abandon ancestral languages. The extraction trajectory shows accumulation from 1948 to peak around year 45 (approximately 1993, post-Intifada institutional consolidation), then slight decline as globalization and digital communication enable competing language maintenance independent of state support. Suppression (0.48): Moderate. The constraint operates through institutional incentive structures, educational policy, and cultural prestige mechanisms rather than legal prohibition or physical coercion. Yiddish/Ladino communities can technically maintain their languages, but face resource barriers, educational bifurcation, and social prestige costs. Theater ratio (0.35): Moderate-low. The reading is grounded in genuine linguistic science—living languages do require daily native use for intergenerational transmission. Unlike piton constraints (pure performance), this reading has real functional content. However, the selective application of the criterion—measuring Hebrew against the 'native daily' standard while treating liturgical and multilingual practices as lesser—introduces performative elements. The ratio rises slightly as institutional enforcement becomes more explicit and less grounded in organic language vitality.
 *
 * PERSPECTIVAL GAP:
 *   The native-daily reading produces maximum perspectival divergence. Diaspora vernacular communities (identity-locked) see snare: they are excluded from the authenticity criterion and bear extraction with no exit. Multilingual communities (moderate/constrained) see tangled rope: genuine coordination benefits alongside asymmetric costs. Hebrew institutional beneficiaries (institutional/arbitrage) see rope: they experience the reading as legitimate coordination and benefit from resource concentration. Nation-state authority (institutional/mobile) sees tangled rope: coordinates a unified linguistic substrate while extracting from competing traditions. Liturgical custodians see piton: their tradition is marginalized and maintained only through institutional inertia and religious obligation, not through prestige or institutional support. Language preservation movements (organized/constrained) see tangled rope: structured pressure from resource concentration and prestige asymmetries. The analytical observer risks seeing mountain: naturalizing the native-daily criterion as linguistic law rather than political choice. The false-summit detector should identify this as naturalization: the beneficiaries and extraction flows are visible; the criterion is not self-evident from linguistic science (competing readings exist); institutional enforcement is necessary to maintain the hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim status and exit options. Hebrew institutions occupy beneficiary + arbitrage positions, producing d ≈ 0.10-0.20 → low/negative χ (they benefit). Diaspora vernacular communities occupy victim + identity_locked positions, producing d ≈ 0.90 → high χ (they bear maximum extraction). Multilingual communities occupy victim + constrained positions, producing d ≈ 0.70 → high χ. Nation-state authority occupies beneficiary + mobile position, producing d ≈ 0.25 → low χ. Liturgical custodians occupy marginal victim + constrained position, producing d ≈ 0.60. Language preservation movements occupy victim + constrained + organized position, producing d ≈ 0.55 (organization reduces d slightly). The perspectival gaps correspond to these directionality differences: high-d agents (identity-locked victims) see snare; medium-d agents see tangled rope; low-d agents (beneficiaries) see rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the native-daily reading is 'establish Hebrew as living vernacular to enable nation-building and multilingual integration.' This mandate was vital during the Yishuv period (1880s-1948) when Hebrew was genuinely marginal and competing with Yiddish, Ladino, and European languages for dominance. Post-1948 Israeli society achieved the mandate: Hebrew is now the overwhelming dominant vernacular among Israeli citizens and diaspora communities with institutional engagement. The mandate has outlived its functional urgency. However, the institutional infrastructure (education policy, cultural prestige allocation, diaspora funding mechanisms) persists in its fully enforced form, even as the founding problem (establishing Hebrew dominance) has been solved. The reading remains classified as tangled_rope rather than piton because some genuine coordination function persists (multilingual Israeli society still benefits from shared linguistic substrate), but the mandatrophy is real and visible: the constraint is maintained at full force to suppress alternatives that pose no realistic competitive threat. Yiddish, Ladino, and multilingual identification are no longer viable challengers to Hebrew dominance; yet the native-daily criterion continues to legitimate resource diversion and educational bifurcation as if the foundational threat remained. A mandatrophy-resolved version would maintain Hebrew education and coordination functions while legitimizing competing traditions as optional enrichments rather than threats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nativity_authenticity_threshold,
    'What constitutes ''native'' speaker status for purposes of determining if Hebrew is living? Birth in Hebrew-speaking community? Childhood acquisition? Generational depth?',
    'Longitudinal demographic analysis of Hebrew competence by acquisition pathway; measurement of whether second-language learners (Ulpan graduates, immigrant adults) can generate novel vernacular utterances at rates comparable to first-language natives; analysis of actual speech communities (kibbutzim, urban Israel) to identify threshold where non-native speakers predominate.',
    'If ''native'' requires childhood acquisition: most Israeli Hebrew speakers (especially post-1980s immigrants'' children) fail authenticity test; reading collapses into performative maintenance (piton). If ''native'' permits adult acquisition: reading holds but loses distinctiveness from hybrid readings; extraction mechanism weakens. If ''native'' is generational (grandparent fluency): reading is extremely stringent and likely creates permanent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nativity_authenticity_threshold, conceptual, 'Definition of ''native speaker'' and its temporal/generational scope').

omega_variable(
    daily_vernacular_scope_ambiguity,
    'Does ''daily vernacular'' include Hebrew used in institutional contexts (schools, courts, government), or only domestic/informal speech? Do liturgical or academic registers count?',
    'Ethnographic documentation of Hebrew usage domains; analysis of which registers sustain the language; measurement of whether institutional-only Hebrew (without domestic use) constitutes ''living'' language status.',
    'If institutional contexts count: most Hebrew speakers qualify; extraction is lower; reading is closer to hybrid. If only domestic informal use counts: many fluent speakers fail test; extraction is higher; reading maintains distinctiveness but may be empirically unfalsifiable (private speech is hard to verify). If academic/literary registers excluded: Israel''s Hebrew literary culture is delegitimized as ''not really living.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(daily_vernacular_scope_ambiguity, conceptual, 'What speech domains/registers constitute ''daily vernacular''').

omega_variable(
    kernel_reading_contestation,
    'Is this reading (native daily) a discovered fact about Hebrew vitality, or a normative choice among competing readings of what makes Hebrew ''living''?',
    'Historical analysis of reading emergence: when did ''native daily'' become the standard definition of living language (not during classical revival, but post-1948 institutionalization)? Comparison of competing readings'' internal coherence and empirical fit. Analysis of whether institutional adoption of this reading was driven by linguistic science or by political nation-building requirements.',
    'If discovered fact: mountain classification is correct; other readings (liturgical, hybrid, multilingual) are false. If normative choice: tangled_rope classification is correct; other readings remain defensible; extraction mechanism is explicitly political. This is the kernel ambiguity: whether the reading describes Hebrew''s nature or Israeli policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether ''native daily'' is discovered linguistic reality or normative political choice').

omega_variable(
    intergenerational_transmission_sufficiency,
    'Does Hebrew generational transmission (parent-to-child, across full childhood) actually occur at rates sufficient to claim ''living'' status, or does institutional support disguise fragility?',
    'Longitudinal cohort studies of Hebrew transmission pathways; measurement of parental Hebrew fluency by immigration generation; analysis of code-switching patterns and dominance relations in families; comparison with uncontroversially living languages (English in USA, Mandarin in China) on the same metrics.',
    'If transmission is robust: reading stands; piton classification is wrong. If transmission is institutional-dependent: reading naturalizes what is actually engineered vitality; true piton or snare classification emerges; the constraint maintains Hebrew only as long as enforcement continues.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_transmission_sufficiency, empirical, 'Whether natural intergenerational transmission sustains Hebrew or institutional support is necessary').

omega_variable(
    competitive_vernacular_vitality,
    'Do Yiddish and Ladino remain genuinely ''living'' languages within diaspora communities, or are they also institutional preservations without daily vernacular speakers?',
    'Demographic analysis of Yiddish/Ladino daily speakers among diaspora communities; measurement of intergenerational transmission rates; comparison of their structural vitality with Hebrew''s on same metrics.',
    'If competing vernaculars are equally institutional/fragile: native-daily reading treats all three symmetrically; victims classification is weakened; extraction mechanism is shared maintenance burden, not asymmetric suppression. If competing vernaculars are more vernacularly robust: native-daily reading''s exclusion of them is false; reading is arbitrary choice among equals, not truth claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_vernacular_vitality, empirical, 'Vitality of competing diaspora vernaculars by same criteria as Hebrew').

omega_variable(
    kernel_sibling_reading_incompatibility,
    'Can the native-daily reading coexist logically with the liturgical reading within a single commitment framework (single nation-state, single educational system), or does adoption of one reading necessarily foreclose the other?',
    'Analysis of bilingual/diglossia language systems that maintain multiple registers (classical and vernacular) in same population. Historical comparison: did pre-1948 Yishuv maintain both readings? Post-1948, did institutional policy choices make them incompatible, or were they always logically foreclosed?',
    'If logically foreclosing: reading_relation is forecloses (rare, strong claim). If institutionally competitive but logically compatible: reading_relation is coexists_with. If this reading creates structural downstream pressure but doesn''t rule out liturgical reading: reading_relation is influences. The classification determines how the sibling readings are computed by the engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_reading_incompatibility, conceptual, 'Logical vs. institutional compatibility of native-daily and liturgical readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_daily_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndr_tr_t0, native_daily_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ndr_tr_t30, native_daily_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(ndr_tr_t60, native_daily_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(ndr_be_t0, native_daily_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ndr_be_t15, native_daily_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(ndr_be_t30, native_daily_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(ndr_be_t45, native_daily_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(ndr_be_t60, native_daily_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ndr_su_t0, native_daily_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(ndr_su_t15, native_daily_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(ndr_su_t30, native_daily_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(ndr_su_t45, native_daily_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement(ndr_su_t60, native_daily_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_daily_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(native_daily_reading, 0.12).
narrative_ontology:affects_constraint(native_daily_reading, hebrew_liturgical_reading).
narrative_ontology:affects_constraint(native_daily_reading, hebrew_hybrid_reading).
narrative_ontology:affects_constraint(native_daily_reading, yiddish_language_vitality).
narrative_ontology:affects_constraint(native_daily_reading, ladino_language_vitality).
narrative_ontology:affects_constraint(native_daily_reading, diaspora_jewish_identity_consolidation).

% DUAL FORMULATION NOTE:
% The native-daily reading is part of a constraint family decomposed from the contested kernel hebrew_living_language. Each reading (native-daily, liturgical, hybrid) instantiates the kernel differently and produces distinct ε values: the native-daily reading has moderate extractiveness (0.52) because it bundles genuine coordination with resource suppression; the liturgical reading would show lower extractiveness (institutional maintenance with minimal extraction from other traditions); the hybrid reading would show lowest extractiveness (coordination without enforced hierarchy). These are NOT the same constraint measured differently—the ε-invariance principle applies. They are distinct structurally because the victim sets differ (native-daily reading targets diaspora vernaculars; liturgical reading targets modern Hebrew development; hybrid reading targets no single tradition as victim). Each reading should be analyzed in its own story with its own beneficiary/victim declarations and network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(native_daily_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
