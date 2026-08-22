% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Vitality: Native Daily Vernacular Constraint
 *   domain: sociolinguistics/language_revitalization/political
 *
 * SUMMARY:
 *   Between 1880 and 1948, the Zionist movement implemented a reading of
 *   Hebrew's future based on the premise that only native generational
 *   transmission constitutes linguistic vitality—that ritual and liturgical
 *   use, however continuous, represents preservation rather than life. This
 *   constraint subordinated the rabbinical tradition's two-millennia
 *   authority over Hebrew and desacralized the language by moving it from
 *   primarily religious domains into secular governance, education, and
 *   everyday use. The constraint required massive institutional enforcement:
 *   the establishment of Hebrew-medium schools, the creation of new
 *   vocabulary for modern domains, the displacement of Yiddish and Arabic as
 *   vernaculars, and the construction of Hebrew as the symbol of national
 *   belonging. The measurement series track extractiveness rising from
 *   negligible (1880, pre-state period) through rapid institutional
 *   consolidation (1920–1948) to stabilization at a high plateau (1948–2020).
 *   Theater ratio rises alongside: the constraint's enforcement increasingly
 *   depends on cultural narrative (declarations that Hebrew is 'alive' now,
 *   that liturgical Hebrew is 'preserved' but not living, that native
 *   speakers are the authentic practitioners) rather than on the
 *   institutional machinery itself, which by 2020 is so established that its
 *   constructedness is largely invisible. Suppression requirement rises
 *   sharply during state-building and stabilizes—the constraint's persistence
 *   depends on continuous institutional suppression of alternative
 *   definitions (the liturgical reading, diaspora alternatives, Palestinian
 *   Arabic claims to equal linguistic status).
 *
 * KEY AGENTS:
 *   - Zionist state builders (institutional power, setting the terms of what counts as linguistic vitality)
 *   - Secular Hebrew speakers (beneficiaries, positioned as the authentic practitioners of a living language)
 *   - Liturgical tradition keepers (moderate power, identity-locked, bearing the cost of desacralization)
 *   - Diaspora Hebrew communities (moderate power, constrained exit, simultaneously benefiting and paying)
 *   - Palestinian Arabs and Palestinians under occupation (powerless, excluded, subject to the constraint's institutional enforcement)
 *   - Academic linguists (observers, measuring vitality on multiple axes and documenting the constraint's constructed definition)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.68).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.72).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vitality: Native Daily Vernacular Constraint").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/political").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'b4139f74-5d9d-44c3-830f-5efbdf58cc28').
narrative_ontology:cs_kernel_codification('b4139f74-5d9d-44c3-830f-5efbdf58cc28', fixed_text).
narrative_ontology:cs_authority_grounding('b4139f74-5d9d-44c3-830f-5efbdf58cc28', lineage).
narrative_ontology:cs_interpretation_layer_present('b4139f74-5d9d-44c3-830f-5efbdf58cc28').
narrative_ontology:cs_reading_relation('b4139f74-5d9d-44c3-830f-5efbdf58cc28', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('b4139f74-5d9d-44c3-830f-5efbdf58cc28', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('b4139f74-5d9d-44c3-830f-5efbdf58cc28', foundational, linguistic_vitality_requires_native_generation).
narrative_ontology:cs_axiom_status(linguistic_vitality_requires_native_generation, holdable).
narrative_ontology:cs_axiom_grounding('b4139f74-5d9d-44c3-830f-5efbdf58cc28', linguistic_vitality_requires_native_generation, empirically_contingent).
narrative_ontology:cs_axiom('b4139f74-5d9d-44c3-830f-5efbdf58cc28', secondary, secular_institutional_standardization_necessary_for_modernity).
narrative_ontology:cs_axiom_status(secular_institutional_standardization_necessary_for_modernity, holdable).
narrative_ontology:cs_axiom_grounding('b4139f74-5d9d-44c3-830f-5efbdf58cc28', secular_institutional_standardization_necessary_for_modernity, instrumental).
narrative_ontology:cs_reference_frame('b4139f74-5d9d-44c3-830f-5efbdf58cc28', hebrew_continuous_through_liturgy).
narrative_ontology:cs_drift_state('b4139f74-5d9d-44c3-830f-5efbdf58cc28', post_1948_state_institutionalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b4139f74-5d9d-44c3-830f-5efbdf58cc28', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_builders).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, secular_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition_keepers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_hebrew_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, diaspora_hebrew_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built the institutional and pedagogical machinery to revive Hebrew as a living vernacular for daily use in Palestine/Israel. Controlled language policy, education curriculum, media, and cultural production. Declared that liturgical Hebrew—the traditional preserve of the rabbinic and prayer tradition—was insufficient for national vitality and that only native-born speakers using Hebrew for everyday communication, creative production, and governance constituted true language revival. This position subordinated the liturgical tradition's authority over Hebrew's definition and future.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_builders, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained a living, vernacular language of daily use, creative expression, and collective belonging. The constraint's enforcement created institutional pathways (schools, media, literature, governance) that made native Hebrew acquisition and use the default path to social belonging and economic participation in the emerging Israeli state. They benefit from being positioned as the authentic practitioners of a 'revived' language, with cultural and political authority over its meaning.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, secular_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% The custodians of rabbinic Hebrew, prayer language, textual interpretation, and religious practice across Jewish diaspora communities. The native-daily-reading constraint subordinates their claim that unbroken liturgical use constitutes the truest form of Hebrew vitality. They bear the cost of desacralization—their Hebrew is relegated to 'preservation' rather than 'life,' their religious authority over language is displaced by secular state authority, and their own native use of Hebrew (in prayer, study, religious discourse) is reframed as 'ritual recitation' rather than authentic living language. Their exit is blocked by the identity-fusion of Hebrew with Jewish religious practice across centuries.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition_keepers, payer,
    moderate, civilizational, identity_locked, global).

% Pre-state and contemporary diaspora communities that maintained Hebrew literacy and daily/weekly use through educational and religious institutions, independent of state coordination. The native-daily-reading constraint implicitly devalues their Hebrew practice as insufficiently 'native' or 'generational'—they are constructed as inheriting rather than creating. They also benefit from the constraint's institutional success in making Hebrew a recognizable global language with literary, media, and cultural prestige. Their exit options are constrained: maintaining Hebrew without the state's educational and cultural apparatus is resource-intensive; abandoning it means severing ties to both the national movement and the religious tradition.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_hebrew_communities, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, diaspora_hebrew_communities, beneficiary).

% Palestinian citizens of Israel and Palestinians in occupied territories are structurally excluded from the native-daily-reading constraint's benefits. The constraint's enforcement via state institutions (Hebrew-dominant schools, Hebrew-only governance and military service for Israeli citizens) operates as a linguistic assimilation mechanism and a mechanism of political marginalization. Their participation would require abandoning Arabic native-generational transmission in favor of Hebrew—the constraint offers no path for linguistic pluralism or co-vitality. They are kept out by the same structural mechanisms that enforce the constraint's definition.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, arab_palestinian_minority, excluded,
    powerless, biographical, trapped, national).

% Study and measure language vitality through empirical indices (native-speaker population, intergenerational transmission, institutional domains of use, lexical expansion, orthographic standardization). They observe that Hebrew's contemporary vitality on multiple empirical indices is real; they also observe that the constraint's definition of vitality as 'native daily use' is a constructed criterion, not an objective discovery, and that liturgical Hebrew demonstrates measurable continuity and functional vitality on other metrics (textual authority, religious institutional support, educational transmission).
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, academic_linguists, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_state_builders).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a shared standard for what constitutes authentic Hebrew and Hebrew vitality: native-generational transmission through secular institutions (schools, media, literature, governance). Coordinated the reconstruction of Hebrew vocabulary for modern domains, the standardization of pronunciation and grammar, and the collective adoption of Hebrew as the language of state, literature, and daily life in a new polity. Without this coordination, Hebrew could have remained primarily liturgical and fragmented across diaspora communities with no unified modern standard.
% TRANSFER_FUNCTION: Transfers linguistic authority from the rabbinic tradition (which held it for two millennia through religious institutional mechanisms) to the secular Zionist state and its cultural institutions. Transfers the definition of 'living language' from unbroken ritual and textual use to native generational transmission. Transfers resources (education funding, media infrastructure, literary prestige, state administrative authority) to secular Hebrew speakers and away from liturgical keepers. Moves the symbolic weight of 'Jewish continuity' from religious practice to national belonging.
% ABSENT_VOICES: Liturgical tradition keepers (present but subordinated, not truly absent). Orthodox and ultra-Orthodox communities that maintained alternative Hebrew usage and resisted the native-daily-reading definition of vitality (present in diaspora, marginalized in state policy). Palestinian Arabs and Palestinians under occupation whose linguistic and national claims are excluded by the constraint's structure. Pre-state diaspora Hebrew communities whose own generational transmission of Hebrew is devalued as insufficient. These voices would argue that liturgical preservation IS vitality, that Hebrew need not choose between religious and secular domains, that linguistic revival need not entail desacralization, and that state-enforced linguistic standardization marginalizes legitimate alternative uses.
% DISAPPEARANCE_RATIONALE: If the native-daily-reading constraint disappeared—if the state ceased to enforce Hebrew as the primary medium of education, governance, and cultural production—Hebrew would not disappear, but the world would rearrange around it. The state apparatus that gave Hebrew demographic reach, lexical standardization, and cultural prestige would cease. Liturgical Hebrew and diaspora Hebrew communities would retain their continuity; Hebrew might return to a primarily religious/ethnic language with strong prestige but smaller native-speaker base; the Israeli state would require a new language policy, possibly bilingual or multilingual; the cultural authority to define 'Hebrew vitality' would shift. The constraint's persistence depends on active state institutional maintenance.
% FOUNDING_PROBLEM: At the turn of the 20th century, Hebrew was primarily a liturgical and scholarly language: the language of prayer, Jewish religious texts, and educated Jewish discourse, but not of daily household communication or economic production for the vast majority of Jews. The Zionist movement sought to build a modern nation-state in Palestine and needed a unifying language. The founding problem was: how to convert a religious language with unbroken continuity but limited vernacular use into a language capable of expressing modernity—of managing a state, conducting science and technology, raising children, conducting commerce—while mobilizing the symbolic and historical authority Hebrew carried as the language of Jewish texts and tradition. The constraint arose from the premise that 'real' language vitality required native generational transmission, not ritual preservation.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians and state planners (Eliezer Ben-Yehuda, David Ben-Gurion, educational authorities) attest the founding problem was real and the native-daily-reading solution was necessary and successful—Hebrew is now spoken natively by millions. Liturgical tradition keepers and diaspora Hebrew scholars attest that the founding problem was posed in misleading terms—Hebrew's ritual preservation already constituted a form of vitality, and the state's solution required suppressing rather than integrating that tradition. Academic linguists outside the Israeli national framework observe both that Hebrew's contemporary vitality is empirically real AND that the constraint's definition of vitality is ideologically constructed and contingent on state institutional support. The founding problem's status remains contested because the two readings of it are incommensurable: one asks 'how to build a modern state language,' the other asks 'how to honor Hebrew's existing vitality while adapting it.'
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at 2020) because the constraint transfers linguistic authority, cultural prestige, and educational resources from the liturgical tradition to the secular state and secular Hebrew speakers. The transfer is not a zero-sum displacement of an equally vital alternative—liturgical Hebrew continues—but rather a redefinition of what counts as vitality, which systematically disadvantages the tradition that held linguistic authority for two millennia. Suppression is higher (0.72) because the constraint's persistence requires active enforcement at the level of institutional policy (education language-medium, state-language law, media production), cultural narrative (the repeated assertion that native speakers are the true custodians), and political power (the state's monopoly on these institutions). Theater is moderate (0.42): the constraint's early enforcement was primarily institutional (building schools, standardizing pronunciation, creating new vocabulary); by 2020, much of the work is cultural-narrative (declarations that Hebrew is vitally alive, that liturgical Hebrew is respected but not living). The measurement series show extractiveness and suppression rising in tandem during the pre-state and early state periods (1880–1970), then stabilizing—the institutional machinery is mature and the definition of vitality has become hegemonic. The theater ratio continues to rise modestly, suggesting that enforcement is increasingly maintained through cultural narrative and less through direct institutional construction. All measurements are authored on a single shared time grid: every metric is assessed at every examined time point (1880, 1920, 1948, 1970, 2000, 2020).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Zionist state builders) and the beneficiaries (secular Hebrew speakers) experience this as a genuine coordination function with a real solution to a real problem: Hebrew needed to become a living vernacular to support a modern nation-state, and the state provided the institutional pathway to accomplish this. The constrained-exit victims (liturgical keepers, diaspora communities) experience the same constraint as extractive: a redefinition of vitality imposed by institutional power that devalues their own native use of Hebrew and subordinates their authority over the language. The excluded seat (Palestinians) experiences it as one mechanism of linguistic and political marginalization among others. The academic observer measures both: Hebrew's demographic vitality and institutional reach are empirically real; the constraint's definition of vitality is ideologically constructed. The engine computes per-seat classifications from the structural data—the Zionist agenda-setter and secular beneficiaries should compute as experiencing genuine coordination; the liturgical keepers and diaspora communities as experiencing extraction masked as coordination; the excluded Palestinian seat should register as experiencing suppression without coordination benefit. This divergence is the point: the same structural arrangement looks like rope to some and tangled rope (or snare with coordination cover) to others.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist state builders: d ≈ 0.0 (full beneficiary). They set the agenda, control the institutional machinery, collect the cultural and political prestige of having revived Hebrew, and benefit from a unified language for state administration and national belonging. They bear minimal cost from the constraint—enforcing it IS their project. Exit is arbitrage: they could define vitality differently if they chose, but the state's power makes their current reading authoritative. Secular Hebrew speakers: d ≈ 0.2 (slight beneficiary, moderate payer). They benefit from being positioned as the authentic practitioners of a living language and gain access to educational and economic opportunities through native Hebrew competence. They also pay a cost: the constraint's enforcement in schools and state institutions requires adopting a particular style and register of Hebrew (secular, modern, national), which may displace or devalue other registers (religious, poetic, intimate, family-specific) that some speakers might prefer. Exit is mobile: secular speakers can learn other languages, adopt other Hebrews, migrate to other linguistic communities. Liturgical tradition keepers: d ≈ 0.85 (near-full target). They bear the cost of desacralization—their Hebrew is redefined as preservation rather than life, their religious authority over the language is displaced, their institutional (rabbinic/yeshiva) control of Hebrew literacy is subordinated to state educational control. They benefit minimally from the constraint (they gain literacy prestige and connection to a national language, but at the cost of religious authority). Exit is identity-locked: rejecting the constraint means rejecting either the Jewish tradition or the broader Jewish polity—neither is available. Diaspora Hebrew communities: d ≈ 0.55 (symmetric, slightly payer). They benefit from the state's institutional support of Hebrew (media, literature, cultural prestige, simplified educational pathways) but pay by having their own native-generational transmission of Hebrew devalued as insufficient—they are positioned as inheritors rather than creators. Exit is constrained: they can maintain Hebrew without the state's support, but it is resource-intensive and culturally devalued. Palestinians under occupation: d ≈ 1.0 (full target, with no coordination benefit). They experience the constraint as suppression without benefit—Hebrew-dominant education and governance institutions as mechanisms of linguistic and political marginalization—and have no exit. This directionality profile is what the engine derives from the authored beneficiary/victim declarations and exit options. The claim that this is a rope (pure coordination) and the metrics that describe it as substantially extractive are held in tension intentionally: the measurement the corpus takes is the gap between the claim and the computed type at each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem status and the disappearance verdict are strategically mismatched, which flags mandatrophy. The founding problem (how to revive Hebrew as a modern vernacular) has achieved its stated goal: Hebrew is demonstrably a living language with millions of native speakers, institutional support, literary and media production, modern vocabulary for all technical domains. The founding problem is substantially SOLVED. Yet the constraint persists at high extractiveness (0.68) and suppression (0.72), and would remain in place if the founding problem vanished. This is mandatrophy: the constraint's original mandate has become obsolete, but the constraint persists as an institutional arrangement whose primary function is no longer coordination but extraction and the maintenance of authority relations. A genuine rope would vanish or downshift dramatically once its coordination function was satisfied; this constraint persists because the extraction and the authority transfer (from liturgical keepers to secular state builders) constitute the ongoing function, not the original coordination task. The theater ratio (0.42 at 2020) is moderate rather than very high, which suggests the constraint still contains some institutional coordination content—the state still maintains Hebrew education and language policy partly for communication efficiency. But the ratio's upward trend (from 0.05 in 1880 to 0.42 by 2020) indicates that theatrical maintenance is increasing relative to functional necessity, consistent with degradation toward piton. The constraint's persistence is maintained by: (1) the continued institutional power of the state, which benefits from linguistic uniformity and cultural authority; (2) the continued political utility of Hebrew as a symbol of national belonging; (3) the accumulated sunk cost of having built this system (schools, curricula, media, literary canon); (4) the hegemonic acceptance of the native-daily-reading definition, which makes alternative readings invisible to those who benefit from the constraint. The constraint does not meet piton's profile (most actors are being harmed and would gladly fix it if the cost were low—piton requires diffuse costs and no concentrated beneficiary). It meets tangled_rope profile durably: genuine coordination function (creating a unified modern language) coexists with asymmetric extraction (linguistic authority transfer), and the extraction persists through continued institutional enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_contingency,
    'Is linguistic vitality a discoverable empirical fact about languages, or is it a normative criterion constructed by those who define it?',
    'Examine whether different linguistic communities and traditions employ different, internally coherent definitions of vitality without contradiction; compare Hebrew''s vitality profile under the native-daily-reading criterion versus the liturgical-preservation criterion on empirical metrics like continuity, intergenerational transmission, lexical innovation, and speaker confidence. If different communities using different definitions produce different but non-contradictory assessments, vitality is partly constructed.',
    'If vitality is constructed (not discovered), the constraint''s authority rests on institutional power to enforce a definition, not on objective linguistic fact. This would move the constraint from rope (discovering and solving a real problem) toward snare (imposing a definition that subordinates alternatives). If vitality is objective, the constraint''s definition correctly identifies the real requirements for Hebrew''s continuation and the engine''s classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_definition_contingency, conceptual, 'Whether linguistic vitality is an objective property or a constructed criterion').

omega_variable(
    liturgical_tradition_suppression_mechanism,
    'Is the measured suppression of liturgical tradition-keepers structural (external barriers to maintaining Hebrew through religious institutions) or internalized (tradition-keepers have absorbed the native-daily-reading definition and devalue their own practice)?',
    'Survey and interview tradition-keepers about their perception of Hebrew vitality; compare communities in diaspora (external barriers weak, state apparatus absent) versus communities in Israel (state institutional pressure present, educational system assimilative). Track changes in religious institutions'' Hebrew pedagogy and confidence when they operate without state suppression pressure versus under it.',
    'If suppression is primarily structural (state policy, education system), removing the constraint would restore liturgical keepers'' institutional capacity and confidence relatively quickly. If suppression is internalized (absorbed definition of vitality), the constraint''s abandonment would not immediately restore their practice—the constraint has sunk into identity and pedagogy. The measured suppression value (0.72) represents total effective suppression; decomposing it into structural and internalized components would inform whether the constraint is merely institutional or has become cultural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_tradition_suppression_mechanism, empirical, 'Structural versus internalized suppression of liturgical tradition maintenance').

omega_variable(
    reading_boundary_empirical_test,
    'Do the liturgical-reading and native-daily-reading truly foreclose each other (as the native-daily reading asserts), or can they coexist in principle?',
    'Test whether a community can simultaneously (a) practice Hebrew daily in secular contexts with native generational transmission AND (b) practice Hebrew in liturgical/religious contexts with ritual authority and continuous textual use, treating both as legitimate forms of vitality. Look for communities that do this in practice (e.g., some diaspora Jewish communities, some Orthodox communities in Israel that use both registers). If such communities exist stably, the readings coexist rather than foreclose.',
    'If the readings truly coexist, the native-daily-reading''s claim that only native generation constitutes vitality is a normative choice, not a logical necessity. This supports the conceptual omega (vitality is constructed) and suggests the constraint''s classification shifts from rope (discovering a real problem) toward tangled_rope with stronger extractive character (imposing a particular definition against an equally viable alternative). If one reading does foreclose the other empirically, the constraint''s logic is sound and the classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_empirical_test, empirical, 'Whether native-daily and liturgical readings logically foreclose or can coexist').

omega_variable(
    palestinian_linguistic_exclusion_coupling,
    'Is the Palestinian Arabic linguistic marginalization a necessary consequence of the native-daily-reading constraint, or a contingent result of how it was enforced?',
    'Examine whether an alternative enforcement of the same constraint (native Hebrew vitality as primary) could have maintained space for Palestinian Arabic vitality and institutional support (bilingual education, equal media prestige, government services in both languages). Compare Israel''s actual language policy with counterfactual policies consistent with native-daily-reading that would not exclude Arabic. If coexistence is structurally possible, the exclusion is not necessary to the constraint''s core; if impossible, native-daily-reading''s enforcement inherently requires Palestinian linguistic suppression.',
    'If exclusion is contingent, the constraint could be modified (redefined to allow linguistic pluralism) without abandoning the native-daily-reading reading. If exclusion is necessary, the constraint''s extraction from Palestinians is structural and not remedial by policy alone. This affects the classification''s robustness: a constraint that necessarily produces exclusion-by-design is more extractive and less remediable than one where exclusion resulted from policy choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_linguistic_exclusion_coupling, conceptual, 'Whether Palestinian linguistic exclusion is necessary to or contingent on the native-daily-reading constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1880, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__native_daily_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_vitality__native_daily_reading, theater_ratio, 1948, 0.32).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_vitality__native_daily_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__native_daily_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__native_daily_reading, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__native_daily_reading, base_extractiveness, 1880, 0.12).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement(hebr_be_t1948, hebrew_vitality__native_daily_reading, base_extractiveness, 1948, 0.62).
narrative_ontology:measurement(hebr_be_t1970, hebrew_vitality__native_daily_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__native_daily_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__native_daily_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_vitality__native_daily_reading, suppression_requirement, 1880, 0.15).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement(hebr_su_t1948, hebrew_vitality__native_daily_reading, suppression_requirement, 1948, 0.68).
narrative_ontology:measurement(hebr_su_t1970, hebrew_vitality__native_daily_reading, suppression_requirement, 1970, 0.71).
narrative_ontology:measurement(hebr_su_t2000, hebrew_vitality__native_daily_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__native_daily_reading, suppression_requirement, 2020, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1880, tn=2020
narrative_ontology:measurement(hebr_grid_01, hebrew_vitality__native_daily_reading, accessibility_collapse(class), 1880, 0.05).
narrative_ontology:measurement(hebr_grid_02, hebrew_vitality__native_daily_reading, accessibility_collapse(class), 2020, 0.61).
narrative_ontology:measurement(hebr_grid_03, hebrew_vitality__native_daily_reading, accessibility_collapse(individual), 1880, 0.08).
narrative_ontology:measurement(hebr_grid_04, hebrew_vitality__native_daily_reading, accessibility_collapse(individual), 2020, 0.65).
narrative_ontology:measurement(hebr_grid_05, hebrew_vitality__native_daily_reading, accessibility_collapse(organizational), 1880, 0.12).
narrative_ontology:measurement(hebr_grid_06, hebrew_vitality__native_daily_reading, accessibility_collapse(organizational), 2020, 0.72).
narrative_ontology:measurement(hebr_grid_07, hebrew_vitality__native_daily_reading, accessibility_collapse(structural), 1880, 0.1).
narrative_ontology:measurement(hebr_grid_08, hebrew_vitality__native_daily_reading, accessibility_collapse(structural), 2020, 0.58).
narrative_ontology:measurement(hebr_grid_09, hebrew_vitality__native_daily_reading, resistance(class), 1880, 0.7).
narrative_ontology:measurement(hebr_grid_10, hebrew_vitality__native_daily_reading, resistance(class), 2020, 0.58).
narrative_ontology:measurement(hebr_grid_11, hebrew_vitality__native_daily_reading, resistance(individual), 1880, 0.55).
narrative_ontology:measurement(hebr_grid_12, hebrew_vitality__native_daily_reading, resistance(individual), 2020, 0.35).
narrative_ontology:measurement(hebr_grid_13, hebrew_vitality__native_daily_reading, resistance(organizational), 1880, 0.62).
narrative_ontology:measurement(hebr_grid_14, hebrew_vitality__native_daily_reading, resistance(organizational), 2020, 0.48).
narrative_ontology:measurement(hebr_grid_15, hebrew_vitality__native_daily_reading, resistance(structural), 1880, 0.5).
narrative_ontology:measurement(hebr_grid_16, hebrew_vitality__native_daily_reading, resistance(structural), 2020, 0.32).
narrative_ontology:measurement(hebr_grid_17, hebrew_vitality__native_daily_reading, stakes_inflation(class), 1880, 0.08).
narrative_ontology:measurement(hebr_grid_18, hebrew_vitality__native_daily_reading, stakes_inflation(class), 2020, 0.52).
narrative_ontology:measurement(hebr_grid_19, hebrew_vitality__native_daily_reading, stakes_inflation(individual), 1880, 0.15).
narrative_ontology:measurement(hebr_grid_20, hebrew_vitality__native_daily_reading, stakes_inflation(individual), 2020, 0.68).
narrative_ontology:measurement(hebr_grid_21, hebrew_vitality__native_daily_reading, stakes_inflation(organizational), 1880, 0.2).
narrative_ontology:measurement(hebr_grid_22, hebrew_vitality__native_daily_reading, stakes_inflation(organizational), 2020, 0.75).
narrative_ontology:measurement(hebr_grid_23, hebrew_vitality__native_daily_reading, stakes_inflation(structural), 1880, 0.12).
narrative_ontology:measurement(hebr_grid_24, hebrew_vitality__native_daily_reading, stakes_inflation(structural), 2020, 0.48).
narrative_ontology:measurement(hebr_grid_25, hebrew_vitality__native_daily_reading, suppression(class), 1880, 0.1).
narrative_ontology:measurement(hebr_grid_26, hebrew_vitality__native_daily_reading, suppression(class), 2020, 0.65).
narrative_ontology:measurement(hebr_grid_27, hebrew_vitality__native_daily_reading, suppression(individual), 1880, 0.08).
narrative_ontology:measurement(hebr_grid_28, hebrew_vitality__native_daily_reading, suppression(individual), 2020, 0.72).
narrative_ontology:measurement(hebr_grid_29, hebrew_vitality__native_daily_reading, suppression(organizational), 1880, 0.18).
narrative_ontology:measurement(hebr_grid_30, hebrew_vitality__native_daily_reading, suppression(organizational), 2020, 0.78).
narrative_ontology:measurement(hebr_grid_31, hebrew_vitality__native_daily_reading, suppression(structural), 1880, 0.15).
narrative_ontology:measurement(hebr_grid_32, hebrew_vitality__native_daily_reading, suppression(structural), 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__native_daily_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The Hebrew vitality kernel comprises three structurally distinct constraint readings. This story instantiates the native-daily-reading: the assertion that only native generational transmission constitutes linguistic vitality, and that ritual/liturgical use—however continuous—represents preservation rather than life. The sibling constraint hebrew_vitality__liturgical_reading embodies the competing reading that unbroken ritual use constitutes vitality. The sibling hebrew_vitality__hybrid_continuity_reading attempts to integrate both readings by positing that Hebrew's strength lies in maintaining multiple institutional homes and registers. The three stories share a kernel (the commitment to Hebrew's continuity and meaning) but diverge radically on what counts as vitality and whose authority over the language is legitimate. This native-daily-reading produces moderate ε (genuine coordination requirement for state-building coexists with authority transfer from liturgical keepers) and should compute as tangled_rope from the state-builder/secular-speaker seats and snare-flavored from the tradition-keeper seats. The engine's per-seat classification divergence is exactly the point: the constraint is genuinely coordination-producing for some parties and genuinely extractive for others. The ε-invariance principle requires three separate files because the three readings assess the same kernel's state (is it vital or merely preserved?) from incommensurable normative positions, producing different ε values. The liturgical reading sees Hebrew vitality as already present and preserved, with ε ≈ 0.1 (the 'constraint' is the defense of existing vitality, not extraction). This native-daily reading sees Hebrew vitality as requiring reconstruction and enforcement, with ε ≈ 0.68 (substantial extraction from tradition-keepers to fund the reconstruction). The hybrid reading would see both as true—ε somewhere between, with both extraction from tradition-keepers AND genuine coordination accomplished. Each reading has its own assessment of what extraction and coordination are occurring; treating them as observables of a single constraint would violate ε-invariance (the referent—Hebrew's state—is the same; the readings of it are incommensurable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
