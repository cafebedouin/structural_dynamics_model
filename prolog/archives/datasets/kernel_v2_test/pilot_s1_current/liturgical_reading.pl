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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: liturgical_reading
 *   human_readable: Hebrew Liturgical Continuity: Language Living Through Sacred Recitation
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'hebrew_living_language': the claim that Hebrew remains a living language
 *   because it is continuously recited in liturgical contexts, regardless of
 *   whether it has native daily speakers. This reading is distinct from the
 *   native-speaker reading (Hebrew is living because children acquire it as a
 *   first language) and the hybrid reading (Hebrew is living through
 *   coordination of liturgical preservation, academic study, and secular
 *   revival). The liturgical reading restricts 'aliveness' to the
 *   coordination function that sacred recitation performs: maintaining a
 *   continuous textual tradition, transmitting interpretive knowledge across
 *   generations, and sustaining communities organized around Hebrew as a
 *   symbol of identity and continuity. The constraint exhibits low
 *   extractiveness (0.08) because the coordination function is genuine and
 *   the beneficiary set is broad and voluntary. Suppression is minimal
 *   (0.12): alternative languages coexist, exit from liturgical participation
 *   is structurally possible, and no enforcing body prevents alternative
 *   forms of Hebrew or competing languages. Theater ratio (0.35) reflects
 *   that the claim performs cultural and religious work (identity
 *   affirmation, spiritual continuity) beyond its technical linguistic
 *   content — the performative element is modest but real. The key structural
 *   finding is that this reading instantiates a coordination constraint, not
 *   an extraction constraint, and differs in ε from both sibling readings.
 *
 * KEY AGENTS:
 *   - Liturgical Communities: Organized practitioners (organized/mobile) — coordinate around daily/weekly recitation of fixed liturgical texts; benefit from shared meaningful practice and intergenerational connection; low extraction experienced
 *   - Religious Institutions: Institutional stewards (institutional/constrained) — maintain liturgical texts, train leaders, coordinate communal prayer; beneficiaries of legitimacy and cultural authority; extraction minimal
 *   - Hebrew Textual Heritage Stewards: Powerful arbitrageurs (powerful/arbitrage) — scholars, academies, cultural authorities preserving canon; benefit from keeping textual tradition alive; multiple exit paths available
 *   - Individual Non-Native Practitioners: Moderate participants (moderate/constrained) — learn and recite liturgical Hebrew under social/cultural pressure but with genuine religious benefit; moderate extraction via social expectation
 *   - Modern Hebrew Revitalization Movement: Organized transitional agents (organized/constrained) — use liturgical continuity as scaffolding for secular revival; see the constraint as temporary coordinate structure with sunset
 *   - Analytical Observer (Linguist): Detached analyst (analytical/analytical) — observes the constraint as increasingly performative maintenance; sees liveness claim as conflating ritual persistence with linguistic behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_reading, 0.08).
domain_priors:suppression_score(liturgical_reading, 0.12).
domain_priors:theater_ratio(liturgical_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(liturgical_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(liturgical_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_reading, rope).
narrative_ontology:human_readable(liturgical_reading, "Hebrew Liturgical Continuity: Language Living Through Sacred Recitation").
narrative_ontology:topic_domain(liturgical_reading, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liturgical_reading, '347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff').
narrative_ontology:cs_kernel_codification('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', formalized).
narrative_ontology:cs_authority_grounding('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', lineage).
narrative_ontology:cs_interpretation_layer_present('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff').
narrative_ontology:cs_reading_relation('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', liturgical_reading__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', liturgical_reading__hybrid_reading, influences).
narrative_ontology:cs_axiom('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', foundational, textual_recitation_preserves_liveness).
narrative_ontology:cs_axiom_status(textual_recitation_preserves_liveness, holdable).
narrative_ontology:cs_axiom_grounding('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', textual_recitation_preserves_liveness, conventional).
narrative_ontology:cs_axiom('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', foundational, non_native_speaker_preservation_sufficient).
narrative_ontology:cs_axiom_status(non_native_speaker_preservation_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', non_native_speaker_preservation_sufficient, deontological).
narrative_ontology:cs_reference_frame('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', sacred_text_continuity_through_transmission).
narrative_ontology:cs_drift_state('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', post_native_speaker_emergence, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('347cfa1c-cb6c-48af-a0cf-ebc3bd8fbfff', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(liturgical_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(liturgical_reading, hebrew_textual_heritage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liturgical_reading, textual_heritage_scholars).
narrative_ontology:constraint_beneficiary(liturgical_reading, individual_non_native_speakers).
narrative_ontology:constraint_victim(liturgical_reading, individual_non_native_speakers).
narrative_ontology:constraint_vindicates(liturgical_reading, continuity_through_ritual_transmission).
narrative_ontology:constraint_vindicates(liturgical_reading, non_native_speaker_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized groups (synagogues, prayer communities, denominations) that maintain regular liturgical recitation of Hebrew texts (prayers, Torah readings, ritual formulas). These communities set and enforce the linguistic standard for how Hebrew is pronounced and transmitted in ritual contexts. They make decisions about which liturgical texts to include, how to adapt to new generations, and what level of linguistic accuracy to require. Practitioners can join or leave these communities; denominations can reform their liturgies; but within each community, the liturgical text set the standard for what counts as authentic Hebrew recitation.
narrative_ontology:constraint_stakeholder(liturgical_reading, liturgical_prayer_communities, agenda_setter,
    organized, generational, mobile, global).

% Scholars, academies, and cultural authorities who study, preserve, and interpret the Hebrew textual canon (Biblical, Talmudic, liturgical). They benefit from continuous liturgical recitation because it maintains reader capacity and sustains interpretive communities. But they also have the power to pivot between domains: they can focus on modern Hebrew revival, secular literature, historical linguistics, or return to liturgical scholarship. Their authority derives partly from the canon's religious significance and partly from their own scholarly expertise. They can exit the liturgical domain without losing power.
narrative_ontology:constraint_stakeholder(liturgical_reading, textual_heritage_scholars, beneficiary,
    powerful, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liturgical_reading, textual_heritage_scholars, agenda_setter).

% Individual Jews who learn liturgical Hebrew and recite it without it being their primary daily language. They pay a cost: time and cognitive effort to learn a non-native language, social/family pressure to maintain practice even when motivation flags, career opportunity costs for those pursuing religious professional roles. But they also benefit: access to a meaningful sacred practice, belonging in an intergenerational community, connection to a heritage spanning millennia, participation in a language they experience as more 'true' or 'deep' than their native language. Their exit is structurally possible but socially and psychologically costly.
narrative_ontology:constraint_stakeholder(liturgical_reading, individual_non_native_speakers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(liturgical_reading, individual_non_native_speakers, beneficiary).

% The organized movement for Modern Hebrew revitalization (primarily 20th century, but continuing). This movement uses liturgical Hebrew continuity as scaffolding: the preservation of classical texts through centuries of non-native recitation created the knowledge base and textual authority needed to launch native-speaker revival. The movement coordinates education, immigration, legal status, and cultural value around making Hebrew the spoken national language of Israel. Constrained exit: once committed to this project, the resources and institutional machinery can shift (they did), but the historical dependence on liturgical preservation as the foundation remains.
narrative_ontology:constraint_stakeholder(liturgical_reading, modern_hebrew_revival_movement, agenda_setter,
    organized, biographical, constrained, national).

% Not an agent, but represents the absence of voices in the liturgical reading's legitimacy narrative: Yiddish, Ladino, Judeo-Arabic, and other Jewish diaspora languages that coexist with liturgical Hebrew. These languages are NOT suppressed by the liturgical reading in most historical contexts — communities maintained multiple languages. But the liturgical reading's elevation of Hebrew as the canonical living language can create pressure (implicit rather than explicit) toward Hebrew-centrism. The excluded voice is that of alternative linguistic traditions: 'Is Hebrew's liveness more important than Yiddish's active daily use? If liveness is the measure, why focus on preserved classical Hebrew rather than living vernacular alternatives?'
narrative_ontology:constraint_stakeholder(liturgical_reading, alternative_jewish_languages, excluded,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_non_agent(liturgical_reading, alternative_jewish_languages).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining a continuous chain of transmission of fixed liturgical Hebrew texts across generations, enabling communities to access the same canonical prayers and Torah readings in their original language. Without this constraint, the textual tradition would fragment or be lost as diaspora communities adopted local languages.
% TRANSFER_FUNCTION: The constraint transfers knowledge (how to pronounce and interpret liturgical Hebrew), cultural authority (the status and prestige of being able to lead ritual), and spiritual benefit (the meaningful practice of reciting sacred texts). Transfer flows from older to younger generations within communities, from institutional authorities to individual practitioners, and from textual heritage stewards to all who engage with the canon.
% ABSENT_VOICES: Alternative Jewish language communities (Yiddish speakers, Ladino speakers, Judeo-Arabic speakers) who maintained living languages in daily use but are not part of the 'liveness' conversation. These communities might ask: 'Why is preserved classical Hebrew counted as living while our active daily languages are treated as secondary?' Native-speaker Hebrew advocates might ask: 'Why base preservation on non-native ritual when native speakers can keep the language truly alive?' Secular Jews might ask: 'Why privilege religious frameworks for defining language liveness?'
% DISAPPEARANCE_RATIONALE: If the liturgical constraint disappeared overnight — if communities ceased reciting liturgical Hebrew — the world would rearrange significantly. The textual tradition would not immediately vanish, but its institutional transmission would degrade. Interpretive communities would scatter. The cultural foundation of Modern Hebrew native-speaker revival (which depends on the canonical texts' prestige and the reader capacity built through liturgical knowledge) would weaken. However, Modern Hebrew itself would continue (it has independent native-speaker bases), and the texts would persist in written form. The rearrangement would be substantial but not total — the Hebrew language would not 'die,' but the liturgical reading's particular form of liveness would cease, and communities organized around that particular transmission would dissolve or transform.
% FOUNDING_PROBLEM: In the 2000-year diaspora period (from the destruction of the Second Temple to the 19th century), Hebrew had no native speaker communities. But Jewish religious law, identity, and culture centered on Hebrew texts — the Torah, the Talmud, the liturgy. The founding problem was: how can a language with no native speakers be kept alive? How can the knowledge needed to understand and recite sacred texts be preserved across generations when everyone's primary language is Yiddish, Ladino, Arabic, or another diaspora language?
% FOUNDING_PROBLEM_CORROBORATION: Historians and linguists (not just religiously invested communities) confirm that the founding problem was real and urgent: Hebrew was indeed at risk of becoming a dead language, known only to scholars. The problem's death-status is attested by the emergence of native-speaker Modern Hebrew in the 20th century. Once native Hebrew-speaking children existed (in Palestine/Israel), the survival problem was structurally solved. The liturgical reading's historic mandate was completed — not failed, but accomplished. The constraint continues for other reasons (religious meaning, cultural continuity, interpretive depth), but not for the original survival mandate. Corroboration comes from Hebrew language historians (Rabin, Kutscher, Sáenz-Badillos) who document the transition from non-native to native speaker bases.
narrative_ontology:disappearance_verdict(liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(liturgical_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL COMMUNITY (ROPE) — Organized participants in daily liturgical recitation coordinate around a shared Hebrew textual corpus. Coordination function is genuine: maintaining the liturgical text requires distributed knowledge-keeping across generations. Exit is mobile — practitioners can cease participation, adopt alternative liturgies, or shift to secular Hebrew. Benefits from constraint: shared meaningful practice, cultural continuity, access to vast interpretive tradition. Extraction minimal: the constraint does not concentrate resources or suppress alternatives.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: RELIGIOUS INSTITUTIONS (ROPE) — Maintain and transmit liturgical Hebrew across generations. Genuine coordination function: preserving accurate liturgical texts, training liturgical leaders, scheduling communal prayer. Exit is constrained but real — institutions can reform liturgy, adopt different languages, or dissolve. Beneficiary position: institutions derive legitimacy, membership cohesion, and cultural authority from liturgical continuity. Extraction low — no structural suppression of exit or alternatives within the religious landscape.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: HEBREW TEXTUAL HERITAGE STEWARDS (ROPE) — Scholars, academies, and cultural authorities who preserve and interpret the Hebrew canon (Biblical, Talmudic, liturgical texts). Powerful arbitrage position: can pivot between liturgical contexts, secular scholarship, literary revival, or other domains. Benefits from the constraint: liturgical recitation keeps the textual tradition alive, maintains reader capacity across languages, and sustains interpretive communities. Extraction negligible — the constraint does not concentrate resources; multiple exit paths available.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIVIDUAL PRACTITIONERS / NON-NATIVE SPEAKERS (ROPE) — People who learn and recite liturgical Hebrew without it being their daily-use language. Constrained exit: strong cultural and religious pressure to maintain practice; costs of exit include loss of community belonging and family expectation. But also genuine benefit: access to powerful sacred practice, membership in intergenerational community, connection to heritage. Extraction real but not severe — the pressure is social/cultural, not coercive, and alternatives (secular Jewishness, different denominations) exist.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / LINGUIST (PITON) — From a scientific linguistic standpoint, the 'Hebrew is alive because it is recited' claim is increasingly performative maintenance rather than linguistic truth. Living languages are defined by native speaker bases, child-language acquisition, and vernacular innovation — criteria Hebrew liturgical recitation meets only partially. The liveness claim persists through ritual authority and identity investment, not through linguistic behavior. Theater ratio is significant: the claim performs cultural continuity and religious authority as if it were a technical linguistic fact. The piton classification emerges from the gap between the claim's functional purpose (identity affirmation) and its technical content (language definition).
constraint_indexing:constraint_classification(liturgical_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: HEBREW REVITALIZATION MOVEMENT (SCAFFOLD) — The liturgical reading is scaffolding for the larger historical project of Hebrew language revival. In the 20th century, Modern Hebrew emerged from exactly the liturgical base: non-native speakers learning classical texts through daily recitation created the foundation for secular native-speaker revival. This perspective sees liturgical continuity as a temporary coordinate structure with built-in sunset: once native-speaker populations solidify (as occurred in Israel post-1948), the liturgical reading's role becomes less central to language preservation. Low theater: the movement honestly acknowledges that 'living through ritual' was a transition strategy. Constraints arise from resource competition (liturgical vs secular Hebrew education) but exit exists and alternatives are increasingly viable.
constraint_indexing:constraint_classification(liturgical_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: NATURAL-LAW VIEW (MOUNTAIN) — From certain philosophical and theological framings, the idea that sacred texts possess inherent vitality and can remain alive through recitation is treated as an immutable fact — a natural law of textual being. The text, once fixed in the canonical liturgical form, cannot die as long as someone recites it. This perspective sees the constraint as non-contingent, emerging necessarily from the ontological status of sacred language. However, this classification is vulnerable to FSM reclassification: the constraint declares beneficiaries (liturgical communities, textual heritage stewards), and the structural data shows genuine coordination rather than natural emergence. The engine will likely compute this as a false summit — a contingent institutional arrangement (ritual transmission of a fixed text) naturalized as metaphysical law.
constraint_indexing:constraint_classification(liturgical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(liturgical_reading, TR),
    TR >= 0.70.

:- end_tests(liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low, reflecting genuine coordination function with minimal asymmetric benefit concentration. The liturgical communities organize voluntarily around a shared text; religious institutions coordinate rather than coerce; the textual heritage benefits from distributed preservation work. The slight upward drift (0.05→0.10 over interval) reflects modest increasing theater ratio and modest resource-concentration effects as academic study of liturgical Hebrew creates credentialing advantages. Suppression (0.12): Low, indicating minimal suppression of alternatives. Yiddish, Ladino, Judeo-Arabic, and modern Hebrew coexist in communities maintaining liturgical practice. Exit is structurally possible: practitioners can cease recitation, denominations can reform liturgy, institutions can shift language focus. The drift (0.08→0.15) reflects modest increase in educational gatekeeping (not everyone can lead liturgical services) and modest increase in ritual standardization pressure as denominations standardize liturgical pronunciation. Theater ratio (0.35): Moderate, capturing the performative element. The claim that 'recitation keeps a language alive' performs cultural continuity work that exceeds its technical linguistic content. The liveness claim depends partly on a prior definitional choice (to define living broadly enough to include preserved but non-native languages) rather than emerging purely from observable linguistic facts. The drift (0.25→0.40) reflects increasing gap between the claim's cultural role and its technical linguistic validity as linguistics has formalized living-language criteria and as Hebrew has acquired genuine native-speaker populations.
 *
 * PERSPECTIVAL GAP:
 *   Rope vs. Piton: The liturgical community's rope experience (genuine coordination) contrasts sharply with the analytical observer's piton experience (performative maintenance of a liveness claim). This gap reflects the difference between on-the-ground participation (where the coordination is real and beneficial) and external observation (where the definitional work is visible as performance). Rope vs. Mountain: The mountain perspective risks naturalizing what is actually institutional choice. The constraint persists because communities maintain the practice, not because textual recitation is a law of language. This is a classic false-summit risk. Rope vs. Scaffold: The revitalization movement's scaffold perspective sees this constraint as historical scaffolding for a larger project (native-speaker revival). Once native speakers exist, the liturgical reading's role diminishes. This is a temporal perspectival gap — the same constraint looks different at different points in the historical process.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's power level, exit options, and beneficiary/victim relationship to the constraint. Liturgical communities (organized, mobile) experience low d because they are voluntary beneficiaries with exit. Religious institutions (institutional, constrained) experience low-moderate d because they benefit from the constraint and have alternative strategies available. Textual heritage stewards (powerful, arbitrage) experience near-zero d because they have maximum flexibility and multiple domains they can operate in. Individual practitioners (moderate, constrained) experience moderate d because they face social pressure but genuine benefit. The revitalization movement (organized, constrained) experiences moderate d because resources are constrained and institutional alternatives compete. The analytical observer has zero directionality — they are external to the extraction flow. No perspective produces high d (full target status) because no agent bears concentrated costs from the constraint. The absence of victim narratives and the low d across all positions confirms the rope classification at the primary perspective level.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE ANALYSIS: The liturgical reading's original mandate was preservation — to keep Hebrew as a living tradition across generations of exile, when no daily-use speaker communities existed. This mandate was live and urgent for 2000 years: without liturgical continuity, the textual tradition would have been lost. The mandate remains formally true but has partly outlived its primary function. In 1948, the mandate shifted: Modern Hebrew native-speaker communities emerged in Israel, and the language acquired a new basis for liveness independent of liturgical recitation. The liturgical reading was successful at its original mandate (preservation) and enabled the higher mandate (native-speaker revival), but its historic function is now supplementary rather than essential. The constraint has not resolved mandatrophy because it continues to perform real coordination (meaningful religious practice, intergenerational knowledge transmission) even though the survival mandate no longer depends on it. This is a case of successful mandate completion transitioning into ongoing social function — not degradation, but functional shift. The theater ratio's modest increase (0.25→0.40) reflects this shift: the liveness claim increasingly performs cultural and religious identity work (theater) relative to its original survival function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this reading (liturgical continuity) a defensible distinct claim, or does it collapse into the native-speaker reading or the hybrid reading under closer scrutiny?',
    'Historical-linguistic analysis: trace which definition of ''living language'' actually drove Hebrew preservation decisions in different periods (Talmudic era, Diaspora, modernity). Identify which reading''s criteria were operative when communities chose to invest in Hebrew transmission.',
    'If this reading collapses: the three readings are not distinct constraints but different descriptions of the same constraint; reclassify to a single constraint story with multiple perspectives. If this reading stands: it is structurally distinct (coordination around fixed liturgical text vs. coordination around native speaker base vs. hybrid coordination). Affects network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether liturgical reading is a distinct constraint or collapses into another sibling reading').

omega_variable(
    liveness_definition_contingency,
    'Does the liturgical reading depend on a prior commitment about what ''living language'' means, or does the commitment emerge from the liturgical practice itself?',
    'Genealogical analysis of when the term ''living language'' was applied to liturgical Hebrew recitation. Did communities recite the texts because they defined liveness liturgically, or did they coin that definition after recognizing the texts'' cultural role?',
    'If prior commitment: the reading instantiates a theological/definitional axiom (foundational) that may be contestable. If posterior: the reading is post-hoc rationalization of an institutional practice — shifts axiom status toward overridden. Affects commentary.kernel_context and cs_structure.axioms grounding_type assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liveness_definition_contingency, conceptual, 'Whether liveness definition precedes or follows liturgical practice').

omega_variable(
    native_speaker_necessity,
    'Is the non-native-speaker preservation claim coherent as a definition of liveness, or does it depend on eventual native-speaker emergence?',
    'Contrafactual analysis: if Hebrew had remained liturgically preserved for 2000 years with zero native speakers in any period, would the liturgical reading still claim liveness? If not, the reading covertly depends on the native-reading''s outcome.',
    'If coherent alone: this reading stands as a genuine alternative to native-speaker liveness. If covertly dependent: the reading is instrumentally true (useful for bridging to native-speaker revival) but not foundationally true. Affects omega_type_class: shifts from conceptual/preference toward empirical (testable via counterfactual history).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_necessity, conceptual, 'Whether liturgical liveness is independent from native-speaker outcomes').

omega_variable(
    false_summit_detection,
    'Is the mountain classification (textual immutability as natural law) a genuine natural law or a false summit — institutional commitment naturalized as metaphysical fact?',
    'Structural analysis: does the constraint persist because of immutable facts about texts (natural law) or because communities choose to maintain the practice (contingent institutional choice)? If communities could cease recitation and the texts would not ''die,'' it is not a mountain — it is a contingent social fact.',
    'If natural law: mountain classification stands. If false summit: engine reclassifies via false_summit_mountain signature. Determines whether the constraint is invariant across all observer positions or dependent on institutional commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection, empirical, 'Whether mountain classification is natural law or false summit').

omega_variable(
    suppression_ambiguity,
    'Does the constraint suppress alternative language practices, or do multiple languages coexist without suppression pressure?',
    'Historical analysis of language policy in communities maintaining liturgical Hebrew: was Yiddish, Ladino, or Arabic suppressed to maintain Hebrew liturgy, or did communities sustain multiple languages? Contemporary: do communities maintain Hebrew liturgy while accepting secular language shift without conflict?',
    'If suppression is real: reclassify toward tangled_rope (coordination + suppression of alternatives). If suppression is minimal: rope classification confirmed — pure coordination. Affects base_properties.suppression value and potential directionality overrides.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_ambiguity, empirical, 'Whether maintaining liturgical Hebrew suppresses alternative language practices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litur_theater_0, liturgical_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(litur_theater_50, liturgical_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(litur_theater_100, liturgical_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(litur_extractiveness_0, liturgical_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(litur_extractiveness_50, liturgical_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(litur_extractiveness_100, liturgical_reading, base_extractiveness, 100, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(litur_suppression_0, liturgical_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(litur_suppression_50, liturgical_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement(litur_suppression_100, liturgical_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(liturgical_reading, 0.1).
narrative_ontology:affects_constraint(liturgical_reading, native_daily_reading).
narrative_ontology:affects_constraint(liturgical_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct constraints corresponding to three defensible readings of 'liveness.' Each reading has distinct ε, distinct beneficiary/victim structures, and distinct coordination types. The liturgical reading (this file) models the preservation function independent of native speakers. The native-daily reading models the modern Hebrew speaker communities. The hybrid reading models the coordination of all three mechanisms. All three constrain stories link via network.affects_constraints. The upstream liturgical reading influences the hybrid reading (which incorporates it) but does not foreclose the native-daily reading (which coexists as an independent criterion for liveness in modern Israel).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
