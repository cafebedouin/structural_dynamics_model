% ============================================================================
% CONSTRAINT STORY: continuity_narrative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_narrative_reading, []).

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
 *   constraint_id: continuity_narrative_reading
 *   human_readable: Hebrew Revival as Continuity Restoration (Continuity Narrative Reading)
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The continuity narrative reading frames Hebrew's revival as restoration
 *   of a natural linguistic state rather than construction of a new language.
 *   This reading emerged in the late 19th century Zionist movement and became
 *   institutionally dominant in the State of Israel. The constraint
 *   coordinates: it provided the legitimacy structure that enabled mass
 *   adoption of Hebrew as a daily spoken language, solving the collective
 *   action problem of getting a population to abandon their native languages
 *   (Yiddish, Ladino, Arabic, European languages) for a liturgical language.
 *   But it also extracts: historical accuracy is subordinated to nationalist
 *   legitimacy claims, counter-narratives documenting Hebrew's constructed
 *   features face institutional suppression, and alternative Jewish
 *   linguistic traditions are marginalized as 'diaspora corruption' rather
 *   than legitimate continuities. The constraint is a tangled rope from the
 *   analytical perspective — coordination and extraction are structurally
 *   inseparable. The theater_ratio trajectory shows initial rise (1890-1950:
 *   the narrative's scholarly apparatus was built and institutionalized)
 *   followed by modest decline (1950-2010: as Hebrew became established, the
 *   restoration myth required less active maintenance). Extractiveness peaked
 *   mid-century when state institutions enforced the narrative most
 *   aggressively, then declined as Hebrew's status became secure. Suppression
 *   peaked in the state-building era (1920-1960) when alternative narratives
 *   threatened the national project's legitimacy, then stabilized at a high
 *   but non-maximal level.
 *
 * KEY AGENTS:
 *   - Counter-Narrative Scholarship: Primary victim (powerless/trapped) — historians and linguists documenting Hebrew's constructed features face institutional marginalization and funding barriers
 *   - Nationalist Legitimacy Project: Primary beneficiary (institutional/arbitrage) — the continuity narrative delivers cultural authority, international recognition of Hebrew's ancient status, and legitimacy for the state-building project
 *   - Hebrew Language Educators: Mixed position (moderate/constrained) — benefit from institutional support while bearing cost of teaching historically inaccurate origin story
 *   - Diaspora Jewish Communities: Mixed position (organized/constrained) — benefit from Hebrew as unifying language while bearing cost of alternative linguistic traditions' subordination
 *   - Yiddish Cultural Continuity: Secondary victim (powerless/trapped) — Yiddish framed as 'diaspora corruption' rather than legitimate Jewish linguistic tradition; decline partly attributable to restoration logic
 *   - Academic Linguistics Establishment: Piton perspective (institutional/mobile) — maintains theatrical scholarly apparatus despite recognition of Hebrew's constructed features
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_narrative_reading, 0.48).
domain_priors:suppression_score(continuity_narrative_reading, 0.62).
domain_priors:theater_ratio(continuity_narrative_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_narrative_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(continuity_narrative_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(continuity_narrative_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(continuity_narrative_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(continuity_narrative_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_narrative_reading, tangled_rope).
narrative_ontology:human_readable(continuity_narrative_reading, "Hebrew Revival as Continuity Restoration (Continuity Narrative Reading)").
narrative_ontology:topic_domain(continuity_narrative_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(continuity_narrative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_narrative_reading, '212bc744-d03c-4b29-a62a-34850a461397').
narrative_ontology:cs_kernel_codification('212bc744-d03c-4b29-a62a-34850a461397', formalized).
narrative_ontology:cs_authority_grounding('212bc744-d03c-4b29-a62a-34850a461397', lineage).
narrative_ontology:cs_interpretation_layer_present('212bc744-d03c-4b29-a62a-34850a461397').
narrative_ontology:cs_reading_relation('212bc744-d03c-4b29-a62a-34850a461397', continuity_narrative_reading__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('212bc744-d03c-4b29-a62a-34850a461397', continuity_narrative_reading__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('212bc744-d03c-4b29-a62a-34850a461397', foundational, restoration_not_construction).
narrative_ontology:cs_axiom_status(restoration_not_construction, holdable).
narrative_ontology:cs_axiom_grounding('212bc744-d03c-4b29-a62a-34850a461397', restoration_not_construction, empirically_contingent).
narrative_ontology:cs_axiom('212bc744-d03c-4b29-a62a-34850a461397', secondary, liturgical_use_constitutes_living_language).
narrative_ontology:cs_axiom_status(liturgical_use_constitutes_living_language, holdable).
narrative_ontology:cs_axiom_grounding('212bc744-d03c-4b29-a62a-34850a461397', liturgical_use_constitutes_living_language, conventional).
narrative_ontology:cs_reference_frame('212bc744-d03c-4b29-a62a-34850a461397', unbroken_liturgical_transmission).
narrative_ontology:cs_drift_state('212bc744-d03c-4b29-a62a-34850a461397', post_critical_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('212bc744-d03c-4b29-a62a-34850a461397', '').
narrative_ontology:cs_kernel_id(continuity_narrative_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, nationalist_legitimacy_project).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, continuity_claimants).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, institutional_hebrew_establishment).
narrative_ontology:constraint_victim(continuity_narrative_reading, counter_narrative_scholarship).
narrative_ontology:constraint_victim(continuity_narrative_reading, constructed_language_historians).
narrative_ontology:constraint_victim(continuity_narrative_reading, yiddish_cultural_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, hebrew_language_teachers).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(continuity_narrative_reading, counter_narrative_scholars).
narrative_ontology:constraint_victim(continuity_narrative_reading, hebrew_language_teachers).
narrative_ontology:constraint_victim(continuity_narrative_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(continuity_narrative_reading, yiddish_cultural_advocates).
narrative_ontology:constraint_vindicates(continuity_narrative_reading, unbroken_tradition_doctrine).
narrative_ontology:constraint_vindicates(continuity_narrative_reading, natural_restoration_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historians and linguists documenting Hebrew's constructed features — extensive lexical innovation, grammatical borrowing from European languages, phonological reconstruction. Face institutional marginalization in Israeli academia, funding barriers, and accusations of delegitimizing the national project. Cannot exit: their professional identity requires engaging the historical record, but the continuity narrative controls institutional resources and publication venues.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, counter_narrative_scholars, payer,
    powerless, biographical, trapped, national).

% State institutions, cultural ministries, and Zionist organizations that established and maintain the continuity narrative as official doctrine. Set curriculum standards, allocate research funding, control publication venues. The restoration framing delivers cultural authority, international recognition of Hebrew's ancient status, and legitimacy for the state-building project. Can exit freely — have resources and authority to shift narratives if strategic interests change.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, nationalist_legitimacy_architects, agenda_setter,
    institutional, immediate, arbitrage, national).

% Educators teaching Hebrew in schools and ulpanim (language immersion programs). Benefit from the continuity narrative's institutional support: curriculum resources, cultural prestige, stable employment in a valorized profession. But bear the cost of teaching a historically inaccurate origin story — must present Hebrew as restored rather than constructed, minimizing Ben-Yehuda's creative role and the extent of lexical innovation. Constrained exit: leaving Hebrew education means abandoning professional investment and cultural capital.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, hebrew_language_teachers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(continuity_narrative_reading, hebrew_language_teachers, payer).

% Jewish communities outside Israel who adopt Hebrew for religious, cultural, or Zionist reasons. Benefit from Hebrew as a unifying language across dispersed populations and a living connection to ancient tradition. But bear the cost of the continuity narrative's subordination of alternative Jewish linguistic traditions — Yiddish, Ladino, Judeo-Arabic framed as 'diaspora corruption' rather than legitimate continuities. Constrained exit: rejecting the continuity narrative means losing access to Hebrew cultural infrastructure (educational materials, media, institutional support).
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(continuity_narrative_reading, diaspora_jewish_communities, payer).

% Advocates for Yiddish language and culture, which declined precipitously in the 20th century. The continuity narrative's restoration logic frames Yiddish as diaspora corruption — an inauthentic Jewish language that should be replaced by 'restored' Hebrew. This framing contributed to institutional suppression of Yiddish in Israel (banned from schools, marginalized in public life) and cultural devaluation in diaspora communities. Trapped: Yiddish's decline is overdetermined (Holocaust, migration, modernization), but the continuity narrative's delegitimization was a contributing factor with no exit path.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, yiddish_cultural_advocates, payer,
    powerless, generational, trapped, global).

% Academic linguists outside the Israeli institutional context who study Hebrew revival as a case of language planning and construction. Recognize Hebrew's constructed features (extensive neologism, grammatical regularization, phonological reconstruction) but maintain diplomatic relations with Israeli academic institutions. The continuity narrative's scholarly apparatus persists theatrically in international conferences and textbooks despite loss of empirical credibility. Mobile exit: can publish critical scholarship without career risk, but often choose not to challenge the narrative directly to preserve institutional relationships.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, international_linguists, observer,
    institutional, civilizational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The continuity narrative solved a genuine collective action problem: establishing Hebrew as the national language of a multilingual immigrant population required a legitimacy story that connected modern speakers to ancient heritage. The restoration framing enabled mass adoption where explicit construction framing (as with Esperanto) would likely have failed — people were willing to abandon their native languages for a 'restored' ancestral tongue but might not have done so for an acknowledged artificial creation.
% TRANSFER_FUNCTION: The arrangement transfers scholarly legitimacy and institutional resources from counter-narrative historians to continuity-affirming researchers; transfers cultural authority from alternative Jewish linguistic traditions (Yiddish, Ladino) to Hebrew as the 'authentic' Jewish language; transfers international prestige from constructed-language status to ancient-language status.
% ABSENT_VOICES: Yiddish speakers whose language was institutionally suppressed in Israel and culturally devalued in diaspora communities; Mizrahi Jews whose Judeo-Arabic traditions were subordinated to the Ashkenazi-dominated Hebrew revival narrative; linguists documenting Hebrew's constructed features who face funding barriers and accusations of political delegitimization. These voices would contest the 'restoration' framing and the subordination of alternative Jewish linguistic traditions, but they were largely excluded from the institutional spaces where the continuity narrative was established and maintained.
% DISAPPEARANCE_RATIONALE: If the continuity narrative disappeared overnight — if Hebrew were widely acknowledged as a constructed language rather than a restored tradition — multiple arrangements would shift: (1) Hebrew's international prestige would decline (ancient language status carries more cultural capital than constructed language status); (2) institutional resources would flow toward counter-narrative scholarship currently marginalized; (3) alternative Jewish linguistic traditions would gain legitimacy (no longer framed as 'diaspora corruption' to be replaced by 'authentic' Hebrew); (4) language education would shift from restoration mythology to honest historical account of Ben-Yehuda's creative work and the extent of lexical innovation. The world rearranges because the continuity narrative structures resource allocation, cultural prestige, and institutional legitimacy.
% FOUNDING_PROBLEM: The founding problem was legitimacy for mass language adoption in the late 19th and early 20th century Zionist movement. How do you get a multilingual immigrant population (speaking Yiddish, Ladino, Arabic, Russian, German, Polish) to abandon their native languages and adopt a liturgical language for daily secular use? The continuity narrative solved this by framing adoption not as learning an artificial creation but as restoring a natural linguistic state that was interrupted but never broken. The restoration framing provided cultural authority and emotional resonance that explicit construction framing would have lacked.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between two camps: (1) Continuity advocates (Israeli cultural institutions, Hebrew language establishment) claim the problem is LIVE — Hebrew's status as a living language still depends on the restoration narrative's legitimacy, and acknowledging construction would undermine speakers' connection to the language. (2) Critical scholars (international linguists, counter-narrative historians, some Israeli academics) claim the problem is DEAD — Hebrew is now established with millions of native speakers whose daily use does not depend on historical continuity claims; the restoration myth persists through institutional inertia, not functional necessity. Corroboration sources: (a) Ghil'ad Zuckermann (linguist, University of Adelaide) documents Hebrew's constructed features and argues the continuity narrative is no longer necessary for Hebrew's vitality. (b) Israeli Ministry of Education curriculum materials continue to teach the restoration narrative, suggesting institutional commitment to its ongoing necessity. (c) Yiddish cultural organizations (YIVO Institute, National Yiddish Book Center) document how the restoration logic contributed to Yiddish's institutional suppression, providing outside-beneficiary-set corroboration that the narrative's legitimacy function came at a cost.
narrative_ontology:disappearance_verdict(continuity_narrative_reading, world_rearranges).
narrative_ontology:founding_problem_status(continuity_narrative_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COUNTER-NARRATIVE SCHOLARSHIP (SNARE) — Scholars documenting Hebrew's constructed features face institutional marginalization, funding barriers, and accusations of delegitimizing the national project. Cannot exit the suppression — their professional identity requires engaging the historical record, but the continuity narrative controls institutional resources and publication venues. Maximum extraction: the constraint extracts their scholarly legitimacy to preserve the restoration myth.
constraint_indexing:constraint_classification(continuity_narrative_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEBREW LANGUAGE EDUCATORS (TANGLED ROPE) — Teachers benefit from the continuity narrative's institutional support (curriculum resources, cultural prestige, employment) while bearing the cost of teaching a historically inaccurate origin story. Constrained exit: leaving Hebrew education means abandoning professional investment, but staying requires perpetuating the myth. Mixed experience: genuine coordination (language pedagogy infrastructure) entangled with extraction (historical accuracy subordinated to nationalist legitimacy).
constraint_indexing:constraint_classification(continuity_narrative_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATIONALIST LEGITIMACY PROJECT (ROPE) — The continuity narrative solves a genuine coordination problem: establishing Hebrew as the national language required a legitimacy story that connected modern speakers to ancient heritage. From this perspective, the constraint is pure coordination — the 'restoration' framing enabled mass adoption where 'constructed language' framing would have failed. Net beneficiary: the narrative delivers cultural continuity claims, institutional authority, and international recognition of Hebrew's ancient status.
constraint_indexing:constraint_classification(continuity_narrative_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIASPORA JEWISH COMMUNITIES (TANGLED ROPE) — Benefit from Hebrew's revival as a living connection to tradition and a unifying language across dispersed communities, but bear the cost of the continuity narrative's suppression of alternative Jewish linguistic traditions (Yiddish, Ladino, Judeo-Arabic). Constrained exit: rejecting the continuity narrative means losing access to the Hebrew cultural infrastructure, but accepting it means subordinating other Jewish languages to 'restoration' logic. Mixed coordination and extraction across generational time.
constraint_indexing:constraint_classification(continuity_narrative_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC LINGUISTICS ESTABLISHMENT (PITON) — The continuity narrative's scholarly apparatus (etymology tracing unbroken lineages, minimizing neologisms, framing Ben-Yehuda as 'reviver' not 'creator') is maintained theatrically. Linguists outside the nationalist project recognize Hebrew's constructed features but the institutional ritual persists: conferences still frame revival as restoration, textbooks still teach the unbroken tradition story. The function has atrophied (the scholarship no longer convinces neutral observers) but the performance continues through inertia and political pressure.
constraint_indexing:constraint_classification(continuity_narrative_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The continuity narrative genuinely coordinates: it provided the legitimacy structure that enabled Hebrew's adoption as a daily spoken language, solving a real collective action problem (how to get a population to adopt a liturgical language for secular use). But it also extracts: historical accuracy is subordinated to nationalist legitimacy, counter-narratives are suppressed, and alternative linguistic traditions are marginalized. The constraint is structurally a tangled rope — coordination and extraction are inseparable, not a cover story over pure extraction.
constraint_indexing:constraint_classification(continuity_narrative_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_narrative_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(continuity_narrative_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(continuity_narrative_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(continuity_narrative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(continuity_narrative_reading, TR),
    TR >= 0.70.

:- end_tests(continuity_narrative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The continuity narrative extracts from counter-narrative scholarship (institutional marginalization, funding barriers) and from alternative Jewish linguistic traditions (Yiddish, Ladino subordinated to 'restoration' logic). But extraction is not maximal — the narrative genuinely solved a coordination problem (mass adoption of Hebrew required a legitimacy story), and some space exists for critical scholarship in international venues. The value reflects real extraction that is structurally entangled with genuine coordination, not pure cover story. Suppression (0.62): High. Institutional enforcement includes: curriculum control (textbooks teach unbroken tradition), funding allocation (grants favor continuity-affirming research), publication barriers (Israeli academic presses marginalize counter-narratives), and political pressure (accusations of delegitimizing the national project). But suppression is not total — international scholarship documents Hebrew's constructed features, and some Israeli scholars publish critical work. Theater_ratio (0.58): Moderate-high. The scholarly apparatus tracing unbroken lineages and minimizing neologisms is substantially performative — neutral linguists recognize Hebrew's constructed features, but the institutional ritual persists. The theater has stabilized rather than continuing to rise, as Hebrew's status is now secure enough that the restoration myth requires less active maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The continuity narrative produces a clear perspectival gap. The nationalist legitimacy project sees pure coordination (Rope) — the restoration framing enabled mass adoption where constructed-language framing would have failed. Hebrew educators see tangled rope — they benefit from institutional support while bearing the cost of perpetuating historical inaccuracy. Counter-narrative scholars see pure extraction (Snare) — their professional legitimacy is extracted to preserve the myth, with no exit option. Diaspora communities see tangled rope across generational time — Hebrew unifies but subordinates other Jewish languages. The academic linguistics establishment sees degraded ritual (Piton) — the scholarly apparatus persists theatrically despite loss of function. The analytical observer sees tangled rope — coordination and extraction are structurally inseparable, not a cover story masking pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position. The nationalist legitimacy project is the primary beneficiary — the continuity narrative delivers cultural authority, legitimacy claims, and international recognition. Low d, negative chi: the constraint runs toward this agent. Counter-narrative scholarship is the primary victim — institutional marginalization and funding barriers extract their scholarly legitimacy. High d, high chi: the constraint runs away from this agent. Hebrew educators are mixed — they benefit from institutional support (beneficiary component) but bear the cost of teaching historical inaccuracy (victim component). Moderate d, moderate chi. Diaspora communities are mixed across time — immediate benefit (Hebrew as unifying language) entangled with generational cost (alternative linguistic traditions subordinated). Yiddish cultural continuity is a secondary victim — the restoration logic frames Yiddish as diaspora corruption rather than legitimate continuity. The academic linguistics establishment has mobile exit options and maintains the theatrical apparatus without bearing significant cost — low d despite institutional role.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity narrative does not exhibit mandatrophy in the classical sense (mandate outliving function) because its function persists: Hebrew remains a living language whose speakers benefit from the cultural continuity claims the narrative provides. However, the constraint exhibits a related pattern: the narrative's scholarly apparatus (etymology tracing unbroken lineages, minimizing neologisms) has become substantially theatrical (piton from the academic linguistics perspective) while the legitimacy function remains active (rope from the nationalist perspective). This is not mandatrophy but rather functional stratification — the scholarly layer has atrophied while the political layer persists. The tangled rope classification from the analytical perspective captures this: genuine coordination (legitimacy enabling adoption) structurally entangled with extraction (historical accuracy subordinated, counter-narratives suppressed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the continuity narrative reading the only coherent framing of Hebrew revival, or do sibling readings (liturgical, native daily) instantiate structurally distinct constraints with different beneficiary/victim sets?',
    'Cross-reading analysis: compare beneficiary/victim structures, extractiveness values, and suppression mechanisms across the three readings. If ε values differ substantially (>0.2), the readings are distinct constraints per ε-invariance.',
    'If readings are ε-invariant: the kernel is a single constraint viewed from different angles (collapse to one story). If readings have distinct ε: the kernel is a family of constraints (preserve decomposition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether sibling readings are distinct constraints or perspectival views').

omega_variable(
    constructed_vs_revived_threshold,
    'What degree of lexical innovation, grammatical regularization, and phonological reconstruction distinguishes ''revival'' from ''construction''?',
    'Comparative analysis of language revival cases: Irish, Cornish, Manx (acknowledged revivals with substantial construction). Quantify: percentage of modern Hebrew vocabulary absent from Biblical/Mishnaic sources; grammatical structures borrowed from European languages; phonological features lost and reconstructed.',
    'If threshold is low (>30% innovation = construction): continuity narrative is false, extraction is higher. If threshold is high (>70% innovation required): continuity narrative is defensible, extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_revived_threshold, empirical, 'Empirical threshold for revival vs construction distinction').

omega_variable(
    legitimacy_counterfactual,
    'Would Hebrew have achieved the same adoption rate and cultural status if framed explicitly as a constructed language rather than a restored tradition?',
    'Historical counterfactual analysis: compare adoption trajectories of explicitly constructed languages (Esperanto, Ido) vs revived languages with continuity narratives (Hebrew, Irish). Control for state support, population size, and cultural prestige.',
    'If constructed framing would have succeeded: the continuity narrative''s extraction is unjustified (pure cover story). If restoration framing was necessary: the narrative''s coordination function is genuine (tangled rope confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_counterfactual, preference, 'Whether restoration framing was necessary for adoption').

omega_variable(
    yiddish_suppression_attribution,
    'Is Yiddish''s decline attributable to the continuity narrative''s ''restoration'' logic (Hebrew as the authentic Jewish language), or to independent sociolinguistic forces (Holocaust, migration, modernization)?',
    'Causal decomposition: compare Yiddish decline trajectories in Israel (where continuity narrative was institutionally enforced) vs diaspora communities (where it was not). Control for Holocaust losses and migration patterns.',
    'If decline is narrative-driven: Yiddish cultural continuity is a victim of this constraint (extraction confirmed). If decline is independent: Yiddish is not a victim of this specific constraint (remove from victims list).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yiddish_suppression_attribution, empirical, 'Causal attribution of Yiddish decline to continuity narrative').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the continuity narrative itself (the claim that revival was restoration), or is it the broader Hebrew-as-living-language commitment that the narrative serves?',
    'Structural analysis: if the kernel is the narrative, then challenging the narrative challenges the kernel (authority_erosion). If the kernel is the living-language commitment, then the narrative is an interpretation layer that can drift without destabilizing the kernel (practice_drift).',
    'If kernel = narrative: drift_state.direction should be authority_erosion (the narrative''s empirical claims are contested). If kernel = living-language commitment: drift_state.direction should be practice_drift (the narrative is one interpretation among others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is the narrative or the commitment it serves').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_narrative_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_narr_theater_1890, continuity_narrative_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cont_narr_theater_1920, continuity_narrative_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(cont_narr_theater_1950, continuity_narrative_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement(cont_narr_theater_1980, continuity_narrative_reading, theater_ratio, 90, 0.62).
narrative_ontology:measurement(cont_narr_theater_2010, continuity_narrative_reading, theater_ratio, 120, 0.58).

% Extraction over time
narrative_ontology:measurement(cont_narr_extract_1890, continuity_narrative_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cont_narr_extract_1920, continuity_narrative_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cont_narr_extract_1950, continuity_narrative_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(cont_narr_extract_1980, continuity_narrative_reading, base_extractiveness, 90, 0.55).
narrative_ontology:measurement(cont_narr_extract_2010, continuity_narrative_reading, base_extractiveness, 120, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cont_narr_suppress_1890, continuity_narrative_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cont_narr_suppress_1920, continuity_narrative_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(cont_narr_suppress_1950, continuity_narrative_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(cont_narr_suppress_1980, continuity_narrative_reading, suppression_requirement, 90, 0.65).
narrative_ontology:measurement(cont_narr_suppress_2010, continuity_narrative_reading, suppression_requirement, 120, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_narrative_reading, identity_coordination).
narrative_ontology:affects_constraint(continuity_narrative_reading, yiddish_diaspora_corruption_narrative).
narrative_ontology:affects_constraint(continuity_narrative_reading, ben_yehuda_creator_myth).

% DUAL FORMULATION NOTE:
% The continuity narrative reading is one of three sibling readings of the hebrew_living_language kernel. Each reading has distinct ε: continuity_narrative (0.48, moderate extraction from counter-narratives), liturgical_reading (lower ε, less suppression of scholarship), native_daily_reading (lowest ε, minimal legitimacy extraction). The readings are linked via network.affects_constraints and form a constraint family per ε-invariance decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
