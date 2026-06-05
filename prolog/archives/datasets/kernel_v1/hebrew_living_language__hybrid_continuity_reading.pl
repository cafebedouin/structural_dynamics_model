% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__hybrid_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__hybrid_continuity_reading
 *   human_readable: Hebrew Language Continuity Through Textual-Liturgical Hybrid (Revival Reading)
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The hybrid continuity reading of Hebrew language revival asserts that
 *   Modern Hebrew lives through an unbroken textual-liturgical thread that
 *   runs from Biblical and Mishnaic Hebrew through medieval and early modern
 *   diaspora scholarship into contemporary vernacular revival. This reading
 *   occupies the institutional center of Israeli language policy and
 *   mainstream Zionist historiography. It claims that the 20th-century
 *   revival did not invent a new language but rather activated and expanded
 *   the living potential already embedded in 2000+ years of textual
 *   continuity — liturgical Hebrew furnished the vocabulary, grammar, and
 *   cultural authority needed to bootstrap vernacular acquisition and
 *   state-level language planning. The reading is one of three competing
 *   narratives about Hebrew's life: the liturgical preservation reading
 *   claims only sacred recitation and textual study maintained Hebrew across
 *   diaspora; the native vernacular reading claims revival failed and Hebrew
 *   remains fundamentally liturgical, with 'native speakers' actually
 *   acquiring a taught literary variety rather than a genuinely
 *   intergenerational vernacular. The hybrid reading's structural delta is:
 *   kernel occupation (Hebrew's ontological status as 'living') maintained
 *   through the liturgical period, then expanded into vernacular domains
 *   without reachability break. This requires continuous institutional
 *   enforcement (state language policy, education mandates, cultural
 *   authority claims), produces extractive effects (ultra-Orthodox and
 *   diaspora Hebrew authorities lose exclusive control of Hebrew's canonical
 *   status), and carries irreducible empirical uncertainty about whether the
 *   claimed continuity is historically accurate or a post-hoc narrative
 *   constructed to legitimize a fundamentally innovative political project.
 *
 * KEY AGENTS:
 *   - State Language Institutions (institutional/arbitrage): Primary beneficiary — Ministry of Education, Hebrew Language Academy, public media. Benefits from massive institutional investment in revival being justified through continuity narrative rather than having to justify innovation.
 *   - Ultra-Orthodox Hebrew Authority (organized/constrained): Secondary victim — Haredi yeshiva networks, halakhic authorities lose exclusive control of Hebrew's sacred status and canonical interpretation. Constrained exit: cannot stop revival but can refuse participation and maintain liturgical purity commitments.
 *   - Diaspora Hebrew Scholarship (powerless/trapped): Primary victim — Jewish scholars outside Israel whose authority was grounded in textual mastery and liturgical knowledge face displacement by native speakers with state institutional backing. Trapped: cannot return to pre-revival authority, cannot participate in state vernacular project.
 *   - Language Revitalization Movement (organized/constrained): Secondary beneficiary — educators, intellectuals, community organizers benefit from the continuity narrative providing legitimacy for their work, but lose control once state appropriates the narrative.
 *   - Secular Hebrew Speakers (moderate/constrained): Tertiary beneficiary — native Hebrew speakers born in Israel gain unified national identity and cultural authority, but depend on continuity narratives to legitimize their language as non-political.
 *   - Liturgical Purity Tradition (institutional/arbitrage): Institutional actor with degraded function (piton) — abstract commitment to sacred Hebrew preserved through institutional inertia, increasingly performative in diaspora contexts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__hybrid_continuity_reading, 0.48).
domain_priors:suppression_score(hebrew_living_language__hybrid_continuity_reading, 0.52).
domain_priors:theater_ratio(hebrew_living_language__hybrid_continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__hybrid_continuity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hebrew_living_language__hybrid_continuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hebrew_living_language__hybrid_continuity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__hybrid_continuity_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__hybrid_continuity_reading, "Hebrew Language Continuity Through Textual-Liturgical Hybrid (Revival Reading)").
narrative_ontology:topic_domain(hebrew_living_language__hybrid_continuity_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__hybrid_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__hybrid_continuity_reading, 'b3fb217a-043c-4487-bcaa-e558fb22d1aa').
narrative_ontology:cs_kernel_codification('b3fb217a-043c-4487-bcaa-e558fb22d1aa', fixed_text).
narrative_ontology:cs_authority_grounding('b3fb217a-043c-4487-bcaa-e558fb22d1aa', extraction).
narrative_ontology:cs_interpretation_layer_present('b3fb217a-043c-4487-bcaa-e558fb22d1aa').
narrative_ontology:cs_reading_relation('b3fb217a-043c-4487-bcaa-e558fb22d1aa', hebrew_living_language__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3fb217a-043c-4487-bcaa-e558fb22d1aa', hebrew_living_language__native_vernacular_reading, influences).
narrative_ontology:cs_axiom('b3fb217a-043c-4487-bcaa-e558fb22d1aa', foundational, continuity_maintains_reachability).
narrative_ontology:cs_axiom_status(continuity_maintains_reachability, holdable).
narrative_ontology:cs_axiom_grounding('b3fb217a-043c-4487-bcaa-e558fb22d1aa', continuity_maintains_reachability, empirically_contingent).
narrative_ontology:cs_axiom('b3fb217a-043c-4487-bcaa-e558fb22d1aa', foundational, institutional_authority_transfer_via_continuity).
narrative_ontology:cs_axiom_status(institutional_authority_transfer_via_continuity, holdable).
narrative_ontology:cs_axiom_grounding('b3fb217a-043c-4487-bcaa-e558fb22d1aa', institutional_authority_transfer_via_continuity, instrumental).
narrative_ontology:cs_reference_frame('b3fb217a-043c-4487-bcaa-e558fb22d1aa', diaspora_liturgical_authority_and_textual_mastery).
narrative_ontology:cs_drift_state('b3fb217a-043c-4487-bcaa-e558fb22d1aa', contemporary_state_backed_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b3fb217a-043c-4487-bcaa-e558fb22d1aa', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__hybrid_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__hybrid_continuity_reading, zionist_state_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language__hybrid_continuity_reading, secular_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__hybrid_continuity_reading, hebrew_language_revival_movement).
narrative_ontology:constraint_victim(hebrew_living_language__hybrid_continuity_reading, ultra_orthodox_hebrew_authority).
narrative_ontology:constraint_victim(hebrew_living_language__hybrid_continuity_reading, liturgical_purity_tradition).
narrative_ontology:constraint_victim(hebrew_living_language__hybrid_continuity_reading, diaspora_hebrew_scholarship).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA HEBREW SCHOLAR (SNARE) — Trapped in a framework where the canonicity claim has shifted. Once held authority through textual mastery and liturgical knowledge; now faces displacement by vernacular revival speakers with institutional state backing. No exit: cannot return to pre-revival authority structure, cannot fully participate in state-backed vernacular project (geographic, resource, institutional barriers). Experiences the hybrid continuity claim as appropriation — the claim of unbroken liturgical-vernacular thread redefines what counts as 'Hebrew continuity' in ways that marginalize the diaspora textual tradition.
constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ULTRA-ORTHODOX HEBREW AUTHORITY (TANGLED ROPE) — Organized actors (yeshiva networks, halakhic authorities, Haredi institutions) experience genuine coordination function through liturgical preservation (Hebrew unites prayer, study, and community identity). But the hybrid continuity reading extracts value: by claiming that liturgical Hebrew IS the foundation of vernacular revival, the state-backed revival movement appropriates the Haredi tradition's authority to legitimize secular modernization. The revival movement says 'we are continuing your textual tradition' while systematically redefining Hebrew away from sacred use. Constrained exit: Haredi communities can refuse participation in revival, but cannot stop the appropriation or prevent their young people from learning state-sanctioned secular Hebrew. Active enforcement required: state language policy, school curricula, institutional investment in revival all enforce the hybrid continuity claim against alternative readings.
constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE LANGUAGE INSTITUTIONS (ROPE) — Institutional beneficiary (Ministry of Education, Hebrew Language Academy, public media). Experiences the hybrid continuity claim as pure coordination: 'We inherit the textual tradition and expand it into vernacular domains.' This legitimizes the massive institutional investment in revival without requiring invention of a new language — they can claim continuity with 2000+ years of textual authority. Arbitrage exit: can always pivot to an alternative national identity (European language, Yiddish, English) if revival fails, but the hybrid reading removes that risk by guaranteeing legitimacy through continuity.
constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LANGUAGE REVITALIZATION MOVEMENT (SCAFFOLD) — Organized agents (educators, intellectuals, community organizers) see the hybrid continuity claim as a temporary coordinate: necessary to bootstrap vernacular adoption by framing revival as continuity rather than rupture. The sunset logic is embedded in the claim itself: once Hebrew becomes a native language again (intergenerational transmission fully restored), the need to justify revival through liturgical continuity disappears. The constraint is functional during the transition phase (pre-native speakers) but dissolves as native speakers emerge. Constrained exit: the movement cannot fully control how the continuity claim gets institutionalized — once the state apparatus adopts it, the movement's sunset goals become secondary to institutional inertia.
constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SECULAR HEBREW SPEAKER (TANGLED ROPE) — Moderate power agents (urban, educated, native Hebrew speakers born in Israel post-1948) experience mixed coordination and extraction. They benefit from a unified national language (coordination) but depend on the state-enforced hybrid continuity claim to access legitimacy and cultural authority — without the 'unbroken continuity' framing, their language would appear as a political invention rather than a resurrection. Constrained exit: can theoretically abandon Hebrew for English or other languages, but doing so erases cultural identity and reduces social/professional mobility. The constraint extracts by making Hebrew identity inseparable from state nationalism and continuity narratives.
constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LITURGICAL PURITY TRADITION (PITON) — The abstract institutional commitment to Hebrew as sacred text and ritual language persists despite functional erosion. Liturgical Hebrew remains 'pure' in recitation and prayer but has become increasingly performative in diaspora communities — maintained through institutional inertia and identity attachment rather than through living vernacular function. The tradition's authority is preserved through the hybrid reading (which claims continuity) but its exclusivity is lost. Theater ratio reflects that liturgical preservation now operates as a cultural/identity performance more than as a living language function.
constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, the hybrid continuity reading could appear as an immutable feature of language revitalization itself: all language revivals must claim continuity with a textual/liturgical tradition to gain legitimacy; there is no other way to revive a dead language. This perspective naturalizes the constraint as a structural law of linguistic anthropology. However, the structural data contradicts mountain classification — the constraint exhibits clear extraction, beneficiary/victim asymmetry, and institutional enforcement, indicating a false summit where contingent institutional arrangements are being naturalized as inevitable linguistic laws.
constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__hybrid_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_living_language__hybrid_continuity_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language__hybrid_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_living_language__hybrid_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(hebrew_living_language__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The hybrid continuity claim extracts value from both the liturgical tradition (state-backed revival appropriates liturgical authority to legitimize secular modernization) and from diaspora scholars (institutional shift of canonical authority from textual mastery to native speaker status). However, the extraction is not maximal (not 0.66+ snare territory) because: (1) state institutions genuinely invest massive resources in revival rather than simply extracting rents; (2) the revival movement's stated goal of restoring vernacular use is achievable and beneficial (not purely predatory); (3) the coordination function is real — the continuity claim does enable Hebrew speakers to maintain cultural identity and linguistic unity. The constraint sits at the tangled_rope boundary: genuine coordination (unified language, cultural identity restoration) coexists with asymmetric extraction (authority redistribution from diaspora and ultra-Orthodox toward state institutions). The extraction increases over the 30-year interval (0.28 → 0.48) as the institutional appropriation becomes consolidated and the alternatives (liturgical-only, native-only readings) are gradually marginalized. Suppression (0.52): Moderate-high. The constraint suppresses alternative readings through institutional channels — education policy mandates the hybrid continuity narrative, state media reinforces it, academia rewards scholarship that emphasizes continuity. Suppression is not total because alternative readings maintain institutional advocates (Haredi institutions, diaspora scholarly networks) and can theoretically challenge the hybrid reading in public discourse. However, the state's institutional dominance makes suppression substantial and active. Theater ratio (0.58): Moderate. The hybrid continuity claim contains both functional and performative elements. The functional element: liturgical texts genuinely do provide vocabulary and cultural authority useful for vernacular revival. The performative element: the claim of 'unbroken continuity' obscures the massive innovations, borrowings, and linguistic planning required to create Modern Hebrew — the performance of continuity serves to naturalize what is actually a contingent institutional project. Over the interval, theater ratio rises (0.35 → 0.58) as the constraint becomes institutionalized — once the state has adopted the continuity narrative, the focus shifts from demonstrating continuity's truth to performing and maintaining cultural belief in continuity.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid continuity reading produces the full range of DR types across observer positions. The ultra-Orthodox authority experiences tangled rope (coordination through shared Hebrew commitment, extraction through loss of interpretive authority). The diaspora scholar experiences snare (trapped in institutional displacement, no exit). The state language institution experiences rope (pure coordination, with institutional arbitrage maintaining the boundary). The secular Hebrew speaker experiences tangled rope (benefits from unified language, depends on continuity narrative for legitimacy). The analytical civilizational observer risks seeing mountain (language revival necessarily requires continuity narratives, therefore this constraint is a natural law of linguistics) but the structural data reveals this as a false summit — the mountain classification naturalizes what are contingent institutional choices made by revival ideologues. The perspectival gap is widest between the beneficiary (state institutions seeing coordination) and the victim (diaspora scholars seeing institutional displacement). The gap reveals that the same claim ('Hebrew lives through continuity') means radically different things to different observers: for the state, continuity is a legitimating narrative justifying investment; for diaspora scholars, continuity is an appropriation that erases their intellectual tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value d is determined by the agent's structural relationship to the extraction flow. Beneficiaries (state institutions, secular speakers) have low d — they experience benefits flowing toward them, low f(d), experienced extractiveness trending negative to neutral. Victims (diaspora scholars, ultra-Orthodox authority) have high d — they experience extraction flowing toward others, high f(d), experienced extractiveness trending positive to maximal. The organized ultra-Orthodox have d ≈ 0.50-0.55 (both beneficiary and victim aspects) because they coordinate through Hebrew but lose authority. The powerless diaspora scholar has d ≈ 0.95 (nearly pure target status) because institutional displacement is unidirectional. The analytical observer's d derives from the canonical fallback (analytical power atom) rather than from explicit beneficiary/victim declaration; their perspective is not affected by the directionality override mechanism because analytical contexts have fixed canonical d.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid continuity reading resolves the mandatrophy at the tangled_rope type by demonstrating that Hebrew language revival genuinely coordinates (unifies speakers, restores cultural practice) while simultaneously extracting (appropriates liturgical authority, displaces diaspora scholars, subordinates ultra-Orthodox interpretation). The constraint is neither pure coordination (rope) nor pure extraction (snare) because both functions are structural: the coordination is necessary for the revival to function as a unifying national project, and the extraction is necessary to transfer authority from diaspora and liturgical gatekeepers to state institutions and native speakers. The mandatrophy is resolved by showing that the perspectival gap is the substance, not a failure of analysis. From institutional contexts, the constraint is rope (pure coordination); from victim contexts, it is snare (pure extraction); from mixed contexts, it is tangled_rope (hybrid). The analytical observer who tries to find 'the true type' commits the oracle gap error — there is no view from nowhere that produces a single type. The system answer is the presheaf over all perspectives, and the hybrid continuity reading's power lies in its ability to occupy multiple perspectival positions simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_claim,
    'Is the claim of unbroken textual-liturgical continuity descriptively accurate about Hebrew''s linguistic history, or is it a post-hoc narrative constructed to legitimize the revival?',
    'Comparative historical linguistics: trace phonological, morphological, and syntactic continuities and discontinuities between Biblical Hebrew, Mishnaic Hebrew, Medieval Hebrew, and Modern Israeli Hebrew. Assess whether the ''unbroken thread'' is empirically sustainable or whether revival involved substantial linguistic innovation absent from continuity narratives.',
    'If continuity claim is accurate: the hybrid reading''s legitimacy is strengthened; the constraint becomes coordination mechanism (lower χ). If continuity is narrative: the constraint is primarily extractive (higher χ); the reading is a false natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_rupture_claim, empirical, 'Whether textual-liturgical continuity is historically accurate or post-hoc legitimation').

omega_variable(
    liturgical_authority_exclusivity_loss,
    'Was the loss of liturgical-exclusive Hebrew authority by ultra-Orthodox and diaspora communities a necessary consequence of the hybrid continuity reading, or a contingent political outcome of state-backed revival?',
    'Counterfactual analysis: what would Hebrew language politics look like if the revival had claimed innovation rather than continuity? How did the continuity framing specifically enable state institutions to appropriate liturgical authority? Comparative case: how did Yiddish revival movements frame linguistic authority (did they claim continuity with Biblical Yiddish, or innovation)?',
    'If necessary consequence: the victim set (ultra-Orthodox, diaspora scholars) was inevitable given any revival attempt (constraint is inherent to revitalization). If contingent: the victim set results from the specific choice of continuity framing over innovation framing (constraint is institutional choice, not structural necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_authority_exclusivity_loss, conceptual, 'Whether liturgical authority loss was necessary or contingent on continuity framing').

omega_variable(
    vernacular_reachability_from_liturgical_text,
    'Can vernacular Hebrew actually be reconstructed from liturgical texts and continuity claims, or does the revival''s apparent success depend on non-liturgical sources (borrowing, linguistic universals, European grammar, conscious linguistic planning)?',
    'Linguistic genealogy: trace the origin of Modern Israeli Hebrew vocabulary, grammar, and phonology. Assess what proportion derives from liturgical sources vs other sources (Yiddish, European languages, Semitic cognates, conscious planning by revival ideologues). Compare to the revival movement''s own historical narratives about ''extracting vernacular from sacred text.''',
    'If vernacular is genuinely recoverable from liturgical continuity: the hybrid reading''s structural claim is vindicated (no reachability break). If vernacular depends on non-liturgical sources: the continuity claim is performative (reachability was bridged by external inputs, not by unbroken thread); constraint shifts toward piton/snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vernacular_reachability_from_liturgical_text, empirical, 'Whether vernacular Hebrew can be reconstructed from liturgical continuity').

omega_variable(
    sibling_reading_empirical_status,
    'What is the empirical status of the two sibling readings (liturgical_preservation_reading claims only sacred continuity; native_vernacular_reading claims revival failed and Hebrew remains liturgical-only)? How do observable facts distinguish between the three readings?',
    'Define falsifiability criteria for each reading: liturgical preservation reading predicts Hebrew remains primarily a ritual/textual language with limited vernacular depth; native vernacular reading predicts intergenerational transmission failure outside Israel and literacy-only transmission; hybrid continuity reading predicts continuous reachability and expanding vernacular domains. Collect longitudinal data on language transmission, domain expansion, speaker identity, and institutional investment across 50+ years.',
    'This omega routes the kernel contest into empirical territory. Different observable outcomes will support different readings. The hybrid reading''s truth value depends on which observables the engine weights most heavily — linguistic continuity, institutional support, native speaker numbers, or cultural authority distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_status, empirical, 'Empirical differentiation criteria for sibling readings of the Hebrew kernel').

omega_variable(
    false_summit_natural_law_risk,
    'Is the analytical mountain perspective (language revival necessarily requires continuity narratives) a genuine natural law of linguistics, or a naturalization of contingent institutional choices made by Zionist revival ideologues?',
    'Comparative language revitalization study: examine historical revivals (Irish, Basque, Welsh, Icelandic, Maori, Hawaiian, Navajo) and assess whether successful revivals consistently deploy continuity narratives or whether innovation narratives can succeed. If success varies by revitalization strategy regardless of continuity framing, the mountain perspective is false.',
    'If continuity narratives are necessary: mountain classification is justified. If contingent: the analytical perspective instantiates the oracle gap (Theorem 4) — the observer''s own institutional position within the revival project prevents seeing that the ''natural law'' is an artifact of revival ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, empirical, 'Whether continuity narratives are natural law or contingent institutional choice in language revival').

omega_variable(
    kernel_reading_dominance_coexistence,
    'Do the three sibling readings (hybrid continuity, liturgical preservation, native vernacular) coexist as live positions in contemporary Hebrew language politics, or has the hybrid reading foreclosed the others through institutional dominance?',
    'Discourse analysis: identify communities and institutions that actively maintain each reading (liturgical reading: yeshivot, diaspora scholarship networks; vernacular reading: linguistic critics of revival narratives; hybrid reading: state institutions, mainstream Israeli culture). Assess whether alternative readings have advocates with institutional power, or whether hybrid reading''s state backing has effectively silenced competitors.',
    'If readings coexist: the reading_relations should include coexists_with for both siblings. If hybrid has foreclosed others: foreclosed relation should be declared (rare, requires careful justification). If hybrid influences but does not foreclose: influences relation is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_dominance_coexistence, empirical, 'Whether sibling readings coexist or have been institutionally foreclosed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__hybrid_continuity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_hybrid_tr_t0, hebrew_living_language__hybrid_continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hebrew_hybrid_tr_t15, hebrew_living_language__hybrid_continuity_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(hebrew_hybrid_tr_t30, hebrew_living_language__hybrid_continuity_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(hebrew_hybrid_be_t0, hebrew_living_language__hybrid_continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hebrew_hybrid_be_t15, hebrew_living_language__hybrid_continuity_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(hebrew_hybrid_be_t30, hebrew_living_language__hybrid_continuity_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_hybrid_su_t0, hebrew_living_language__hybrid_continuity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(hebrew_hybrid_su_t15, hebrew_living_language__hybrid_continuity_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(hebrew_hybrid_su_t30, hebrew_living_language__hybrid_continuity_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__hybrid_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__hybrid_continuity_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_living_language__hybrid_continuity_reading, hebrew_living_language__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__hybrid_continuity_reading, hebrew_living_language__native_vernacular_reading).
narrative_ontology:affects_constraint(hebrew_living_language__hybrid_continuity_reading, zionist_nationalism_institutional_legitimacy).
narrative_ontology:affects_constraint(hebrew_living_language__hybrid_continuity_reading, diaspora_hebrew_authority_displacement).

% DUAL FORMULATION NOTE:
% The three sibling readings of the hebrew_living_language kernel each constitute a separate constraint story with different ε values and victim/beneficiary structures. The hybrid_continuity_reading (this story) has ε=0.48 (moderate extractive coordination). The liturgical_preservation_reading would have ε≈0.15 (low extraction, high coordination, pure rope perspective). The native_vernacular_reading would have ε≈0.62 (higher extraction, lower coordination, mixed snare/tangled_rope from linguistics perspective). Each reading is a distinct claim about what observables count as evidence for Hebrew's 'living' status; they are not alternative measurements of a single constraint but structurally different constraints sharing a common kernel text. The ε-invariance principle requires separate stories for different observable definitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language__hybrid_continuity_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
