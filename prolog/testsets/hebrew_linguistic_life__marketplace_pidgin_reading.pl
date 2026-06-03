% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life_marketplace_pidgin_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew as Marketplace Pidgin (Alive Through Practical Coordination)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   Hebrew linguistic aliveness is contested across three structurally
 *   distinct readings instantiated as separate constraints. This JSON
 *   describes the MARKETPLACE PIDGIN READING: Hebrew was continuously alive
 *   in Jerusalem markets pre-1880 as modified Medieval Hebrew pidgin, serving
 *   as inter-communal medium for practical coordination despite deviation
 *   from liturgical form and non-native-speaker status of many users. This
 *   reading claims Hebrew was alive during the precise period when the other
 *   two readings claim it was dead: the liturgical preservation reading
 *   requires unbroken recitation and study (supported by rabbis but not
 *   market traders); the native-generational reading requires mother-tongue
 *   acquisition by children (absent pre-1880). The marketplace reading
 *   resolves this temporal gap by proposing a third vitality criterion:
 *   functional coordination across linguistic communities, regardless of
 *   sacred status or native-speaker requirements. The constraint exhibits the
 *   classic signature of a contested kernel: all three readings point to the
 *   same historical object (Hebrew language, ~1600-1880 Jerusalem) but
 *   instantiate different constraints with different epsilon values,
 *   beneficiary/victim structures, and classifications. The marketplace
 *   reading is neither pure preservation nor pure revival but continuous
 *   adaptation.
 *
 * KEY AGENTS:
 *   - Jerusalem mercantile community: Primary beneficiary (organized/mobile) — benefits from Hebrew's flexibility as negotiation medium, solves coordination problem
 *   - Inter-communal traders (diaspora networks): Primary beneficiary (organized/mobile) — use marketplace Hebrew to maintain Jewish merchant identity while coordinating across linguistic communities
 *   - Liturgical Hebrew purists (rabbinical establishment): Primary victim of the marketplace reading (institutional/arbitrage) — the marketplace vitality claim undermines their framework requiring pure preservation of sacred form
 *   - Native-speaker doctrine enforcers (modern nationalist apparatus): Secondary victim (institutional/arbitrage) — the marketplace vitality claim undermines the nation-state's resurrection narrative
 *   - Marketplace institution itself (the shuk as organized entity): Beneficiary and victim (organized/constrained) — benefits from pidgin coordination but suppressed from claiming this legitimately
 *   - Marginalized native speaker cohorts (families using marketplace Hebrew as mother tongue): Victims (moderate/constrained) — denied recognition as 'true' speakers by purists; recognize themselves in the marketplace reading but lack institutional voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.38).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.52).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew as Marketplace Pidgin (Alive Through Practical Coordination)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '7167c714-a85b-4332-bdd4-a863ed0b7f8c').
narrative_ontology:cs_kernel_codification('7167c714-a85b-4332-bdd4-a863ed0b7f8c', distributed).
narrative_ontology:cs_authority_grounding('7167c714-a85b-4332-bdd4-a863ed0b7f8c', extraction).
narrative_ontology:cs_reading_relation('7167c714-a85b-4332-bdd4-a863ed0b7f8c', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('7167c714-a85b-4332-bdd4-a863ed0b7f8c', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('7167c714-a85b-4332-bdd4-a863ed0b7f8c', foundational, functional_coordination_constitutes_linguistic_aliveness).
narrative_ontology:cs_axiom_status(functional_coordination_constitutes_linguistic_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('7167c714-a85b-4332-bdd4-a863ed0b7f8c', functional_coordination_constitutes_linguistic_aliveness, instrumental).
narrative_ontology:cs_axiom('7167c714-a85b-4332-bdd4-a863ed0b7f8c', secondary, adaptation_preserves_continuity).
narrative_ontology:cs_axiom_status(adaptation_preserves_continuity, holdable).
narrative_ontology:cs_axiom_grounding('7167c714-a85b-4332-bdd4-a863ed0b7f8c', adaptation_preserves_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('7167c714-a85b-4332-bdd4-a863ed0b7f8c', continuous_marketplace_coordination_medium).
narrative_ontology:cs_drift_state('7167c714-a85b-4332-bdd4-a863ed0b7f8c', post_nationalist_historiography, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('7167c714-a85b-4332-bdd4-a863ed0b7f8c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_mercantile_community).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, inter_communal_traders).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_hebrew_purists).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, native_speaker_gatekeepers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL PURIST (SNARE) — Trapped by the phenomenological collapse of the marketplace pidgin reading into 'mere corruption' of the sacred tongue. The purist sees the marketplace use as extraction from Hebrew's sanctity, yet cannot exit this classification because accepting marketplace vitality would dissolve the purist's entire framework for Hebrew's 'true' function. Full experienced extraction — the market-reading dissolves the purist's epistemic authority without offering an alternative.
constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MARKETPLACE TRADER COMMUNITY (ROPE) — Organized agents engaged in practical multilingual coordination. The marketplace pidgin (modified Medieval Hebrew + Aramaic + Greek + Arabic lexical layers) solves a genuine coordination problem: enabling commerce across linguistic communities without requiring fluency in any single 'native' form. The traders experience the constraint as coordination, not extraction. Low chi: they have exit options (use lingua francas, shift to Greek or Arabic for specific transactions) and genuine benefit from Hebrew's flexibility.
constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: MARGINALIZED NATIVE SPEAKER COHORT (TANGLED ROPE) — Agents who claim Hebrew as mother tongue but face suppression by purists who refuse to recognize market-pidgin as 'real' Hebrew. This cohort experiences both coordination (the marketplace enables their participation) and extraction (they are denied recognition as 'true' speakers because they use a modified form). Constrained exit: they can suppress their own linguistic innovation and conform to liturgical standards, but this costs identity and practical utility. Mixed chi: genuine coordination function + asymmetric extraction through delegitimization.
constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL DOCTRINE ENFORCER (MOUNTAIN) — The rabbinic and scholarly apparatus enforcing Hebrew as either purely liturgical (rabbis) or purely native-revival (modern nationalists). Both positions treat marketplace pidgin as 'dead language in disguise' and enforce the mountain classification through institutional suppression of market-use documentation. This perspective has arbitrage: by maintaining the mountain claim, they preserve their epistemic authority to define what counts as Hebrew. However, the structural data contradicts the mountain classification — the engine's false summit detector will reveal this as naturalization of a normative stance, not an immutable law.
constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MARKETPLACE INSTITUTION (TANGLED ROPE) — The Jerusalem marketplace (shuk) as an organized entity that benefits from Hebrew's pidgin form (enables commerce, maintains Jewish merchant identity, coordinates across diaspora traders) while simultaneously suppressing documentation of this coordination (purists and later nationalist revivers both erase the marketplace pidgin from historical record, treating it as non-authentic). The market depends on the pidgin but cannot claim it as 'real' Hebrew within dominant frameworks. Constrained exit: merchants must continue using the pidgin to function, but are denied legitimacy for their practice.
constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MODERN NATION-STATE DOCTRINE (PITON) — 20th-century Zionist institutional framework claiming Hebrew revival as pure native-speaker language acquisition (Eliezer Ben-Yehuda narrative) erases the marketplace pidgin entirely through historiographical omission. This doctrine performs a neat institutional inversion: it canonizes the marketplace pidgin's legacy vocabulary while denying the pidgin itself ever existed as a 'real' living practice. Theater ratio high: the nation-state narrative requires the fantasy of pure revival (dead language → native speakers) to justify legitimacy; acknowledging continuous marketplace use would dissolve the resurrection myth. Piton: the doctrine persists through nationalist institutional inertia, not because it matches the empirical record. Arbitrage: nation-state benefits from the revival myth regardless of historical accuracy.
constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — From the criterion of inter-communal practical coordination as the metric of linguistic aliveness, the marketplace pidgin reading is a clean rope classification. The marketplace pidgin genuinely solves a coordination problem (enabling commerce and cultural exchange across linguistic communities). No suppression gate is violated: merchants freely adopt the pidgin to solve a problem they face. Low theater: the pidgin's function is transparent (communication works or it doesn't). This perspective sees Hebrew as continuously alive in the marketplace, neither dead nor pure, merely adapted to functional demands.
constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_linguistic_life__marketplace_pidgin_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, TR),
    TR >= 0.70.

:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The marketplace pidgin is a genuine coordination solution (low epsilon) until it becomes suppressed and delegitimized by purist doctrine. The measurement trajectory shows extractiveness rising from 0.22 to 0.42 as suppression mechanisms intensified (particularly post-1880 when nationalist doctrine began erasing marketplace use from historical record). The base value 0.38 reflects the state at maximum marketplace vitality (~1800 CE) when practical coordination benefits (low epsilon component) coexisted with moderate suppression and delegitimization (pushing epsilon upward). Suppression (0.52): Moderate-high. The marketplace pidgin faced two suppression vectors: (1) rabbinical refusal to recognize market-use as legitimate Hebrew, and (2) modern nationalist scholars' historiographical erasure of the marketplace entirely in favor of the revival narrative. The measurement trajectory shows suppression rising from 0.35 (early marketplace period with loose rabbinical oversight) to 0.68 (modern nationalist period with systematic documentation suppression). Theater ratio (0.48): Moderate. The marketplace pidgin was functionally transparent — it worked for coordination or it didn't, with minimal performative content. However, the early period (~1600) involved some code-switching and linguistic performance to navigate purist objections; this drops as the marketplace becomes more established; then rises again post-1880 as institutional pressure forces merchants to perform 'proper Hebrew' while continuing to use the pidgin in practice.
 *
 * PERSPECTIVAL GAP:
 *   The marketplace pidgin reading produces maximal perspectival divergence across the observation site. The liturgical purist sees the marketplace as extraction from Hebrew's sacred nature (snare perspective), experiencing the pidgin's flexibility as corruption. The marketplace trader community sees genuine coordination (rope perspective) — the pidgin solves their problem without external coercion. The marginalized native-speaker cohort experiences tangled rope: they are both enabled and delegitimized by the marketplace form. The institutional doctrine enforcers see the pidgin as dead-language-in-disguise and enforce the mountain classification through suppression (but structural data reveals this as a false summit — the mountain is political, not natural-law). The marketplace institution itself is tangled rope: it benefits from the pidgin but is constrained by institutional denial of legitimacy. The modern nationalist piton keeps the pidgin alive as vocabulary while denying it ever existed as a living practice. The analytical observer sees the pidgin as continuous linguistic vitality (rope) — it does what living languages do (adapt, coordinate, transmit meaning). The readings diverge maximally at the exit_options and agent_power dimensions: those with authority to define Hebrew (institutional, arbitrage) see mountain/snare; those who use the pidgin practically (organized, mobile) see rope; those caught between are tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the extraction flow. The marketplace traders (beneficiaries with mobile exit) experience low directionality (d ≈ 0.25): they have options and gain genuine benefit, so the sigmoid f(d) produces low chi despite moderate epsilon. The liturgical purists (positioned as victims by the marketplace reading, despite institutional power) experience high directionality (d ≈ 0.75): the marketplace vitality reading dissolves their epistemic framework, trapping them in a snare of their own making — their institutional authority depends on the mountain classification that the marketplace evidence undermines. The marginalized native-speaker cohort (constrained victims) experience moderate-high directionality (d ≈ 0.65): they are trapped by delegitimization despite functioning as native speakers in the marketplace form. The institutional doctrine enforcers face unusual directionality: despite their powerful position, the marketplace reading threatens their legitimacy directly, so they appear as victims (d ≈ 0.70) rather than beneficiaries. The analytical observer (d ≈ 0.72, canonical) is neutral — seeing the structure clearly requires no directional bias. The constraint's chi formula computation should show beneficiaries (traders) with suppressed chi despite moderate epsilon (due to low f(d)), and victims (purists, nationalists) with amplified chi (due to high f(d)) despite the same epsilon. This models the perspectival reversal: the marketplace reading advantages the powerless traders and threatens the institutional powerful.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (multiple irreducible doctrinal incompleteness) here is acute: the three readings cannot all be simultaneously true within a single institutional framework. Rabbinical doctrine requires (liturgical preservation criterion) that Hebrew remained alive through study and recitation — a claim that is empirically true but does not address the marketplace question. Modern nationalist doctrine requires (native-generational criterion) that Hebrew was dead until Zionist revival in the 1880s-1920s — a claim that is empirically false if the marketplace pidgin evidence is admitted. The marketplace reading requires (practical coordination criterion) that Hebrew was alive whenever it functioned inter-communally — a claim that is structurally coherent but only if one adopts a different vitality metric than the competing readings. The three readings are not merely 'different perspectives on the same truth' — they are instantiations of three different constraints with different epsilon values and different observable dependencies. The marketplace reading (this constraint) has epsilon 0.38 because the marketplace pidgin genuinely solves a coordination problem (low base epsilon) but is suppressed by institutional actors who benefit from alternative framings (high suppression pushing epsilon upward). The liturgical preservation reading would have epsilon ~0.15 (a mountain, maybe — recitation and study are indeed happening continuously, with minimal suppression). The native-generational reading would have epsilon ~0.72 (a snare — the native-speaker doctrine suppresses evidence of the marketplace alive-ness and extracts institutional legitimacy from the false 'dead → revival' narrative). The mandatrophy is not resolved by choosing one reading but by recognizing that the hebrew_linguistic_life kernel admits multiple coherent readings with different structural properties. The analytical move is to stop asking 'which reading is the true one?' and instead to model the constraint family as a presheaf: 'Hebrew linguistic aliveness' is different at each observational site (marketplace vs synagogue vs schoolroom), and the three readings carve out three of the major sites. Accepting the marketplace reading does not invalidate the liturgical preservation reading — it merely recognizes that both were happening simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidgin_boundary_threshold,
    'At what degree of modification from ''Classical Hebrew'' does the marketplace form cease to count as Hebrew and become ''corrupted'' or ''a different language''?',
    'Comparative analysis of documented marketplace Hebrew vocabulary, phonology, and morphosyntax against attested medieval Hebrew and Aramaic; establishment of lexical/phonological change rates that distinguish living adaptation from language death + new-language emergence',
    'If threshold is permissive (modification rates < 15% per generation): marketplace pidgin counts as continuously alive Hebrew. If threshold is restrictive (any deviation from liturgical form disqualifies): marketplace pidgin is classified as dead Hebrew and Hebrew-Aramaic creole, not Hebrew itself. This directly determines the epistemic validity of the marketplace_pidgin_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_boundary_threshold, empirical, 'Threshold for modification rate distinguishing adaptation from language death').

omega_variable(
    inter_communal_coordination_necessity,
    'Did the Jerusalem marketplace REQUIRE Hebrew as a lingua franca, or could traders equally well have coordinated via Greek, Aramaic, or Arabic?',
    'Historical analysis of merchant networks, trade routes, and linguistic evidence in marketplace documents (receipts, contracts, gravestones); comparison of transaction efficiency in multilingual vs Hebrew-mediated marketplaces; examination of diaspora trader networks and their preferred medium',
    'If Hebrew was necessary (no viable alternative): the marketplace pidgin is a genuine coordination solution (Rope from trader perspective, validates tangled_rope classification). If Hebrew was redundant (other lingua francas equally available): the marketplace use is performative maintenance of Jewish identity, not functional coordination (suggests piton or lower chi for marketplace institution).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inter_communal_coordination_necessity, empirical, 'Whether Hebrew was functionally necessary for marketplace coordination').

omega_variable(
    liturgical_purist_vs_marketplace_foreclosure,
    'Does the liturgical preservation doctrine logically foreclose the marketplace vitality doctrine, or do both framings remain coherently holdable by different institutional actors?',
    'Examination of whether a single framework can hold both (liturgical preservation AND marketplace practical function) without internal contradiction. Test: can a scholar simultaneously claim (a) Hebrew was alive in liturgy and (b) Hebrew was alive in the marketplace without contradiction? Or does accepting (b) necessarily invalidate the theoretical commitments required for (a)?',
    'If internally contradictory: the readings have forecloses relation (one logically rules out the other). If coherently coexistable: the readings have coexists_with relation (both frameworks are true from different institutional positions). This determines the cs_structure.reading_relations type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_purist_vs_marketplace_foreclosure, conceptual, 'Whether liturgical and marketplace readings are logically compatible').

omega_variable(
    marketplace_record_suppression_mechanism,
    'Why was the marketplace Hebrew pidgin systematically erased from historical documentation and scholarly record despite its continuous practical use?',
    'Analysis of who controlled written record-keeping (rabbis, scholars, later nationalists); examination of incentive structures for each group to erase or preserve marketplace evidence; comparison of oral tradition vs written documentation of marketplace practice; investigation of selection bias in which texts survived',
    'If suppression was active institutional policy: the marketplace_pidgin_reading reveals a snare mechanism targeting evidence itself — the constraint extracts the pidgin''s epistemic recognition. If suppression was passive (marketplace seemed ''uncultured'' so wasn''t written down): the constraint is primarily about delegitimization, not suppression of coordination. This affects the suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_record_suppression_mechanism, empirical, 'Mechanism driving erasure of marketplace Hebrew from historical record').

omega_variable(
    native_speaker_reading_compatibility,
    'If marketplace traders raised children who acquired the marketplace pidgin as mother tongue alongside or instead of liturgical Hebrew, does this compromise the native_generational_reading''s core premise?',
    'Historical documentation of intergenerational Hebrew transmission in merchant families; analysis of whether children of marketplace traders used the modified form natively or learned it as a second code; examination of family documentation and personal accounts from pre-1880 Jerusalem',
    'If children did acquire marketplace Hebrew as mother tongue: the native_generational_reading and marketplace_pidgin_reading begin to coexist rather than compete — both can be true. If marketplace Hebrew was never natively acquired (always learned as second language): the readings remain in stronger tension. This affects reading_relations classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(native_speaker_reading_compatibility, empirical, 'Whether marketplace Hebrew was natively acquired by second-generation speakers').

omega_variable(
    kernel_identity_stability,
    'What is the identity of the Hebrew kernel that all three readings claim to interpret? Is there a single persistent entity (Hebrew language) that the readings differ about, or do the readings actually constitute three different language-concepts?',
    'Philosophical analysis of language identity conditions; examination of whether the three readings point to a shared referent or three distinct historical objects; investigation of whether ''Hebrew'' in 1800 CE marketplace is the same entity as ''Hebrew'' in liturgical recitation or ''Hebrew'' in 1920s native-speaker schooling',
    'If single kernel: the readings are genuinely three interpretations of one constraint. If three kernels: the constraint family should decompose into three independent constraints, each with its own epsilon and type. This affects the entire committer framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_stability, conceptual, 'Identity stability of the Hebrew language kernel across readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_mktp_theater_t0, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hebrew_mktp_theater_t15, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(hebrew_mktp_theater_t30, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(hebrew_mktp_extract_t0, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hebrew_mktp_extract_t15, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(hebrew_mktp_extract_t30, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_mktp_suppress_t0, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hebrew_mktp_suppress_t15, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(hebrew_mktp_suppress_t30, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, resource_allocation).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, nationalist_language_revival_narrative).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_gatekeeping_via_native_speaker_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is part of the hebrew_linguistic_life constraint family (3 readings total). The marketplace_pidgin_reading (this file) isolates the practical coordination vitality metric from the liturgical preservation and native-generational readings. Each reading has its own epsilon value and classification; the network edges link them as competing framings of a single kernel. The marketplace reading is downstream of the liturgical preservation reading in that it preserves the linguistic form (vocabulary, core grammar) that the liturgical reading maintained, but reinterprets the mechanism of 'aliveness' from sacred transmission to practical use. The native-generational reading is downstream of the marketplace reading in that it colonizes the marketplace pidgin's vocabulary and structures as the foundation of modern Hebrew while erasing the marketplace's prior existence. The nationalist_language_revival_narrative and linguistic_gatekeeping_via_native_speaker_doctrine are downstream institutional implementations that depend on the native-generational reading's victory over the marketplace reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__marketplace_pidgin_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
