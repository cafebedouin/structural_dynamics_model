% ============================================================================
% CONSTRAINT STORY: marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marketplace_pidgin_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marketplace_pidgin_reading
 *   human_readable: Hebrew as Living Marketplace Pidgin (1400–1880 Jerusalem)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'hebrew_linguistic_life': the marketplace pidgin reading asserts that
 *   Hebrew functioned as a living inter-communal medium in Jerusalem markets
 *   from approximately 1400–1880, enabling practical coordination across
 *   Jewish merchant networks and between Arabic-speaking and Aramaic-speaking
 *   Jewish communities, independently of whether speakers were native-born,
 *   whether the language served sacred liturgical function, or whether it
 *   preserved classical grammar. This reading emphasizes functional
 *   continuity (markets existed, commerce required a common medium, Hebrew
 *   served that function with continuous adaptation to phonological and
 *   morphosyntactic features of the multilingual Jerusalem context) over
 *   either pure preservation (the liturgical reading) or pure revival (the
 *   native-generational reading). The marketplace pidgin reading is not a
 *   historical discovery about what 'really happened' in medieval Jerusalem
 *   but a theoretical frame: a specific way of identifying and measuring
 *   linguistic life that benefits merchant-network actors and
 *   functional-linguistic frameworks while it constrains and delegitimizes
 *   sacred-preservationist and native-speaker frameworks. The constraint
 *   exhibits tangled_rope structure: it coordinates practical commerce
 *   (genuine function) while asymmetrically extracting legitimacy from
 *   competing definitions of linguistic aliveness (suppressing sacred and
 *   native-speaker criteria).
 *
 * KEY AGENTS:
 *   - Jewish Merchant Networks: Primary beneficiary (institutional/arbitrage) — the marketplace pidgin is their coordination mechanism; they capture legitimacy from functional definition of aliveness
 *   - Arabic-Speaking Jewish Communities: Secondary beneficiary (powerful/mobile) — gain access to Jewish commercial networks and governance by using marketplace Hebrew without learning liturgical form
 *   - Liturgical Preservation Advocates: Primary victim (powerless/trapped) — the reading subordinates sacred-language criteria to functional criteria, delegitimizing their preservation mission
 *   - Native Speaker Ideologues: Secondary victim (moderate/constrained) — the reading proves Hebrew can be alive without native-speaker populations, constraining native-speaker revival arguments
 *   - Jerusalem Rabbinical Authority: Institutional enforcer (institutional/arbitrage) — tacitly tolerates marketplace pidgin as temporary coordination scaffold while defending liturgical Hebrew's sacred boundary
 *   - Zionist Nationalist Movement: Retrospective piton actor (institutional/arbitrage) — adopts marketplace reading post-1880 as historical legitimation of 'continuous Hebrew tradition,' maintaining it as performative artifact after functional requirement disappears
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing 'function determines life' as universal linguistic law rather than recognizing it as a sociolinguistic positioning that benefits specific actors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marketplace_pidgin_reading, 0.35).
domain_priors:suppression_score(marketplace_pidgin_reading, 0.48).
domain_priors:theater_ratio(marketplace_pidgin_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marketplace_pidgin_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marketplace_pidgin_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(marketplace_pidgin_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marketplace_pidgin_reading, tangled_rope).
narrative_ontology:human_readable(marketplace_pidgin_reading, "Hebrew as Living Marketplace Pidgin (1400–1880 Jerusalem)").
narrative_ontology:topic_domain(marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(marketplace_pidgin_reading, distributed).
narrative_ontology:cs_authority_grounding(marketplace_pidgin_reading, distributed).
narrative_ontology:cs_kernel_id(marketplace_pidgin_reading, hebrew_linguistic_life).
narrative_ontology:cs_reading_relation(marketplace_pidgin_reading, liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation(marketplace_pidgin_reading, native_generational_reading, influences).
narrative_ontology:cs_axiom(marketplace_pidgin_reading, foundational, function_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(function_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding(marketplace_pidgin_reading, function_constitutes_linguistic_life, instrumental).
narrative_ontology:cs_axiom(marketplace_pidgin_reading, foundational, native_speaker_status_not_necessary).
narrative_ontology:cs_axiom_status(native_speaker_status_not_necessary, holdable).
narrative_ontology:cs_axiom_grounding(marketplace_pidgin_reading, native_speaker_status_not_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame(marketplace_pidgin_reading, medieval_hebrew_marketplace_reality).
narrative_ontology:cs_drift_state(marketplace_pidgin_reading, post_1880_nationalist_adoption, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marketplace_pidgin_reading, jewish_merchant_networks).
narrative_ontology:constraint_beneficiary(marketplace_pidgin_reading, arabic_speaking_jewish_communities).
narrative_ontology:constraint_victim(marketplace_pidgin_reading, liturgical_hebrew_preservation_advocates).
narrative_ontology:constraint_victim(marketplace_pidgin_reading, native_speaker_ideologues).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL PRESERVATION ADVOCATE (SNARE) — Trapped in the constraint that accepts marketplace pidgin as 'alive Hebrew' rather than degradation. Cannot exit the framework that defines aliveness by functional coordination rather than sacred purity. Bears the full cost of having sacred language subordinated to commercial practicality. No organizational power; the marketplace definition of life excludes their criteria entirely.
constraint_indexing:constraint_classification(marketplace_pidgin_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: NATIVE SPEAKER IDEOLOGUE (TANGLED ROPE) — Constrained by the linguistic reality that no monolingual Hebrew-native child population existed in Jerusalem markets pre-1880; all Hebrew speakers were multilingual code-switchers. This reading coordinates practical multilingual reality but asymmetrically extracts legitimacy — it redefines 'native' away from the ideology of language purity. Mixed experience: the marketplace definition enables their own multilingual practice but delegitimizes the native-speaker purity norm.
constraint_indexing:constraint_classification(marketplace_pidgin_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: JEWISH MERCHANT NETWORKS (ROPE) — Primary beneficiaries. The marketplace pidgin is a coordination mechanism enabling cross-community trade, credit systems, and information flow. Hebrew as functional medium (modified Medieval Hebrew with Arabic-influenced morphology and syntax) solves the multi-generational, multi-origin problem of commerce in Jerusalem without requiring liturgical purity or native-speaker status. Net beneficiary; experiences low extraction because the constraint enables their primary function.
constraint_indexing:constraint_classification(marketplace_pidgin_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ARABIC-SPEAKING JEWISH COMMUNITIES (ROPE) — Beneficiaries. The marketplace reading enables them to function as Hebrew speakers without learning liturgical Hebrew or abandoning Arabic as primary language. Coordination benefit: practical access to Jewish merchant networks, cross-communal governance, and religious-commercial interface. Mobile because they can shift between Hebrew marketplace practice and Arabic home language. Moderate power through economic participation.
constraint_indexing:constraint_classification(marketplace_pidgin_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: JERUSALEM RABBINICAL AUTHORITY (SCAFFOLD) — Organized enforcer with constrained exit. The marketplace pidgin is tacitly tolerated (not officially endorsed) as temporary coordination mechanism required for commerce and governance. Enforcement is minimal because the pidgin does not threaten liturgical Hebrew's sacred function. The authority sees this as scaffolding: marketplace Hebrew enables the practical city to function while liturgical Hebrew preserves the sacred. Implicit sunset: as education spreads, marketplace pidgin would be replaced by more 'proper' Hebrew (which historically happened post-1880).
constraint_indexing:constraint_classification(marketplace_pidgin_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: DIASPORA NATIONALIST MOVEMENT (PITON) — The Zionist movement post-1880 redefined this marketplace pidgin as evidence of 'continuous Hebrew tradition' and 'Jewish linguistic nationalism,' adopting the marketplace reading retrospectively while reframing it through native-speaker ideology. The pidgin itself becomes performative artifact — invoked as proof of Hebrew's 'aliveness' but no longer functionally necessary (modern educated Hebrew replaced marketplace pidgin). Theater ratio high because the pidgin is now invoked symbolically rather than functionally. Piton: degraded inertial constraint, maintained through narrative-historical legitimacy rather than current coordination requirement.
constraint_indexing:constraint_classification(marketplace_pidgin_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing linguistics perspective, any language remains alive as long as it functions as a medium for practical community coordination, regardless of native-speaker status or sacred function. This is a natural law of linguistic life: function determines vitality, not blood status or ritual authority. However, this reading is a false summit — the framework's analytical instruments cannot detect that 'function determines life' is itself a contested ideological position that benefits actors (merchant networks) who benefit from defining life functionally rather than through sacred preservation.
constraint_indexing:constraint_classification(marketplace_pidgin_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marketplace_pidgin_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marketplace_pidgin_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marketplace_pidgin_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(marketplace_pidgin_reading, TR),
    TR >= 0.70.

:- end_tests(marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate-low. The marketplace pidgin reading coordinates genuine merchant-network function — commerce did require a common medium, and Hebrew served that function. But the reading also extracts legitimacy from sacred and native-speaker frameworks by redefining what counts as linguistic life. The extractiveness value reflects that the coordination benefit is real but asymmetrically distributed: merchant networks benefit, while liturgical and native-speaker advocates bear the cost of delegitimation. Suppression (0.48): Moderate. The reading suppresses alternatives through definitional authority — it asserts that functional coordination defines linguistic aliveness, making sacred and native-speaker criteria appear secondary or ideological. But suppression is not total because liturgical Hebrew continued to function (sacred contexts remained), and revival movements post-1880 successfully established native-speaker Hebrew despite the marketplace reading's refutation. Theater ratio (0.55): Moderate. The marketplace reading includes both functional elements (genuine coordination requirement, real merchant-network activity) and performative elements (the modern retrospective interpretation through nationalist lens, the invocation of marketplace pidgin as proof of 'continuous tradition' after its functional role ended). The theater ratio increases over the interval as modern nationalist frameworks adopt the marketplace reading for legitimation purposes beyond its original functional context.
 *
 * PERSPECTIVAL GAP:
 *   The marketplace pidgin reading produces maximal perspectival divergence. Merchant networks see coordination (Rope) — the pidgin is their enabling medium. Rabbinical authority sees temporary scaffold — marketplace Hebrew is acceptable because it does not threaten liturgical function. Nationalist movement sees piton — the marketplace reading becomes inert historical legitimation after the functional requirement vanishes. Liturgical advocates see snare — they are trapped by functional criteria that make their preservation mission appear secondary. Native-speaker ideologues see tangled rope — the reading enables their modern project (proving Hebrew learnable without native speakers) while constraining their historical narrative (proving pre-modern native speakers). The analytical observer risks seeing natural law — function determines life universally — when the reading is actually a positioned framework that benefits merchant networks and constrains sacred-preservationist positions. The perspectival gap reveals that 'linguistic aliveness' is not a natural-language property but a normative category: different frameworks measure life differently (function vs. sacred function vs. native generation), and the marketplace reading asserts one framework's authority over others.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's relationship to the constraint's extraction flow. Merchant networks are primary beneficiaries with full arbitrage options (low d, negative χ) — they can move to other media but choose Hebrew because it coordinates their networks. Arabic-speaking Jews are beneficiaries with mobile exit (moderate d) — they benefit from functional inclusion but could use other languages. Liturgical advocates are primary victims with trapped exit (high d, high χ) — they cannot exit the constraint that subordinates their criteria to functional ones. Native-speaker ideologues are secondary victims with constrained exit (high-moderate d) — the marketplace reading constrains their revival arguments but does not foreclose them. The rabbinical authority experiences low extraction (arbitrage options; can enforce or not enforce) because they can accommodate both marketplace and liturgical Hebrew. The nationalist movement post-1880 experiences negative extraction as beneficiary (the reading legitimizes their narrative). The analytical observer at civilizational scale risks false-summit directionality: naturalizing 'function determines life' makes the reading appear d=0.72 (observer position) when it actually benefits specific institutional actors (should be d ≈ 0.25 institutional beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The marketplace pidgin reading exemplifies how mandatrophy is resolved through recognizing that no single type captures the constraint's structure from all positions. The reading is simultaneously: (1) Rope for merchants (genuine coordination), (2) Scaffold for authority (temporary tolerance), (3) Piton for nationalists (retrospective legitimation), (4) Snare for liturgical advocates (delegitimation), (5) Tangled Rope for native-speaker ideologues (enables modern project; constrains historical narrative), (6) False Summit for analytical observers (risks naturalizing 'function determines life'). The constraint does not resolve to one type because the seven perspectives occupy fundamentally different structural positions relative to the extraction flow. The mandatrophy is not solved by 'finding the right measurement' but by recognizing that the presheaf of classifications (one per position) is the complete description. What appears as mandatrophy from a single analytical position becomes a consistent multi-positioned structure when all perspectives are included.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidgin_vs_modified_classical,
    'What structural markers distinguish a ''living marketplace pidgin'' from ''modified classical Hebrew with dialectal variation''?',
    'Morphosyntactic analysis: systematic creolization markers (grammatical simplification, category collapse, morpheme reanalysis) vs. dialect-internal variation; comparative analysis of Jerusalem merchant Hebrew vs. contemporary Italian Hebrew texts and Sephardic liturgical Hebrew',
    'If pidgin: reading is correct — continuous functional adaptation. If dialect: reading risks anachronistic projection of functional categories onto medieval code-switching. If spectrum (both simultaneously): classification as tangled_rope vs. rope requires distinguishing coordination function from drift direction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pidgin_vs_modified_classical, empirical, 'Whether marketplace Hebrew shows pidgin-level grammatical restructuring or dialect-level variation').

omega_variable(
    function_versus_sacred_boundary,
    'Is the boundary between ''functional marketplace medium'' and ''sacred liturgical language'' a structural linguistic fact or a sociolinguistic category imposed by the religious authority?',
    'Historical documentation: were speakers required to shift registers between market and prayer? Did the same individuals use Hebrew seamlessly across contexts or did they treat marketplace and liturgical Hebrew as distinct languages? Examination of transliterated marketplace documents for religious content.',
    'If structural linguistic fact: marketplace and liturgical Hebrew are genuinely separate, and this reading correctly identifies marketplace aliveness. If sociolinguistic category: the boundary is enforced social distinction, and the reading reveals how religious authority constructs ''sacred language'' via context restriction rather than linguistic difference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(function_versus_sacred_boundary, empirical, 'Whether functional/sacred distinction is linguistic or socially imposed').

omega_variable(
    continuity_versus_revival,
    'Can ''continuous marketplace adaptation'' be empirically distinguished from ''liturgical preservation with market-dialect emergence''? Or are these the same phenomenon viewed from different community positions?',
    'Diachronic corpus analysis: tracing specific Hebrew features from 1400–1880 marketplace texts through post-1880 revival period. Do modern Hebrew features appear first in marketplace sources (suggesting continuous development) or in revival period texts (suggesting new creation)? Cross-community transmission: did Arabic-speaking Jews transmit marketplace Hebrew as living practice or as isolated texts?',
    'If continuous: reading is correct — Hebrew was alive in markets pre-1880. If revival-era creation: reading is retrospective — marketplace texts are artifacts that appear continuous only when viewed through modern nationalist lens. If both (layered): marketplace communication was genuinely alive but the modern nationalist ''continuity'' narrative is constructed — affects mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_versus_revival, empirical, 'Whether marketplace Hebrew represents continuous functional adaptation or revival-era historical construction').

omega_variable(
    reading_foreclosure_boundary,
    'Does this reading''s functional-life criterion logically foreclose the native-speaker reading, or can a framework hold both (''Hebrew was alive in markets AND should be revived through native speakers'')?',
    'Philosophical-logical analysis: are ''life through function'' and ''life through native generation'' contradictory axioms in the same framework, or alternative criteria that rank differently? Historical: did any early modern or modern Jewish authority hold both criteria simultaneously without treating them as competing?',
    'If foreclosed: reading creates a zero-sum competition with native-speaker ideology. If coexists: reading is orthogonal to native-speaker project and could support it (marketplace pidgin proves Hebrew can be learned as non-native, therefore learnable by modern speakers). Affects whether cs_structure.reading_relations to native_generational_reading should be ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether functional-life criterion logically forecloses native-speaker criterion').

omega_variable(
    beneficiary_circularity_risk,
    'Does this reading risk circular reasoning: ''Hebrew was alive because merchant networks used it for coordination; merchant networks existed because Hebrew enabled coordination''?',
    'Counterfactual analysis: could merchant networks have functioned equally well using only Arabic or Aramaic? What coordination tasks required Hebrew specifically (vs. what tasks required any common medium)? Comparative analysis: how did non-Hebrew merchant networks in Mediterranean coordinate without comparable ''sacred language + market pidgin'' structure?',
    'If circular: extracted benefit is endogenous to the reading rather than evidence of prior aliveness. If evidence-based: merchant-network coordination was genuinely dependent on Hebrew''s specific features, supporting the reading. Affects directionality analysis of beneficiaries: are they beneficiaries of the constraint or creators of it?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_circularity_risk, empirical, 'Risk of circular reasoning in defining Hebrew''s functional necessity').

omega_variable(
    modernist_retrospection_contamination,
    'To what degree has post-1880 Zionist historiography contaminated our reading of pre-1880 marketplace Hebrew, causing us to project modern functional-life criteria backward onto medieval code-switching?',
    'Source criticism: separating contemporary (pre-1880) attestations from retrospective (post-1880 nationalist) reinterpretations. Are the marketplace texts we examine original documents from 1400–1880, or transcriptions/translations made by modern nationalist scholars seeking to prove ''continuous tradition''? Analysis of which scholars first applied ''continuous marketplace Hebrew'' frame and whether their framing anticipated nationalist arguments.',
    'High-risk omega: if modernist retrospection is substantial, the reading may be a false bottom (constructed reading masquerading as historical discovery). Affects claimed_type classification: if contaminated, extractiveness rises (the reading benefits nationalist projects at cost of linguistic truth) and classification shifts toward snare. If low contamination: reading stands as empirical discovery about medieval marketplace reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_retrospection_contamination, empirical, 'Degree of retrospective nationalist historiographic contamination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marketplace_pidgin_reading, 1400, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, marketplace_pidgin_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mark_tr_t100, marketplace_pidgin_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(mark_tr_t200, marketplace_pidgin_reading, theater_ratio, 200, 0.55).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, marketplace_pidgin_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mark_be_t100, marketplace_pidgin_reading, base_extractiveness, 100, 0.32).
narrative_ontology:measurement(mark_be_t200, marketplace_pidgin_reading, base_extractiveness, 200, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marketplace_pidgin_reading, resource_allocation).
narrative_ontology:affects_constraint(marketplace_pidgin_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(marketplace_pidgin_reading, native_generational_reading).

% DUAL FORMULATION NOTE:
% The constraint 'hebrew_linguistic_life' (kernel) decomposes into three constraints: marketplace_pidgin_reading (ε=0.35, this story), liturgical_preservation_reading (ε≈0.42, separate story), and native_generational_reading (ε≈0.45, separate story). Each reading has its own extractiveness value because each privileges different observables: marketplace_pidgin_reading measures life through coordination function; liturgical_preservation_reading measures life through sacred continuity; native_generational_reading measures life through native-speaker population. The three stories are linked by network edges — each affects the others because they are competing frameworks for the same kernel. The three together instantiate the manifest's kernel_id commitment-system structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marketplace_pidgin_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
