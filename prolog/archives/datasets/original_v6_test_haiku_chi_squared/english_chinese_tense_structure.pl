% ============================================================================
% CONSTRAINT STORY: english_chinese_tense_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_english_chinese_tense_structure, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: english_chinese_tense_structure
 *   human_readable: Obligatory Tense Marking vs. Aspect-Context Encoding in English and Mandarin Chinese
 *   domain: linguistics/cognitive_science
 *
 * SUMMARY:
 *   The structural distinction between obligatory tense marking (English) and
 *   aspect-context encoding (Mandarin Chinese) generates a persistent tension
 *   in formal linguistics between treating English structures as the
 *   canonical case and recognizing that high-complexity languages encode
 *   temporal information through cognitively legitimate alternative
 *   mechanisms. This constraint exhibits extraction masked as coordination
 *   through institutional gatekeeping: English tense-obligatory analysis is
 *   treated as the unmarked default, forcing Mandarin-first researchers to
 *   justify their aspect-centered frameworks rather than vice versa. The
 *   constraint operates at multiple levels: (1) theoretical — which formal
 *   framework represents the natural law of human language; (2) institutional
 *   — which analytical approach dominates peer review and textbook curricula;
 *   (3) cognitive — whether obligatory tense marking reflects genuine
 *   cognitive necessity or pragmatic cultural choice. The theater_ratio
 *   (0.58) reflects that much contemporary work within the tense-obligatory
 *   tradition consists of theoretical elaboration (abstract tense features,
 *   syntax-semantics mapping frameworks) with declining motivation from
 *   empirical language data. The aspect-context tradition has genuine
 *   explanatory coverage but must constantly justify itself as a departure
 *   from the unmarked case rather than as an equally fundamental mechanism.
 *
 * KEY AGENTS:
 *   - English tense-obligatory speakers: Primary beneficiary (institutional/arbitrage) — their native language structure is treated as the theoretical baseline; can arbitrage between different analytical frameworks without losing descriptive adequacy for their data
 *   - Mandarin Chinese speakers: Primary victim (powerless/trapped) — trapped in a linguistic classification system that treats their language as marked, deficient in tense, requiring explanation for why they get along without obligatory marking
 *   - Aspect-centered research tradition (cognitive linguistics, functional linguists, sinologists): Secondary victim (moderate/constrained) — constrained by pressure to adopt tense-based terminology and frameworks to achieve publication and funding; cannot fully exit without abandoning credibility
 *   - English formalist linguistics establishment: Primary beneficiary and enforcer (institutional/arbitrage) — maintains tense-obligatory frameworks through peer review gatekeeping, textbook curricula, and theoretical elaboration
 *   - Cognitive science and psycholinguistics communities: Analytical observer with constraint (organized/constrained) — increasingly generate evidence that tense obligatoriness is not a cognitive universal, but cannot easily shift analytical frameworks without losing technical apparatus and cross-referential infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(english_chinese_tense_structure, 0.38).
domain_priors:suppression_score(english_chinese_tense_structure, 0.52).
domain_priors:theater_ratio(english_chinese_tense_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(english_chinese_tense_structure, extractiveness, 0.38).
narrative_ontology:constraint_metric(english_chinese_tense_structure, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(english_chinese_tense_structure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(english_chinese_tense_structure, tangled_rope).
narrative_ontology:human_readable(english_chinese_tense_structure, "Obligatory Tense Marking vs. Aspect-Context Encoding in English and Mandarin Chinese").
narrative_ontology:topic_domain(english_chinese_tense_structure, "linguistics/cognitive_science").

domain_priors:requires_active_enforcement(english_chinese_tense_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(english_chinese_tense_structure, english_tense_system_speakers).
narrative_ontology:constraint_beneficiary(english_chinese_tense_structure, linguistic_formalism_tradition).
narrative_ontology:constraint_victim(english_chinese_tense_structure, mandarin_chinese_speakers).
narrative_ontology:constraint_victim(english_chinese_tense_structure, aspect_centered_analysis_frameworks).
narrative_ontology:constraint_victim(english_chinese_tense_structure, cross_linguistic_cognitive_validity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANDARIN SPEAKER (SNARE) — Trapped in tense-based grammatical classification systems despite Mandarin encoding temporal information through aspect, context, and pragmatics rather than obligatory morphology. Native speakers cannot opt out of being classified as 'tenseless' in formal analysis. No exit option. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.66.
constraint_indexing:constraint_classification(english_chinese_tense_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ASPECT-CENTERED RESEARCHERS (TANGLED ROPE) — Constrained by pressure to adopt tense-based terminology and frameworks to be published in major venues, yet also benefit from the resulting tension as a productive research domain. Must translate their findings into tense-obligatory frameworks. d≈0.72, f(d)≈1.10, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(english_chinese_tense_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENGLISH FORMALIST TRADITION (ROPE) — Benefits from tense as a central analytical category that privileges English-language structure as the canonical case. Can arbitrage between tense-obligatory and aspect-optional frameworks without losing descriptive power for their primary data. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(english_chinese_tense_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC PUBLISHING (TANGLED ROPE) — Institutional coordination function: tense-aspect frameworks enable systematic comparison across languages. But also enforces extraction: journals privilege tense-based analysis in formal presentations, requiring aspect-first researchers to re-frame findings. Constraint is self-reinforcing through peer review gatekeeping. d≈0.45, f(d)≈0.42, σ=1.2 → χ≈0.24.
constraint_indexing:constraint_classification(english_chinese_tense_structure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UNIVERSAL GRAMMAR FORMALISM (PITON) — Tense-marking as a core universal principle persists through institutional inertia despite cognitive evidence that many high-complexity languages encode time aspectually. The framework is maintained through theoretical elaboration (abstract tense features) even as empirical motivation weakens. theater_ratio=0.58 reflects sustained theoretical activity with declining functional necessity. d≈0.02, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(english_chinese_tense_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COGNITIVE-FUNCTIONAL OBSERVER (TANGLED ROPE) — Sees genuine coordination problem: languages DO need to encode temporal information reliably. But also sees extraction: English-like tense systems are treated as the unmarked case, forcing other languages into deficit-framed categories ('tenseless', 'aspect-prominent'). The classification is neither purely natural law nor purely extraction — it is a presheaf over observation site. d≈0.60, f(d)≈0.80, σ=1.0 → χ≈0.30.
constraint_indexing:constraint_classification(english_chinese_tense_structure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(english_chinese_tense_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(english_chinese_tense_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(english_chinese_tense_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(english_chinese_tense_structure, TR),
    TR >= 0.70.

:- end_tests(english_chinese_tense_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The tense-obligatory framework extracts from aspect-first researchers primarily through gatekeeping and prestige rather than through direct coercion. Mandarin speakers are not prevented from speaking; rather, their language is classified as lacking a feature that their language simply does not require. The extraction occurs in the academic sphere — published analyses of Mandarin often frame aspect-context as a workaround for the lack of tense rather than as a primary mechanism. Suppression (0.52): Moderate-high. Barriers include: (1) institutional — tense-based frameworks dominate linguistic journals and graduate curricula; (2) cognitive — the theoretical apparatus of tense-based formalism (Tense features in syntax, scope relationships, compositionality) is deeply embedded in training; (3) linguistic — the formalism was developed for English and transplanting it to Mandarin requires constant theoretical elaboration to handle non-tense phenomena. Yet suppression is not absolute — aspect-centered work does get published, and Mandarin-first perspectives have institutional homes. Theater ratio (0.58): Moderate-high. Recent work in formal semantics around tense involves elaborate theoretical constructs (speech-time vs reference-time vs event-time, abstract tense features in minimalist syntax) with diminishing empirical motivation from languages where tense is not obligatory. The theater has grown over the 80-year interval as formalist complexity increased while empirical coverage of non-tense languages remained relatively shallow. The constraint has shifted from a genuine theoretical disagreement (early 20th century) to an institutional arrangement sustained through career incentives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival disagreement rooted in structural position. The English speaker sees coordination (Rope) — tense-obligatory analysis simply describes their language naturally. The aspect-centered researcher sees extraction (Tangled Rope) — the tense framework is forced upon analysis of Mandarin where it does not fit natively. The Mandarin speaker sees a pure snare (Snare) — classified as deficient in a feature they never needed to produce. The formalist establishment sees efficiency and coordination (Rope) — tense is a formal primitive that makes cross-linguistic analysis tractable. The broader cognitive-functional observer sees extraction masquerading as coordination (Tangled Rope) — the tense-obligatory framework provides genuine organizational benefits for some purposes but imposes costs on describing many of the world's languages. The piton perspective (institutional inertia in Universal Grammar) sees a degraded theoretical framework maintained through elaboration rather than explanatory necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   English tense speakers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Their language structure is the default case; they can arbitrage between frameworks without losing capacity. Mandarin speakers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. They are trapped in a classification system that treats their language as marked/deficient; they cannot opt out. Aspect researchers: Victim + constrained → d≈0.72, f(d)≈1.10. High extraction. They face publication barriers, must constantly justify aspect-first approaches, and bear the burden of re-framing findings to fit tense-obligatory templates. English formalist establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Their theoretical tradition is the institutional default; they maintain explanatory arbitrage. Cognitive-functional observer: Mixed → d≈0.60, f(d)≈0.80. Sees both coordination function (temporal encoding is genuine coordination problem) and extraction (English structure treated as unmarked).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY IDENTIFIED AND RESOLVED: The initial tension is between (1) treating tense-obligatory marking as a universal principle of human language (Mountain or Rope perspective), and (2) recognizing that Mandarin Chinese and hundreds of other languages function perfectly with aspect-context encoding (Snare perspective for speakers of these languages). The mandatrophy resolves by recognizing that the constraint is NOT a natural law about human language structure — it is an institutional choice about which language structure serves as the theoretical baseline. The tense-obligatory framework was developed for English and Indo-European languages and has been reified as a universal principle through peer review gatekeeping, textbook curricula, and career incentives. This is extraction (Snare for Mandarin speakers, Tangled Rope for aspect researchers) justified through false naturalization (the mountain perspective). The cognitive-functional observer's Tangled Rope classification captures the true structure: genuine coordination problem (need to encode time) + asymmetric extraction (English structure privileged as default). The piton perspective reveals that formalist elaboration has become increasingly performative as empirical evidence accumulates that tense obligatoriness is not a universal principle — the constraint persists through institutional inertia rather than explanatory necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligatory_vs_optional_tense,
    'Is tense in English truly obligatory in the sense of logical necessity, or merely obligatory in the sense of grammatical convention?',
    'Analysis of narrative null-tense contexts (telegraphic speech, stage directions, recipe instructions) where English omits tense markers; cross-linguistic survey of which languages have non-negotiable tense and which have optional systems; experimental evidence of processing cost for tense omission vs. aspect omission',
    'If logical necessity: tense is a cognitive universal and aspect is derivative (Mountain perspective justified). If grammatical convention: tense is an institutional choice reified as natural law (Snare perspective justified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(obligatory_vs_optional_tense, empirical, 'Whether English tense obligatoriness reflects cognitive necessity or institutional convention').

omega_variable(
    aspect_coverage_completeness,
    'Can Mandarin Chinese aspect-context encoding reliably express all temporal distinctions that English tense-aspect encode, or are there meaningful gaps?',
    'Systematic comparison of temporal meaning space: can Mandarin express perfective/imperfective/habitual/prospective meanings with equal precision and naturalness to English? Psycholinguistic evidence of comprehension difficulty when Mandarin speakers process complex tense sequences; analysis of how Mandarin translation handles English tense-heavy discourse (literature, legal contracts, scientific writing)',
    'If complete: aspect encoding is functionally equivalent and tense is extraction. If incomplete: tense obligatoriness may reflect genuine cognitive necessity for precise temporal reference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aspect_coverage_completeness, empirical, 'Whether Mandarin aspect-context encoding covers full temporal meaning space').

omega_variable(
    cognitive_load_and_processing,
    'Do obligatory tense systems impose higher cognitive load for temporal reasoning, or do they reduce load by making temporal structure explicit?',
    'Psycholinguistic experiments: reading times for tense-rich vs aspect-rich narratives; error rates in temporal comprehension tasks across English and Mandarin speakers; eye-tracking during processing of temporally complex events; neuroimaging of temporal reasoning in obligatory-tense vs aspect-first speakers',
    'If tense reduces load: obligatory marking is coordination function (Rope). If tense increases load: obligatory marking is extraction masquerading as efficiency (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_load_and_processing, empirical, 'Cognitive load imposed by obligatory tense vs aspect-context encoding').

omega_variable(
    formalist_explanatory_adequacy,
    'Does the tense-based Universal Grammar framework explain more linguistic phenomena with fewer parameters than aspect-first frameworks?',
    'Comparative theory assessment: (1) count free parameters and stipulations in each framework when applied to the same 200-language sample; (2) measure predictive accuracy for novel constructions; (3) evaluate parsimony of feature inventories; (4) assess whether apparent simplicity comes from ignoring data (English focus) vs genuine explanatory power',
    'If formalist tense framework is more parsimonious: coordination function is genuine (Rope). If simplicity comes from English-language bias: it is extraction justified by false efficiency (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalist_explanatory_adequacy, empirical, 'Comparative explanatory power of tense-based vs aspect-first formal frameworks').

omega_variable(
    mandarin_bilingual_acquisition,
    'When English-Mandarin bilingual children acquire tense, do they map it to pre-existing aspect-context categories, or do they create a novel cognitive category?',
    'Longitudinal acquisition study: track bilingual children''s tense and aspect marking in both languages; analyze interference patterns (does Mandarin aspect structure influence English tense development?); examine comprehension accuracy for tense vs aspect in each language during critical periods; compare acquisition trajectory to monolingual speakers in each language',
    'If tense maps to aspect: tense is a surface realization of underlying aspect (aspect is primary). If tense creates novel category: tense may reflect genuine cognitive feature (Mandarin perspective is deficit, not difference).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandarin_bilingual_acquisition, empirical, 'Cognitive basis of tense acquisition in bilingual Mandarin-English children').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(english_chinese_tense_structure, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ectensr_tr_t0, english_chinese_tense_structure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ectensr_tr_t40, english_chinese_tense_structure, theater_ratio, 40, 0.52).
narrative_ontology:measurement(ectensr_tr_t80, english_chinese_tense_structure, theater_ratio, 80, 0.58).

% Extraction over time
narrative_ontology:measurement(ectensr_be_t0, english_chinese_tense_structure, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ectensr_be_t40, english_chinese_tense_structure, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(ectensr_be_t80, english_chinese_tense_structure, base_extractiveness, 80, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(english_chinese_tense_structure, information_standard).
narrative_ontology:affects_constraint(english_chinese_tense_structure, linguistic_universals_genetic_hypothesis).
narrative_ontology:affects_constraint(english_chinese_tense_structure, sino_tibetan_aspect_grammaticalization).

% DUAL FORMULATION NOTE:
% The tense-obligatory constraint is downstream of broader debates about linguistic universals and the relationship between formal properties and cognitive necessity. It affects constraints on aspect grammaticalization in Sino-Tibetan languages by treating aspect as a secondary phenomenon. The three constraints form a family: the obligatory tense constraint operates at the level of formal theory; the linguistic universals constraint operates at the meta-theoretical level; the aspect grammaticalization constraint operates at the descriptive level for Sino-Tibetan languages.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(english_chinese_tense_structure, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
