% ============================================================================
% CONSTRAINT STORY: english_chinese_tense_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Obligatory Tense Marking vs. Aspect-Context Encoding in English-Chinese Linguistic Structure
 *   domain: linguistic/cognitive_science
 *
 * SUMMARY:
 *   The obligatory tense marking system in English, compared to
 *   aspect-context encoding in Chinese, creates a structural constraint in
 *   cross-linguistic learning and typological understanding. English
 *   grammaticizes temporal reference through morphological inflection on
 *   finite verbs (walked, walk, will walk), creating a gate that
 *   Chinese-speaking learners must traverse without functional equivalent in
 *   their L1 grammar. This constraint exhibits simultaneously a coordination
 *   function (tense helps disambiguate temporal reference in edge cases) and
 *   an extraction function (learners bear disproportionate cost of
 *   morphological system that conveys semantics already encoded in aspect and
 *   context). The theater_ratio (0.58) reflects that traditional English
 *   grammar pedagogy treats tense as a primary semantic unit through explicit
 *   morphological drilling, despite cognitive science evidence that learners
 *   extract temporal reference primarily from aspect-context patterns in
 *   input, not from tense morphology. The constraint's extractiveness
 *   increased over the interval (0.22 → 0.38) because pedagogical enforcement
 *   intensified as standardized testing regimes (TOEFL, IELTS, Cambridge
 *   examinations) made tense accuracy high-stakes, while simultaneously
 *   cognitive linguistic research accumulated evidence that tense is
 *   functionally redundant. Theater ratio rose (0.42 → 0.58) as the gap
 *   widened between pedagogical emphasis and cognitive reality.
 *
 * KEY AGENTS:
 *   - Chinese Speakers Learning English: Primary victims (powerless/trapped) — no functional exit from obligatory tense gate; bear full cost of acquiring redundant marking system
 *   - English Speakers Learning Chinese: Secondary victims (moderate/constrained) — experience loss of morphological marking but gain semantic clarity; exit available through explicit temporal deixis
 *   - English Language Institution: Primary beneficiary (institutional/arbitrage) — maintains tense morphology as legacy coordinating mechanism; experiences system as functionally necessary
 *   - Language Pedagogy & Testing: Extractor/beneficiary (institutional/arbitrage) — enforces tense morphology through curricula and high-stakes testing regimes; captures value through certification gatekeeping
 *   - SLA Research Community: Organized agent (organized/constrained) — building alternative frameworks treating tense as derivative; creating sunset pathway through Aspect Hypothesis and frequency-based models
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional pedagogical choice as inherent language typology feature
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
narrative_ontology:human_readable(english_chinese_tense_structure, "Obligatory Tense Marking vs. Aspect-Context Encoding in English-Chinese Linguistic Structure").
narrative_ontology:topic_domain(english_chinese_tense_structure, "linguistic/cognitive_science").

domain_priors:requires_active_enforcement(english_chinese_tense_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(english_chinese_tense_structure, english_language_cognition).
narrative_ontology:constraint_beneficiary(english_chinese_tense_structure, formal_grammar_parsimony).
narrative_ontology:constraint_victim(english_chinese_tense_structure, chinese_language_expressiveness).
narrative_ontology:constraint_victim(english_chinese_tense_structure, cross_linguistic_translation_fidelity).
narrative_ontology:constraint_victim(english_chinese_tense_structure, language_learners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE SPEAKER / LANGUAGE LEARNER (SNARE) — Trapped in mandatory tense inflection system with no functional equivalent in native grammar. Bears full cost of acquiring redundant marking system that conveys no new semantic information beyond aspect-context already encoded. No exit option — tense morphology is enforced by English grammar gate. Maximum experienced extraction.
constraint_indexing:constraint_classification(english_chinese_tense_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENGLISH SPEAKER / LANGUAGE LEARNER (TANGLED ROPE) — Constrained by loss of overt tense marking, but benefits from reduced morphological complexity and closer alignment with semantic primitives (aspect + context). Moderate extraction: gains semantic clarity but loses morphological shortcuts developed in L1. Exit is constrained but available through code-switching or explicit temporal deixis.
constraint_indexing:constraint_classification(english_chinese_tense_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENGLISH LANGUAGE INSTITUTION (ROPE) — Maintains tense morphology as coordinating mechanism for temporal reference across diverse pragmatic contexts. Benefits from inherited system that provides redundant encoding (tense + aspect + context) that disambiguates edge cases. Low suppression from this perspective: tense system enables communication, and English speakers experience it as natural.
constraint_indexing:constraint_classification(english_chinese_tense_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SLA RESEARCH & PEDAGOGY (SCAFFOLD) — Organized agents (cognitive linguists, SLA researchers, pedagogues) are building alternative frameworks that treat tense as derivative from aspect-context rather than primitive. Interacting Factors Hypothesis, Aspect Hypothesis, and frequency-based learning models all bypass mandatory tense gate by reframing it as learnable through input patterns rather than system requirement. Sunset clause: as evidence accumulates that aspect-context suffices, mandatory tense marking is reconceptualized as a performance variant rather than a competence requirement.
constraint_indexing:constraint_classification(english_chinese_tense_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL GRAMMAR PEDAGOGY (PITON) — Tense morphology persists in language teaching as performative gate: textbooks drill tense inflection as if it were a primary semantic unit, despite cognitive science evidence that learners parse it from aspect-context. The pedagogical theater (tense conjugation tables, 'past simple vs. past continuous' contrast drills) maintains institutional inertia through published curricula and testing regimes (TOEFL, IELTS), not because the system is functionally optimal. Seen from inside pedagogy, the constraint is degraded — teachers recognize that learners acquire tense from input patterns, not from explicit rules.
constraint_indexing:constraint_classification(english_chinese_tense_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TYPOLOGICAL NATURALISM (MOUNTAIN) — From civilizational/universal perspective, obligatory tense marking appears as a natural typological feature: English has grammaticized temporal reference through morphology; Chinese has not. This classification treats the constraint as a brute language typology fact — immutable feature of the English language system. However, structural data contradicts mountain classification: the constraint depends on enforcement through pedagogy and L1-to-L2 transfer, not on irreducible logical/physical limits. Engine false summit detection reveals that 'language typology' naturalizes what is actually an institutional teaching regime.
constraint_indexing:constraint_classification(english_chinese_tense_structure, mountain,
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
 *   Extractiveness (0.38): Moderate. The tense system imposes costs on L2 learners (acquisition burden, error production, cognitive load) but these costs are not absolute. Learners can and do acquire tense through input exposure and aspect-context patterns without explicit instruction. The extraction is real (non-native speakers bear disproportionate cost relative to native speakers) but not maximal because the system provides genuine disambiguation function in edge cases (distinguishing 'I worked there' from 'I work there' from 'I am working there'). The value reflects that part of the cost is legitimate learning overhead (all L2 morphology is costly), and part is extractive redundancy (aspect-context already encodes this information). Suppression (0.52): Moderate-high. Significant barriers include: lack of functional equivalents in Asian languages (Chinese, Korean, Japanese), making the morphosyntactic category conceptually opaque; high-stakes standardized testing that gatekeeps tense accuracy; pedagogical emphasis on explicit morphological rules rather than pattern-based acquisition; publication bias toward 'correct' tense in academic writing. Suppression is not total because learners can achieve communicative competence with systematic tense errors, and creole languages and advanced learner varieties show that communication succeeds with aspect-context alone. Theater ratio (0.58): Moderate-high. Pedagogical performance includes explicit tense drilling (conjugation tables, contrast exercises) that occupies substantial instructional time despite low correlation with actual acquisition. Textbooks present tense as discrete morphosyntactic category rather than as derivative feature emerging from aspect-context computation. Yet the theater is not overwhelming (0.7+) because tense does provide genuine disambiguation and English speakers do need morphological accuracy for standardized contexts.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival divergence between beneficiary and victim. The English institution (Rope perspective) sees tense as solving a real coordination problem: disambiguation in contexts where aspect and context alone are ambiguous. Chinese learners (Snare perspective) see tense as pure extraction: a morphological burden conveying information already available from aspect-context. The pedagogical system (Piton perspective) sees tense as largely performative: drilling continues through curricular inertia, not because learners acquire it through explicit rules. The SLA research community (Scaffold perspective) sees a sunset: frequency-based models and aspect-first pedagogy are building an alternative where tense emerges from input patterns rather than explicit instruction, making the constraint time-limited. The analytical observer risks the mountain classification by treating English typology as a brute fact of the language system rather than as an institutional pedagogical choice. The perspectival gap widens over the interval as cognitive linguistic evidence accumulated (2000-2020) that tense is a late-acquired derivation rather than a core semantic unit, while pedagogical enforcement intensified through standardized testing.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain places Chinese-speaking learners at high d (near 1.0, victim/trapped status): no functional L1 equivalent, no bypass option, disproportionate cost relative to benefit. They experience maximum f(d) and thus maximum effective extraction chi. English speakers learning Chinese occupy lower d (~0.55, constrained exit): they lose marking but gain clarity, and they have partial workarounds (explicit temporal deixis, aspectual particles). The English language institution occupies low d (~0.15, beneficiary with arbitrage): they experience tense as coordinating mechanism that is optional to use but available to choose. Pedagogical enforcement raises d slightly for all learners (trapped exit option gets higher d than arbitrage). The SLA research community occupies d around 0.40-0.50 (organized agents with constrained exit): they have agency to reframe the constraint but face institutional resistance from testing regimes and published curricula.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the 'is tense marking coordination or extraction' question has no single answer — it is both, perspectivally. For the English institution maintaining the morphosyntactic system, tense is genuine coordination: it disambiguates temporal reference in the small set of contexts where aspect-context fail. For the Chinese-speaking learner, the same morphosyntax is extraction: the learner bears costs (acquisition burden, production errors, cognitive load) without semantic gain (aspect-context already encodes the temporal information). The Tangled Rope classification resolves the mandatrophy by acknowledging both the coordination function (required for the rope perspective to classify correctly) and the extraction function (required for the snare perspective to classify correctly). The Scaffold perspective shows that the constraint is time-limited: as SLA pedagogy shifts to frequency-based and aspect-first models, the extractive gate is being dismantled while the coordination function is preserved (learners still produce tense-marked output, but they acquire it through input patterns rather than explicit instruction). The false summit (mountain perspective) is detected because tense marking is not a law of nature or irreducible logical limit — it is an institutional choice that alternative language systems (Chinese, creole languages) handle identically through aspect-context encoding, proving that the constraint is contingent on English's specific historical development, not on any universal requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aspect_context_sufficiency,
    'Do aspect-context markers alone provide equivalent temporal reference specificity to obligatory tense marking for all English discourse contexts?',
    'Corpus analysis comparing tense + aspect vs. aspect + context in disambiguating temporal reference; psycholinguistic experiments measuring processing costs of ambiguous temporal reference; translation adequacy studies (English to context-marking languages)',
    'If sufficient: tense is redundant extraction (strengthens snare classification). If insufficient: tense serves genuine coordination function (strengthens rope classification from beneficiary perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aspect_context_sufficiency, empirical, 'Whether aspect-context encoding provides equivalent temporal specificity to tense morphology').

omega_variable(
    native_speaker_tense_processing,
    'Do native English speakers process tense morphology consciously as semantic marker, or extract temporal reference primarily from aspect-context with tense as redundant signal?',
    'Eye-tracking during reading with tense-stripped vs. tense-intact inputs; ERP studies measuring neural response to tense violations vs. aspect/context violations; corpus analysis of spontaneous speech with explicit vs. implicit temporal reference',
    'If primarily aspect-context: tense is performative (theater_ratio justifies rise). If conscious processing required: tense is functional coordination (theater_ratio overestimated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(native_speaker_tense_processing, empirical, 'Native speaker processing of tense as explicit vs. redundant signal').

omega_variable(
    creole_emergence_trajectory,
    'When pidgins grammaticize into creoles, do they reinvent obligatory tense morphology independently, or do they persist with aspect-context encoding?',
    'Comparative creolistics: survey obligatory vs. optional tense marking across 200+ creoles; historical reconstruction of contact languages; documentation of emerging creoles (Nicaraguan Sign Language, Nicaraguan creole English)',
    'If tense reinvents independently: suggests functional necessity (strengthen rope). If aspect-context stable across creolization: suggests tense is extractive overlay (strengthen snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creole_emergence_trajectory, empirical, 'Tense marking emergence pattern across creole languages').

omega_variable(
    l2_acquisition_cognitive_load,
    'Does obligatory tense morphology impose measurable cognitive load during L2 production that cannot be explained by other morphological complexity factors?',
    'Controlled L2 production experiments with eye-tracking, speech timing analysis, and error coding; comparison of L2 acquisition trajectories in tense-obligatory vs. tense-optional languages, controlling for other morphological complexity; longitudinal corpus analysis of L2 learner English tense omission patterns',
    'If significant independent load: validates snare victim status. If load attributable to general morphological complexity: suggests extraction is secondary effect (reduce extractiveness estimate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(l2_acquisition_cognitive_load, empirical, 'Cognitive load of tense morphology independent of general morphological complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(english_chinese_tense_structure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(engtcn_tr_t0, english_chinese_tense_structure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(engtcn_tr_t10, english_chinese_tense_structure, theater_ratio, 10, 0.51).
narrative_ontology:measurement(engtcn_tr_t20, english_chinese_tense_structure, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(engtcn_be_t0, english_chinese_tense_structure, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(engtcn_be_t10, english_chinese_tense_structure, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(engtcn_be_t20, english_chinese_tense_structure, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(english_chinese_tense_structure, information_standard).
narrative_ontology:affects_constraint(english_chinese_tense_structure, l2_morphological_acquisition_complexity).
narrative_ontology:affects_constraint(english_chinese_tense_structure, cross_linguistic_transfer_asymmetry).

% DUAL FORMULATION NOTE:
% Obligatory tense marking in English is downstream of historical grammaticization processes (Romance/Germanic verb system evolution) but represents a distinct structural constraint on current language learners. The upstream constraints have their own extractiveness values reflecting typological typicality; the tense structure constraint has its own extractiveness reflecting contemporary pedagogical enforcement and L2 acquisition burden.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(english_chinese_tense_structure, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
