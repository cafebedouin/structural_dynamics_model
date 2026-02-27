% ============================================================================
% CONSTRAINT STORY: sapir_whorf_hypothesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sapir_whorf_hypothesis, []).

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
 *   constraint_id: sapir_whorf_hypothesis
 *   human_readable: Sapir-Whorf Hypothesis (Linguistic Relativity)
 *   domain: social/cognitive/linguistics
 *
 * SUMMARY:
 *   The Sapir-Whorf hypothesis (linguistic relativity) creates a structural
 *   constraint operating on both individual cognition and institutional
 *   knowledge production. At its core, the hypothesis proposes that language
 *   structure shapes thought — a claim that is intuitively appealing but
 *   empirically contested. The constraint operates as a tangled hybrid: it
 *   genuinely coordinates language communities (people speaking the same
 *   language develop shared conceptual frameworks useful for collective
 *   life), but it simultaneously extracts from speakers of minority or
 *   non-dominant languages by constraining their cognitive categories and
 *   limiting their access to dominant-language epistemic institutions. The
 *   strong form of the hypothesis (language determines thought) has been
 *   largely refuted by modern cognitive science, yet persists in educational
 *   practice, linguistic activism, and popular discourse through
 *   institutional inertia and ideological commitment — the piton pattern.
 *   Simultaneously, a weak reformulation (language influences cognition) is
 *   being empirically grounded through neuroscience and cross-linguistic
 *   behavioral research, following the scaffold pattern. The constraint thus
 *   exhibits all six DR types depending on observational position: snare for
 *   minority speakers, piton for strong-doctrine persistence, scaffold for
 *   cognitive linguistics movement reform, rope for institutional linguistic
 *   coordination, and mountain only as a false summit risking naturalization
 *   of contingent arrangements.
 *
 * KEY AGENTS:
 *   - Minority Language Speakers: Primary victims (powerless/trapped) — cognitive categories constrained by minority language structure; limited access to dominant-language epistemic resources
 *   - Dominant Language Speakers (English): Primary beneficiary (institutional/arbitrage) — cognition benefited by linguistically-codified conceptual distinctions; access to globally-dominant scientific/academic institutions
 *   - Bilingual Translators: Secondary victims (moderate/constrained) — face untranslatability costs and cognitive burden of mapping incommensurable categories; also benefit from mediation role
 *   - Cognitive Linguistics Researchers: Organized reformers (organized/constrained) — building empirically-grounded weak Whorfian framework with explicit sunset for strong doctrine
 *   - Educational Institutions: Institutional enforcement (institutional/arbitrage) — perpetuate strong Whorfian doctrine through curricula despite weak empirical foundation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing language-cognition binding as law rather than contingent structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sapir_whorf_hypothesis, 0.38).
domain_priors:suppression_score(sapir_whorf_hypothesis, 0.52).
domain_priors:theater_ratio(sapir_whorf_hypothesis, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, extractiveness, 0.38).
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sapir_whorf_hypothesis, tangled_rope).
narrative_ontology:human_readable(sapir_whorf_hypothesis, "Sapir-Whorf Hypothesis (Linguistic Relativity)").
narrative_ontology:topic_domain(sapir_whorf_hypothesis, "social/cognitive/linguistics").

domain_priors:requires_active_enforcement(sapir_whorf_hypothesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sapir_whorf_hypothesis, dominant_language_speakers).
narrative_ontology:constraint_beneficiary(sapir_whorf_hypothesis, linguistic_research_institutions).
narrative_ontology:constraint_victim(sapir_whorf_hypothesis, minority_language_communities).
narrative_ontology:constraint_victim(sapir_whorf_hypothesis, cross_linguistic_translation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY LANGUAGE SPEAKER (SNARE) — Trapped within linguistic structure that constrains cognitive categories and expressive possibilities. Cannot easily exit language community. Bears cost of cognitive limitation relative to dominant-language speakers. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BILINGUAL TRANSLATOR (TANGLED ROPE) — Constrained by need to map incommensurable conceptual categories across languages, but also benefits from position enabling cross-cultural mediation and cognitive flexibility. Experiences both extraction (untranslatable nuance loss) and coordination benefit (bridge function). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT LANGUAGE RESEARCH COMMUNITY (ROPE) — English-speaking scientific institutions benefit from linguistic coordination advantage: their language becomes default for knowledge dissemination. This appears as coordination (shared knowledge standard) rather than extraction because the mechanism is institutional convention rather than coercive suppression. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Net beneficiary through convention.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STRONG WHORFIAN DOCTRINE (PITON) — The strong form (language DETERMINES thought) has been empirically refuted but persists in educational theory, linguistic activism, and popular discourse through institutional inertia. theater_ratio=0.68 indicates substantial performative commitment despite weak evidential foundation. The doctrine is maintained through citation cascades and ideological investment, not through functional verification.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COGNITIVE LINGUISTICS MOVEMENT (SCAFFOLD) — Organized reformulation of weak Whorfian claims (language influences, not determines thought) with empirical grounding and sunset logic. Structured field research, neural imaging, and cross-cultural validation are replacing ideological assertion. The movement explicitly targets replacement of strong doctrine with testable weak form. has_sunset_clause implicit: strong Whorfian framework is being methodically displaced by evidence-based alternatives over generational timescale. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.22.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At civilizational/universal perspective, some binding between linguistic structure and cognitive category formation is inevitable from embodied cognition principles: thought is partially linguistic; language is partially structured by extralinguistic reality. The fundamental fact that language both shapes and reflects cognition may be immutable. However, structural data (ε=0.38, suppression=0.52, theater=0.68) contradicts pure mountain classification — the constraint's extractive and performative components are contingent, not necessary. Engine will flag as false summit.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sapir_whorf_hypothesis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sapir_whorf_hypothesis, TR),
    TR >= 0.70.

:- end_tests(sapir_whorf_hypothesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint operates through cognitive limitation and institutional access barriers rather than direct coercion. Minority speakers genuinely experience reduced conceptual resolution in domains where their language lacks granular distinctions (e.g., time, kinship, color) relative to dominant speakers. However, the extraction is not totalizing — humans show remarkable cognitive plasticity and can acquire dominant-language concepts through learning. The measurement shows trajectory rising from 0.20 (1960s: hypothesis accepted without strong evidence) to 0.38 (2024: empirically contested but institutionally embedded). Suppression (0.52): Moderate-high. Significant barriers to exit include (1) language acquisition cost in adulthood, (2) cultural/identity attachment to minority language, (3) institutional bias favoring dominant languages in science, law, governance. Not total suppression because (1) multilingualism is possible, (2) some minority-language domains maintain superiority (e.g., Inuit snow terminology), (3) machine translation reduces some barriers. Theater ratio (0.68): High. The constraint's performative component has increased over 60 years. Strong Whorf doctrine persists in educational curricula and cultural activism despite empirical refutation, maintained through citation cascades and ideological investment rather than evidential support. Modern cognitive linguistics explicitly targets replacement of performative strong doctrine with testable weak claims, suggesting scaffold sunset mechanism is operational.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The minority speaker experiences snare (trapped in constrained cognition). The bilingual translator experiences tangled rope (extraction via untranslatability cost, but coordination benefit via mediation). The dominant-language institution experiences rope (coordination advantage feels natural, not extractive). The strong-doctrine perpetuator experiences piton (doctrine persists through institutional inertia despite empirical weakness). The cognitive linguistics reformer experiences scaffold (weak doctrine replacing strong through field research, with explicit sunset for discredited claims). The civilization-level analytical observer risks mountain (naturalizing language-cognition binding as immutable law). The perspectival gap is widest between snare and rope: identical linguistic structure feels like extraction to minority speaker, coordination to dominant speaker. This asymmetry drives the tangled_rope classification at moderate levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant language speakers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Linguistic structure benefits them through institutional convention (English = science lingua franca). Minority language speakers: Victim + trapped → d≈0.92, f(d)≈1.38. Cognitive categories constrained; cannot easily exit language community. Bilingual translators: Victim + constrained → d≈0.68, f(d)≈1.05. Trapped by untranslatability problem; benefit from mediation role provides partial exit. Cognitive linguistics institutions: Organized + constrained → d≈0.45, f(d)≈0.48. Constrained by need to maintain rigorous evidence standards; benefit from epistemic authority position. Educational institutions (strong-doctrine perpetuators): Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Piton classification emerges from theater gate (≥0.70 would trigger piton, here 0.68 is borderline; actual piton emerges from doctrine persistence despite refutation, not from high chi).
 *
 * MANDATROPHY ANALYSIS:
 *   KEY ISSUE: The constraint exhibits classic mandatrophy — the strong Whorfian claim (language determines thought) is treated as both an empirical hypothesis AND a moral/political principle. This creates tension: (1) Empirically, strong Whorf is refuted (cognition is not wholly determined by language — humans show cognitive plasticity, cross-linguistic conceptual translation works). (2) Morally/politically, some version of linguistic relativity is deployed to defend minority language preservation (if your language encodes unique worldview, it deserves protection). The mandatrophy resolves by recognizing these are SEPARATE constraints: (A) the empirical claim 'language determines cognition' (ε≈0.08, Mountain or Rope), (B) the institutional perpetuation of strong doctrine despite refutation (ε≈0.68, Piton), (C) the moral claim 'linguistic diversity has value' (separate normative constraint). The confusion arises because agents conflate empirical falsity with moral irrelevance. The response is constraint decomposition: write separate stories for empirical linguistic relativity vs. institutional strong-Whorf persistence vs. linguistic-diversity moral principle. The tangled_rope classification at ε=0.38 reflects the ACTUAL constraint experienced by language communities: a mix of genuine cognitive influence (coordination) + institutional barriers (extraction) + performative strong-doctrine commitment. This honest classification prevents false naturalization of contingent institutional bias as empirical scientific fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    language_determines_vs_influences,
    'Does language determine cognition (strong Whorf) or merely influence it (weak Whorf)? Is this a binary or a spectrum?',
    'Neuroscience (fMRI during language use and abstract thought), cross-linguistic behavioral experiments (color categorization, spatial reasoning, temporal concepts), longitudinal studies of linguistic change and cognitive shift in language communities',
    'Strong determination: snare classification confirmed for all non-dominant speakers. Mere influence: scaffold/rope classifications become dominant; constraint''s extractiveness drops to ε≤0.15.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(language_determines_vs_influences, empirical, 'Whether language determines or merely influences cognition').

omega_variable(
    incommensurability_vs_translation,
    'Are certain concepts truly untranslatable (linguistic incommensurability), or is apparent untranslatability a failure of translation effort rather than a law of language structure?',
    'Comparative analysis of technical translation success rates; etymology tracing of concept loans across language families; machine translation capability benchmarks on cultural-semantic phenomena; long-term tracking of whether ''untranslatable'' concepts acquire translation equivalents over decades',
    'True incommensurability: suppression and extraction gates hold; victims genuinely trapped. False incommensurability: suppression drops; constraint becomes coordination problem (Rope) rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incommensurability_vs_translation, empirical, 'Whether linguistic incommensurability is structural or translatable').

omega_variable(
    dominant_language_advantage_causation,
    'Does English dominance in global science cause epistemic bias, or does English dominance result from pre-existing epistemic advantage (better scientific institutions)? Which direction runs the causation?',
    'Historical analysis of parallel research communities in non-English languages (Soviet physics, Japanese materials science); counterfactual reconstruction of scientific epistemic status if German had remained dominant lingua franca post-WWII; analysis of whether non-English scientific communities show systematically different conceptual frameworks or merely translation lag',
    'Language causes epistemic bias: snare for non-English speakers. Institutional advantage causes language dominance: constraint is institutional favoritism (separate constraint), not Whorfian binding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_language_advantage_causation, empirical, 'Whether linguistic dominance causes or results from epistemic advantage').

omega_variable(
    weak_whorf_empirical_status,
    'Is the weak Whorfian claim (language influences cognition) actually empirically testable, or is it unfalsifiable because any cognitive difference could be attributed to either language or non-linguistic culture?',
    'Philosophy of science analysis of Whorfian claims structure; review of experimental designs attempting to isolate language effects from cultural/environmental confounds; meta-analysis of replication rates for weak Whorf experimental evidence',
    'If unfalsifiable: constraint is theater (piton confirmed). If testable: weak Whorf can be refined to measurable form and strong doctrine displaced by scaffold mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(weak_whorf_empirical_status, conceptual, 'Whether weak Whorfian claims are empirically testable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sapir_whorf_hypothesis, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swh_tr_t0, sapir_whorf_hypothesis, theater_ratio, 0, 0.42).
narrative_ontology:measurement(swh_tr_t30, sapir_whorf_hypothesis, theater_ratio, 30, 0.58).
narrative_ontology:measurement(swh_tr_t60, sapir_whorf_hypothesis, theater_ratio, 60, 0.68).

% Extraction over time
narrative_ontology:measurement(swh_be_t0, sapir_whorf_hypothesis, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(swh_be_t30, sapir_whorf_hypothesis, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(swh_be_t60, sapir_whorf_hypothesis, base_extractiveness, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sapir_whorf_hypothesis, information_standard).
narrative_ontology:affects_constraint(sapir_whorf_hypothesis, english_lingua_franca_dominance).
narrative_ontology:affects_constraint(sapir_whorf_hypothesis, conceptual_incommensurability_thesis).

% DUAL FORMULATION NOTE:
% The Sapir-Whorf hypothesis conflates multiple distinct constraints: (1) linguistic_relativity_empirical (ε≈0.08, whether language influences cognition — largely confirmed in weak form), (2) strong_whorf_institutional_persistence (ε≈0.68, performative perpetuation of refuted doctrine — piton), (3) linguistic_diversity_moral_claim (normative constraint, separate framework). This story focuses on the institutional/cognitive binding constraint (ε=0.38). The upstream constraint is english_lingua_franca_dominance (institutional favoritism), which creates structural conditions enabling Whorfian extraction. Downstream is conceptual_incommensurability_thesis (empirical claim about translation limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sapir_whorf_hypothesis, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
