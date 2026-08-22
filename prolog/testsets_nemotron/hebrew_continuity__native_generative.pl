% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity Through Native Generative Use
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the 'native generative' reading of Hebrew
 *   continuity: the claim that Hebrew lives *only* through native speaker
 *   intuition and daily generative use. This reading emerged from the Zionist
 *   revival project (1881 onward), was institutionalized through the Hebrew
 *   Language Academy (1953), and became the state-backed standard after 1948.
 *   It extracts authority and resources from competing Hebrew traditions —
 *   especially liturgical-only communities (Haredi, diaspora traditional)
 *   whose Hebrew is deemed 'dead' for lacking native child speakers. The
 *   constraint is a tangled rope: it performs genuine coordination (unifying
 *   a multilingual immigrant population into a single speech community) while
 *   asymmetrically extracting legitimacy from non-native Hebrew traditions.
 *   Active enforcement occurs through education law, Academy prescriptions,
 *   media norms, and the 'native speaker' gate in linguistic discourse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.35).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.65).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.35).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity Through Native Generative Use").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '7e2bfcee-9cc7-485a-a4f7-f61459ac923e').
narrative_ontology:cs_kernel_codification('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', formalized).
narrative_ontology:cs_authority_grounding('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', lineage).
narrative_ontology:cs_interpretation_layer_present('7e2bfcee-9cc7-485a-a4f7-f61459ac923e').
narrative_ontology:cs_reading_relation('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', foundational, native_child_speakers_required_for_vitality).
narrative_ontology:cs_axiom_status(native_child_speakers_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', native_child_speakers_required_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', foundational, generative_daily_use_as_vitality_criterion).
narrative_ontology:cs_axiom_status(generative_daily_use_as_vitality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', generative_daily_use_as_vitality_criterion, empirically_contingent).
narrative_ontology:cs_axiom('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', secondary, phonological_standardization_as_reconstruction).
narrative_ontology:cs_axiom_status(phonological_standardization_as_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', phonological_standardization_as_reconstruction, conventional).
narrative_ontology:cs_reference_frame('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', pre_state_hebrew_revival_ideology).
narrative_ontology:cs_drift_state('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', post_1948_state_institutionalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e2bfcee-9cc7-485a-a4f7-f61459ac923e', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_educational_institutions).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, modern_hebrew_literary_community).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_only_hebrew_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_hebrew_learners_without_immersion).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, traditional_religious_scholars_using_liturgical_hebrew).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Native Hebrew speakers in Israel constitute the living speech community whose intuition defines grammaticality and whose daily generative use creates new lexical items, syntactic patterns, and pragmatic norms. They benefit from institutional recognition of their variety as the standard, access to education and media in their native language, and the cultural capital of linguistic authenticity. Their exit options are strong — they can function fully in Hebrew across all domains, and English serves as a high-prestige alternative for international communication.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_hebrew_speakers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, native_hebrew_speakers, agenda_setter).

% The state education system (preschool through university) is the primary engine of Hebrew transmission and standardization. It sets curriculum, certifies teachers, administers matriculation exams, and defines the linguistic norms that legitimate native generative use. It benefits from a unified national language that enables civic cohesion and economic participation. Its exit is constrained — abandoning Hebrew as the medium of instruction would fracture the education system and require massive restructuring, but the institution could theoretically shift toward bilingual or English-medium models under political pressure.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% The Academy of the Hebrew Language (HaAkademia LaLashon HaIvrit) is the official state body tasked with prescriptive standardization: coining neologisms, setting orthography, ruling on grammar. It draws authority from the native generative reading — its legitimacy depends on the claim that it serves and reflects the living speech community. It benefits from institutional recognition, state funding, and intellectual authority. Its exit is constrained — it could not easily pivot to serving a different linguistic authority (e.g., liturgical Hebrew) without losing its statutory mandate and public relevance.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academy, agenda_setter,
    organized, generational, constrained, national).

% Writers, poets, journalists, and translators who create Hebrew literature and media. They benefit from a rich, expanding native lexicon and syntax that enables artistic expression. Their work both draws from and feeds the generative use that defines the constraint. Exit is mobile — they could write in other languages (English, Russian, Arabic, Yiddish) and some do, but Hebrew is their primary medium and audience.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, modern_hebrew_literary_community, beneficiary,
    organized, biographical, mobile, national).

% Haredi and other traditional communities worldwide whose Hebrew competence is confined to liturgical recitation, textual study, and ritual formula. Under the native generative reading, their Hebrew is classified as 'dead' or 'non-living' because it lacks native child speakers and daily generative use. They bear the cost of symbolic exclusion: their centuries-old transmission chain is delegitimized, their authority over the language is denied, and their educational systems (yeshivas, kollels) are treated as preserving a fossil rather than a living tongue. Exit is identity-locked — Hebrew is constitutive of their religious identity, communal boundary, and connection to sacred texts; abandoning or redefining it would dissolve the community's self-understanding.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_only_hebrew_communities, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, liturgical_only_hebrew_communities, excluded).

% Jewish diaspora learners who study Hebrew through supplementary schools, university courses, or apps but lack immersion environments. The native generative reading measures their Hebrew against the Israeli native-speaker norm, rendering their competence perpetually deficient. They bear the cost of a standard they cannot meet without aliyah or intensive immersion — their Hebrew is treated as 'not real' Hebrew. Exit is constrained — they can abandon Hebrew study, shift to English or local languages for Jewish expression, or pursue immersion at high personal cost.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_hebrew_learners_without_immersion, payer,
    moderate, biographical, constrained, global).

% Rabbis, poskim, and scholars whose Hebrew operates in the domain of halakhic discourse, responsa literature, and Torah commentary. Their Hebrew is generative within its domain (new responsa are composed daily) but is excluded by the native generative criterion because it lacks native child speakers and secular daily use. They bear the cost of having their linguistic creativity invisibilized and their authority over Hebrew's boundaries denied. Exit is identity-locked — their scholarly identity and religious authority are constituted through this Hebrew register.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, traditional_religious_scholars_using_liturgical_hebrew, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, traditional_religious_scholars_using_liturgical_hebrew, excluded).

% Researchers of language revitalization, contact linguistics, and sociolinguistics who study Hebrew as a unique case of a liturgical language acquiring native speakers. They observe the structural dynamics without personal stake in the vitality contest. Their analytical exit is unrestricted.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sociolinguistic_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, standardized national language for a diverse immigrant population, enabling civic participation, economic integration, scientific discourse, and cultural production across generations. The constraint coordinates lexical expansion, phonological convergence, and grammatical standardization around the native speaker intuition of the Israeli speech community.
% TRANSFER_FUNCTION: Moves linguistic authority, institutional resources, educational funding, and cultural legitimacy from liturgical/traditional Hebrew communities and diaspora learners to the Israeli native-speaker speech community and its state-backed institutions (education, Academy, media). The transfer is enacted through curriculum mandates, Academy rulings, media norms, and the 'native speaker' gatekeeping criterion.
% ABSENT_VOICES: Pre-state Hebrew revivalists who envisioned a bilingual Hebrew-Yiddish culture (marginalized by the monolingual ideology); Mizrahi Jews whose Arabic-inflected Hebrew was stigmatized by the European-derived standard; Palestinian citizens of Israel whose Arabic was suppressed to make space for Hebrew dominance; non-Zionist Haredi communities who reject the nationalist framing of Hebrew revival but are counted as 'liturgical-only' in this reading's victim set.
% DISAPPEARANCE_RATIONALE: If the native generative criterion vanished overnight, the Academy's prescriptive authority would lose its grounding, educational standards would fragment, diaspora Hebrew programs would lose their benchmark, and Haredi liturgical Hebrew would gain equal legitimacy — the entire institutional architecture of Modern Hebrew would reorganize around a pluralistic or liturgical-centered definition of 'living Hebrew.'
% FOUNDING_PROBLEM: The pre-state Yishuv faced a babel of Jewish languages (Yiddish, Ladino, Arabic, Russian, etc.) with no common spoken tongue for nation-building, defense, and civic life. The liturgical Hebrew of prayer and study had no native speakers and lacked vocabulary for modern concepts. The founding problem was creating a spoken national language from a textual tradition.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and the Academy attest the founding problem is solved (live Hebrew exists) but the *criterion* remains live because vitality requires continuous generative use. Haredi scholars and diaspora educators attest the founding problem was misdiagnosed — Hebrew was never dead, only dormant in speech, and the 'revival' created a new language (Israeli) distinct from Hebrew. Sociolinguists (Ghil'ad Zuckermann, Bernard Spolsky) corroborate the contested status: the 'revival' is better understood as a new language with Hebrew lexicon and European substrate, making the founding problem's framing itself contested.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).
:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects the real but partial transfer: the native generative standard enables massive coordination (education, science, law, culture) but delegates liturgical Hebrew to a subordinate status. Suppression (0.65) is high because the constraint's persistence depends on actively maintaining the native-speaker gate — through immigration policy (aliyah as immersion), education monopoly, and the delegitimization of diaspora Hebrew competence. Theater ratio (0.15) is low: the coordination function is real and the enforcement is functional, not performative. Accessibility collapse (0.72) is high but not total: liturgical Hebrew persists robustly outside the constraint's recognition. Resistance (0.45) is moderate: Haredi communities maintain parallel transmission; diaspora learners advocate alternative standards; scholars contest the 'Israeli vs Hebrew' distinction.
 *
 * PERSPECTIVAL GAP:
 *   From the native speaker seat, the constraint is a rope (genuine coordination enabling modern life). From the liturgical-only seat, it is a snare (extraction of legitimacy, suppression of their Hebrew). From the analyst seat, it is a tangled rope (both coordination and extraction are structurally real). The engine computes this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Native Hebrew speakers and Israeli institutions sit at the beneficiary/agenda-setter end (d ≈ 0.1–0.2): they collect linguistic authority, state resources, and cultural capital. Liturgical-only communities are identity-locked victims (d ≈ 0.9): their Hebrew is constitutively excluded, exit dissolves identity. Diaspora learners are constrained victims (d ≈ 0.7): they can exit to English but lose Jewish linguistic continuity. The Academy and education system are agenda-setters with constrained exit — they administer the constraint but depend on it for legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a spoken national language) is substantially solved — native Hebrew exists. But the constraint persists and intensifies because the *criterion* (native generative use as the *only* vitality metric) has become self-justifying: the Academy, education system, and demographic policy all require the 'living language' narrative to maintain their authority. Mandatrophy is unresolved — the coordination function is live, but the extraction of legitimacy from other Hebrew traditions has become a structural feature, not a transitional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_criterion_naturalness,
    'Is the ''native speaker intuition'' criterion a natural linguistic fact (languages require native speakers to be alive) or a constructed ideological boundary that excludes valid Hebrew traditions?',
    'Comparative analysis of other revitalized languages (Māori, Welsh, Hawaiian) where ''new native speakers'' emerge without delegitimizing traditional speakers; historical analysis of whether the criterion was adopted for scientific or political reasons in the Hebrew case.',
    'If natural, the constraint approaches mountain-like status for the coordination function; if constructed, the extraction of legitimacy from liturgical communities is an ideological choice, not a linguistic necessity — strengthening the snare reading for victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_criterion_naturalness, conceptual, 'Whether the native-speaker vitality criterion is a linguistic universal or a Zionist ideological construction.').

omega_variable(
    israeli_hebrew_continuity_with_historical_hebrew,
    'Is Modern Israeli Hebrew a direct continuation of historical Hebrew (Mishnaic/Biblical) or a new language (Israeli) with Hebrew lexicon and European substrate?',
    'Structural linguistic analysis (phonology, syntax, semantics) comparing Israeli Hebrew to historical strata and to Yiddish/Russian/Polish substrates; sociolinguistic analysis of speaker intuitions about continuity.',
    'If Israeli is a new language, the native generative reading''s claim to represent ''Hebrew continuity'' is a mislabeling — the constraint extracts the prestige of ''Hebrew'' for a different language. This would increase extractiveness for all victim seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(israeli_hebrew_continuity_with_historical_hebrew, empirical, 'The genetic/structural relationship between Israeli Hebrew and historical Hebrew — core to the kernel contest.').

omega_variable(
    suppression_mechanism_in_education_policy,
    'Is the suppression of liturgical Hebrew in state education structural (curriculum mandates, funding rules) or internalized (Haredi communities accepting ''dead language'' framing for their Hebrew)?',
    'Ethnographic study of Haredi Hebrew ideologies: do they accept the ''dead'' label or reject it as category error? Policy analysis of whether diaspora Hebrew programs are denied funding for not meeting native-speaker benchmarks.',
    'If internalized, effective suppression is higher than structural measures suggest — the victims carry the constraint''s delegitimization internally. If purely structural, exit (though identity-locked) remains conceptually possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_education_policy, empirical, 'Structural vs. internalized suppression of non-native Hebrew traditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 1881, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1881, hebrew_continuity__native_generative, theater_ratio, 1881, 0.05).
narrative_ontology:measurement(hebr_tr_t1922, hebrew_continuity__native_generative, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_continuity__native_generative, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(hebr_tr_t1967, hebrew_continuity__native_generative, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_continuity__native_generative, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_continuity__native_generative, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1881, hebrew_continuity__native_generative, base_extractiveness, 1881, 0.15).
narrative_ontology:measurement(hebr_be_t1922, hebrew_continuity__native_generative, base_extractiveness, 1922, 0.25).
narrative_ontology:measurement(hebr_be_t1948, hebrew_continuity__native_generative, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(hebr_be_t1967, hebrew_continuity__native_generative, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(hebr_be_t1990, hebrew_continuity__native_generative, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__native_generative, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1881, hebrew_continuity__native_generative, suppression_requirement, 1881, 0.2).
narrative_ontology:measurement(hebr_su_t1922, hebrew_continuity__native_generative, suppression_requirement, 1922, 0.4).
narrative_ontology:measurement(hebr_su_t1948, hebrew_continuity__native_generative, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(hebr_su_t1967, hebrew_continuity__native_generative, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(hebr_su_t1990, hebrew_continuity__native_generative, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__native_generative, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.08).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, israeli_language_education_policy).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, diaspora_hebrew_education_funding).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, academy_of_hebrew_language_mandate).

% DUAL FORMULATION NOTE:
% This constraint is one member of the hebrew_continuity kernel family. The native_generative reading claims Hebrew vitality requires native child speakers and daily generative use. The liturgical_preservation reading claims vitality through ritual recitation and textual transmission. The bridge_pidginized reading claims vitality through diaspora contact use. Their ε values differ substantially: native_generative (ε≈0.35, tangled_rope), liturgical_preservation (ε≈0.15, rope/scaffold), bridge_pidginized (ε≈0.25, rope). They are linked by shared referent (Hebrew continuity) but structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__native_generative, institutional, 0.15).
constraint_indexing:directionality_override(hebrew_continuity__native_generative, organized, 0.85).
constraint_indexing:directionality_override(hebrew_continuity__native_generative, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
