% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Native-Generative Standard as the Criterion of Hebrew Vitality
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Hebrew-continuity
 *   kernel: the claim that Hebrew lives ONLY through native speaker intuition
 *   and daily generative use. On this reading, the Ben-Yehuda-era
 *   vernacularization project and the subsequent Israeli Hebrew Language
 *   Academy's standard-setting apparatus constitute the authoritative kernel
 *   of what counts as a 'living' Hebrew. The coordination function is genuine
 *   — a modern state needed a shared, generatively teachable vernacular. But
 *   the same standard that solved that problem now operates to reclassify
 *   centuries of liturgical, diglossic, and heritage-language Hebrew practice
 *   as inferior or dead, transferring cultural legitimacy and institutional
 *   resources toward native-Israeli speech communities and away from diaspora
 *   and religious communities whose relationship to Hebrew is real but
 *   structurally different. Sibling readings (liturgical_preservation,
 *   bridge_pidginized) are NOT part of this story — they are separate
 *   constraints with their own ε and their own beneficiary/victim structures,
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - israeli_hebrew_language_academy: institutional agenda-setter, certifies authentic generative usage
 *   - sabra_native_speaker_generation: beneficiary, native intuition becomes the reference standard
 *   - zionist_national_revival_project: institutional beneficiary, needs vernacularization as proof of national resurrection
 *   - diaspora_liturgical_hebrew_communities: payer, centuries of textual transmission reclassified as 'not alive'
 *   - ultra_orthodox_loshn_koydesh_communities: payer/excluded, diglossic theological practice treated as failed revival rather than alternative continuity
 *   - historical_linguists_and_hebraists: analytical observer, evaluates competing definitions of linguistic vitality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.58).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.62).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Native-Generative Standard as the Criterion of Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, 'c6509f7b-667e-4cc1-b086-b63a92470499').
narrative_ontology:cs_kernel_codification('c6509f7b-667e-4cc1-b086-b63a92470499', formalized).
narrative_ontology:cs_authority_grounding('c6509f7b-667e-4cc1-b086-b63a92470499', expertise).
narrative_ontology:cs_interpretation_layer_present('c6509f7b-667e-4cc1-b086-b63a92470499').
narrative_ontology:cs_reading_relation('c6509f7b-667e-4cc1-b086-b63a92470499', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('c6509f7b-667e-4cc1-b086-b63a92470499', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('c6509f7b-667e-4cc1-b086-b63a92470499', foundational, only_native_child_acquisition_constitutes_life).
narrative_ontology:cs_axiom_status(only_native_child_acquisition_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('c6509f7b-667e-4cc1-b086-b63a92470499', only_native_child_acquisition_constitutes_life, conventional).
narrative_ontology:cs_axiom('c6509f7b-667e-4cc1-b086-b63a92470499', secondary, generative_daily_use_is_necessary_not_merely_sufficient).
narrative_ontology:cs_axiom_status(generative_daily_use_is_necessary_not_merely_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('c6509f7b-667e-4cc1-b086-b63a92470499', generative_daily_use_is_necessary_not_merely_sufficient, instrumental).
narrative_ontology:cs_reference_frame('c6509f7b-667e-4cc1-b086-b63a92470499', ben_yehuda_vernacular_revival_founding).
narrative_ontology:cs_drift_state('c6509f7b-667e-4cc1-b086-b63a92470499', contemporary_israeli_hebrew, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6509f7b-667e-4cc1-b086-b63a92470499', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, sabra_native_speaker_generation).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, zionist_national_revival_project).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_state_education_system).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_liturgical_hebrew_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, yiddish_and_ladino_heritage_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, elderly_immigrant_hebrew_learners).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, ultra_orthodox_loshn_koydesh_communities).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, language_revival_is_possible_through_native_transmission).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, vernacularization_precedes_national_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the standard of what counts as living Hebrew: coins new vocabulary, standardizes phonology, and certifies which forms are 'authentic' generative usage versus archaic or foreign-influenced speech. Its authority rests entirely on the claim that only naturally reproducing, child-acquired Hebrew is truly alive.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_hebrew_language_academy, agenda_setter,
    institutional, generational, analytical, national).

% Acquired Hebrew as a first language from birth in Mandate Palestine and later Israel. Their intuitive command of the language becomes the reference standard against which all other Hebrew competence is measured; they occupy the linguistic center of gravity and inherit cultural and institutional authority simply by virtue of how they learned to speak.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sabra_native_speaker_generation, beneficiary,
    organized, biographical, mobile, national).

% The nation-building enterprise that required a modern vernacular capable of carrying secular, technical, and everyday life to ground a new national identity distinct from diaspora religious life. It benefits enormously from the native-generative standard because it supplies the legitimating proof that a 'dead' language was resurrected into a living nation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, zionist_national_revival_project, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, zionist_national_revival_project, agenda_setter).

% Builds curricula, immersion schooling, and ulpan programs around the premise that full Hebrew competence means native-like generative fluency. Immigrant children are pushed hard toward native-equivalent acquisition; the system's legitimacy depends on demonstrating successful vernacularization across generations.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_state_education_system, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, israeli_state_education_system, agenda_setter).

% Maintain Hebrew through prayer, textual study, and ritual recitation across centuries without any generative daily-use function. Under the native-generative standard, their Hebrew is reclassified as 'dead,' 'liturgical only,' or merely preserved rather than alive — a judgment that devalues their centuries of unbroken transmission relative to a 70-year-old vernacular revival.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_liturgical_hebrew_communities, payer,
    moderate, generational, trapped, global).

% Their vernaculars, which had functioned as the actual daily Jewish languages of the diaspora for a thousand years, were displaced and delegitimized by the same nation-building project that elevated Hebrew. They bear the cost of the vernacular-Hebrew standard twice over: their languages are neither the sacred one nor the revived national one.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, yiddish_and_ladino_heritage_speakers, payer,
    powerless, generational, trapped, regional).

% Adult immigrants who learn Hebrew as a second language in ulpanim can never fully meet the native-generative standard due to well-documented critical-period effects on phonology and intuitive grammar. They are permanently positioned as approximating a fluency they structurally cannot attain, regardless of effort or years of daily use.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, elderly_immigrant_hebrew_learners, payer,
    powerless, biographical, trapped, national).

% Deliberately maintain a diglossic separation between sacred Hebrew (loshn koydesh, reserved for prayer and study) and Yiddish or other vernaculars for daily life, on theological grounds that profane the holy tongue by using it for mundane generative speech. The native-generative standard treats their entire linguistic ideology as a failure to revive rather than as a considered alternative kernel of continuity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, ultra_orthodox_loshn_koydesh_communities, payer,
    moderate, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, ultra_orthodox_loshn_koydesh_communities, excluded).

% Study the documented case of Hebrew's revival as evidence for or against the possibility of engineered language revitalization. They evaluate competing definitions of 'living language' and their consequences for how minority and heritage language communities are treated in policy debates worldwide.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, historical_linguists_and_hebraists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, teachable, intuitively-acquirable standard of Hebrew competence that allows a modern nation-state to conduct government, education, commerce, and culture in one shared vernacular rather than fragmenting across liturgical registers, immigrant home languages, and regional dialects.
% TRANSFER_FUNCTION: Moves linguistic legitimacy, institutional funding, and cultural prestige toward those whose Hebrew was (or can approximate) native-child acquisition, and away from communities whose Hebrew survives through liturgical, textual, or diglossic transmission — reclassifying the latter's living practice as archaic or dead.
% ABSENT_VOICES: Ultra-Orthodox loshn koydesh communities and elderly first-generation immigrants are rarely consulted in Academy standard-setting; their objection — that intuitive native acquisition is not the only, or even the most historically durable, form of linguistic continuity — is documented in religious and sociolinguistic literature but structurally outside the Academy's decision process.
% DISAPPEARANCE_RATIONALE: If the native-generative standard were abandoned, the Israeli state education and cultural apparatus that certifies 'authentic' Hebrew would lose its legitimating criterion, native speakers would lose their automatic status as linguistic authorities, and multiple co-existing forms of Hebrew continuity (liturgical, diglossic, heritage) would gain equal standing — the sabra generation and the Academy dispute whether this would be a loss of a real achievement or merely the removal of an artificial hierarchy imposed on equally valid practices.
% FOUNDING_PROBLEM: In the late 19th and early 20th centuries, Hebrew had no native speakers and no vocabulary for modern secular, technical, or everyday life; the Zionist project needed a shared vernacular that immigrant Jews from mutually unintelligible language backgrounds could use to build a functioning society, rather than relying on Yiddish, Arabic, Russian, or liturgical Hebrew fragments.
% FOUNDING_PROBLEM_CORROBORATION: Independent historical linguists (e.g., scholarship on the Ben-Yehuda revival and subsequent critique) corroborate that the founding problem — absence of a shared modern vernacular — was real and substantially solved by the mid-20th century; the same scholarship, corroborated by advocates for Yiddish and Ladino heritage preservation and by Haredi religious authorities, argues the standard now persists less to solve a live coordination problem and more to police linguistic legitimacy and cultural hierarchy among Jewish communities.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, contested).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the native-generative standard does more than solve a coordination problem: it actively reallocates cultural authority and resources away from communities with equally continuous but structurally different relationships to Hebrew. Suppression starts high (0.70) during the founding decades, when explicit policy discouraged Yiddish and diaspora Hebrew registers in favor of enforced vernacular immersion, and declines modestly (to 0.62) as the standard becomes self-sustaining through institutional inertia rather than active campaign. Theater ratio rises gradually (0.10 to 0.28) as Academy activity increasingly performs linguistic purism (neologism campaigns, phonological standardization drives) beyond what functional communication requires. Accessibility collapse (0.6) is substantial but not total: liturgical and heritage Hebrew practices persist in parallel, just devalued, not eliminated.
 *
 * PERSPECTIVAL GAP:
 *   From the Academy and sabra-generation seat, the native-generative standard looks like rope: a hard-won, genuinely coordinating achievement that solved a real national vernacular problem. From the seat of diaspora liturgical communities, Yiddish/Ladino heritage speakers, and Haredi loshn koydesh communities, the same standard operates as tangled rope shading toward snare: their long-standing, functioning relationship to Hebrew is reclassified as deficient by a criterion they had no part in setting, with real institutional and cultural costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The Academy, the sabra generation, the Zionist revival project, and the state education system are declared beneficiaries: they collect legitimacy, institutional funding, and cultural authority from the standard's operation, giving them low directionality (d near beneficiary end). Diaspora liturgical communities, Yiddish/Ladino heritage speakers, elderly immigrant learners, and Haredi loshn koydesh communities are declared victims: the standard extracts recognition and resources from their practice by definitional fiat, giving them high directionality (d near target end). Elderly immigrant learners are a distinctive victim class — trapped not by policy but by critical-period acquisition biology, unable to ever fully satisfy the standard regardless of effort, which the story treats as a structural rather than a willful exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no shared modern vernacular for a diverse immigrant population) was real and is now substantially solved — Hebrew functions as a fully generative daily vernacular for millions. Whether the native-generative standard's continued role as the SOLE criterion of linguistic legitimacy still serves that founding function, or has drifted into gatekeeping unrelated communities' equally valid continuity practices, is exactly the contested founding_problem_status this story declares. The mismatch (founding problem largely solved, yet the standard is still deployed to delegitimize liturgical and heritage Hebrew) is the signal the classification is built to surface, not a claim this story resolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_generative_kernel_reading_ambiguity,
    'Is the native-generative criterion the uniquely correct account of what makes Hebrew a ''living'' language, or is it one contestable reading among several (liturgical_preservation, bridge_pidginized) that happens to have captured state institutional power?',
    'Comparative sociolinguistic analysis of language vitality frameworks (UNESCO''s language vitality factors, diglossia theory) applied neutrally across all three readings, plus historical analysis of whether the native-generative standard was adopted for descriptive accuracy or nation-building expediency.',
    'If native-generative is one reading among structurally equal alternatives, the Academy''s authority to declare other Hebrew practices ''dead'' is a contestable political act rather than a linguistic fact, strengthening the tangled_rope classification. If it is genuinely the only sense in which a language can be ''alive'' in the relevant technical sense, the classification shifts toward a more legitimate coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_generative_kernel_reading_ambiguity, conceptual, 'Whether the native-generative kernel reading is privileged by linguistic fact or by institutional power.').

omega_variable(
    critical_period_biological_constraint,
    'Is the exclusion of elderly immigrant learners from ever meeting the native-generative standard a genuine biological mountain (critical period effects on language acquisition are well-documented) layered onto a constructed social standard, or is the standard itself constructed such that it specifically weaponizes a real biological fact against a specific population?',
    'Distinguish the biological finding (critical period effects on native-like phonological/grammatical acquisition are real and well-replicated) from the policy choice (making native-like acquisition the sole criterion of linguistic legitimacy, rather than one criterion among several, such as functional daily use).',
    'If resolved toward ''constructed weaponization,'' elderly immigrant learners'' victim status is strengthened and the standard''s extractiveness score should rise; if resolved toward ''unavoidable byproduct of any generative standard,'' their victimhood is better modeled as an unfortunate externality of a genuinely necessary coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_period_biological_constraint, empirical, 'Whether critical-period exclusion of adult learners is constructed harm or unavoidable biological externality.').

omega_variable(
    cs_framing_kernel_versus_legitimacy_narrative,
    'Should the CS kernel be framed as ''Hebrew the language system'' (the object of the Academy''s standardization) or as ''the legitimacy narrative of successful revival'' (the Zionist national story that the native-generative standard exists to vindicate)? The obvious framing is the former; the less obvious framing treats the revival-success narrative itself as the operative kernel, with the Academy''s linguistic rulings as downstream interpretive acts serving that narrative''s legitimacy.',
    'Examine whether Academy rulings that conflict with the ''successful revival'' narrative (e.g., acknowledging substantial residual Yiddish/Arabic substrate influence, or acknowledging incomplete native acquisition among some populations) are treated identically to rulings that confirm it, or whether the narrative exerts asymmetric pressure on which linguistic facts get emphasized.',
    'Under the language-system framing, this constraint is CS with authority_grounding=expertise (linguists standardizing a real language). Under the legitimacy-narrative framing, authority_grounding shifts toward extraction (the Academy''s rulings serve the national-revival story''s need for confirmation), which would change the CS classification and strengthen the tangled_rope/extractive reading. This story adopts the language-system framing as primary, guided by the Academy''s formal linguistic mandate and expert composition, but flags the alternative framing as live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_versus_legitimacy_narrative, conceptual, 'Whether the CS kernel is the language standard itself or the national revival legitimacy narrative it serves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__native_generative, theater_ratio, 20, 0.13).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__native_generative, theater_ratio, 40, 0.17).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.21).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__native_generative, theater_ratio, 80, 0.25).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__native_generative, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__native_generative, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__native_generative, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__native_generative, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__native_generative, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__native_generative, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'Hebrew continuity' per the ε-invariance principle. native_generative (this story) claims tangled_rope with ε=0.58, treating native-child acquisition as the sole criterion of vitality and reclassifying liturgical/heritage Hebrew as dead. liturgical_preservation and bridge_pidginized are separate constraints with their own ε values, beneficiaries, and victim sets — they are not alternative measurements of this same constraint but structurally distinct claims about what makes Hebrew 'alive,' each instantiating a different kernel reading with different institutional authorities and different victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
