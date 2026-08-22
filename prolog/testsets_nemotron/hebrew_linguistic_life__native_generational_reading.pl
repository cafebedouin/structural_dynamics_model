% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__native_generational_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew as Native Generational Mother Tongue Requirement
 *   domain: sociolinguistic/nationalist/religious
 *
 * SUMMARY:
 *   This constraint embodies the native-generational reading of Hebrew
 *   linguistic life: a language counts as 'alive' only when children acquire
 *   it as mother tongue and use it for all daily functions including secular
 *   mundane speech. Under this reading, Hebrew was genuinely dead between ~70
 *   CE and 1880 CE — a dormancy period of ~1800 years during which it
 *   functioned exclusively as a liturgical and literary language without
 *   native speakers. The revival (1880s onward) required massive
 *   institutional engineering: schools, press, lexical modernization, and
 *   active suppression of competing Jewish vernaculars (Yiddish, Ladino,
 *   Judeo-Arabic). The constraint operates as a tangled rope: it coordinates
 *   a national linguistic project (beneficiaries: Zionist institutions,
 *   Hebrew Academy, state education) while extracting from linguistic
 *   minorities who were coerced into abandonment (victims:
 *   Yiddish/Ladino/Arabic-speaking Jewish communities). Theatrical
 *   maintenance is substantial: ceremonial 'language revival' narratives mask
 *   ongoing enforcement of Hebrew monolingualism in education, media, and
 *   public life. The engine will compute per-seat classifications from the
 *   structural data; this reading claims tangled_rope while the metrics
 *   describe high extraction and active suppression.
 *
 * KEY AGENTS:
 *   - zionist_institutions: agenda_setter (institutional/biographical/arbitrage/global) — designed and enforced the revival, control education and language planning
 *   - hebrew_academy: agenda_setter (institutional/generational/analytical/global) — official lexical authority, legitimates the constraint
 *   - state_education_ministry: agenda_setter (institutional/biographical/mobile/national) — enforces Hebrew monolingual curriculum
 *   - yiddish_speaking_communities: payer (organized/biographical/trapped/national) — coerced abandonment of 1000-year vernacular
 *   - ladino_speaking_communities: payer (organized/biographical/trapped/national) — coerced abandonment of 500-year vernacular
 *   - arabic_jewish_communities: payer (organized/biographical/trapped/national) — coerced abandonment of Judeo-Arabic vernaculars
 *   - mizrahi_jewish_communities: payer (organized/biographical/constrained/national) — pressured to adopt Hebrew, lost ancestral languages
 *   - diaspora_jewish_communities: excluded (organized/biographical/constrained/global) — their linguistic vitality claims (Yiddish, etc.) are structurally excluded from 'alive' definition
 *   - linguistic_scholars: observer (analytical/civilizational/analytical/universal) — study the revival as sociolinguistic phenomenon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.78).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.82).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew as Native Generational Mother Tongue Requirement").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistic/nationalist/religious").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '3621f382-6e4b-4c67-b379-ed24e5570c2f').
narrative_ontology:cs_kernel_codification('3621f382-6e4b-4c67-b379-ed24e5570c2f', formalized).
narrative_ontology:cs_authority_grounding('3621f382-6e4b-4c67-b379-ed24e5570c2f', extraction).
narrative_ontology:cs_interpretation_layer_present('3621f382-6e4b-4c67-b379-ed24e5570c2f').
narrative_ontology:cs_reading_relation('3621f382-6e4b-4c67-b379-ed24e5570c2f', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('3621f382-6e4b-4c67-b379-ed24e5570c2f', hebrew_linguistic_life__marketplace_pidgin_reading, influences).
narrative_ontology:cs_axiom('3621f382-6e4b-4c67-b379-ed24e5570c2f', foundational, native_acquisition_necessary_for_linguistic_life).
narrative_ontology:cs_axiom_status(native_acquisition_necessary_for_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('3621f382-6e4b-4c67-b379-ed24e5570c2f', native_acquisition_necessary_for_linguistic_life, empirically_contingent).
narrative_ontology:cs_axiom('3621f382-6e4b-4c67-b379-ed24e5570c2f', foundational, secular_mundane_use_required_for_vitality).
narrative_ontology:cs_axiom_status(secular_mundane_use_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('3621f382-6e4b-4c67-b379-ed24e5570c2f', secular_mundane_use_required_for_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('3621f382-6e4b-4c67-b379-ed24e5570c2f', pre_revival_dormancy).
narrative_ontology:cs_drift_state('3621f382-6e4b-4c67-b379-ed24e5570c2f', contemporary_sovereign_hebrew, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3621f382-6e4b-4c67-b379-ed24e5570c2f', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_academy).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, state_education_ministry).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speaking_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_jewish_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, mizrahi_jewish_communities).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, national_language_unity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, generational_transmission_as_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designed and enforced the Hebrew revival as a nation-building project. Control the definition of 'linguistic life,' capture state resources for language planning, and gain legitimacy from the 'successful revival' narrative. Could redefine the criterion but choose to maintain exclusivity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Official lexical authority that legitimates the native-generational criterion. Decides which neologisms enter standard Hebrew, effectively shaping the language's secular mundane vocabulary. Their authority depends on the constraint's monopoly on 'correct' Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_academy, agenda_setter,
    institutional, generational, analytical, global).

% Enforces Hebrew monolingual curriculum in state schools. Allocates resources to Hebrew education while denying funding to Yiddish/Ladino/Judeo-Arabic heritage programs. Could permit multilingual education but maintains exclusion as policy.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, state_education_ministry, agenda_setter,
    institutional, biographical, mobile, national).

% Coerced abandonment of a 1000-year vernacular with full secular literary culture. Pre-state Yiddish schools, press, theater systematically suppressed (1930s-50s). Intergenerational transmission broken by Hebrew-only education and stigmatization of Yiddish as 'galut language.' Exit requires leaving the national project.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_communities, payer,
    organized, biographical, trapped, national).

% Coerced abandonment of a 500-year vernacular (Judeo-Spanish) with distinct literary tradition. Sephardic immigrants pressured to adopt Hebrew; Ladino press and education suppressed. Community elders who resisted were marginalized. Language death largely complete by 1970s.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speaking_communities, payer,
    organized, biographical, trapped, national).

% Coerced abandonment of Judeo-Arabic vernaculars (multiple distinct dialects). Mizrahi immigrants subjected to 'absorption' policies that treated Arabic as enemy language. Children punished for speaking Arabic in schools. Cultural heritage systematically erased; intergenerational transmission severed within one generation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, arabic_jewish_communities, payer,
    organized, biographical, trapped, national).

% Pressured to adopt Hebrew as primary identity marker; ancestral languages (Judeo-Arabic, Judeo-Persian, etc.) lost. Some retain liturgical Hebrew but not vernacular. Social mobility historically required Hebrew monolingualism. Recent heritage revival movements face institutional indifference.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, mizrahi_jewish_communities, payer,
    organized, biographical, constrained, national).

% Communities maintaining Yiddish (Hasidic), Ladino, or other Jewish languages as native vernaculars. Their linguistic vitality is structurally discounted by the 'native acquisition = life' criterion because they lack state sovereignty. Would argue for pluralist vitality definitions but are excluded from Israeli language policy discourse.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, diaspora_jewish_communities, excluded,
    organized, biographical, constrained, global).

% Study the Hebrew revival as a unique sociolinguistic phenomenon. Document language shift, revival mechanics, and minority language erosion. Their analysis reveals the constraint's structure but carries no enforcement power. Some advocate for heritage language recognition.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, zionist_institutions).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared secular vernacular for a territorially-concentrated population drawn from linguistically diverse origins, enabling mass communication, civic participation, and national cohesion in a newly formed state.
% TRANSFER_FUNCTION: Moves linguistic capital, educational resources, cultural legitimacy, and intergenerational transmission rights from minority Jewish languages (Yiddish, Ladino, Judeo-Arabic, etc.) to Hebrew and its institutional custodians. The constraint extracts the 'right to be a living language' from competing vernaculars and concentrates it in Hebrew.
% ABSENT_VOICES: Pre-state Yiddishist and Bundist activists who argued for Yiddish as national language; Sephardic and Mizrahi intellectuals who proposed multilingual models; contemporary heritage language activists in diaspora communities maintaining Jewish languages outside Hebrew. They are absent because the constraint's definition of 'alive' structurally excludes non-sovereign native acquisition.
% DISAPPEARANCE_RATIONALE: If the native-generational criterion vanished overnight, Hebrew would remain the dominant majority language but the ideological mandate for monolingual exclusivity would collapse. Yiddish, Ladino, Judeo-Arabic, and other Jewish languages could claim legitimate 'living' status within Israel, triggering resource claims for education, media, and cultural institutions. The Zionist institutional monopoly on linguistic legitimacy would fracture.
% FOUNDING_PROBLEM: Creating a unifying spoken language for a territorially-concentrated nation-building project drawing immigrants from dozens of linguistic backgrounds, where no existing vernacular was shared by a majority.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions and Hebrew Academy attest the problem remains live (ongoing immigration integration, need for shared civic language). Yiddishist historians (e.g., David Roskies), Mizrahi studies scholars (e.g., Yehouda Shenhav), and linguistic anthropologists (e.g., Bernard Spolsky) attest the founding problem was substantially solved by 1970s and the constraint persists as extraction. UNESCO's endangered languages framework corroborates that Jewish linguistic diversity was destroyed by the revival, not preserved by it.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__native_generational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__native_generational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint demands total linguistic replacement — not coexistence — and the beneficiaries (state institutions) capture the legitimacy and resources of 'revival' while the costs (language death, cultural erasure) fall on minority communities. Suppression is very high (0.82) because the constraint's persistence depends on active enforcement: Hebrew-only education laws, suppression of Yiddish theater/press (1930s-50s), stigmatization of diaspora languages as 'galut' (exile). Theater ratio is moderate (0.41): the 'miraculous revival' narrative performs coordination function while masking extraction. Accessibility collapse (0.68) reflects that once the 'native acquisition = life' criterion is accepted, alternatives (diglossia, liturgical vitality, pidgin functionality) become cognitively inaccessible as valid 'life.' Resistance (0.73) is high: Yiddishists, Bundists, Mizrahi activists, and Arab-Jewish intellectuals resisted abandonment — resistance was met with institutional marginalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda setters (Zionist institutions, Hebrew Academy, Education Ministry) are structural beneficiaries: they control the definition, capture state resources, and gain legitimacy from 'successful revival.' Their exit options are arbitrage/mobile — they could redefine 'alive' but choose not to. Payers (Yiddish/Ladino/Arabic/Mizrahi communities) are structural targets: they bore the costs of language shift, had trapped/constrained exit (social mobility required Hebrew adoption), and were stigmatized for retaining ancestral languages. Excluded voices (diaspora communities maintaining Yiddish) are structurally excluded from the 'alive' definition — their continued native acquisition is discounted because it lacks state sovereignty. Observer seat (linguistic scholars) sees full structure analytically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a unifying spoken language for a territorially-concentrated nation-building project) was live in 1880-1948 but is contested today: Hebrew is now the dominant majority language with millions of native speakers, yet the constraint persists in demanding *exclusive* Hebrew monolingualism and delegitimizing other Jewish languages. The coordination function (shared vernacular for nation-building) has been achieved; the extraction function (suppression of linguistic diversity) persists. This is a classic mandatrophy pattern: the constraint's mandate (revival) has been fulfilled but the enforcement machinery (monolingual ideology, suppression of alternatives) remains active and expands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_period_ontology,
    'Was Hebrew genuinely ''dead'' 70-1880 CE, or does the native-generational criterion impose a retrospective death sentence on a continuously transmitted liturgical language?',
    'Comparative sociolinguistic analysis of other liturgical languages (Classical Arabic, Sanskrit, Ge''ez) — do speakers of those traditions experience their language as ''dead'' during non-vernacular periods? Historical testimony from pre-Zionist Jewish communities about their relationship to Hebrew.',
    'If Hebrew was experienced as ''living'' by its liturgical users, the native-generational reading imposes an external epistemic framework that extracts legitimacy from the liturgical tradition. This would increase the constraint''s extractiveness and support tangled_rope over rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_period_ontology, conceptual, 'Whether the ''dead language'' period is an empirical fact or a reading-imposed category').

omega_variable(
    coercion_vs_voluntary_shift,
    'To what extent was the abandonment of Yiddish/Ladino/Judeo-Arabic driven by state coercion vs. voluntary pragmatic shift by speakers seeking mobility?',
    'Historical analysis of language policy (e.g., 1930s Yiddish theater bans, 1950s Mizrahi ''absorption'' policies), demographic studies of intergenerational transmission breakdown, oral histories of language shift.',
    'If primarily coercive, suppression metric is validated and tangled_rope/snare classification strengthened. If primarily voluntary, the constraint may be more rope-like (coordination with incidental language shift). The engine''s suppression metric currently assumes active enforcement — this omega tests that assumption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_vs_voluntary_shift, empirical, 'Mechanism of language shift: state suppression vs. speaker agency').

omega_variable(
    contemporary_extraction_persistence,
    'Does the constraint still actively extract in 2024, or has it transitioned to a piton (inertial maintenance) now that Hebrew is securely established?',
    'Analysis of current language policy: Are there active penalties for non-Hebrew education? Is Yiddish/Ladino/Judeo-Arabic revitalization actively discouraged or merely unsupported? Resource allocation for minority language education vs. Hebrew enforcement.',
    'If active suppression continues (e.g., restrictions on non-Hebrew schools, denial of funding for heritage languages), tangled_rope stands. If suppression has decayed to ceremonial enforcement, piton classification may apply. Theater ratio trajectory (measurements) informs this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_extraction_persistence, empirical, 'Whether current enforcement is functional extraction or theatrical inertia').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''hebrew_linguistic_life'' kernel admit only these three readings, or is there a fourth: a pluralist reading where multiple vitality criteria coexist without hierarchy?',
    'Survey contemporary Jewish linguistic thought: Do any authoritative voices (rabbinic, academic, communal) hold a pluralist position? If so, why is it structurally excluded from the kernel''s declared reading set?',
    'If a coherent pluralist reading exists but is excluded from the kernel framing, the three-reading decomposition is itself extractive — it forces a false trichotomy. This would be a meta-level extraction: the kernel definition suppresses a non-extractive alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s reading set is complete or artificially constrained').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state policy, educational exclusion) or internalized (communities adopting ''Hebrew is our only real language'' as self-concept)?',
    'Post-exit suppression trajectory: communities that maintain Yiddish/Ladino outside Israel (e.g., Hasidic enclaves, Sephardic diaspora) — do they experience suppression as external or internal? Attitude studies of Israeli Mizrahi Jews toward Judeo-Arabic heritage.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This would amplify effective extraction for payer seats and support tangled_rope over rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for linguistic minorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1948, 0.38).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.45).
narrative_ontology:measurement(hebr_be_t1900, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1900, 0.52).
narrative_ontology:measurement(hebr_be_t1920, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1920, 0.61).
narrative_ontology:measurement(hebr_be_t1948, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1948, 0.73).
narrative_ontology:measurement(hebr_be_t1970, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1970, 0.76).
narrative_ontology:measurement(hebr_be_t2000, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement(hebr_be_t2024, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.4).
narrative_ontology:measurement(hebr_su_t1900, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(hebr_su_t1920, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(hebr_su_t1948, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(hebr_su_t1970, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1970, 0.82).
narrative_ontology:measurement(hebr_su_t2000, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(hebr_su_t2024, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, zionist_national_education_system).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, israeli_language_law_2018).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_linguistic_life' kernel. The native_generational_reading declares Hebrew dead 70-1880 CE and identifies linguistic diversity (Yiddish/Ladino/Judeo-Arabic speakers) as victims of revival. The liturgical_preservation_reading declares continuous life through sacred transmission with no victim set. The marketplace_pidgin_reading identifies merchant classes as beneficiaries of trade-language functionality. The three readings have different ε values (this reading highest), different victim/beneficiary structures, and different coordination/extraction balances. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__native_generational_reading, institutional, 0.1).
constraint_indexing:directionality_override(hebrew_linguistic_life__native_generational_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
