% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew Native Generational Linguistic Restoration
 *   domain: sociolinguistics/nationalism/religious_identity
 *
 * SUMMARY:
 *   This constraint instantiates the native-generational reading of the
 *   Hebrew linguistic-life kernel. The claim (native-speaker childhood
 *   acquisition as the sole criterion for linguistic aliveness) functioned as
 *   an explicit ideological principle driving the Hebrew revival movement
 *   (1880–1948) and state policy thereafter. The kernel contest involves
 *   three structurally distinct definitions of what makes a language alive:
 *   this reading privileges generational native-speaker continuity and
 *   excludes liturgical preservation and marketplace lingua-franca functions
 *   as insufficient for aliveness. The consequence is the systematic
 *   displacement of Yiddish, Ladino, and Judeo-Arabic from mother-tongue
 *   status through institutional coercion, educational policy, and cultural
 *   stigmatization. The constraint is CLAIMED as tangled_rope (coordination
 *   of a unified national linguistic identity) while the authored metrics
 *   capture substantially extractive, actively enforced coercion of competing
 *   language communities.
 *
 * KEY AGENTS:
 *   - hebrew_revival_movement_leadership: agenda_setter, institutional power — sets the definition of linguistic life and enforces it through educational policy
 *   - ashkenazi_hebrew_speakers: beneficiary, organized power — gain institutional legitimacy and national identity privilege through native-speaker status
 *   - yiddish_speakers: victim/payer, organized power (declining) — bear coercive abandonment of mother tongue through stigmatization and educational exclusion
 *   - ladino_speakers: victim/payer, moderate power — experience similar displacement with less organizational capacity for resistance
 *   - arabic_jewish_communities: victim/payer, powerless — bear the highest linguistic and cultural extraction cost through geographic displacement and institutional devaluation
 *   - second_generation_native_hebrew_learners: dual-beneficiary/payer, identity-locked — gain full inclusion in the national project while experiencing severed heritage-language continuity
 *   - linguistic_scholars: observer, institutional power — increasingly document that Yiddish, Ladino, and Judeo-Arabic remain alive by empirical standards despite institutional pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.81).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.78).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Native Generational Linguistic Restoration").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/nationalism/religious_identity").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, 'a21884da-f9f8-4d95-b6b0-0bc687552d1d').
narrative_ontology:cs_kernel_codification('a21884da-f9f8-4d95-b6b0-0bc687552d1d', formalized).
narrative_ontology:cs_authority_grounding('a21884da-f9f8-4d95-b6b0-0bc687552d1d', extraction).
narrative_ontology:cs_interpretation_layer_present('a21884da-f9f8-4d95-b6b0-0bc687552d1d').
narrative_ontology:cs_reading_relation('a21884da-f9f8-4d95-b6b0-0bc687552d1d', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('a21884da-f9f8-4d95-b6b0-0bc687552d1d', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('a21884da-f9f8-4d95-b6b0-0bc687552d1d', foundational, native_speaker_childhood_acquisition_necessary).
narrative_ontology:cs_axiom_status(native_speaker_childhood_acquisition_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a21884da-f9f8-4d95-b6b0-0bc687552d1d', native_speaker_childhood_acquisition_necessary, conventional).
narrative_ontology:cs_axiom('a21884da-f9f8-4d95-b6b0-0bc687552d1d', foundational, secular_daily_speech_required_for_aliveness).
narrative_ontology:cs_axiom_status(secular_daily_speech_required_for_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('a21884da-f9f8-4d95-b6b0-0bc687552d1d', secular_daily_speech_required_for_aliveness, conventional).
narrative_ontology:cs_reference_frame('a21884da-f9f8-4d95-b6b0-0bc687552d1d', hebrew_national_vernacular_standard).
narrative_ontology:cs_drift_state('a21884da-f9f8-4d95-b6b0-0bc687552d1d', contemporary_diaspora_heritage_language_revival, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a21884da-f9f8-4d95-b6b0-0bc687552d1d', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_national_identity_project).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, ashkenazi_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_jewish_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, linguistic_pluralism).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).

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
 *   Extractiveness reaches 0.81 by 2026 because the constraint enforces a narrow definition of aliveness that delegitimizes and displaces competing Jewish linguistic traditions; it is not merely descriptive but prescriptive and coercive. Suppression is high (0.78) and RISING from 1880–1948 (state establishment phase) then slightly DECLINING post-1980 as diaspora heritage-language communities gain some international institutional recognition, but the decline is modest because institutional enforcement machinery remains embedded in education and culture policy. Theater is moderate (0.42): the coordination function (national linguistic unification) is real, but an increasing share of measured extraction supports linguistic enforcement rather than functional coordination — the constraint operates theatrically to maintain the binary (alive Hebrew vs. dead diaspora languages) rather than to solve an active coordination problem. Accessibility collapse is substantial (0.72) by 2026: once the native-generational standard is institutionalized and internalized, alternatives (multilingualism, diglossia, heritage-language transmission) become structurally inaccessible to individuals born into the constraint; the cost of exit from 'being Hebrew' has become identity-fusion. Resistance has DECLINED (from 0.70 in 1880 to 0.58 in 2026) as generational turnover and institutional normalization have made the standard feel natural rather than imposed. The coercion grid shows the constraint pressing hardest at the individual level (accessibility 0.80, stakes 0.83, suppression 0.81 by 2026) where childhood acquisition is decided, while organizational resistance persists among heritage-language communities but at declining intensity.
 *
 * PERSPECTIVAL GAP:
 *   The Hebrew-revival leadership and current Ashkenazi institutional beneficiaries perceive this constraint as coordination (unifying diaspora, enabling return, establishing national identity) with minimal coercive content — the linguistic standard feels to them like discovered necessity, not imposed choice. Yiddish and Ladino speakers experience it as pure extraction: coercive language shift, loss of cultural heritage, institutional erasure of their belonging within Jewish identity. Arabic-speaking communities experience the highest extractiveness: geographic displacement compounded by linguistic de-legitimation. Second-generation learners occupy a fraught middle: they gain full institutional belonging (beneficiary) but experience identity fusion such that exit from Hebrew means exit from Jewishness-as-defined-by-the-state (identity_locked). Linguistic scholars observe from outside that the constraint's persistence is not functionally necessary (multilingual Jewish communities maintained coherence before and alongside the native-generational standard) and increasingly classify the extraction as political rather than coordinative.
 *
 * DIRECTIONALITY LOGIC:
 *   The agenda-setter (hebrew_revival_movement_leadership) has d near 0.0 (full beneficiary): they set the rules, control the enforcement machinery, and gain ideological authority from the constraint. Ashkenazi Hebrew speakers have d around 0.15–0.25 (moderate beneficiary): they benefit from institutional privilege and identity validation without bearing direct costs. Yiddish and Ladino speakers have d around 0.80–0.88 (near-target): they bear coercive language shift, educational exclusion of heritage languages, and cultural stigmatization. Arabic-speaking communities have d near 0.92 (full target): they combine linguistic victimization with geographic displacement and institutional marginalization. The second-generation native learners have d around 0.40–0.55 (mixed): they benefit institutionally but at the cost of identity-locked fusion, severed heritage transmission, and internalized suppression (they do not experience external barriers to Yiddish/Ladino in adulthood, but the constraint has made them unable to imagine acquiring their parents' languages without losing their sense of Jewish belonging). Linguistic pluralism is excluded (d not computed) because it has no seat in the institutional conversation. These directionality values derive from the beneficiary/victim declarations plus exit options: trapped/identity-locked agents sit nearer the target end; agents with institutional power and low exit cost sit near the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy symptoms in the post-1980 period. The founding problem (Hebrew as liturgical corpus becoming a national vernacular lingua franca to unify diaspora) was substantively solved by 1948 (Hebrew became the state language, native acquisition became normative in Israel proper). The contemporary extraction (enforcing native-generational-only aliveness, displacing competing linguistic identities) persists not because the founding coordination problem remains live, but because the constraint has become institutionalized as identity machinery. Yiddish, Ladino, and Judeo-Arabic continue as living languages in diaspora communities and heritage contexts; linguistic scholarship documents their vitality; the constraint persists as a prescriptive framework that declares them dead regardless of empirical evidence. The theater ratio (0.42, rising from 0.18 in 1880) captures this drift: increasing share of the suppression and enforcement activity serves identity standardization rather than functional coordination. The mismatch between founding_problem_status=live (the revival movement and current authorities claim unification remains necessary) and empirical contestation (scholars document that Jewish identity and Jewish culture sustain themselves plurilingually) signals mandatrophy: the constraint persists through institutional inertia and identity-fusion rather than because the original problem requires it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_aliveness_definition,
    'What empirical criteria should determine whether a language is alive? Is native-speaker childhood acquisition a necessary or sufficient criterion, or one among many valid measures?',
    'Linguistic vitality indices (UNESCO, Ethnologue) measure speaker populations, intergenerational transmission, domains of use, written literature, and institutional status. Yiddish, Ladino, and Judeo-Arabic remain alive by most empirical metrics despite the constraint''s prescriptive definition.',
    'If empirical aliveness is adopted instead of prescriptive nativism, the constraint''s classification shifts from tangled_rope (preserving coordination function) to snare (enforcing a linguistic standard that erases competing languages without functional necessity). The victim set is legitimized; the extraction becomes pure rather than mixed with coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_aliveness_definition, empirical, 'Whether native-speaker status is a necessary criterion for linguistic aliveness or an ideological preference.').

omega_variable(
    coordination_necessity_vs_enforced_standardization,
    'Is the native-generational standard functionally necessary for Jewish national identity and diaspora coordination, or would multilingualism achieve the same functions?',
    'Historical analysis of pre-1880 multilingual Jewish communities (Yiddish, Ladino, Judeo-Arabic in daily use; Hebrew in liturgical/scholarly spheres) and post-2000 diaspora communities that maintain Jewish identity and cultural continuity with heritage languages. Natural experiments from bilingual Jewish communities.',
    'If multilingualism is functionally sufficient for Jewish identity and coordination, the constraint is pure extraction disguised as coordination (reclassification to snare). If the native-generational standard is functionally necessary for state-level unification and international Jewish identity, the coordination justification holds (constraint remains tangled_rope with real mixed function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_enforced_standardization, empirical, 'Whether linguistic standardization solves a real coordination problem or enforces an arbitrary nationalist preference.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.78) primarily structural (institutional policy, educational exclusion, labor market discrimination) or internalized (Hebrew-speaker children have fused identity with Jewishness-as-Hebrew, making Yiddish/Ladino feel foreign or shameful)?',
    'Post-exit trajectory analysis: do Diaspora communities that reduce exposure to the constraint (heritage-language schools, multilingual family contexts, transnational communities) show persistent suppression, or does suppression decline when institutional enforcement is removed? Ethnographic study of identity reconstructions in heritage-language revival movements.',
    'If suppression is primarily internalized (second- and third-generation effects persist even absent institutional enforcement), the constraint''s effective suppression is higher than the structural measure suggests — the target carries the extraction forward even after exit. If structural, suppression weakens as enforcement capacity declines and offers tactical openings for heritage-language revitalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of competing languages is structural policy or internalized identity fusion.').

omega_variable(
    kernel_boundary_contest,
    'Does the kernel ''Hebrew linguistic life'' have a single correct reading, or are the three readings (liturgical, marketplace, native-generational) genuinely incommensurable value commitments that cannot be unified?',
    'Examination of the historical record: did the revival movement choose the native-generational reading from among alternatives (suggesting it is one reading among coherent options) or did it discover/invent this reading as the only natural standard (suggesting it is the right reading, others are confusions)? Comparative study of other language revivals (Irish, Welsh, Catalan) and how they resolved the definition question.',
    'If readings are incommensurable value commitments, the constraint''s legitimacy cannot be resolved by evidence alone — classification uncertainty enters via omega_type_class=preference (normative choice about what counts as living language). If the native-generational reading is discoverable rather than chosen, it has stronger legitimacy claims and the constraint''s extraction may be reframed as necessary cost of linguistic restoration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_boundary_contest, conceptual, 'Whether the native-generational reading is one coherent alternative among others or the natural/unique standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.18).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(hebr_tr_t2010, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(hebr_tr_t2026, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement(hebr_be_t1920, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(hebr_be_t1948, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement(hebr_be_t1980, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1980, 0.76).
narrative_ontology:measurement(hebr_be_t2010, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(hebr_be_t2026, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.45).
narrative_ontology:measurement(hebr_su_t1920, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1920, 0.62).
narrative_ontology:measurement(hebr_su_t1948, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1948, 0.74).
narrative_ontology:measurement(hebr_su_t1980, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(hebr_su_t2010, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(hebr_su_t2026, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2026, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% The Hebrew linguistic-life kernel decomposes into three structurally distinct constraints: (1) liturgical_preservation_reading — Hebrew alive through sacred-text continuity (low extraction, mountain-class); (2) marketplace_pidgin_reading — Hebrew alive through inter-communal coordination (moderate extraction, rope-class); (3) native_generational_reading (this story) — Hebrew alive through native-speaker childhood acquisition (high extraction, tangled_rope/snare-class). Each reading has its own ε, beneficiary/victim structure, and victim set. The native-generational reading forecloses the pluralist alternative: if Hebrew must be the sole native language for it to be alive, then Yiddish/Ladino/Judeo-Arabic cannot simultaneously be alive in the same framework. The three readings coexist across different institutional and ideological positions but influence each other: the native-generational reading's institutional success (state policy, educational hegemony) has suppressed the other readings' social bases, making them harder to maintain empirically even if they remain conceptually defensible. This story instantiates only the native-generational reading; the sibling readings are separate constraint files linked here for family coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__native_generational_reading, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
