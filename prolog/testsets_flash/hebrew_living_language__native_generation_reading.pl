% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__native_generation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Hebrew as a Living Language: Native Generative Speech Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story analyzes the 'native generation' reading of Hebrew
 *   as a living language, which posits that Hebrew's vitality depends on its
 *   generative use in daily spoken communication by native speakers, rather
 *   than solely through liturgical or literary means. This reading drove the
 *   active revitalization efforts in the late 19th and 20th centuries,
 *   leading to the suppression of other Jewish vernaculars. The constraint is
 *   classified as a Tangled Rope due to its genuine coordination function
 *   (creating a common language) coupled with significant asymmetric
 *   extraction from speakers of other languages and active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.65).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.7).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew as a Living Language: Native Generative Speech Reading").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '772d0346-6e24-492e-aa6d-b2a1fd3f83a6').
narrative_ontology:cs_kernel_codification('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', formalized).
narrative_ontology:cs_authority_grounding('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', lineage).
narrative_ontology:cs_interpretation_layer_present('772d0346-6e24-492e-aa6d-b2a1fd3f83a6').
narrative_ontology:cs_reading_relation('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', hebrew_living_language__liturgical_continuity_reading, influences).
narrative_ontology:cs_reading_relation('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', foundational, generative_speech_is_life).
narrative_ontology:cs_axiom_status(generative_speech_is_life, holdable).
narrative_ontology:cs_axiom_grounding('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', generative_speech_is_life, conventional).
narrative_ontology:cs_axiom('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', secondary, national_unity_requires_common_vernacular).
narrative_ontology:cs_axiom_status(national_unity_requires_common_vernacular, holdable).
narrative_ontology:cs_axiom_grounding('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', national_unity_requires_common_vernacular, instrumental).
narrative_ontology:cs_reference_frame('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', hebrew_as_daily_vernacular).
narrative_ontology:cs_drift_state('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('772d0346-6e24-492e-aa6d-b2a1fd3f83a6', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_revivalists).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, non_hebrew_vernacular_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocated for and actively enforced the shift to Hebrew as a spoken vernacular, believing it essential for national identity and cultural renewal. They established schools, published dictionaries, and promoted Hebrew in all spheres of daily life, often discouraging other Jewish languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_revivalists, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from a unified national language, which facilitates administration, education, and military communication. The state actively supports Hebrew language programs and policies that prioritize Hebrew over other languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, israeli_state_institutions, beneficiary,
    institutional, generational, constrained, national).

% Experienced social pressure, cultural marginalization, and sometimes outright suppression of their native language in favor of Hebrew. Many were forced to adopt Hebrew for social and economic integration, leading to a decline in Yiddish fluency among younger generations.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    powerless, biographical, identity_locked, regional).

% Similar to Yiddish speakers, they faced pressure to abandon their traditional language, Ladino, in favor of modern Hebrew. This led to a significant loss of Ladino speakers and cultural heritage.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    powerless, biographical, identity_locked, regional).

% Includes speakers of various other languages (e.g., Arabic, Russian, Amharic) who, upon immigrating to Israel, are expected to adopt Hebrew as their primary language for full participation in society. They bear the cost of language acquisition and potential loss of their native tongue's prominence.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, non_hebrew_vernacular_speakers, payer,
    moderate, biographical, constrained, national).

% Analyze the sociolinguistic processes of Hebrew revitalization, documenting the successes and the costs, including the suppression of other Jewish languages. They provide an external, analytical perspective on the constraint's operation and impact.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, linguistic_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a common, generative, spoken language for a diverse population, enabling daily communication, cultural expression, and national cohesion, replacing a multitude of liturgical and diaspora vernaculars.
% TRANSFER_FUNCTION: Transfers linguistic dominance and cultural capital from existing Jewish vernaculars (like Yiddish and Ladino) to modern Hebrew, requiring speakers of other languages to adopt Hebrew for full social participation.
% ABSENT_VOICES: The voices of those who wished to maintain and develop their non-Hebrew Jewish vernaculars (e.g., Yiddishists, Ladino preservationists) were often marginalized or actively suppressed in the early stages of the revival. They would argue for linguistic pluralism and against the forced assimilation into Hebrew.
% DISAPPEARANCE_RATIONALE: If the constraint that Hebrew must be a natively generated, daily spoken language vanished, the linguistic landscape of Israel would fundamentally change. Other languages might regain prominence, the national identity tied to Hebrew would weaken, and the social fabric built around a common vernacular would need to re-form.
% FOUNDING_PROBLEM: The Jewish people lacked a common, generative spoken language for daily life, relying on diverse diaspora vernaculars and a liturgical/literary Hebrew that was not used for everyday communication, hindering national unity and modern cultural development.
% FOUNDING_PROBLEM_CORROBORATION: Hebrew revivalists and Israeli state institutions attest that the problem of national cohesion and modern cultural expression through a common language remains live. Linguistic scholars corroborate the historical problem of linguistic fragmentation and the success of Hebrew in addressing it, while also documenting the costs to other languages.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_living_language__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high because the policy of prioritizing Hebrew for daily speech imposed significant costs on speakers of other Jewish languages, forcing them to abandon or marginalize their native tongues for social and economic integration. Suppression (0.70) is high, reflecting the active institutional efforts to promote Hebrew and discourage other languages, including educational policies and social pressure. Theater ratio (0.10) is low, as the efforts were genuinely aimed at creating a spoken language, not merely symbolic performance. The metrics show an increase in extractiveness and suppression during the peak of the revival, then a slight stabilization as Hebrew became firmly established.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hebrew revivalists and Israeli state institutions, this constraint was a necessary Rope, solving a critical coordination problem for national identity and communication. From the perspective of Yiddish and Ladino speakers, it operated as a Snare, extracting their linguistic heritage and forcing assimilation. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revivalists and Israeli state institutions are clear beneficiaries, as they achieved their goal of a unified national language. Speakers of Yiddish, Ladino, and other non-Hebrew vernaculars are victims, bearing the costs of linguistic shift and cultural loss. Their 'identity_locked' exit options reflect the deep cultural and social ties to their languages, making exit (abandoning their language) a profound personal and communal cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (creating a living, spoken Hebrew) is largely resolved, as Hebrew is now a vibrant, natively spoken language. However, the extractive and suppressive mechanisms persist, albeit with less intensity than during the peak revival period. This suggests a drift towards a Piton or a more entrenched Snare, where the initial coordination function has been achieved, but the structure continues to extract from those who bear its costs, sustained by institutional inertia and the beneficiaries' continued advantage. The 'contested' status of the founding problem reflects this ongoing debate about whether the costs are still justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of other Jewish languages structural (institutional policies, lack of resources) or internalized (social pressure, perceived necessity for integration)?',
    'Post-revival linguistic surveys and qualitative studies examining the persistence of language shift attitudes after formal suppressive policies have relaxed. If internalized suppression persists, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would amplify the Snare-like qualities for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-Hebrew vernaculars.').

omega_variable(
    kernel_reading_legitimacy,
    'Is this ''native generation'' reading of Hebrew''s vitality the most legitimate interpretation of what constitutes a ''living language'' for the Jewish people, or do other readings (liturgical, literary) hold equal or greater validity?',
    'A shift in collective consensus among Jewish communities globally, or a re-evaluation by linguistic and cultural authorities that grants equal status to other forms of Hebrew''s ''liveness''.',
    'If other readings gain legitimacy, the extractiveness and suppression associated with this reading would be re-evaluated as unnecessary, potentially reclassifying the constraint as a Snare rather than a Tangled Rope, as its coordination function would be seen as less essential.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Contestation over the definition of a ''living language'' for Hebrew.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1880, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__native_generation_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1910, hebrew_living_language__native_generation_reading, theater_ratio, 1910, 0.08).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_living_language__native_generation_reading, theater_ratio, 1940, 0.12).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_living_language__native_generation_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__native_generation_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_living_language__native_generation_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__native_generation_reading, base_extractiveness, 1880, 0.3).
narrative_ontology:measurement(hebr_be_t1910, hebrew_living_language__native_generation_reading, base_extractiveness, 1910, 0.45).
narrative_ontology:measurement(hebr_be_t1940, hebrew_living_language__native_generation_reading, base_extractiveness, 1940, 0.6).
narrative_ontology:measurement(hebr_be_t1970, hebrew_living_language__native_generation_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__native_generation_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(hebr_be_t2020, hebrew_living_language__native_generation_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__native_generation_reading, suppression_requirement, 1880, 0.2).
narrative_ontology:measurement(hebr_su_t1910, hebrew_living_language__native_generation_reading, suppression_requirement, 1910, 0.4).
narrative_ontology:measurement(hebr_su_t1940, hebrew_living_language__native_generation_reading, suppression_requirement, 1940, 0.65).
narrative_ontology:measurement(hebr_su_t1970, hebrew_living_language__native_generation_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__native_generation_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(hebr_su_t2020, hebrew_living_language__native_generation_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Hebrew as a living language' kernel. This 'native generation' reading emphasizes daily spoken use, while the 'liturgical continuity' reading focuses on religious practice and the 'literary revival' reading on written production. Each reading has distinct beneficiaries, victims, and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
