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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Hebrew as Generative Native Speech
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint defines Hebrew as a 'living language' exclusively through
 *   the lens of generative daily speech by native speakers, distinguishing it
 *   from memorized recitation or literary production. It is one reading of
 *   the broader 'hebrew_living_language' kernel. The constraint operates as a
 *   tangled rope, coordinating the creation of a new linguistic community
 *   while simultaneously extracting from and suppressing other existing
 *   Jewish linguistic practices. The metrics reflect the active enforcement
 *   and social pressure required to establish and maintain this specific
 *   definition of linguistic vitality.
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
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew as Generative Native Speech").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, 'd9493d7b-99f8-4fe3-89d7-92b419e638bf').
narrative_ontology:cs_kernel_codification('d9493d7b-99f8-4fe3-89d7-92b419e638bf', formalized).
narrative_ontology:cs_authority_grounding('d9493d7b-99f8-4fe3-89d7-92b419e638bf', practice).
narrative_ontology:cs_interpretation_layer_present('d9493d7b-99f8-4fe3-89d7-92b419e638bf').
narrative_ontology:cs_reading_relation('d9493d7b-99f8-4fe3-89d7-92b419e638bf', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('d9493d7b-99f8-4fe3-89d7-92b419e638bf', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('d9493d7b-99f8-4fe3-89d7-92b419e638bf', foundational, generative_daily_speech_is_life).
narrative_ontology:cs_axiom_status(generative_daily_speech_is_life, holdable).
narrative_ontology:cs_axiom_grounding('d9493d7b-99f8-4fe3-89d7-92b419e638bf', generative_daily_speech_is_life, empirically_contingent).
narrative_ontology:cs_axiom('d9493d7b-99f8-4fe3-89d7-92b419e638bf', secondary, linguistic_autonomy_from_diaspora).
narrative_ontology:cs_axiom_status(linguistic_autonomy_from_diaspora, holdable).
narrative_ontology:cs_axiom_grounding('d9493d7b-99f8-4fe3-89d7-92b419e638bf', linguistic_autonomy_from_diaspora, conventional).
narrative_ontology:cs_reference_frame('d9493d7b-99f8-4fe3-89d7-92b419e638bf', modern_national_vernacular_ideal).
narrative_ontology:cs_drift_state('d9493d7b-99f8-4fe3-89d7-92b419e638bf', contemporary_israeli_society, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d9493d7b-99f8-4fe3-89d7-92b419e638bf', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_revivalists).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, native_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, liturgical_hebrew_scholars).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, haskalah_literary_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The intellectual and political leaders who championed the idea of Hebrew as a modern, spoken vernacular. They actively promoted its use in daily life, education, and public discourse, often discouraging the use of other Jewish languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_revivalists, agenda_setter,
    institutional, generational, mobile, national).

% Individuals born into or raised within the modern Hebrew-speaking environment, for whom Hebrew is their primary, generative language. They benefit from a shared national language and cultural identity, but are identity-locked into this linguistic framework.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, native_hebrew_speakers, beneficiary,
    moderate, biographical, identity_locked, national).

% Speakers of Yiddish, a prominent Jewish vernacular, who faced significant social and institutional pressure to abandon their language in favor of modern Hebrew during the revival period. Their linguistic heritage was often devalued or suppressed.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    powerless, biographical, trapped, regional).

% Speakers of Ladino (Judeo-Spanish), another significant Jewish vernacular, who experienced similar pressures to shift to modern Hebrew. Their language, too, was marginalized in the drive for linguistic unity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    powerless, biographical, trapped, regional).

% Scholars and religious leaders who maintained Hebrew primarily as a language of prayer, study, and sacred texts. While they valued Hebrew, their understanding of its 'living' status was rooted in continuity of tradition, not daily generative speech, and their perspective was often sidelined by the revivalists.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, liturgical_hebrew_scholars, payer,
    organized, generational, constrained, global).

% The community of writers and intellectuals from the Jewish Enlightenment (Haskalah) who had already revived Hebrew for modern literary purposes. Their focus on written, rather than spoken, generative competence meant their contribution was re-framed or superseded by the native-generation ideal.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, haskalah_literary_community, payer,
    organized, biographical, constrained, global).

% Academics and activists who analyze language policy and revitalization efforts, often critiquing the suppression of linguistic diversity that can accompany national language projects. They observe the historical and ongoing impacts of this constraint.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, linguistic_diversity_advocates, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a common, generative, daily spoken language for a national identity, enabling spontaneous communication and cultural expression among a diverse population.
% TRANSFER_FUNCTION: Transfers linguistic legitimacy, cultural capital, and social resources from existing forms of Hebrew (liturgical, literary) and other Jewish vernaculars (Yiddish, Ladino) to the newly defined generative native speech. It also transfers social pressure towards the adoption of modern Hebrew.
% ABSENT_VOICES: Speakers of other Jewish vernaculars (e.g., Judeo-Arabic, Bukhori, Juhuri) whose languages were also suppressed or marginalized in favor of modern Hebrew, and who were not part of the primary revivalist discourse. Their perspectives on linguistic vitality and cultural continuity were largely unheard in the dominant narrative.
% DISAPPEARANCE_RATIONALE: If this definition of 'living' vanished, the cultural and linguistic landscape of modern Israel would be fundamentally different. The legitimacy of other Jewish languages would rise, the unique status of modern Hebrew would diminish, and the very foundation of Israeli national identity, as tied to a revived vernacular, would be challenged.
% FOUNDING_PROBLEM: The perceived lack of a common, daily spoken language for the Jewish people, particularly in the context of Zionist nation-building, and the desire to reclaim Hebrew from its status as primarily a liturgical or literary language, seen as insufficient for modern national life.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and its educational and cultural institutions actively promote modern Hebrew as a living language, and many native speakers attest to its vitality and centrality to their identity. However, scholars of other Jewish languages and some diaspora communities contest the historical necessity or justice of the suppression of other vernaculars, arguing the founding problem was solved at too high a cost to linguistic diversity. Legislative hearings and academic studies from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.65) because the promotion of modern Hebrew involved the active marginalization and suppression of other Jewish vernaculars and alternative definitions of Hebrew's vitality. Suppression is high (0.7) due to institutional policies and social pressures that discouraged Yiddish, Ladino, and other languages in favor of modern Hebrew. Theater ratio is low (0.1) because the goal was genuine, functional, generative speech, not merely performative use. The measurement series show a rise in extractiveness and suppression as the revival gained momentum and institutional backing, then stabilized as modern Hebrew became firmly established.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hebrew revivalists and native speakers, this constraint is a successful coordination effort that created a vibrant national language. From the perspective of Yiddish or Ladino speakers, it was a highly extractive and suppressive force that led to the decline of their mother tongues. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revivalists and native Hebrew speakers are beneficiaries, as they gain a shared, living national language and cultural identity (low directionality). Speakers of Yiddish, Ladino, and scholars of liturgical/literary Hebrew are payers/victims, as their linguistic practices were devalued or suppressed (high directionality). Linguistic diversity advocates act as observers, analyzing the broader societal impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_of_vernaculars_justified,
    'Was the active suppression of other Jewish vernaculars (Yiddish, Ladino) a necessary cost for the successful revival of modern Hebrew as a generative native language, or an avoidable act of linguistic extraction?',
    'Comparative historical analysis of other language revitalization movements that did or did not involve suppression of co-existing vernaculars, assessing their success and societal costs.',
    'If deemed necessary, the extractiveness is partially re-framed as a coordination cost. If avoidable, it reinforces the classification of the constraint as highly extractive and highlights a moral cost of the revival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_vernaculars_justified, conceptual, 'Assesses the necessity and justification of linguistic suppression in the Hebrew revival.').

omega_variable(
    generative_vs_recitative_boundary,
    'How strictly can ''generative'' daily speech be empirically distinguished from highly fluent, extensive memorized recitation in a language with a long textual tradition?',
    'Linguistic studies employing corpus analysis and psycholinguistic experiments to identify markers of true generative competence versus extensive rote learning and formulaic speech.',
    'If the distinction is empirically fuzzy, the ''native_generation_reading'' loses some of its definitional clarity, potentially blurring the lines with the ''liturgical_continuity_reading'' and reducing the perceived ''naturalness'' of its claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generative_vs_recitative_boundary, empirical, 'Examines the empirical boundary between generative and recitative language use.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is one reading of the ''hebrew_living_language'' kernel, specifically the ''native_generation_reading''. Sibling readings (''liturgical_continuity_reading'', ''literary_revival_reading'') would define ''living'' differently. What are the precise structural implications of adopting a sibling reading?',
    'Detailed structural analysis of each sibling reading, mapping their definitions of ''living'' to distinct beneficiary/victim sets, extractiveness profiles, and claimed types.',
    'Adopting the ''liturgical_continuity_reading'' would shift beneficiaries to religious scholars and victims to those who prioritize secular use, likely lowering extractiveness. Adopting the ''literary_revival_reading'' would shift beneficiaries to writers and victims to those who prioritize spoken use, with moderate extractiveness. The disagreement is located in the fundamental definition of linguistic vitality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Documents the structural differences between this reading and its siblings within the ''hebrew_living_language'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1880, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__native_generation_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__native_generation_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__native_generation_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_living_language__native_generation_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_living_language__native_generation_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_living_language__native_generation_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__native_generation_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_living_language__native_generation_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__native_generation_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__native_generation_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__native_generation_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(hebr_be_t1940, hebrew_living_language__native_generation_reading, base_extractiveness, 1940, 0.63).
narrative_ontology:measurement(hebr_be_t1960, hebrew_living_language__native_generation_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement(hebr_be_t1980, hebrew_living_language__native_generation_reading, base_extractiveness, 1980, 0.64).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__native_generation_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(hebr_be_t2020, hebrew_living_language__native_generation_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__native_generation_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement(hebr_su_t1900, hebrew_living_language__native_generation_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__native_generation_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(hebr_su_t1940, hebrew_living_language__native_generation_reading, suppression_requirement, 1940, 0.68).
narrative_ontology:measurement(hebr_su_t1960, hebrew_living_language__native_generation_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(hebr_su_t1980, hebrew_living_language__native_generation_reading, suppression_requirement, 1980, 0.69).
narrative_ontology:measurement(hebr_su_t2000, hebrew_living_language__native_generation_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(hebr_su_t2020, hebrew_living_language__native_generation_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
