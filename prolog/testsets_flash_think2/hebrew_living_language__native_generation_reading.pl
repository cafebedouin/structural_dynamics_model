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
 *   domain: historical_linguistics/language_revitalization/cultural_identity
 *
 * SUMMARY:
 *   This constraint is the 'native generation' reading of the 'Hebrew as a
 *   living language' kernel. It asserts that Hebrew is only truly living when
 *   spoken natively and generatively in daily life, explicitly excluding
 *   other forms like memorized recitation or literary production. This
 *   reading drove the language revival movement and the establishment of
 *   Hebrew as the national language of Israel, but at the cost of suppressing
 *   other Jewish vernaculars. The claimed type is 'tangled_rope' because it
 *   provided a genuine coordination function (a common national language) but
 *   involved significant extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.65).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.75).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew as Generative Native Speech").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/cultural_identity").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '0d0d1093-8809-434e-8a33-25bc7db8c144').
narrative_ontology:cs_kernel_codification('0d0d1093-8809-434e-8a33-25bc7db8c144', formalized).
narrative_ontology:cs_authority_grounding('0d0d1093-8809-434e-8a33-25bc7db8c144', extraction).
narrative_ontology:cs_interpretation_layer_present('0d0d1093-8809-434e-8a33-25bc7db8c144').
narrative_ontology:cs_reading_relation('0d0d1093-8809-434e-8a33-25bc7db8c144', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('0d0d1093-8809-434e-8a33-25bc7db8c144', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('0d0d1093-8809-434e-8a33-25bc7db8c144', foundational, daily_generative_speech_is_life).
narrative_ontology:cs_axiom_status(daily_generative_speech_is_life, holdable).
narrative_ontology:cs_axiom_grounding('0d0d1093-8809-434e-8a33-25bc7db8c144', daily_generative_speech_is_life, conventional).
narrative_ontology:cs_axiom('0d0d1093-8809-434e-8a33-25bc7db8c144', secondary, linguistic_nationalism_is_necessary).
narrative_ontology:cs_axiom_status(linguistic_nationalism_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('0d0d1093-8809-434e-8a33-25bc7db8c144', linguistic_nationalism_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('0d0d1093-8809-434e-8a33-25bc7db8c144', ancient_hebrew_vernacular).
narrative_ontology:cs_drift_state('0d0d1093-8809-434e-8a33-25bc7db8c144', post_revival_institutionalization, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('0d0d1093-8809-434e-8a33-25bc7db8c144', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_revival_movement).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, native_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, israeli_state).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, liturgical_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ideological and organizational force behind the revival, defining what constitutes a 'living' language and actively promoting its adoption as a daily vernacular. They benefit from the success of this definition.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_revival_movement, agenda_setter,
    institutional, generational, arbitrage, national).

% As the embodiment of the 'living language' ideal, their linguistic practice is validated and elevated. They benefit from the cultural and social capital associated with being native speakers of the national language.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, native_hebrew_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Speakers of Yiddish whose language was actively suppressed or devalued in favor of modern Hebrew, facing social and institutional pressure to abandon their vernacular for the 'living' language.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    powerless, biographical, trapped, regional).

% Similar to Yiddish speakers, Ladino speakers experienced marginalization and pressure to adopt Hebrew, with their traditional language deemed 'not living' in the new framework.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    powerless, biographical, trapped, regional).

% Communities outside Israel whose traditional forms of Hebrew (liturgical, scholarly) and Jewish vernaculars are implicitly or explicitly devalued by the 'native generative speech' criterion, creating a cultural cost and a sense of linguistic inferiority.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, diaspora_jewish_communities, payer,
    moderate, generational, constrained, global).

% Individuals dedicated to the study and use of Hebrew in its traditional, non-generative forms (prayer, textual exegesis). Their lifelong practice is deemed 'not living' by this constraint, challenging their professional and personal identity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, liturgical_scholars, payer,
    moderate, biographical, identity_locked, global).

% The primary institutional enforcer and beneficiary of a unified national language. It actively promotes and enforces the use of modern Hebrew through educational systems, media, and public policy, deriving national cohesion and identity from this linguistic project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Academics who analyze the historical processes, motivations, and consequences of language revitalization, including the social and linguistic costs borne by speakers of other languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a common, generative, daily spoken language for a national community, enabling modern communication, cultural cohesion, and national self-determination.
% TRANSFER_FUNCTION: Transfers linguistic legitimacy, cultural capital, and institutional resources from traditional/diasporic forms of Hebrew and other Jewish vernaculars to modern, natively spoken Hebrew. It also transfers social pressure and marginalization to non-Hebrew speakers.
% ABSENT_VOICES: Speakers of Yiddish, Ladino, and other Jewish vernaculars who were marginalized or suppressed during the revival process. They would argue for linguistic pluralism, the inherent value of their own languages, and broader definitions of 'living' Hebrew.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the linguistic landscape of Israel and Jewish identity globally would be profoundly different. Other languages might have thrived, the definition of 'living' Hebrew would be much broader, and the cultural project of the Israeli state would lack its linguistic cornerstone.
% FOUNDING_PROBLEM: The perceived lack of a modern, daily spoken language for the Jewish people, hindering national self-determination and modern cultural expression, and the fragmentation of Jewish communities by diverse vernaculars.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Zionism and language revitalization, sociolinguists studying language death and revival, and contemporary Israeli educators generally attest to the historical problem. However, the 'live' status of the problem and the necessity of the specific solution are contested by some scholars and cultural groups.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high because the demand for native, generative daily speech imposed significant costs on those who spoke other languages or used Hebrew in traditional ways. Suppression is high due to active institutional efforts to displace other languages (Yiddish, Ladino) and enforce Hebrew as the sole vernacular. Theater ratio is low as the goal was genuine functional speech, not mere performance. The metrics show a rising trend in extraction and suppression as the revival movement gained institutional power and enforced its definition of 'living' Hebrew.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Hebrew revival movement and the Israeli state, this constraint was a necessary and beneficial coordination mechanism for national identity. From the perspective of Yiddish or Ladino speakers, it was a highly extractive and suppressive force that devalued their heritage. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hebrew revival movement and the Israeli state are agenda-setters and beneficiaries, defining and enforcing the constraint while benefiting from a unified national language. Native Hebrew speakers are beneficiaries as their linguistic practice is validated. Speakers of Yiddish, Ladino, and diaspora communities are payers, bearing the cost of linguistic displacement and devaluation. Liturgical scholars are also payers, as their traditional forms of Hebrew are deemed 'not living'.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_necessity_vs_nationalism,
    'Was the exclusive focus on native, generative daily speech a genuine linguistic necessity for Hebrew''s revival, or primarily an instrumental project of cultural nationalism?',
    'Comparative studies of other language revitalization efforts that adopted more pluralistic approaches, or counterfactual historical analysis of alternative revival paths.',
    'If primarily nationalist, the measured extraction and suppression are more clearly attributable to political goals rather than linguistic function, strengthening the ''snare'' aspect of the constraint. If linguistically necessary, it supports the ''tangled_rope'' framing as a high-cost coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_necessity_vs_nationalism, conceptual, 'Ambiguity between linguistic necessity and nationalist instrumentalism.').

omega_variable(
    impact_on_diaspora_languages,
    'What was the precise causal impact of the Hebrew revival movement''s definition of ''living'' language on the vitality and transmission of Yiddish, Ladino, and other Jewish vernaculars?',
    'Sociolinguistic studies tracking language shift rates, intergenerational transmission, and institutional support for these languages before, during, and after the peak of the Hebrew revival.',
    'Strong evidence of direct causal suppression would increase the effective suppression metric and push the classification closer to ''snare'' for the affected communities. Weaker evidence would suggest more independent decline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impact_on_diaspora_languages, empirical, 'Quantifying the suppressive impact on other Jewish languages.').

omega_variable(
    definition_of_living_language,
    'Is ''native generative daily speech'' the only valid or most appropriate criterion for defining a ''living'' language, or are other forms (liturgical, literary, scholarly) equally valid expressions of linguistic vitality?',
    'Philosophical and sociolinguistic debate on language ontology and vitality, considering diverse cultural contexts and historical precedents.',
    'If other forms are deemed equally valid, the ''extraction'' from liturgical scholars and diaspora communities is recontextualized as an arbitrary imposition rather than a necessary cost of revival, shifting the classification towards ''snare'' for those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Contestation over the definition of a ''living'' language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1880, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__native_generation_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__native_generation_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__native_generation_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_living_language__native_generation_reading, theater_ratio, 1940, 0.18).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_living_language__native_generation_reading, theater_ratio, 1960, 0.19).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_living_language__native_generation_reading, theater_ratio, 1980, 0.2).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__native_generation_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__native_generation_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__native_generation_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(hebr_be_t1940, hebrew_living_language__native_generation_reading, base_extractiveness, 1940, 0.62).
narrative_ontology:measurement(hebr_be_t1960, hebrew_living_language__native_generation_reading, base_extractiveness, 1960, 0.64).
narrative_ontology:measurement(hebr_be_t1980, hebrew_living_language__native_generation_reading, base_extractiveness, 1980, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__native_generation_reading, suppression_requirement, 1880, 0.5).
narrative_ontology:measurement(hebr_su_t1900, hebrew_living_language__native_generation_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__native_generation_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(hebr_su_t1940, hebrew_living_language__native_generation_reading, suppression_requirement, 1940, 0.72).
narrative_ontology:measurement(hebr_su_t1960, hebrew_living_language__native_generation_reading, suppression_requirement, 1960, 0.74).
narrative_ontology:measurement(hebr_su_t1980, hebrew_living_language__native_generation_reading, suppression_requirement, 1980, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Hebrew as a living language' kernel. This 'native generation' reading focuses on daily spoken language, while sibling readings emphasize liturgical continuity and literary production. The readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
