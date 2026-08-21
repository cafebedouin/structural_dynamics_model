% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity: Native Generative Reading
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'native generative' reading of Hebrew
 *   continuity, which asserts that Hebrew is a living language only through
 *   its daily, generative use by native speakers. This reading gained
 *   prominence during the Zionist project of language revitalization and has
 *   become the dominant paradigm in modern Israel. It structurally
 *   marginalizes and delegitimizes other forms of Hebrew, such as liturgical
 *   or academic use, by deeming them 'dead' or 'inauthentic.' The high
 *   extractiveness and suppression reflect the active enforcement of this
 *   narrow definition and the costs borne by communities whose Hebrew is
 *   excluded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.85).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.9).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.85).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, snare).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity: Native Generative Reading").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '20055f3c-f18f-4d99-bde9-6b65f7805a31').
narrative_ontology:cs_kernel_codification('20055f3c-f18f-4d99-bde9-6b65f7805a31', formalized).
narrative_ontology:cs_authority_grounding('20055f3c-f18f-4d99-bde9-6b65f7805a31', expertise).
narrative_ontology:cs_interpretation_layer_present('20055f3c-f18f-4d99-bde9-6b65f7805a31').
narrative_ontology:cs_reading_relation('20055f3c-f18f-4d99-bde9-6b65f7805a31', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('20055f3c-f18f-4d99-bde9-6b65f7805a31', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('20055f3c-f18f-4d99-bde9-6b65f7805a31', foundational, language_is_living_only_if_natively_spoken).
narrative_ontology:cs_axiom_status(language_is_living_only_if_natively_spoken, holdable).
narrative_ontology:cs_axiom_grounding('20055f3c-f18f-4d99-bde9-6b65f7805a31', language_is_living_only_if_natively_spoken, empirically_contingent).
narrative_ontology:cs_axiom('20055f3c-f18f-4d99-bde9-6b65f7805a31', secondary, lexical_expansion_and_phonological_standardization_are_essential_to_modern_language).
narrative_ontology:cs_axiom_status(lexical_expansion_and_phonological_standardization_are_essential_to_modern_language, holdable).
narrative_ontology:cs_axiom_grounding('20055f3c-f18f-4d99-bde9-6b65f7805a31', lexical_expansion_and_phonological_standardization_are_essential_to_modern_language, conventional).
narrative_ontology:cs_reference_frame('20055f3c-f18f-4d99-bde9-6b65f7805a31', modern_spoken_hebrew_norm).
narrative_ontology:cs_drift_state('20055f3c-f18f-4d99-bde9-6b65f7805a31', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('20055f3c-f18f-4d99-bde9-6b65f7805a31', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, modern_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_linguistic_institutions).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, non_native_hebrew_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., Academy of the Hebrew Language) actively promote and standardize modern Hebrew, defining its lexicon, grammar, and pronunciation. They enforce the 'native generative' ideal through education, media, and cultural policy, effectively delegitimizing other forms of Hebrew as 'dead' or 'inauthentic'.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_linguistic_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% As native speakers of modern Hebrew, they are the living embodiment of this reading's ideal. They benefit from the linguistic standardization and the cultural prestige associated with being the 'true' inheritors and perpetuators of the language. Their daily use reinforces the constraint.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, modern_hebrew_speakers, beneficiary,
    powerful, biographical, mobile, national).

% These communities primarily use Hebrew for prayer, study, and ritual, often with different pronunciations, grammatical structures, and lexical sets than modern Hebrew. Their form of Hebrew is often dismissed as 'dead' or 'irrelevant' by proponents of the native generative reading, leading to a loss of linguistic and cultural legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_liturgical_communities, payer,
    powerless, generational, identity_locked, global).

% Individuals who learn Hebrew as a second language, often for religious, academic, or cultural reasons. They face the implicit or explicit judgment that their Hebrew, lacking native intuition, is less 'real' or 'living,' despite their efforts to engage with the language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, non_native_hebrew_learners, payer,
    moderate, biographical, constrained, global).

% Linguists and scholars who study Hebrew's history, revitalization, and various forms. They can analyze the structural implications of the native generative reading but are not directly subject to its enforcement or extraction, though their work may be influenced by its dominant paradigm.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academics, observer,
    analytical, generational, analytical, global).

% Advocates for Hebrew as a contact language or pidginized form for broader Jewish diaspora interaction. Their vision is excluded by the native generative reading, which insists on full native fluency and generative use as the sole criterion for a 'living' language, leaving no space for intermediate or functional forms.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, bridge_pidginized_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, standardized, and actively evolving form of Hebrew for daily communication, national identity, and cultural production, enabling coherent linguistic interaction within the modern Israeli context.
% TRANSFER_FUNCTION: Transfers linguistic authority, cultural legitimacy, and educational resources from traditional/liturgical forms of Hebrew to modern, natively spoken Hebrew, effectively delegitimizing other modes of engagement with the language.
% ABSENT_VOICES: Liturgical-only communities and advocates for pidginized Hebrew are structurally excluded. They would argue for a broader, more inclusive definition of 'living' Hebrew that recognizes diverse forms of use and transmission, challenging the singular focus on native generative fluency.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the singular focus on native generative use would dissolve. Other forms of Hebrew (liturgical, academic, pidginized) would gain legitimacy, leading to a more pluralistic understanding of Hebrew's continuity. The cultural and national identity built around modern Hebrew as the *sole* living form would undergo significant re-evaluation.
% FOUNDING_PROBLEM: Hebrew was primarily a liturgical and scholarly language, not a daily spoken language, leading to concerns about its vitality, relevance, and ability to serve as the national language of a modern state.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., Israeli linguistic institutions) argue the problem is still live, emphasizing the ongoing need to maintain and evolve modern Hebrew. Critics (e.g., diaspora scholars, some linguists) contend that the original problem of 'dead' Hebrew was largely solved by the success of revitalization, and the current constraint now serves to exclude rather than to genuinely revitalize; independent historical and sociolinguistic analyses support this contested status.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because this reading extracts the very definition of 'living Hebrew' from alternative forms, concentrating linguistic and cultural authority. Suppression is also very high, as it actively enforces this definition through educational systems, linguistic academies, and social pressure, effectively suppressing the legitimacy and visibility of other Hebrew traditions. The theater ratio is low because the efforts to promote and standardize modern Hebrew are genuinely functional in establishing a national language, even if the exclusionary aspect is extractive. Accessibility collapse is high for non-native speakers to achieve 'native generative' status, and for liturgical communities whose forms are deemed invalid. Resistance is moderate from those whose linguistic practices are delegitimized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of modern Hebrew speakers and institutions, this constraint is a necessary coordination for national identity and a vibrant, living language. From the perspective of diaspora liturgical communities, it is a snare that extracts their linguistic heritage and cultural legitimacy, imposing a narrow, exclusionary definition of Hebrew's continuity.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli linguistic institutions and modern Hebrew speakers are the primary beneficiaries, as they define and embody the 'living' language. Diaspora liturgical communities and non-native Hebrew learners are the victims, as their forms of Hebrew are delegitimized and their linguistic practices suppressed. Advocates for pidginized Hebrew are excluded, as their alternative vision for Hebrew's role is entirely outside this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Is ''living language'' exclusively defined by native generative use, or can it encompass other forms of active, continuous engagement (e.g., liturgical, scholarly, pidginized)?',
    'Cross-cultural linguistic studies of language vitality in diverse contexts, particularly those with non-native or ritual-based continuity, and a re-evaluation of the criteria for language ''death'' and ''life''.',
    'If ''living language'' is defined more broadly, the extractiveness and suppression of this constraint would decrease, potentially reclassifying it from a Snare to a Tangled Rope or even a Rope, as it would no longer actively delegitimize other forms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the definition of ''living language'' and its impact on Hebrew''s continuity.').

omega_variable(
    legitimacy_of_diaspora_hebrew,
    'To what extent do diaspora liturgical communities perceive their Hebrew as ''dead'' or ''inauthentic'' versus a distinct, legitimate form of continuity?',
    'Sociolinguistic surveys and ethnographic studies within diaspora communities to assess self-perception of linguistic vitality and cultural value, independent of Israeli linguistic norms.',
    'If diaspora communities largely perceive their Hebrew as a living, legitimate form, the ''victim'' status and associated extractiveness of this constraint would be reinforced, highlighting the imposition of an external standard. If they largely internalize the ''dead'' label, it points to a deeper identity-lock mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_diaspora_hebrew, empirical, 'Perception of linguistic legitimacy among diaspora Hebrew users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1900, hebrew_continuity__native_generative, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(hebr_tr_t1925, hebrew_continuity__native_generative, theater_ratio, 1925, 0.08).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_continuity__native_generative, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(hebr_tr_t1975, hebrew_continuity__native_generative, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_continuity__native_generative, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_continuity__native_generative, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1900, hebrew_continuity__native_generative, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(hebr_be_t1925, hebrew_continuity__native_generative, base_extractiveness, 1925, 0.45).
narrative_ontology:measurement(hebr_be_t1950, hebrew_continuity__native_generative, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(hebr_be_t1975, hebrew_continuity__native_generative, base_extractiveness, 1975, 0.78).
narrative_ontology:measurement(hebr_be_t2000, hebrew_continuity__native_generative, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__native_generative, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1900, hebrew_continuity__native_generative, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(hebr_su_t1925, hebrew_continuity__native_generative, suppression_requirement, 1925, 0.55).
narrative_ontology:measurement(hebr_su_t1950, hebrew_continuity__native_generative, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(hebr_su_t1975, hebrew_continuity__native_generative, suppression_requirement, 1975, 0.8).
narrative_ontology:measurement(hebr_su_t2000, hebrew_continuity__native_generative, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__native_generative, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
