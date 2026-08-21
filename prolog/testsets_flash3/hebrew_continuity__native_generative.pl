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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity: Native Generative Reading
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'native generative' reading of Hebrew
 *   continuity, asserting that Hebrew truly lives only through the intuition
 *   and daily use of native speakers. This reading emphasizes lexical
 *   expansion and phonological standardization, often at the expense of other
 *   forms of Hebrew. It is a tangled rope because it genuinely coordinates a
 *   modern national language while extracting linguistic legitimacy and
 *   cultural capital from non-native and liturgical communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.68).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.75).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity: Native Generative Reading").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, 'a0209562-7427-46d5-83db-0e282109ad34').
narrative_ontology:cs_kernel_codification('a0209562-7427-46d5-83db-0e282109ad34', formalized).
narrative_ontology:cs_authority_grounding('a0209562-7427-46d5-83db-0e282109ad34', lineage).
narrative_ontology:cs_interpretation_layer_present('a0209562-7427-46d5-83db-0e282109ad34').
narrative_ontology:cs_reading_relation('a0209562-7427-46d5-83db-0e282109ad34', hebrew_continuity__liturgical_preservation, influences).
narrative_ontology:cs_reading_relation('a0209562-7427-46d5-83db-0e282109ad34', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('a0209562-7427-46d5-83db-0e282109ad34', foundational, language_vitality_requires_native_generative_use).
narrative_ontology:cs_axiom_status(language_vitality_requires_native_generative_use, holdable).
narrative_ontology:cs_axiom_grounding('a0209562-7427-46d5-83db-0e282109ad34', language_vitality_requires_native_generative_use, empirically_contingent).
narrative_ontology:cs_axiom('a0209562-7427-46d5-83db-0e282109ad34', secondary, lexical_expansion_and_phonological_standardization_are_essential).
narrative_ontology:cs_axiom_status(lexical_expansion_and_phonological_standardization_are_essential, holdable).
narrative_ontology:cs_axiom_grounding('a0209562-7427-46d5-83db-0e282109ad34', lexical_expansion_and_phonological_standardization_are_essential, conventional).
narrative_ontology:cs_reference_frame('a0209562-7427-46d5-83db-0e282109ad34', modern_spoken_hebrew_as_national_language).
narrative_ontology:cs_drift_state('a0209562-7427-46d5-83db-0e282109ad34', contemporary_globalized_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a0209562-7427-46d5-83db-0e282109ad34', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_linguistic_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, non_native_hebrew_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institutional body that codifies and promotes modern Hebrew, emphasizing its native, generative use. It actively standardizes vocabulary and grammar, often marginalizing non-native or liturgical forms as 'inauthentic' or 'dead'.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_linguistic_academy, agenda_setter,
    institutional, generational, constrained, national).

% Their daily, intuitive use of Hebrew is validated as the 'true' form of the language. They benefit from the linguistic standardization and the cultural centrality of modern Hebrew, which reinforces their identity and social cohesion.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Their traditional, liturgical use of Hebrew is often dismissed as archaic or non-living by the native-generative framework. They bear the cost of linguistic delegitimization, facing pressure to adopt modern Israeli Hebrew or have their connection to the language devalued. Their identity is deeply tied to their traditional Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_liturgical_communities, payer,
    powerless, generational, identity_locked, global).

% Academics who study Hebrew in its various historical and non-native forms. They face pressure to align their research with the native-generative paradigm, potentially having their work on liturgical or historical Hebrew marginalized as less relevant to the 'living' language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, non_native_hebrew_scholars, payer,
    moderate, biographical, constrained, global).

% Analyze the success and methods of Hebrew revitalization, often noting the tension between native-speaker ideals and the historical continuity of the language through other forms. They can offer alternative frameworks for language vitality.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, language_revitalization_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, standardized form of Hebrew for daily communication, education, and national identity, enabling efficient and unambiguous generative use across a diverse population.
% TRANSFER_FUNCTION: Transfers linguistic authority and cultural capital from diverse historical and liturgical forms of Hebrew to the modern, natively spoken form, consolidating it within the Israeli national context.
% ABSENT_VOICES: Historical linguists advocating for a broader definition of language vitality that includes non-native and liturgical uses, and diaspora communities who feel their connection to Hebrew is being invalidated, are often excluded from the discourse on 'living' Hebrew. They would argue for a more inclusive understanding of Hebrew's continuity.
% DISAPPEARANCE_RATIONALE: If the constraint that Hebrew lives only through native generative use vanished, the Israeli linguistic academy's authority would diminish, and there would be a re-evaluation of the legitimacy of other forms of Hebrew. Diaspora communities might feel less pressure to adopt modern Hebrew, and the definition of 'living' Hebrew would broaden, leading to a more diverse linguistic landscape.
% FOUNDING_PROBLEM: The problem of establishing a common, modern, spoken language for the nascent Israeli state, distinct from the liturgical and scholarly uses of Hebrew that were not suitable for daily generative communication.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli linguistic academy and native Hebrew speakers attest that the problem of maintaining a vibrant, generative modern Hebrew is still live, citing ongoing needs for linguistic standardization and adaptation to new contexts. Language revitalization theorists corroborate the historical necessity of establishing a modern spoken language, though they may contest the exclusivity of the 'native generative' definition.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because this reading actively devalues and marginalizes other forms of Hebrew, effectively extracting their linguistic legitimacy. Suppression is also high, as institutional bodies actively enforce the native-generative standard, suppressing alternatives. The theater ratio is low because the effort to standardize and promote modern Hebrew is largely functional, not performative. Accessibility collapse is moderate, as alternatives (liturgical, scholarly Hebrew) still exist but are deemed less legitimate. Resistance is moderate, coming from diaspora communities and some scholars.
 *
 * PERSPECTIVAL GAP:
 *   Native speakers and the linguistic academy perceive this constraint as a necessary coordination mechanism for a living language. Diaspora communities and non-native scholars experience it as an extractive force that diminishes their connection to Hebrew. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli Linguistic Academy and native Hebrew speakers are beneficiaries, as their linguistic practices are validated and promoted. Diaspora liturgical communities and non-native Hebrew scholars are victims, as their forms of Hebrew are delegitimized. Language revitalization theorists act as observers, analyzing the dynamics without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_language_vitality,
    'Is ''native generative use'' the sole valid criterion for language vitality, or can other forms of use (liturgical, scholarly, pidginized) also constitute a ''living'' language?',
    'Cross-linguistic studies of language revitalization and maintenance in diverse contexts, particularly those with long histories of non-native or ritualized use.',
    'If other forms are recognized as vital, the extractiveness and suppression of this constraint would decrease, potentially reclassifying it as a rope or even a mountain (if the coordination function is primary and non-extractive). If it remains the sole criterion, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_language_vitality, conceptual, 'Ambiguity in the definition of a ''living'' language and its implications for Hebrew''s continuity.').

omega_variable(
    impact_on_diaspora_identity,
    'To what extent does the emphasis on native generative Hebrew undermine the Jewish identity and cultural continuity of diaspora communities who primarily engage with Hebrew through liturgical or scholarly means?',
    'Sociological and ethnographic studies within diaspora communities, measuring self-reported connection to Hebrew and Jewish identity in response to modern Hebrew''s dominance.',
    'If the impact is severe, it strengthens the ''victim'' status of diaspora communities and increases the perceived extractiveness of the constraint. If the impact is negligible, it weakens the victim claim and suggests a more benign coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_diaspora_identity, empirical, 'The social and cultural cost borne by diaspora communities due to the native-generative paradigm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1920, hebrew_continuity__native_generative, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_continuity__native_generative, theater_ratio, 1940, 0.08).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_continuity__native_generative, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_continuity__native_generative, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_continuity__native_generative, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_continuity__native_generative, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1920, hebrew_continuity__native_generative, base_extractiveness, 1920, 0.4).
narrative_ontology:measurement(hebr_be_t1940, hebrew_continuity__native_generative, base_extractiveness, 1940, 0.5).
narrative_ontology:measurement(hebr_be_t1960, hebrew_continuity__native_generative, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(hebr_be_t1980, hebrew_continuity__native_generative, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(hebr_be_t2000, hebrew_continuity__native_generative, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(hebr_be_t2020, hebrew_continuity__native_generative, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1920, hebrew_continuity__native_generative, suppression_requirement, 1920, 0.3).
narrative_ontology:measurement(hebr_su_t1940, hebrew_continuity__native_generative, suppression_requirement, 1940, 0.45).
narrative_ontology:measurement(hebr_su_t1960, hebrew_continuity__native_generative, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(hebr_su_t1980, hebrew_continuity__native_generative, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(hebr_su_t2000, hebrew_continuity__native_generative, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(hebr_su_t2020, hebrew_continuity__native_generative, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel. This 'native_generative' reading emphasizes modern, spoken Hebrew, while 'liturgical_preservation' focuses on ritual use and 'bridge_pidginized' on contact language. Each represents a distinct structural claim about how Hebrew lives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
