% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Language Vitality via Liturgical Preservation
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'living' if its sacred texts are
 *   continuously recited, studied, and used in ritual, asserting that
 *   liturgical transmission alone suffices for vitality. This is one reading
 *   of the broader 'living_language_status' kernel. It coordinates a
 *   community around a fixed corpus but extracts from those who seek broader
 *   definitions of language vitality, such as daily spoken use or modern
 *   literary production. The claimed type is 'tangled_rope' because it
 *   provides a coordination function (preserving sacred texts) but also
 *   involves asymmetric extraction of interpretive authority and suppression
 *   of alternative linguistic practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.25).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.7).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Language Vitality via Liturgical Preservation").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, 'fcb54001-9cc9-48ce-a93a-224a7a520aba').
narrative_ontology:cs_kernel_codification('fcb54001-9cc9-48ce-a93a-224a7a520aba', fixed_text).
narrative_ontology:cs_authority_grounding('fcb54001-9cc9-48ce-a93a-224a7a520aba', lineage).
narrative_ontology:cs_interpretation_layer_present('fcb54001-9cc9-48ce-a93a-224a7a520aba').
narrative_ontology:cs_reading_relation('fcb54001-9cc9-48ce-a93a-224a7a520aba', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('fcb54001-9cc9-48ce-a93a-224a7a520aba', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('fcb54001-9cc9-48ce-a93a-224a7a520aba', foundational, liturgical_use_confers_vitality).
narrative_ontology:cs_axiom_status(liturgical_use_confers_vitality, holdable).
narrative_ontology:cs_axiom_grounding('fcb54001-9cc9-48ce-a93a-224a7a520aba', liturgical_use_confers_vitality, theological).
narrative_ontology:cs_axiom('fcb54001-9cc9-48ce-a93a-224a7a520aba', secondary, daily_use_is_not_a_prerequisite).
narrative_ontology:cs_axiom_status(daily_use_is_not_a_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('fcb54001-9cc9-48ce-a93a-224a7a520aba', daily_use_is_not_a_prerequisite, conventional).
narrative_ontology:cs_reference_frame('fcb54001-9cc9-48ce-a93a-224a7a520aba', traditional_liturgical_continuity).
narrative_ontology:cs_drift_state('fcb54001-9cc9-48ce-a93a-224a7a520aba', contemporary_sociolinguistic_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fcb54001-9cc9-48ce-a93a-224a7a520aba', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, liturgical_practitioners).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, linguistic_revivalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the criteria for a language's 'living' status, emphasizing continuous liturgical use. Benefits from the interpretive monopoly and cultural capital derived from being the custodians of sacred texts and rituals.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Participate in the continuous recitation, study, and ritual use of sacred texts, thereby affirming the language's living status within this framework. They gain social standing and spiritual fulfillment from their role.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, liturgical_practitioners, beneficiary,
    organized, biographical, constrained, local).

% Their daily, non-liturgical use of the language is often delegitimized or deemed insufficient for 'living' status by this framework, potentially leading to a sense of cultural alienation or desecration if they deviate from prescribed usage.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, secular_speech_community, excluded).

% Advocate for the language's vitality through native generational transmission and modern literary production. Their efforts are often dismissed as not truly contributing to the language's 'living' status under the liturgical preservation definition, undermining their legitimacy.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, linguistic_revivalists, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, linguistic_revivalists, excluded).

% Study language vitality from a scientific perspective, often using criteria like native speakers, daily use, and new literary production. They observe the dynamics of this constraint and its impact on language communities without directly participating in its enforcement or benefiting from its operation.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, sociolinguists, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community around the continuous study, recitation, and ritual use of sacred texts, ensuring the language's preservation through liturgical transmission and maintaining its sacred status.
% TRANSFER_FUNCTION: Transfers interpretive authority and cultural capital to the rabbinical authority and liturgical practitioners, while implicitly delegitimizing alternative forms of language vitality and secular uses of the language.
% ABSENT_VOICES: Secular speech communities and linguistic revivalists are often excluded from the discourse on language vitality within this framework; they would argue for broader definitions of 'living' based on daily use or modern literary output.
% DISAPPEARANCE_RATIONALE: If this definition of language vitality vanished, the status of many historically liturgical languages would be re-evaluated, potentially empowering secular communities and revivalist movements. The authority structures built around this definition would lose a key grounding for their interpretive monopoly.
% FOUNDING_PROBLEM: To ensure the continuity and sacred status of a language, particularly in diaspora or periods of political subjugation, by defining its vitality through its ritual and textual use, independent of daily spoken use or secular innovation.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and community leaders within traditions that maintain liturgical languages corroborate this, emphasizing the historical role of ritual in preserving linguistic and cultural heritage. Sociolinguists might acknowledge the historical function but contest its contemporary applicability as the sole definition of 'living'.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.25) because the primary function is coordination around a shared sacred practice, but there is a subtle extraction of legitimacy from alternative language uses. Suppression is high (0.70) because this reading actively delegitimizes or excludes other criteria for language vitality, requiring active enforcement of its interpretive framework. Theater ratio is moderate (0.40) as ritual performance is integral to the function of preservation, but some aspects may become more performative as the definition is challenged. Accessibility collapse is high (0.80) from the perspective of this reading, as it claims to offer the sufficient condition for vitality, thereby collapsing other alternatives. Resistance is moderate (0.60) from those who advocate for different definitions of 'living'.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinical authority, this constraint is a vital rope, ensuring the continuity of sacred tradition. From the perspective of secular speech communities or linguistic revivalists, it functions as a snare or tangled rope, suppressing broader linguistic vitality and maintaining an exclusionary definition.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and liturgical practitioners are beneficiaries, gaining interpretive monopoly and cultural capital. Secular speech communities and linguistic revivalists are victims, as their forms of language engagement are delegitimized or excluded. Sociolinguists act as observers, analyzing the constraint's effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_vs_necessity_ambiguity,
    'Does ''preservation through liturgical transmission suffices'' imply that other forms of vitality are unnecessary, or merely that liturgical use is one valid path?',
    'Analysis of authoritative texts and pronouncements from the rabbinical authority: explicit statements on the status of non-liturgical use would clarify the intent.',
    'If it implies other forms are unnecessary, the suppression and extraction are higher, reinforcing the ''tangled_rope'' classification. If it allows for other valid paths, the constraint''s extractiveness and suppression would be lower, leaning towards a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_vs_necessity_ambiguity, conceptual, 'Ambiguity in the scope of ''sufficiency'' for language vitality.').

omega_variable(
    impact_of_native_generation_reading,
    'How would the ''native_generation_reading'' (requiring daily mother-tongue transmission) structurally impact this ''liturgical_preservation_reading''?',
    'Empirical observation of communities where both readings are present: does the presence of native speakers diminish the perceived vitality of liturgical-only languages, or are they seen as distinct categories?',
    'The ''native_generation_reading'' directly challenges the ''sufficiency'' claim of this reading. If widely adopted, it would reclassify many liturgically preserved languages as ''dead'' or ''revived'', undermining the authority of this constraint and its beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impact_of_native_generation_reading, empirical, 'Structural impact of the native generation reading on liturgical preservation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of secular language use structural (lack of educational resources, social pressure) or internalized (belief that secular use is profane)?',
    'Post-exit suppression trajectory: if individuals continue to avoid secular use even after leaving the direct influence of the rabbinical authority, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for secular language use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(livi_tr_t10, living_language_status__liturgical_preservation_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(livi_tr_t30, living_language_status__liturgical_preservation_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(livi_tr_t50, living_language_status__liturgical_preservation_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(livi_be_t10, living_language_status__liturgical_preservation_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(livi_be_t30, living_language_status__liturgical_preservation_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(livi_be_t50, living_language_status__liturgical_preservation_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(livi_su_t10, living_language_status__liturgical_preservation_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(livi_su_t30, living_language_status__liturgical_preservation_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(livi_su_t50, living_language_status__liturgical_preservation_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'living_language_status' kernel, each with different criteria for language vitality. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
