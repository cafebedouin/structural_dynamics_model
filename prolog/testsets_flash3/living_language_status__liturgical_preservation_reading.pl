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
 *   human_readable: Living Language Status: Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'living' if its sacred texts are
 *   continuously recited, studied, and used in ritual, asserting that
 *   liturgical transmission alone suffices for vitality. This is one reading
 *   of the 'living_language_status' kernel, which is contested by other
 *   readings emphasizing native generational transmission or literary
 *   continuity. This reading primarily coordinates the preservation of a
 *   fixed religious corpus and the authority of its interpreters.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.25).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.4).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status: Liturgical Preservation Reading").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, 'e5465cbd-53fa-464a-ac6f-e2d2161a2754').
narrative_ontology:cs_kernel_codification('e5465cbd-53fa-464a-ac6f-e2d2161a2754', fixed_text).
narrative_ontology:cs_authority_grounding('e5465cbd-53fa-464a-ac6f-e2d2161a2754', lineage).
narrative_ontology:cs_interpretation_layer_present('e5465cbd-53fa-464a-ac6f-e2d2161a2754').
narrative_ontology:cs_reading_relation('e5465cbd-53fa-464a-ac6f-e2d2161a2754', living_language_status__native_generation_reading, influences).
narrative_ontology:cs_reading_relation('e5465cbd-53fa-464a-ac6f-e2d2161a2754', living_language_status__literary_continuity_reading, influences).
narrative_ontology:cs_axiom('e5465cbd-53fa-464a-ac6f-e2d2161a2754', foundational, sacred_text_transmission_is_vitality).
narrative_ontology:cs_axiom_status(sacred_text_transmission_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('e5465cbd-53fa-464a-ac6f-e2d2161a2754', sacred_text_transmission_is_vitality, theological).
narrative_ontology:cs_axiom('e5465cbd-53fa-464a-ac6f-e2d2161a2754', foundational, liturgical_use_suffices_for_living_status).
narrative_ontology:cs_axiom_status(liturgical_use_suffices_for_living_status, holdable).
narrative_ontology:cs_axiom_grounding('e5465cbd-53fa-464a-ac6f-e2d2161a2754', liturgical_use_suffices_for_living_status, conventional).
narrative_ontology:cs_reference_frame('e5465cbd-53fa-464a-ac6f-e2d2161a2754', ancient_liturgical_continuity).
narrative_ontology:cs_drift_state('e5465cbd-53fa-464a-ac6f-e2d2161a2754', contemporary_secularization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e5465cbd-53fa-464a-ac6f-e2d2161a2754', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, religious_scholars).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the criteria for a language's 'living' status within religious communities. Benefits from the interpretive monopoly over sacred texts and the associated cultural authority. Their identity is fused with the preservation of the liturgical tradition.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the continued relevance and funding for the study of sacred texts. Their careers and intellectual identity are tied to the liturgical tradition. They contribute to the continuous recitation and study.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, religious_scholars, beneficiary,
    organized, biographical, constrained, global).

% Their use of the language in daily, non-liturgical contexts is implicitly or explicitly delegitimized as not contributing to the language's 'living' status, potentially leading to a sense of cultural alienation or 'desecration' if they deviate from prescribed usage. They bear the cost of a narrow definition of vitality.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% Advocate for the language's use in modern, secular contexts and for native generational transmission. Their efforts are often dismissed or undervalued by the liturgical preservation reading, as their criteria for 'living' differ fundamentally.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, linguistic_revivalists, excluded,
    moderate, generational, mobile, national).

% Study language vitality using empirical metrics like native speaker numbers, domains of use, and generational transmission. They analyze the claims of various readings without endorsing any particular one, focusing on descriptive accuracy.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, sociolinguists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous recitation, study, and ritual use of sacred texts, ensuring the preservation of a fixed liturgical corpus and its associated interpretive traditions across generations.
% TRANSFER_FUNCTION: Transfers cultural authority and interpretive monopoly over the language's 'living' status to rabbinical and religious scholarly institutions, from secular or modernizing linguistic movements.
% ABSENT_VOICES: Linguistic revivalists and secular educators, who would argue for a broader definition of language vitality based on daily use and native generational transmission, are often marginalized or excluded from the discourse on the language's 'living' status by this reading.
% DISAPPEARANCE_RATIONALE: If this definition of 'living language' vanished, the authority of religious institutions over the language's status would diminish, potentially empowering secular linguistic movements and altering the cultural landscape of language preservation efforts. The funding and focus on liturgical study might also shift.
% FOUNDING_PROBLEM: The problem of ensuring the continuity and sacred status of a language primarily used for religious texts, in the face of diaspora, assimilation, and the rise of vernaculars.
% FOUNDING_PROBLEM_CORROBORATION: Religious communities and scholars attest that the problem of preserving sacred languages remains live, citing ongoing challenges to religious observance and cultural continuity. Sociolinguists corroborate the historical challenge of language maintenance in diaspora, though they may dispute the efficacy or exclusivity of liturgical preservation as a solution.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.25) because the primary function is coordination around a shared sacred practice, with costs mainly borne by those outside the interpretive authority. Suppression is moderate (0.4) as this reading implicitly suppresses alternative definitions of vitality, but does not actively coerce participation. Theater ratio is low (0.1) as the recitation and study are genuine practices, not merely performative. Accessibility collapse is moderate (0.6) as it narrows the definition of 'living' but doesn't eliminate other forms of language use. Resistance is moderate (0.3) from those advocating for broader definitions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinical authority, this is a pure rope, ensuring the continuity of a sacred tradition. From the perspective of secular speech communities or linguistic revivalists, it is a constraint that limits the definition of 'living' and potentially extracts cultural legitimacy from their efforts.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and religious scholars are beneficiaries, as this reading preserves their interpretive monopoly and cultural relevance (low d). The secular speech community is a payer, as their non-liturgical use is implicitly devalued (higher d). Linguistic revivalists are excluded, as their criteria for vitality are outside this reading's scope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Is ''living'' status for a language fundamentally defined by liturgical transmission, native generational use, or literary productivity?',
    'Conceptual analysis and community consensus on the primary purpose of language vitality (e.g., sacred continuity vs. daily communication vs. cultural innovation).',
    'Resolution would either validate this reading''s claim (if sacred continuity is prioritized) or reclassify the constraint as a snare or tangled rope if its primary function is found to be the suppression of alternative definitions to maintain institutional authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the core definition of a ''living language''.').

omega_variable(
    suppression_of_alternative_vitality,
    'To what extent does the emphasis on liturgical preservation actively suppress or merely marginalize efforts to revive the language through native generational transmission or modern literary output?',
    'Empirical study of resource allocation, institutional policies, and public discourse within communities that adhere to this reading, specifically examining the active discouragement or defunding of non-liturgical language initiatives.',
    'If active suppression is found, the ''suppression'' metric would be higher, potentially shifting the classification towards a tangled rope or snare, as the coordination function would be seen as cover for maintaining a narrow, extractive definition of vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_vitality, empirical, 'Distinguishing between passive marginalization and active suppression of alternative language vitality efforts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(livi_tr_t10, living_language_status__liturgical_preservation_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(livi_tr_t30, living_language_status__liturgical_preservation_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(livi_tr_t50, living_language_status__liturgical_preservation_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(livi_be_t10, living_language_status__liturgical_preservation_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(livi_be_t30, living_language_status__liturgical_preservation_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(livi_be_t50, living_language_status__liturgical_preservation_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(livi_su_t10, living_language_status__liturgical_preservation_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(livi_su_t30, living_language_status__liturgical_preservation_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(livi_su_t50, living_language_status__liturgical_preservation_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'living_language_status' kernel. Its definition of vitality (liturgical preservation) directly influences the perceived legitimacy and resource allocation for other readings, such as those emphasizing native generational transmission or literary continuity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
