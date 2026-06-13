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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status: Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'living' if its sacred texts are
 *   continuously recited, studied, and used in ritual, asserting that
 *   liturgical transmission alone suffices for vitality. This is a specific
 *   reading of the broader 'living language status' kernel, emphasizing
 *   continuity of sacred practice over native generational transmission or
 *   new literary production. It coordinates a community around a fixed,
 *   sacred corpus, benefiting religious authorities who interpret and
 *   transmit these texts, while potentially marginalizing secular uses or
 *   communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.2).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.3).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status: Liturgical Preservation Reading").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, 'a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df').
narrative_ontology:cs_kernel_codification('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', fixed_text).
narrative_ontology:cs_authority_grounding('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', lineage).
narrative_ontology:cs_interpretation_layer_present('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df').
narrative_ontology:cs_reading_relation('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', foundational, sacred_text_transmission_is_life).
narrative_ontology:cs_axiom_status(sacred_text_transmission_is_life, holdable).
narrative_ontology:cs_axiom_grounding('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', sacred_text_transmission_is_life, theological).
narrative_ontology:cs_axiom('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', secondary, daily_speech_is_not_necessary_for_vitality).
narrative_ontology:cs_axiom_status(daily_speech_is_not_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', daily_speech_is_not_necessary_for_vitality, conventional).
narrative_ontology:cs_reference_frame('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', ancient_liturgical_tradition).
narrative_ontology:cs_drift_state('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', contemporary_sociolinguistics_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('a7dbad0b-d2a7-47e5-bc6f-5fd8fc04c3df', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, religious_scholars).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).

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
 *   The extractiveness (0.2) is low because the primary function is coordination around a shared sacred practice, which benefits the community by preserving a cultural and religious heritage. Suppression (0.3) is moderate, as it implicitly discourages or delegitimizes alternative forms of language 'life' (e.g., secular daily speech) without direct coercion. Theater ratio (0.1) is low, as the rituals and study are genuinely performed for their stated purpose. The metrics reflect a system that primarily coordinates, with some implicit costs for those whose definition of 'living' differs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinical authority and religious scholars, this constraint is a pure Rope, ensuring the continuity of a sacred tradition. From the perspective of a secular speech community, it might be seen as a Tangled Rope, as it coordinates the preservation of the language but at the cost of delegitimizing its use in daily, non-sacred contexts, thereby extracting the potential for broader linguistic flourishing.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and religious scholars are primary beneficiaries (d=0.0-0.1) as their interpretive monopoly and social role are preserved and reinforced by this definition. The secular speech community is a victim (d=0.6-0.7) as their potential for broader, non-liturgical use of the language is implicitly suppressed or deemed 'not living.' The constraint subsidizes the religious institutions by validating their mode of transmission as the sole criterion for vitality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a coordination mechanism (preserving sacred texts) as pure extraction, while still acknowledging the implicit costs and potential for delegitimization for those outside the liturgical framework. The low extractiveness and suppression suggest it's not a Snare, but the presence of victims and the potential for perspectival divergence indicate it's not a pure Mountain either. It functions as a Rope for its core beneficiaries, but with a subtle extractive component for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of language vitality, or a specific reading of the ''living language status'' kernel that serves particular institutional interests?',
    'Comparative analysis of language revitalization efforts across different traditions, assessing outcomes where liturgical use is the primary or sole mode of transmission versus those with native generation or literary production.',
    'If primarily an institutional reading, the constraint''s classification shifts from Rope (coordination around a shared sacred practice) to Tangled Rope (coordination for some, extraction for others, by legitimizing a specific form of language use while delegitimizing others).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''liturgical_preservation_reading'' of the ''living_language_status'' kernel. Sibling readings (''native_generation_reading'', ''literary_continuity_reading'') would shift the beneficiary/victim structure and extractiveness.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of resources for secular use) or internalized (belief that secular use desecrates the language)?',
    'Post-exit suppression trajectory: if secular communities continue to avoid using the language for daily speech even after resources for such use become available, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the secular community carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism regarding secular use of the language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(livi_tr_t10, living_language_status__liturgical_preservation_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(livi_tr_t30, living_language_status__liturgical_preservation_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(livi_be_t10, living_language_status__liturgical_preservation_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(livi_be_t30, living_language_status__liturgical_preservation_reading, base_extractiveness, 30, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(livi_su_t10, living_language_status__liturgical_preservation_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(livi_su_t30, living_language_status__liturgical_preservation_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'living_language_status' kernel. Other readings, such as 'native_generation_reading' and 'literary_continuity_reading', represent alternative definitions of language vitality with different structural properties and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
