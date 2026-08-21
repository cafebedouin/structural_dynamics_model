% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Living Language Status (Literary Continuity Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'living' based on its continuous
 *   use for new literary and intellectual work, independent of native speaker
 *   numbers. This reading was crucial for movements like the Haskalah (Jewish
 *   Enlightenment) and the revival of Hebrew, which demonstrated vitality
 *   through periodicals and modern literature. It grants cultural authority
 *   to intellectual elites and de-emphasizes daily or liturgical use as
 *   primary indicators of a language's life. This is one reading of the
 *   'living_language_status' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.15).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.2).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Living Language Status (Literary Continuity Reading)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '5fd9ff87-edad-4dff-ab07-537e2390b752').
narrative_ontology:cs_kernel_codification('5fd9ff87-edad-4dff-ab07-537e2390b752', implicit).
narrative_ontology:cs_authority_grounding('5fd9ff87-edad-4dff-ab07-537e2390b752', practice).
narrative_ontology:cs_interpretation_layer_present('5fd9ff87-edad-4dff-ab07-537e2390b752').
narrative_ontology:cs_reading_relation('5fd9ff87-edad-4dff-ab07-537e2390b752', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fd9ff87-edad-4dff-ab07-537e2390b752', living_language_status__native_generation_reading, influences).
narrative_ontology:cs_axiom('5fd9ff87-edad-4dff-ab07-537e2390b752', foundational, literary_production_is_vitality).
narrative_ontology:cs_axiom_status(literary_production_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('5fd9ff87-edad-4dff-ab07-537e2390b752', literary_production_is_vitality, conventional).
narrative_ontology:cs_axiom('5fd9ff87-edad-4dff-ab07-537e2390b752', secondary, native_speaker_status_is_secondary).
narrative_ontology:cs_axiom_status(native_speaker_status_is_secondary, holdable).
narrative_ontology:cs_axiom_grounding('5fd9ff87-edad-4dff-ab07-537e2390b752', native_speaker_status_is_secondary, conventional).
narrative_ontology:cs_reference_frame('5fd9ff87-edad-4dff-ab07-537e2390b752', haskalah_intellectual_revival).
narrative_ontology:cs_drift_state('5fd9ff87-edad-4dff-ab07-537e2390b752', contemporary_sociolinguistics, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5fd9ff87-edad-4dff-ab07-537e2390b752', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups gain cultural authority and legitimacy for their intellectual and literary output by defining 'living' through continuous literary production. This allows them to claim vitality for languages like Hebrew without requiring mass native speakers.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals, beneficiary,
    organized, generational, mobile, global).

% Speakers who use the language in daily life but do not engage in literary or intellectual production are implicitly excluded from this definition of vitality, diminishing the perceived value of their linguistic practice. Their contribution to the language's 'life' is devalued.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers, payer,
    powerless, biographical, constrained, local).

% Academics who study language vitality, often engaging with various definitions and their implications for language policy and cultural identity. They analyze the criteria for 'living' and its impact on communities.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, linguistic_scholars, observer,
    analytical, generational, analytical, global).

% Communities that maintain a language primarily through liturgical use, without significant new literary output, find their claim to a 'living' language status challenged by this reading. They are excluded from the dominant discourse on vitality.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, religious_communities, excluded,
    organized, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of intellectuals and writers to produce new works in a language, establishing a shared criterion for its cultural relevance and 'living' status, thereby fostering a literary tradition.
% TRANSFER_FUNCTION: Transfers cultural authority and recognition of 'vitality' to literary and intellectual elites, while implicitly devaluing non-literary forms of language use.
% ABSENT_VOICES: Speakers who use the language primarily for daily communication or liturgical purposes, but not for new literary creation, are marginalized. They would argue for a broader definition of 'living' that includes their forms of use.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the cultural and intellectual movements that rely on it (like the Haskalah) would lose a key justification for their work. The status of languages like Hebrew would be re-evaluated, potentially shifting focus to native speakers or liturgical use, leading to a significant reordering of linguistic hierarchies and cultural narratives.
% FOUNDING_PROBLEM: To establish a criterion for language vitality that supports cultural revival and intellectual production, particularly for languages with limited native speakers but rich literary potential.
% FOUNDING_PROBLEM_CORROBORATION: Linguistic historians and cultural studies scholars corroborate that this problem was live during periods of national revival and continues to be relevant in discussions of language policy and cultural identity, particularly for languages undergoing revitalization efforts.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint primarily coordinates intellectual effort and grants cultural legitimacy rather than imposing direct material costs. Suppression is also low (0.2) as it operates more through cultural influence and academic discourse than coercive enforcement. The 'victim' group (illiterate speakers) experiences a devaluation of their linguistic practice, which is a form of soft extraction, but not direct material loss. The claimed type is 'rope' because it facilitates coordination among literary producers and provides a shared framework for language status, with minimal direct coercion.
 *
 * PERSPECTIVAL GAP:
 *   Intellectuals and literary figures perceive this as a beneficial coordination mechanism that validates their work and promotes cultural flourishing. Conversely, communities whose language use is primarily oral, traditional, or liturgical may perceive it as an exclusionary definition that marginalizes their linguistic heritage. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim and secular intellectuals are beneficiaries (d near 0.0) as this reading legitimizes their cultural projects and intellectual output. Illiterate or non-literary speakers are payers (d near 1.0) as their forms of language use are implicitly devalued. Linguistic scholars are observers (d near 0.5) analyzing the impact. Religious communities are excluded, as their primary mode of language preservation (liturgical) is not recognized as 'living' by this definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_vitality_scope,
    'Is the ''literary continuity'' definition of a living language universally applicable, or is it specific to contexts of language revival and elite cultural production?',
    'Comparative sociolinguistic studies across diverse language communities, including those without strong literary traditions or undergoing different forms of revitalization.',
    'If context-specific, the constraint''s claim to universal validity would be weakened, potentially reclassifying it as a ''snare'' for communities where it inappropriately devalues non-literary forms of vitality. If universal, its ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_vitality_scope, conceptual, 'The generalizability of the literary continuity criterion for language vitality.').

omega_variable(
    cultural_authority_vs_linguistic_reality,
    'To what extent does this reading reflect actual linguistic vitality versus serving as a tool for cultural elites to assert authority over language status?',
    'Analysis of language use patterns among diverse speaker populations, independent of literary output, and examination of power dynamics within language planning and policy.',
    'If primarily a tool for elite authority, the ''extractiveness'' and ''suppression'' metrics would be re-evaluated upwards, potentially shifting the classification towards ''tangled_rope'' or ''snare'' due to the implicit exclusion and devaluation of non-elite linguistic practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_authority_vs_linguistic_reality, empirical, 'The balance between descriptive accuracy and power dynamics in defining language vitality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(livi_be_t1800, living_language_status__literary_continuity_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(livi_be_t1850, living_language_status__literary_continuity_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(livi_be_t1900, living_language_status__literary_continuity_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(livi_be_t1950, living_language_status__literary_continuity_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(livi_be_t2000, living_language_status__literary_continuity_reading, base_extractiveness, 2000, 0.16).
narrative_ontology:measurement(livi_be_t2020, living_language_status__literary_continuity_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1800, living_language_status__literary_continuity_reading, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement(livi_su_t1850, living_language_status__literary_continuity_reading, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement(livi_su_t1900, living_language_status__literary_continuity_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(livi_su_t1950, living_language_status__literary_continuity_reading, suppression_requirement, 1950, 0.19).
narrative_ontology:measurement(livi_su_t2000, living_language_status__literary_continuity_reading, suppression_requirement, 2000, 0.21).
narrative_ontology:measurement(livi_su_t2020, living_language_status__literary_continuity_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
