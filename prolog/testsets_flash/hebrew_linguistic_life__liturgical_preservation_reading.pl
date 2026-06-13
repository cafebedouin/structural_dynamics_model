% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Linguistic Life: Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines the 'life' of the Hebrew language not by its use
 *   in daily vernacular speech or native acquisition, but by the continuous,
 *   unbroken chain of its recitation, study, and transmission in sacred texts
 *   and liturgical contexts. It asserts that Hebrew never 'died' and
 *   therefore did not need 'revival' by figures like Eliezer Ben-Yehuda,
 *   whose project is viewed as a desecration of its sacred status. This is
 *   one reading of the 'hebrew_linguistic_life' kernel, which has sibling
 *   readings focused on vernacular use and native acquisition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.15).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.2).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life: Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'd5f0ee75-fdd4-4e2b-918f-b725464e2ebd').
narrative_ontology:cs_kernel_codification('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', fixed_text).
narrative_ontology:cs_authority_grounding('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', lineage).
narrative_ontology:cs_interpretation_layer_present('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd').
narrative_ontology:cs_reading_relation('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', foundational, hebrew_never_died).
narrative_ontology:cs_axiom_status(hebrew_never_died, holdable).
narrative_ontology:cs_axiom_grounding('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', hebrew_never_died, theological).
narrative_ontology:cs_axiom('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', foundational, liturgical_use_is_linguistic_life).
narrative_ontology:cs_axiom_status(liturgical_use_is_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', liturgical_use_is_linguistic_life, conventional).
narrative_ontology:cs_reference_frame('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', unbroken_sacred_chain).
narrative_ontology:cs_drift_state('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', contemporary_secular_linguistics, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d5f0ee75-fdd4-4e2b-918f-b725464e2ebd', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, traditional_religious_scholars).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because it asserts a natural, unbroken continuity of Hebrew's life through sacred tradition, independent of secular linguistic criteria. Extractiveness (0.15) is low, representing the 'cost' of maintaining this interpretive framework against competing views, primarily through scholarly and communal effort rather than direct coercion. Suppression (0.2) is also low, as it primarily involves the suppression of alternative definitions of linguistic vitality within the traditional framework, rather than active coercion of individuals. Theater ratio is very low (0.05) as the liturgical practices are genuinely functional for the communities involved. Accessibility collapse is high (0.8) because, within this framework, alternatives to liturgical preservation as the definition of 'life' are largely dismissed. Resistance is low (0.05) because within the communities that hold this view, there is little internal resistance to the definition itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of traditional religious scholars, this constraint is a self-evident truth, a Mountain reflecting the inherent nature of Hebrew's sacred continuity. From the perspective of secular linguists, it is a constructed claim that serves to maintain the authority of religious institutions, potentially computing as a Tangled Rope or Snare if their definitions of 'linguistic life' are considered. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional religious scholars and liturgical communities are beneficiaries, as their identity and practices are validated by this definition of Hebrew's life. The 'sacred tradition itself' is identified as a victim, as its integrity is perceived to be harmed by alternative, secular definitions of linguistic vitality. Secular Hebrew speakers are excluded, as their usage is deemed irrelevant to the language's 'true' life.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_claim,
    'Is the continuous ''life'' of Hebrew through liturgical preservation a natural, inherent property of the language''s sacred status, or a constructed claim that benefits identifiable religious institutions?',
    'Analysis of historical shifts in linguistic theory and religious doctrine, examining whether the definition of ''linguistic life'' has evolved in response to external pressures or internal theological developments.',
    'If primarily a constructed claim, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope), reflecting the institutional benefits and the suppression of alternative definitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_claim, conceptual, 'Ambiguity between inherent sacred status and institutional construction of linguistic vitality.').

omega_variable(
    ben_yehuda_desecration_or_revival,
    'Is Eliezer Ben-Yehuda''s project of reviving Hebrew as a spoken vernacular a desecration of its sacred tradition (as this reading claims), or a legitimate act of linguistic revitalization?',
    'Historical and sociological analysis of the impact of Ben-Yehuda''s work on both secular and religious communities, and the long-term effects on Hebrew''s status and usage.',
    'If Ben-Yehuda''s project is widely accepted as legitimate revitalization, this reading''s claim of ''desecration'' would be undermined, potentially weakening its authority and leading to a re-evaluation of its ''emerges_naturally'' status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ben_yehuda_desecration_or_revival, empirical, 'Contested interpretation of modern Hebrew''s ''revival''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1800, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1800, 0.03).
narrative_ontology:measurement(hebr_tr_t1850, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1850, 0.04).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1800, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(hebr_be_t1850, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(hebr_be_t1900, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(hebr_be_t1950, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(hebr_be_t2000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(hebr_be_t2024, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1800, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement(hebr_su_t1850, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement(hebr_su_t1900, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(hebr_su_t1950, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1950, 0.19).
narrative_ontology:measurement(hebr_su_t2000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(hebr_su_t2024, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_linguistic_life' kernel, each defining linguistic vitality differently. This reading emphasizes liturgical preservation, while others focus on native acquisition or marketplace use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
