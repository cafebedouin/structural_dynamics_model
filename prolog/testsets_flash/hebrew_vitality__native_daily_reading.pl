% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Vitality: Native Daily Reading as Sole Criterion
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint asserts that only the daily, native use of Hebrew
 *   constitutes true linguistic vitality, relegating ritual recitation to
 *   mere preservation. This reading emerged strongly during the Zionist
 *   project, which sought to transform Hebrew from a sacred, liturgical
 *   language into a modern, spoken vernacular. The constraint required
 *   significant institutional enforcement (e.g., language academies,
 *   educational systems) and lexical expansion to achieve its goals, often at
 *   the expense of the existing liturgical tradition and its associated
 *   communities. It is one reading of the 'hebrew_vitality' kernel, distinct
 *   from 'liturgical_reading' and 'hybrid_continuity_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.65).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.7).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vitality: Native Daily Reading as Sole Criterion").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7').
narrative_ontology:cs_kernel_codification('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', formalized).
narrative_ontology:cs_authority_grounding('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', lineage).
narrative_ontology:cs_interpretation_layer_present('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7').
narrative_ontology:cs_reading_relation('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', foundational, native_daily_use_is_sole_vitality).
narrative_ontology:cs_axiom_status(native_daily_use_is_sole_vitality, holdable).
narrative_ontology:cs_axiom_grounding('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', native_daily_use_is_sole_vitality, conventional).
narrative_ontology:cs_axiom('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', secondary, liturgical_use_is_preservation_not_life).
narrative_ontology:cs_axiom_status(liturgical_use_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', liturgical_use_is_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', modern_national_language_paradigm).
narrative_ontology:cs_drift_state('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', contemporary_globalized_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e2d2fa2e-a1ec-452b-8ee4-0b0ae2cbbdc7', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, secular_hebrew_academics).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_jewish_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a 'tangled_rope' because it genuinely coordinated the revival of Hebrew as a spoken language (a collective action problem) but did so through asymmetric extraction. It extracted from the liturgical tradition by devaluing its form of 'vitality' and from diaspora communities by imposing a new standard of 'authenticity'. The high suppression (0.70) reflects the active institutional effort to establish and enforce this new linguistic norm, including the suppression of alternative views on Hebrew's role. Extractiveness (0.65) is moderate, reflecting the costs borne by those whose existing relationship with Hebrew was devalued. Theater ratio is low (0.10) as the project was genuinely functional in its aim to create a living language.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Zionist state-building project and secular Hebrew academics, this constraint was a necessary 'rope' for national and cultural revival. From the perspective of the liturgical tradition and many diaspora communities, it was a 'snare' that devalued their heritage and imposed a new, often alienating, standard of linguistic authenticity. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and secular Hebrew academics are primary beneficiaries (d near 0.0) as they gained a national language and academic legitimacy. The liturgical tradition and diaspora Jewish communities are victims (d near 1.0) as their forms of Hebrew engagement were devalued and their cultural practices challenged. The constraint subsidized the creation of a new linguistic reality by extracting from an existing one.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to establish Hebrew as a living, spoken language. While this goal was largely achieved, the constraint persists in its implicit devaluation of other forms of Hebrew vitality. The classification as 'tangled_rope' prevents mislabeling it as a pure 'rope' (ignoring the extraction) or a pure 'snare' (ignoring the genuine coordination function of language revival). The 'contested' status of the founding problem highlights this ongoing tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of language vitality, or a specific reading of the ''hebrew_vitality'' kernel that prioritizes native, daily use over other forms of continuity?',
    'Comparative analysis of language revitalization efforts across different linguistic traditions, assessing the role of liturgical vs. vernacular use in long-term language survival and evolution.',
    'If it is merely one reading, its classification as a ''tangled_rope'' highlights the extractive nature of this specific interpretation, rather than an inherent property of Hebrew''s vitality. If it were a universal truth, it would be a ''mountain''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''native_daily_reading'' of the ''hebrew_vitality'' kernel.').

omega_variable(
    desacralization_impact,
    'To what extent did the emphasis on native, daily Hebrew use lead to the desacralization of the liturgical tradition, and was this an intended or unintended consequence?',
    'Historical and sociological studies tracing the shift in cultural perception and use of Hebrew within Jewish communities, particularly comparing communities that adopted vernacular Hebrew with those that maintained primarily liturgical use.',
    'If desacralization was an intended consequence, it strengthens the ''snare'' aspect of the constraint, as it actively undermined an existing form of vitality. If unintended, it points to a more complex ''tangled_rope'' where coordination for one goal (vernacular revival) had unforeseen extractive side effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desacralization_impact, empirical, 'Impact of vernacular revival on liturgical Hebrew''s sacred status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__native_daily_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t10, hebrew_vitality__native_daily_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__native_daily_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(hebr_tr_t30, hebrew_vitality__native_daily_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__native_daily_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hebr_be_t10, hebrew_vitality__native_daily_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__native_daily_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(hebr_be_t30, hebrew_vitality__native_daily_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__native_daily_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hebr_su_t10, hebrew_vitality__native_daily_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__native_daily_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(hebr_su_t30, hebrew_vitality__native_daily_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, israeli_national_identity_formation).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'hebrew_vitality' constraint family, which decomposes into multiple readings of how Hebrew's vitality is defined and sustained. This specific reading emphasizes vernacular, daily use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
