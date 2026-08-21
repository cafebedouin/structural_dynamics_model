% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality: Hybrid Continuity Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents an analytical reading of the 'Hebrew Vitality'
 *   kernel, asserting that liturgical preservation was a necessary but
 *   insufficient enabler for its modern vernacular revival. It posits that
 *   successful revitalization required both the existing substrate of ritual
 *   use and active, conscious reconstruction efforts. This reading attempts
 *   to synthesize the insights of purely liturgical and purely native-speaker
 *   definitions of language vitality, offering a more nuanced understanding
 *   of the historical process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.05).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.02).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, mountain).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality: Hybrid Continuity Reading").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:emerges_naturally(hebrew_vitality__hybrid_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '71332d11-dff7-4f41-b94c-2d3d495f68a3').
narrative_ontology:cs_kernel_codification('71332d11-dff7-4f41-b94c-2d3d495f68a3', distributed).
narrative_ontology:cs_authority_grounding('71332d11-dff7-4f41-b94c-2d3d495f68a3', diffuse_epistemic).
narrative_ontology:cs_reading_relation('71332d11-dff7-4f41-b94c-2d3d495f68a3', hebrew_vitality__liturgical_reading, influences).
narrative_ontology:cs_reading_relation('71332d11-dff7-4f41-b94c-2d3d495f68a3', hebrew_vitality__native_daily_reading, influences).
narrative_ontology:cs_axiom('71332d11-dff7-4f41-b94c-2d3d495f68a3', foundational, vitality_requires_both_substrate_and_reconstruction).
narrative_ontology:cs_axiom_status(vitality_requires_both_substrate_and_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('71332d11-dff7-4f41-b94c-2d3d495f68a3', vitality_requires_both_substrate_and_reconstruction, empirically_contingent).
narrative_ontology:cs_axiom('71332d11-dff7-4f41-b94c-2d3d495f68a3', secondary, liturgical_use_is_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(liturgical_use_is_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('71332d11-dff7-4f41-b94c-2d3d495f68a3', liturgical_use_is_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('71332d11-dff7-4f41-b94c-2d3d495f68a3', historical_linguistic_synthesis).
narrative_ontology:cs_drift_state('71332d11-dff7-4f41-b94c-2d3d495f68a3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('71332d11-dff7-4f41-b94c-2d3d495f68a3', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading synthesizes historical and linguistic data to explain how a language can transition from a liturgical-only state to a vernacular one, identifying the necessary conditions for such a revival.
% TRANSFER_FUNCTION: This constraint describes the flow of linguistic and cultural capital across generations and contexts, from liturgical use to modern vernacular, highlighting the interplay of preservation and active reconstruction.
% ABSENT_VOICES: This is an analytical reading; there are no 'absent voices' in the sense of suppressed parties. However, proponents of purely liturgical or purely native-speaker definitions of vitality might find this synthesis incomplete or overly complex.
% DISAPPEARANCE_RATIONALE: This constraint describes a historical and linguistic reality, not an active human-enforced arrangement. If this analytical framework disappeared, the historical facts of Hebrew's revival would remain, though their interpretation might revert to more polarized views.
% FOUNDING_PROBLEM: The problem this analytical framework addresses is understanding the complex, multi-faceted process of language revival, specifically how a language can be 'dead' in daily use yet 'alive' enough in other contexts to be successfully revived.
% FOUNDING_PROBLEM_CORROBORATION: Linguists, historians, and sociologists of language corroborate this problem, noting the ongoing debates about language vitality and the unique case of Hebrew. Their work provides the empirical and theoretical basis for this hybrid understanding, independent of any specific religious or nationalist agenda.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_vitality__hybrid_continuity_reading),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect this reading's nature as an analytical synthesis of historical and linguistic facts. It is 'Mountain-like' in that it describes an emergent historical process rather than an actively enforced human constraint. Extractiveness, suppression, and theater ratio are all very low because this is a descriptive framework, not an extractive mechanism. Accessibility collapse is high because the historical conditions for such a revival are specific and not easily replicated or ignored.
 *
 * PERSPECTIVAL GAP:
 *   This reading aims to bridge the perspectival gap between those who emphasize liturgical continuity and those who prioritize native daily use for defining language vitality. It argues that both perspectives capture part of the truth, but neither is sufficient on its own to explain the full phenomenon of Hebrew's revival.
 *
 * DIRECTIONALITY LOGIC:
 *   As an analytical framework describing a historical process, this constraint has no direct beneficiaries or victims in the traditional sense. Its 'beneficiaries' are those who gain a more complete understanding of language revival, and its 'victims' are none. This is why no stakeholders are declared.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    analytical_vs_actionable_constraint,
    'Is this ''hybrid continuity'' framework an analytical description of a historical process (a Mountain), or does it implicitly function as an actionable constraint guiding language revitalization efforts (a Rope/Scaffold)?',
    'Examine how this framework is used by language planners and policymakers: if it directly informs and structures their interventions, it has an actionable component. If it remains purely descriptive, it is a Mountain.',
    'If actionable, its extractiveness and suppression might be higher, reflecting the costs and enforcement of specific revitalization strategies, potentially reclassifying it as a Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_vs_actionable_constraint, conceptual, 'Ambiguity between descriptive analysis and prescriptive guidance.').

omega_variable(
    quantifying_substrate_contribution,
    'How can the ''substrate'' contribution of liturgical preservation be quantitatively distinguished from the ''reconstruction'' efforts in Hebrew''s revival?',
    'Develop metrics for measuring the linguistic features, vocabulary, and grammatical structures directly inherited from liturgical sources versus those innovated or adapted during the vernacularization process.',
    'A clearer distinction would refine the understanding of the relative importance of each factor, potentially shifting emphasis in future language revitalization models.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantifying_substrate_contribution, empirical, 'Measuring the relative contributions of preservation vs. reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 1800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1800, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1800, 0.01).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 1900, 0.01).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 2020, 0.01).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1800, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 2020, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1800, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1800, 0.02).
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 1900, 0.02).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 2020, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_vitality' kernel. This 'hybrid_continuity_reading' synthesizes the insights of the 'liturgical_reading' and 'native_daily_reading', arguing that both preservation and reconstruction were necessary for Hebrew's modern vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
