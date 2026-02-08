% ============================================================================
% CONSTRAINT STORY: spv_variations
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_spv_variations, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: spv_variations
 * human_readable: Stratospheric Polar Vortex Variations (P2/P3)
 * domain: atmospheric_science
 * * SUMMARY:
 * This constraint maps the primary stratospheric variations (P2 and P3)
 * that lead to extreme cold-air outbreaks (CAOs) in the continental US
 * despite overall winter warming trends. The constraint is the physical
 * phenomenon itself, but also the human classification system used to predict it.
 * * KEY AGENTS:
 * - The US Resident: Subject, powerless against extreme cold and snow.
 * - Forecast Systems: Beneficiary of the classification system, which enables coordination.
 * - Atmospheric Scientists: Analytical observer of the physical laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is low (0.02) as this is a natural law (Mountain) based on
% stratospheric dynamics rather than human coordination.
domain_priors:base_extractiveness(spv_variations, 0.02). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(spv_variations, 0.10).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(spv_variations, 0.05).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(spv_variations, extractiveness, 0.02).
narrative_ontology:constraint_metric(spv_variations, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(spv_variations, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% The underlying phenomenon is a natural law.
narrative_ontology:constraint_claim(spv_variations, mountain).

% Structural property derivation hooks:
% The classification system provides a coordination function for forecasters.
narrative_ontology:constraint_beneficiary(spv_variations, forecast_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE DISRUPTED CITIZEN (MOUNTAIN)
% Extreme cold and snow are unchangeable physical limits of the winter environment.
constraint_indexing:constraint_classification(spv_variations, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SUBSEASONAL FORECASTER (ROPE)
% Stratospheric clusters (P1-P5) serve as coordination tools to predict
% transition periods in weeks 3 and 4. The model is a Rope.
constraint_indexing:constraint_classification(spv_variations, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The underlying dynamics are an irreducible physical law (Mountain),
% regardless of the human models built to interpret them. The low extraction
% and suppression scores confirm this objective classification.
constraint_indexing:constraint_classification(spv_variations, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spv_variations_tests).

test(perspectival_gap) :-
    % Verify the vortex is a Mountain for the subject but a Rope for forecasters.
    constraint_indexing:constraint_classification(spv_variations, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spv_variations, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(spv_variations, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(spv_variations, E),
    E =< 0.15. % Validates the Mountain/Rope status for natural laws and pure coordination.

:- end_tests(spv_variations_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This story demonstrates a key perspectival gap. The Stratospheric Polar Vortex
 * is a physical phenomenon, correctly classified as a 'Mountain' from the
 * powerless (citizen) and analytical (scientist) perspectives. Its metrics
 * (extraction=0.02, suppression=0.10) are well within the Mountain thresholds.
 * However, the human-created *model* for predicting its behavior (the P-cluster
 * system) functions as a 'Rope' for the institutional agents (forecasters) who
 * use it. This model is a pure coordination tool that allows for better planning
 * and resource allocation. The story correctly separates the underlying physical
 * constraint from the coordination layer built on top of it. The previous version's
 * Piton and Scaffold classifications were removed as they were inconsistent with
 * the low base metrics (theater_ratio=0.05) and lacked required structural
 * properties (e.g., a sunset clause for Scaffold).
 * * MANDATROPHY ANALYSIS:
 * Not applicable. This is a low-extraction constraint, so Mandatrophy resolution
 * is not required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_enso_strat_link,
    'Does La Niña directly drive P2 variability or is P2 a response to complex tropospheric ENSO influences?',
    'Longer-term reanalysis (beyond 41 years) and high-resolution climate modeling.',
    'Direct Link = Higher predictability of NWUS cold; Complex Response = Lower confidence in ratio shifts.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(spv_variations, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints (base_extractiveness <= 0.46).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The P-cluster system is an information standard for interpreting atmospheric data.
narrative_ontology:coordination_type(spv_variations, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(spv_variations, 0.0).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(spv_variations, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (spv_variations_us_cold)
% ============================================================================
constraint_victim(spv_variations, affected_populations_during_cold_outbreaks).
