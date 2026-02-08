% ============================================================================
% CONSTRAINT STORY: SPV_VARIATIONS_US_COLD
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
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
    narrative_ontology:constraint_metric/3,
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
 * This constraint maps the two primary stratospheric variations (P2 and P3)
 * that lead to extreme cold-air outbreaks (CAOs) in the continental US
 * despite overall winter warming trends[cite: 10, 12].
 * * KEY AGENTS:
 * - The US Resident: Subject (Powerless against extreme cold/snow [cite: 22, 654]).
 * - Forecast Systems: Beneficiary (Institutional coordination/S2S forecasting [cite: 769]).
 * - Atmospheric Scientists: Auditor (Analytical/K-means clustering [cite: 9, 776]).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is low (0.02) as this is a natural law (Mountain) based on
% stratospheric dynamics rather than human coordination[cite: 12, 654].
domain_priors:base_extractiveness(spv_variations, 0.02).
domain_priors:suppression_score(spv_variations, 0.10).
domain_priors:theater_ratio(spv_variations, 0.05).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(spv_variations, extractiveness, 0.02).
narrative_ontology:constraint_metric(spv_variations, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(spv_variations, theater_ratio, 0.05).

% Binary flags
% No sunset clause as these physical variations are persistent features[cite: 22, 654].
domain_priors:requires_active_enforcement(spv_variations). % Active monitoring required for forecasting[cite: 769].

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISRUPTED CITIZEN (MOUNTAIN)
% Extreme cold and snow are unchangeable physical limits of the winter environment[cite: 155, 161].
constraint_indexing:constraint_classification(spv_variations, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SUBSEASONAL FORECASTER (ROPE)
% Stratospheric clusters (P1-P5) serve as coordination tools to predict
% transition periods in weeks 3 and 4[cite: 769, 770].
constraint_indexing:constraint_classification(spv_variations, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Ancient physical dynamics (Rossby waves) that remain functionally valid but are
% increasingly "theatrical" as climate warming shifts their impact[cite: 16, 18, 524].
constraint_indexing:constraint_classification(spv_variations, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(spv_variations, TR), TR > 0.04.

% PERSPECTIVE 4: THE CLIMATE MODELLER (SCAFFOLD)
% The current "P-cluster" system acts as a support for understanding
% immediate weather, while the climate shifts toward more P5 days[cite: 522, 588].
constraint_indexing:constraint_classification(spv_variations, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spv_variations_tests).

test(perspectival_gap) :-
    % Verify the vortex is a Mountain for the subject but a Rope for forecasters[cite: 769].
    constraint_indexing:constraint_classification(spv_variations, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spv_variations, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(spv_variations, E),

    E =< 0.05. % Validates the Mountain status for natural laws[cite: 654].

:- end_tests(spv_variations_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The SPV variations are classified as a 'Mountain' at the individual level
 * because weather is an irreducible limit[cite: 155, 654]. However, for
 * institutions (NOAA, forecasters), it is a 'Rope'—a coordination system
 * derived from K-means clustering that allows for better planning and
 * resource allocation[cite: 769, 801].
 * * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * While warming trends are overall increasing, extreme cold events
 * (P2/P3) are not diminishing in frequency for the central-eastern US
 *[cite: 10, 23]. The 'Piton' classification addresses the inertial
 * maintenance of cold weather patterns in an otherwise warming system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_enso_strat_link,
    'Does La Niña directly drive P2 variability or is P2 a response to complex tropospheric ENSO influences? [cite: 751]',
    'Longer-term reanalysis (beyond 41 years) and high-resolution climate modeling[cite: 651, 758].',
    'Direct Link = Higher predictability of NWUS cold; Complex Response = Lower confidence in ratio shifts[cite: 524, 751].',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(spv_variations, 1980, 2021). % Study period[cite: 780].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
