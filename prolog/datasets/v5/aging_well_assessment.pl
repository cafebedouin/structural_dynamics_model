% ============================================================================
% CONSTRAINT STORY: AGING_WELL_ASSESSMENT
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_aging_well, []).

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
 * * constraint_id: aging_longevity_tests
 * human_readable: The Functional Aging Trajectory Suite
 * domain: health/biological
 * * SUMMARY:
 * A set of physical proxies (Sitting-Rising, Walking Speed, Grip, Balance)
 * used to predict mortality and functional independence. These tests represent
 * the 'biological bank account' of an individual.
 * * KEY AGENTS:
 * - The Aging Individual: Subject (Powerless vs. Biological Time).
 * - Kinesiologist/Doctor: Beneficiary (Institutional/Providing the 'Map').
 * - The 90-year-old Trainee: Auditor (Analytical/Proving plasticity).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is low (0.04) as these are biological "Mountain" constraints
% of physics and physiology, not human-imposed predatory traps.
domain_priors:base_extractiveness(aging_longevity_tests, 0.04).
domain_priors:suppression_score(aging_longevity_tests, 0.15).
domain_priors:theater_ratio(aging_longevity_tests, 0.05).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(aging_longevity_tests, extractiveness, 0.04).
narrative_ontology:constraint_metric(aging_longevity_tests, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aging_longevity_tests, theater_ratio, 0.05).

% Mandatory keys for classification engine v3.4
% These ensure the system correctly identifies the 'Mountain' status (E <= 0.05).
% Binary flags
domain_priors:requires_active_enforcement(aging_longevity_tests). % Requires regular re-testing/training.

% REQUIRED: Sunset clause for Scaffold classification.
% Rationale: Functional markers like strength serve as a temporary bridge 
% (Scaffold) that eventually sunsets as biological 'Mountain' limits 
% are reached in late senescence.
narrative_ontology:has_sunset_clause(aging_longevity_tests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SEDENTARY ADULT (MOUNTAIN)
% Biological decline appears as an unchangeable, irreducible limit of nature.
constraint_indexing:constraint_classification(aging_longevity_tests, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ACTIVE TRAINEE (ROPE)
% The tests are used to coordinate 'light activity' to make gains in function.
constraint_indexing:constraint_classification(aging_longevity_tests, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MEDICAL INSTITUTION (TANGLED ROPE)
% Provides coordination for longevity while extracting 'health-span' value,
% though base extraction remains low.
constraint_indexing:constraint_classification(aging_longevity_tests, tangled_rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(aging_longevity_tests, E), E < 0.05.

% PERSPECTIVE 4: THE NONAGENARIAN (SCAFFOLD)
% Strength and balance serve as a temporary support structure to delay
% nursing home placement.
constraint_indexing:constraint_classification(aging_longevity_tests, scaffold,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aging_well_tests).

test(biological_reality) :-
    % Verify that for a powerless agent, the constraint is a Mountain (biological fact).
    constraint_indexing:constraint_classification(aging_longevity_tests, mountain, context(agent_power(powerless), _, _, _)).

test(mountain_threshold) :-
    domain_priors:base_extractiveness(aging_longevity_tests, E),
    E =< 0.05. % Ensures classification as an irreducible Mountain.

:- end_tests(aging_well_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.04) is minimal because these tests reflect
 * 'proxies for activities' of daily living. The Perspectival Gap exists
 * between seeing these as a 'death sentence' (Mountain) and seeing them as
 * a 'warning sign' for intervention (Rope).
 * * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * While mortality rates for low scorers are 'four times higher', this
 * is not a 'Snare' because the constraint is biological, not institutional.
 * The system correctly identifies it as a 'Mountain' that can be
 * navigated via the 'Rope' of training.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_grip_causality,
    'Is grip strength a causal factor or merely a proxy for general activity?',
    'Intervention study: Does isolated hand-grip training improve all-cause mortality?',
    'Causal = Specific Training; Proxy = Holistic Lifestyle focus.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(aging_longevity_tests, 0, 10).

% Note: Extraction (0.04) is below the 0.46 threshold; 
% Section 8 (Temporal Measurements) is optional but recommended for tracking 
% age-related drift.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
