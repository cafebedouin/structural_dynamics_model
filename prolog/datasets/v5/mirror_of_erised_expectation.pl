% ============================================================================
% CONSTRAINT STORY: MIRROR_OF_ERISED_EXPECTATION
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_erised_expectation, []).

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
 * * constraint_id: erised_expectation
 * human_readable: The Erised Career/Stability Mirror
 * domain: psychological/economic
 * * SUMMARY:
 * This constraint represents the internalised Millennial expectation of a
 * "magical" meritocracy where ordinary people defeat evil and achieve
 * stability. It acts as a beautiful but deceptive reflection of
 * a world that was "too good to be true".
 * * KEY AGENTS:
 * - The Graying Millennial: Subject (Powerless/Entranced)
 * - The Gig Economy/Market: Beneficiary (Institutional/Extractive)
 * - The Disillusioned Zoomer: Observer (Analytical/Jaded)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.65) because the hope of "meritocratic magic" keeps
% the subject participating in declining systems.
domain_priors:base_extractiveness(erised_expectation, 0.65).
domain_priors:suppression_score(erised_expectation, 0.70).   % Difficulty of seeing reality through the "mirror"
domain_priors:theater_ratio(erised_expectation, 0.85).       % High internal "theatrical" performance of nostalgia

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(erised_expectation, extractiveness, 0.65).
narrative_ontology:constraint_metric(erised_expectation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(erised_expectation, theater_ratio, 0.85).

% Binary flags
domain_priors:requires_active_enforcement(erised_expectation). % Self-enforcement of nostalgic worldviews

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENTRANCED SUBJECT (SNARE)
% The subject "wastes away" before the mirror, unable to distinguish the possible from the real.
constraint_indexing:constraint_classification(erised_expectation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE MODERN MARKET (TANGLED ROPE)
% Uses the "optimistic vision" to maintain worker faith while extraction remains high.
constraint_indexing:constraint_classification(erised_expectation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE EXTERNAL CRITIC / ZOOMER (PITON)
% Sees a "naive" politics and worldview that is "dated" and non-functional.
constraint_indexing:constraint_classification(erised_expectation, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))) :-
    domain_priors:theater_ratio(erised_expectation, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(erised_expectation_tests).

test(psychological_trap) :-
    % Verify it is perceived as a Snare (trap) for the powerless subject.
    constraint_indexing:constraint_classification(erised_expectation, snare, context(agent_power(powerless), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(erised_expectation, E),

    E > 0.46. % Confirms it meets the Snare/Tangled high-extraction threshold.

:- end_tests(erised_expectation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Mirror of Erised" functions as a Snare because it reflects a desire for
 * moral simplicity and success that "drives men mad" when it doesn't align
 * with the "rare combination" of peace and affluence required for liberalism.
 * Millennials are "entranced" by the memory of 1990s optimism.
 * * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction (0.65) is resolved through the 'Piton' classification
 * for observers. It identifies that the constraint is no longer a functional
 * 'Rope' (coordination), but an inertial leftover of a "hopeful vision"
 * that no longer lands in today's climate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_mirror_shatter,
    'At what level of economic decline does the Mirror of Erised shatter for Millennials?',
    'Longitudinal study of "Harry Potter adult" identification vs. cost-of-living index.',
    'Shattering = Mass conversion to radical "revolution" (Zoomer-left/right).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(erised_expectation, 1997, 2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
