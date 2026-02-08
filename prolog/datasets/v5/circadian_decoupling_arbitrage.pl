% ============================================================================
% CONSTRAINT STORY: circadian_decoupling_arbitrage
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_circadian_arbitrage, []).

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
 * * constraint_id: circadian_decoupling_arbitrage
 * human_readable: The Circadian Decoupling Arbitrage
 * domain: bio_industrial
 * * SUMMARY:
 * This constraint describes the structural bypass of the biological 24-hour 
 * cycle. By using high-intensity blue light, melatonin suppressors, and 
 * modafinil, the subject enters a state of "perpetual noon." While 
 * profitable (Rope/Scaffold), it creates an extraction of health and 
 * social connectivity (Snare) that is invisible at the immediate time scale.
 * * KEY AGENTS:
 * - The Shift-Worker: Subject (Powerless). Trading biological health for wage premiums.
 * - The Global Logistics Hub: Beneficiary (Institutional). Requires 24/7 uptime.
 * - The Chronobiologist: Auditor (Analytical). Monitors the metabolic debt.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.62) is high because the system 
% externalizes the long-term metabolic cost (diabetes, cardiac stress) 
% onto the individual while capturing 100% of the additional "awake" utility.
domain_priors:base_extractiveness(circadian_decoupling_arbitrage, 0.62). 
domain_priors:suppression_score(circadian_decoupling_arbitrage, 0.70).   % High: Exit requires leaving high-wage industrial roles.
domain_priors:theater_ratio(circadian_decoupling_arbitrage, 0.15).      % Low: This is a highly functional, non-theatrical physical intervention.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, extractiveness, 0.62).
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, theater_ratio, 0.15).
domain_priors:requires_active_enforcement(circadian_decoupling_arbitrage). % Enforcement via environmental lighting and scheduling.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SHIFT-WORKER (SNARE)
% For the worker, the system is a Snare. The immediate paycheck is the bait, 
% while the biographical cost is a trap they cannot easily exit.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE LOGISTICS HUB (ROPE)
% To the institution, this is a Rope. It coordinates labor across global 
% time zones, ensuring that "time" is never a bottleneck for capital.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE CHRONOBIOLOGIST (MOUNTAIN)
% Viewed analytically, the 24-hour cycle is an immutable Mountain. 
% Decoupling is not a "fix," but a temporary and dangerous loan from nature.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(universal))).

% PERSPECTIVE 4: THE PHARMA-TECH CONTRACTOR (SCAFFOLD)
% The intervention is sold as a Scaffold—a temporary biological "bridge" 
% until full automation renders human shift-work obsolete.
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(circadian_decoupling_arbitrage).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(circadian_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, rope, context(agent_power(institutional), _, _, _)).

test(mountain_verification) :-
    % Verify that the analytical perspective recognizes the biological limit as a Mountain.
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(circadian_decoupling_arbitrage, E),

    E >= 0.46. 

:- end_tests(circadian_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Circadian decoupling is a "Biological Loan" with a high interest rate. 
 * The system classifies it as a Scaffold because it is inherently unstable 
 * over long time horizons. The Perspectival Gap is driven by the Time Horizon: 
 * the Institution sees "Immediate/Generational" efficiency, while the 
 * Subject eventually feels the "Biographical" extraction of health.
 *
 * MANDATROPHY ANALYSIS:
 * This is a "Predatory Scaffold." It provides the support needed to work 
 * (the coordination), but the extraction (metabolic debt) is so high that 
 * it eventually collapses the Subject's ability to participate in the 
 * system at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_metabolic_reversibility,
    'Is the metabolic debt incurred during decoupling permanently reversible?',
    'Longitudinal studies of retired night-shift workers vs. control groups.',
    'If irreversible, the Scaffold is a Snare; if reversible, it remains a Scaffold.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Mandatory for Scaffold classification logic
narrative_ontology:has_sunset_clause(circadian_decoupling_arbitrage). 
narrative_ontology:interval(circadian_decoupling_arbitrage, 0, 24). % Hours of decoupling.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
