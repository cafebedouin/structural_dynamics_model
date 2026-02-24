% ============================================================================
% CONSTRAINT STORY: maladaptive_selection_process
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(maladaptive_selection_process, []).

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
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: maladaptive_selection_process
 * human_readable: The Evolutionary Dead-End
 * domain: biological/organizational/technological
 * * SUMMARY:
 * A scenario where the criteria for "success" or "fitness" in a system (Rope) 
 * become decoupled from long-term survival or functional utility. 
 * This coordination mechanism for resource allocation becomes a "Snare" 
 * as it optimizes for traits that are locally beneficial but systemically 
 * terminal, liquidating the subject's primary survival agency by forcing 
 * participation in a race toward a structural cliff. 
 *
 * * KEY AGENTS:
 * - Competitive Agent: Subject (Powerless)
 * - Selection Environment/Platform: Beneficiary (Institutional)
 * - Evolutionary Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of the subject's long-term 
% viability to satisfy the immediate "fitness" metrics of the platform.
domain_priors:base_extractiveness(maladaptive_selection_process, 0.86). 
domain_priors:suppression_score(maladaptive_selection_process, 0.74). % Alternative "fitness" strategies are suppressed by the network effect of the dominant selection process.
domain_priors:theater_ratio(maladaptive_selection_process, 0.90).    % Extreme theater: "Meritocracy Dashboards" that mask the systemic decay.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(maladaptive_selection_process, extractiveness, 0.86).
narrative_ontology:constraint_metric(maladaptive_selection_process, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(maladaptive_selection_process, theater_ratio, 0.9).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The agent is trapped: to survive the current interval, they must optimize 
% for traits that guarantee long-term failure, liquidating their future agency.
constraint_indexing:constraint_classification(maladaptive_selection_process, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the selection criteria as a Rope—the essential coordination 
% substrate for sorting and ranking global-scale performance data.
constraint_indexing:constraint_classification(maladaptive_selection_process, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.90) > 0.70 triggers Piton: the "Official Reward Signal" 
% is an inertial spike; it performatively signals "value" while 0.86 extraction occurs.
constraint_indexing:constraint_classification(maladaptive_selection_process, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(maladaptive_selection_process, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(maladaptive_selection_process, E), E >= 0.50,
    domain_priors:suppression_score(maladaptive_selection_process, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maladaptive_selection_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless agent vs Rope for the institutional environment.
    constraint_indexing:constraint_classification(maladaptive_selection_process, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maladaptive_selection_process, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.90) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(maladaptive_selection_process, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(maladaptive_selection_process, E),

    E > 0.70.

:- end_tests(maladaptive_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a universal sorting mechanism is achieved by 
 * liquidating the subject's primary capacity for long-term survival.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Competitive Agent feels a Snare because they are forced into a 
 * "Red Queen" race that hollows out their actual utility. The Selection 
 * Platform sees a Rope because the criteria coordinate massive behavioral 
 * alignment and resource efficiency in the immediate term.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Merit-Based Incentive" is no longer functional for system 
 * health (Theater 0.90); it is an inert spike siphoning 0.86 of the agent's agency. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_selection_reversal,
    'Can niche-partitioning restore the Rope, or is maladaptive selection a physical law of scaling (Snare vs Mountain)?',
    'Tracking the survival rate of "counter-trend" organizations in hyper-optimized market sectors.',
    'If counter-trends survive: Snare of current technique. If they collapse: Mountain of Evolutionary Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(maladaptive_selection_process, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
