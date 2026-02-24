% ============================================================================
% CONSTRAINT STORY: PORTUGUESE_PRESIDENTIAL_TERM_LIMITS
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_portugal_presidential_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: portuguese_presidential_term_limits
 * human_readable: Portuguese Constitutional Term Limits (Article 123)
 * domain: political/legal
 * * SUMMARY:
 * Under the Portuguese Constitution, a President cannot serve a third 
 * consecutive term. This creates an absolute "Mountain" 
 * constraint for the 2026 election, as the highly popular incumbent, 
 * Marcelo Rebelo de Sousa, is ineligible to run. 
 * This structural limit forces a total reconfiguration of the political 
 * field, transitioning from a stable incumbency to an open, multi-polar race.
 * * KEY AGENTS:
 * - Marcelo Rebelo de Sousa: Subject (Institutional) - The popular incumbent barred from re-election.
 * - Admiral Gouveia e Melo: Beneficiary (Organized) - Leading polling candidate benefiting from the open field[cite: 12, 14].
 * - Portuguese Electorate: Subject (Powerless) - Forbidden from voting for their most preferred candidate due to legal limits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Mountain status: Extraction is effectively zero as it is a fixed constitutional rule.
domain_priors:base_extractiveness(portuguese_presidential_term_limits, 0.02). 
% Suppression: Absolute. No legal "exit" exists for a third term[cite: 4].
domain_priors:suppression_score(portuguese_presidential_term_limits, 0.98).   
% Theater: Low. The rule is strictly enforced by the Constitutional Court.
domain_priors:theater_ratio(portuguese_presidential_term_limits, 0.05).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, extractiveness, 0.02).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, theater_ratio, 0.05).

% Binary flags
domain_priors:requires_active_enforcement(portuguese_presidential_term_limits).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LEGAL ANALYST (MOUNTAIN)
% Viewed as an irreducible physical-like limit of the democratic cycle.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE DISAPPOINTED VOTER (SNARE)
% What the law calls a "limit," a voter wanting a third term for the incumbent 
% experiences as a "Snare" that extracts their democratic choice.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE STRATEGIC CANDIDATE (ROPE)
% Viewed as a Coordination mechanism (Rope) that ensures healthy turnover.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, rope, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portuguese_presidential_2026_tests).

test(mountain_validation) :-
    domain_priors:base_extractiveness(portuguese_presidential_term_limits, E),

    E =< 0.05,
    constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain, context(agent_power(analytical), _, _, _)).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(portuguese_presidential_term_limits, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portuguese_presidential_term_limits, rope, context(agent_power(organized), _, _, _)).

:- end_tests(portuguese_presidential_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The term limit is a 'Mountain' because it acts as an immutable boundary for 
 * political actors in 2026. However, for a segment of the electorate that 
 * overwhelmingly approves of Rebelo de Sousa, this limit is a 'Snare'—it 
 * suppresses their preferred coordination (re-election) to force a change 
 * they may not desire.
 *
 * [RESOLVED MANDATROPHY]
 * Extraction is low (0.02), avoiding the high-extraction triggers, yet the 
 * Perspectival Gap remains high due to the discrepancy between constitutional 
 * stability and voter preference.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_independent_surge,
    'Will the "Mountain" of term limits lead to a permanent shift toward non-partisan Presidents?',
    'Analyzing the performance of Gouveia e Melo vs party-backed candidates in Jan 2026[cite: 12, 14].',
    'Structural Shift (Rope evolves) vs Party Restoration (Return to status quo)',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portuguese_presidential_term_limits, 1976, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
