% ============================================================================
% CONSTRAINT STORY: liquidity_illusion
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(liquidity_illusion, []).

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
 * * constraint_id: liquidity_illusion
 * human_readable: The Exit Door Mirage
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where a financial or digital market maintains the appearance of 
 * high liquidity (easy entry/exit) during normal conditions. This "Rope" of 
 * coordination encourages mass participation, but the liquidity is a 
 * "Piton" or "Snare" because it vanishes the moment a systemic shock occurs, 
 * leaving all participants trapped in a crowded room with a shrinking exit.
 * * KEY AGENTS:
 * - Retail Trader: Subject (Powerless)
 * - Market Maker / Exchange: Beneficiary (Institutional)
 * - Financial Stability Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) as the illusion allows the institution to capture 
% the "liquidity premium" while externalizing the risk of a "flash crash" 
% or bank run onto the subject.
domain_priors:base_extractiveness(liquidity_illusion, 0.85). 
domain_priors:suppression_score(liquidity_illusion, 0.68). 
domain_priors:theater_ratio(liquidity_illusion, 0.92). % Extreme theater: High-frequency bids that disappear on stress.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(liquidity_illusion, extractiveness, 0.85).
narrative_ontology:constraint_metric(liquidity_illusion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(liquidity_illusion, theater_ratio, 0.92).

% This is a structural property of modern automated markets.
% narrative_ontology:has_sunset_clause(liquidity_illusion). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: the "instant exit" they were promised is 
% unavailable precisely when it is needed most.
constraint_indexing:constraint_classification(liquidity_illusion, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the deep order book (even if illusory) as a Rope— 
% the only way to coordinate mass investment and capital efficiency.
constraint_indexing:constraint_classification(liquidity_illusion, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "liquidity" is a 
% non-functional, performative artifact that does not withstand pressure.
constraint_indexing:constraint_classification(liquidity_illusion, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of coordination intent (Ropes) 
% entangled with predatory risk-concealment (Snare).
constraint_indexing:constraint_classification(liquidity_illusion, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(liquidity_illusion, E), E >= 0.50,
    domain_priors:suppression_score(liquidity_illusion, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liquidity_illusion_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(liquidity_illusion, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liquidity_illusion, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(liquidity_illusion, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.85) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(liquidity_illusion, E),

    E > 0.70.

:- end_tests(liquidity_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the 
 * system's "coordination" is a performative loop that fails under stress.
 * 
 * * PERSPECTIVAL GAP:
 * The Retail Trader feels a Snare because they are lulled into 
 * risk-taking by the illusion of a "safety exit." The Market Maker 
 * sees a Rope because the illusion is what draws in the volume 
 * necessary for coordination.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an 
 * analytical observer, the "liquidity" is no longer functional relative 
 * to its stated purpose (Theater 0.92); the system is an inert spike 
 * siphoning 0.85 of the risk-adjusted surplus from participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_exit_transparency,
    'Can "real-time" auditing reveal the mirage (Snare) or is the uncertainty fundamental (Mountain)?',
    'Tracking the delta between advertised "limit orders" and "order cancellations" during volatility.',
    'If reveals: Snare of theater. If cancels remain opaque: Mountain of Information Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(liquidity_illusion, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
