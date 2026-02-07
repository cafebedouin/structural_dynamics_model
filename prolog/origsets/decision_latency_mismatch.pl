% ============================================================================
% CONSTRAINT STORY: decision_latency_mismatch
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(decision_latency_mismatch, []).

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
 * * constraint_id: decision_latency_mismatch
 * human_readable: High-Frequency Regulatory Lag
 * domain: technological/economic
 * * SUMMARY:
 * This constraint represents the structural gap between algorithmic execution 
 * speeds (nanoseconds) and human/institutional regulatory oversight (months/years). 
 * This lag functions as a predatory extraction mechanism for high-frequency 
 * actors while appearing as a natural "law of physics" to retail participants.
 * * KEY AGENTS:
 * - Retail Trader: Subject (Powerless)
 * - Arbitrage Fund: Beneficiary (Institutional)
 * - SEC/Regulator: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.68) because the lag allows for front-running and rent extraction.
domain_priors:base_extractiveness(decision_latency_mismatch, 0.68). 
domain_priors:suppression_score(decision_latency_mismatch, 0.55).
domain_priors:theater_ratio(decision_latency_mismatch, 0.30).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(decision_latency_mismatch, extractiveness, 0.68).
narrative_ontology:constraint_metric(decision_latency_mismatch, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(decision_latency_mismatch, theater_ratio, 0.3).

% This is not a scaffold; it is a structural byproduct of mismatched systems.
% narrative_ontology:has_sunset_clause(decision_latency_mismatch). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The retail trader is trapped by the lag, losing value to "invisible" slippage.
constraint_indexing:constraint_classification(decision_latency_mismatch, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutional actors view the latency as a necessary "coordination" buffer 
% that provides market liquidity, even if they extract profit from it.
constraint_indexing:constraint_classification(decision_latency_mismatch, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The auditor sees both the liquidity provision (Rope) and the rent-seeking (Snare).
% Since Extraction (0.68) > 0.50 and Suppression (0.55) > 0.40, this triggers Tangled.
constraint_indexing:constraint_classification(decision_latency_mismatch, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(decision_latency_mismatch, E), E >= 0.50,
    domain_priors:suppression_score(decision_latency_mismatch, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decision_latency_mismatch_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(decision_latency_mismatch, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decision_latency_mismatch, rope, 
        context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(decision_latency_mismatch, E),

    (E =< 0.05 ; E >= 0.46).

:- end_tests(decision_latency_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 'decision_latency_mismatch' is assigned an extractiveness of 0.68 because it 
 * creates a consistent transfer of wealth from slow-moving nodes to fast-moving nodes 
 * without proportional value creation. 
 * * PERSPECTIVAL GAP:
 * To the 'powerless', the lag is a Snare—it is a trap where they are 
 * front-run by algorithms. To the 'institutional' actor, it is a Rope—a functional 
 * requirement of the "plumbing" of global finance that ensures they remain mobile.
 * * [RESOLVED MANDATROPHY]:
 * By classifying the observer perspective as a 'tangled_rope', we acknowledge that 
 * the latency does provide a coordination function (market stability/time to audit) 
 * while simultaneously facilitating extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required because extraction (0.68) > 0.46
omega_variable(
    omega_decision_latency,
    'Is the latency a technological limitation (Mountain) or a policy choice (Snare)?',
    'Implementation of sub-microsecond regulatory hardware timestamps.',
    'If hardware-resolved: Transition to Mountain. If policy-maintained: Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py
narrative_ontology:interval(decision_latency_mismatch, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
