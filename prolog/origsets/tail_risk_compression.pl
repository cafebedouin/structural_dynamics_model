% ============================================================================
% CONSTRAINT STORY: tail_risk_compression
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(tail_risk_compression, []).

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
 * * constraint_id: tail_risk_compression
 * human_readable: The Volatility Suppression Trap
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where systemic risks are artificially suppressed to maintain 
 * short-term stability, effectively "compressing" volatility into a hidden 
 * but catastrophic tail-end event. It functions as a Rope for current 
 * managers but a massive, deferred Snare for those present when the 
 * compression inevitably fails.
 * * KEY AGENTS:
 * - Market Participant: Subject (Powerless)
 * - Central Risk Authority: Beneficiary (Institutional)
 * - Actuarial Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) because the suppression siphons the system's ability 
% to self-correct, charging a "stability fee" that pays out in total collapse.
domain_priors:base_extractiveness(tail_risk_compression, 0.88).
domain_priors:suppression_score(tail_risk_compression, 0.72).
domain_priors:theater_ratio(tail_risk_compression, 0.65).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(tail_risk_compression, extractiveness, 0.88).
narrative_ontology:constraint_metric(tail_risk_compression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tail_risk_compression, theater_ratio, 0.65).

% This is an inherent property of time-lagged risk systems, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(tail_risk_compression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the individual, the compression is a snare: they are forced into a system 
% that masks its own impending destruction.
constraint_indexing:constraint_classification(tail_risk_compression, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The authority views compression as a vital Rope—a coordination mechanism to 
% prevent localized panics and ensure smooth institutional functioning.
constraint_indexing:constraint_classification(tail_risk_compression, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature of providing current coordination while 
% engineering a future extraction event.
constraint_indexing:constraint_classification(tail_risk_compression, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(tail_risk_compression, E), E >= 0.50,
    domain_priors:suppression_score(tail_risk_compression, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tail_risk_compression_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(tail_risk_compression, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tail_risk_compression, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(tail_risk_compression, E),

    (E =< 0.05 -> true ; E >= 0.46).

:- end_tests(tail_risk_compression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects the extreme "Mandatrophy" where 
 * coordination exists only as a byproduct of deferred systemic failure.
 * * PERSPECTIVAL GAP:
 * The individual feels a Snare because they pay for a stability they won't 
 * benefit from; the institution sees a Rope because the immediate lack of 
 * volatility allows for continued coordination.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Tangled Rope classification. This recognizes that the 
 * system is still performing a coordination function (market order), even 
 * if that function is highly predatory in its long-term extraction profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_tail_density,
    'Is the compression a result of regulatory intent (Snare) or a byproduct of computational efficiency (Mountain)?',
    'Simulating system response to extreme out-of-distribution shocks.',
    'If intent-driven: Snare. If byproduct: Mountain of technical debt.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(tail_risk_compression, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
