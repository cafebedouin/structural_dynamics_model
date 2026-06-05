% ============================================================================
% CONSTRAINT STORY: institutional_trust_decay
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_institutional_trust_decay, []).

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
 * * constraint_id: institutional_trust_decay
 * human_readable: The Legitimacy Void
 * domain: social
 * * SUMMARY:
 * A systemic condition where the shared belief in institutional competence and 
 * integrity evaporates. This transforms social "Ropes" into perceived "Snares," 
 * as coordination becomes indistinguishable from coercion.
 * * KEY AGENTS:
 * - The Dissident: Subject (Powerless) - Views all official data as a predatory trap.
 * - The Press Secretary: Beneficiary (Institutional) - Managing the theatrical "Piton" of authority.
 * - The Sociometrician: Auditor (Analytical) - Quantifying the gap between signal and belief.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(institutional_trust_decay, 0.68). % High: The cost of non-cooperation extracts social capital.
domain_priors:suppression_score(institutional_trust_decay, 0.55).   % High: Lack of trusted alternative institutions.
domain_priors:theater_ratio(institutional_trust_decay, 0.88).       % Extremely High: Indicators of a Piton status.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(institutional_trust_decay, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_trust_decay, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(institutional_trust_decay, theater_ratio, 0.88).

% Binary flags
domain_priors:requires_active_enforcement(institutional_trust_decay).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual feels trapped in a system that no longer yields a ROI on trust.
constraint_indexing:constraint_classification(institutional_trust_decay, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution maintains that the structures are still essential coordination mechanisms.
constraint_indexing:constraint_classification(institutional_trust_decay, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Detection of "Piton" status: high theater_ratio indicates inert structures.
constraint_indexing:constraint_classification(institutional_trust_decay, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(institutional_trust_decay, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_trust_decay_tests).

test(perspectival_gap) :-
    % Verify variance: Snare for the subject, Rope for the institution.
    constraint_indexing:constraint_classification(institutional_trust_decay, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_trust_decay, rope, context(agent_power(institutional), _, _, _)).

test(piton_analysis) :-
    % Verify that the analytical observer detects a Piton due to high theater_ratio.
    constraint_indexing:constraint_classification(institutional_trust_decay, piton, context(agent_power(analytical), _, _, _)).

:- end_tests(institutional_trust_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Base extractiveness (0.68) captures the "trust tax" where transactions 
 * require expensive verification instead of low-cost social capital.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the high Theater Ratio (0.88). The system 
 * identifies that while the institution acts as if it is a "Rope," it is 
 * functionally a "Piton" kept alive by administrative inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
% High extraction (> 0.46) requires omega variable.
omega_variable(
    omega_relegitimation_threshold,
    'Can trust be restored via reform (Scaffold), or is the decay a physical Mountain of entropy?',
    'Historical comparison of reformist vs. revolutionary outcomes in trust-depleted states.',
    'If reform: Transition to Scaffold; If entropy: Descent into Snare-totalitarianism.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(institutional_trust_decay, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
