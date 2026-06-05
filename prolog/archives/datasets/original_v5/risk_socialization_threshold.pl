% ============================================================================
% CONSTRAINT STORY: risk_socialization_threshold
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(risk_socialization_threshold, []).

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
 * * constraint_id: risk_socialization_threshold
 * human_readable: The Asymmetric Liability Trap
 * domain: economic/political
 * * SUMMARY:
 * A scenario where a systemic entity (e.g., a "too big to fail" bank or a 
 * state-backed utility) privatizes profits during stability but socializes 
 * losses during a crisis. The threshold represents the point where the 
 * entity's private risk is transferred to the public ledger. It functions 
 * as a Rope for short-term economic coordination but becomes a massive 
 * Snare for taxpayers who bear the unpriced downside.
 * * KEY AGENTS:
 * - Average Taxpayer: Subject (Powerless)
 * - Systemic Institution: Beneficiary (Institutional)
 * - Forensic Macroeconomist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) because the systemic "safety net" siphons public 
% wealth to protect private equity, liquidating the subject's future surplus.
domain_priors:base_extractiveness(risk_socialization_threshold, 0.88). 
domain_priors:suppression_score(risk_socialization_threshold, 0.73). 
domain_priors:theater_ratio(risk_socialization_threshold, 0.85). % High theater: "Market stability" used as moral justification.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(risk_socialization_threshold, extractiveness, 0.88).
narrative_ontology:constraint_metric(risk_socialization_threshold, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(risk_socialization_threshold, theater_ratio, 0.85).

% This is a structural feature of state-backed capitalism.
% narrative_ontology:has_sunset_clause(risk_socialization_threshold). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the taxpayer, the threshold is a snare: they receive no upside 
% but are legally bound to cover the systemic downside.
constraint_indexing:constraint_classification(risk_socialization_threshold, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the threshold as a Rope—the only way to coordinate 
% capital at a scale that ensures systemic continuity.
constraint_indexing:constraint_classification(risk_socialization_threshold, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: the "risk management" is 
% a performative facade for a pure wealth transfer.
constraint_indexing:constraint_classification(risk_socialization_threshold, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of coordination intent (Ropes) 
% entangled with extreme, systemic extraction (Snare).
constraint_indexing:constraint_classification(risk_socialization_threshold, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(risk_socialization_threshold, E), E >= 0.50,
    domain_priors:suppression_score(risk_socialization_threshold, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(risk_socialization_threshold_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(risk_socialization_threshold, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(risk_socialization_threshold, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection by systems auditors.
    constraint_indexing:constraint_classification(risk_socialization_threshold, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.88) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(risk_socialization_threshold, E),

    E > 0.70.

:- end_tests(risk_socialization_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of systemic stability is essentially a pretext 
 * for predatory resource siphoning.
 * 
 * * PERSPECTIVAL GAP:
 * The Average Taxpayer feels a Snare because they pay for a "safety net" 
 * they do not control and cannot exit. The Systemic Institution sees a 
 * Rope because it allows for high-risk coordination that would otherwise 
 * be impossible under true market conditions.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an 
 * analytical observer, the "stability" is no longer functional relative 
 * to the public good (Theater 0.85); the system is an inert spike of 
 * logic siphoning 0.88 of the risk-adjusted public surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_threshold_volatility,
    'Can the threshold be mathematically fixed, or is it a political variable (Snare vs Mountain)?',
    'Tracking the delta between institutional "reserve" levels and actual bailout amounts.',
    'If fixed: Mountain of Math. If moving: Snare of Political Arbitrage.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(risk_socialization_threshold, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
