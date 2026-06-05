% ============================================================================
% CONSTRAINT STORY: fiscal_dominance_trap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(fiscal_dominance_trap, []).

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
 * * constraint_id: fiscal_dominance_trap
 * human_readable: The Debt-Monetary Bind
 * domain: economic/political
 * * SUMMARY:
 * This constraint represents a state where a central bank is no longer able 
 * to act independently to control inflation because doing so would cause 
 * the government's debt servicing costs to explode, leading to insolvency. 
 * Monetary policy becomes a "Rope" for government survival at the cost of 
 * being a "Snare" for the public's purchasing power through forced inflation.
 * * KEY AGENTS:
 * - Fixed-Income Saver: Subject (Powerless)
 * - Treasury Department: Beneficiary (Institutional)
 * - Monetary Policy Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) because the system liquidates the real value of 
% private savings to subsidize public debt, siphoning the subject's 
% long-term security.
domain_priors:base_extractiveness(fiscal_dominance_trap, 0.87). 
domain_priors:suppression_score(fiscal_dominance_trap, 0.72). 
domain_priors:theater_ratio(fiscal_dominance_trap, 0.85). % High theater: maintaining the "illusion" of central bank independence.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(fiscal_dominance_trap, extractiveness, 0.87).
narrative_ontology:constraint_metric(fiscal_dominance_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fiscal_dominance_trap, theater_ratio, 0.85).

% This is a structural insolvency state, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(fiscal_dominance_trap). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: their savings lose value as inflation is 
% permitted to rise, yet the "safe" assets they hold are the very 
% instruments being used to sustain the trap.
constraint_indexing:constraint_classification(fiscal_dominance_trap, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Treasury views this as a Rope—the essential coordination of 
% monetary and fiscal policy required to avoid a catastrophic 
% sovereign default and systemic collapse.
constraint_indexing:constraint_classification(fiscal_dominance_trap, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: "Independent Monetary 
% Policy" is an inertial spike of institutional rhetoric masking 
% the reality of fiscal subjugation.
constraint_indexing:constraint_classification(fiscal_dominance_trap, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.87) and suppression (0.72) trigger the hybrid 
% Tangled Rope signature at the historical scale.
constraint_indexing:constraint_classification(fiscal_dominance_trap, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(fiscal_dominota_trap, E), E >= 0.50,
    domain_priors:suppression_score(fiscal_dominance_trap, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_dominance_trap_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless saver vs Rope for the institutional treasury.
    constraint_indexing:constraint_classification(fiscal_dominance_trap, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_dominance_trap, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater (0.85) results in Piton detection by systems auditors.
    constraint_indexing:constraint_classification(fiscal_dominance_trap, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.87) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(fiscal_dominance_trap, E),

    E > 0.70.

:- end_tests(fiscal_dominance_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * preservation of the institutional status quo siphons the life-savings 
 * of the subject population.
 * 
 * * PERSPECTIVAL GAP:
 * The Fixed-Income Saver feels a Snare because they pay for government 
 * solvency through a "hidden tax" of inflation. The Treasury sees a Rope 
 * because without this coordination, the government would lose its 
 * primary tool for managing sovereign liabilities and maintaining social 
 * coordination at scale.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. This identifies 
 * that the "coordination" is now purely performative (Theater 0.85); the 
 * system is an inert spike of logic siphoning the public's real wealth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_inflation_threshold,
    'At what level of inflation does the Snare trigger a revolution (Snare vs Mountain)?',
    'Tracking the historical correlation between real interest rates and social unrest.',
    'If order persists: Mountain of Social Compliance. If order breaks: Snare of Miscalculation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(fiscal_dominance_trap, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
