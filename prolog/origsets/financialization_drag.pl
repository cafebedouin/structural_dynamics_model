% ============================================================================
% CONSTRAINT STORY: financialization_drag
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(financialization_drag, []).

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
 * * constraint_id: financial_drag
 * human_readable: The Financialization Gravity Well
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where the primary mechanism for resource allocation shifts from 
 * real-world production to financial engineering. Over time, the "Rope" of 
 * capital coordination becomes a parasitic "Snare" as the financial sector 
 * siphons the surplus of productive industries to service its own internal 
 * complexity. This creates a "drag" on innovation, where the smartest minds 
 * are incentivized to optimize spreadsheets rather than physics.
 * * KEY AGENTS:
 * - Industrial R&D Lead: Subject (Powerless)
 * - Asset Management Firm: Beneficiary (Institutional)
 * - Macro-Economic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the "drag" siphons the species' real 
% productive potential into zero-sum arbitrage.
domain_priors:base_extractiveness(financial_drag, 0.84). 
domain_priors:suppression_score(financial_drag, 0.72). 
domain_priors:theater_ratio(financial_drag, 0.78). % High theater: The focus on "quarterly earnings" over long-term utility.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(financial_drag, extractiveness, 0.84).
narrative_ontology:constraint_metric(financial_drag, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(financial_drag, theater_ratio, 0.78).

% This is a structural property of late-stage markets, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(financial_drag). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the R&D lead, financialization is a snare: they cannot fund long-term 
% breakthroughs because the "cost of capital" is tethered to short-term returns.
constraint_indexing:constraint_classification(financial_drag, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views financialization as a vital Rope—the ultimate tool 
% for global capital coordination and "efficient" market clearing.
constraint_indexing:constraint_classification(financial_drag, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.78) > 0.70 triggers Piton: the "market signals" have 
% atrophied; the system is an inert spike of logic maintained by performative accounting.
constraint_indexing:constraint_classification(financial_drag, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of coordination intent (Ropes) 
% masking deep, structural extraction of real-world value (Snare).
constraint_indexing:constraint_classification(financial_drag, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(financial_drag, E), E >= 0.50,
    domain_priors:suppression_score(financial_drag, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_drag_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(financial_drag, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_drag, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_detection) :-
    % Ensure high theater ratio (0.78) triggers Piton classification.
    constraint_indexing:constraint_classification(financial_drag, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(financial_drag, E),

    E > 0.70.

:- end_tests(financial_drag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * financial sector's coordination function has been subsumed by rent-extraction.
 * 
 * * PERSPECTIVAL GAP:
 * The R&D Lead feels a Snare because their productive agency is limited 
 * by a "hurdle rate" that only zero-sum financial products can meet. The 
 * Asset Management Firm sees a Rope because they are coordinating trillions 
 * of dollars with unmatched liquidity and speed.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an 
 * analytical observer, the "coordination" is no longer functional relative 
 * to the species' progress (Theater 0.78); the system is an inert spike 
 * siphoning 0.84 of the potential industrial surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_real_world_decoupling,
    'Can a "real-world" economy decouple from financialized capital (Snare vs Mountain)?',
    'Tracking the delta between industrial R&D output and stock market performance.',
    'If decoupling succeeds: Snare of policy. If industries fail without financial propping: Mountain of Debt.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(financial_drag, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
