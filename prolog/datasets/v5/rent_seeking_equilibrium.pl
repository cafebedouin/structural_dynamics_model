% ============================================================================
% CONSTRAINT STORY: rent_seeking_equilibrium
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(rent_seeking_equilibrium, []).

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
 * * constraint_id: rent_seeking_equilibrium
 * human_readable: The Toll-Bridge Stagnation
 * domain: economic/political
 * * SUMMARY:
 * A scenario where economic agents invest more resources in capturing 
 * existing wealth through political or legal influence (rent-seeking) than 
 * in creating new value. This "Rope" of institutional stability coordinates 
 * elite interests but acts as a "Snare" for the productive economy, 
 * liquidating innovation and labor surplus into static barriers to entry.
 * * KEY AGENTS:
 * - Aspiring Entrepreneur: Subject (Powerless)
 * - Incumbent Lobbyist: Beneficiary (Institutional)
 * - Institutional Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the equilibrium siphons the surplus of 
% productive labor into "rent" payments for access to artificially 
% restricted markets.
domain_priors:base_extractiveness(rent_seeking_equilibrium, 0.84). 
domain_priors:suppression_score(rent_seeking_equilibrium, 0.76). % High suppression of non-rent-seeking alternatives.
domain_priors:theater_ratio(rent_seeking_equilibrium, 0.88).    % Piton threshold (> 0.70) triggered by "regulatory compliance" branding.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(rent_seeking_equilibrium, extractiveness, 0.84).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, theater_ratio, 0.88).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the entrepreneur, the equilibrium is a snare: the "cost of entry" is 
% a political fee rather than a technical challenge, trapping them in 
% low-margin compliance.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The incumbent views the regulation as a Rope—the essential coordination 
% substrate that prevents "market chaos" and ensures long-term 
% institutional stability.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 triggers Piton: the "market safety" 
% regulations are an inertial spike of logic masking pure wealth transfer.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) masking as coordination (Rope).
constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(rent_seeking_equilibrium, E), E >= 0.50,
    domain_priors:suppression_score(rent_seeking_equilibrium, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rent_seeking_equilibrium_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional incumbent.
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(rent_seeking_equilibrium, E),

    E > 0.70.

:- end_tests(rent_seeking_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of market regulation is effectively consumed 
 * by the parasitic liquidation of the subject's competitive agency.
 
 * * PERSPECTIVAL GAP:
 * The Aspiring Entrepreneur feels a Snare because their success depends 
 * on political connections rather than merit. The Lobbyist sees a Rope 
 * because the barriers to entry prevent the "friction" of price wars 
 * and coordinate stable, high-margin industry behavior.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "coordination" is no longer functional relative to 
 * wealth creation (Theater 0.88); it is an inert spike siphoning 
 * 0.84 of the productive surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_regulatory_capture,
    'Can the system distinguish "rent-seeking" from "essential safety" (Snare vs Mountain)?',
    'Tracking the delta between compliance costs and actual safety outcomes over a 20-year horizon.',
    'If delta is high: Snare of policy. If delta is zero: Mountain of Irreducible Risk.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(rent_seeking_equilibrium, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
