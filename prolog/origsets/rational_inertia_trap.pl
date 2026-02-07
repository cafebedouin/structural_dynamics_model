% ============================================================================
% CONSTRAINT STORY: rational_inertia_trap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(rational_inertia_trap, []).

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
 * * constraint_id: rational_inertia_trap
 * human_readable: The Legacy Protocol Lock-in
 * domain: technological/economic
 * * SUMMARY:
 * A scenario where a superior technological or social protocol exists, but 
 * individual agents find it rational to remain in the inferior legacy 
 * system because the local cost of switching exceeds the local immediate 
 * benefit, even though the collective gain would be massive.
 * * KEY AGENTS:
 * - Small Developer: Subject (Powerless)
 * - Platform Incumbent: Beneficiary (Institutional)
 * - Market Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================= */

% High extraction (0.75) as the trap siphons innovation potential and maintenance costs.
domain_priors:base_extractiveness(rational_inertia_trap, 0.75). 
domain_priors:suppression_score(rational_inertia_trap, 0.68).
domain_priors:theater_ratio(rational_inertia_trap, 0.72). % High theater: constant "innovation" talk with zero structural change.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(rational_inertia_trap, extractiveness, 0.75).
narrative_ontology:constraint_metric(rational_inertia_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rational_inertia_trap, theater_ratio, 0.72).

% This is not a scaffold; it is a stable equilibrium trap.
% narrative_ontology:has_sunset_clause(rational_inertia_trap). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The small developer is trapped: switching alone is suicide, staying is slow extraction.
constraint_indexing:constraint_classification(rational_inertia_trap, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The incumbent views the inertia as a Rope—it provides market stability and predictable coordination.
constraint_indexing:constraint_classification(rational_inertia_trap, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.72) > 0.70 triggers Piton: a non-functional inertial spike.
constraint_indexing:constraint_classification(rational_inertia_trap, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(rational_inertia_trap, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.75) and suppression (0.68) trigger the hybrid Tangled Rope signature.
constraint_indexing:constraint_classification(rational_inertia_trap, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(rational_inertia_trap, E), E >= 0.50,
    domain_priors:suppression_score(rational_inertia_trap, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_inertia_trap_tests).

test(perspectival_gap) :-
    % Verify Snare for powerless vs Rope for institutional.
    constraint_indexing:constraint_classification(rational_inertia_trap, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_inertia_trap, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio results in Piton classification.
    constraint_indexing:constraint_classification(rational_inertia_trap, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(rational_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) reflects the 'Mandatrophy' threshold where 
 * coordination is secondary to the extraction of rent from legacy users.
 * * PERSPECTIVAL GAP:
 * The individual experiences a Snare because they cannot exit without 
 * localized ruin; the institution experiences a Rope because they harvest 
 * the "network effect" as a coordination benefit.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Piton and Tangled Rope classifications. The system is 
 * identified as having atrophied beyond coordination, maintained purely 
 * through high-theater market signaling (0.72) and lack of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_switching_barrier,
    'Is the barrier a technical impossibility (Mountain) or a predatory design (Snare)?',
    'Open-source bridge implementation and adoption rate tracking.',
    'If bridge allows exit: Snare. If bridge fails due to complexity: Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(rational_inertia_trap, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
