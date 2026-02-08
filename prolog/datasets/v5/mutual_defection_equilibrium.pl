% ============================================================================
% CONSTRAINT STORY: mutual_defection_equilibrium
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(mutual_defection_equilibrium, []).

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
 * * constraint_id: mutual_defection_equilibrium
 * human_readable: The Infinite Prisoner's Dilemma Stalemate
 * domain: economic/social
 * * SUMMARY:
 * This constraint represents a stable social state where all agents defect 
 * because no single agent can gain by switching to cooperation alone. 
 * While the state is optimal for no one, it acts as a 'Mountain' of 
 * inescapable logic for the individual and a 'Snare' for the collective.
 * * KEY AGENTS:
 * - Individual Actor: Subject (Powerless)
 * - Systemic Incentive Structure: Beneficiary (Institutional)
 * - Game Theory Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.82) because the equilibrium wastes the vast majority 
% of potential surplus that cooperation would generate.
domain_priors:base_extractiveness(mutual_defection_equilibrium, 0.82). 
domain_priors:suppression_score(mutual_defection_equilibrium, 0.70).
domain_priors:theater_ratio(mutual_defection_equilibrium, 0.15). % Low theater; it is a raw logical limit.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(mutual_defection_equilibrium, extractiveness, 0.82).
narrative_ontology:constraint_metric(mutual_defection_equilibrium, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(mutual_defection_equilibrium, theater_ratio, 0.15).

% No sunset clause; it is a stable attractor in the absence of external force.
% narrative_ontology:has_sunset_clause(mutual_defection_equilibrium). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the powerless individual, the need to defect is an unchangeable law of survival.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institutional structure 'coordinates' this state of defection to ensure 
% predictable (if low-value) behavior and prevent sudden system shocks.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% From a historical perspective, the equilibrium is a snare that extracts 
% potential progress and locks the species into a zero-sum loop.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, snare, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% Extraction (0.82) and Suppression (0.70) trigger the hybrid Tangled Rope signature.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(mutual_defection_equilibrium, E), E >= 0.50,
    domain_priors:suppression_score(mutual_defection_equilibrium, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mutual_defection_equilibrium_tests).

test(perspectival_gap) :-
    % Verify the 'Individual' sees a Mountain (necessity) while the 'Auditor' sees a Snare (trap).
    constraint_indexing:constraint_classification(mutual_defection_equilibrium, mountain, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mutual_defection_equilibrium, snare, 
        context(agent_power(analytical), time_horizon(historical), _, _)).

test(extraction_threshold) :-
    % Ensure extraction meets the high-extraction core limits (> 0.46).
    domain_priors:base_extractiveness(mutual_defection_equilibrium, E),

    E >= 0.46.

:- end_tests(mutual_defection_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) reflects the total loss of cooperative surplus. 
 * Low theater (0.15) indicates that this isn't a performative failure; it is 
 * a grounded failure of incentives.
 * * PERSPECTIVAL GAP:
 * The Individual Actor feels a Mountain because they have no degrees of freedom; 
 * to cooperate is to be exploited. The Game Theory Auditor identifies a Snare 
 * because the system is designed (intentionally or not) to extract stability 
 * at the cost of the actors' welfare.
 * * [RESOLVED MANDATROPHY]:
 * This is resolved by the Tangled Rope classification at the global index. 
 * It acknowledges that 'mutual defection' is a form of coordination—it creates 
 * a stable, predictable environment—but it is highly extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for extraction (0.82) > 0.46.
omega_variable(
    omega_cooperation_barrier,
    'Is the defection due to lack of trust (Snare) or lack of communication bandwidth (Mountain)?',
    'Implementation of an un-hackable, high-bandwidth reputation ledger.',
    'If cooperation emerges: Snare. If defection remains: Mountain of biology.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(mutual_defection_equilibrium, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
