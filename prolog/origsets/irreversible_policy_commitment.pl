% ============================================================================
% CONSTRAINT STORY: irreversible_policy_commitment
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(irreversible_policy_commitment, []).

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
 * * constraint_id: irreversible_policy_commitment
 * human_readable: The Burned Bridge Protocol
 * domain: political/economic
 * * SUMMARY:
 * This constraint represents a policy decision that, once implemented, 
 * permanently alters the institutional landscape or social contract such 
 * that the cost of reversal is effectively infinite. It functions as a 
 * Rope for immediate coordination but settles into a Mountain for all 
 * future generations, who inherit the path without the leverage to pivot.
 * * KEY AGENTS:
 * - Future Citizen: Subject (Powerless)
 * - Founding Negotiator: Beneficiary (Institutional)
 * - Constitutional Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.72) because the commitment siphons future optionality 
% to secure present-day stability or political wins.
domain_priors:base_extractiveness(irreversible_policy_commitment, 0.72). 
domain_priors:suppression_score(irreversible_policy_commitment, 0.85). % Alternatives are structurally/legally annihilated.
domain_priors:theater_ratio(irreversible_policy_commitment, 0.35).    % Low theater; the irreversibility is functional and hard-coded.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(irreversible_policy_commitment, extractiveness, 0.72).
narrative_ontology:constraint_metric(irreversible_policy_commitment, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(irreversible_policy_commitment, theater_ratio, 0.35).

% This is the antithesis of a scaffold; it specifically lacks an exit/sunset.
% narrative_ontology:has_sunset_clause(irreversible_policy_commitment). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the future citizen, the policy is an immutable law of nature with zero degrees of freedom.
constraint_indexing:constraint_classification(irreversible_policy_commitment, mountain, 
    context(agent_power(powerless), 
            time_horizon(generational), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The founding institution views this as a vital Rope for long-term coordination and credibility.
constraint_indexing:constraint_classification(irreversible_policy_commitment, rope, 
    context(agent_power(institutional), 
            time_horizon(immediate), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Seen as a snare that extracts future freedom of action for present benefit.
constraint_indexing:constraint_classification(irreversible_policy_commitment, snare, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% Extraction (0.72) and Suppression (0.85) trigger the hybrid Tangled Rope signature.
constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(irreversible_policy_commitment, E), E >= 0.50,
    domain_priors:suppression_score(irreversible_policy_commitment, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irreversible_policy_commitment_tests).

test(perspectival_gap) :-
    % Verify the "Founding" Rope vs the "Inherited" Mountain gap.
    constraint_indexing:constraint_classification(irreversible_policy_commitment, mountain, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irreversible_policy_commitment, rope, 
        context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction meets the high-extraction core limits (> 0.46).
    domain_priors:base_extractiveness(irreversible_policy_commitment, E),

    E >= 0.46.

:- end_tests(irreversible_policy_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.72) represents the 'Mandatrophy' threshold where the 
 * value of coordination is purchased by permanently liquidating the optionality 
 * of all future participants. 
 * * PERSPECTIVAL GAP:
 * The Negotiator sees a Rope because they are actively using the constraint to 
 * coordinate (e.g., a treaty or constitutional lock). The Future Citizen 
 * sees a Mountain because they have no mechanism to renegotiate the terms.
 * * [RESOLVED MANDATROPHY]:
 * This is resolved by the Tangled Rope classification at the civilizational level. 
 * It acknowledges the coordination benefit (stability) while explicitly flagging 
 * the 0.72 extraction of agency as a predatory structural outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required as extraction (0.72) > 0.46.
omega_variable(
    omega_reversal_cost,
    'Is the irreversibility a physical limit (Mountain) or a legal fiction (Snare)?',
    'Historical analysis of "Black Swan" events that previously broke similar commitments.',
    'If crisis breaks it: Snare. If crisis confirms it: Mountain of History.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(irreversible_policy_commitment, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
