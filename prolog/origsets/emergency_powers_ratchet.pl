% ============================================================================
% CONSTRAINT STORY: emergency_powers_ratchet
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(emergency_powers_ratchet, []).

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
 * * constraint_id: emergency_powers_ratchet
 * human_readable: The Permanent Crisis Scaffold
 * domain: political/legal/social
 * * SUMMARY:
 * This constraint represents the systemic tendency for temporary "emergency" 
 * legal powers, enacted during a crisis (war, pandemic, economic collapse), 
 * to persist and become normalized as permanent administrative fixtures. 
 * While initially a "Rope" for urgent coordination, the ratchet mechanism 
 * transforms it into a "Snare" for the populace, as the high-suppression 
 * state outlives the crisis that justified it.
 * * KEY AGENTS:
 * - Civil Subject: Subject (Powerless)
 * - Executive State Body: Beneficiary (Institutional)
 * - Constitutional Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the normalization of emergency law siphons 
% the subject's fundamental liberties into the state's permanent 
% administrative surplus.
domain_priors:base_extractiveness(emergency_powers_ratchet, 0.84). 
domain_priors:suppression_score(emergency_powers_ratchet, 0.78). 
domain_priors:theater_ratio(emergency_powers_ratchet, 0.65). % Crisis theater remains necessary to justify the "temporary" status.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(emergency_powers_ratchet, extractiveness, 0.84).
narrative_ontology:constraint_metric(emergency_powers_ratchet, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(emergency_powers_ratchet, theater_ratio, 0.65).

% This constraint is characterized by the DECEPTIVE use of sunset clauses.
% narrative_ontology:has_sunset_clause(emergency_powers_ratchet). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: the "emergency" measures restrict their 
% options for exit or redress, yet there is no observable end-point 
% to the state of exception.
constraint_indexing:constraint_classification(emergency_powers_ratchet, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The state views the expanded powers as a Rope—the only way to coordinate 
% complex societal responses and maintain order in an increasingly 
% volatile environment.
constraint_indexing:constraint_classification(emergency_powers_ratchet, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.84) and suppression (0.78) trigger the hybrid 
% Tangled Rope signature at the historical scale.
constraint_indexing:constraint_classification(emergency_powers_ratchet, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(emergency_powers_ratchet, E), E >= 0.50,
    domain_priors:suppression_score(emergency_powers_ratchet, S), S > 0.40.

% PERSPECTIVE 4: THE LEGAL ARCHITECT (SCAFFOLD)
% During the initial 'immediate' horizon, the powers are perceived as a 
% temporary Scaffold for coordination.
constraint_indexing:constraint_classification(emergency_powers_ratchet, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_powers_ratchet_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless citizen vs Rope for the institutional state.
    constraint_indexing:constraint_classification(emergency_powers_ratchet, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_powers_ratchet, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(emergency_powers_ratchet, E),

    E > 0.70.

:- end_tests(emergency_powers_ratchet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * coordination benefit of the emergency has been subsumed by the permanent 
 * extraction of civil liberties.
 
 * * PERSPECTIVAL GAP:
 * The Civil Subject feels a Snare because their legal baseline is 
 * permanently lowered. The Executive Body sees a Rope because the 
 * administrative efficiency gained during the crisis is too valuable to 
 * relinquish for future coordination needs.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes the 
 * genuine coordination intent (Rope) that is now inextricably 
 * "tangled" with a high-extraction predatory structure (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_crisis_definition,
    'Is the "crisis" a discrete event (Snare) or a permanent state of the world (Mountain)?',
    'Tracking the frequency of crisis declarations vs the expiration of associated powers.',
    'If powers expire: Snare of policy. If crises are constant: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(emergency_powers_ratchet, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
