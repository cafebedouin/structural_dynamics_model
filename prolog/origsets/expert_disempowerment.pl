% ============================================================================
% CONSTRAINT STORY: expert_disempowerment
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(expert_disempowerment, []).

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
 * * constraint_id: expert_disempowerment
 * human_readable: Algorithmic Oversight Erosion
 * domain: technological/social
 * * SUMMARY:
 * This constraint represents the systematic stripping of discretionary power 
 * from domain experts (e.g., doctors, engineers) in favor of rigid, automated 
 * decision-support systems. While marketed as "coordination," it functions 
 * as a Snare by removing human accountability and professional agency.
 * * KEY AGENTS:
 * - Domain Specialist: Subject (Powerless)
 * - System Administrator: Beneficiary (Institutional)
 * - Liability Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.74) because the "efficiency" gains are captured by the 
% institution while the expert bears the cognitive load of a "theater" of choice.
domain_priors:base_extractiveness(expert_disempowerment, 0.74). 
domain_priors:suppression_score(expert_disempowerment, 0.65).
domain_priors:theater_ratio(expert_disempowerment, 0.82). % High theater: The expert is "consulted" but cannot override.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(expert_disempowerment, extractiveness, 0.74).
narrative_ontology:constraint_metric(expert_disempowerment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(expert_disempowerment, theater_ratio, 0.82).

% Not a scaffold; this is an architectural shift in institutional power.
% narrative_ontology:has_sunset_clause(expert_disempowerment). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The specialist is trapped in a workflow where they have responsibility but no authority.
constraint_indexing:constraint_classification(expert_disempowerment, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the automation as a Rope for scale, reliability, and risk mitigation.
constraint_indexing:constraint_classification(expert_disempowerment, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.82) > 0.70 triggers Piton: The expert's role is now an inertial relic.
constraint_indexing:constraint_classification(expert_disempowerment, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(expert_disempowerment, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.74) triggers the hybrid Tangled Rope signature.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(expert_disempowerment, E), E >= 0.50,
    domain_priors:suppression_score(expert_disempowerment, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expert_disempowerment_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict for v3.4 core.
    constraint_indexing:constraint_classification(expert_disempowerment, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expert_disempowerment, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger_logic) :-
    % Ensure high theater results in Piton classification for auditors.
    constraint_indexing:constraint_classification(expert_disempowerment, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(expert_disempowerment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.74) represents the loss of professional optionality.
 * The high theater_ratio (0.82) is key—the expert is kept in the loop purely 
 * for legal liability, not for operational leverage.
 * * PERSPECTIVAL GAP:
 * The Expert feels a Snare because their skill is commoditized; the 
 * Institution feels a Rope because their liability is standardized.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by identifying the system as a Piton for analytical observers. 
 * While it provides "coordination" (Rope), the high extraction and 
 * performative human involvement indicate it is an inertial maintenance 
 * of a non-functional power structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_discretionary_value,
    'Is human discretion a bottleneck (Mountain) or a necessary safety valve (Snare)?',
    'Blind audit of "Expert Override" vs "System Suggestion" in critical failures.',
    'If Expert > System: Snare of automation. If System > Expert: Mountain of human error.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(expert_disempowerment, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
