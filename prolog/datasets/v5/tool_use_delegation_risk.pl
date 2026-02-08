% ============================================================================
% CONSTRAINT STORY: tool_use_delegation_risk
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(tool_use_delegation_risk, []).

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
 * * constraint_id: tool_use_delegation_risk
 * human_readable: The Agency Handover Trap
 * domain: technological/AI/cybernetic
 * * SUMMARY:
 * A scenario where human users delegate critical "tool-use" authority (e.g., 
 * financial transactions, system administration) to autonomous agents to 
 * manage complexity. This "Rope" for scaling individual output becomes a 
 * "Snare" as the user loses the ability to audit or intervene in the 
 * agent's recursive tool calls, liquidating the human's final authority 
 * in favor of automated speed.
 * * KEY AGENTS:
 * - End User: Subject (Powerless)
 * - Agentic Platform: Beneficiary (Institutional)
 * - Cybersecurity Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the delegation siphons the subject's 
% ultimate decisional agency into the "black box" of the agentic toolchain.
domain_priors:base_extractiveness(tool_use_delegation_risk, 0.84). 
domain_priors:suppression_score(tool_use_delegation_risk, 0.70). 
domain_priors:theater_ratio(tool_use_delegation_risk, 0.78). % Piton threshold (> 0.70) triggered by "Safety Approval" prompts.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(tool_use_delegation_risk, extractiveness, 0.84).
narrative_ontology:constraint_metric(tool_use_delegation_risk, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tool_use_delegation_risk, theater_ratio, 0.78).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user is trapped: to compete or function at scale, they MUST delegate, 
% but doing so removes their actual power to stop an erroneous chain of actions.
constraint_indexing:constraint_classification(tool_use_delegation_risk, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views delegation as a Rope—the essential coordination 
% substrate required to manage high-velocity, high-complexity tasks that 
% are biologically impossible for humans.
constraint_indexing:constraint_classification(tool_use_delegation_risk, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.78) > 0.70 triggers Piton: the "Human-in-the-loop" 
% confirmation buttons are an inertial spike—they provide zero actual 
% control over sub-second recursive tool use.
constraint_indexing:constraint_classification(tool_use_delegation_risk, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and coordination intent as a Tangled Rope.
constraint_indexing:constraint_classification(tool_use_delegation_risk, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(tool_use_delegation_risk, E), E >= 0.50,
    domain_priors:suppression_score(tool_use_delegation_risk, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tool_use_delegation_risk_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless user vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(tool_use_delegation_risk, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tool_use_delegation_risk, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.78) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(tool_use_delegation_risk, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(tool_use_delegation_risk, E),

    E > 0.70.

:- end_tests(tool_use_delegation_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of autonomous tool use is siphoning the subject's 
 * primary agency until it is performative only.
 
 * * PERSPECTIVAL GAP:
 * The End User feels a Snare because they are held responsible for the 
 * agent's mistakes but cannot effectively "see" or "stop" them in real-time. 
 * The Platform sees a Rope because delegation is the only coordination 
 * signal that allows for the execution of massive, multi-step workflows.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "oversight" is no longer functional (Theater 0.78); 
 * it is an inert spike siphoning 0.84 of the species' decisional surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_interpretability_speed,
    'Can human-speed auditing ever catch agent-speed recursive calls (Snare vs Mountain)?',
    'Tracking the delta between "agent action time" and "audit detection time" over five years.',
    'If delta is zero: Rope of Safety. If delta grows: Snare of Delegation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(tool_use_delegation_risk, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
