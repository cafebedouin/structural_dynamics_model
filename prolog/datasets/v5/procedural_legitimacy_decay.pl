% ============================================================================
% CONSTRAINT STORY: procedural_legitimacy_decay
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(procedural_legitimacy_decay, []).

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
 * * constraint_id: procedural_legitimacy_decay
 * human_readable: The Hollow Formality Trap
 * domain: social/political
 * * SUMMARY:
 * This constraint represents a bureaucratic process that has lost its 
 * justifying social consensus but maintains its legal force. As the gap 
 * between 'process' and 'result' grows, the system transitions from a 
 * coordination 'Rope' into an extractive 'Snare' or an inertial 'Piton'.
 * * KEY AGENTS:
 * - Applicant: Subject (Powerless)
 * - Administrative Body: Beneficiary (Institutional)
 * - Political Philosopher: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.76) as participants must expend high effort for low-legitimacy outcomes.
domain_priors:base_extractiveness(procedural_legitimacy_decay, 0.76). 
domain_priors:suppression_score(procedural_legitimacy_decay, 0.62).
domain_priors:theater_ratio(procedural_legitimacy_decay, 0.88). % Extreme theater triggers Piton classification.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(procedural_legitimacy_decay, extractiveness, 0.76).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, theater_ratio, 0.88).

% This is not a scaffold; it is a structural erosion of trust.
% narrative_ontology:has_sunset_clause(procedural_legitimacy_decay). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the powerless agent, the decay is a snare that extracts resources with no return.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutional actors view the hollow procedure as a vital coordination rope for stability.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 confirms the auditor sees an inertial, non-functional spike.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(procedural_legitimacy_decay, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction/suppression signatures identify the hybrid extraction nature.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(procedural_legitimacy_decay, E), E >= 0.50,
    domain_priors:suppression_score(procedural_legitimacy_decay, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_legitimacy_decay_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict for the v3.4 core.
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_validation) :-
    % Ensure high theater results in Piton detection by systems auditors.
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(procedural_legitimacy_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.76) reflects a process that has become a tax on 
 * participation rather than a service of coordination.
 * * PERSPECTIVAL GAP:
 * To the 'powerless', the decay is a Snare—an obligatory trap. To 
 * the 'institutional' actor, it is a Rope—a necessary formal sequence.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Piton classification. The 'Tangled Rope' signature is 
 * functionally atrophied; the system is being maintained solely through 
 * high theater (0.88), indicating institutional inertia over utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_legitimacy_pivot,
    'Is the decay caused by procedural complexity or active bad-faith gatekeeping?',
    'Natural language audit of administrative outcome justifications vs intent.',
    'If gatekeeping: Snare. If complexity: Mountain of institutional debt.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(procedural_legitimacy_decay, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
