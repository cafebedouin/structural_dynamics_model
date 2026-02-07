% ============================================================================
% CONSTRAINT STORY: knowledge_action_gap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(knowledge_action_gap, []).

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
 * * constraint_id: knowledge_action_gap
 * human_readable: The Informational Friction Barrier
 * domain: social/technological
 * * SUMMARY:
 * This constraint represents the structural disconnect between having access 
 * to perfect information regarding a systemic risk and the inability to 
 * coordinate a response due to high switching costs or habit-loops. 
 * It functions as a 'Mountain' to individuals but a 'Snare' to future auditors.
 * * KEY AGENTS:
 * - Informed Individual: Subject (Powerless)
 * - Habitual Infrastructure: Beneficiary (Institutional)
 * - Behavioral Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.71) because the gap effectively siphons optionality.
domain_priors:base_extractiveness(knowledge_action_gap, 0.71). 
domain_priors:suppression_score(knowledge_action_gap, 0.55).
domain_priors:theater_ratio(knowledge_action_gap, 0.25).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(knowledge_action_gap, extractiveness, 0.71).
narrative_ontology:constraint_metric(knowledge_action_gap, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(knowledge_action_gap, theater_ratio, 0.25).

% No sunset clause; it is an inherent property of complex behavioral systems.
% narrative_ontology:has_sunset_clause(knowledge_action_gap). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the powerless individual, the gap feels like an unchangeable law of human nature.
constraint_indexing:constraint_classification(knowledge_action_gap, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutions view the gap as a 'Rope' that maintains predictability and stability 
% by preventing "volatile" reactions to every new piece of information.
constraint_indexing:constraint_classification(knowledge_action_gap, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% From an analytical distance, this gap is a snare that extracts future value 
% by suppressing necessary immediate changes.
constraint_indexing:constraint_classification(knowledge_action_gap, snare, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% High extraction (0.71) triggers the hybrid Tangled Rope signature for deep audit.
constraint_indexing:constraint_classification(knowledge_action_gap, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(knowledge_action_gap, E), E >= 0.50,
    domain_priors:suppression_score(knowledge_action_gap, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_action_gap_tests).

test(perspectival_gap) :-
    % Verify the "Individual powerless" sees a Mountain while the "Institutional" sees a Rope.
    constraint_indexing:constraint_classification(knowledge_action_gap, mountain, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_action_gap, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(knowledge_action_gap, E),

    (E =< 0.05 ; E >= 0.46).

:- end_tests(knowledge_action_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction value (0.71) reflects the lost potential energy of un-applied 
 * knowledge. The low theater ratio (0.25) distinguishes this from a Piton.
 * * PERSPECTIVAL GAP:
 * The Individual feels the gap as a Mountain (I know I should change, but 
 * the world won't let me). The Institution feels it as a Rope (The gap keeps 
 * the current system functional and coordinated).
 * * [RESOLVED MANDATROPHY]:
 * Resolved by acknowledging the Tangled Rope classification. The gap is 
 * not pure extraction; it provides the 'latency' necessary for social 
 * cohesion, even if that latency becomes predatory at the 0.71 threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required as extraction (0.71) > 0.46.
omega_variable(
    omega_cognitive_load,
    'Is the gap a hard neurological limit (Mountain) or a designed friction (Snare)?',
    'Introduction of sub-cognitive behavioral nudges in an isolated cohort.',
    'If gap remains: Mountain of biology. If gap closes: Snare of architecture.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(knowledge_action_gap, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
