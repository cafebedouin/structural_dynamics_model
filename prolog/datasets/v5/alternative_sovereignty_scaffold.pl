% ============================================================================
% CONSTRAINT STORY: alternative_sovereignty_scaffold
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_alternative_sovereignty_scaffold, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: alternative_sovereignty_scaffold
 * human_readable: The Decentralized Parallel
 * domain: technological/social
 * * SUMMARY:
 * A framework of decentralized protocols (crypto-networks, P2P legal templates) 
 * that provide coordination outside traditional state "Ropes."
 * It functions as a Scaffold—temporary infrastructure that allows a population 
 * to migrate away from failing institutional Snares.
 * * KEY AGENTS:
 * - The User: Subject (Moderate) - Utilizing P2P tools to bypass decayed systems.
 * - The Protocol Developer: Architect (Organized) - Coding the "Scaffold" rules.
 * - The State Auditor: Auditor (Analytical) - Perceiving the network as a "Snare" to tax authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(alternative_sovereignty_scaffold, 0.22). % Low: Focus is on coordination over rent-seeking.
domain_priors:suppression_score(alternative_sovereignty_scaffold, 0.35).   % Low/Moderate: Entry is voluntary, but network effects limit exit.
domain_priors:theater_ratio(alternative_sovereignty_scaffold, 0.10).       % Very Low: Functional code-is-law, not theatrical.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, extractiveness, 0.22).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, theater_ratio, 0.1).

% Binary flags
narrative_ontology:has_sunset_clause(alternative_sovereignty_scaffold).    % Defined as a transitionary Scaffold.
domain_priors:requires_active_enforcement(alternative_sovereignty_scaffold). % Enforcement via protocol consensus.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REFUGEE (ROPE)
% To the individual escaping trust decay, the network is a pure coordination Rope.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, rope, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE ARCHITECT (SCAFFOLD)
% The developers view this as a temporary bridge to a new social contract.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(alternative_sovereignty_scaffold).

% PERSPECTIVE 3: THE STATE ANALYST (SNARE)
% To the institutional incumbent, the network is a Snare that extracts tax base and control.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, snare, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(trapped), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alt_sovereignty_tests).

test(scaffold_to_rope_shift) :-
    % Verify that for organized power, it is a Scaffold, but for the user, it is a Rope.
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, rope, context(agent_power(powerless), _, _, _)).

test(extraction_threshold) :-
    % Verify the base extraction is low enough to qualify as a non-predatory system.
    domain_priors:base_extractiveness(alternative_sovereignty_scaffold, E), E < 0.46.

:- end_tests(alt_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The low base_extractiveness (0.22) reflects the efficiency of code-based 
 * coordination compared to high-friction human bureaucracies.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the "Scaffold" status. This network is not 
 * intended to be a permanent "Mountain" of governance, but a temporary support 
 * while older institutions undergo decay or reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_protocol_capture,
    'Will the decentralized Scaffold harden into a new, predatory Snare via centralizing miners/nodes?',
    'Gini coefficient analysis of protocol governance tokens over a 10-year horizon.',
    'If centralized: Becomes a Snare; If decentralized: Remains a Rope/Scaffold.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(alternative_sovereignty_scaffold, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
