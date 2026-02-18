% ============================================================================
% CONSTRAINT STORY: alternative_sovereignty_scaffold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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
 * - The User/Refugee: Subject (Powerless) - Utilizing P2P tools to bypass decayed systems.
 * - The Protocol Developer: Architect (Organized) - Coding the "Scaffold" rules.
 * - The State Analyst: Beneficiary of Incumbent System (Institutional) - Perceiving the network as a "Snare" to tax authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(alternative_sovereignty_scaffold, 0.22). % Low: Focus is on coordination over rent-seeking.
domain_priors:suppression_score(alternative_sovereignty_scaffold, 0.35).   % Low/Moderate: Entry is voluntary, but network effects limit exit.
domain_priors:theater_ratio(alternative_sovereignty_scaffold, 0.10).       % Very Low: Functional code-is-law, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, extractiveness, 0.22).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(alternative_sovereignty_scaffold, scaffold).
narrative_ontology:human_readable(alternative_sovereignty_scaffold, "The Decentralized Parallel").
narrative_ontology:topic_domain(alternative_sovereignty_scaffold, "technological/social").

% Binary flags
narrative_ontology:has_sunset_clause(alternative_sovereignty_scaffold).    % Defined as a transitionary Scaffold.
domain_priors:requires_active_enforcement(alternative_sovereignty_scaffold). % Enforcement via protocol consensus.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, decentralized_network_users).
narrative_ontology:constraint_victim(alternative_sovereignty_scaffold, legacy_state_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE REFUGEE (ROPE)
% To the individual escaping trust decay, the network is a pure coordination Rope.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: THE STATE ANALYST (SNARE)
% To the institutional incumbent, the network is a Snare that extracts tax base and control.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, snare,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% The developers view this as a temporary bridge to a new social contract.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(alternative_sovereignty_scaffold).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SCAFFOLD)
% The analytical observer, noting the low extraction, sunset clause, and coordination
% function, classifies it as a Scaffold.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alt_sovereignty_tests).

test(perspectival_gap_user_vs_state) :-
    % Verify the core conflict: users see a Rope, the state sees a Snare.
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, snare, context(agent_power(institutional), _, _, _)).

test(architect_is_scaffold) :-
    % Verify that for organized power (the architect), it is a Scaffold.
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold, context(agent_power(organized), _, _, _)).

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
 * coordination compared to high-friction human bureaucracies. The key perspectival
 * gap is between the user and the state. The user, fleeing a high-extraction
 * environment, perceives the low-extraction alternative as a pure Rope. The
 * state, however, experiences the loss of its tax base and regulatory control
 * as a form of extraction, hence its classification as a Snare. The architect
 * and the analytical observer, aware of the sunset clause and its transitional
 * purpose, correctly identify it as a Scaffold.
 *
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the "Scaffold" classification. This network is not
 * intended to be a permanent "Mountain" of governance, but a temporary support
 * while older institutions undergo decay or reform. Its explicit sunset clause
 * and low extraction prevent it from being misclassified as a permanent Snare,
 * even though one index (the state) perceives it that way.
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

% Required for external script parsing
narrative_ontology:interval(alternative_sovereignty_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for this constraint as its base_extractiveness (0.22)
% is below the 0.46 threshold for mandatory drift detection. The constraint is
% modeled as stable over its interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system provides an alternative infrastructure for social/economic coordination.
narrative_ontology:coordination_type(alternative_sovereignty_scaffold, global_infrastructure).

% Network relationships (structural influence edges)
% This scaffold directly affects the efficacy of traditional state mechanisms.
narrative_ontology:affects_constraint(alternative_sovereignty_scaffold, state_taxation_regime).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */