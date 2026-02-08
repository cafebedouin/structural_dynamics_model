% ============================================================================
% CONSTRAINT STORY: hydra_game
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_hydra_game, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: hydra_game
 * human_readable: The Hydra Game (Kirby-Paris Theorem)
 * domain: technological
 * * SUMMARY:
 * The Hydra Game is a mathematical game on a rooted tree. While the player ("Hercules") is guaranteed to win, the proof of termination requires axioms beyond standard Peano Arithmetic. The game's length grows so rapidly that it is physically impossible to complete for any non-trivial starting position, effectively extracting the utility of the "guaranteed win" for any physical agent.
 * * KEY AGENTS:
 * - Physical Computer: Subject (Powerless), attempting to simulate the game.
 * - Axiomatic Architect: Beneficiary (Institutional), using the game to benchmark formal systems.
 * - Proof Theorist: Auditor (Analytical), observing the game as a fixed mathematical truth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(hydra_game, 0.70). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(hydra_game, 0.50).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(hydra_game, 0.0).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(hydra_game, extractiveness, 0.70).
narrative_ontology:constraint_metric(hydra_game, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(hydra_game, theater_ratio, 0.0).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(hydra_game, mountain).

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(hydra_game, axiomatic_architects).
narrative_ontology:constraint_victim(hydra_game, physical_computation).
narrative_ontology:constraint_victim(hydra_game, finitist_philosophers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For any physical machine, the game is a Snare. The promise of a "guaranteed win"
% is a trap, as the resources required (time, energy) exceed those available in
% the physical universe. It extracts all computational utility.
constraint_indexing:constraint_classification(hydra_game, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the architect of formal systems, the game is a Rope. It's a coordination
% tool to benchmark the strength of axiomatic systems. Its unprovability in PA
% justifies the need for stronger systems like ZFC.
constraint_indexing:constraint_classification(hydra_game, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% To the proof theorist, the game's termination is a fixed, unchangeable fact
% of the transfinite mathematical landscape (a property of the ordinal ε₀).
% It is an immutable law, hence a Mountain.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hydra_game_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(hydra_game, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hydra_game, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_view_is_mountain) :-
    constraint_indexing:constraint_classification(hydra_game, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hydra_game, ExtMetricName, E),
    E >= 0.46.

:- end_tests(hydra_game_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high base extractiveness (0.70) represents the game's extraction of "realizability." It is computable in theory but physically impossible to execute, making the promise of a win meaningless for any physical agent. The suppression score (0.50) reflects how the game's transfinite nature suppresses finitistic intuition, as its termination is unprovable in standard arithmetic.
 * The Perspectival Gap is stark:
 * - For a physical computer (powerless), it's a Snare that consumes all resources for no reward.
 * - For a logician (institutional), it's a Rope used to coordinate research and justify moving to stronger axiomatic systems.
 * - For a pure analyst, it's a Mountain—an immutable feature of mathematics.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A naive analysis might classify this as a pure Mountain, as it's a mathematical truth. However, this misses the perspectival reality for different agents. By classifying it as a Snare for physical computers and a Rope for system architects, the Deferential Realism framework captures the dual nature of the constraint: it is both a fixed law AND a tool with asymmetric outcomes. This prevents the system from mislabeling the practical, extractive consequences of an abstract "truth."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hydra_game,
    'Is the unprovability of termination in Peano Arithmetic a fundamental limit of finitist systems (Mountain), or a constructed boundary designed to be overcome by stronger systems (Rope)?',
    'Discovery of a novel, finitistic proof technique that resolves the game within PA, or a meta-mathematical proof that no such technique can exist.',
    'If Mountain, it confirms a hard limit on formal systems. If Rope, it suggests mathematical "difficulty" is a social and technical construct.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hydra_game, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As a timeless mathematical object,
% the Hydra Game's properties are constant from the moment of its discovery.
% The trajectory is flat, representing its unchanging nature.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(hydra_game_tr_t0, hydra_game, theater_ratio, 0, 0.0).
narrative_ontology:measurement(hydra_game_tr_t5, hydra_game, theater_ratio, 5, 0.0).
narrative_ontology:measurement(hydra_game_tr_t10, hydra_game, theater_ratio, 10, 0.0).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(hydra_game_ex_t0, hydra_game, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(hydra_game_ex_t5, hydra_game, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(hydra_game_ex_t10, hydra_game, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The game serves as a benchmark for comparing the strength of formal systems,
% functioning as an information standard.
narrative_ontology:coordination_type(hydra_game, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */