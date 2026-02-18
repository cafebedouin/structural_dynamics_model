% ============================================================================
% CONSTRAINT STORY: minimax_theorem_game_equilibrium
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_minimax_theorem_game_equilibrium, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: minimax_theorem_game_equilibrium
 *   human_readable: Minimax Theorem Enforcement in Strategic Games
 *   domain: economic
 *
 * SUMMARY:
 *   The Minimax Theorem guarantees the existence of a Nash equilibrium in zero-sum games, where one player's gain is exactly balanced by the losses of the other player. While mathematically sound, enforcing this equilibrium in real-world strategic interactions often leads to a constraint where powerful players extract value by subtly influencing the game's rules or information available to others, even if they theoretically should expect the other player to act optimally. This constraint describes the difference between theoretical minimax solutions and actual power dynamics in game situations.
 *
 * KEY AGENTS (by structural relationship):
 *   - Weaker Player: Primary target (powerless/trapped) — Bears extraction due to information asymmetry or rule manipulation.
 *   - Powerful Player: Primary beneficiary (institutional/arbitrage) — Benefits from influencing the game in their favor.
 *   - Game Designer/Rule Maker: Secondary actor (powerful/constrained) — Constrained by the need to maintain the appearance of a fair game while potentially favoring one player.
 *   - Analytical Observer: Analytical observer (analytical/analytical) — Sees the complete structure of the game and the theorem's application.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(minimax_theorem_game_equilibrium, 0.48). % Moderate extraction, representing the inherent advantage taken by more powerful player through subtle manipulation.
domain_priors:suppression_score(minimax_theorem_game_equilibrium, 0.62).   % Moderate suppression. Other strategies aren't impossible, but the dominant player ensures they are less effective.
domain_priors:theater_ratio(minimax_theorem_game_equilibrium, 0.20).       % Relatively low theater; the effect is subtle, not overtly performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(minimax_theorem_game_equilibrium, extractiveness, 0.48).
narrative_ontology:constraint_metric(minimax_theorem_game_equilibrium, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(minimax_theorem_game_equilibrium, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(minimax_theorem_game_equilibrium, tangled_rope).
narrative_ontology:human_readable(minimax_theorem_game_equilibrium, "Minimax Theorem Enforcement in Strategic Games").
narrative_ontology:topic_domain(minimax_theorem_game_equilibrium, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(minimax_theorem_game_equilibrium). % Active enforcement of the equilibrium to maintain the extraction dynamic.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(minimax_theorem_game_equilibrium, powerful_player).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(minimax_theorem_game_equilibrium, weaker_player).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(minimax_theorem_game_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(minimax_theorem_game_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(minimax_theorem_game_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(minimax_theorem_game_equilibrium_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(minimax_theorem_game_equilibrium, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(minimax_theorem_game_equilibrium, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(minimax_theorem_game_equilibrium, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(minimax_theorem_game_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.48, indicating a moderate level of inherent extraction. The suppression score is 0.62, as weaker players often have their options limited, though not entirely eliminated. The theater ratio is low (0.20) because the manipulations are often subtle and not for public display. The requirement for active enforcement stems from the need to maintain the equilibrium, preventing weaker players from breaking free from the imposed constraints.
 *
 * PERSPECTIVAL GAP:
 *   The weaker player perceives a snare, as their options are limited and they are extracted from. The powerful player perceives a rope, as they view the system as beneficial for them, allowing them to extract value within the rules. The analytical observer sees a tangled rope, acknowledging both the coordination aspects of game equilibrium and the extraction imposed by the powerful player.
 *
 * DIRECTIONALITY LOGIC:
 *   The powerful player benefits by subtly influencing the rules and/or information available, allowing them to extract value consistently. The weaker player bears the cost, as their options are limited, and they are often in a position where they are predictably taken advantage of. The beneficiary declaration maps to the structural relationship by explicitly stating who benefits (powerful player) and who bears the cost (weaker player).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a tangled rope prevents mislabeling the situation as pure coordination (rope). Although the minimax theorem provides a framework for achieving equilibrium (coordination), powerful players often manipulate the system to extract value, leading to an imbalanced outcome that isn't purely coordinated. It also prevents mistaking it as pure extraction (snare) as the theorem does provide some level of theoretical coordination, even if it's subverted in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_minimax_theorem_game_equilibrium,
    'To what extent can the seemingly "fair" rules of the game be subtly manipulated by powerful players to extract value from weaker players?',
    'Empirical analysis of real-world game scenarios, including economic and political interactions, focusing on rule-making processes and information asymmetry.',
    'If True (high manipulation): The constraint leans more towards a snare. If False (low manipulation): The constraint leans more towards a rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(minimax_theorem_game_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(minimax_theorem_game_equilibrium_tr_t0, minimax_theorem_game_equilibrium, theater_ratio, 0, 0.15).
narrative_ontology:measurement(minimax_theorem_game_equilibrium_tr_t5, minimax_theorem_game_equilibrium, theater_ratio, 5, 0.20).
narrative_ontology:measurement(minimax_theorem_game_equilibrium_tr_t10, minimax_theorem_game_equilibrium, theater_ratio, 10, 0.25).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(minimax_theorem_game_equilibrium_ex_t0, minimax_theorem_game_equilibrium, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(minimax_theorem_game_equilibrium_ex_t5, minimax_theorem_game_equilibrium, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(minimax_theorem_game_equilibrium_ex_t10, minimax_theorem_game_equilibrium, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(minimax_theorem_game_equilibrium, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(minimax_theorem_game_equilibrium, 0.3).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(minimax_theorem_game_equilibrium, other_constraint_id).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(minimax_theorem_game_equilibrium, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */