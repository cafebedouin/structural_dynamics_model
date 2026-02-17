% ============================================================================
% CONSTRAINT STORY: germany_tennet_takeover
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_germany_tennet_takeover, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: germany_tennet_takeover
 *   human_readable: German Government Stake in TenneT Germany
 *   domain: economic/political
 *
 * SUMMARY:
 *   The German government is buying a significant stake in TenneT Germany, a
 *   crucial electricity grid operator, to ensure energy security during the
 *   green energy transition and prevent foreign takeovers. This acquisition is
 *   framed as a vital infrastructure investment, funded by taxpayers.
 *
 * KEY AGENTS (by structural relationship):
 *   - German Taxpayers: Primary target (powerless/trapped) — bear extraction via taxes.
 *   - German Government & Industry: Primary beneficiary (institutional/arbitrage) — benefits from grid control and stability.
 *   - Analytical Observer: Sees the full hybrid structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(germany_tennet_takeover, 0.35). % Moderate extraction reflecting use of public funds for a project with a genuine public good component.
domain_priors:suppression_score(germany_tennet_takeover, 0.55).   % Structural property (raw, unscaled). State-backed monopoly on critical infrastructure suppresses alternatives. Raised from 0.20 to meet Tangled Rope threshold (>= 0.40).
domain_priors:theater_ratio(germany_tennet_takeover, 0.10).       % Piton detection (>= 0.70). Low theater; this is a functional, not performative, action.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(germany_tennet_takeover, extractiveness, 0.35).
narrative_ontology:constraint_metric(germany_tennet_takeover, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(germany_tennet_takeover, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(germany_tennet_takeover, tangled_rope).
narrative_ontology:human_readable(germany_tennet_takeover, "German Government Stake in TenneT Germany").

% --- Binary flags ---
domain_priors:requires_active_enforcement(germany_tennet_takeover). % Required for Tangled Rope. The state's legal/financial framework enforces the monopoly.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, german_state_and_industry).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(germany_tennet_takeover, german_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% Taxpayers are victims with trapped exit. Engine derives d≈0.95 -> f(d)≈1.42.
% χ = 0.35 * 1.42 * 1.0 (national) = 0.497.
% The metrics (ε=0.35, suppression=0.55, χ=0.497) meet the Tangled Rope criteria.
% It feels extractive, but doesn't meet the high thresholds for a pure Snare.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The state is a beneficiary with arbitrage exit. Engine derives d≈0.05 -> f(d)≈-0.12.
% χ = 0.35 * -0.12 * 1.0 (national) = -0.042.
% The negative effective extraction reflects a perceived net benefit and control gain, classifying it as a Rope.
constraint_indexing:constraint_classification(germany_tennet_takeover, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Acknowledges both the coordination function (beneficiary exists) and the asymmetric
% extraction (victim exists), enforced by the state. This is the definition of a Tangled Rope.
% Engine derives d≈0.72 -> f(d)≈1.15. χ = 0.35 * 1.15 * 1.2 (global) = 0.483.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germany_tennet_takeover_tests).

test(perspectival_gap_tangled_rope_vs_rope) :-
    % Verify the core perspectival gap between the target and beneficiary.
    constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germany_tennet_takeover, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(germany_tennet_takeover_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The German government's stake in TenneT is a classic Tangled Rope. The base
 *   extractiveness (0.35) is moderate, reflecting the use of public funds for a
 *   project with a genuine public good component. The suppression score (0.55)
 *   is significant, as a state-backed monopoly on critical infrastructure
 *   inherently limits alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the perception of a necessary coordination tool and a
 *   coercive extraction mechanism.
 *   - For taxpayers (powerless/trapped), the mandatory funding and lack of alternatives
 *     results in a Tangled Rope classification (χ≈0.50). It is extractive and coercive.
 *   - For the state (institutional/arbitrage), it's a Rope providing control and
 *     stability, with a perceived net benefit (negative effective extraction, χ≈-0.04).
 *   - The Analytical observer synthesizes these views. It sees the valid coordination
 *     function (grid stability) and the asymmetric extraction (taxpayer funding),
 *     which, combined with state enforcement, defines a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the clear structural roles. The `german_state_and_industry`
 *   is the beneficiary, gaining strategic control over energy infrastructure. This
 *   drives their directionality `d` towards 0. `german_taxpayers` are the victims,
 *   bearing the financial cost without direct consent, driving their `d` towards 1.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   The Tangled Rope classification is crucial. A simpler model might call this a
 *   Snare (focusing only on the taxpayer) or a Rope (focusing only on the state's
 *   intent). Tangled Rope correctly identifies that it is BOTH: a coordination
 *   mechanism funded by asymmetric extraction. This prevents the system from
 *   ignoring either the genuine benefit or the coercive cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_germany_tennet_takeover,
    'Will state ownership lead to greater efficiency and security, or will it introduce political inefficiencies that outweigh the benefits?',
    'Long-term comparative analysis of grid performance, costs, and innovation rates against privately-held grid operators in similar markets.',
    'If more efficient, it validates the Rope aspect. If less efficient, it strengthens the Tangled Rope/Snare aspect and increases the effective extraction over time.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(germany_tennet_takeover, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% While base_extractiveness (0.35) is below the 0.46 threshold requiring this
% data, it is included to model the initial state and potential for future drift.

% Theater ratio over time (starts low and stays low):
narrative_ontology:measurement(germany_tennet_takeover_tr_t0, germany_tennet_takeover, theater_ratio, 0, 0.05).
narrative_ontology:measurement(germany_tennet_takeover_tr_t5, germany_tennet_takeover, theater_ratio, 5, 0.10).
narrative_ontology:measurement(germany_tennet_takeover_tr_t10, germany_tennet_takeover, theater_ratio, 10, 0.10).

% Extraction over time (initial investment cost, then stable):
narrative_ontology:measurement(germany_tennet_takeover_ex_t0, germany_tennet_takeover, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(germany_tennet_takeover_ex_t5, germany_tennet_takeover, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(germany_tennet_takeover_ex_t10, germany_tennet_takeover, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a state intervention to secure a foundational utility.
narrative_ontology:coordination_type(germany_tennet_takeover, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */