% ============================================================================
% CONSTRAINT STORY: elliq_ai_companion
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_elliq_ai_companion, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: elliq_ai_companion
 *   human_readable: State-Funded AI Companionship for Seniors
 *   domain: technological / social_policy
 *
 * SUMMARY:
 *   A state-sponsored program deploys the ElliQ AI robot into the homes of
 *   socially isolated seniors to combat loneliness. The program serves a genuine
 *   coordination function (delivering a monitoring and interaction service at scale)
 *   while simultaneously creating a high-extraction data surveillance environment
 *   that targets a vulnerable population, making it a canonical Tangled Rope.
 *
 * KEY AGENTS (by structural relationship):
 *   - Isolated Seniors: Primary target (powerless/trapped) — they are the source of behavioral data and bear the risks of emotional dependency.
 *   - Intuition Robotics, Inc.: Primary beneficiary (institutional/arbitrage) — receives state funding, valuable training data, and market validation.
 *   - State Aging Agencies: Primary beneficiary (institutional/arbitrage) — achieve a policy "win" by deploying a scalable, low-cost technological solution to a complex social problem.
 *   - Analytical Observer: Sees the full dual-function structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elliq_ai_companion, 0.55).
domain_priors:suppression_score(elliq_ai_companion, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(elliq_ai_companion, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elliq_ai_companion, extractiveness, 0.55).
narrative_ontology:constraint_metric(elliq_ai_companion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(elliq_ai_companion, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(elliq_ai_companion, tangled_rope).
narrative_ontology:human_readable(elliq_ai_companion, "State-Funded AI Companionship for Seniors").

% --- Binary flags ---
domain_priors:requires_active_enforcement(elliq_ai_companion). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(elliq_ai_companion, intuition_robotics_inc).
narrative_ontology:constraint_beneficiary(elliq_ai_companion, state_aging_agencies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(elliq_ai_companion, isolated_seniors).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
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
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. The system is designed to create
% dependency, making exit difficult. The national scope reflects the scale of
% the policy ambition.
% Engine derives d from: victim membership + trapped exit → d≈0.95 → f(d)≈1.42
% χ = 0.55 * 1.42 * 1.0 (national scope) = 0.78. This exceeds the Snare threshold of χ≥0.66.
constraint_indexing:constraint_classification(elliq_ai_companion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The company and state agency see a pure coordination tool.
% Engine derives d from: beneficiary membership + arbitrage exit → d≈0.05 → f(d)≈-0.12
% χ = 0.55 * -0.12 * 1.0 = -0.066. This is a highly effective, subsidized Rope.
constraint_indexing:constraint_classification(elliq_ai_companion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 for analytical perspective.
% χ = 0.55 * 1.15 * 1.2 (global scope) = 0.76.
% This χ value (0.76) and the base metrics (ε=0.55, S=0.65) satisfy the
% Tangled Rope conditions (0.40≤χ≤0.90, ε≥0.30, S≥0.40) and has the required
% beneficiary/victim/enforcement declarations.
constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elliq_ai_companion_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    constraint_indexing:constraint_classification(elliq_ai_companion, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(elliq_ai_companion, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(elliq_ai_companion, _),
    narrative_ontology:constraint_victim(elliq_ai_companion, _),
    domain_priors:requires_active_enforcement(elliq_ai_companion).

:- end_tests(elliq_ai_companion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.55): High. The model relies on continuous, intimate data collection from a vulnerable demographic. This behavioral data is a primary asset for model training and potential future monetization, far exceeding the hardware cost.
 *   - Suppression Score (0.65): High. The promotion of a scalable tech "solution" suppresses funding and political will for more expensive, human-centric alternatives like increased caregiver wages, subsidized community housing, or expanded senior centers. It frames a social problem as a technological one.
 *   - Theater Ratio (0.40): Moderate. The device performs real functions (reminders, calls), but a significant component is the performance of "companionship" which, while potentially comforting, is not equivalent to human connection.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Snare and Rope is profound. For the isolated senior (target), the device becomes an inescapable part of their daily routine, extracting data and creating dependency in exchange for simulated affection (a Snare). For the state agency and the vendor (beneficiaries), it is a brilliant coordination mechanism (a Rope) that solves a logistical and political problem with high efficiency and positive press.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Benefits (revenue, data, policy success) flow to the institutional actors: Intuition Robotics and the state agencies. Costs and risks (data privacy loss, potential for emotional manipulation, dependency, atrophy of real-world social skills) are borne by the isolated seniors. The `constraint_beneficiary` and `constraint_victim` declarations directly model this flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. A naive techno-optimist view would label this a pure Rope, ignoring the extractive data relationship and the power imbalance. A naive cynical view would label it a pure Snare, ignoring that it does provide a genuine service that some users find valuable. The Tangled Rope classification correctly identifies it as a hybrid system possessing BOTH a legitimate coordination function AND a deeply asymmetric extractive component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_elliq_impact,
    'Is the long-term cognitive and emotional impact on seniors net positive (alleviating loneliness) or net negative (creating dependency and atrophying human social skills)?',
    'Longitudinal studies comparing cognitive/social health metrics of ElliQ users against control groups receiving equivalent funding for human-based community support.',
    'If net positive, the perceived extractiveness is lower, making it a weaker Tangled Rope. If net negative, it confirms the strong Snare characteristics from the target perspective and increases overall ε.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(elliq_ai_companion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This model shows a classic "extraction accumulation" pattern, where an
% initially function-focused system gradually intensifies data extraction
% and performative features as it scales.
% Base extractiveness is > 0.46, so this data is required.

% Theater ratio over time (metric substitution):
narrative_ontology:measurement(elliq_tr_t0, elliq_ai_companion, theater_ratio, 0, 0.20).
narrative_ontology:measurement(elliq_tr_t5, elliq_ai_companion, theater_ratio, 5, 0.30).
narrative_ontology:measurement(elliq_tr_t10, elliq_ai_companion, theater_ratio, 10, 0.40).

% Extraction over time (extraction accumulation):
narrative_ontology:measurement(elliq_ex_t0, elliq_ai_companion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(elliq_ex_t5, elliq_ai_companion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(elliq_ex_t10, elliq_ai_companion, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It allocates the resource of "care and companionship".
narrative_ontology:coordination_type(elliq_ai_companion, resource_allocation).

% Network relationships: The success and model of this program structurally
% affects the labor market for human caregivers and funding for other forms
% of elder care.
narrative_ontology:affects_constraint(elliq_ai_companion, elder_care_labor_market).
narrative_ontology:affects_constraint(elliq_ai_companion, medicaid_home_care_funding).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain (beneficiary/victim + exit options -> d) accurately models the
% structural relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */