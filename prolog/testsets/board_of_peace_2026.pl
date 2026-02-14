% ============================================================================
% CONSTRAINT STORY: board_of_peace_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_board_of_peace_2026, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: board_of_peace_2026
 *   human_readable: "The 2026 'Board of Peace' Initiative"
 *   domain: political
 *
 * SUMMARY:
 *   Based on a hypothetical 2026 news report, a new international body called
 *   the "Board of Peace" is established by a US administration. It offers
 *   members "fast-track arbitration for trade disputes" and "security
 *   guarantees". While framed as a global coordination mechanism for peace
 *   and prosperity, it functions as a parallel power structure to existing
 *   institutions (UN, WTO), creating a coercive dynamic where participation
 *   and alignment with the board's architects are necessary to avoid
 *   economic and political marginalization.
 *
 * KEY AGENTS (by structural relationship):
 *   - US Administration & Core Allies: Primary beneficiary (institutional/arbitrage) — architects of the system, benefit from favorable arbitration and increased geopolitical leverage.
 *   - Non-Aligned Member States: Primary target (powerless/trapped) — smaller nations pressured to join, bearing the costs of unfavorable rulings and policy coercion.
 *   - Legacy International Bodies (UN, WTO): Inter-institutional victim (institutional/constrained) — their authority and coordinating function are suppressed by the new body.
 *   - Analytical Observer: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(board_of_peace_2026, 0.48).
domain_priors:suppression_score(board_of_peace_2026, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(board_of_peace_2026, 0.80).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(board_of_peace_2026, extractiveness, 0.48).
narrative_ontology:constraint_metric(board_of_peace_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(board_of_peace_2026, theater_ratio, 0.80).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(board_of_peace_2026, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(board_of_peace_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(board_of_peace_2026, us_administration_and_core_allies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(board_of_peace_2026, non_aligned_member_states).
narrative_ontology:constraint_victim(board_of_peace_2026, legacy_international_bodies).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% A non-aligned nation pressured to join. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.48 (ε) * 1.42 (f(d)) * 1.0 (σ(National)) ≈ 0.68.
%   With χ > 0.66 and suppression > 0.60, this is a Snare.
constraint_indexing:constraint_classification(board_of_peace_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The US administration that designed the board. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
%   χ = 0.48 (ε) * -0.12 (f(d)) * 1.0 (σ(National)) ≈ -0.06.
%   The negative extraction makes this appear as a pure coordination tool.
constraint_indexing:constraint_classification(board_of_peace_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
%   χ = 0.48 (ε) * 1.15 (f(d)) * 1.2 (σ(Global)) ≈ 0.66.
%   With 0.40 ≤ χ ≤ 0.90, ε > 0.30, suppression > 0.40, and having both
%   beneficiary and victim groups, this is a canonical Tangled Rope.
constraint_indexing:constraint_classification(board_of_peace_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% Perspective 4: A Legacy International Body (e.g., the UN)
% As an institution, it is powerful, but its relationship to this new
% constraint is that of a victim with constrained exit options.
% Engine derives d from: victim + institutional + constrained -> d ≈ 0.65 -> f(d) ≈ 1.0.
%   χ = 0.48 (ε) * 1.0 (f(d)) * 1.2 (σ(Global)) ≈ 0.58.
%   The result is still a Tangled Rope, but the measured extraction (χ) is
%   lower than for the analytical observer, reflecting its partial ability
%   to mitigate the constraint's effects. This demonstrates a measurable
%   perspectival gap between different institutional actors.
constraint_indexing:constraint_classification(board_of_peace_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(board_of_peace_2026_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the primary target and beneficiary.
    constraint_indexing:constraint_classification(board_of_peace_2026, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(board_of_peace_2026, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(board_of_peace_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify all three structural conditions for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(board_of_peace_2026, _),
    narrative_ontology:constraint_victim(board_of_peace_2026, _),
    domain_priors:requires_active_enforcement(board_of_peace_2026).

:- end_tests(board_of_peace_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): The mechanism is not pure theft; it offers a genuine coordination function (trade arbitration). However, the structure allows for significant, systemic bias in outcomes, making it highly extractive.
 *   - Suppression (0.75): The board is explicitly designed to be an alternative to, and thereby suppress the influence of, existing international institutions like the UN and WTO. High score reflects its role as an institutional competitor.
 *   - Theater Ratio (0.80): The name "Board of Peace" and the rhetoric of making the world "richer, safer" is a performative layer designed to mask a geopolitical power play. The ratio of performative signaling to actual function is very high.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the architects (US Admin), the costs are externalized, making the board a highly efficient coordination tool (Rope). For a smaller nation forced to participate, the biased arbitration and coercive policy alignment are experienced as a pure extraction mechanism with no viable alternative (Snare). The analytical observer sees both functions simultaneously, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `us_administration_and_core_allies`. They designed the rules and have `arbitrage` exit (they can ignore or reinterpret rulings that don't suit them). This structural advantage drives their derived directionality (d) close to 0.
 *   - Victims: `non_aligned_member_states`. They are rule-takers with `trapped` exit (non-participation leads to isolation). This structural disadvantage drives their d close to 1.
 *   The system correctly derives the directionality from these declared structural relationships.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights the importance of inter-institutional perspectives. Both the US Admin and the UN are `institutional` actors. However, their relationship to the constraint is opposite. The US is a beneficiary with `arbitrage` exit, while the UN is a victim with `constrained` exit. The v6.0 directionality engine correctly assigns them different `d` values based on their declared victim/beneficiary status and exit options, resulting in different perceived extraction (χ) and demonstrating a measurable gap between institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the institution, preventing a misclassification of "it has a coordination function, therefore it is a Rope." The high suppression score and the declaration of a victim group are critical inputs that allow the system to see past the "peace board" theater and identify the underlying coercive, extractive structure, classifying it correctly as a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_board_of_peace_2026,
    'Was the arbitration mechanism designed with intentional bias, or is it a neutral system that is merely being exploited by its powerful members?',
    'A long-term statistical analysis of a decade of arbitration rulings, comparing outcomes for powerful vs. less-powerful member states.',
    'If designed for bias, it is a canonical Tangled Rope. If it is an exploited neutral system, it is a Rope that has degraded due to insufficient safeguards against power asymmetry.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(board_of_peace_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction (ε=0.48) is > 0.46, so temporal data is required.
% This models the board becoming more overtly extractive over its first decade
% as initial pretenses of neutrality are eroded.
%
% Theater ratio over time (starts high and stays high):
narrative_ontology:measurement(bop_tr_t0, board_of_peace_2026, theater_ratio, 0, 0.75).
narrative_ontology:measurement(bop_tr_t5, board_of_peace_2026, theater_ratio, 5, 0.78).
narrative_ontology:measurement(bop_tr_t10, board_of_peace_2026, theater_ratio, 10, 0.80).

% Extraction over time (models extraction_accumulation drift):
narrative_ontology:measurement(bop_ex_t0, board_of_peace_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(bop_ex_t5, board_of_peace_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bop_ex_t10, board_of_peace_2026, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It serves both to enforce geopolitical alignment and
% re-allocate resources via trade disputes. 'enforcement_mechanism' captures
% the primary geopolitical function.
narrative_ontology:coordination_type(board_of_peace_2026, enforcement_mechanism).

% Network relationships: This constraint directly impacts the effectiveness
% and legitimacy of existing global governance constraints.
narrative_ontology:affects_constraint(board_of_peace_2026, un_security_council_veto).
narrative_ontology:affects_constraint(board_of_peace_2026, wto_dispute_settlement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain
% (beneficiary/victim declarations + exit_options) correctly computes the
% directionality (d) for all key agents, including the distinct institutional
% perspectives of the US administration and legacy international bodies.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */