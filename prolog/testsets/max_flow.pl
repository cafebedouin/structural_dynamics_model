% ============================================================================
% CONSTRAINT STORY: max_flow_min_cut
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_max_flow_min_cut, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: max_flow_min_cut
 *   human_readable: Socio-Technical Bottlenecks Governed by Max-Flow Principles
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint models the socio-technical application of the Max-Flow
 *   Min-Cut theorem, not the mathematical theorem itself. It describes how
 *   physical or policy-enforced bottlenecks in networks (e.g., logistics,
 *   internet bandwidth, trade routes) are used for both coordination and
 *   extraction. The system's throughput is hard-capped by its narrowest point,
 *   creating opportunities for rent-seeking by the bottleneck's controller.
 *
 * KEY AGENTS (by structural relationship):
 *   - End Users: Primary target (powerless/trapped) — experience the system's
 *     limits as a hard constraint and bear the costs of congestion or tolls.
 *   - Bottleneck Controllers: Primary beneficiary (institutional/arbitrage) —
 *     own or regulate the network's narrowest points, enabling them to
 *     extract rent and control flow.
 *   - Network Engineers: Secondary actor (organized/mobile) — use the
 *     principle as a coordination tool to optimize network design.
 *   - Analytical Observer: Sees the dual function of coordination and extraction.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of 2 stories decomposed from "The Max-Flow Min-Cut Theorem".
 * Decomposed because ε differs across observables (ε-invariance principle).
 * This story models the socio-technical application. The mathematical law is
 * modeled separately.
 * Related stories:
 *   - max_flow_min_cut_theorem (ε=0.01, Mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(max_flow_min_cut, 0.50).
domain_priors:suppression_score(max_flow_min_cut, 0.60).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(max_flow_min_cut, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(max_flow_min_cut, extractiveness, 0.50).
narrative_ontology:constraint_metric(max_flow_min_cut, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(max_flow_min_cut, theater_ratio, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(max_flow_min_cut, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(max_flow_min_cut). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(max_flow_min_cut, bottleneck_controllers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(max_flow_min_cut, end_users).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

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
% Agent who bears the most extraction. For an end user of a national ISP or
% logistics network, the bottleneck is a coercive limit with no alternatives.
% Engine derives d from victim membership + trapped exit → high χ.
% χ = 0.50 * f(0.95) * σ(national) = 0.50 * 1.42 * 1.0 = 0.71. (Snare)
constraint_indexing:constraint_classification(max_flow_min_cut, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The bottleneck controller who owns the infrastructure sees the limit as a
% tool for generating revenue, a pure coordination/benefit mechanism.
% Engine derives d from beneficiary membership + arbitrage exit → negative χ.
% χ = 0.50 * f(0.05) * σ(national) = 0.50 * -0.12 * 1.0 = -0.06. (Rope)
constraint_indexing:constraint_classification(max_flow_min_cut, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (efficient network design)
% and the asymmetric extraction (rent-seeking). This dual nature is the
% definition of a Tangled Rope.
% Engine derives d ≈ 0.72 for analytical perspective.
% χ = 0.50 * f(0.72) * σ(global) = 0.50 * 1.15 * 1.2 = 0.69. (Tangled Rope/Snare)
constraint_indexing:constraint_classification(max_flow_min_cut, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(max_flow_min_cut_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the gap between the user (Snare) and the owner (Rope).
    constraint_indexing:constraint_classification(max_flow_min_cut, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(max_flow_min_cut, rope, context(agent_power(institutional), _, arbitrage, _)).

test(analytical_claim_is_tangled_rope) :-
    % Verify the analytical observer sees the dual function.
    constraint_indexing:constraint_classification(max_flow_min_cut, tangled_rope, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(max_flow_min_cut, tangled_rope).

test(tangled_rope_structural_gates_met) :-
    % Verify all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(max_flow_min_cut, _),
    narrative_ontology:constraint_victim(max_flow_min_cut, _),
    domain_priors:requires_active_enforcement(max_flow_min_cut).

:- end_tests(max_flow_min_cut_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file conflated a mathematical theorem (a Mountain) with its
 *   socio-technical application, leading to metric conflicts. This version
 *   resolves the conflict by focusing strictly on the application: the use of
 *   network bottlenecks for control and extraction.
 *   - Base Extractiveness (ε=0.50): Represents significant potential for
 *     rent-seeking (e.g., congestion pricing, toll roads, ISP data caps).
 *   - Suppression (0.60): High, as controllers of critical infrastructure
 *     often lobby against or acquire competitors to maintain the bottleneck.
 *   - `requires_active_enforcement`: Added because maintaining a bottleneck
 *     for profit requires enforcing property rights or regulatory capture.
 *   This combination of metrics and structural data correctly identifies the
 *   constraint as a Tangled Rope from an analytical view.
 *
 * PERSPECTIVAL GAP:
 *   - End Users (powerless/trapped) see a Snare. They are forced to use the
 *     network and pay the price of the bottleneck, with no viable alternatives.
 *     The high χ (0.71) reflects this coercive extraction.
 *   - Bottleneck Controllers (institutional/arbitrage) see a Rope. For them,
 *     the constraint is a pure tool for coordination and revenue generation,
 *     with negative effective extraction (χ = -0.06).
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying this as a Tangled Rope, the system avoids two errors. It
 *   rejects the controller's claim that this is purely a Rope (coordination),
 *   acknowledging the high extraction felt by users. It also rejects the user's
 *   claim that it is purely a Snare (extraction), acknowledging that the
 *   underlying network does provide a genuine coordination function. The
 *   `tangled_rope` classification correctly captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_max_flow_min_cut,
    'Is the bottleneck a result of physical constraints or deliberate policy/underinvestment?',
    'Forensic engineering and economic analysis of the network owner\'s capital expenditures vs. revenue.',
    'If physical, it leans towards a natural Rope. If deliberate, it is a clear Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(max_flow_min_cut, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (ε > 0.46).
% Models a network that was initially built for public good (low extraction)
% but was later privatized or exploited for rent-seeking, causing extraction
% to accumulate over time.
%
% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(max_flow_min_cut_tr_t0, max_flow_min_cut, theater_ratio, 0, 0.05).
narrative_ontology:measurement(max_flow_min_cut_tr_t5, max_flow_min_cut, theater_ratio, 5, 0.05).
narrative_ontology:measurement(max_flow_min_cut_tr_t10, max_flow_min_cut, theater_ratio, 10, 0.05).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(max_flow_min_cut_ex_t0, max_flow_min_cut, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(max_flow_min_cut_ex_t5, max_flow_min_cut, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(max_flow_min_cut_ex_t10, max_flow_min_cut, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint is fundamentally about managing flows.
narrative_ontology:coordination_type(max_flow_min_cut, resource_allocation).

% Network relationships: This socio-technical constraint is affected by the
% underlying mathematical law.
narrative_ontology:affects_constraint(max_flow_min_cut_theorem, max_flow_min_cut).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */