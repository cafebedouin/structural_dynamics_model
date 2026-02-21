% ============================================================================
% CONSTRAINT STORY: demand_response_cost_shift
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_demand_response_cost_shift, []).

:- use_module(library(plunit)).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).
:- use_module(utility(config)).


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
 *   constraint_id: demand_response_cost_shift
 *   human_readable: Smart Grid Demand Response Rate Structure
 *   domain: economic/technological
 *
 * SUMMARY:
 *   "Demand Response" (DR) programs are designed to stabilize electrical grids
 *   by incentivizing users to reduce consumption during peak hours. However,
 *   the costs of infrastructure and rebates are socialized across all
 *   ratepayers, while the benefits (rebates) are disproportionately captured
 *   by higher-income households with smart appliances and flexible schedules.
 *   This creates a structural transfer of wealth from low-income to high-income
 *   households under the guise of a grid coordination mechanism.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-income households: Primary target (powerless/trapped) — bear extraction via higher rates without access to benefits.
 *   - High-income households: Primary beneficiary (powerful/arbitrage) — receive rebates and grid stability funded by the wider rate base.
 *   - Utility companies: Institutional beneficiary (institutional/arbitrage) — achieve grid stability at lower capital cost than building new capacity.
 *   - Energy researchers: Analytical observer — identifies both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demand_response_cost_shift, 0.48).
domain_priors:suppression_score(demand_response_cost_shift, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(demand_response_cost_shift, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demand_response_cost_shift, extractiveness, 0.48).
narrative_ontology:constraint_metric(demand_response_cost_shift, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(demand_response_cost_shift, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(demand_response_cost_shift, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(demand_response_cost_shift). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, high_income_households).
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, utility_companies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(demand_response_cost_shift, low_income_households).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (LOW-INCOME HOUSEHOLDS)
% They experience only the rate increases funding the program, without the
% means to access the benefits. Their trapped status and victimhood derive a
% high directionality (d ≈ 0.95), making effective extraction χ very high.
% From this perspective, the constraint is a pure Snare.
constraint_indexing:constraint_classification(demand_response_cost_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (UTILITY COMPANIES)
% They benefit from a highly effective grid coordination mechanism that reduces
% capital expenditure. Their institutional power and arbitrage exit derive a
% low directionality (d ≈ 0.05), making effective extraction χ negative.
% From this perspective, the constraint is a pure Rope.
constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ENERGY RESEARCHERS)
% The analytical observer can see both the genuine coordination function (grid
% stability) and the asymmetric extractive flow. This dual nature, combined
% with the high suppression and active enforcement, is the classic signature
% of a Tangled Rope.
constraint_indexing:constraint_classification(demand_response_cost_shift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: SECONDARY BENEFICIARY (HIGH-INCOME HOUSEHOLDS)
% As beneficiaries with flexible consumption and access to smart technology,
% they can arbitrage the system for rebates. Like the utility, they perceive
% the program as a beneficial coordination mechanism (Rope). Their power is
% less than institutional but their exit is still high.
constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demand_response_cost_shift_tests).

test(perspectival_gap, [nondet]) :-
    % Verify the core perspectival gap: Rope for beneficiaries, Snare for targets.
    constraint_indexing:constraint_classification(demand_response_cost_shift, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope, [nondet]) :-
    % The system's final judgment should recognize the hybrid nature.
    constraint_indexing:constraint_classification(demand_response_cost_shift, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that the structural data supports Tangled Rope classification.
    narrative_ontology:constraint_beneficiary(demand_response_cost_shift, _),
    narrative_ontology:constraint_victim(demand_response_cost_shift, _),
    domain_priors:requires_active_enforcement(demand_response_cost_shift).

:- end_tests(demand_response_cost_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): Represents the significant, non-trivial transfer of wealth via socialized rate increases that fund rebates for a subset of users. It's high enough to cause harm but not totalizing.
 *   - Suppression Score (S=0.75): Low-income households have very few alternatives. They are typically served by a single utility monopoly and lack the capital for off-grid solutions, making their exit options 'trapped'. Suppression is a structural property and is not scaled.
 *   - Theater Ratio (T=0.15): The program is highly functional. It genuinely solves the problem of peak load on the grid. Its extractive nature is a consequence of its implementation, not a failure of its primary function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and a classic feature of Tangled Ropes.
 *   - Beneficiaries (utilities, high-income users) see a 'Rope'. They experience a clever coordination mechanism that creates value (grid stability, rebates) with minimal apparent downside *to them*. Their directionality `d` is low, so the system's extractiveness is invisible or even appears as a subsidy.
 *   - Victims (low-income users) see a 'Snare'. They experience only the punitive side: rate increases without any access to the offsetting benefits. Their directionality `d` is high, so the system's extractiveness is magnified into a purely coercive mechanism.
 *   - The Analytical perspective correctly identifies it as a 'Tangled Rope' because it acknowledges the validity of both claims: the coordination is real, and the extraction is real.
 *
 * DIRECTIONALITY LOGIC:
 *   The `constraint_beneficiary` and `constraint_victim` declarations directly model the structure described in the source material.
 *   - `low_income_households` are victims because the rate structure extracts from them to fund the system.
 *   - `high_income_households` and `utility_companies` are beneficiaries because they capture the financial rewards (rebates) and functional benefits (grid stability) of the system.
 *   The engine uses these declarations, combined with the `trapped` vs. `arbitrage` exit options, to derive the starkly different directionality values (`d`) that drive the perspectival gap in `χ`.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors.
 *   1. It is not a pure Snare, because that would ignore the program's legitimate and valuable grid coordination function. Calling it a Snare would be an oversimplification that misses *why* the constraint is so stable and popular among powerful actors.
 *   2. It is not a pure Rope, because that would ignore the regressive, extractive wealth transfer at its core. Calling it a Rope would be to adopt the beneficiary's perspective uncritically and erase the harm done to the victims.
 *   The Tangled Rope classification captures this duality, identifying it as a system where a valid coordination goal has been instrumented to create an asymmetric extractive flow.
 *   Furthermore, the "Dynamic Coalition" extension suggests that if low-income households were to organize (e.g., via a ratepayers union), their power could shift from `powerless` to `organized`, potentially forcing a renegotiation of the constraint's terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dr_intent,
    'Is the regressive cost-shift an intentional feature designed to subsidize affluent users, or an unintentional (but structurally convenient) side-effect of optimizing for grid efficiency above all else?',
    'Analysis of internal regulatory filings and utility planning documents to determine if equity impacts were modeled and ignored, or never considered.',
    'If intentional, the base extractiveness (ε) might be higher, and the constraint is more stable. If unintentional, it represents a policy failure that could be corrected, making it more like a flawed Scaffold.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(demand_response_cost_shift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This constraint is high-extraction
% (ε > 0.46), so measurements are required. The trajectory shows how the
% extractive nature of the program intensified over time as it scaled from a
% pilot to a mature system, a classic example of extraction_accumulation.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(dr_tr_t0, demand_response_cost_shift, theater_ratio, 0, 0.10).
narrative_ontology:measurement(dr_tr_t5, demand_response_cost_shift, theater_ratio, 5, 0.12).
narrative_ontology:measurement(dr_tr_t10, demand_response_cost_shift, theater_ratio, 10, 0.15).

% Extraction over time (shows accumulation):
narrative_ontology:measurement(dr_ex_t0, demand_response_cost_shift, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(dr_ex_t5, demand_response_cost_shift, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(dr_ex_t10, demand_response_cost_shift, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This program is a mechanism for allocating a scarce resource (peak grid capacity).
narrative_ontology:coordination_type(demand_response_cost_shift, resource_allocation).

% Network relationships (structural influence edges)
% The need for DR programs is driven by the challenge of integrating
% intermittent renewable energy sources into the grid.
narrative_ontology:affects_constraint(renewable_energy_intermittency, demand_response_cost_shift).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */