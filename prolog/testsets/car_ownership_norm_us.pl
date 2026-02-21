% ============================================================================
% CONSTRAINT STORY: car_ownership_norm_us
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_car_ownership_norm_us, []).

:- use_module(library(plunit)).
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
 *   constraint_id: car_ownership_norm_us
 *   human_readable: The Norm of Individual Car Ownership in the US
 *   domain: economic
 *
 * SUMMARY:
 *   Based on economist Dean Baker's analysis, this constraint models the
 *   socio-economic system in the United States that normalizes and often
 *   necessitates individual car ownership. While providing a genuine
 *   coordination function (transportation), it operates as a highly
 *   extractive system, transferring wealth from individuals to a complex
 *   of automotive, financial, and energy industries. The lack of viable
 *   alternatives (e.g., robust public transit) in many regions suppresses
 *   exit, locking individuals into a cycle of debt and high recurring costs.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low/middle-income households: Primary target (powerless/trapped) — bears the high costs of ownership, debt, and depreciation.
 *   - Automotive-financial-industrial complex: Primary beneficiary (institutional/arbitrage) — profits from manufacturing, sales, financing, insurance, fuel, and maintenance.
 *   - Urban planners & policymakers: Secondary actors (organized/constrained) — operate within the system, often reinforcing it through zoning and infrastructure decisions.
 *   - System analysts (e.g., Dean Baker): Analytical observer — sees the dual nature of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(car_ownership_norm_us, 0.55).
domain_priors:suppression_score(car_ownership_norm_us, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(car_ownership_norm_us, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(car_ownership_norm_us, extractiveness, 0.55).
narrative_ontology:constraint_metric(car_ownership_norm_us, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(car_ownership_norm_us, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(car_ownership_norm_us, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(car_ownership_norm_us).
domain_priors:requires_active_enforcement(car_ownership_norm_us). % Enforced by infrastructure, zoning, lack of alternatives.

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, automotive_financial_industrial_complex).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(car_ownership_norm_us, low_middle_income_households).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

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
% A household for whom a car is a non-negotiable requirement for employment,
% but also a major financial burden with no viable alternatives.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% χ = 0.55 * 1.42 * 1.0 (national) ≈ 0.78. This meets Snare criteria (χ ≥ 0.66).
constraint_indexing:constraint_classification(car_ownership_norm_us, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% An auto manufacturer, lender, or insurance company profiting from the system.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
% χ = 0.55 * -0.12 * 1.0 ≈ -0.07. This negative extraction classifies as Rope.
constraint_indexing:constraint_classification(car_ownership_norm_us, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An economist or sociologist viewing the entire system, recognizing both the
% coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for this perspective.
% χ = 0.55 * 1.15 * 1.0 ≈ 0.63. This is in the Tangled Rope range (0.40 ≤ χ ≤ 0.90).
constraint_indexing:constraint_classification(car_ownership_norm_us, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(car_ownership_norm_us_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap between the trapped target and the institutional beneficiary.
    constraint_indexing:constraint_classification(car_ownership_norm_us, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(car_ownership_norm_us, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(car_ownership_norm_us, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify that all three structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(car_ownership_norm_us, _),
    narrative_ontology:constraint_victim(car_ownership_norm_us, _),
    domain_priors:requires_active_enforcement(car_ownership_norm_us).

:- end_tests(car_ownership_norm_us_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect the significant, often
 *     underestimated, transfer of wealth from owners to industries via
 *     depreciation, interest, insurance, and maintenance. This is the core
 *     of Baker's argument.
 *   - Suppression (0.70): High because in many parts of the US, alternatives
 *     are structurally unavailable due to decades of policy focused on
 *     automotive infrastructure over public transit, creating a captive market.
 *   - Theater (0.20): Low because the primary function (transportation) is
 *     real and heavily used. The theater is in the marketing of "freedom,"
 *     not in the core utility.
 *   - The combination of a real coordination function with high, asymmetrically
 *     distributed costs makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the `low_middle_income_households` (target), the
 *   constraint is a Snare. The necessity of a car for economic survival,
 *   coupled with its high cost, functions as a debt trap. For the
 *   `automotive_financial_industrial_complex` (beneficiary), it is a Rope:
 *   a brilliantly coordinated, highly profitable, and self-sustaining market.
 *   The analytical view reconciles these by identifying the structure as a
 *   Tangled Rope, acknowledging both the coordination and the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `automotive_financial_industrial_complex`. This group actively
 *     lobbies to maintain the system (e.g., highway funding over transit) and
 *     profits directly from every part of the car ownership lifecycle. Their
 *     `arbitrage` exit option reflects their ability to shift capital (e.g.,
 *     from sedans to SUVs, from ICE to EV, from sales to financing).
 *   - Victim: `low_middle_income_households`. They bear the full cost. Their
 *     `trapped` exit option reflects the reality in car-dependent regions where
 *     losing one's car means losing one's job. This structural relationship
 *     drives the directionality (d) towards 1.0 for this group.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. First, it doesn't
 *   dismiss the system as a pure Snare from all perspectives, which would
 *   ignore the genuine coordination problem of transportation that it *does*
 *   solve. Second, it doesn't accept the beneficiary's "Rope" narrative, which
 *   masks the immense and coercive extraction. The Tangled Rope classification
 *   captures this essential duality, preventing the mislabeling of systemic
 *   extraction as mere market coordination.
 *   The Dynamic Coalition extension is relevant here: while individual households
 *   are `powerless`, organized movements (e.g., urbanists, public transit advocates)
 *   can achieve `organized` power, shifting the dynamics of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_car_ownership_norm_us,
    'Has the system''s primary function shifted from transportation coordination to pure wealth extraction, with transportation serving merely as the pretext?',
    'A comprehensive national-level economic study comparing the total societal cost of the current system (including externalities like pollution, accidents, time in traffic) against its net economic productivity, versus a model with robust, well-funded public and mixed-transit alternatives.',
    'If the primary function is still coordination, it remains a Tangled Rope. If it is now pure extraction, the constraint should be re-classified as a Snare even from the analytical perspective, indicating systemic failure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(car_ownership_norm_us, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has become more extractive over time as public transit options
% were dismantled, suburbs expanded, and car loans became more financialized.
% This data models the 'extraction_accumulation' lifecycle drift.
% Base extractiveness > 0.46, so temporal data is required.

% Theater ratio over time (the "freedom" narrative has remained fairly constant):
narrative_ontology:measurement(conu_tr_t0, car_ownership_norm_us, theater_ratio, 0, 0.30).
narrative_ontology:measurement(conu_tr_t5, car_ownership_norm_us, theater_ratio, 5, 0.25).
narrative_ontology:measurement(conu_tr_t10, car_ownership_norm_us, theater_ratio, 10, 0.20).

% Extraction over time (increasing due to financialization and rising costs):
narrative_ontology:measurement(conu_ex_t0, car_ownership_norm_us, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(conu_ex_t5, car_ownership_norm_us, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(conu_ex_t10, car_ownership_norm_us, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The road network and associated norms represent a form of global infrastructure.
narrative_ontology:coordination_type(car_ownership_norm_us, global_infrastructure).

% Network relationships (structural influence edges)
% Car dependency is structurally reinforced by zoning laws that mandate sprawl.
narrative_ontology:affects_constraint(car_ownership_norm_us, suburban_zoning_laws).
narrative_ontology:affects_constraint(car_ownership_norm_us, consumer_debt_market).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation based on
% the declared beneficiary/victim groups and their exit options accurately
% models the structural power dynamics of the system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */