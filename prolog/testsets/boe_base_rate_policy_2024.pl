% ============================================================================
% CONSTRAINT STORY: boe_base_rate_policy_2024
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-20
% ============================================================================

:- module(constraint_boe_base_rate_policy_2024, []).

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
 *   constraint_id: boe_base_rate_policy_2024
 *   human_readable: Bank of England's 5.25% Base Interest Rate Policy (2024)
 *   domain: economic
 *
 * SUMMARY:
 *   The Bank of England's Monetary Policy Committee (MPC) is maintaining the
 *   UK's base interest rate at 5.25%, a 16-year high, as a mechanism to
 *   control inflation. While inflation has returned to the 2% target, the
 *   bank cites concerns about underlying price pressures. This policy
 *   creates a sharp divide: it imposes severe financial strain on mortgage
 *   holders while providing the best returns for savers in over a decade.
 *
 * KEY AGENTS (by structural relationship):
 *   - uk_variable_rate_mortgage_holders: Primary target (powerless/trapped) — bears direct financial extraction via increased loan payments.
 *   - uk_savers: Primary beneficiary (moderate/mobile) — benefits from higher returns on savings.
 *   - uk_commercial_banks: Institutional beneficiary (institutional/arbitrage) — benefits from the interest rate spread.
 *   - bank_of_england_mpc: Institutional architect/enforcer (institutional/constrained) — sets the rate, constrained by its inflation-control mandate.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boe_base_rate_policy_2024, 0.48).
domain_priors:suppression_score(boe_base_rate_policy_2024, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(boe_base_rate_policy_2024, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, extractiveness, 0.48).
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(boe_base_rate_policy_2024, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(boe_base_rate_policy_2024). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, uk_savers).
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, uk_commercial_banks).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, uk_variable_rate_mortgage_holders).
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

% PERSPECTIVE 1: THE MORTGAGE HOLDER (PRIMARY TARGET)
% Engine derives d from victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ = 0.48 * 1.42 * 1.0 (national) ≈ 0.68. This meets the snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SAVER (PRIMARY BENEFICIARY)
% Engine derives d from beneficiary membership + mobile exit -> d ≈ 0.15 -> f(d) ≈ -0.01 -> low/negative χ.
% χ = 0.48 * -0.01 * 1.0 (national) ≈ -0.005. This is a clear Rope.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. Global scope amplifies χ.
% χ = 0.48 * 1.15 * 1.2 (global) ≈ 0.66. Meets Tangled Rope criteria (0.40 <= χ <= 0.90).
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% Perspective 4A: Bank of England (Architect/Enforcer)
% Constrained exit reflects their mandate. They are not a direct beneficiary and
% experience political pressure. Derived d is higher than a beneficiary.
% d ~ 0.6 -> f(d) ~ 0.85 -> χ ~ 0.41. This is a Tangled Rope.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4B: Commercial Banks (Institutional Beneficiary)
% Beneficiary + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ.
% χ = 0.48 * -0.12 * 1.0 (national) ≈ -0.058. A highly beneficial Rope.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boe_base_rate_policy_2024_tests).

test(perspectival_gap_victim_vs_beneficiary) :-
    % Verify the core perspectival gap between those who pay and those who profit.
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Victim (powerless/trapped) sees SNARE, Beneficiary (institutional/arbitrage) sees ROPE. Gap confirmed.~n').

test(tangled_rope_analytical_view) :-
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('... Analytical observer correctly identifies TANGLED_ROPE.~n').

test(tangled_rope_gate_requirements) :-
    narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, _),
    narrative_ontology:constraint_victim(boe_base_rate_policy_2024, _),
    domain_priors:requires_active_enforcement(boe_base_rate_policy_2024),
    format('... All three gate requirements for Tangled Rope are met.~n').

:- end_tests(boe_base_rate_policy_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.48) is high because the policy's primary mechanism is a
 *   forced wealth transfer from borrowers to savers/lenders to cool economic demand.
 *   Suppression (0.65) is high as mortgage holders have few-to-no alternatives to the
 *   prevailing interest rates set by the BoE. Theater (0.15) is low; the BoE's
 *   actions are highly functional, not performative. These metrics, combined with
 *   the presence of both beneficiaries and victims, make it a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For a mortgage holder (powerless/trapped), the policy is
 *   purely extractive, with no discernible coordination benefit, making it a Snare.
 *   For commercial banks (institutional/arbitrage) and savers (moderate/mobile),
 *   it is a highly beneficial coordination mechanism (a Rope) that increases their
 *   returns. The disagreement is not about interpretation; it is a direct result
 *   of their opposing structural positions relative to the cash flow created by the
 *   interest rate.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'uk_savers' and 'uk_commercial_banks' directly benefit from
 *     the higher rates. The derivation engine assigns them low directionality (d),
 *     resulting in low or negative effective extraction (χ).
 *   - Victims: 'uk_variable_rate_mortgage_holders' bear the direct cost. The
 *     engine assigns them high directionality (d), resulting in high χ.
 *   This mapping correctly models the structural reality of the monetary policy.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the Bank of England and commercial banks. The
 *   BoE, with 'constrained' exit, is modeled as an actor bound by its mandate,
 *   viewing the policy as a necessary but costly tool (Tangled Rope). Commercial
 *   banks, with 'arbitrage' exit, are modeled as pure beneficiaries who exploit
 *   the rate environment for profit (Rope). This captures the crucial difference
 *   between the policy's architect and its financial intermediaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The system correctly classifies this as a Tangled Rope from an analytical view,
 *   preventing the error of labeling it a pure Snare (which would ignore the valid
 *   coordination goal of inflation control) or a pure Rope (which would ignore the
 *   massive, asymmetric extraction imposed on borrowers). It captures the dual
 *   nature of the policy. The potential for coalition power among mortgage holders,
 *   especially in an election year, could shift their power from 'powerless' to
 *   'organized', but their exit options remain trapped, preserving the Snare classification
 *   from their perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_boe_base_rate_policy_2024,
    'Is the 5.25% rate an over-correction that unnecessarily extracts from borrowers, or is it the minimum necessary level to prevent an inflationary rebound?',
    'Observing inflation and wage growth data over the 12 months following an eventual rate cut. If inflation remains stable, the policy was likely an over-correction.',
    'If over-correction, the constraint is more extractive than necessary (ε could be re-evaluated higher). If necessary, the ε=0.48 value is justified as the structural cost of stability.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(boe_base_rate_policy_2024, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction > 0.46, so temporal data is required. This models the
% intensification of the policy as rates were hiked from near-zero levels.
% T=0: Start of hiking cycle. T=10: Current peak rate.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(boe_base_rate_policy_2024_tr_t0, boe_base_rate_policy_2024, theater_ratio, 0, 0.10).
narrative_ontology:measurement(boe_base_rate_policy_2024_tr_t5, boe_base_rate_policy_2024, theater_ratio, 5, 0.12).
narrative_ontology:measurement(boe_base_rate_policy_2024_tr_t10, boe_base_rate_policy_2024, theater_ratio, 10, 0.15).

% Extraction over time (shows accumulation as rates rose):
narrative_ontology:measurement(boe_base_rate_policy_2024_ex_t0, boe_base_rate_policy_2024, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(boe_base_rate_policy_2024_ex_t5, boe_base_rate_policy_2024, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(boe_base_rate_policy_2024_ex_t10, boe_base_rate_policy_2024, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The policy is fundamentally about re-allocating capital
% and spending power to manage aggregate demand.
narrative_ontology:coordination_type(boe_base_rate_policy_2024, resource_allocation).

% Network relationships: Monetary policy has direct causal effects on other
% major economic constraints.
narrative_ontology:affects_constraint(boe_base_rate_policy_2024, uk_housing_market_affordability).
narrative_ontology:affects_constraint(boe_base_rate_policy_2024, small_business_lending_access).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations combined with distinct exit_options
% (trapped, mobile, constrained, arbitrage) accurately computes the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */