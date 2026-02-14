% ============================================================================
% CONSTRAINT STORY: fcc_dji_covered_list
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_fcc_dji_covered_list, []).

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
 *   constraint_id: fcc_dji_covered_list
 *   human_readable: "FCC 'Covered List' Ban on DJI Drones"
 *   domain: technological/political
 *
 * SUMMARY:
 *   The U.S. Federal Communications Commission (FCC) has placed SZ DJI
 *   Technology Co., a leading Chinese drone manufacturer, on its "Covered
 *   List," citing national security risks. This action prohibits the
 *   authorization of future DJI equipment, effectively banning their new
 *   products from the U.S. market. The constraint is a regulatory barrier
 *   that realigns the U.S. drone market away from Chinese technology, creating
 *   both beneficiaries (domestic competitors) and victims (DJI and U.S. users).
 *
 * KEY AGENTS (by structural relationship):
 *   - DJI & US Drone Users: Primary targets (powerless/trapped) — bear the cost of lost market access and higher prices/fewer choices.
 *   - US Domestic Drone Manufacturers: Primary beneficiaries (institutional/arbitrage) — benefit from the elimination of a major competitor.
 *   - US National Security Agencies (FCC/DoD): Institutional beneficiaries & enforcers (institutional/constrained) — benefit from achieving a stated security policy goal.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fcc_dji_covered_list, 0.52).
domain_priors:suppression_score(fcc_dji_covered_list, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(fcc_dji_covered_list, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fcc_dji_covered_list, extractiveness, 0.52).
narrative_ontology:constraint_metric(fcc_dji_covered_list, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fcc_dji_covered_list, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fcc_dji_covered_list, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(fcc_dji_covered_list). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, us_domestic_drone_manufacturers).
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, us_national_security_agencies).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fcc_dji_covered_list, dji_technology_co).
narrative_ontology:constraint_victim(fcc_dji_covered_list, us_drone_users).

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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1A: THE INDIVIDUAL USER (POWERLESS VICTIM)
% Represents hobbyists or small businesses who relied on DJI. They are victims
% with no ability to influence the policy and face higher costs for alternatives.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(fcc_dji_covered_list, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 1B: THE CORPORATE TARGET (DJI)
% DJI is completely locked out of future market access. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(fcc_dji_covered_list, snare,
    context(agent_power(powerful), % Powerful company, but powerless against state action
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US DRONE MANUFACTURERS)
% A protected market is created for them. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(fcc_dji_covered_list, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination (industrial/security policy) and the extraction
% (costs to users/DJI), classifying it as a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INSTITUTIONAL ENFORCER (FCC)
% As an institutional actor and beneficiary, it sees a Rope. However, its exit
% is constrained by political mandate, differentiating it from the commercial
% beneficiaries who can arbitrage the situation.
% The derived 'd' will be higher than for the domestic manufacturers but still low.
constraint_indexing:constraint_classification(fcc_dji_covered_list, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ORGANIZED END USER (E.G., PUBLIC SAFETY AGENCY)
% This user is also a victim, facing higher costs and fewer options. Their
% exit is constrained. Their perspective is a high-extraction Tangled Rope,
% bordering on a Snare, as they may derive some secondary benefit from
% using "secure" equipment but pay a direct cost.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fcc_dji_covered_list_tests).

test(perspectival_gap) :-
    % Verify the gap between a target (powerless user) and beneficiary (US mfg).
    constraint_indexing:constraint_classification(fcc_dji_covered_list, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(fcc_dji_covered_list, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, _),
    narrative_ontology:constraint_victim(fcc_dji_covered_list, _),
    domain_priors:requires_active_enforcement(fcc_dji_covered_list).

:- end_tests(fcc_dji_covered_list_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): Represents the significant economic value
 *     transferred. This includes the lost market share for DJI and the price
 *     premium US users will pay for domestically-sourced, potentially
 *     less-capable alternatives. It's a measure of the market distortion.
 *   - Suppression Score (0.75): The constraint is highly suppressive. It's a
 *     direct, state-enforced ban on the market leader, eliminating the primary
 *     alternative for many users based on price and performance.
 *   - Theater Ratio (0.20): While there's a performative element to national
 *     security posturing, the ban has direct, tangible market effects. It is
 *     primarily a functional regulatory action, not just for show.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For US domestic drone manufacturers, the ban is a perfect
 *   Rope: it coordinates the market to their benefit, solving the collective
 *   action problem of competing against a dominant, state-backed foreign firm.
 *   For DJI and powerless US users, it is a Snare: a coercive measure that
 *   removes choice, increases costs, and extracts value with no reciprocal
 *   coordination benefit for them.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear and asymmetric.
 *   - Beneficiaries: `us_domestic_drone_manufacturers` and
 *     `us_national_security_agencies`. They gain market share and achieve policy
 *     goals, respectively. They have `arbitrage` or `constrained` exit options.
 *   - Victims: `dji_technology_co` and `us_drone_users`. They lose market
 *     access and economic surplus. Their `trapped` or `constrained` exit
 *     options lead to high `d` values, resulting in high effective
 *     extraction (χ).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the commercial beneficiary (US manufacturers)
 *   and the institutional enforcer (FCC). Both are beneficiaries and see a Rope.
 *   However, the FCC has `constrained` exit due to its public mandate and the
 *   political capital invested, while the manufacturers have `arbitrage` exit
 *   as they are free to exploit the newly protected market. This nuance is
 *   captured by the engine's directionality derivation without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial here. Labeling this a pure Snare
 *   (protectionism) would ignore the genuine (or at least claimed) coordination
 *   function around national security. Labeling it a pure Rope (security standard)
 *   would ignore the massive, asymmetric costs imposed on specific parties.
 *   The Tangled Rope classification correctly identifies it as a hybrid policy
 *   where a coordination goal is used to justify significant extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_fcc_dji_covered_list,
    'Is the national security threat posed by DJI drones substantial and unique, or is it primarily a pretext for industrial protectionism?',
    'Declassified intelligence assessments or verifiable, independent third-party hardware/software tear-downs that demonstrate data exfiltration capabilities.',
    'If the threat is real and severe, the coordination function is primary, validating the Tangled Rope classification. If the threat is minimal or pretextual, the constraint is functionally a Snare disguised as a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fcc_dji_covered_list, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time, moving from concern to active ban.
% Since base_extractiveness (0.52) > 0.46, temporal data is required.

% Theater ratio over time (remains low as actions are functional):
narrative_ontology:measurement(fcc_dji_covered_list_tr_t0, fcc_dji_covered_list, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fcc_dji_covered_list_tr_t5, fcc_dji_covered_list, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fcc_dji_covered_list_tr_t10, fcc_dji_covered_list, theater_ratio, 10, 0.20).

% Extraction over time (grows as restrictions become binding):
narrative_ontology:measurement(fcc_dji_covered_list_ex_t0, fcc_dji_covered_list, base_extractiveness, 0, 0.20). % t=0: Initial security concerns, minor market impact
narrative_ontology:measurement(fcc_dji_covered_list_ex_t5, fcc_dji_covered_list, base_extractiveness, 5, 0.35). % t=5: DoD ban, growing pressure
narrative_ontology:measurement(fcc_dji_covered_list_ex_t10, fcc_dji_covered_list, base_extractiveness, 10, 0.52).% t=10: Full FCC Covered List ban

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This regulation is a mechanism to enforce a specific security policy.
narrative_ontology:coordination_type(fcc_dji_covered_list, enforcement_mechanism).

% Network relationships (structural influence edges)
% This action is part of a broader strategy of technological decoupling from China.
narrative_ontology:affects_constraint(fcc_dji_covered_list, tech_decoupling_strategy). % Hypothetical parent constraint
narrative_ontology:affects_constraint(fcc_dji_covered_list, semiconductor_supply_chain_resilience).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The standard derivation chain,
% using the declared beneficiary/victim groups and the specified exit_options
% for each perspective, accurately models the directionality of the constraint
% for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */