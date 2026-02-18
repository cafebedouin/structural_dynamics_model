% ============================================================================
% CONSTRAINT STORY: wpl_scotland
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_wpl_scotland, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wpl_scotland
 *   human_readable: Scotland's Workplace Parking Levy (WPL)
 *   domain: economic
 *
 * SUMMARY:
 *   Scottish councils have been given the power to implement a Workplace Parking
 *   Levy (WPL), a charge on employers for providing parking spaces. Employers can
 *   pass this cost onto their staff. The stated goal is to reduce car dependency,
 *   cut emissions, and raise funds for public transport, but it imposes a direct
 *   financial burden on commuters who drive.
 *
 * KEY AGENTS (by structural relationship):
 *   - Car Commuters: Primary target (powerless/trapped) — bear the direct cost of the levy, especially low-income and shift workers with few alternatives.
 *   - Local Councils: Primary beneficiary (institutional/arbitrage) — receive the revenue and political credit for environmental action.
 *   - Large Employers: Secondary actor (organized/constrained) - targeted by the levy but can act as a pass-through entity to their employees.
 *   - Analytical Observer: A policy analyst — sees the dual nature of the policy as both a coordination mechanism and an extractive tool.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wpl_scotland, 0.52). % A significant annual cost relative to the benefit of "free" parking.
domain_priors:suppression_score(wpl_scotland, 0.65).   % High suppression for those lacking viable public transport alternatives.
domain_priors:theater_ratio(wpl_scotland, 0.15).       % Assumes genuine intent to fund public transport, not just raise revenue.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wpl_scotland, extractiveness, 0.52).
narrative_ontology:constraint_metric(wpl_scotland, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(wpl_scotland, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(wpl_scotland, tangled_rope).
narrative_ontology:human_readable(wpl_scotland, "Scotland's Workplace Parking Levy (WPL)").
narrative_ontology:topic_domain(wpl_scotland, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(wpl_scotland). % Councils must monitor employers and collect the levy.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(wpl_scotland, local_councils).
narrative_ontology:constraint_beneficiary(wpl_scotland, public_transport_users).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(wpl_scotland, car_commuters).
narrative_ontology:constraint_victim(wpl_scotland, low_income_shift_workers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all met)

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
% A low-income worker with no public transport options for their shift times.
% Engine derives d from: victim membership + trapped exit → d≈0.95 → f(d)≈1.42
% χ = 0.52 * 1.42 * σ(regional=0.9) ≈ 0.664. This exceeds the Snare threshold (χ≥0.66).
constraint_indexing:constraint_classification(wpl_scotland, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The local council implementing the policy.
% Engine derives d from: beneficiary membership + arbitrage exit → d≈0.05 → f(d)≈-0.12
% χ = 0.52 * -0.12 * σ(regional=0.9) ≈ -0.056. A negative χ is a clear Rope.
constraint_indexing:constraint_classification(wpl_scotland, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.52 * 1.15 * σ(global=1.2) ≈ 0.718. This χ is in the Tangled Rope
% range (0.40 ≤ χ ≤ 0.90), and the constraint has the required structural
% properties (coordination and extraction).
constraint_indexing:constraint_classification(wpl_scotland, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wpl_scotland_tests).

test(perspectival_gap) :-
    % Verify the massive gap between the target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(wpl_scotland, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(wpl_scotland, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(wpl_scotland, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_conditions_met) :-
    % Verify all three structural conditions for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(wpl_scotland, _),
    narrative_ontology:constraint_victim(wpl_scotland, _),
    domain_priors:requires_active_enforcement(wpl_scotland).

:- end_tests(wpl_scotland_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): Represents a substantial financial burden, high enough to change behavior and qualify as significant extraction.
 *   - Suppression (0.65): Reflects the lack of viable alternatives for many workers, particularly those outside urban centers or working unsocial hours. The policy actively suppresses the choice of free workplace parking.
 *   - The combination of a genuine coordination function (funding public transit, reducing congestion) and high, asymmetric extraction makes this a textbook Tangled Rope from an analytical perspective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For a council (`institutional`/`arbitrage`), it's a pure coordination tool (Rope) that generates revenue for public goods with costs externalized to others. For a trapped commuter (`powerless`/`trapped`), it's a pure coercive extraction (Snare) with no discernible benefit, just a new tax on their livelihood. This gap is the defining feature of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Costs flow from car commuters to the local council. The `constraint_beneficiary` and `constraint_victim` declarations directly model this flow. Commuters are victims because they bear the cost. Councils are beneficiaries because they receive the revenue and control the policy's implementation and revenue allocation. This structural data correctly drives the `d` derivation, producing the Snare/Rope gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors.
 *   1. It does not mistake the policy for a pure Rope, which would ignore the severe, coercive costs imposed on a specific, often vulnerable, population.
 *   2. It does not mistake it for a pure Snare, which would ignore the genuine, if contested, public coordination goal. The Tangled Rope classification correctly identifies that the policy *simultaneously* serves a coordination function while being highly extractive, preventing the policy's stated purpose from masking its real-world impact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_wpl_scotland,
    'Will the revenue generated by the WPL be effectively reinvested into public transport improvements that actually benefit the commuters paying the levy?',
    'Long-term (5-10 year) audits of council spending, correlated with public transport usage and satisfaction surveys in affected areas.',
    'If YES, the Tangled Rope classification holds. If NO (revenue is diverted to general funds or ineffective projects), the constraint degrades towards a pure Snare as its coordination function becomes purely theatrical.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(wpl_scotland, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε > 0.46), so temporal data is required.
% We model the levy starting as a proposal and becoming more concrete and extractive.
% Theater may creep up if promised benefits fail to materialize over the interval.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(wpl_scotland_tr_t0, wpl_scotland, theater_ratio, 0, 0.10).
narrative_ontology:measurement(wpl_scotland_tr_t5, wpl_scotland, theater_ratio, 5, 0.12).
narrative_ontology:measurement(wpl_scotland_tr_t10, wpl_scotland, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(wpl_scotland_ex_t0, wpl_scotland, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(wpl_scotland_ex_t5, wpl_scotland, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(wpl_scotland_ex_t10, wpl_scotland, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy is a mechanism for funding and allocating resources.
narrative_ontology:coordination_type(wpl_scotland, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain, based on the declared beneficiary/victim groups and exit options,
% accurately models the structural relationships and produces the correct
% perspectival classifications.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */