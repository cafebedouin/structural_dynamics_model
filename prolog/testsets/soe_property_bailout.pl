% ============================================================================
% CONSTRAINT STORY: soe_property_bailout
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_soe_property_bailout, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: soe_property_bailout
 *   human_readable: State-Directed Purchase of Distressed Real Estate Assets
 *   domain: economic
 *
 * SUMMARY:
 *   A state-directed policy in China where State-Owned Enterprises (SOEs)
 *   are instructed to purchase foreclosed or distressed properties from
 *   failing private developers. The stated goal is to stabilize a declining
 *   real estate market, prevent a systemic collapse, complete unfinished
 *   projects, and convert some properties to affordable housing.
 *
 * KEY AGENTS (by structural relationship):
 *   - Failed Private Developers: Primary target (powerless/trapped) — forced to sell assets at distressed prices.
 *   - The State/CCP: Primary beneficiary (institutional/arbitrage) — achieves market stability and political control.
 *   - State-Owned Enterprises: Secondary beneficiary/instrument (institutional/constrained) — executes the policy, gaining assets and market share.
 *   - Chinese Taxpayers: Indirect victim (powerless/trapped) — bear the ultimate financial risk of the socialized losses.
 *   - Analytical Observer: Analytical observer — sees both the coordination and extraction functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soe_property_bailout, 0.55).
domain_priors:suppression_score(soe_property_bailout, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(soe_property_bailout, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soe_property_bailout, extractiveness, 0.55).
narrative_ontology:constraint_metric(soe_property_bailout, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(soe_property_bailout, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(soe_property_bailout, tangled_rope).
narrative_ontology:human_readable(soe_property_bailout, "State-Directed Purchase of Distressed Real Estate Assets").

% --- Binary flags ---
domain_priors:requires_active_enforcement(soe_property_bailout). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(soe_property_bailout, chinese_state_apparatus).
narrative_ontology:constraint_beneficiary(soe_property_bailout, state_owned_enterprises).
narrative_ontology:constraint_beneficiary(soe_property_bailout, state_owned_banks).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(soe_property_bailout, failed_private_developers).
narrative_ontology:constraint_victim(soe_property_bailout, chinese_taxpayers).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (FAILED PRIVATE DEVELOPERS)
% Agent who bears the most extraction. They are forced sellers with no other viable
% buyers, making them powerless and trapped.
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.55 * 1.42 * 1.0 (national scope) ≈ 0.78. This is a clear Snare (χ > 0.66).
constraint_indexing:constraint_classification(soe_property_bailout, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE STATE/CCP)
% The state designs and benefits from the policy, seeking stability. It has full
% freedom to alter or end the policy (arbitrage exit).
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
%   χ = 0.55 * -0.12 * 1.0 ≈ -0.066. This is a pure coordination Rope.
constraint_indexing:constraint_classification(soe_property_bailout, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the vital coordination function and the high asymmetric extraction.
%   canonical d ≈ 0.72 → f(d) ≈ 1.15
%   χ = 0.55 * 1.15 * 1.2 (global scope) ≈ 0.76. Meets Tangled Rope χ criteria.
%   The structural elements (beneficiary, victim, enforcement) confirm Tangled Rope.
constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% State-Owned Enterprises (SOEs) are institutional actors but are instruments
% of the state, not the architects. Their exit is constrained by political mandate.
%   beneficiary membership + constrained exit → d ≈ 0.15 → f(d) ≈ -0.01
%   χ = 0.55 * -0.01 * 1.0 ≈ -0.0055. They also perceive it as a Rope,
%   but their derived directionality is less favorable than the state's,
%   reflecting their role as an instrument rather than the ultimate beneficiary.
constraint_indexing:constraint_classification(soe_property_bailout, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soe_property_bailout_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    % Verify the core perspectival gap between the trapped target and the architect.
    constraint_indexing:constraint_classification(soe_property_bailout, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(soe_property_bailout, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical view must correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(soe_property_bailout, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(soe_property_bailout, _),
    narrative_ontology:constraint_victim(soe_property_bailout, _),
    domain_priors:requires_active_enforcement(soe_property_bailout).

:- end_tests(soe_property_bailout_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.55): High, reflecting the non-market, distressed prices forced upon private developers and the socialization of risk onto taxpayers.
 *   - Suppression Score (0.75): Very high. The policy explicitly crowds out private buyers and suppresses normal market price discovery for distressed assets.
 *   - Theater Ratio (0.40): Moderate. While the policy has a genuine function, there is a significant performative aspect of "managing the crisis" for political stability.
 *   The combination of a genuine coordination function (market stabilization) with high, asymmetric extraction and active enforcement makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For failed private developers (powerless, trapped), the policy is a pure Snare. Their assets are extracted with no recourse and the coordination benefit is abstract and irrelevant to their situation. For the state (institutional, arbitrage), it is a pure Rope—a tool of economic coordination that costs them nothing directly and achieves a primary political goal (stability). The value of χ flips from ~0.78 to ~ -0.07 across these two perspectives, a classic sign of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `chinese_state_apparatus` benefits from systemic stability and control. `state_owned_enterprises` and `state_owned_banks` benefit by acquiring assets and avoiding a cascade of defaults.
 *   - Victims: `failed_private_developers` are the direct victims, losing their assets. `chinese_taxpayers` are the indirect victims, as they underwrite the risk transferred to SOE balance sheets.
 *   This clear division drives the directionality `d`, leading to the stark perspectival classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It does not mislabel the policy as a pure Snare, which would ignore its undeniable (and arguably necessary) function of preventing a financial meltdown. It also does not mislabel it as a benign Rope, which would ignore the coercive extraction imposed on developers and the immense risk transferred to the public. The Tangled Rope classification captures this duality: a necessary coordination achieved through highly extractive means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_soe_property_bailout,
    'Is the SOE asset purchase program a temporary stabilization measure (Scaffold-like) or a permanent shift towards state control of the housing market (entrenched Tangled Rope)?',
    'Observing whether the policy is wound down after 5-10 years as the market stabilizes, or if it becomes an institutionalized feature of the economic landscape.',
    'If temporary, the constraint could be re-evaluated as a successful, albeit extractive, Scaffold. If permanent, it represents a long-term, high-extraction Tangled Rope that fundamentally alters market structure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(soe_property_bailout, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), requiring temporal data.
% We model a scenario where the policy starts as a targeted emergency measure
% and becomes more systematized and extractive over time.

% Theater ratio over time (performative aspects grow as it becomes policy):
narrative_ontology:measurement(soe_property_bailout_tr_t0, soe_property_bailout, theater_ratio, 0, 0.20).
narrative_ontology:measurement(soe_property_bailout_tr_t5, soe_property_bailout, theater_ratio, 5, 0.35).
narrative_ontology:measurement(soe_property_bailout_tr_t10, soe_property_bailout, theater_ratio, 10, 0.40).

% Extraction over time (initial deals may be less harsh, becomes more efficient):
narrative_ontology:measurement(soe_property_bailout_ex_t0, soe_property_bailout, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(soe_property_bailout_ex_t5, soe_property_bailout, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(soe_property_bailout_ex_t10, soe_property_bailout, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy is a textbook example of state-directed resource allocation.
narrative_ontology:coordination_type(soe_property_bailout, resource_allocation).

% Network relationships (structural influence edges)
% This policy is deeply coupled with other major constraints in China's political economy.
narrative_ontology:affects_constraint(soe_property_bailout, local_government_debt).
narrative_ontology:affects_constraint(soe_property_bailout, common_prosperity_policy).
narrative_ontology:affects_constraint(soe_property_bailout, private_sector_capital_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation based
% on beneficiary/victim declarations and exit options accurately models the
% structural relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */