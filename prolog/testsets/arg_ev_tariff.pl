% ============================================================================
% CONSTRAINT STORY: arg_ev_tariff
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-23
% ============================================================================

:- module(constraint_arg_ev_tariff, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: arg_ev_tariff
 *   human_readable: Argentine Tariff on Chinese Electric Vehicles
 *   domain: economic/political
 *
 * SUMMARY:
 *   Argentina's government under Javier Milei is contemplating imposing
 *   tariffs on Chinese electric vehicles (EVs), mirroring similar protectionist
 *   measures by the US and EU. This policy serves a dual purpose: it aligns
 *   Argentina geopolitically with Western powers while simultaneously
 *   suppressing lower-cost competition to protect established Western
 *   automakers and potentially foster a domestic EV industry. The constraint
 *   is the tariff itself, a regulatory barrier that reallocates market access
 *   and consumer surplus.
 *
 * KEY AGENTS (by structural relationship):
 *   - Argentine Consumers: Primary target (powerless/trapped) — face higher prices and fewer affordable options.
 *   - Chinese EV Manufacturers: Primary target (organized/mobile) — face market exclusion and lost revenue.
 *   - US/EU Automakers: Primary beneficiary (institutional/arbitrage) — gain market share from suppressed competition.
 *   - Argentine State (Milei Govt): Architect & secondary beneficiary (institutional/constrained) — gains geopolitical alignment, but constrained by trade bloc (Mercosur) politics and economic realities.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arg_ev_tariff, 0.55).
domain_priors:suppression_score(arg_ev_tariff, 0.75).   % Structural property (raw, unscaled). High due to direct suppression of alternatives.
domain_priors:theater_ratio(arg_ev_tariff, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arg_ev_tariff, extractiveness, 0.55).
narrative_ontology:constraint_metric(arg_ev_tariff, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(arg_ev_tariff, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(arg_ev_tariff, tangled_rope).
narrative_ontology:human_readable(arg_ev_tariff, "Argentine Tariff on Chinese Electric Vehicles").
narrative_ontology:topic_domain(arg_ev_tariff, "economic/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(arg_ev_tariff). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(arg_ev_tariff, us_eu_automakers).
narrative_ontology:constraint_beneficiary(arg_ev_tariff, argentine_state_geopolitical).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(arg_ev_tariff, argentine_consumers).
narrative_ontology:constraint_victim(arg_ev_tariff, chinese_ev_manufacturers).
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
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (ARGENTINE CONSUMER)
% Bears the full cost of higher prices and reduced choice.
% Engine derives d from: victim membership + trapped exit → d≈0.95 → f(d)≈1.42 → high χ
% χ = 0.55 * 1.42 * 1.0 (national) ≈ 0.78. This exceeds the Snare threshold (χ≥0.66).
constraint_indexing:constraint_classification(arg_ev_tariff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US/EU AUTOMAKERS)
% Benefits from suppressed competition.
% Engine derives d from: beneficiary membership + arbitrage exit → d≈0.05 → f(d)≈-0.12 → negative χ
constraint_indexing:constraint_classification(arg_ev_tariff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the geopolitical coordination and the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.55 * 1.15 * 1.2 (global) ≈ 0.76. Meets Tangled Rope criteria (0.40 ≤ χ ≤ 0.90).
constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint operates differently on different institutional actors.

% PERSPECTIVE 4A: THE ARCHITECT (ARGENTINE STATE)
% An institutional beneficiary, but with constrained options.
% Engine derives d from: beneficiary + constrained exit → d≈0.25 → f(d)≈0.14
% χ = 0.55 * 0.14 * 1.0 (national) ≈ 0.08. Clearly a Rope from this view.
constraint_indexing:constraint_classification(arg_ev_tariff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: THE EXCLUDED COMPETITOR (CHINESE EV MANUFACTURERS)
% An organized victim, but with mobile exit (can sell elsewhere).
% Engine derives d from: victim + mobile exit → d≈0.85 → f(d)≈1.15
% χ = 0.55 * 1.15 * 1.0 (national) ≈ 0.63. Below Snare threshold but high,
% qualifying as a Tangled Rope as they see the geopolitical coordination but
% primarily experience the extraction.
constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arg_ev_tariff_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between consumer and western automaker.
    constraint_indexing:constraint_classification(arg_ev_tariff, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(arg_ev_tariff, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_inter_institutional) :-
    % Verify gap between the Argentine state and Chinese manufacturers.
    constraint_indexing:constraint_classification(arg_ev_tariff, rope, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope, context(agent_power(organized), _, exit_options(mobile), _)).

test(analytical_claim_matches_type) :-
    narrative_ontology:constraint_claim(arg_ev_tariff, tangled_rope),
    constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gates_pass) :-
    narrative_ontology:constraint_beneficiary(arg_ev_tariff, _),
    narrative_ontology:constraint_victim(arg_ev_tariff, _),
    domain_priors:requires_active_enforcement(arg_ev_tariff).

:- end_tests(arg_ev_tariff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect the significant price advantage of Chinese EVs, meaning a tariff to neutralize this advantage is highly extractive for consumers.
 *   - Suppression (0.75): The explicit goal of the tariff is to suppress a category of market actor, justifying a high score.
 *   - The combination of high extraction and high suppression, alongside a clear coordination function, makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For an Argentine consumer, the tariff is a pure Snare: it removes affordable options and raises prices with no direct benefit. For a Western automaker, it's a perfect Rope: it coordinates the market to their advantage, eliminating a key competitor at no cost to them. The Argentine state sees it as a Rope, a tool for achieving geopolitical alignment. The analytical view correctly identifies this hybrid nature as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `us_eu_automakers` gain direct market access. `argentine_state_geopolitical` benefits from aligning with powerful trade blocs, a non-monetary but structurally significant gain.
 *   - Victims: `argentine_consumers` pay the direct monetary cost. `chinese_ev_manufacturers` suffer the direct market exclusion. These declarations directly inform the directionality `d` for each perspective.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights how two institutional actors experience the same constraint differently. The Argentine state (architect) sees it as a coordination tool (Rope) because their exit is `constrained` by geopolitical pressures. Chinese manufacturers (target) see it as a coercive, extractive barrier (Tangled Rope) because while they are `mobile` and can sell elsewhere, they are the direct target of the extraction within this scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not label the tariff as a pure Snare, which would ignore its genuine (if debatable) function in geopolitical coordination. It also avoids labeling it as a benign Rope, which would ignore the significant, quantifiable extraction imposed on consumers and Chinese firms. The Tangled Rope classification captures this essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_arg_ev_tariff,
    'Will the tariff successfully foster a competitive domestic or regional (Mercosur) EV industry?',
    'Empirical observation of investment flows, factory construction, and market share of domestic/regional EVs over a 5-10 year period.',
    'If YES, the constraint has a stronger long-term coordination function, potentially reducing its perceived extractiveness over a generational timescale. If NO, it solidifies its status as a pure transfer of wealth from consumers to foreign (Western) corporations, making the Snare perspective dominant.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(arg_ev_tariff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), so temporal data
% is required to model its implementation over time.
% We model the period from pre-policy (T=0) to full implementation (T=5).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(arg_ev_tariff_tr_t0, arg_ev_tariff, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arg_ev_tariff_tr_t5, arg_ev_tariff, theater_ratio, 5, 0.20).
narrative_ontology:measurement(arg_ev_tariff_tr_t10, arg_ev_tariff, theater_ratio, 10, 0.20).

% Extraction over time (ramps up as tariff is implemented):
narrative_ontology:measurement(arg_ev_tariff_ex_t0, arg_ev_tariff, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(arg_ev_tariff_ex_t5, arg_ev_tariff, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(arg_ev_tariff_ex_t10, arg_ev_tariff, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The tariff primarily functions to allocate market share.
narrative_ontology:coordination_type(arg_ev_tariff, resource_allocation).

% Network relationships: This policy is a node in a larger network of
% geopolitical and trade conflicts.
narrative_ontology:affects_constraint(us_china_trade_war, arg_ev_tariff).
narrative_ontology:affects_constraint(eu_protectionism, arg_ev_tariff).
narrative_ontology:affects_constraint(arg_ev_tariff, mercosur_trade_policy).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The default structural derivation
% (beneficiary/victim + exit_options -> d) accurately captures the
% perspectival differences between the consumers, western automakers,
% the Argentine state, and Chinese manufacturers.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */