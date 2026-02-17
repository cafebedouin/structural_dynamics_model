% ============================================================================
% CONSTRAINT STORY: gold_fomo_cycle
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_gold_fomo_cycle, []).

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
 *   constraint_id: gold_fomo_cycle
 *   human_readable: The Gold Price 'Fear of Missing Out' Cycle
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint models the market dynamic during a gold price rally where
 *   media hype and rapid price appreciation create a "fear of missing out"
 *   (FOMO) among retail investors. This influx of late-stage capital provides
 *   exit liquidity for earlier, more sophisticated institutional players and
 *   central banks, creating a structural wealth transfer from the former to
 *   the latter under the guise of a universal "safe haven" asset.
 *
 * KEY AGENTS (by structural relationship):
 *   - Retail Investors: Primary target (powerless/trapped) — bears extraction by buying high due to FOMO.
 *   - Institutional Traders & Central Banks: Primary beneficiary (institutional/arbitrage) — benefits from retail-driven liquidity and price momentum to secure profits or rebalance portfolios.
 *   - Financial Media: Secondary actor/enforcement mechanism — amplifies the narrative, accelerating the cycle.
 *   - Analytical Observer: Analytical observer — sees the full structure of information asymmetry and wealth transfer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fomo_cycle, 0.65).
domain_priors:suppression_score(gold_fomo_cycle, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(gold_fomo_cycle, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fomo_cycle, extractiveness, 0.65).
narrative_ontology:constraint_metric(gold_fomo_cycle, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(gold_fomo_cycle, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(gold_fomo_cycle, tangled_rope).
narrative_ontology:human_readable(gold_fomo_cycle, "The Gold Price 'Fear of Missing Out' Cycle").

% --- Binary flags ---
domain_priors:requires_active_enforcement(gold_fomo_cycle). % Required for Tangled Rope. The cycle is enforced by media narratives and broker marketing.

% --- Emergence flag ---
% This is a human-constructed market dynamic, not a natural law.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, institutional_traders).
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, central_banks).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(gold_fomo_cycle, retail_investors).

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

% PERSPECTIVE 1: THE RETAIL INVESTOR (PRIMARY TARGET)
% They are victims with trapped exit (selling means realizing a loss), leading
% to a high directionality (d ≈ 0.95) and high effective extraction (χ).
% They experience the dynamic as a high-stakes, coercive trap.
constraint_indexing:constraint_classification(gold_fomo_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))). % Gold market is global.

% PERSPECTIVE 2: THE INSTITUTIONAL TRADER (PRIMARY BENEFICIARY)
% They are beneficiaries with arbitrage exit, leading to a low/negative
% directionality (d ≈ 0.05) and low/negative effective extraction (χ).
% For them, the influx of retail money is a coordination mechanism that
% provides liquidity and confirms market trends.
constraint_indexing:constraint_classification(gold_fomo_cycle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the genuine coordination function for institutional players
% (gold as a hedge/portfolio diversifier) and the asymmetric extraction from
% retail investors. The combination of these two elements, requiring active
% enforcement via media narratives, is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fomo_cycle_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the retail investor (target)
    % and the institutional trader (beneficiary).
    constraint_indexing:constraint_classification(gold_fomo_cycle, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(gold_fomo_cycle, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify all three structural conditions for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(gold_fomo_cycle, _),
    narrative_ontology:constraint_victim(gold_fomo_cycle, _),
    domain_priors:requires_active_enforcement(gold_fomo_cycle).

:- end_tests(gold_fomo_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High, representing the significant potential for wealth transfer from late-cycle retail buyers to early institutional sellers. This isn't just transaction costs; it's the structural disadvantage of entering an already mature trend.
 *   - Suppression (0.70): High. The powerful, simple narrative of "gold is a safe haven against chaos" suppresses more nuanced, complex analyses (e.g., "gold is a non-yielding speculative asset whose price is driven by institutional flows"). The price action itself suppresses caution.
 *   - Enforcement: The cycle is actively enforced by financial media, which profits from engagement driven by dramatic market moves, and by brokerage platforms that benefit from increased trading volume.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the retail investor (powerless, trapped), the FOMO cycle is a Snare. They are lured in by the promise of safety and profit, only to find themselves providing exit liquidity for others. For the institutional trader (institutional, arbitrage), this same dynamic is a Rope. It's a predictable coordination signal; retail interest confirms the trend and provides the deep market needed to execute large trades without moving the price. What one sees as a trap, the other sees as a tool.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `institutional_traders` and `central_banks`. They have superior information, timing, and capital. They benefit from the price appreciation and the liquidity provided by retail inflows. Their `arbitrage` exit option gives them a low `d` value.
 *   - Victims: `retail_investors`. They have lagging information and are driven by emotion (FOMO). They bear the costs of buying near the peak. Their `trapped` exit status (psychological and financial barriers to selling at a loss) gives them a high `d` value.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It avoids calling the dynamic a pure Rope, which would legitimize the institutional perspective and ignore the severe, asymmetric extraction from retail participants. It also avoids calling it a pure Snare from all perspectives, which would miss the fact that for a certain class of actors, gold markets *do* serve a genuine, if cynical, coordination function. The Tangled Rope classification captures this duality: it is a system with a real coordination function that has been co-opted for asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_gold_fomo_cycle,
    'Is the current gold rally primarily driven by structural fundamentals (central bank de-dollarization, geopolitical risk) or by speculative momentum amplified by retail FOMO?',
    'Observing price stability and trading volumes after a significant de-escalation of geopolitical tensions or a shift in central bank policy.',
    'If primarily fundamental, the extraction (ε) is lower as prices are justified. If primarily speculative, the extraction is higher as a sharp correction is more likely.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gold_fomo_cycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is high-extraction (ε=0.65 > 0.46), so temporal data is required.
% The model assumes this dynamic has intensified over the last decade with the
% rise of social media-driven trading and frictionless retail platforms.

% Theater ratio over time: The narrative has become more performative.
narrative_ontology:measurement(gold_fomo_cycle_tr_t0, gold_fomo_cycle, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gold_fomo_cycle_tr_t5, gold_fomo_cycle, theater_ratio, 5, 0.35).
narrative_ontology:measurement(gold_fomo_cycle_tr_t10, gold_fomo_cycle, theater_ratio, 10, 0.40).

% Extraction over time: The efficiency of extraction has increased.
narrative_ontology:measurement(gold_fomo_cycle_ex_t0, gold_fomo_cycle, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(gold_fomo_cycle_ex_t5, gold_fomo_cycle, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(gold_fomo_cycle_ex_t10, gold_fomo_cycle, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Gold markets serve to allocate capital towards a non-sovereign store of value.
narrative_ontology:coordination_type(gold_fomo_cycle, resource_allocation).

% Network relationships (structural influence edges)
% The fear driving investors to gold is often linked to a loss of faith in
% conventional fiat currencies.
narrative_ontology:affects_constraint(fiat_currency_debasement_fear, gold_fomo_cycle).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain (beneficiary/victim + exit_options -> d) accurately captures the
% structural relationships and produces the required perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */