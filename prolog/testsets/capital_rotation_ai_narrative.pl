% ============================================================================
% CONSTRAINT STORY: capital_rotation_ai_narrative
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-18
% ============================================================================

:- module(constraint_capital_rotation_ai_narrative, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: capital_rotation_ai_narrative
 *   human_readable: Market Narrative: US AI Stock Unsustainability
 *   domain: economic
 *
 * SUMMARY:
 *   A dominant market narrative positing the unsustainability of the US AI
 *   stock market rally. This narrative acts as a constraint on global fund
 *   managers, creating a coercive pressure to rotate capital out of US tech
 *   giants and into Asian technology firms perceived as undervalued AI
 *   supply-chain "winners".
 *
 * KEY AGENTS (by structural relationship):
 *   - US Tech-Centric Fund Managers: Primary target (moderate/constrained) — bears the cost of re-allocation and the career risk of defying the narrative.
 *   - Asian Tech Sector Investors: Primary beneficiary (institutional/arbitrage) — benefits from capital inflows driven by the narrative.
 *   - US Tech Stock Retail Investors: Secondary target (powerless/trapped) - may suffer losses from capital outflows they cannot easily arbitrage.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_rotation_ai_narrative, 0.48).
domain_priors:suppression_score(capital_rotation_ai_narrative, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(capital_rotation_ai_narrative, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, extractiveness, 0.48).
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(capital_rotation_ai_narrative, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(capital_rotation_ai_narrative). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, asian_tech_sector_investors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, us_tech_centric_fund_managers).
%
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

% PERSPECTIVE 1: THE PRIMARY TARGET (US Fund Manager)
% Agent who bears the most extraction via career risk and forced re-allocation.
% Engine derives d from victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42.
% χ = 0.48 * 1.42 * 1.2 (global scope) ≈ 0.82. This is a clear Snare (χ >= 0.66).
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (Asian Sector Investor)
% Agent who benefits most from capital inflows. Engine derives d from:
% beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12.
% χ = 0.48 * -0.12 * 1.2 (global scope) ≈ -0.07. This is a clear Rope (negative χ).
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees both coordination and extraction.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15.
% χ = 0.48 * 1.15 * 1.2 (global scope) ≈ 0.66. This meets the Tangled Rope
% threshold (0.40 <= χ <= 0.90), matching the base metrics.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: A MODERATE ACTOR (The Fund Manager as organized/mobile)
% A more nuanced view of the fund manager: not powerless, but constrained.
% Engine derives d from victim membership + mobile exit -> d ≈ 0.85 -> f(d) ≈ 1.15.
% χ = 0.48 * 1.15 * 1.2 (global scope) ≈ 0.66. Still a Snare, showing robustness.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_rotation_ai_narrative_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, rope, context(agent_power(institutional), _, _, _)),
    format('... Perspectival gap test passed (Snare vs Rope)\n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('... Analytical classification test passed (Tangled Rope)\n').

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, _),
    narrative_ontology:constraint_victim(capital_rotation_ai_narrative, _),
    domain_priors:requires_active_enforcement(capital_rotation_ai_narrative).

test(threshold_validation) :-
    narrative_ontology:constraint_metric(capital_rotation_ai_narrative, extractiveness, E),
    narrative_ontology:constraint_metric(capital_rotation_ai_narrative, suppression_requirement, S),
    E >= 0.30, % Tangled Rope extraction floor
    S >= 0.40. % Tangled Rope suppression floor

:- end_tests(capital_rotation_ai_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This score reflects the significant coercive power of the
 *     narrative. It's not a direct tax, but it forces the movement of billions of
 *     dollars and imposes substantial career risk on managers who dissent, which
 *     constitutes a form of extraction.
 *   - Suppression Score (0.65): The narrative strongly suppresses alternative
 *     investment strategies. In a momentum-driven market, ignoring such a
 *     dominant trend is professionally perilous, effectively foreclosing the
 *     option to "stay the course" in US tech without significant risk.
 *   - Enforcement: The constraint is enforced through decentralized market mechanisms:
 *     price signals, fund flow data, and performance benchmarks that punish laggards.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The beneficiary (Asian tech investors) sees a beneficial
 *   coordination signal directing capital towards them (Rope). The target
 *   (US-centric fund managers) experiences a coercive force that limits their
 *   autonomy and forces costly re-allocation to mitigate career risk (Snare).
 *   The analytical observer, seeing both the coordination function and the
 *   asymmetric costs, correctly identifies the hybrid nature of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: the narrative benefits the recipients of capital
 *   (asian_tech_sector_investors) at the expense of the source of capital
 *   (us_tech_centric_fund_managers and their underlying investors). The `victim`
 *   and `beneficiary` declarations directly map to this structural relationship,
 *   allowing the engine to correctly derive the high-d (target) and low-d
 *   (beneficiary) perspectives that create the classification gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. A pure Snare
 *   classification would miss the fact that this narrative provides a genuine
 *   coordination function for global capital seeking the "next big trade".
 *   A pure Rope classification would ignore the coercive, extractive pressure
 *   on managers and the clear asymmetry of winners and losers. The Tangled Rope
 *   classification correctly identifies it as a system with both coordination
 *   and extraction, preventing Mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_capital_rotation_ai,
    'Is this narrative a reflection of weakening US tech fundamentals, or is it a self-fulfilling speculative mania?',
    'Comparative earnings reports from US vs. Asian tech sectors over the next 8-12 quarters.',
    'If fundamentals are real, the constraint is closer to a pure Rope coordinating an efficient market shift. If its pure mania, it is a pure Snare setting up the next bubble.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(capital_rotation_ai_narrative, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This narrative is intensifying over the modeled interval (e.g., 24 months).
% Extraction accumulation is modeled as the narrative solidifies and the
% pressure to conform grows. Theater remains low as it's a functional constraint.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(cra_tr_t0, capital_rotation_ai_narrative, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cra_tr_t5, capital_rotation_ai_narrative, theater_ratio, 5, 0.12).
narrative_ontology:measurement(cra_tr_t10, capital_rotation_ai_narrative, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cra_ex_t0, capital_rotation_ai_narrative, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cra_ex_t5, capital_rotation_ai_narrative, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(cra_ex_t10, capital_rotation_ai_narrative, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The narrative acts as a standard for interpreting market data and coordinating action.
narrative_ontology:coordination_type(capital_rotation_ai_narrative, information_standard).

% Network relationships (structural influence edges)
% This narrative influences and is influenced by geopolitical constraints around tech supply chains.
narrative_ontology:affects_constraint(capital_rotation_ai_narrative, semiconductor_supply_chain_geopolitics).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain using beneficiary/victim declarations and exit options accurately
% models the structural relationships and produces the correct directionality
% values (d) for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */