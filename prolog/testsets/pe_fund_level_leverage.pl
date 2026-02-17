% ============================================================================
% CONSTRAINT STORY: pe_fund_level_leverage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_pe_fund_level_leverage, []).

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
 *   constraint_id: pe_fund_level_leverage
 *   human_readable: "Shadow Leverage" via fund-level debt in Private Equity
 *   domain: economic
 *
 * SUMMARY:
 *   Private equity firms (General Partners, GPs) are increasingly using
 *   "fund-level leverage," such as Net Asset Value (NAV) loans, to borrow
 *   against the entire portfolio of a fund. While presented as a tool for
 *   liquidity and value enhancement, this practice introduces opaque, systemic
 *   risk for investors (Limited Partners, LPs), magnifies GP fees, and
 *   transforms the risk profile of the entire fund without direct LP consent
 *   on each transaction.
 *
 * KEY AGENTS (by structural relationship):
 *   - Fund Limited Partners (LPs): Primary target (powerless/trapped) — bear the magnified, opaque risk of default and forced asset sales.
 *   - PE General Partners (GPs): Primary beneficiary (institutional/arbitrage) — benefit from increased management fees, greater chance of performance fees, and enhanced operational flexibility.
 *   - Fund-Level Lenders (Credit Funds): Secondary beneficiary (institutional/arbitrage) — benefit by originating secured, high-yield loans.
 *   - Regulators (e.g., SEC): Inter-institutional actor (institutional/constrained) — tasked with oversight but constrained by industry complexity and lobbying, often reacting after risks have materialized.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pe_fund_level_leverage, 0.68).
domain_priors:suppression_score(pe_fund_level_leverage, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(pe_fund_level_leverage, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pe_fund_level_leverage, extractiveness, 0.68).
narrative_ontology:constraint_metric(pe_fund_level_leverage, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(pe_fund_level_leverage, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pe_fund_level_leverage, tangled_rope).
narrative_ontology:human_readable(pe_fund_level_leverage, "\"Shadow Leverage\" via fund-level debt in Private Equity").

% --- Binary flags ---
domain_priors:requires_active_enforcement(pe_fund_level_leverage). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(pe_fund_level_leverage, pe_general_partners).
narrative_ontology:constraint_beneficiary(pe_fund_level_leverage, fund_level_lenders).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(pe_fund_level_leverage, fund_limited_partners).
narrative_ontology:constraint_victim(pe_fund_level_leverage, regulators). % Victims of complexity/capture

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

% PERSPECTIVE 1: THE PRIMARY TARGET (LIMITED PARTNERS)
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.68 * 1.42 * 1.0 (national scope) ≈ 0.96. This is a clear Snare.
constraint_indexing:constraint_classification(pe_fund_level_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (GENERAL PARTNERS)
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.68 * -0.12 * 1.0 (national scope) ≈ -0.08. A pure coordination Rope.
constraint_indexing:constraint_classification(pe_fund_level_leverage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes both the coordination function and the severe asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.68 * 1.15 * 1.2 (global scope) ≈ 0.94. Very high extraction, but with a
% coordination function present, it classifies as Tangled Rope.
constraint_indexing:constraint_classification(pe_fund_level_leverage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: REGULATORS
% An institutional actor, but their exit is constrained and they are victims
% of regulatory complexity and capture.
% Engine derives d from: victim + constrained exit → d ≈ 0.65 -> f(d) ≈ 1.0.
% χ = 0.68 * 1.0 * 1.0 (national scope) ≈ 0.68. This is a Tangled Rope,
% reflecting their awareness of the problem's dual nature.
constraint_indexing:constraint_classification(pe_fund_level_leverage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: FUND-LEVEL LENDERS (Credit Funds)
% An institutional beneficiary, similar to GPs. They see a valuable financial product.
% Engine derives d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.68 * -0.12 * 1.0 (national scope) ≈ -0.08. This is a Rope.
constraint_indexing:constraint_classification(pe_fund_level_leverage, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pe_fund_level_leverage_tests).

test(perspectival_gap_target_vs_beneficiary, [nondet]) :-
    % Verify the core perspectival gap between LPs and GPs.
    constraint_indexing:constraint_classification(pe_fund_level_leverage, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(pe_fund_level_leverage, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap_gp_vs_regulator, [nondet]) :-
    % Verify that two institutional actors classify differently due to exit options.
    constraint_indexing:constraint_classification(pe_fund_level_leverage, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(pe_fund_level_leverage, tangled_rope,
        context(agent_power(institutional), _, exit_options(constrained), _)).

test(analytical_classification_is_tangled_rope) :-
    % The canonical analytical classification must be Tangled Rope.
    narrative_ontology:constraint_claim(pe_fund_level_leverage, tangled_rope).

:- end_tests(pe_fund_level_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.68): High, reflecting multiple extraction channels: inflated management fees on a leveraged asset base, higher probability of achieving performance fees ("carry"), and the strategic value of controlling liquidity timing.
 *   - Suppression Score (0.75): High. LPs' capital is locked for 10+ years, partnership agreements grant GPs broad discretion, and information asymmetry is severe. There is no viable alternative or exit for an LP within an existing fund.
 *   - Theater Ratio (0.40): Moderate. The practice is framed as value-enhancing coordination (providing liquidity to return capital), and it does have that function. However, the primary driver is GP-centric financial engineering, creating a significant gap between the stated and actual purpose.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the GP (beneficiary), this is a sophisticated coordination tool (a Rope) to optimize fund performance. For the LP (target), it is an opaque mechanism that transfers risk to them without their consent, making their investment illiquid AND highly leveraged (a Snare). The LP is trapped by the fund's legal structure, while the GP enjoys arbitrage optionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is defined by the flow of risk and fees.
 *   - Beneficiaries: General Partners and Lenders. GPs receive higher fees and greater control. Lenders receive returns on secured loans. They are members of the `constraint_beneficiary` group.
 *   - Victims: Limited Partners. They unknowingly bear subordinated, magnified risk. Their claim on assets is now behind that of the new lenders. They are the clear members of the `constraint_victim` group.
 *   This structural relationship directly informs the `d` value calculation, creating the stark Snare/Rope divide.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the GP/Lender and the Regulator, even though all are `institutional`. The GP/Lender has `arbitrage` exit; they can choose to engage in this market or not. The Regulator has a `constrained` exit; they are mandated to oversee this market and cannot simply walk away. This difference in exit optionality, combined with their differing structural relationships (beneficiary vs. victim-of-complexity), correctly yields different classifications (Rope vs. Tangled Rope), capturing the real-world dynamic of a regulator who sees the problem but is slow or unable to resolve it.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a quintessential Tangled Rope, which prevents misclassification. A pure Snare analysis would miss the genuine (though often exaggerated) coordination function of providing fund liquidity. A pure Rope analysis, which is how the GPs frame it, would ignore the massive, non-consensual transfer of risk and extraction of value. The Tangled Rope classification correctly identifies that a coordination mechanism has been co-opted for asymmetric extraction, which is the core of the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_pe_fund_level_leverage,
    'Does fund-level leverage, on average and over a full market cycle, produce net-positive risk-adjusted returns for Limited Partners?',
    'Empirical analysis of PE fund performance through a significant downturn (e.g., 2026-2028), comparing returns and default rates of leveraged vs. unleveraged funds from the same vintage.',
    'If true, the constraint is less extractive (lower ε) than modeled. If false, the constraint is even more extractive and purely a Snare from almost all non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pe_fund_level_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This practice has evolved from a niche tool to a widespread financial
% engineering technique, showing clear extraction_accumulation drift.
% T=0 represents its early, limited use.
% T=10 represents the current widespread, systemic state.

% Theater ratio over time:
narrative_ontology:measurement(pe_fund_level_leverage_tr_t0, pe_fund_level_leverage, theater_ratio, 0, 0.20).
narrative_ontology:measurement(pe_fund_level_leverage_tr_t5, pe_fund_level_leverage, theater_ratio, 5, 0.35).
narrative_ontology:measurement(pe_fund_level_leverage_tr_t10, pe_fund_level_leverage, theater_ratio, 10, 0.40).

% Extraction over time:
narrative_ontology:measurement(pe_fund_level_leverage_ex_t0, pe_fund_level_leverage, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(pe_fund_level_leverage_ex_t5, pe_fund_level_leverage, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(pe_fund_level_leverage_ex_t10, pe_fund_level_leverage, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint functions as a mechanism to allocate capital and liquidity
% within and between financial actors.
narrative_ontology:coordination_type(pe_fund_level_leverage, resource_allocation).

% Network relationships (structural influence edges)
% The risks created by this constraint directly affect the stability of the
% private credit market that provides the loans.
narrative_ontology:affects_constraint(pe_fund_level_leverage, private_credit_default_risk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation chain,
% using the declared beneficiary/victim groups and the specified exit_options
% for each perspective, accurately models the directionality of the system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */