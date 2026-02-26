% ============================================================================
% CONSTRAINT STORY: capital_rotation_ai_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-19
% Status: [ACTIVE]
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
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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
 *   A dominant market narrative emerged in 2025-2026 positing the
 *   unsustainability of the US AI stock market rally. This narrative, fueled
 *   by concerns over valuation, circular investments, and unrealized
 *   productivity gains, acts as a powerful constraint on market behavior. It
 *   coordinates a massive rotation of capital from AI-focused tech stocks to
 *   other sectors. This process is not neutral; it creates clear winners
 *   (short-sellers, active managers, non-AI sectors) and losers (retail
 *   investors holding AI stocks, AI companies seeking capital). The
 *   constraint is the narrative itself, which functions as both a
 *   price-discovery mechanism and an engine of wealth transfer.
 *
 * KEY AGENTS:
 *   - Retail Investors (long AI): Primary victims (powerless/trapped) — often the last to react to the narrative shift, incurring significant losses.
 *   - Short-Selling Hedge Funds: Primary beneficiaries (institutional/arbitrage) — profit directly from the price declines coordinated by the narrative.
 *   - AI Companies: Secondary victims (powerful/constrained) — face increased cost of capital and pressure on their valuations.
 *   - Non-AI Sectors: Secondary beneficiaries (powerful/mobile) — receive the inflow of rotated capital.
 *   - Financial Media: Institutional enforcers/beneficiaries (institutional/arbitrage) — amplify and sustain the narrative, benefiting from increased engagement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_rotation_ai_narrative, 0.65).
domain_priors:suppression_score(capital_rotation_ai_narrative, 0.7).
domain_priors:theater_ratio(capital_rotation_ai_narrative, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, extractiveness, 0.65).
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_rotation_ai_narrative, tangled_rope).
narrative_ontology:human_readable(capital_rotation_ai_narrative, "Market Narrative: US AI Stock Unsustainability").
narrative_ontology:topic_domain(capital_rotation_ai_narrative, "economic").

domain_priors:requires_active_enforcement(capital_rotation_ai_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, short_sellers).
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, active_fund_managers).
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, non_ai_sectors).
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, financial_media_outlets).
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, retail_investors_long_ai).
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, ai_companies_needing_capital).
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, employees_with_stock_options).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped by the narrative, often buying high and selling low. Lacks the tools for sophisticated hedging or timely exit, bearing the full cost of the capital rotation. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SHORT-SELLER (ROPE) — Benefits directly from the narrative, which acts as a coordination mechanism to drive prices down. Can enter and exit positions freely to maximize profit. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Negative effective extraction indicates a net subsidy.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: AI COMPANY EXECUTIVE (TANGLED ROPE) — Experiences the market as both a source of capital (coordination) and a hostile force driven by the narrative (extraction). Constrained from exiting and must manage company strategy in response to the narrative's pressure. d≈0.60, f(d)≈0.88, σ=1.2 → χ≈0.69.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-AI SECTOR (ROPE) — A primary beneficiary of the capital rotation. The narrative acts as a pure coordination signal to reallocate capital towards their sector, boosting valuations. Experiences no meaningful extraction. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the legitimate price-discovery function of the market narrative (coordination) and the severe, asymmetric wealth transfer it facilitates (extraction). The system is a hybrid, actively enforced by media and influential investors. This is the basis for the constraint's claimed_type. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_rotation_ai_narrative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_rotation_ai_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(capital_rotation_ai_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high because the narrative directly facilitates a large-scale wealth transfer from one group of investors to another. It is not merely informational; it is causally effective in moving prices and creating losses for the unprepared. Suppression (0.70) is high because once a market narrative of this magnitude takes hold, it is reinforced by herd behavior, media cycles, and analyst reports, making counter-narratives difficult to sustain. Theater (0.40) is moderate; while based on some fundamental concerns, the narrative is amplified by performative analysis and media hype designed to capture attention.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a hedge fund with arbitrage capability, the narrative is a pure Rope—a tool for coordinating a profitable trade. For a retail investor who bought into the AI hype and is now trapped in losing positions, it is a Snare—an inescapable trap that extracts their wealth. For an AI executive, it is a Tangled Rope—the very market that funds their innovation (coordination) is now actively working against their valuation (extraction). This demonstrates how the same economic phenomenon is classified differently based on an agent's structural ability to profit from or be harmed by the information flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries like short-sellers have arbitrage exit options, leading to a low derived directionality (d) and negative effective extraction (χ), classifying the constraint as a Rope from their view. Victims like retail investors are trapped, leading to a high d and a high χ, classifying it as a Snare. The analytical perspective acknowledges both the coordination and extraction functions, resulting in the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint story resolves the mandatrophy of labeling complex market dynamics. It avoids simplistic labels like 'efficient market' (a false Rope) or 'outright manipulation' (a simple Snare). By using indexical classification, it correctly models the phenomenon as a Tangled Rope from an analytical viewpoint: a system with a genuine coordination function (price discovery, capital reallocation) that is deeply intertwined with a severe, asymmetric extractive function (wealth transfer). The 'correct' classification depends entirely on the observer's structural position within the capital flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_vs_fundamental,
    'Is the market correction driven primarily by the self-fulfilling narrative, or by a genuine reassessment of AI''s fundamental economic impact?',
    'Time-series analysis comparing productivity data, corporate earnings, and capital expenditure against market sentiment indicators and media coverage.',
    'If driven by fundamentals, the constraint is closer to a Rope (efficient market). If primarily narrative-driven, it confirms the Snare/Tangled Rope classification (wealth transfer via information control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_vs_fundamental, empirical, 'Distinguishing between a narrative-driven correction and a fundamental-based repricing.').

omega_variable(
    circular_investment_exposure,
    'To what extent are AI valuations propped up by circular investments between major tech firms and the AI startups they fund?',
    'Forensic accounting and network analysis of investment flows between large cap tech companies and the AI ecosystem.',
    'High exposure would indicate the underlying valuations are fragile, making the ''unsustainability'' narrative a more accurate reflection of reality and increasing the system''s intrinsic extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circular_investment_exposure, empirical, 'Quantifying the impact of circular investments on AI market valuations.').

omega_variable(
    regulatory_intervention_timing,
    'Will financial regulators intervene to curb speculative behavior or address market concentration, and if so, when?',
    'Monitoring of policy proposals, regulatory body statements (e.g., SEC, Fed), and legislative action.',
    'Significant intervention could transform the constraint into a Scaffold by imposing temporary limits, while inaction allows the Tangled Rope/Snare dynamics to persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_timing, preference, 'The uncertainty of regulatory action to mitigate market volatility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_rotation_ai_narrative, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capi_tr_t0, capital_rotation_ai_narrative, theater_ratio, 0, 0.25).
narrative_ontology:measurement(capi_tr_t5, capital_rotation_ai_narrative, theater_ratio, 5, 0.35).
narrative_ontology:measurement(capi_tr_t10, capital_rotation_ai_narrative, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(capi_be_t0, capital_rotation_ai_narrative, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(capi_be_t5, capital_rotation_ai_narrative, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(capi_be_t10, capital_rotation_ai_narrative, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_rotation_ai_narrative, resource_allocation).
narrative_ontology:affects_constraint(capital_rotation_ai_narrative, semiconductor_supply_chain_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
