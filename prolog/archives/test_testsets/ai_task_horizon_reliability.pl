% ============================================================================
% CONSTRAINT STORY: ai_task_horizon_reliability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_task_horizon_reliability, []).

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
 *   constraint_id: ai_task_horizon_reliability
 *   human_readable: The AI Task Horizon and Reliability Bottleneck
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint describes the structural limit on current AI systems,
 *   where reliability collapses as task complexity and duration (the 'task
 *   horizon') increase. This creates a bottleneck for economic deployment, as
 *   the most valuable tasks are often the most complex. The system generates
 *   both real productivity gains on short-horizon tasks and significant
 *   extractive costs from misapplication, wasted investment, and labor market
 *   disruption on long-horizon tasks. The constraint is 'enforced' by intense
 *   market pressure and capital allocation, compelling adoption even where
 *   reliability is insufficient.
 *
 * KEY AGENTS:
 *   - AI Developers/Platform Owners: Primary beneficiaries (institutional/arbitrage) - Capture value from compute and subscriptions.
 *   - Top-Quartile Adopters: Secondary beneficiaries (powerful/mobile) - Use AI strategically as a temporary scaffold for competitive advantage.
 *   - Deskilled Workforce: Primary victims (powerless/trapped) - Face job insecurity and skill atrophy from mandated use of unreliable tools.
 *   - Median Adopters/Laggards: Secondary victims (moderate/constrained) - Invest heavily due to market pressure but fail to achieve positive ROI due to the reliability bottleneck.
 *   - Hype-Cycle Investors: Participants (powerful/mobile) - Drive the enforcement mechanism through capital allocation, often based on performative metrics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_task_horizon_reliability, 0.48).
domain_priors:suppression_score(ai_task_horizon_reliability, 0.62).
domain_priors:theater_ratio(ai_task_horizon_reliability, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_task_horizon_reliability, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_task_horizon_reliability, tangled_rope).
narrative_ontology:human_readable(ai_task_horizon_reliability, "The AI Task Horizon and Reliability Bottleneck").
narrative_ontology:topic_domain(ai_task_horizon_reliability, "technological/economic").

domain_priors:requires_active_enforcement(ai_task_horizon_reliability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_task_horizon_reliability, ai_developers_and_platform_owners).
narrative_ontology:constraint_beneficiary(ai_task_horizon_reliability, top_quartile_adopters).
narrative_ontology:constraint_beneficiary(ai_task_horizon_reliability, highly_skilled_operators).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, median_adopters_and_laggards).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, deskilled_workforce).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, investors_in_overhyped_applications).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESKILLED WORKFORCE (SNARE) — Faces job displacement or skill atrophy from unreliable AI tools they are mandated to use. They cannot exit the labor market and bear the costs of failed AI implementations and shifting job requirements. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.82.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AI PLATFORM OWNER (ROPE) — Provides a coordination tool that unlocks productivity. Reliability issues are seen as engineering challenges, not structural flaws. They profit from subscriptions and compute usage regardless of end-user success, experiencing the system as a net benefit. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes the dual nature of the constraint: a genuine coordination function (automating simple tasks) combined with significant asymmetric extraction (wasted investment, deskilling, market concentration). This is the canonical classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.66.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: TOP-QUARTILE ADOPTER (SCAFFOLD) — A technologically sophisticated firm that uses current AI as a temporary support to gain a competitive edge. They understand the limitations and have a strategy to migrate to better tools as they emerge, effectively treating the current reliability bottleneck as having a sunset clause. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.29.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HYPE-CYCLE INVESTOR (PITON) — Engages in the investment cycle as a performative act, driven by market narratives rather than the technology's current functional reliability. The high theater (demos vs. reality) makes this a piton: the activity persists due to institutional momentum, even if the underlying utility is degraded or not yet realized. theater_ratio=0.65 is close to the 0.70 gate.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_task_horizon_reliability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_task_horizon_reliability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_task_horizon_reliability, TR),
    TR >= 0.70.

:- end_tests(ai_task_horizon_reliability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.48): Moderate-high. Represents the value extracted from failed projects, wasted employee time managing unreliable outputs, and the market concentration benefiting platform owners. Suppression (0.62): High. The intense hype cycle, network effects of dominant platforms, and high switching costs for integrated systems suppress alternatives and critical evaluation. Theater Ratio (0.65): High. There is a significant gap between controlled demos and real-world performance. Marketing and media narratives often obscure the practical limitations of the technology.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. Platform owners see a pure coordination tool (Rope) for solving problems. Workers on the receiving end of a flawed implementation experience a coercive, job-threatening system (Snare). Sophisticated firms see a temporary tool they can exploit and discard (Scaffold). The analyst sees the whole picture: a system with a genuine coordination function but with deeply embedded, asymmetric extraction (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (platform owners, top adopters) have arbitrage or mobile exit options, leading to low derived directionality (d) and thus low or negative effective extraction (χ). They experience the constraint as a benefit or a manageable tool. Victims (workers, median firms) are trapped or constrained, leading to high derived 'd' and high positive 'χ'. They bear the full cost of the system's failures. This structural difference in position and exit capability is what creates the wide perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that the 'AI bottleneck' is not a single phenomenon. Labeling it purely as a 'tool' (Rope) ignores the coercive impact on labor. Labeling it purely as 'exploitation' (Snare) ignores the real productivity gains achieved by skilled users. The Deferential Realism framework correctly models it as a multi-faceted structure whose classification depends entirely on the observer's position relative to the flows of cost and benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    architectural_limit_vs_engineering_problem,
    'Is the reliability bottleneck for long-horizon tasks a fundamental limitation of current AI architectures or merely an engineering problem solvable with more scale and data?',
    'Longitudinal studies of model performance on complex, multi-step reasoning tasks as scale increases; discovery of novel architectures that overcome these limitations.',
    'If a fundamental limit, the constraint is a durable Mountain/Snare for complex tasks. If an engineering problem, it''s a temporary Scaffold that will be dismantled by progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_limit_vs_engineering_problem, empirical, 'Whether the reliability bottleneck is a fundamental architectural limit or a solvable engineering issue.').

omega_variable(
    upskilling_vs_deskilling_equilibrium,
    'Will the widespread deployment of AI lead to a net upskilling of the workforce (AI as a tool) or net deskilling and displacement (AI as a replacement)?',
    'Cross-sector analysis of labor productivity, wage polarization, and demand for specific skills in AI-intensive industries over a 5-10 year period.',
    'Upskilling equilibrium implies the constraint is a Rope or Scaffold. Deskilling equilibrium implies it is a Snare or Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(upskilling_vs_deskilling_equilibrium, empirical, 'The long-term equilibrium between workforce upskilling and deskilling due to AI adoption.').

omega_variable(
    productivity_gain_durability,
    'Are the observed productivity gains from AI durable and compounding, or a one-time benefit from automating the lowest-hanging fruit?',
    'Firm-level analysis of productivity growth rates pre- and post-AI adoption, controlling for the initial wave of simple task automation.',
    'Durable gains suggest a Rope. One-time benefits followed by a plateau suggest a Piton, where adoption becomes performative after initial wins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_gain_durability, empirical, 'Whether AI productivity gains are durable or a one-time effect from automating simple tasks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_task_horizon_reliability, 2023, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_t_tr_t0, ai_task_horizon_reliability, theater_ratio, 0, 0.75).
narrative_ontology:measurement(ai_t_tr_t2, ai_task_horizon_reliability, theater_ratio, 2, 0.6).
narrative_ontology:measurement(ai_t_tr_t5, ai_task_horizon_reliability, theater_ratio, 5, 0.65).

% Extraction over time
narrative_ontology:measurement(ai_t_be_t0, ai_task_horizon_reliability, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_t_be_t2, ai_task_horizon_reliability, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(ai_t_be_t5, ai_task_horizon_reliability, base_extractiveness, 5, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_task_horizon_reliability, resource_allocation).
narrative_ontology:affects_constraint(ai_task_horizon_reliability, labor_market_polarization).
narrative_ontology:affects_constraint(ai_task_horizon_reliability, corporate_capital_expenditure_cycles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
