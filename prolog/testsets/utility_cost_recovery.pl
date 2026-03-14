% ============================================================================
% CONSTRAINT STORY: utility_cost_recovery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_utility_cost_recovery, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: utility_cost_recovery
 *   human_readable: Utility Cost Recovery in Public Service Infrastructure
 *   domain: economic_policy/infrastructure_finance
 *
 * SUMMARY:
 *   Utility cost recovery mechanisms determine how the costs of water,
 *   electricity, natural gas, and wastewater treatment infrastructure are
 *   distributed across consumer populations. The constraint exhibits genuine
 *   coordination functions — multiple users must share fixed infrastructure
 *   costs, and transparent mechanisms allocate these costs fairly — alongside
 *   extractive dynamics where cost recovery methodologies systematically
 *   shift burdens to price-sensitive, low-income consumers. The constraint's
 *   extractiveness has risen over the measurement interval (0.35 → 0.52) as
 *   aging infrastructure has increased replacement costs and climate-driven
 *   service demands (drought-resistant distribution, flood-resilient systems,
 *   renewable integration) have expanded capital requirements. Theater ratio
 *   remains low (0.35) because cost-of-service regulation produces relatively
 *   transparent rate-setting processes with documented methodologies;
 *   however, the predictability of outcomes within a range of discretionary
 *   choices (depreciation schedules, capital cost assumptions, allocative
 *   formulas) creates performative legitimacy for distributions that are
 *   substantially driven by regulatory power rather than unavoidable cost
 *   structures.
 *
 * KEY AGENTS:
 *   - Low-Income Household: Primary victim (powerless/trapped) — no exit options due to service necessity; faces disconnection risk and debt accumulation as rates rise above income growth
 *   - Middle-Income Household: Secondary victim (moderate/constrained) — constrained by consumption options but can relocate or reduce demand; bears cost increases but with flexibility
 *   - Utility Operator: Primary beneficiary (institutional/arbitrage) — benefits from assured cost recovery and regulatory certainty; can optimize capital spending and efficiency gain allocation
 *   - Regulatory Commission: Organized actor (organized/constrained) — maintains coordination function but also exercises allocative power; constrained by political economy and legal mandates
 *   - Infrastructure Investors: Secondary beneficiary (institutional/arbitrage) — benefit from stable returns on utility infrastructure investment; capture dividends from cost recovery guarantees
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing monopoly utility regulation as inevitable rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(utility_cost_recovery, 0.52).
domain_priors:suppression_score(utility_cost_recovery, 0.48).
domain_priors:theater_ratio(utility_cost_recovery, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(utility_cost_recovery, extractiveness, 0.52).
narrative_ontology:constraint_metric(utility_cost_recovery, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(utility_cost_recovery, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(utility_cost_recovery, tangled_rope).
narrative_ontology:human_readable(utility_cost_recovery, "Utility Cost Recovery in Public Service Infrastructure").
narrative_ontology:topic_domain(utility_cost_recovery, "economic_policy/infrastructure_finance").

domain_priors:requires_active_enforcement(utility_cost_recovery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(utility_cost_recovery, utility_operators).
narrative_ontology:constraint_beneficiary(utility_cost_recovery, infrastructure_investors).
narrative_ontology:constraint_victim(utility_cost_recovery, low_income_consumers).
narrative_ontology:constraint_victim(utility_cost_recovery, price_sensitive_demand).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME CONSUMER (SNARE) — Trapped by essential service dependency and geographic immobility. No alternative water, electricity, or natural gas suppliers exist. Rising cost recovery rates force choice between utility access and other necessities. Suppression is high: debt penalties, disconnection threats, credit reporting create cascading barriers to exit. Maximum experienced extraction.
constraint_indexing:constraint_classification(utility_cost_recovery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME CONSUMER (TANGLED ROPE) — Constrained but not trapped. Can reduce consumption through efficiency, relocate if rates become intolerable, or switch to alternative fuels/providers where available. Benefits from reliable infrastructure coordination (genuine service provision coordination exists). Bears extraction through above-inflation rate increases and cost-shifting. Mixed experience: genuine coordination benefit with asymmetric cost allocation.
constraint_indexing:constraint_classification(utility_cost_recovery, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UTILITY OPERATOR (ROPE) — Experiences cost recovery as a coordination mechanism: transparent rate-setting methodology allocates shared infrastructure costs across user base. Benefits from regulatory certainty and assured cost recovery. Exit options: can adjust capital spending, pursue efficiency gains, or seek regulatory approval for rate mechanisms. Perceives constraint as enabling sustainable service provision.
constraint_indexing:constraint_classification(utility_cost_recovery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Organized actors balancing multiple constituencies: consumer protection, utility financial sustainability, infrastructure investment requirements, and political pressure. Genuine coordination function (rate-setting mechanisms, cost allocation rules, fairness standards). Also extraction: captures administrative authority, sets agendas, determines what counts as 'reasonable' cost recovery. Constrained by legal mandates, political limits, and judicial review.
constraint_indexing:constraint_classification(utility_cost_recovery, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY MODEL (PITON) — Traditional cost-of-service ratemaking (Revenue Requirement = Operating Expenses + Depreciation + Return on Equity) has become largely performative. The model assumes transparent, auditable cost accounting, but utilities have significant discretion in cost allocation, depreciation schedules, and capital structure decisions. Theater ratio (0.35) reflects that substantial regulatory process (rate cases, expert testimony, commission deliberation) produces predetermined outcomes under political economy constraints. The model persists through institutional inertia despite alternatives (performance-based ratemaking, revenue decoupling) existing.
constraint_indexing:constraint_classification(utility_cost_recovery, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, cost recovery for essential infrastructure is an immutable requirement: someone must pay for pipes, wires, and treatment facilities, and that cost is inescapable. This perspective risks naturalizing a contingent institutional design (monopoly utility operators with cost-of-service regulation) as a law of nature. The engine's false summit detector will identify this as misclassification — alternative delivery models (municipal utilities, cooperatives, regional authorities) show cost recovery is feasible under different institutional arrangements.
constraint_indexing:constraint_classification(utility_cost_recovery, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(utility_cost_recovery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(utility_cost_recovery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(utility_cost_recovery, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(utility_cost_recovery, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(utility_cost_recovery, TR),
    TR >= 0.70.

:- end_tests(utility_cost_recovery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Cost recovery captures genuine coordination value (infrastructure must be paid for) but also reflects regulatory discretion in cost allocation. Utilities have measurable discretion over what counts as 'necessary' cost (capital depreciation rates, return-on-equity targets, cost allocation across customer classes). The rising trend (0.35 → 0.52) reflects both real infrastructure aging and rent-seeking behavior as regulatory capture increases the proportion of costs classified as recoverable. Suppression (0.48): Moderate. Barriers to exit exist (essential service, geographic monopoly, switching costs) but are not absolute — distributed alternatives (solar, water recycling) are technically feasible and increasingly cost-competitive. Suppression reflects institutional lock-in (regulatory barriers to alternative providers, grid dependency) rather than pure impossibility. Theater ratio (0.35): Low-moderate. Cost-of-service ratemaking involves genuine rate cases with documented evidence, expert testimony, and commission deliberation. The process is not purely performative because outcomes do vary with evidence and advocacy. However, the theater index reflects that regulatory discretion (how depreciation is calculated, what return-on-equity is 'fair') predetermined within political economy constraints produces predictable distributions. The trend (0.28 → 0.35) reflects increasing procedural complexity and legitimation theater as regulatory capture intensifies.
 *
 * PERSPECTIVAL GAP:
 *   Why does the utility operator see rope while the low-income consumer sees snare? Directional difference: the operator benefits (beneficiary + arbitrage exit = low d → negative χ); the consumer bears costs (victim + trapped exit = high d → high χ). Same structural mechanism, opposite effective directions. Why does the regulatory commission see tangled rope rather than rope? Because the commission exercises allocative power (extraction) while performing coordination (allocation). The gap reveals the power distribution: the commission's power to define 'reasonable' cost recovery is deployed to benefit operators, not consumers. The false mountain at the civilizational scale represents the biggest gap: the analytical view that cost recovery is inherent to infrastructure naturalizes the specific institutional choice (monopoly utility regulation) as inevitable, missing that municipal utilities, cooperatives, and public authorities show cost recovery is compatible with different power distributions and beneficiary classes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position: low-income consumers are victims with trapped exit (d ≈ 0.95), experiencing maximum f(d) ≈ 1.42. Utility operators are beneficiaries with arbitrage exit (d ≈ 0.05), experiencing f(d) ≈ -0.12 (negative effective extraction — they benefit). Middle-income consumers are mixed (constrained exit, d ≈ 0.70, f(d) ≈ 1.00). Regulatory commissions face asymmetric power: they are nominally neutral (d ≈ 0.50) but structurally captured by utility interests, so effective d should be overridden higher (d ≈ 0.65) to reflect that commission power is deployed in utility favor. The scope modifier σ(S) applies: national scope (σ=1.0) means extraction is not amplified by scope (unlike global infrastructure where σ=1.2). However, within-region concentration of low-income consumers in specific service territories creates effective local scope (σ=0.8 locally) where suppression is higher and alternatives fewer.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination from extraction within a single institutional mechanism. Cost recovery is genuinely necessary coordination — infrastructure has costs that must be allocated. The mandatrophy arises because the cost-of-service regulatory model claims to be transparent allocation (rope) while functionally operating as an asymmetric extraction mechanism (snare for low-income consumers). The tangled rope classification captures this: genuine coordination function (cost allocation) exists alongside asymmetric extraction (burden shifted to trapped consumers via regulatory discretion). The regulatory commission is where the extraction-coordination boundary is enforced or violated: a captured regulator sees cost recovery as pure coordination (rope) and legitimizes allocative choices that benefit utilities; an independent regulator sees mixed coordination-extraction (tangled rope) and constrains allocative power to protect vulnerable consumers. The piton perspective (degraded model) and the mountain perspective (naturalized as immutable) both represent failure modes: the piton sees the regulatory process as theater divorced from substantive outcomes; the mountain sees cost recovery as inevitable law, missing that alternative institutional designs are feasible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_accounting_elasticity,
    'How much of the cost burden genuinely reflects unavoidable infrastructure costs versus allocations influenced by regulatory discretion and accounting choices?',
    'Comparative cost accounting across utilities with different regulatory frameworks and ownership structures; detailed audit of cost allocation methodologies; counterfactual analysis of alternative depreciation schedules and capital cost assumptions',
    'If unavoidable costs are 70%+: cost recovery is necessary coordination with limited extraction; snare classification downgraded. If unavoidable costs are <50%: cost recovery mechanism is primarily extractive; snare classification upheld and piton theater increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_accounting_elasticity, empirical, 'Proportion of cost burden driven by unavoidable infrastructure versus regulatory discretion').

omega_variable(
    demand_substitution_feasibility,
    'Can consumers exit utility dependence through alternative technologies (distributed solar, rainwater harvesting, liquefied natural gas, neighborhood microgrids) faster than regulatory cost recovery increases?',
    'Tracking adoption curves for distributed technologies relative to utility rate increase timelines; cost comparison analysis between utility service and alternatives across income levels; longitudinal data on voluntary disconnection and off-grid switching',
    'If alternatives mature faster: exit_options shift from trapped to constrained for low-income consumers; snare downgraded to tangled rope. If utilities maintain technological lock-in: trapped status persists and extraction mechanisms strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_substitution_feasibility, empirical, 'Feasibility and speed of consumer exit through distributed alternatives').

omega_variable(
    regulatory_capture_mechanism,
    'Are regulatory commissions captured by utility interests, such that cost recovery mechanisms consistently bias cost allocation toward utility benefit and away from consumer protection?',
    'Analysis of rate case outcomes: percentage of utility requests approved unchanged vs modified vs rejected; longitudinal tracking of commission composition and career paths of commissioners; stakeholder influence mapping (expert testimony frequencies, lobbying spend, revolving-door employment)',
    'If capture is high: regulatory commission shifts from tangled_rope to snare for consumers; extraction mechanisms intensify and cost recovery becomes pure institutional extraction. If capture is low: regulatory commission maintains genuine coordination function and cost recovery classifications remain mixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Degree of regulatory capture by utility interests in cost recovery proceedings').

omega_variable(
    cross_subsidy_direction,
    'Do cost recovery mechanisms redistribute from high-consumption to low-consumption households, or from low-income to high-income households?',
    'Household-level analysis of rate structures and burden: fixed charges, volumetric rates, tiered pricing; correlation of rate burden with income, consumption, and household demographics; comparison of affordability metrics across income deciles',
    'If subsidies flow to low-income: cost recovery partially resolves equity tension; snare classification weakened. If subsidies flow away from low-income (common under flat rate designs): extractive mechanism is strengthened and snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_subsidy_direction, empirical, 'Direction and magnitude of cross-subsidies within rate structure').

omega_variable(
    infrastructure_necessity_threshold,
    'What portion of utility infrastructure costs are necessary to provide basic service, and what portion represents luxury/redundancy/wasteful practices?',
    'Engineering analysis of system components (pipes, treatment, distribution); benchmarking against utilities operating at lower cost per unit; identification of deferred maintenance vs overbuilding; comparative study of service reliability vs cost across operators',
    'If necessity threshold is 80%+: cost recovery burden is mostly required; extraction component is limited. If threshold is <60%: substantial costs reflect inefficiency or wasteful practice; cost recovery mechanism is capturing inefficiency-induced rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_necessity_threshold, empirical, 'Necessity threshold for utility infrastructure costs versus discretionary spending').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(utility_cost_recovery, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(util_tr_t0, utility_cost_recovery, theater_ratio, 0, 0.28).
narrative_ontology:measurement(util_tr_t10, utility_cost_recovery, theater_ratio, 10, 0.32).
narrative_ontology:measurement(util_tr_t20, utility_cost_recovery, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(util_be_t0, utility_cost_recovery, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(util_be_t10, utility_cost_recovery, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(util_be_t20, utility_cost_recovery, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(utility_cost_recovery, resource_allocation).
narrative_ontology:affects_constraint(utility_cost_recovery, energy_poverty).
narrative_ontology:affects_constraint(utility_cost_recovery, water_access_equity).
narrative_ontology:affects_constraint(utility_cost_recovery, stranded_asset_risk).
narrative_ontology:affects_constraint(utility_cost_recovery, regulatory_capture_utilities).

% DUAL FORMULATION NOTE:
% Utility cost recovery is upstream of energy poverty and water access equity constraints — the cost recovery mechanism creates the extraction conditions that manifest as poverty traps. Cost recovery is downstream of stranded asset risk: utilities seek cost recovery for legacy infrastructure (coal plants, aging pipes) that may be made obsolete by distributed alternatives, creating a coupling where cost recovery mechanisms actively suppress the exit pathways that would otherwise resolve the snare. Regulatory capture mechanisms are orthogonal: capture drives the boundary between coordination and extraction in cost recovery allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(utility_cost_recovery, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
