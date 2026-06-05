% ============================================================================
% CONSTRAINT STORY: ghost_gdp_circulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ghost_gdp_circulation, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ghost_gdp_circulation
 *   human_readable: Ghost GDP Circulation — Machine Production Without Machine Consumption
 *   domain: macroeconomics/labor_economics/financial_systems
 *
 * SUMMARY:
 *   Ghost GDP circulation describes economic output that appears in national
 *   accounts but does not flow through the consumer economy because automated
 *   production systems generate profits without corresponding wage income.
 *   Machines produce but do not consume. This structural delta manifests as
 *   declining velocity of money, rising corporate profit share relative to
 *   household income, and consumption falling as a percentage of GDP. The
 *   constraint is downstream of labor_share_collapse (the mountain constraint
 *   describing automation's structural displacement of labor income) but
 *   represents a distinct coordination mechanism: how does an economy
 *   allocate production when the production function decouples from the
 *   consumption function? The hypothesis type is rope (pure coordination)
 *   with low extractiveness because the constraint primarily coordinates
 *   capital allocation rather than extracting rents. The declining
 *   circulation velocity is not suppression of alternatives — capital owners
 *   can invest, households can access credit, and policy mechanisms
 *   (taxation, transfers) remain available. The constraint emerges from the
 *   structural properties of automated production, not from active
 *   enforcement.
 *
 * KEY AGENTS:
 *   - Capital Owners: Primary beneficiary (institutional/arbitrage) — capture productivity gains as financial returns; experience non-circulation as efficient capital allocation
 *   - Automated Production Systems: Structural actor (institutional/arbitrage) — generate output without wage costs; profits accumulate in corporate balance sheets
 *   - Financial Intermediaries: Primary beneficiary (institutional/arbitrage) — facilitate capital circulation through investment markets; benefit from asset appreciation
 *   - High-Income Professionals: Secondary beneficiary (powerful/mobile) — access capital returns through stock compensation and retirement accounts
 *   - Middle-Income Households: Neutral actor (moderate/constrained) — experience wage stagnation offset by lower consumer prices; modest net benefit
 *   - Policy Reformers: Organized agents (organized/constrained) — see coordination failure with policy sunset (UBI, wealth funds, capital taxation)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees coordination mechanism for allocating production in post-labor economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ghost_gdp_circulation, 0.18).
domain_priors:suppression_score(ghost_gdp_circulation, 0.12).
domain_priors:theater_ratio(ghost_gdp_circulation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ghost_gdp_circulation, extractiveness, 0.18).
narrative_ontology:constraint_metric(ghost_gdp_circulation, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(ghost_gdp_circulation, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ghost_gdp_circulation, rope).
narrative_ontology:human_readable(ghost_gdp_circulation, "Ghost GDP Circulation — Machine Production Without Machine Consumption").
narrative_ontology:topic_domain(ghost_gdp_circulation, "macroeconomics/labor_economics/financial_systems").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ghost_gdp_circulation, capital_owners).
narrative_ontology:constraint_beneficiary(ghost_gdp_circulation, automated_production_systems).
narrative_ontology:constraint_beneficiary(ghost_gdp_circulation, financial_intermediaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPITAL OWNERS (ROPE) — Experience the constraint as pure coordination: machines produce efficiently, profits accumulate in financial instruments, and capital flows to highest returns. The non-circulation is a feature, not a bug — retained earnings and financial asset appreciation are legitimate forms of wealth storage. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.03. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(ghost_gdp_circulation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH-INCOME PROFESSIONALS (ROPE) — Benefit from capital returns through retirement accounts, stock compensation, and real estate appreciation. Experience declining velocity as a coordination problem solved by financial markets: money doesn't need to circulate through consumption when it can circulate through investment. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.002. Near-zero effective extraction.
constraint_indexing:constraint_classification(ghost_gdp_circulation, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-INCOME HOUSEHOLDS (ROPE) — Experience modest wage stagnation but benefit from lower consumer prices due to automation efficiency. The constraint appears as a coordination mechanism: production efficiency translates to purchasing power even if nominal wages are flat. Access to credit markets provides consumption smoothing. d≈0.35, f(d)≈0.25, σ=1.0 → χ≈0.045. Low effective extraction.
constraint_indexing:constraint_classification(ghost_gdp_circulation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICY REFORMERS (SCAFFOLD) — See the declining velocity as a temporary coordination failure with a policy sunset: universal basic income, sovereign wealth funds, or capital taxation can recirculate ghost GDP back into consumer economy. The constraint is a transitional problem during the automation wave, solvable through institutional redesign. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.072.
constraint_indexing:constraint_classification(ghost_gdp_circulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the structural delta (machines produce but don't consume) is a coordination challenge, not extraction. GDP measures production; consumption is only one use of production. Capital accumulation, reinvestment, and financial intermediation are alternative circulation pathways. The constraint coordinates production efficiency with capital allocation. Extractiveness is low because the system generates real output gains — automation increases total factor productivity. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.25.
constraint_indexing:constraint_classification(ghost_gdp_circulation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ghost_gdp_circulation_tests).
:- end_tests(ghost_gdp_circulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint generates real productivity gains through automation — total output increases even as circulation velocity declines. Capital owners capture these gains, but the gains are real, not purely extractive rents. The low extractiveness reflects that machines genuinely produce more efficiently than human labor in many domains, and the profit accumulation represents legitimate returns to capital investment in automation technology. This is not a zero-sum transfer but a positive-sum coordination of production efficiency with capital allocation. Suppression (0.12): Very low. Alternative circulation pathways exist: progressive taxation, sovereign wealth funds, universal basic income, credit markets, and asset ownership democratization. The constraint does not actively suppress these alternatives — they are policy choices, not structural impossibilities. Some suppression exists (political economy barriers to redistribution, wealth concentration feedback loops) but it is modest. Theater ratio (0.22): Low. GDP accounting accurately measures production; financial markets genuinely allocate capital; automation genuinely increases productivity. There is some theater in the form of 'trickle-down' rhetoric that overstates consumption benefits, but the core mechanisms are functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits minimal perspectival gap because all agents experience it primarily as coordination rather than extraction. Capital owners see efficient capital allocation. High-income professionals see financial market coordination. Middle-income households see purchasing power maintained through lower prices. Policy reformers see a solvable coordination problem. The analytical observer sees a structural coordination mechanism for post-labor production. The gap is not 'rope vs snare' but 'rope with different circulation pathways.' The policy reformer's scaffold perspective is the only divergence — they see a sunset through institutional redesign, while other perspectives see a stable equilibrium. This minimal gap is appropriate for a low-extractiveness constraint: when a constraint genuinely coordinates rather than extracts, most perspectives converge on rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Capital owners: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary — capture productivity gains as financial returns. High-income professionals: Beneficiary + mobile → d≈0.15, f(d)≈-0.01. Near-zero effective extraction — benefit from capital returns but less directly than owners. Middle-income households: Mixed (beneficiary via lower prices, neutral on wages) + constrained → d≈0.35, f(d)≈0.25. Low effective extraction — purchasing power roughly maintained. Policy reformers: Organized + constrained → d≈0.40, f(d)≈0.40. Low effective extraction — have agency to redesign institutions. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Rope classification despite higher d because base extractiveness is low (0.18) — even with analytical scaling, χ≈0.25, well below snare threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low extractiveness produces rope classification across nearly all perspectives. The structural delta (machines produce but don't consume) could hypothetically be framed as extraction — 'capital steals productivity gains from labor' — but the metrics contradict this framing. Extractiveness is low (0.18) because automation generates real output gains, not zero-sum transfers. Suppression is low (0.12) because alternative circulation pathways (taxation, transfers, credit) remain available. Theater is low (0.22) because the mechanisms are functional. The mandatrophy question 'Is this coordination or extraction?' is answered by the structural data: it is primarily coordination. The policy reformer's scaffold perspective acknowledges that the coordination could be improved (recirculation via UBI or wealth funds), but improvement opportunity does not imply current extraction. A rope can have a better configuration without being a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ghost_gdp_circulation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ghost_gdp_circulation, resource_allocation).

% DUAL FORMULATION NOTE:
% Ghost GDP circulation is downstream of labor_share_collapse (the mountain constraint describing automation's structural displacement of labor income). Labor share collapse is the cause (ε≈0.08, mountain — structural property of automation technology). Ghost GDP circulation is the effect (ε=0.18, rope — coordination mechanism for allocating production in a post-labor economy). The two constraints have different ε values because they describe different structural phenomena: labor share collapse is nearly immutable (mountain), while ghost GDP circulation is a coordination problem with policy solutions (rope/scaffold).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
