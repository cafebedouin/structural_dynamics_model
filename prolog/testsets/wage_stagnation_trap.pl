% ============================================================================
% CONSTRAINT STORY: wage_stagnation_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wage_stagnation_trap, []).

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
 *   constraint_id: wage_stagnation_trap
 *   human_readable: Wage Stagnation Trap
 *   domain: labor_economics/political_economy
 *
 * SUMMARY:
 *   The wage stagnation trap represents a structural constraint operating
 *   across labor markets in advanced capitalist economies since approximately
 *   1980. Nominal wages have continued to rise, but real wage growth
 *   (adjusted for inflation and cost of living) has decoupled from
 *   productivity growth, benefiting capital holders and low-wage employers at
 *   the expense of workers. The constraint exhibits characteristics of a
 *   tangled rope — genuine coordination mechanisms for labor (minimum wage
 *   law, sectoral bargaining standards in some countries) exist alongside
 *   asymmetric extraction (monopsony power, union decline, wage suppression
 *   through labor law). The constraint is maintained through active
 *   institutional enforcement (labor law suppression, immigration controls,
 *   intellectual property regimes that limit worker mobility) rather than
 *   natural market forces alone. Theater increases over the interval as the
 *   constraint becomes more explicit: productivity-wage decoupling is now
 *   widely documented, yet framed as inevitable market dynamics rather than
 *   policy choice.
 *
 * KEY AGENTS:
 *   - Wage Workers: Primary victims (powerless/trapped) — stagnant nominal wages despite inflation and productivity gains; limited geographic mobility and skill constraints
 *   - Capital Holders: Primary beneficiaries (institutional/arbitrage) — capture productivity gains as profits; benefit from global labor supply competition and automation threats
 *   - Low-Wage Employers: Secondary beneficiaries (powerful/mobile) — suppress wages through labor market concentration and scheduling volatility
 *   - Organized Labor: Secondary victim (organized/constrained) — benefit from coordination (bargaining standards) but face extraction through declining membership and union-busting
 *   - Progressive Policy Coalition: Organized agent (powerful/mobile) — perceives temporary policy failure solvable through labor law reform; believes sunset path exists
 *   - Neoliberal Economic Framework: Institutional actor (institutional/arbitrage) — maintains theater of inevitability; requires continuous enforcement but appears natural
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wage_stagnation_trap, 0.58).
domain_priors:suppression_score(wage_stagnation_trap, 0.68).
domain_priors:theater_ratio(wage_stagnation_trap, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wage_stagnation_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(wage_stagnation_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wage_stagnation_trap, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wage_stagnation_trap, tangled_rope).
narrative_ontology:human_readable(wage_stagnation_trap, "Wage Stagnation Trap").
narrative_ontology:topic_domain(wage_stagnation_trap, "labor_economics/political_economy").

domain_priors:requires_active_enforcement(wage_stagnation_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wage_stagnation_trap, capital_holders).
narrative_ontology:constraint_beneficiary(wage_stagnation_trap, low_wage_employers).
narrative_ontology:constraint_victim(wage_stagnation_trap, wage_workers).
narrative_ontology:constraint_victim(wage_stagnation_trap, labor_market_elasticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE WORKER (SNARE) — Trapped in stagnant nominal wages despite inflation and productivity gains. Material barriers: limited geographic mobility due to housing costs and family ties, skill constraints requiring expensive retraining, job switching costs, and employer monopsony power in concentrated labor markets. No meaningful exit option. Bears full extraction: real wages decline while effort and output increase.
constraint_indexing:constraint_classification(wage_stagnation_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED LABOR (TANGLED ROPE) — Union members benefit from collective bargaining coordination (wage standards, workplace safety agreements) but face asymmetric extraction through declining membership, union busting, and sectoral shifts. Exit costs are high (loss of bargaining power, career penalties) but not impossible. Experience shows genuine coordination function alongside structural extraction.
constraint_indexing:constraint_classification(wage_stagnation_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CAPITAL HOLDER (ROPE) — Experiences wage stagnation constraint as efficient labor-cost coordination mechanism. Benefits from arbitrage options (offshoring, automation, wage competition across regions). Net beneficiary. Constraint coordinates global labor supply pressure into wage suppression without requiring direct coercion — market forces naturalize the extraction.
constraint_indexing:constraint_classification(wage_stagnation_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LOW-WAGE SERVICE SECTOR (TANGLED ROPE) — Coordinated through minimum wage policy and sectoral standards, but extraction persists through scheduling volatility, benefits witholding, and wage theft. Organized collective action (service worker unions) creates genuine coordination around working conditions. Exit is constrained (few alternatives in low-skill service work) but not trapped. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(wage_stagnation_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE POLICY COALITION (SCAFFOLD) — See wage stagnation as a temporary policy failure solvable through minimum wage increases, wage transparency mandates, union organizing rights, and sectoral bargaining. Sunset logic: as these policies mature and shift bargaining power, wage growth should re-couple to productivity. Low extraction from this perspective because the coalition has structural agency and perceives an achievable exit path.
constraint_indexing:constraint_classification(wage_stagnation_trap, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: NEOLIBERAL FRAMEWORK (PITON) — The constraint appears as a natural consequence of competitive market forces and labor supply dynamics. But the framework is substantially theatrical: the 'inevitable' wage stagnation requires active policy enforcement (union-busting laws, anti-labor court rulings, immigration controls calibrated to suppress wages, intellectual property regimes limiting worker mobility). The framework has degraded from coordination mechanism to inertial theater — it persists despite growing contradictions and policy choices that maintain it.
constraint_indexing:constraint_classification(wage_stagnation_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, wage stagnation appears as an immutable consequence of global labor supply exceeding demand, competitive pressure on wages, and capital mobility. Capital flows to lowest-cost labor; wages converge to subsistence globally. This perspective risks naturalizing what is structurally a policy-maintained extraction — the constraint's persistence requires continuous institutional work (labor law suppression, immigration policy, monopsony enforcement) that is not natural law but active enforcement.
constraint_indexing:constraint_classification(wage_stagnation_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wage_stagnation_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wage_stagnation_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wage_stagnation_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wage_stagnation_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wage_stagnation_trap, TR),
    TR >= 0.70.

:- end_tests(wage_stagnation_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Wage workers experience real income loss (wages fail to keep pace with inflation and productivity growth). The extraction is not maximal because workers retain some agency — job switching, skill development, organizing — though at high cost. The measurement trajectory shows extraction accumulating from 0.25 (1980) to 0.58 (present) as union decline accelerates and monopsony concentration increases. Suppression (0.68): High. Barriers to exit are substantial: geographic immobility (housing costs, family ties), skill constraints requiring expensive retraining, monopsony power concentrating job options, and labor law suppression of organizing. But suppression is not total — some workers can relocate, some can skill up, some can organize. Theater ratio (0.45): Moderate. The constraint operates partly through genuine market dynamics (capital-labor substitution, global competition) and partly through institutional theater (labor law framing market outcomes as inevitable, media narratives naturalizing stagnation). Theater has increased over time as the decoupling becomes harder to deny — the narrative work required to maintain 'this is just the market' increases proportionally.
 *
 * PERSPECTIVAL GAP:
 *   The magnitude of perspectival disagreement reveals the constraint's hybrid nature. A trapped worker classifies this as snare (pure extraction, d≈0.95, f(d)≈1.42). A capital holder classifies this as rope (market coordination, d≈0.05, f(d)≈-0.12). An organized labor representative classifies this as tangled rope (mixed coordination and extraction, d≈0.55). The engine computes chi from these differing (P,T,E,S) tuples, and the result is a presheaf showing that all three classifications are structurally real — they describe different aspects of the same constraint from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position in the extraction flow. Trapped workers with no arbitrage options have high d (0.85+), experiencing maximum f(d) and high chi. Capital holders with global arbitrage options have low d (0.10-0.20), experiencing negative or minimal f(d). Organized labor with some exit options and some coordination benefits has moderate d (0.50-0.60). The organized policy coalition has high mobility and perceived agency, so d is lower (0.40-0.50) despite being a 'powerful' actor opposing capital. The neoliberal framework appears institutional but is actually capturing the institutional analytical position — it should perhaps be reclassified to show this capture, but as written it occupies the institutional piton role (degraded theater).
 *
 * MANDATROPHY ANALYSIS:
 *   The wage stagnation constraint avoids mandatrophy through its genuine hybrid nature. It is not mislabeled extraction (snare labeled as rope) because the capital-labor coordination function is real — labor market institutions (minimum wage, overtime rules, sectoral standards) do coordinate expectations and reduce friction. Nor is it mislabeled coordination (rope) because the asymmetric extraction is also real and measurable — capital's share of income rises while labor's share stagnates despite productivity gains. The constraint's mandatrophy is resolved by acknowledging that it performs both functions with different strength for different agents. The engine's role is to measure the ratio and locate it correctly in the tangled rope zone (0.40 ≤ χ ≤ 0.90). The temporal measurement trajectory (extraction rising from 0.25 to 0.58 over 30 years) indicates gradual shift toward snare characteristics — the coordination function has weakened (union decline, labor law suppression) while extraction has intensified (monopsony growth, wage-productivity decoupling). A future update may classify this as solidifying into snare if the trajectory continues and coordination function disappears entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_stagnation_causation,
    'Is wage stagnation driven by structural supply/demand (natural law) or by policy choices that suppress wage growth (institutional extraction)?',
    'International comparison: OECD countries with similar capital mobility but different labor law and unionization show divergent wage trajectories. Germany, Scandinavia have stronger wage growth despite similar global pressures. This suggests policy causation dominates.',
    'If policy-driven: the mountain perspective is a false summit (piton/naturalization). If supply/demand-driven: the mountain is correct and policy interventions have limited effect. Classification hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_stagnation_causation, empirical, 'Whether wage stagnation is structural or policy-driven').

omega_variable(
    labor_market_concentration_degree,
    'What fraction of wage stagnation is attributable to employer monopsony concentration vs global labor supply pressure?',
    'Panel analysis of employer concentration metrics (Hirschman-Herfindahl index by labor market, wage posting patterns) correlated with wage growth. Time-series comparison of labor concentration changes to wage growth changes.',
    'If monopsony dominates: snare classification is more accurate (trapped workers, concentrated extraction). If global supply dominates: rope is more accurate (market-mediated, less extractive). Affects directionality d for powerless workers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_concentration_degree, empirical, 'Relative contribution of monopsony vs global supply to wage stagnation').

omega_variable(
    productivity_wage_decoupling_mechanism,
    'Why have productivity and wages decoupled since 1980? Is the mechanism capital-labor share shift (extraction), or measurement error (misdeclassification)?',
    'Reconciliation of productivity data (output per hour vs real wages). Analysis of whether productivity gains flow to capital through profits, land rents, or are mismeasured. Sectoral decomposition showing where decoupling occurs.',
    'If true decoupling via profit concentration: snare characteristics confirmed (extraction is real and measurable). If measurement artifact: extraction is lower and constraint is more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_wage_decoupling_mechanism, empirical, 'Mechanism driving productivity-wage decoupling').

omega_variable(
    union_decline_causation,
    'Is union decline an exogenous shock (deindustrialization, globalization) or endogenous extraction (employer union-busting, labor law suppression)?',
    'Cross-sectional variation: compare industries with similar globalization pressure but different labor law regimes. Countries with strong anti-union law show faster union decline. Temporal correlation of labor law changes to union decline.',
    'If exogenous: unions were overdue for decline and wage stagnation follows. If endogenous extraction: the constraint is actively maintained and could be reversed by labor law reform. Changes classification confidence from mountain to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_decline_causation, empirical, 'Whether union decline is exogenous or policy-driven').

omega_variable(
    wage_suppression_visibility,
    'Why do workers not perceive wage stagnation as extraction? Is it cognitive capture (identity-locked), or structural opacity (theater)?',
    'Survey data on worker perception vs objective wage metrics. Analysis of media framing of wage stagnation vs productivity gains. Ethnographic study of whether suppression is internalized or simply not visible.',
    'If identity-locked (workers blame themselves, accept stagnation as deserved): the constraint is more snare-like (cognitive suppression reinforces structural trap). If theater (suppression is institutional and visible to organized agents): the constraint is more tangled_rope (some agents can mobilize against it).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wage_suppression_visibility, empirical, 'Whether wage suppression is cognitively captured or structurally opaque').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wage_stagnation_trap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wage_tr_t0, wage_stagnation_trap, theater_ratio, 0, 0.3).
narrative_ontology:measurement(wage_tr_t10, wage_stagnation_trap, theater_ratio, 10, 0.35).
narrative_ontology:measurement(wage_tr_t20, wage_stagnation_trap, theater_ratio, 20, 0.42).
narrative_ontology:measurement(wage_tr_t30, wage_stagnation_trap, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(wage_be_t0, wage_stagnation_trap, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(wage_be_t10, wage_stagnation_trap, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(wage_be_t20, wage_stagnation_trap, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(wage_be_t30, wage_stagnation_trap, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wage_stagnation_trap, resource_allocation).
narrative_ontology:affects_constraint(wage_stagnation_trap, labor_union_decline).
narrative_ontology:affects_constraint(wage_stagnation_trap, housing_cost_spiral).
narrative_ontology:affects_constraint(wage_stagnation_trap, monopsony_labor_concentration).

% DUAL FORMULATION NOTE:
% Wage stagnation trap is upstream of three structurally distinct downstream constraints: (1) labor_union_decline (caused by wage pressure, feeds back to cause more wage suppression), (2) housing_cost_spiral (caused by wage stagnation, further immobilizes workers), (3) monopsony_labor_concentration (enables wage suppression, reinforced by lack of worker mobility). Each downstream constraint has its own epsilon and perspectives. The wage stagnation trap story models the aggregate macro phenomenon; the downstream stories model specific institutional mechanisms maintaining it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wage_stagnation_trap, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
