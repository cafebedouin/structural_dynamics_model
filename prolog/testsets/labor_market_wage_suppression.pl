% ============================================================================
% CONSTRAINT STORY: labor_market_wage_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_wage_suppression, []).

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
 *   constraint_id: labor_market_wage_suppression
 *   human_readable: Labor Market Wage Suppression via Coordination and Asymmetric Extraction
 *   domain: economic/labor
 *
 * SUMMARY:
 *   Labor market wage suppression is a structural constraint operating
 *   through multiple simultaneous mechanisms: monopsony employer power in
 *   concentrated industries, geographic immobility of workers, information
 *   asymmetries about wage norms and job opportunities, and asymmetric
 *   capital mobility that allows employers to credibly threaten offshoring or
 *   automation. The constraint exhibits genuine coordination functions (job
 *   matching, skill development, capital allocation) alongside asymmetric
 *   extraction (systematically suppressed wages below marginal product of
 *   labor, concentrated wealth capture). The extractiveness has increased
 *   over the measurement interval (0.35 → 0.58) as employer concentration has
 *   increased and union density has declined. Theater ratio remains low
 *   (0.38) because wage suppression operates through structural economic
 *   mechanisms rather than performative rituals — the suppression is real
 *   rather than theatrical. The constraint classifies as Tangled Rope at the
 *   analytical level: it possesses both coordination function (matching
 *   workers to jobs, incentivizing skill development) and asymmetric
 *   extraction (wealth flows disproportionately to capital holders). From
 *   different structural positions, the same constraint appears as pure
 *   extraction (Snare) to immobilized workers, mixed coordination-extraction
 *   (Tangled Rope) to mobile but cost-burdened workers, pure coordination
 *   (Rope) to employers, organized resistance (Tangled Rope) to unions,
 *   solvable policy problem (Scaffold) to progressive coalitions, and
 *   degraded economic theory (Piton) to the neoclassical consensus.
 *
 * KEY AGENTS:
 *   - Low-skilled workers without geographic mobility: Primary victim (powerless/trapped) — face suppressed wages with no realistic exit options
 *   - Mobile but cost-burdened workers: Secondary victim (moderate/constrained) — can exit but face high costs (relocation, education, credential acquisition)
 *   - Monopsonistic employers and concentrated industries: Primary beneficiary (institutional/arbitrage) — capture suppression benefits while maintaining capital mobility
 *   - Union movement: Secondary actor (organized/constrained) — fights suppression through collective action but faces structural barriers
 *   - Progressive policy coalition: Organized agent (organized/constrained) — implements temporary scaffolding (minimum wage, labor standards)
 *   - Neoclassical economics consensus: Institutional frame (institutional/arbitrage) — naturalizes suppression as market equilibrium
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks treating contingent institutional arrangements as natural laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_wage_suppression, 0.58).
domain_priors:suppression_score(labor_market_wage_suppression, 0.72).
domain_priors:theater_ratio(labor_market_wage_suppression, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_wage_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_market_wage_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(labor_market_wage_suppression, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_wage_suppression, tangled_rope).
narrative_ontology:human_readable(labor_market_wage_suppression, "Labor Market Wage Suppression via Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(labor_market_wage_suppression, "economic/labor").

domain_priors:requires_active_enforcement(labor_market_wage_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_wage_suppression, employers_concentrated_market_power).
narrative_ontology:constraint_beneficiary(labor_market_wage_suppression, capital_holders).
narrative_ontology:constraint_victim(labor_market_wage_suppression, low_skilled_workers).
narrative_ontology:constraint_victim(labor_market_wage_suppression, workers_without_mobility).
narrative_ontology:constraint_victim(labor_market_wage_suppression, workers_geographically_constrained).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILIZED LOW-WAGE WORKER (SNARE) — Workers in economically depressed regions or with family obligations cannot exit the labor market; face suppressed wages with no realistic alternative. Geographic constraint + economic dependency create structural entrapment. Experiences maximum extraction with no coordination benefit.
constraint_indexing:constraint_classification(labor_market_wage_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MOBILE BUT COST-BURDENED WORKER (TANGLED ROPE) — Can relocate or acquire skills, but faces high costs (housing, education, relocation). Experiences both coordination (labor matching, skill development infrastructure) and extraction (suppressed starting wages, credential requirements). Exit possible at significant personal cost.
constraint_indexing:constraint_classification(labor_market_wage_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONOPSONISTIC EMPLOYER (ROPE) — Large firms or concentrated industries experience wage suppression as a coordination mechanism: stabilized labor costs enable predictable capital allocation and workforce planning. Benefits from wage floor suppression while maintaining the fiction that 'market forces' determine wages. High mobility in capital markets — can relocate production, offshore, or arbitrage labor across regions.
constraint_indexing:constraint_classification(labor_market_wage_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UNION MOVEMENT (TANGLED ROPE) — Organized labor benefits from the coordination infrastructure (collective bargaining frameworks, labor standards enforcement) but is itself suppressed by legal and economic barriers (right-to-work laws, capital mobility, gig economy fragmentation). Seeks to exit suppression through collective action but faces escalating costs as employers respond with outsourcing and automation.
constraint_indexing:constraint_classification(labor_market_wage_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE POLICY COALITION (SCAFFOLD) — Minimum wage policies, labor standards enforcement, and sectoral bargaining represent temporary support structures with implicit sunset clauses. Effective only if underlying political will persists; vulnerable to capture and degradation. Sees wage suppression as a solvable policy problem with specific temporal horizons (legislation requiring periodic renewal).
constraint_indexing:constraint_classification(labor_market_wage_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NEOCLASSICAL ECONOMICS CONSENSUS (PITON) — Competitive labor market theory predicts wages equal marginal product of labor; wage suppression is theorized as either temporary or evidence of low worker productivity. The theory persists despite persistent counterevidence (monopsony power, information asymmetries, structural immobility). Theater ratio high because the theory is frequently invoked to justify suppression while failing to predict or prevent it. Institutional inertia maintains the framework despite degraded explanatory power.
constraint_indexing:constraint_classification(labor_market_wage_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilization-scale analytical perspective, some wage suppression reflects inherent limits on worker bargaining power in any large-scale labor market: information asymmetries, employer collective action, and capital mobility create structural floors on worker compensation. However, the magnitude of suppression observed in data contradicts the mountain classification — the engine will flag this as a false summit, revealing that 'inherent to market dynamics' naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(labor_market_wage_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_wage_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_wage_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_wage_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_wage_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_market_wage_suppression, TR),
    TR >= 0.70.

:- end_tests(labor_market_wage_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Wage suppression has quantifiable economic impact: studies show workers in concentrated labor markets earn 5-15% below competitive rates. The measurement interval shows growth from 0.35 to 0.58, reflecting increasing employer concentration and declining union density. Not maximal (0.80+) because some coordination function is genuine — labor matching and skill development infrastructure do create value. Suppression (0.72): High. Multiple mechanisms reduce worker exit options: geographic immobility (relocation costs, family obligations, housing markets), credential requirements (education, licensing), information asymmetries about wage norms and alternatives, weak unemployment insurance, and employer collective action (wage suppression agreements, non-compete clauses). Capital mobility creates a credible threat of offshoring or automation, further suppressing outside options. Theater ratio (0.38): Moderate-low. Wage suppression operates through structural economic mechanisms rather than performative ones. Markets are real, employer power is real, worker constraints are real. This is not theatrical suppression — workers are not deceived about extraction; they experience it directly. Theater appears in the theoretical justifications (neoclassical consensus) rather than in the suppression mechanism itself.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. Immobilized workers classify it as Snare (pure extraction, no coordination benefit). Employers classify it as Rope (coordination of labor supply). Unions classify it as Tangled Rope (genuine labor coordination alongside extraction of union power). Progressive policy sees Scaffold (temporary support with political sunset). Neoclassical economists see Piton (degraded theory maintained by institutional inertia). The analytical observer risks seeing Mountain (inherent limit on worker bargaining in any large market). These gaps reflect genuine structural differences: the constraint creates different experiences and exit options for different agents. The largest gap is between the employer's Rope (wage suppression enables predictable labor cost management) and the trapped worker's Snare (no exit, pure extraction). The medium gap is between the policy coalition's Scaffold (temporary, solvable) and the analytical observer's Mountain (inherent to market dynamics).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) differ substantially across perspectives because agents have different structural relationships to the extraction. Trapped workers experience d ≈ 0.95 (full targets), generating high f(d) ≈ 1.42. Cost-burdened mobile workers experience d ≈ 0.65-0.75 (significant extraction but with some agency), generating f(d) ≈ 1.00. Employers experience d ≈ 0.15-0.20 (net beneficiaries), generating negative f(d) ≈ -0.01 to 0.02. Unions experience d ≈ 0.70 (targets of employer action but organized), generating f(d) ≈ 1.10. Policy coalitions experience d ≈ 0.50 (symmetric pressure — fighting suppression but facing political limits), generating f(d) ≈ 0.65. The Tangled Rope classification holds because the constraint possesses both genuine coordination (d_beneficiary approaches 0, indicating coordination benefit) and asymmetric extraction (d_victim approaches 1.0, indicating victimization). The scope modifier σ(S) = 1.0 (national scope) reflects that suppression is primarily a national labor market phenomenon, though global supply chains enable international arbitrage.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via perspectival indexing. The mandatrophy is not 'is wage suppression Rope or Snare?' but 'from which structural position and with what exit options?' The constraint genuinely coordinates labor supply and matches workers to jobs (Rope functions). It simultaneously extracts wealth asymmetrically from low-bargaining-power workers (Snare mechanisms). Both are true simultaneously. The Tangled Rope classification at the analytical level captures this: the constraint requires active enforcement (employers must enforce no-poaching agreements, suppress union organizing, maintain geographic fragmentation) and possesses both beneficiaries (employers, capital holders) and victims (suppressed workers). The false summit occurs in the Piton perspective (neoclassical theory) and the Mountain perspective (natural law). The engine will flag these as misclassifications, revealing that 'this is how competitive markets work' and 'worker bargaining power is inherent to markets' are naturalizations of contingent institutional arrangements (employer concentration, capital mobility credibility, worker immobility, union decline). The true classification holds across structural positions: Tangled Rope at the analytical level with substantial perspectival variation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monopsony_vs_competition_threshold,
    'What market concentration threshold transitions labor markets from competitive to monopsonistic wage suppression?',
    'Labor market concentration index (HHI) correlation with wage growth; cross-industry analysis of wage suppression vs. employer concentration ratios',
    'If threshold crossed, classification shifts from Rope (market coordination) to Snare (pure extraction) for affected workers. Current evidence suggests many regional labor markets exceed monopsony thresholds, but there is no consensus threshold definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopsony_vs_competition_threshold, empirical, 'Market concentration threshold for monopsonistic wage suppression').

omega_variable(
    geographic_mobility_barriers_quantification,
    'What proportion of wage suppression is attributable to worker geographic immobility vs. employer monopsony power?',
    'Cross-regional wage differential analysis; worker migration elasticity studies; counterfactual modeling of wage outcomes under increased mobility',
    'If mobility barriers dominant: worker classification shifts toward trapped. If employer power dominant: classification shifts toward snare independent of mobility. Current evidence suggests both mechanisms operate with unclear relative magnitudes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_mobility_barriers_quantification, empirical, 'Relative contribution of mobility barriers vs. monopsony power').

omega_variable(
    alternative_wage_coordination_sufficiency,
    'Do sectoral bargaining, profit-sharing, and cooperative ownership models provide genuine alternatives to suppression, or do they merely redistribute suppression across different worker classes?',
    'Longitudinal wage growth comparison: countries/sectors with sectoral bargaining vs. those with decentralized labor markets; analysis of wage distribution within alternative models',
    'If genuine alternatives exist: scaffold perspective confirmed with real sunset pathway. If suppression persists: scaffold may be aspirational rather than structural, and alternative models shift suppression rather than eliminate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_wage_coordination_sufficiency, empirical, 'Whether alternative wage coordination models eliminate or redistribute suppression').

omega_variable(
    capital_mobility_constraint_looseness,
    'What is the true elasticity of capital relocating in response to wage demands — how mobile is capital actually?',
    'Empirical study of production relocation in response to labor cost changes; sector-by-sector analysis of offshoring propensity; automation investment elasticity',
    'If capital truly mobile (low relocation cost): employer arbitrage option is structural and suppression is inescapable. If capital constrained (high relocation cost): suppression mechanisms become negotiable through political action.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_mobility_constraint_looseness, empirical, 'Capital mobility elasticity in response to wage demands').

omega_variable(
    identity_locked_labor_commitment,
    'To what degree is suppression maintained by workers'' internalized commitment to labor market participation and identity fusion with employment, vs. by structural barriers to exit?',
    'Post-pandemic labor market dynamics analysis; worker preference surveys regarding work-life tradeoffs; cross-cultural analysis of labor participation identity',
    'If identity-locked: classification includes identity_locked exit options for some worker perspectives. If structural barriers dominant: classification remains trapped/constrained. Current evidence suggests both mechanisms operate in different demographic groups.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_labor_commitment, conceptual, 'Identity-fusion vs. structural-barrier mechanisms in labor market participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_wage_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lwsup_tr_t0, labor_market_wage_suppression, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lwsup_tr_t20, labor_market_wage_suppression, theater_ratio, 20, 0.32).
narrative_ontology:measurement(lwsup_tr_t40, labor_market_wage_suppression, theater_ratio, 40, 0.38).
narrative_ontology:measurement(lwsup_tr_t10, labor_market_wage_suppression, theater_ratio, 10, 0.28).

% Extraction over time
narrative_ontology:measurement(lwsup_be_t0, labor_market_wage_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lwsup_be_t20, labor_market_wage_suppression, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(lwsup_be_t40, labor_market_wage_suppression, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(lwsup_be_t10, labor_market_wage_suppression, base_extractiveness, 10, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_wage_suppression, resource_allocation).
narrative_ontology:affects_constraint(labor_market_wage_suppression, gig_economy_labor_classification).
narrative_ontology:affects_constraint(labor_market_wage_suppression, union_power_degradation).
narrative_ontology:affects_constraint(labor_market_wage_suppression, monopsony_employer_concentration).

% DUAL FORMULATION NOTE:
% Wage suppression is upstream of specific sectoral constraints (gig economy classification, union power decline) and represents a general labor market mechanism. Decomposition is possible: monopsony power in specific industries (e.g., nursing, meat processing, retail) exhibits higher extractiveness (0.70+); coordinated labor markets (e.g., Germanic sectoral bargaining) exhibit lower extractiveness (0.15-0.25). This story represents the aggregate national pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_market_wage_suppression, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
