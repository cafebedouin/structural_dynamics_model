% ============================================================================
% CONSTRAINT STORY: labor_arbitrage_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_arbitrage_suppression, []).

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
 *   constraint_id: labor_arbitrage_suppression
 *   human_readable: Labor Arbitrage Suppression in Global Supply Chains
 *   domain: labor/economics/global_supply_chains
 *
 * SUMMARY:
 *   Labor arbitrage suppression describes the structural enforcement of wage
 *   differentials across jurisdictions through restrictions on worker
 *   mobility (immigration law, visa systems, credential non-recognition) and
 *   capital protections (tariffs, trade agreements with labor side-letters).
 *   The constraint benefits capital-holding firms and high-wage labor markets
 *   while extracting from low-wage workers and the global labor market
 *   integrity. The extractiveness value (0.58) reflects growing accumulation:
 *   as manufacturing globalization has deepened, capital has gained the
 *   ability to arbitrage labor costs while suppression mechanisms have
 *   hardened (visa restrictions, border enforcement, credential barriers).
 *   The suppression value (0.72) is high: legal, economic, and informational
 *   barriers make exit nearly impossible for trapped workers. Theater ratio
 *   (0.48) is moderate: enforcement involves genuine legal/enforcement
 *   infrastructure rather than purely performative ritual, though labor
 *   regulation in high-wage countries has become increasingly theatrical as
 *   production moves offshore. This constraint exhibits five of the six DR
 *   types: snare for trapped workers, rope for beneficiary firms, tangled
 *   rope for organized labor in high-wage jurisdictions, piton for degraded
 *   domestic labor regulation, scaffold for regional harmonization movements,
 *   and a false mountain from the naturalizing analytical view.
 *
 * KEY AGENTS:
 *   - Low-Wage Workers (Global South): Primary victims (powerless/trapped) — bear extraction through suppressed wages, restricted mobility, enforced geographic immobility
 *   - Labor Market Integrity: Primary collective victim (powerless/trapped) — abstract good damaged by wage suppression, cannot exit or organize
 *   - Capital-Holding Firms: Primary beneficiaries (institutional/arbitrage) — extract maximum value by accessing suppressed labor while retaining full capital mobility
 *   - Organized Labor (High-Wage Jurisdictions): Secondary actor (organized/constrained) — benefits from wage protection but constrained by threat of capital exit, maintains enforcement through political action
 *   - Nation-State Labor Regulation: Institutional actor (institutional/arbitrage) — maintains performative enforcement domestically while actual production and enforcement leak offshore
 *   - Regional Labor Harmonization Movements: Organized reformers (organized/constrained) — building alternative enforcement pathways with generational sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent enforcement as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_arbitrage_suppression, 0.58).
domain_priors:suppression_score(labor_arbitrage_suppression, 0.72).
domain_priors:theater_ratio(labor_arbitrage_suppression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_arbitrage_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_arbitrage_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(labor_arbitrage_suppression, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_arbitrage_suppression, snare).
narrative_ontology:human_readable(labor_arbitrage_suppression, "Labor Arbitrage Suppression in Global Supply Chains").
narrative_ontology:topic_domain(labor_arbitrage_suppression, "labor/economics/global_supply_chains").

domain_priors:requires_active_enforcement(labor_arbitrage_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_arbitrage_suppression, capital_holding_firms).
narrative_ontology:constraint_beneficiary(labor_arbitrage_suppression, consumer_markets_high_income).
narrative_ontology:constraint_victim(labor_arbitrage_suppression, low_wage_workers).
narrative_ontology:constraint_victim(labor_arbitrage_suppression, labor_market_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-WAGE WORKERS (SNARE) — Trapped by geography, visa restrictions, and economic necessity. Exit options are severely constrained: cannot move to higher-wage jurisdictions (immigration barriers), cannot refuse work (economic dependency), cannot organize (enforcement against collective action). Experiences maximum extraction with suppression of alternatives enforced through legal and economic coercion.
constraint_indexing:constraint_classification(labor_arbitrage_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR MARKET INTEGRITY (SNARE) — Collective epistemic and economic good that cannot exit. Wage suppression in one jurisdiction exerts downward pressure on others through competitive dynamics. The integrity of labor markets as price-discovery mechanisms is compromised; wages reflect coercion-enabled arbitrage rather than true scarcity. This agent bears extraction cost with no exit option and no organizational capacity.
constraint_indexing:constraint_classification(labor_arbitrage_suppression, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CAPITAL-HOLDING FIRMS (ROPE) — Primary beneficiary with maximum exit options through capital mobility and geographic arbitrage. Experiences the constraint as a coordination solution: labor mobility restrictions enable stable low-wage supply chains by preventing wage escalation through arbitrage. Net flow of extraction is toward this agent; they are the beneficiary of suppression enforcement.
constraint_indexing:constraint_classification(labor_arbitrage_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED LABOR MOVEMENT (TANGLED ROPE) — Organized agents in high-wage countries face mixed extraction and coordination. They benefit from labor suppression in low-wage jurisdictions (protects domestic wage floors) but also lose bargaining power as global supply chains enable capital mobility. They sustain enforcement through political action (trade policy, immigration restriction, tariffs) that maintains the arbitrage suppression, creating both coordination benefit (wage stability) and extractive cost (foreclosed global labor solidarity, reduced negotiating power against capital flight).
constraint_indexing:constraint_classification(labor_arbitrage_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NATION-STATE LABOR REGULATION (PITON) — Labor law and minimum wage enforcement in high-wage countries is substantially performative: multinational supply chains route production to jurisdictions with minimal enforcement, making domestic regulation effective only for domestic production. The ritual of labor protection persists (labor boards, wage councils, union contracts) but the functional enforcement has degraded as capital exits to lower-enforcement jurisdictions. Theater maintained through nationalist framing ('protect local workers') while actual function is minimal.
constraint_indexing:constraint_classification(labor_arbitrage_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL LABOR HARMONIZATION (SCAFFOLD) — Organized movement toward regional labor standards (EU minimum wages, USMCA labor provisions, RCEP labor chapters) represents a temporary scaffold: creating enforcement mechanisms that raise the floor while capital mobility remains partially constrained within regional blocs. If successful over a generational timescale, regional harmonization could create multiple higher-wage equilibria, breaking the single global arbitrage equilibrium. This perspective sees suppression as a resolvable coordination failure with a sunset: once regional blocs establish labor standards, the arbitrage suppression mechanism loses force.
constraint_indexing:constraint_classification(labor_arbitrage_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, wage differences between jurisdictions are inherent to uneven development and capital accumulation. Labor arbitrage suppression could appear as an immutable consequence of differential productivity, education, and capital endowment — a natural law of global economics. However, this perspective misidentifies contingent enforcement mechanisms (immigration law, visa restrictions, trade policy) as natural facts. The structural data reveals this as false summit: the suppression is actively maintained through institutional action, not inherent to physics or logic.
constraint_indexing:constraint_classification(labor_arbitrage_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_arbitrage_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_arbitrage_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_arbitrage_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_arbitrage_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_arbitrage_suppression, TR),
    TR >= 0.70.

:- end_tests(labor_arbitrage_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extraction has increased over the 30-year interval as globalization deepened and capital mobility increased. At t=0 (1996), manufacturing was still regionally concentrated and labor arbitrage was constrained by communication/transportation costs. By t=30 (2026), supply chains are optimized for wage arbitrage with minimal transaction friction. The rising trajectory reflects growing rent-taking: firms can now access 10x wage differentials (US: $25/hour vs Bangladesh: $2.50/hour) with low friction, extracting the differential as profit. Suppression (0.72): High and stable. Immigration restrictions, visa requirements, credential non-recognition, and border enforcement have not relaxed despite labor mobility rhetoric — they have intensified (biometric borders, deportation machinery). Legal structures enforcing geographic wage differentiation have strengthened over the interval. Theater ratio (0.48): Low-moderate. Unlike some extractive constraints that hide behind ritual, labor arbitrage suppression is enforced through explicit legal mechanisms. Immigration law is not theater — it is direct structural enforcement. However, labor regulation in high-wage countries has become theatrical as enforcement has offshore-shifted: minimum wage boards and union contracts perform the function of labor protection while actual industrial production has moved beyond their jurisdiction. The theater has increased slightly as domestic regulation has become increasingly divorced from actual production location.
 *
 * PERSPECTIVAL GAP:
 *   The gap between trapped worker and institutional beneficiary is the core diagnostic: the same constraint (labor mobility restrictions + wage suppression) produces snare from the trapped perspective and rope from the beneficiary perspective. This maximal gap reveals the constraint's extractive function. The organized labor perspective (tangled rope) shows partial consciousness of extraction — they benefit domestically but lose globally. The piton perspective reveals degradation: regulation persists as legal/institutional structure while function has leaked offshore. The scaffold perspective (regional harmonization) is forward-looking, suggesting the constraint could be disrupted. The false mountain reveals how naturalizing language ('inevitable wage differences,' 'economic gravity') obscures contingent enforcement. The perspectival architecture shows a snare being maintained through institutional layering and legitimation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position. Low-wage workers are trapped victims: their d approaches 1.0 (maximum target), f(d) high, experienced χ maximum. Capital-holding firms are beneficiaries with arbitrage exit: their d approaches 0.0 (full beneficiary), f(d) negative or minimal, experienced χ becomes negative or near-zero (they experience the constraint as enabling, not extractive). Organized labor in high-wage countries have mixed structural position: they benefit from suppression protection (d lower) but face extraction from global wage competition (d higher) — the net d is intermediate, producing moderate χ and tangled rope classification. The piton perspective (institutional labor regulation) has arbitrage exit (capital can still leave) so derives low d, but the theater gate dominates — the actual enforcement function has atrophied even though the legal structure persists. Regional harmonization movements have constrained exit (cannot escape global supply chains) so derive intermediate d, but the scaffold classification reflects the generational sunset: the constraint is perceived as resolvable, not permanent.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandate/trophy tension by distinguishing coordination function from extraction mechanism. The beneficiary would claim the constraint is pure coordination — efficiently matching labor supply to capital needs across geographies. The victim would claim pure extraction — deliberately suppressing wages through legal coercion. The tangled rope perspective shows both are partially correct: genuine coordination of global supply chains occurs (firms do lower transaction costs, global production efficiency increases) but it is layered with extraction (workers bear costs of that coordination, wage suppression transfers gains to capital). The framework resolves the conflict by naming this hybrid explicitly: tangled rope requires active enforcement (yes, immigration law enforces it), benefits genuine parties (yes, firms do coordinate), and produces asymmetric extraction (yes, workers bear disproportionate costs). No false natural law — just a snare dressed in coordination language. The piton perspective adds degradation: labor regulation persists theatrically long after real enforcement has moved offshore, suggesting the institutional structure has outlived its stated function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_nature,
    'Is labor arbitrage suppression enforced primarily through legal/state mechanisms (visa, immigration, trade barriers) or through structural factors (language, cultural distance, credential non-recognition)?',
    'Counterfactual analysis: if legal barriers were removed but structural barriers remained, would wage convergence accelerate? Historical case studies of labor movement liberalization (EU freedom of movement, post-Soviet migration).',
    'If primarily legal: the constraint is a contingent snare that policy can dismantle. If primarily structural: even legal liberalization will not eliminate suppression — reclassify as mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, empirical, 'Relative contribution of legal vs structural mechanisms to labor mobility barriers').

omega_variable(
    wage_suppression_flow_direction,
    'Does labor suppression in low-wage jurisdictions reduce global wage inequality or widen it by preventing higher-wage jurisdictions from facing labor scarcity pressures?',
    'Global wage distribution analysis; counterfactual modeling of wage distribution if arbitrage suppression were removed; comparison of wage inequality trends pre/post-liberalization in migrant-sending vs migrant-receiving regions.',
    'If suppression widens inequality: victim classification accurate, snare diagnosis confirmed. If suppression reduces inequality: classification shifts toward rope/tangled_rope (complex distributional effects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_flow_direction, empirical, 'Direction of inequality effects from labor arbitrage suppression').

omega_variable(
    regional_harmonization_feasibility,
    'Can regional labor standards actually constrain capital mobility within blocs, or does enforcement still leak to lower-cost jurisdictions outside the bloc (Bangladesh sourcing instead of Mexican sourcing)?',
    'Supply chain geographic analysis before/after implementation of regional labor standards (EU, USMCA); tracking of capital relocation patterns in response to regional wage floor increases.',
    'If leak is minimal: scaffold sunset is real and medium confidence. If leak is substantial: regional harmonization merely relocates arbitrage rather than eliminating it — scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_harmonization_feasibility, empirical, 'Whether regional labor standards can contain capital arbitrage').

omega_variable(
    organized_labor_benefit_asymmetry,
    'Does organized labor in high-wage countries benefit net from global arbitrage suppression (protected domestic wages) or lose net (foreclosed wage growth from labor scarcity, reduced bargaining power against credible capital flight threat)?',
    'Wage growth comparisons: organized labor sectors where arbitrage suppression is strongest vs sectors where competition is open; analysis of bargaining power elasticity to capital mobility threats.',
    'If net benefit: organized labor''s tangled rope classification as beneficiary-victim hybrid is accurate. If net cost: reclassify organized labor as secondary victim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(organized_labor_benefit_asymmetry, empirical, 'Net benefit/cost of arbitrage suppression for organized labor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_arbitrage_suppression, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(las_tr_t0, labor_arbitrage_suppression, theater_ratio, 0, 0.42).
narrative_ontology:measurement(las_tr_t15, labor_arbitrage_suppression, theater_ratio, 15, 0.45).
narrative_ontology:measurement(las_tr_t30, labor_arbitrage_suppression, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(las_be_t0, labor_arbitrage_suppression, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(las_be_t15, labor_arbitrage_suppression, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(las_be_t30, labor_arbitrage_suppression, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_arbitrage_suppression, resource_allocation).
narrative_ontology:affects_constraint(labor_arbitrage_suppression, global_wage_inequality).
narrative_ontology:affects_constraint(labor_arbitrage_suppression, immigration_policy_enforcement).
narrative_ontology:affects_constraint(labor_arbitrage_suppression, supply_chain_optimization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_arbitrage_suppression, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
