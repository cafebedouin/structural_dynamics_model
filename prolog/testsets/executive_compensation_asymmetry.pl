% ============================================================================
% CONSTRAINT STORY: executive_compensation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_executive_compensation_asymmetry, []).

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
 *   constraint_id: executive_compensation_asymmetry
 *   human_readable: Executive Compensation Asymmetry
 *   domain: economic/corporate_governance
 *
 * SUMMARY:
 *   Executive compensation asymmetry represents a structural constraint on
 *   labor income distribution and wage formation across the developed
 *   economy. Since the 1980s, CEO-to-worker compensation ratios have expanded
 *   from approximately 20:1 to 300:1+ in large U.S. corporations, while
 *   median real wages have stagnated. This constraint exhibits mixed
 *   characteristics: it coordinates incentives (legitimate principal-agent
 *   problem between shareholders and executives) while simultaneously
 *   enabling extraction (executives capture productivity gains that
 *   historically flowed to workers). The constraint's mechanism combines
 *   performative institutional architecture (compensation committees,
 *   independent director structures, benchmarking studies) with real power
 *   asymmetries (executives control information, influence board selection,
 *   can arbitrage to competing firms). The theater has increased over time as
 *   complexity of compensation packages has grown, masking the underlying
 *   extraction through apparent expertise and rigor. Reform coalitions
 *   propose stakeholder governance and ratio caps as sunset mechanisms,
 *   suggesting the constraint is not immutable. However, institutional
 *   inertia and the ratchet effect in compensation (rarely reversible)
 *   suggest long time horizons for any transformation.
 *
 * KEY AGENTS:
 *   - Workers: Primary victims (powerless/trapped) — wage stagnation across decades, information asymmetries about compensation structures, limited exit options due to job market segmentation
 *   - Organized Labor: Secondary actor (organized/constrained) — retains some coordination power in collective bargaining but exempted from executive compensation norms; faces declining unionization and decentralized bargaining
 *   - Executives: Primary beneficiaries (powerful/mobile) — capture asymmetric bonuses, equity options, and golden parachutes; high exit capacity but coordinated via peer norms
 *   - Institutional Investors: Secondary beneficiary (institutional/arbitrage) — benefit from shareholder value optimization narrative; can arbitrage between holdings but compensation schemes built into valuation
 *   - Compensation Consultants: Tertiary beneficiary (institutional/arbitrage) — profit from complexity; create benchmarking methodologies that ratchet wages upward
 *   - Compensation Committees: Performative institutional actor (institutional/arbitrage) — maintain legitimation theater; ostensibly independent but dependent on executive-selected consultants and peer group definitions
 *   - Reform Coalition: Organized agents (organized/constrained) — progressive legislators, governance activists, stakeholder capitalism advocates seeking sunset mechanisms via say-on-pay votes and ratio caps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(executive_compensation_asymmetry, 0.58).
domain_priors:suppression_score(executive_compensation_asymmetry, 0.68).
domain_priors:theater_ratio(executive_compensation_asymmetry, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(executive_compensation_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(executive_compensation_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(executive_compensation_asymmetry, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(executive_compensation_asymmetry, tangled_rope).
narrative_ontology:human_readable(executive_compensation_asymmetry, "Executive Compensation Asymmetry").
narrative_ontology:topic_domain(executive_compensation_asymmetry, "economic/corporate_governance").

domain_priors:requires_active_enforcement(executive_compensation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(executive_compensation_asymmetry, executives).
narrative_ontology:constraint_beneficiary(executive_compensation_asymmetry, institutional_investors).
narrative_ontology:constraint_beneficiary(executive_compensation_asymmetry, compensation_consultants).
narrative_ontology:constraint_victim(executive_compensation_asymmetry, workers).
narrative_ontology:constraint_victim(executive_compensation_asymmetry, shareholders).
narrative_ontology:constraint_victim(executive_compensation_asymmetry, public_coffers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKERS (SNARE) — Trapped by labor market segmentation and information asymmetry. Wage stagnation across decades while executive compensation accelerates. No meaningful exit: retraining costs, relocation barriers, family dependencies. Suppression mechanisms: wage secrecy norms, stock market framing, ratchet effects in labor supply. Maximum extraction experienced.
constraint_indexing:constraint_classification(executive_compensation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED LABOR (TANGLED ROPE) — Constrained by declining unionization and decentralized bargaining, but retains some coordination function. Collective agreements still structure compensation across many firms, but the ceiling has shifted — rank-and-file can negotiate collective benefit, while executives capture the productivity gains. Mixed mechanism: real coordination (labor standards, benefit pooling) alongside asymmetric extraction (executives exempt from collective constraints).
constraint_indexing:constraint_classification(executive_compensation_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTORS (ROPE) — Powerful beneficiaries with arbitrage capacity. Can switch between stock holdings, but executive compensation is built into valuation models. Experience the constraint as coordination mechanism: aligning executive incentives with shareholder returns supposedly solves principal-agent problem. Low suppression for this agent — can exit individual positions, can influence boards. Effective extraction runs toward this group.
constraint_indexing:constraint_classification(executive_compensation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXECUTIVES (TANGLED ROPE) — Powerful beneficiaries with high mobility. Compensation schemes coordinate effort incentives (valid coordination function) but extract value via asymmetric bonus structure, equity options, golden parachutes. Can exit easily to rival firms or private equity. Suppression is low (can negotiate, can leave), but the constraint maintains extraction through market-wide norm coordination: if one executive refuses equity packages, board will replace them with one who accepts. Extraction mediated through coordination norm, not force.
constraint_indexing:constraint_classification(executive_compensation_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized activists, progressive legislators, and governance reformers see this as a temporary institutional failure with a resolvable sunset. Say-on-pay votes, ratio caps, clawback provisions, and stakeholder capitalism models represent alternative pathways. Low suppression because this coalition can organize and advocate. Theater is moderate (compliance theater around compensation committees), not dominant. Classification derives from sunset clause potential: if stakeholder governance or stronger wage/benefit ratios replace shareholder primacy, the extraction mechanism loses force.
constraint_indexing:constraint_classification(executive_compensation_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPENSATION COMMITTEE STRUCTURE (PITON) — The institutional apparatus around compensation setting (committees, consultants, benchmarking studies) is largely performative. Committees are nominally independent but rely on executive-selected consultants; benchmarking methodologies ratchet wages upward by design (executives compare themselves to top quartile of peer group, pulling entire distribution higher). The theater ratio is high (elaborate committee deliberation, independent director claims) but the functional outcome is predetermined. Piton classification: the structure persists through inertia and legitimation theater, not because it solves the incentive alignment problem effectively.
constraint_indexing:constraint_classification(executive_compensation_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk perspective that frames executive compensation asymmetry as an inevitable consequence of labor market economics: scarce talent, winner-take-all markets, and information asymmetry are structural features of capitalism itself. From this view, the constraint is immutable — compensation gaps are natural law-like outcomes of market sorting. However, this naturalizes contingent institutional arrangements (at-will employment, equity option schemes, corporate tax incentives) that could be redesigned. False summit detection: cross-national variance (Nordic companies with lower CEO/worker ratios) shows the constraint is not immutable.
constraint_indexing:constraint_classification(executive_compensation_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(executive_compensation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(executive_compensation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(executive_compensation_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(executive_compensation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(executive_compensation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(executive_compensation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The constraint shows genuine coordination function (aligning executive effort incentives with shareholder returns has theoretic validity), but extraction mechanisms are substantial. CEO compensation growth rates (average ~7% annually) far exceed worker compensation growth (average ~1-2% annually) despite correlated productivity measures, indicating extraction beyond incentive alignment. The value has increased from 0.32 to 0.58 over the measurement interval, reflecting accumulated asymmetry and ratchet effects — compensation levels rarely reset downward, creating monotonic extraction accumulation. Suppression (0.68): High. Workers face substantial barriers to exit: job market segmentation by skill/credential, relocation costs, family dependencies, information asymmetries about compensation equity, and wage secrecy norms that prevent comparison across firms. Organized labor faces declining unionization (U.S. private sector union density ~6%) and decentralized bargaining that reduces collective power. Suppression mechanisms are structural (labor market conditions) rather than explicitly coercive, but the effect is similar — workers cannot easily exit or renegotiate. Theater ratio (0.64): Moderate-high. Compensation committee processes, independent director assessments, and benchmarking methodologies constitute substantial performative activity. The apparatus is complex (proxy statements run 50+ pages detailing compensation 'philosophy') but functional outcomes are predetermined. Benchmarking studies typically compare to top quartile of peer group, mechanically ratcheting all compensation upward. The theater has increased from 0.48 to 0.64 as complexity has grown, suggesting Goodhart drift — compensation committees are increasingly focused on process legitimacy rather than actual incentive alignment.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically divergent classifications from different structural positions. Workers (powerless/trapped) classify it as snare: maximum extraction with no exit. Organized labor (organized/constrained) experiences it as tangled rope: real coordination function for collective agreements, but executives exempt from collective wage ceilings, creating asymmetric extraction. Institutional investors (institutional/arbitrage) see rope: coordination mechanism (aligning executives with shareholders) with low suppression (can arbitrage between holdings). Executives (powerful/mobile) experience tangled rope: coordination function (effort incentives) but with asymmetric capture of gains and lower suppression than workers face. Reform coalitions (organized/constrained) see scaffold: temporary institutional failure with viable sunset mechanisms (stakeholder governance, ratio caps) that could restructure the constraint. Compensation committees (institutional/arbitrage) maintain piton classification: elaborate performative structure (independent directors, benchmarking, committee deliberations) that persists through inertia despite functional degradation. The analytical observer risks mountain classification (natural outcome of labor market sorting), but the substantial cross-national variance in CEO/worker ratios (Nordic firms maintain 20-40:1 ratios despite comparable markets) reveals the constraint as contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to the extraction flow. Workers (powerless/trapped) experience maximum extraction: d ≈ 0.95. They are both victims and lack exit capacity, producing high f(d) and maximum experienced extractiveness (χ near 1.0). Organized labor (organized/constrained) experiences moderate extraction: d ≈ 0.55. They are partially victims (wage growth suppressed relative to productivity) but retain some coordination power and constrained exit (can organize alternative contracts or shift industries). Institutional investors (institutional/arbitrage) derive low d ≈ 0.15: they are beneficiaries with high exit capacity, producing negative f(d) and extraction flowing toward them. Executives (powerful/mobile) derive d ≈ 0.25-0.35: they are beneficiaries but experience pseudo-extraction through peer coordination norms (if one executive refuses equity schemes, board replaces them). The constraint's extraction is mediated through coordination norm rather than force, creating lower suppression than workers experience. Compensation consultants and committees (institutional/arbitrage) derive low d values by beneficiary status, but their effective role is servicing executive preference extraction. The directionality computation captures the key asymmetry: workers bear the extracted value; executives and institutional investors receive it; the apparatus (consultants, committees) extracts fees from managing the flow.
 *
 * MANDATROPHY ANALYSIS:
 *   TENSION RESOLUTION: The constraint satisfies tangled rope requirements — genuine coordination function (executive effort incentives + shareholder alignment) exists alongside asymmetric extraction (compensation growth outpacing worker growth + ratchet effects). The classification is not mandatrophic because both functions are verifiable and necessary to explain the structural data. If we removed the coordination function (assume compensation is purely extractive rental), we would predict: (a) immediate executive exit to firms with better compensation packages (not observed — executives remain with even poorly-compensated peer groups due to norm coordination), (b) shareholder dissatisfaction with unaligned executive incentives (not universal — many shareholders explicitly defend high compensation as necessary incentive), (c) constant ratcheting of compensation to extract maximum (observed, but constrained by stakeholder pressure and governance norms — not unlimited extraction). The tangled rope classification preserves both observations: genuine coordination is happening (norm coordination among executives, incentive alignment among shareholders) AND genuine extraction is happening (worker wages suppressed, CEO-worker ratios expanding). Mandatrophy is resolved by recognizing that the two mechanisms are separable in principle: stakeholder governance reforms could preserve the coordination function (executive effort incentives) while removing or reducing the extraction mechanism (asymmetric bonus/equity capture). This disaggregation is what the reform coalition perspective instantiates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_productivity_contribution,
    'What proportion of executive compensation reflects marginal productivity contribution vs. extraction via asymmetric bargaining power?',
    'Longitudinal comparison of executive compensation changes vs. firm performance; cross-sectional comparison across firms with similar performance but different compensation structures; analysis of compensation changes after executive replacement',
    'If marginal productivity dominates: constraint should classify as lower-extraction rope. If asymmetric bargaining dominates: constraint is snare/tangled rope. Splits the classification between compensation-as-incentive and compensation-as-extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_productivity_contribution, empirical, 'Productivity contribution vs. extraction in executive compensation').

omega_variable(
    stakeholder_capitalism_viability,
    'Can stakeholder governance models and ratio-capped compensation actually displace shareholder primacy, or are they captured by existing power structures?',
    'Longitudinal tracking of firms adopting stakeholder governance; measurement of actual wage/benefit ratio trends; analysis of board composition changes and decision-making autonomy post-reform',
    'If viable: scaffold classification is confirmed and sunset mechanism is real (20-40 year horizon). If captured: reform is theater and classification should trend toward piton/snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_capitalism_viability, empirical, 'Viability of stakeholder capitalism as alternative to shareholder primacy').

omega_variable(
    global_wage_suppression_coupling,
    'Does executive compensation asymmetry in developed markets mechanically suppress wages in developing markets via supply chain integration and labor arbitrage?',
    'Analysis of wage trends in offshore manufacturing relative to executive compensation trends in parent companies; identification of explicit cost-cutting mandates linked to executive bonus targets',
    'If coupled: the extraction is internationalizing and suppression is increasing over time. Measurement trajectory should show rising extractiveness. If decoupled: extraction is primarily domestic and may stabilize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_wage_suppression_coupling, empirical, 'Coupling between executive compensation and global wage suppression').

omega_variable(
    equity_option_illusion,
    'Do equity compensation schemes genuinely align executive and worker incentives, or do they create illusory wealth that extracts from workers during downturns and concentrates gains during booms?',
    'Longitudinal analysis of equity-based compensation during market crashes vs. booms; comparison of executive wealth retention across economic cycles; measurement of worker benefit/debt changes during same periods',
    'If illusory: equity-based compensation is a mechanism for extracting downside risk to workers while concentrating upside to executives. Suppression values should increase and extractiveness classification should shift higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_option_illusion, empirical, 'Whether equity compensation genuinely aligns incentives or transfers risk').

omega_variable(
    labor_market_segmentation_threshold,
    'At what compensation ratio does the labor market segment into immobile and mobile tiers, preventing worker exit and creating trap dynamics?',
    'Cross-sectional analysis of labor mobility by compensation decile; measurement of retraining and relocation costs relative to wage differentials; tracking of career trajectory divergence across compensation distribution',
    'If threshold < 10:1 ratio: trap dynamics emerge at current levels. If threshold > 20:1 ratio: current ratios leave some exit capacity. Determines whether classification should shift from snare toward constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_segmentation_threshold, empirical, 'Labor market segmentation threshold creating trap dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(executive_compensation_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exec_comp_tr_t0, executive_compensation_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(exec_comp_tr_t10, executive_compensation_asymmetry, theater_ratio, 10, 0.56).
narrative_ontology:measurement(exec_comp_tr_t20, executive_compensation_asymmetry, theater_ratio, 20, 0.64).
narrative_ontology:measurement(exec_comp_tr_t5, executive_compensation_asymmetry, theater_ratio, 5, 0.52).
narrative_ontology:measurement(exec_comp_tr_t15, executive_compensation_asymmetry, theater_ratio, 15, 0.6).

% Extraction over time
narrative_ontology:measurement(exec_comp_be_t0, executive_compensation_asymmetry, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(exec_comp_be_t10, executive_compensation_asymmetry, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(exec_comp_be_t20, executive_compensation_asymmetry, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(exec_comp_be_t5, executive_compensation_asymmetry, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(exec_comp_be_t15, executive_compensation_asymmetry, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(executive_compensation_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(executive_compensation_asymmetry, labor_market_segmentation).
narrative_ontology:affects_constraint(executive_compensation_asymmetry, shareholder_primacy_norm).
narrative_ontology:affects_constraint(executive_compensation_asymmetry, income_inequality_ratchet).

% DUAL FORMULATION NOTE:
% Executive compensation asymmetry decomposes into distinct structural constraints: (1) labor market segmentation (worker mobility barriers) drives suppression; (2) shareholder primacy norm (executive incentive alignment) drives coordination function; (3) compensation committee theater (benchmarking ratchet effects) drives extraction accumulation. This story models the unified constraint; upstream stories should address segmentation and norm separately to reveal how they couple into the unified extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(executive_compensation_asymmetry, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
