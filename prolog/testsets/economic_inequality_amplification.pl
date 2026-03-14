% ============================================================================
% CONSTRAINT STORY: economic_inequality_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_economic_inequality_amplification, []).

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
 *   constraint_id: economic_inequality_amplification
 *   human_readable: Economic Inequality Amplification Through Institutional Mechanisms
 *   domain: economic/political
 *
 * SUMMARY:
 *   Economic inequality amplification is a tangled_rope constraint operating
 *   through institutional mechanisms that simultaneously coordinate resource
 *   allocation and extract value from debt-dependent and wage-dependent
 *   populations toward asset-holders and creditor classes. The constraint
 *   operates via interconnected mechanisms: progressive compression of
 *   labor's share of output, financialization of basic needs (housing,
 *   healthcare, education), tax regimes that preferentially treat capital
 *   gains over wages, monetary policy that inflates asset prices while
 *   stagnating wages, intellectual property structures that concentrate
 *   rents, and institutional degradation of labor's collective bargaining
 *   power. The constraint exhibits a full perspectival range: powerless wage
 *   workers and debt-trapped households experience snare-like extraction with
 *   material and psychological suppression; small business operators
 *   experience tangled_rope mixing genuine coordination benefits with
 *   asymmetric extraction; asset-holders and creditor institutions experience
 *   pure coordination (rope) enabling wealth accumulation; organized labor
 *   experiences constrained tangled_rope with some agency; and the welfare
 *   state has degraded to theatrical performance (piton) while naturalizing
 *   framing treats inequality amplification as natural law (mountain, flagged
 *   as false summit). The extractiveness of the constraint has risen
 *   substantially over the 40-year measurement interval (0.32 → 0.58), while
 *   theater ratio has also increased (0.25 → 0.48), suggesting gradual
 *   degradation of coordination function alongside rising extraction.
 *
 * KEY AGENTS:
 *   - Wage-Dependent Workers: Primary victims (powerless/trapped) — face stagnant nominal wages, declining real purchasing power, and minimal wealth accumulation capacity despite productivity growth
 *   - Debt-Trapped Households: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused into debt servicing; suppression combines material (credit system dependency) and psychological (moral obligation internalization) mechanisms
 *   - Small Business Operators: Secondary victims (moderate/constrained) — benefit from credit access and market infrastructure but bear disproportionate costs of capital, regulation, and vendor lock-in
 *   - Asset-Holding Class: Primary beneficiaries (institutional/arbitrage) — benefit from asset price inflation, leverage amplification, and tax-advantaged wealth accumulation with high exit options
 *   - Creditor Institutions: Primary beneficiaries (institutional/arbitrage) — profit from spread between borrowing costs and lending rates; coordinate with asset-holders through securitization and capital markets
 *   - Labor Coalition: Organized secondary actor (organized/constrained) — provides genuine coordination (bargaining, mutual aid) but constrained by legal barriers and globalized labor competition; experiences tangled_rope with some agency
 *   - Welfare State / Redistributive Apparatus: Institutional actor (institutional/constrained) — nominal redistributive function has atrophied (piton classification); maintains legitimating performance while actual redistribution declines
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(economic_inequality_amplification, 0.58).
domain_priors:suppression_score(economic_inequality_amplification, 0.65).
domain_priors:theater_ratio(economic_inequality_amplification, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(economic_inequality_amplification, extractiveness, 0.58).
narrative_ontology:constraint_metric(economic_inequality_amplification, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(economic_inequality_amplification, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(economic_inequality_amplification, tangled_rope).
narrative_ontology:human_readable(economic_inequality_amplification, "Economic Inequality Amplification Through Institutional Mechanisms").
narrative_ontology:topic_domain(economic_inequality_amplification, "economic/political").

domain_priors:requires_active_enforcement(economic_inequality_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(economic_inequality_amplification, asset_holders).
narrative_ontology:constraint_beneficiary(economic_inequality_amplification, creditor_class).
narrative_ontology:constraint_victim(economic_inequality_amplification, wage_dependent_workers).
narrative_ontology:constraint_victim(economic_inequality_amplification, small_business_operators).
narrative_ontology:constraint_victim(economic_inequality_amplification, debt_trapped_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-DEPENDENT WORKER (SNARE) — Trapped in labor market structures offering limited exit. Faces compound extraction: wage stagnation relative to productivity gains, rising cost of living (housing, healthcare, education), debt servicing obligations, and minimal wealth accumulation capacity. Cannot exit through arbitrage (no capital), organizational power (dispersed workforce), or mobility (retraining costs, geographic immobility). Experiences the constraint as pure extraction with maximum suppression — the structural barriers to exit are material and comprehensive.
constraint_indexing:constraint_classification(economic_inequality_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEBT-TRAPPED HOUSEHOLD (SNARE) — Structurally mobile (could theoretically relocate, change employment, default strategically) but identity-locked into debt servicing. The household's identity is constituted through the role of responsible borrower — default is unthinkable not because of material barriers alone but because it requires becoming a 'bad actor' in their own self-concept. The constraint combines material barriers (credit system dependency, collateral seizure threat) with internalized obligation (moral narrative of debt repayment). Suppression includes both structural (legal, financial) and internalized (shame, identity fusion) mechanisms. Extracted through monthly payments, opportunity costs, and perpetual financial precarity.
constraint_indexing:constraint_classification(economic_inequality_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS OWNER (TANGLED ROPE) — Experiences genuine coordination: credit access enables business formation, supply chain integration solves logistics problems, market infrastructure provides customer reach. But constrained by high borrowing costs relative to corporate competitors, regulatory compliance burdens, and vulnerability to predatory lending or vendor lock-in. Some agency and benefit from the system, but asymmetric extraction: costs of capital, insurance, and compliance fall disproportionately on small operators. Cannot arbitrage (no capital mobility), but not fully trapped (can exit through sale, closure, pivot).
constraint_indexing:constraint_classification(economic_inequality_amplification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ASSET-HOLDING CLASS (ROPE) — Benefits from coordination: asset price inflation (real estate, equities, bonds) creates wealth accumulation; leverage amplifies returns; tax-deferred or tax-advantaged investment vehicles compound gains. Experiences the constraint as pure coordination mechanism — the system that amplifies inequality also enables their wealth growth and provides arbitrage opportunities (sell before downturn, buy at bottom, geographic arbitrage, sector rotation). No meaningful suppression; high exit options through portfolio diversification and capital mobility.
constraint_indexing:constraint_classification(economic_inequality_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDITOR INSTITUTION (ROPE) — Experiences the constraint as pure coordination. The financial architecture that amplifies inequality also enables profitable lending, risk distribution through securitization, and arbitrage between borrowing costs (low) and lending rates (high). Coordinated with asset-holders and beneficiary classes; extracted from borrowers. Has maximal exit options (capital mobility, portfolio reallocation, regulatory arbitrage across jurisdictions).
constraint_indexing:constraint_classification(economic_inequality_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR COALITION (TANGLED ROPE) — Organized agents (unions, worker advocacy groups, mutual aid networks) experience the constraint as both coordination problem and extraction mechanism. Genuine coordination: collective bargaining improves wages and conditions relative to atomized individual negotiation; mutual aid solves access problems (healthcare, childcare); organizing creates voice where none existed. But constrained by legal barriers (right-to-work laws, union busting), fragmentation (gig economy dispersal), and globalized labor competition. Some agency (strikes, organizing, political pressure) but extraction persists through wage suppression, benefit erosion, and regulatory constraints on collective action.
constraint_indexing:constraint_classification(economic_inequality_amplification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: WELFARE STATE / REDISTRIBUTIVE APPARATUS (PITON) — Progressive taxation, social safety nets, and wealth redistribution mechanisms are largely theatrical in advanced economies. Nominal rates are high; effective rates are low (capital gains preferential treatment, carried interest, corporate tax avoidance). The redistributive apparatus persists through institutional inertia and legitimating narrative ('we care about fairness') while actual extraction flows upward. The system's primary function (reducing inequality) has atrophied; it is maintained through performance (means-tested programs, work requirements, stigma-laden benefits) rather than material redistribution. Theater ratio high; functional redistribution minimal.
constraint_indexing:constraint_classification(economic_inequality_amplification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From this perspective, inequality amplification is treated as an inherent feature of markets and human nature: 'capital seeks returns,' 'talent commands premium,' 'risk-taking must be rewarded,' 'inequality motivates effort.' This framing naturalizes contingent institutional choices (tax structure, monetary policy, corporate law, intellectual property regimes, labor law) as laws of nature. The analytical observer risks seeing mountains where contingent arrangements exist — the engine's false summit detector will flag this as naturalization rather than genuine natural law.
constraint_indexing:constraint_classification(economic_inequality_amplification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(economic_inequality_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(economic_inequality_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(economic_inequality_amplification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(economic_inequality_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(economic_inequality_amplification, TR),
    TR >= 0.70.

:- end_tests(economic_inequality_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting substantial asymmetric value transfer from wage-dependent and debt-dependent populations to asset-holding classes. Not at snare level (0.66+) because coordination functions genuinely exist (credit access enables business formation and consumption smoothing; financial markets enable capital allocation; labor markets provide employment). The extractiveness is embedded within coordination rather than pure predation. Suppression (0.65): Moderate-high, reflecting multiple overlapping suppression mechanisms: labor market power imbalances (weak worker bargaining, gig economy fragmentation), credit dependency (collateral seizure threat, credit score system), debt servicing obligations (identity-locked moral obligation), legal barriers (labor law constraints, union busting), and globalized labor competition (outsourcing threat). Theater ratio (0.48): Moderate, reflecting that inequality-amplifying mechanisms are partly functional (financial intermediation genuinely improves capital allocation) and partly theatrical (welfare state performance, meritocratic narratives masking inheritance of wealth and opportunity). The rising theater ratio (0.25 → 0.48) suggests that as core mechanisms have become more extractive, theatrical legitimation has increased to maintain political viability.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Asset-holders see rope (coordination enabling their wealth accumulation through capital markets, leverage, and tax-advantaged vehicles). Small business operators see tangled_rope (coordination provides credit access and market infrastructure; extraction shows through asymmetric costs). Wage workers see snare (trapped extraction with no exit). Debt-trapped households see snare with identity-lock (structurally mobile but identity-fused into repayment; exit would require becoming a 'bad actor'). Organized labor sees constrained tangled_rope (genuine collective voice and mutual aid coordination; extraction persists through legal and competitive suppression). The welfare state sees itself as functional coordinator but analytical inspection reveals piton-level theater (means-testing, work requirements, benefit stigma rising while actual redistribution declines). The analytical observer risks seeing mountain (inequality is natural, capital markets are efficient) — but the structural data reveals this as naturalization of contingent institutional choices (tax structure, monetary policy, corporate law, labor law) that have been deliberately shifted over 40 years toward capital and away from labor.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across agent perspectives. Wage-dependent workers and debt-trapped households occupy maximum-extraction positions (d ≈ 0.90–0.95): they are victims with trapped or identity-locked exit, they bear costs, and they have minimal arbitrage capacity. Asset-holders and creditor institutions occupy beneficiary positions (d ≈ 0.05–0.15): they are net beneficiaries with arbitrage options, extraction flows toward them. Small business operators occupy mixed positions (d ≈ 0.55–0.65): they experience both coordination benefits and asymmetric extraction; their exit is constrained but not trapped. The labor coalition occupies an organized-constrained position (d ≈ 0.40–0.50): they have some collective agency and benefits but face significant suppression and extraction. This directionality dispersion — ranging from 0.05 for creditors to 0.95 for trapped wage workers — produces the perspectival gap: the same constraint appears as pure coordination (rope) to beneficiaries, tangled_rope to mixed agents, and snare to trapped agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in economic inequality amplification is resolved by recognizing that beneficiary and victim perspectives are genuinely experiencing different structural realities, not different interpretations of the same reality. The asset-holder experiencing rope and the wage worker experiencing snare are not observing the same constraint from different angles — they are embedded in fundamentally different structural positions within the same system. The mandatrophy attempted to collapse these to a single type; the framework properly expands them across six types and links them through directionality derivation. The false summit (mountain classification) is detected because the naturalizing frame ('inequality is natural') contradicts the policy-reversibility evidence: identical technology produces vastly different inequality across countries with different institutional arrangements. The piton classification of the welfare state is central to mandatrophy resolution: the degradation of redistributive function explains why institutional legitimacy persists despite rising inequality — the welfare state's theatrical performance maintains political viability while the actual redistributive mechanism atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_returns_vs_wage_growth_causation,
    'Is the divergence between capital returns and wage growth a consequence of technological change (exogenous) or a result of institutional design and policy choices (endogenous)?',
    'Cross-national comparison: countries with identical technology but different labor law, tax structure, and monetary policy show vastly different capital-wage divergence rates. Decomposition analysis of productivity gains shows whether workers capture productivity improvements through wages or capitalists capture through returns on capital.',
    'If exogenous: constraint is mountain (technological necessity). If endogenous: constraint is tangled_rope or snare (policy-contingent extraction). Current evidence strongly favors endogenous — labor''s share collapse post-1980 correlates with union decline, top tax rate cuts, and monetary policy shifts, not with technology acceleration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_returns_vs_wage_growth_causation, empirical, 'Whether capital-wage divergence is technological or institutional in origin').

omega_variable(
    debt_trap_psychological_internalization,
    'To what extent are debt-servicing behaviors driven by material necessity (true trapped exit) versus internalized moral obligation and identity fusion (identity_locked exit)?',
    'Behavioral studies measuring default vs payment decisions when material consequences are held constant; narrative analysis of borrowers'' language about debt obligation; comparison of default rates across cultures with different debt moral narratives. Experimental conditions where legal/credit consequences are eliminated can isolate identity component.',
    'If primarily internalized: suppression metric should weight identity-lock heavily; the constraint''s extractive power persists even if material barriers are removed. If primarily material: identity-lock is secondary coping mechanism, not primary binding mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_trap_psychological_internalization, empirical, 'Psychological internalization of debt obligation versus material necessity').

omega_variable(
    welfare_state_functional_capacity,
    'Has the welfare state''s redistributive function genuinely atrophied to piton-level (theater ≥ 0.70), or does it still provide material redistribution masked by theater?',
    'Longitudinal analysis of effective tax rates (including tax expenditures, avoided taxes, loopholes) and actual income redistribution measured by Gini coefficient pre-transfer vs post-transfer. Comparison of program outcomes: measured benefit levels vs cost of living, reciprocal work requirements vs actual job creation from work-conditional programs.',
    'If truly degraded piton: welfare state is performative maintenance with minimal redistribution; theater disguises extraction flow direction. If still functional: piton classification is premature; constraint remains tangled_rope. Current evidence suggests theater is rising (means-testing, work requirements, stigma) while redistribution is declining.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_state_functional_capacity, empirical, 'Whether welfare state remains functionally redistributive or has degraded to theatrical performance').

omega_variable(
    inequality_amplification_institutional_reversibility,
    'Are the mechanisms of inequality amplification (tax structure, corporate governance, intellectual property, monetary policy, labor law) structurally reversible through policy change, or are they locked in by path dependence and vested interests?',
    'Historical case studies of inequality reduction (Nordic social democracies post-WWII, US post-WWII to 1970s, post-colonial tax reforms) showing reversibility timeline and political coalition requirements. Comparison of jurisdictions with identical technology but vastly different inequality outcomes. Analysis of vested interests'' capacity to block reversal policies.',
    'If reversible: constraint is tangled_rope with visible sunset through democratic process. If locked in: constraint approaches snare classification. If path-dependent but reversible through coordinated institutional reform: constraint is scaffold with long sunset horizon (50+ years).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inequality_amplification_institutional_reversibility, empirical, 'Reversibility of inequality amplification mechanisms through policy reform').

omega_variable(
    global_capital_mobility_suppression_mechanism,
    'Does global capital mobility function as genuine exit for capital holders and creditors, or does it function as suppression mechanism against labor (labor cannot flee, so capital''s threat to flee suppresses wages)?',
    'Behavioral economics: measure actual capital flight versus threat of capital flight in response to labor organizing, wage increases, or tax changes. Decompose suppression into capital''s exit value versus labor''s inability to coordinate escape.',
    'If capital mobility is genuine exit: creditors and asset-holders should see lower extraction. If capital mobility functions as suppression threat: suppression metric should rise; capital holders benefit from labor''s immobility more than from their own mobility. Current evidence suggests capital mobility threat functions primarily as suppression mechanism against labor (wage moderation, benefit erosion under threat of outsourcing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_capital_mobility_suppression_mechanism, empirical, 'Capital mobility as exit versus capital mobility threat as suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(economic_inequality_amplification, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(econ_ineq_tr_t0, economic_inequality_amplification, theater_ratio, 0, 0.25).
narrative_ontology:measurement(econ_ineq_tr_t20, economic_inequality_amplification, theater_ratio, 20, 0.38).
narrative_ontology:measurement(econ_ineq_tr_t40, economic_inequality_amplification, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(econ_ineq_be_t0, economic_inequality_amplification, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(econ_ineq_be_t20, economic_inequality_amplification, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(econ_ineq_be_t40, economic_inequality_amplification, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(economic_inequality_amplification, resource_allocation).
narrative_ontology:affects_constraint(economic_inequality_amplification, wage_stagnation_paradox).
narrative_ontology:affects_constraint(economic_inequality_amplification, housing_financialization).
narrative_ontology:affects_constraint(economic_inequality_amplification, debt_servicing_constraint).
narrative_ontology:affects_constraint(economic_inequality_amplification, labor_power_fragmentation).
narrative_ontology:affects_constraint(economic_inequality_amplification, tax_avoidance_architecture).

% DUAL FORMULATION NOTE:
% Economic inequality amplification decomposes into multiple structurally distinct constraints with different ε values: wage_stagnation_paradox (ε=0.35, coordination failure), housing_financialization (ε=0.62, snare), debt_servicing_constraint (ε=0.58, snare with identity-lock), labor_power_fragmentation (ε=0.45, tangled_rope), and tax_avoidance_architecture (ε=0.52, tangled_rope). Each story captures a mechanistically distinct amplification pathway; this story captures the systemic effect of all amplification mechanisms operating together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(economic_inequality_amplification, institutional, 0.08).
constraint_indexing:directionality_override(economic_inequality_amplification, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
