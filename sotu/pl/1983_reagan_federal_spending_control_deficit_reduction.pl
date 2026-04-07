% ============================================================================
% CONSTRAINT STORY: 1983_reagan_federal_spending_control_deficit_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1983_reagan_federal_spending_control_deficit_reduction, []).

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
 *   constraint_id: 1983_reagan_federal_spending_control_deficit_reduction
 *   human_readable: Federal Budget Discipline and Deficit Reduction as Precondition for Economic Recovery
 *   domain: governance/fiscal_policy/macroeconomics
 *
 * SUMMARY:
 *   The Reagan administration positioned federal budget discipline and
 *   deficit reduction as both a diagnosis of economic disorder and a
 *   necessary precondition for sustained recovery. The constraint operates
 *   across two temporal scales: immediate (spending cuts in 1981-1983) and
 *   long-term (deficit reduction enabling lower inflation and interest rates
 *   by 1985-1990). The mechanism exhibits genuine coordination function
 *   (capital markets require confidence in fiscal sustainability) alongside
 *   asymmetric extraction (current welfare recipients and federal employees
 *   bear immediate costs; future households and capital markets receive
 *   future benefits). The structure is a classic Tangled Rope: the constraint
 *   both solves a collective action problem (rebuilding creditor confidence)
 *   and redistributes resources from politically weak current beneficiaries
 *   to powerful future beneficiaries (capital markets, future taxpayers). The
 *   theater ratio reflects the gap between deficit reduction rhetoric
 *   (reconciliation bills, Gramm-Rudman-Hollings targets) and actual
 *   structural change to entitlements, which remained largely protected.
 *   Congressional spending discipline persists through performative
 *   commitment (legislative ritual) with limited structural enforcement,
 *   indicating piton-level degradation alongside the tangled rope primary
 *   classification.
 *
 * KEY AGENTS:
 *   - Federal Employees and Welfare Recipients: Primary victims (powerless/trapped) — bear immediate spending cuts with no exit capacity or future benefit recovery path
 *   - Rural and Regional Communities: Secondary victims (moderate/constrained) — face infrastructure investment cuts; dependent on federal programs with limited private alternatives
 *   - Capital Markets and Future Creditors: Primary beneficiaries (institutional/arbitrage) — benefit from deficit reduction through lower interest rates and crowding-out relief
 *   - Future Households: Nominal beneficiaries (analytical/analytical) — avoid projected $4,300 per-capita debt burden, but benefit is distant and heavily discounted
 *   - Large Corporations and High-Income Earners: Mixed beneficiaries (powerful/mobile) — benefit from capital availability and lower inflation but face potential tax/subsidy changes
 *   - Congressional Budget Authority: Institutional enforcer (institutional/arbitrage) — maintains performative spending discipline while lacking structural mechanism for entitlement reform
 *   - Labor Unions and Public Sector Workers: Organized victims (organized/constrained) — organized opposition to cuts but structurally dependent on public sector employment
 *   - Federal Reserve: Policy coordinator (institutional/arbitrage) — coordinates with Reagan coalition on inflation targeting; benefits from spending discipline credibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1983_reagan_federal_spending_control_deficit_reduction, 0.58).
domain_priors:suppression_score(1983_reagan_federal_spending_control_deficit_reduction, 0.62).
domain_priors:theater_ratio(1983_reagan_federal_spending_control_deficit_reduction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1983_reagan_federal_spending_control_deficit_reduction, extractiveness, 0.58).
narrative_ontology:constraint_metric(1983_reagan_federal_spending_control_deficit_reduction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(1983_reagan_federal_spending_control_deficit_reduction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1983_reagan_federal_spending_control_deficit_reduction, tangled_rope).
narrative_ontology:human_readable(1983_reagan_federal_spending_control_deficit_reduction, "Federal Budget Discipline and Deficit Reduction as Precondition for Economic Recovery").
narrative_ontology:topic_domain(1983_reagan_federal_spending_control_deficit_reduction, "governance/fiscal_policy/macroeconomics").

domain_priors:requires_active_enforcement(1983_reagan_federal_spending_control_deficit_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1983_reagan_federal_spending_control_deficit_reduction, future_households).
narrative_ontology:constraint_beneficiary(1983_reagan_federal_spending_control_deficit_reduction, capital_markets).
narrative_ontology:constraint_beneficiary(1983_reagan_federal_spending_control_deficit_reduction, low_inflation_sectors).
narrative_ontology:constraint_victim(1983_reagan_federal_spending_control_deficit_reduction, federal_employees).
narrative_ontology:constraint_victim(1983_reagan_federal_spending_control_deficit_reduction, welfare_recipients).
narrative_ontology:constraint_victim(1983_reagan_federal_spending_control_deficit_reduction, rural_infrastructure_beneficiaries).
narrative_ontology:constraint_victim(1983_reagan_federal_spending_control_deficit_reduction, current_generation_fiscal_burden_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL EMPLOYEES AND WELFARE RECIPIENTS (SNARE) — Trapped in immediate spending cuts with no exit capacity. Face direct reduction in employment, benefits, or program access. Cannot negotiate participation; bear costs now; receive promised future benefits (lower inflation) that are uncertain and arrive years later. Maximum extraction from this perspective: the constraint redistributes present resources away from this group toward future creditors and capital markets.
constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL AND REGIONAL COMMUNITIES (TANGLED ROPE) — Constrained by dependence on federal infrastructure investment and rural subsidies. Bear asymmetric burden of spending cuts (agricultural programs, rural electrification, water infrastructure). Also coordinate collective action through these same programs. High suppression (geographic isolation, limited private alternatives) combined with mixed benefits (some infrastructure maintained, some new investment foregone). Constrained exit: moving to urban centers is costly; staying means accepting local resource degradation.
constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CAPITAL MARKETS AND FUTURE CREDITORS (ROPE) — Primary beneficiary from deficit reduction. Lower deficit spending reduces crowding out of private investment, lowers long-term interest rates, increases available capital for private sector. Arbitrage capacity: these actors can exit constraints through capital reallocation (move funds to other nations if U.S. fiscal discipline is weak). Net benefit: constraint coordinates expectations about fiscal sustainability while ensuring capital flows to this sector. Experiences constraint as coordination mechanism, not extraction.
constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE CORPORATIONS AND HIGH-INCOME EARNERS (TANGLED ROPE) — Mixed position. Benefit from lower deficit spending (reduced crowding out, lower future tax burden, lower inflation). Also coordinate through tax expenditures and corporate subsidies that may be reduced. Mobile exit capacity (capital flight, offshore investment) but partially constrained by nationalism/reputation. High suppression of alternatives during crisis period (capital controls implicit in fiscal emergency framing). Extraction is asymmetric: some corporations benefit (those dependent on capital availability); others face rate pressure (those dependent on government contracts or subsidies).
constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CONGRESSIONAL BUDGET AUTHORITY (PITON) — The institutional mechanism for enforcing spending discipline is largely theater. Congress declares commitment to deficit reduction (reconciliation bills, budget caps) but lacks structural mechanism to enforce spending discipline against constituent demands. Theater ratio high: legislation, committees, deficit speeches persist; actual structural change to entitlements minimal. Piton classification derives from degraded functional capacity: the mechanism persists through political inertia (Reagan presidency framing) and performative compliance, not through genuine structural constraint on spending. The institution sees its own process as theater while maintaining the ritual.
constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MACROECONOMIC POLICY REFORM COALITION (SCAFFOLD) — Organized agents (Federal Reserve, fiscal hawks, deficit commissions) view deficit reduction as temporary structural adjustment with a sunset: sustained low inflation + productivity growth will eventually restore fiscal space. The constraint is temporary discipline to reset expectations and break stagflation cycle. Exit path: if inflation breaks and productivity rises, spending discipline can ease. Theater ratio moderate: genuine policy coordination (Fed-Treasury coordination, inflation targeting) alongside performative deficit rhetoric. Sunset condition: 5-10 year inflation break + productivity acceleration → relaxed constraint.
constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Civilizational/universal perspective sees deficit constraint as immutable: governments cannot persistently exceed revenues without currency collapse; inflation is inevitable under unsustainable deficits; recovery requires fiscal discipline as a structural necessity, not policy choice. This perspective risks naturalizing the institutional Reagan coalition's framing (deficit reduction is law-like rather than contingent political choice) and obscuring the beneficiary structure. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: LABOR UNIONS AND PUBLIC SECTOR WORKERS (TANGLED ROPE) — Organized agents (AFL-CIO, public sector unions) face coordinated constraint: spending cuts directly reduce their employment and bargaining power. Also coordinate collective action through public sector employment and federal contractor networks. Constrained exit: unionized workers cannot easily relocate or find equivalent public sector employment in other regions; union organizations depend on public sector membership. Moderate suppression: unionized workers have collective voice but face organized opposition (Reagan coalition). Extraction is asymmetric: wages/jobs extracted from union membership; coordination function (public goods provision through government employment) partially maintained but reduced.
constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1983_reagan_federal_spending_control_deficit_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1983_reagan_federal_spending_control_deficit_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1983_reagan_federal_spending_control_deficit_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1983_reagan_federal_spending_control_deficit_reduction, TR),
    TR >= 0.70.

:- end_tests(1983_reagan_federal_spending_control_deficit_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts present resources from federal beneficiaries and redirects toward capital markets and future growth. The extraction is not total Snare level (0.66+) because capital markets genuinely face crowding-out risk under high deficits, and the coordination function is real — sustaining creditor confidence is necessary for continued government borrowing. However, extraction exceeds pure coordination because the benefit distribution is asymmetric: capital markets capture immediate interest-rate gains; future households capture distant debt avoidance; current welfare recipients capture no benefit from the constraint itself, only promised future inflation reduction. Suppression (0.62): High. Multiple mechanisms suppress alternatives: political framing that deficit reduction is necessary (eliminates debate space); economic emergency rhetoric (inflation 1980-1982 provides genuine crisis cover); federal employee dependency (limited private-sector alternative employment, especially in rural regions); welfare recipient immobility (poverty prevents relocation). Congressional budgeting reduces formal transparency (reconciliation bill procedures bundle spending cuts into single votes, reducing legislative visibility). Theater ratio (0.68): High and increasing over the interval. Reconciliation bills and deficit targets create performative legislation; actual entitlement reform remains minimal; agency reorganizations announce savings without structural spending change; most deficit reduction comes from reduced defense spending growth and interest payment increases, not from the announced welfare/federal employment cuts. Theater increases from 0.50 to 0.72 as the constraint's gap widens between rhetorical commitment and structural implementation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Welfare recipients see Snare (pure extraction, no exit, no benefit). Regional communities see Tangled Rope (mixed coordination and extraction, constrained but not trapped). Capital markets see Rope (coordination function, arbitrage exit, net benefit). Large corporations see Tangled Rope (some benefits, some extraction, mobile but constrained). Congressional budget authority sees Piton (performative ritual masking weak structural enforcement). Labor unions see Tangled Rope (organized opposition but constrained by structural dependency). The analytical observer sees Mountain (immutable fiscal law) — which the engine will reclassify as false summit via structural data showing beneficiary concentration in capital markets and future creditors. The perspectival gap is driven by directionality: federal employees and welfare recipients experience high d (they are targets; high f(d)); capital markets experience low d (they are beneficiaries; low or negative f(d)); unions experience moderate d (organized opposition reduces extraction severity). At the biographical time horizon, current welfare recipients experience Mountain (unchangeable constraint from their position), while the policy coalition sees Scaffold (temporary discipline with sunset once inflation breaks). This perspectival divergence is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary Structure: Capital markets (institutional/arbitrage exit) benefit from deficit reduction via crowding-out relief and creditor confidence. Future households (analytical/long-term) nominally benefit from debt avoidance, but this benefit is highly discounted and abstract. Federal employees and welfare recipients (powerless/trapped; moderate/constrained) are structural victims — they bear immediate spending cuts. Directionality derives from who captures the first-order effects: capital markets capture immediate interest-rate gains (d ≈ 0.10-0.20 for institutional beneficiaries). Federal employees face immediate job loss and wage pressure (d ≈ 0.85-0.95 for trapped victims). Regional communities face infrastructure cuts with limited private alternatives (d ≈ 0.70-0.80 for constrained victims). Labor unions have organized exit capacity and can mobilize political opposition (d ≈ 0.45-0.55 for organized victims). The derivation chain applies sigmoid f(d) to each agent's d value: low-d beneficiaries experience negative or very low χ (constraint subsidizes them); high-d victims experience high χ (constraint extracts from them). The analytical observer's d ≈ 0.72 (from canonical fallback for analytical power atom) produces moderate χ, which prevents misclassification as pure Rope or pure Snare — the observer sees mixed function. The directionality logic explains why perspectives range from Snare (trapped beneficiaries) to Rope (beneficiary capital markets) to Piton (institutional theater) despite identical base extractiveness across all contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY PERSPECTIVAL DECOMPOSITION: This constraint resolves the mandatrophy by showing that all six types are legitimate perspectival readings reflecting real structural differences in agent position. The mandatrophy is not 'what is the true type?' but 'from which agent position are you measuring?'. The piton perspective identifies genuine institutional theater (reconciliation bills announce savings that don't materialize). The snare perspective identifies genuine extraction asymmetry (welfare recipients bear costs with no offsetting benefit). The rope perspective identifies genuine coordination function (capital markets do face crowding-out risk). The scaffold perspective identifies genuine sunset logic (if inflation breaks, spending discipline can ease). The mountain perspective identifies the risk of naturalizing contingent institutional arrangements (deficits face fiscal constraints, but these constraints are not laws of nature). The analytical observer's false summit is the constraint's most important diagnostic feature: the Reagan coalition frames deficit reduction as immutable fiscal necessity (mountain), but the structural data reveals it as a contingent institutional arrangement with identifiable beneficiaries (capital markets, future creditors) and victims (current welfare recipients, federal employees). The constraint's true nature emerges from the perspectival presheaf: it is a Tangled Rope with piton-level degradation and mountain-level false summit risk, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_causation_mechanism,
    'Does deficit spending directly cause inflation, or does inflation arise from other monetary/supply factors independent of fiscal deficits?',
    'Cross-national econometric analysis of deficit-inflation correlation controlling for money supply growth, supply shocks, and wage-setting institutions. Comparison of high-deficit / low-inflation cases (Japan 1990s, U.S. 2009-2015) and low-deficit / high-inflation cases.',
    'If deficits cause inflation: spending discipline is necessary condition for recovery (constraint correctly classified as Tangled Rope). If inflation is independent: spending discipline imposes costs without recovery benefits (constraint reclassifies toward pure Snare). If mixed causation: classification depends on weight assigned to fiscal vs. monetary mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_causation_mechanism, empirical, 'Causal mechanism linking deficit spending to inflation').

omega_variable(
    crowding_out_magnitude,
    'How much does deficit spending crowd out private investment, and at what deficit threshold does crowding out become economically significant?',
    'Interest rate elasticity of private investment; comparison of private investment rates in high vs. low deficit periods; analysis of long-term capital formation under different fiscal regimes.',
    'If crowding out is severe and immediate: benefit to capital markets is real and large; extraction from current generation is justified by future growth. If crowding out is weak or lagged: benefit to capital markets is marginal; extraction cost is not offset by growth gains. Magnitude determines whether the tangled rope classification overstates coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_magnitude, empirical, 'Magnitude and threshold of crowding-out effects').

omega_variable(
    temporary_vs_permanent_extraction,
    'Is the extraction from current welfare recipients and federal employees temporary (lasting 2-5 years until inflation breaks) or permanent (representing a durable reallocation away from social spending)?',
    'Post-recovery spending trajectories; whether entitlement baselines return to pre-constraint levels (adjusted for inflation) or remain permanently reduced. Comparison of Reagan-era spending cuts against post-1985 spending growth.',
    'If temporary: scaffold perspective is accurate; constraint has genuine sunset. If permanent: extraction is durable; constraint reclassifies from Tangled Rope toward sustained Snare. Classification depends on regime durability, not just immediate policy goals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporary_vs_permanent_extraction, empirical, 'Duration and reversibility of extraction').

omega_variable(
    per_capita_debt_burden_internalization,
    'Do future households actually internalize the projected $4,300 per-capita debt burden avoidance as a real benefit, or is this a discount-rate phenomenon (future benefit too distant to motivate present sacrifice)?',
    'Intergenerational preference studies; political economy analysis of whether voters accept present costs for future benefits; comparison of deficit-reduction coalitions across generations.',
    'If internalized: future benefit is real constraint on present behavior; extraction is justified by intergenerational transfer of benefit. If not internalized: future benefit is theoretical only; extraction is pure present cost with no offsetting benefit for decision-makers. Classification depends on whether future benefit is actionable or abstract.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(per_capita_debt_burden_internalization, preference, 'Intergenerational internalization of future debt burden avoidance').

omega_variable(
    political_durability_of_restraint,
    'Can Congress maintain spending discipline under electoral pressure, or does budget discipline degrade as soon as electoral competition resumes (piton degradation)?',
    'Post-1985 budget trajectory; frequency and magnitude of reconciliation bill reversals; comparison of deficit reduction commitment against actual expenditure growth.',
    'If durable: spending discipline is genuine structural constraint (Tangled Rope remains valid). If degraded: constraint is performative ritual (piton classification is correct). Theater ratio should increase over time if durability is weak.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_durability_of_restraint, empirical, 'Political sustainability of spending discipline across electoral cycles').

omega_variable(
    supply_side_response_magnitude,
    'Does reduced deficit spending generate productivity growth and expanded tax base (supply-side response), or do spending cuts reduce aggregate demand without offsetting supply expansion?',
    'Productivity growth 1983-1989 decomposed into capital deepening from reduced crowding out vs. other sources. Comparison of tax receipts under constraint vs. baseline projections.',
    'If supply-side response is large: scaffold sunset is realistic; future growth offsets present extraction. If minimal: extraction is not offset; constraint approaches pure Snare. Classification depends on growth dividend materialization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_response_magnitude, empirical, 'Supply-side growth response to deficit reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1983_reagan_federal_spending_control_deficit_reduction, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reagan_deficit_tr_t0, 1983_reagan_federal_spending_control_deficit_reduction, theater_ratio, 0, 0.5).
narrative_ontology:measurement(reagan_deficit_tr_t2, 1983_reagan_federal_spending_control_deficit_reduction, theater_ratio, 2, 0.62).
narrative_ontology:measurement(reagan_deficit_tr_t4, 1983_reagan_federal_spending_control_deficit_reduction, theater_ratio, 4, 0.68).
narrative_ontology:measurement(reagan_deficit_tr_t8, 1983_reagan_federal_spending_control_deficit_reduction, theater_ratio, 8, 0.72).

% Extraction over time
narrative_ontology:measurement(reagan_deficit_be_t0, 1983_reagan_federal_spending_control_deficit_reduction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(reagan_deficit_be_t2, 1983_reagan_federal_spending_control_deficit_reduction, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(reagan_deficit_be_t4, 1983_reagan_federal_spending_control_deficit_reduction, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(reagan_deficit_be_t8, 1983_reagan_federal_spending_control_deficit_reduction, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1983_reagan_federal_spending_control_deficit_reduction, resource_allocation).
narrative_ontology:affects_constraint(1983_reagan_federal_spending_control_deficit_reduction, inflation_expectations_formation_1980s).
narrative_ontology:affects_constraint(1983_reagan_federal_spending_control_deficit_reduction, labor_union_bargaining_power_decline).
narrative_ontology:affects_constraint(1983_reagan_federal_spending_control_deficit_reduction, capital_mobility_constraints_liberalization).
narrative_ontology:affects_constraint(1983_reagan_federal_spending_control_deficit_reduction, entitlement_political_lock_in).

% DUAL FORMULATION NOTE:
% The deficit reduction constraint has multiple structurally distinct decompositions depending on observable: (1) Macroeconomic strand (ε=0.48, Tangled Rope) — fiscal discipline as coordination for inflation control; (2) Political economy strand (ε=0.72, Snare) — deficit reduction as vehicle for welfare state retrenchment; (3) Institutional strand (ε=0.35, Piton) — reconciliation process as performative budget theater. All three stories share the same base policies but differ in ε because they measure different aspects of the constraint's function. This story focuses on the macroeconomic strand; political economy and institutional decompositions should be authored as separate constraint families linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1983_reagan_federal_spending_control_deficit_reduction, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
