% ============================================================================
% CONSTRAINT STORY: obligation_floor_as_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_obligation_floor_as_control, []).

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
 *   constraint_id: obligation_floor_as_control
 *   human_readable: Obligation Floor as Labor Control Mechanism
 *   domain: political_economy/consumer_finance/social_control
 *
 * SUMMARY:
 *   The obligation floor constraint describes how fixed monthly debt
 *   obligations (mortgage/rent, car payments, student loans, credit cards,
 *   medical debt) convert worker optionality into creditor and employer
 *   leverage by creating non-negotiable baseline costs. A worker with
 *   $2000/month in fixed obligations cannot walk away from a job paying
 *   $3000/month even if conditions are exploitative, cannot participate in a
 *   strike that risks termination, and cannot demand higher wages if the
 *   threat of job loss means defaulting on debts. The constraint operates at
 *   multiple scales: individual (household budget), institutional (labor
 *   market dynamics), and civilizational (the shift from pension-based to
 *   debt-based working-class security). The primary observable is the
 *   correlation between household debt-to-income ratio and labor market exit
 *   rates: higher debt predicts lower job mobility, lower strike
 *   participation, and slower wage growth, controlling for industry and skill
 *   level. The constraint exhibits genuine coordination function (credit
 *   enables consumption smoothing, homeownership, education investment)
 *   alongside asymmetric extraction (debt service suppresses labor power,
 *   transfers wealth to creditors, and increases employer leverage). The
 *   theater_ratio (0.48) reflects the gap between the stated purpose of
 *   consumer credit (enabling opportunity and economic mobility) and its
 *   structural function (binding workers to employment regardless of
 *   conditions). The ratio has increased over the 40-year interval as credit
 *   access expanded while bankruptcy protections weakened, student debt
 *   became non-dischargeable, and medical debt proliferated.
 *
 * KEY AGENTS:
 *   - Indebted Workers: Primary victims (powerless/trapped at immediate horizon, moderate/constrained at biographical horizon) — bear the extraction through suppressed wages, reduced mobility, and inability to refuse exploitative conditions
 *   - Creditor Class: Primary beneficiaries (institutional/arbitrage) — extract through interest payments and benefit from secondary labor market effects (lower wages increase corporate profits)
 *   - Employers in Tight Labor Markets: Secondary beneficiaries (institutional/arbitrage) — benefit from reduced worker bargaining power without directly imposing the debt constraint
 *   - Labor Organizing Capacity: Abstract victim (powerless/trapped) — collective action capacity suppressed by individual debt burdens; cannot organize effectively when members cannot risk job loss
 *   - Labor Organizing Coalitions: Organized agents (organized/constrained) — see both coordination and extraction; constrained by the political power that debt itself suppresses
 *   - Financial Reform Coalitions: Organized agents (organized/mobile) — see the constraint as solvable through policy redesign; can shift focus to other reform domains
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies the constraint as Tangled Rope, exhibiting genuine coordination function with embedded asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(obligation_floor_as_control, 0.38).
domain_priors:suppression_score(obligation_floor_as_control, 0.62).
domain_priors:theater_ratio(obligation_floor_as_control, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(obligation_floor_as_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(obligation_floor_as_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(obligation_floor_as_control, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(obligation_floor_as_control, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(obligation_floor_as_control, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(obligation_floor_as_control, tangled_rope).
narrative_ontology:human_readable(obligation_floor_as_control, "Obligation Floor as Labor Control Mechanism").
narrative_ontology:topic_domain(obligation_floor_as_control, "political_economy/consumer_finance/social_control").

domain_priors:requires_active_enforcement(obligation_floor_as_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(obligation_floor_as_control, creditor_class).
narrative_ontology:constraint_beneficiary(obligation_floor_as_control, employers_in_tight_labor_markets).
narrative_ontology:constraint_victim(obligation_floor_as_control, indebted_workers).
narrative_ontology:constraint_victim(obligation_floor_as_control, labor_organizing_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEBTED WORKER / IMMEDIATE HORIZON (SNARE) — Trapped by monthly payment obligations with no exit option at immediate time scale. Cannot miss rent, car payment, or minimum credit card payment without cascading consequences. Experiences maximum extraction: the obligation floor eliminates all negotiating leverage with employer because walking away from exploitative conditions means defaulting on debts. The coordination function (access to housing, transportation, consumption smoothing) is invisible at this time scale — only the binding constraint is salient.
constraint_indexing:constraint_classification(obligation_floor_as_control, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INDEBTED WORKER / BIOGRAPHICAL HORIZON (TANGLED ROPE) — At biographical time scale, the worker can see both the coordination function (credit enabled home purchase, education, vehicle ownership that increased earning capacity) and the extraction mechanism (debt service reduces savings, limits job mobility, suppresses wage demands). Constrained exit: can refinance, consolidate, or slowly pay down debt, but cannot escape the structural position without years of discipline or windfall. Mixed experience: genuine benefits received alongside genuine costs imposed.
constraint_indexing:constraint_classification(obligation_floor_as_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITOR CLASS (ROPE) — Experiences the constraint as pure coordination: extending credit enables consumption, homeownership, and economic activity that would not otherwise occur. The labor control effect is an externality from this perspective, not the primary function. Arbitrage exit: can shift capital to other lending markets, other asset classes, or other jurisdictions with minimal friction. Net beneficiary: extraction flows toward creditors through interest payments and through the secondary labor market effect (indebted workers accept lower wages, increasing corporate profits and equity returns).
constraint_indexing:constraint_classification(obligation_floor_as_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYERS IN TIGHT LABOR MARKETS (ROPE) — Benefit from reduced worker bargaining power without directly imposing the constraint. The obligation floor is a coordination mechanism from this perspective: workers with fixed costs are more reliable (lower turnover, higher attendance) and more compliant (lower strike participation, fewer demands). Employers experience this as a labor market feature that reduces hiring and retention costs. Arbitrage exit: can relocate to jurisdictions with different household debt profiles or automate positions if labor costs rise.
constraint_indexing:constraint_classification(obligation_floor_as_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR ORGANIZING COALITIONS (TANGLED ROPE) — Organized labor sees both the coordination function (credit access enables working-class homeownership and consumption) and the extraction mechanism (debt service suppresses strike funds, reduces worker willingness to risk job loss, fragments class solidarity). Constrained exit: can advocate for debt jubilee, stronger bankruptcy protections, or wage increases that outpace debt service, but cannot eliminate the structural constraint without political power that the constraint itself suppresses. Generational time horizon: sees the historical shift from pension-based to debt-based working-class security as a deliberate substitution that increased capital's leverage.
constraint_indexing:constraint_classification(obligation_floor_as_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FINANCIAL REFORM COALITIONS (SCAFFOLD) — Organized reformers (consumer protection advocates, progressive policy groups, some regulators) see the obligation floor as a temporary coordination failure with a policy sunset: stronger bankruptcy protections, student debt cancellation, medical debt elimination, and public housing expansion would reduce the binding constraint without eliminating credit access. Mobile exit: coalition members can shift focus to other reform domains if this one proves intractable. Sees the constraint as solvable through institutional redesign rather than inherent to credit markets.
constraint_indexing:constraint_classification(obligation_floor_as_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the obligation floor exhibits both genuine coordination (credit markets enable intertemporal consumption smoothing and capital formation) and structural extraction (the specific institutional design — non-dischargeable student debt, medical debt from privatized healthcare, housing costs from undersupply — creates binding constraints that suppress labor power beyond what credit access requires). The analytical classification is Tangled Rope because the constraint genuinely solves a coordination problem (intertemporal resource allocation) while embedding asymmetric extraction (creditor and employer leverage over workers). This is the basis for the claimed_type.
constraint_indexing:constraint_classification(obligation_floor_as_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(obligation_floor_as_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(obligation_floor_as_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(obligation_floor_as_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(obligation_floor_as_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts through multiple channels: direct wealth transfer via interest payments (creditor extraction), suppressed wage growth due to reduced worker bargaining power (employer extraction), and reduced labor mobility (both creditor and employer extraction). However, extraction is not maximal because the constraint does provide genuine coordination value — credit access enables consumption smoothing, homeownership, and education investment that increase worker welfare and productivity. The value (0.38) reflects that roughly 40% of the constraint's effect is extractive overhead beyond what coordination requires, based on the gap between interest rates and risk-adjusted cost of capital, the labor market power effect, and the wealth transfer from debtors to creditors. Suppression (0.62): Moderate-high. Significant barriers to exit include: non-dischargeable student debt (bankruptcy reform), medical debt from privatized healthcare (no public option), housing costs from undersupply (zoning restrictions), and the immediate consequences of default (credit score damage, eviction, repossession). However, suppression is not total — some workers can refinance, consolidate, pay down debt, or access family support. The value reflects that most indebted workers face high but not insurmountable barriers to reducing their obligation floor. Theater_ratio (0.48): Moderate. The gap between stated purpose (economic opportunity, mobility, homeownership) and structural function (labor control, wealth extraction) is significant but not dominant. Consumer credit does enable genuine opportunity for many households — the theater is in the framing of debt as purely empowering rather than as a double-edged mechanism that both enables and constrains. The ratio has increased as credit access expanded while protections weakened, making the opportunity framing less accurate over time.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how structural position determines classification. Indebted workers at immediate time scale see pure extraction (Snare) — the obligation floor is a binding constraint with no visible coordination function. The same workers at biographical time scale see mixed coordination and extraction (Tangled Rope) — credit enabled homeownership and education, but debt service now suppresses wages and mobility. Creditors see pure coordination (Rope) — they are solving the legitimate problem of intertemporal resource allocation, and the labor control effect is an externality. Employers see coordination (Rope) — workers with fixed costs are more reliable and compliant, reducing hiring and retention costs. Labor organizing coalitions see Tangled Rope — credit access benefits workers but debt burdens suppress collective action. Financial reform coalitions see Scaffold — the constraint is a solvable policy problem with a sunset path through institutional redesign. The analytical observer sees Tangled Rope as the structural reality — genuine coordination function with embedded asymmetric extraction. The perspectival gap is not a disagreement about facts but a difference in what aspects of the constraint are salient from each structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Indebted workers are victims with trapped or constrained exit options depending on time horizon, yielding high directionality values and high experienced extraction. At immediate time scale (trapped exit), the worker experiences maximum extraction — the obligation floor eliminates all negotiating leverage. At biographical time scale (constrained exit), the worker can see both costs and benefits, experiencing moderate extraction. Creditor class and employers are beneficiaries with arbitrage exit options, yielding low directionality values and low or negative experienced extraction — they benefit from the constraint through interest payments and labor market effects. Labor organizing coalitions are victims (the constraint suppresses their primary function) but with organized power and constrained exit, yielding moderate directionality and moderate experienced extraction. Financial reform coalitions are not direct victims (they are advocates rather than indebted workers themselves) and have mobile exit options, yielding low directionality and low experienced extraction — they see the constraint as a solvable policy problem. The analytical observer uses the canonical analytical directionality, experiencing moderate extraction that reflects the constraint's mixed coordination-extraction character.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that Tangled Rope is the correct analytical classification when a constraint exhibits BOTH genuine coordination function AND asymmetric extraction. The creditor perspective (Rope) is not wrong about the coordination function — credit markets do enable intertemporal consumption smoothing and capital formation. The worker perspective at immediate time scale (Snare) is not wrong about the extraction mechanism — debt obligations do suppress labor power and transfer wealth to creditors. The analytical resolution is that both are true simultaneously: the constraint coordinates (enabling consumption and investment that would not otherwise occur) while extracting (suppressing wages, reducing mobility, transferring wealth). The Tangled Rope classification captures this duality. The constraint is NOT a Rope that workers misperceive as a Snare due to limited information, nor is it a Snare that creditors misrepresent as a Rope through ideological cover. It is structurally both, and the classification system must accommodate constraints that genuinely serve dual functions. The mandatrophy is resolved by recognizing that coordination and extraction are not mutually exclusive — a constraint can do both, and Tangled Rope is the type that names this structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_credit_access,
    'Would alternative credit structures (public banking, zero-interest social credit, universal basic services) provide equivalent consumption smoothing and capital access without the labor control effect?',
    'Comparative analysis of labor market outcomes in jurisdictions with public banking systems (North Dakota, German Sparkassen, Singaporean HDB housing loans) vs. private credit-dependent systems; controlled experiments with UBI/UBS pilots measuring job mobility and wage demands',
    'If alternatives provide equivalent coordination: the labor control effect is extractive overhead, not inherent cost, strengthening Snare classification from worker perspective. If alternatives fail: the obligation floor may be closer to genuine coordination cost, strengthening Rope classification from creditor perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_credit_access, empirical, 'Whether alternative credit structures can provide coordination without labor control').

omega_variable(
    debt_threshold_nonlinearity,
    'Is there a debt-to-income threshold below which the labor control effect disappears, or is the relationship linear across the full range?',
    'Regression analysis of household debt-to-income ratio against labor market exit rates, strike participation, and wage growth, controlling for industry, geography, and skill level; identification of potential threshold effects or nonlinearities',
    'If threshold exists: low-debt households experience Rope (genuine coordination), high-debt households experience Snare (binding constraint), suggesting the constraint type is debt-level dependent. If linear: the extraction mechanism operates at all debt levels, just with varying intensity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_threshold_nonlinearity, empirical, 'Whether labor control effect has a debt-to-income threshold or is linear').

omega_variable(
    employer_awareness,
    'Do employers consciously factor worker debt levels into wage-setting and working conditions, or is the labor control effect an emergent property of worker behavior that employers passively benefit from?',
    'Analysis of internal HR documents, wage-setting models, and employer surveys; investigation of whether firms in high-household-debt regions systematically offer lower wages or worse conditions than firms in low-debt regions for equivalent positions',
    'If conscious: the constraint is actively enforced extraction (Snare from worker perspective, deliberate strategy from employer perspective). If emergent: the constraint is a structural feature that employers benefit from without designing (Tangled Rope from both perspectives, with employers as incidental beneficiaries).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employer_awareness, empirical, 'Whether employers consciously exploit worker debt or passively benefit').

omega_variable(
    historical_contingency,
    'Is the current obligation floor level (median US household debt-to-income ~1.5x) a stable equilibrium or a historically contingent outcome of specific policy choices (bankruptcy reform, student loan non-dischargeability, healthcare privatization, housing undersupply)?',
    'Historical analysis of household debt levels across different policy regimes; cross-national comparison of debt-to-income ratios under different institutional structures; identification of policy interventions that shifted equilibrium debt levels',
    'If contingent: the constraint is a constructed Snare that could be dismantled through policy reversal, validating Scaffold perspective. If equilibrium: the constraint reflects deeper structural features of credit markets, validating Mountain-adjacent interpretation (though beneficiary presence rules out true Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_contingency, conceptual, 'Whether current debt levels are equilibrium or policy-contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(obligation_floor_as_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oblig_theater_1980, obligation_floor_as_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(oblig_theater_1990, obligation_floor_as_control, theater_ratio, 10, 0.38).
narrative_ontology:measurement(oblig_theater_2000, obligation_floor_as_control, theater_ratio, 20, 0.42).
narrative_ontology:measurement(oblig_theater_2010, obligation_floor_as_control, theater_ratio, 30, 0.45).
narrative_ontology:measurement(oblig_theater_2020, obligation_floor_as_control, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(oblig_extract_1980, obligation_floor_as_control, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(oblig_extract_1990, obligation_floor_as_control, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(oblig_extract_2000, obligation_floor_as_control, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(oblig_extract_2010, obligation_floor_as_control, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(oblig_extract_2020, obligation_floor_as_control, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(obligation_floor_as_control, resource_allocation).
narrative_ontology:affects_constraint(obligation_floor_as_control, healthcare_cost_trap).
narrative_ontology:affects_constraint(obligation_floor_as_control, student_debt_non_dischargeability).
narrative_ontology:affects_constraint(obligation_floor_as_control, housing_undersupply_rent_extraction).
narrative_ontology:affects_constraint(obligation_floor_as_control, bankruptcy_reform_creditor_priority).

% DUAL FORMULATION NOTE:
% The obligation floor is a composite constraint that emerges from the interaction of multiple institutional constraints: non-dischargeable student debt, medical debt from privatized healthcare, housing costs from undersupply, and weakened bankruptcy protections. Each of these upstream constraints has its own extractiveness value reflecting its specific institutional design. The obligation floor represents the aggregate effect: the sum of fixed monthly obligations that bind workers to employment. The network edges indicate that changes to any upstream constraint (e.g., student debt cancellation, public healthcare, housing supply expansion, bankruptcy reform) would reduce the obligation floor's binding force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
