% ============================================================================
% CONSTRAINT STORY: student_debt_servitude
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_student_debt_servitude, []).

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
 *   constraint_id: student_debt_servitude
 *   human_readable: Student Debt Servitude: Educational Financing as Coercive Extraction
 *   domain: economic/educational/labor
 *
 * SUMMARY:
 *   Student debt servitude describes the structural constraint created by the
 *   U.S. higher education financing system, in which post-secondary education
 *   access is conditioned on borrowing against future labor, creating a
 *   binding obligation that suppresses wage expectations, geographic
 *   mobility, and intergenerational asset accumulation for 10-20+ years. The
 *   constraint exhibits classic snare characteristics: (1) trapped victims
 *   with no viable exit (college degree has become credentialing requirement;
 *   alternative pathways are underfunded and socially stigmatized; default
 *   carries permanent creditworthiness consequences and wage garnishment);
 *   (2) clear beneficiaries (financial institutions capturing interest
 *   revenue, employers benefiting from wage suppression and reduced mobility,
 *   government capturing tax revenue from interest deductions); (3) high
 *   suppression (systemic underfunding of alternative education pathways,
 *   social pressure toward debt-financed college, non-dischargeable
 *   bankruptcy status making debt permanent); (4) minimal coordination
 *   function (unlike genuine education investment coordination, the debt
 *   structure serves primarily redistributive extraction rather than
 *   efficiency enhancement). The constraint shows escalating extractiveness
 *   over the 30-year interval as real debt burdens have grown relative to
 *   starting wages, and theater_ratio has slightly increased as financial aid
 *   bureaucracy has expanded without reducing debt loads. The classification
 *   as snare is robust across most perspectives; the primary perspectival gap
 *   appears between creditors (who experience rope/coordination) and
 *   borrowers (who experience snare/extraction), revealing that the same
 *   institution functions as coordination from the beneficiary view and
 *   extraction from the victim view.
 *
 * KEY AGENTS:
 *   - Student Borrowers / Graduates: Primary victims (powerless/trapped, moderate/constrained, analytical/analytical) — bear full burden of extractive debt servicing; no exit without permanent creditworthiness damage; face lifetime wage suppression and asset accumulation impairment
 *   - Financial Services Institutions: Primary beneficiaries (institutional/arbitrage) — Federal student loan servicers, private loan companies; capture interest revenue, government subsidies, and guaranteed repayment enforcement; experience constraint as coordination mechanism
 *   - Employers: Secondary beneficiary (institutional/constrained) — benefit from debt-suppressed wage expectations and reduced employee geographic mobility; constrained from unilaterally raising wages due to competitor access to same labor pool
 *   - U.S. Federal Government: Beneficiary and enforcer (institutional/arbitrage) — captures revenue through loan origination fees and interest; enforces collection through tax return intercept and wage garnishment; maintains system through policy and budgetary incentive alignment
 *   - Educational Institutions: Institutional stakeholder (institutional/constrained) — universities depend on tuition revenue (much of it debt-financed); constrained from reducing tuition independently due to competitive enrollment pressures; benefit indirectly from debt system's enabling of high tuition prices
 *   - Public Higher Education Advocates: Organized challengers (organized/constrained) — policy movements advocating debt forgiveness, free college, income-based repayment; constrained by political economy of creditor/government/employer coalition; see sunset mechanisms in proposed alternatives
 *   - Analytical Observer: Full structural view (analytical/analytical) — observes snare structure, no coordination function, clear beneficiaries and trapped victims, suppression mechanisms, international policy alternatives demonstrating contingency of U.S. model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_debt_servitude, 0.58).
domain_priors:suppression_score(student_debt_servitude, 0.72).
domain_priors:theater_ratio(student_debt_servitude, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_debt_servitude, extractiveness, 0.58).
narrative_ontology:constraint_metric(student_debt_servitude, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(student_debt_servitude, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(student_debt_servitude, snare).
narrative_ontology:human_readable(student_debt_servitude, "Student Debt Servitude: Educational Financing as Coercive Extraction").
narrative_ontology:topic_domain(student_debt_servitude, "economic/educational/labor").

domain_priors:requires_active_enforcement(student_debt_servitude).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(student_debt_servitude, creditor_financial_institutions).
narrative_ontology:constraint_beneficiary(student_debt_servitude, government_revenue_capture).
narrative_ontology:constraint_beneficiary(student_debt_servitude, employer_wage_suppression_benefit).
narrative_ontology:constraint_victim(student_debt_servitude, student_borrowers).
narrative_ontology:constraint_victim(student_debt_servitude, labor_market_mobility).
narrative_ontology:constraint_victim(student_debt_servitude, intergenerational_asset_accumulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDEBTED GRADUATE (SNARE) — Structurally trapped by debt obligations. Cannot exit higher education without debt (opportunity cost of foregone earnings); cannot escape debt without income; cannot increase income sufficiently due to debt servicing burden. Effective extraction rate approaches maximum for biographical horizon. No arbitrage option exists — default carries permanent creditworthiness collapse and wage garnishment. The graduate's labor is encumbered for 10-20 years minimum.
constraint_indexing:constraint_classification(student_debt_servitude, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE WORKING-CLASS BORROWER (GENERATIONAL VIEW) (SNARE) — At moderate power level with high temporal scope, the constraint shows persistent extraction across generations. Borrowers can eventually pay off debt (exit exists) but at severe lifetime opportunity cost: delayed family formation, suppressed homeownership, depleted intergenerational asset transfer. The working class sees not individual debt but family-level wealth transfer from debtors to creditors across decades. High suppression because alternative education pathways (apprenticeship, trade schools) are systemically disfavored relative to college-debt-requiring track.
constraint_indexing:constraint_classification(student_debt_servitude, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE FINANCIAL SERVICES INSTITUTION (ROPE) — Primary beneficiary experiencing the debt structure as pure coordination mechanism. Student lending appears as: reliable revenue stream, low default risk (due to non-dischargeable bankruptcy status), government guarantee on federal loans, and inflation-hedged portfolio. The institution experiences servicing costs as legitimate operational expense, not extraction. Arbitrage options: exit federal lending to focus on private market where prices are uncapped. This perspective sees the system as genuine coordination — risk-pooling with borrowers.
constraint_indexing:constraint_classification(student_debt_servitude, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE EMPLOYER (TANGLED_ROPE) — Experiences coordinated labor supply (educated workforce available) with asymmetric extraction benefit. Graduates with debt service obligations have suppressed wage expectations and reduced geographic mobility — employers can pay below market-clearing wages because graduates cannot afford to relocate or wait for better offers. The employer benefits from coordination (access to skilled labor) AND extraction (ability to suppress wages due to debt captivity). Constrained exit: competitors also employ debt-trapped workers, so none can unilaterally increase wages without losing labor to competitors.
constraint_indexing:constraint_classification(student_debt_servitude, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HIGHER EDUCATION ADVOCATE (SCAFFOLD) — Organized movement (policy advocates, debt relief campaigns, free college proposals) sees student debt as a temporary, surmountable coordination failure. Public university funding + income-based repayment + eventual forgiveness programs represent sunset mechanisms — structured to reduce extraction over time as enrollment-demand policies shift toward free/subsidized models. Constraint has apparent sunset: if free college or substantial debt forgiveness passes, extraction mechanism terminates. Classified as Scaffold because the sunset clause is visible in policy proposals, even if not yet implemented.
constraint_indexing:constraint_classification(student_debt_servitude, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EDUCATIONAL-INDUSTRIAL THEATER (PITON) — At civilizational scope, the student debt system appears substantially performative. The explicit rationale — 'debt discipline ensures skin in the game and academic accountability' — fails empirical scrutiny: debt levels do not correlate with degree completion or educational quality. Theater_ratio 0.48 reflects high performative content: extensive financial aid bureaucracy, repayment processing theater, and default consequences rituals persist despite weak instrumental function. The system maintains itself through institutional inertia: universities depend on tuition revenue, lenders depend on loan volume, government depends on deficit reduction accounting (loans shown as assets rather than redistributive transfers). Function has partially atrophied; theater persists.
constraint_indexing:constraint_classification(student_debt_servitude, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From full analytical position, student debt servitude is a structurally coherent snare: it extracts labor value from a cohort with no viable exit (trapped), sustains itself through institutional enforcement (requires_active_enforcement: true), suppresses alternative pathways (trade apprenticeship systematically underfunded relative to debt-financed college), and serves clear beneficiaries (creditors, employers, government revenue). No coordination function justifies the extraction — the 'educational investment' rationale naturalizes what is primarily a wealth transfer mechanism from graduates to financial institutions and employers. The civilizational view shows that alternatives exist (Germany's tuition-free model, employer-funded apprenticeship, publicly-funded university), making U.S. student debt a policy choice, not a natural law.
constraint_indexing:constraint_classification(student_debt_servitude, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(student_debt_servitude_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(student_debt_servitude, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(student_debt_servitude, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(student_debt_servitude, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(student_debt_servitude, TR),
    TR >= 0.70.

:- end_tests(student_debt_servitude_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value but not at maximum snare levels (>0.75) because some borrowers do eventually escape debt, wealth recovery is possible post-payoff (though impaired relative to tuition-free comparators), and alternatives exist (although underfunded and stigmatized). The value reflects the average lifetime extraction across the borrower cohort: 10-15 years of debt servicing represents ~15-20% of working-life earnings for many graduates, with downstream opportunity costs (delayed family formation, suppressed homeownership) extending extraction effects. Suppression (0.72): High. Multiple layers of suppression exist: (1) systemic suppression of alternative pathways (trade/apprenticeship programs funded at 1/10th the per-capita rate of college; employer training programs scaled back); (2) social/cultural suppression (college debt becomes normalized; 'personal responsibility' framing naturalizes individual obligation); (3) institutional suppression (non-dischargeable bankruptcy status means default carries permanent consequences; income-based repayment caps still require decades of servicing). Theater ratio (0.48): Moderate. The constraint shows substantial performative elements (financial aid bureaucracy, loan servicing theater, default consequences rituals) but lower than pure piton levels because the underlying extraction mechanism is functionally real — interest actually accrues, debt actually constrains behavior, wages actually decline relative to tuition-free comparators. The theater has increased from 0.35 to 0.48 over 30 years as financial aid complexity expanded (FAFSA, EFC calculations, repayment plan options) without reducing extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a profound perspectival gap between creditors (rope/coordination perspective) and borrowers (snare/extraction perspective) on the same set of base properties. The creditor institution views student lending as: risk-pooling coordination (pooling idiosyncratic student success risk into portfolio), revenue smoothing coordination (predictable cash flows for portfolio management), and positive social contribution (enabling educational access). From the creditor's arbitrage position, the constraint appears as genuine coordination with legitimate overhead costs. The borrower experiences the same system as unidirectional extraction: debt obligations suppress present consumption and future opportunity without reciprocal benefit. The analytical observer sees the structural asymmetry: creditors have arbitrage (can exit federal lending for private markets), while borrowers are trapped (cannot exit degree credential requirement or default without permanent consequence). This perspectival gap is the diagnostic signature of a snare: from the beneficiary view, it appears as legitimate coordination; from the victim view, it appears as extraction; the analytical view reveals structural asymmetry that resolves the gap in favor of extraction classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position and exit options. The trapped borrower (d ≈ 0.95) experiences maximum extractiveness: no arbitrage option, no exit path, high suppression transforms base extractiveness ε (0.58) to effective χ via high f(d). The creditor institution (d ≈ 0.05) experiences negative/minimal effective extraction — they are the beneficiary, arbitrage-enabled, high-power actor; for them, the system appears coordinative. The employer (d ≈ 0.40-0.50, constrained by competitor access to same labor pool) experiences moderate effective extraction: benefits from wage suppression but cannot unilaterally appropriate all gains. The analytical observer (d ≈ 0.72) sees the weighted average: across the population, most agents are trapped/constrained (high d), with concentrated benefit to institutional actors (low d), yielding high average d and high-magnitude snare classification. The directionality derivation chain is straightforward: beneficiary + arbitrage = low d = low χ; victim + trapped = high d = high χ; the asymmetry is the snare.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY (ε=0.58 > 0.46, therefore mandatrophy_resolved: true): The constraint is classified as snare (not disguised-coordination or false-coordination). The resolution is structural: (1) Beneficiaries are clearly identifiable (creditors, employers, government); (2) Victims are clearly trapped with no exit; (3) Suppression mechanisms are systemic (alternative pathways deliberately underfunded, debt non-dischargeable, social pressure toward debt-financed college); (4) Coordination function is negligible — the educational value transfer happens independently of the debt structure; debt merely captures rent from that value rather than enabling coordination. If this were a legitimate coordination mechanism (like genuine education financing where debt enables access that wouldn't otherwise exist), we would expect: low suppression of alternatives, low institutional enforcement requirements, strong beneficiary interest in exit options for targets. Instead, we observe: high suppression of alternatives, strong enforcement via wage garnishment and credit destruction, zero beneficiary interest in enabling borrower exit. This confirms snare classification and resolves mandatrophy by demonstrating that the extraction is not conditional on coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_labor_supply_constraint,
    'Would labor supply remain adequate without the debt-driven wage suppression mechanism if alternative education pathways (apprenticeship, trade, direct employment) were equally funded and socially prestigious?',
    'Comparative analysis of labor market outcomes in countries with different education financing models (Germany apprenticeship model vs. U.S. debt-financed college); simulation of wage and employment outcomes under counterfactual full funding of alternative pathways',
    'If adequate supply exists without debt: the suppression gate (0.72) is artificially inflated by policy choice, not structural necessity. The snare classification remains valid but the ''necessity'' narrative collapses. If labor supply would be inadequate: some extraction is structurally required for labor allocation efficiency, suggesting tangled_rope reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_labor_supply_constraint, empirical, 'Whether adequate labor supply requires debt-driven wage suppression').

omega_variable(
    debt_service_lifecycle_trap,
    'Does the 10-20 year debt servicing window create a permanent lock on career mobility and family formation, or is it a time-bounded constraint that liberates borrowers post-payoff?',
    'Longitudinal tracking of borrower cohorts: asset accumulation, homeownership, family formation, wealth gaps at 5, 10, 15, and 20-year marks post-graduation; comparison with tuition-free cohorts (international or historical)',
    'If permanent lock persists post-payoff: borrowers internalize constraints (identity_locked potential), suggesting escalation from trapped to deeper psychological capture. If constraint truly releases post-payoff: exit is possible and the trapped classification is accurate but overstates lifetime extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_service_lifecycle_trap, empirical, 'Lifecycle permanence of debt-driven constraint after repayment').

omega_variable(
    beneficiary_coordination_necessity,
    'Is the financial services benefit contingent on the snare structure (requires extraction to justify institutional profit), or could creditors achieve adequate returns from voluntary lower-extraction models?',
    'Analysis of lending profitability under counterfactual scenarios: interest-free/income-based-only lending, employer-funded training, public university funding; comparison of financial institution profit margins under different models',
    'If coordination is independent of extraction: beneficiaries could transition to Rope model without revenue collapse, suggesting the snare is policy choice not structural necessity. If extraction is essential to profitability: beneficiary''s institutional dependence on the snare is real and mandatrophy is resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_coordination_necessity, empirical, 'Whether financial institution beneficiaries require snare-level extraction for profitability').

omega_variable(
    employer_wage_suppression_quantification,
    'How much of observed wage suppression for college graduates is attributable to debt-servicing burden vs. other factors (degree saturation, globalization, labor bargaining power decline)?',
    'Regression analysis controlling for degree type, field, regional labor markets, and employer size; comparison of wage growth trajectories for debt-laden vs. debt-free cohorts; study of wage/mobility differences in countries without student debt systems',
    'If debt accounts for <20% of suppression: extraction is real but smaller than classified. If >40%: extraction is substantial and the employer tangled_rope perspective is validated as significant beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_wage_suppression_quantification, empirical, 'Quantification of employer wage-suppression benefit from student debt').

omega_variable(
    intergenerational_wealth_transfer_magnitude,
    'Over a 40-year generational cycle, how much total wealth is transferred from borrower families to creditors and employers through student debt and its downstream suppression effects?',
    'Intergenerational wealth accounting: compare cumulative lifetime earnings, asset accumulation, and intergenerational transfers for borrower cohorts vs. tuition-free comparison groups; estimate opportunity cost of delayed family formation, homeownership, and entrepreneurship',
    'If magnitude is < 5% of generational wealth: extraction is real but economically modest. If > 20%: the constraint represents massive wealth redistribution, justifying mandatrophy resolution at high severity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_wealth_transfer_magnitude, empirical, 'Intergenerational wealth transfer magnitude attributable to student debt').

omega_variable(
    identity_locked_debt_internalization,
    'Do borrowers experience debt servitude as a natural, deserved life stage (identity-fused with ''the responsible debtor'' role) or as an external cage?',
    'Qualitative analysis of borrower narratives: survey and interview data on subjective experience of constraint; analysis of rhetoric shift after major policy changes (e.g., forgiveness proposals) that alter perceived permanence',
    'If identity-locked: suppression is sustained by internalized framing, not only by material barriers. Constraint is more difficult to disrupt because targets have adopted the extractive narrative as self-identity. If externally experienced: constraint is structurally material and psychologically resisted, suggesting easier policy reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_debt_internalization, empirical, 'Whether debt servitude is identity-locked or externally experienced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(student_debt_servitude, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sds_tr_t0, student_debt_servitude, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sds_tr_t15, student_debt_servitude, theater_ratio, 15, 0.42).
narrative_ontology:measurement(sds_tr_t30, student_debt_servitude, theater_ratio, 30, 0.48).
narrative_ontology:measurement(sds_tr_t10, student_debt_servitude, theater_ratio, 10, 0.4).
narrative_ontology:measurement(sds_tr_t25, student_debt_servitude, theater_ratio, 25, 0.46).

% Extraction over time
narrative_ontology:measurement(sds_be_t0, student_debt_servitude, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sds_be_t15, student_debt_servitude, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(sds_be_t30, student_debt_servitude, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(sds_be_t5, student_debt_servitude, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(sds_be_t20, student_debt_servitude, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(student_debt_servitude, resource_allocation).
narrative_ontology:affects_constraint(student_debt_servitude, wage_stagnation_structural).
narrative_ontology:affects_constraint(student_debt_servitude, housing_affordability_crisis).
narrative_ontology:affects_constraint(student_debt_servitude, intergenerational_wealth_gap).
narrative_ontology:affects_constraint(student_debt_servitude, labor_mobility_suppression).
narrative_ontology:affects_constraint(student_debt_servitude, family_formation_deferral).

% DUAL FORMULATION NOTE:
% Student debt servitude is upstream of multiple downstream constraints in the labor market and wealth accumulation systems. High debt burdens suppress wages for college-educated workers, which becomes a structural factor in overall wage stagnation. Debt servicing obligations delay family formation and home purchase, feeding into housing affordability constraints. Intergenerational wealth transfer is impaired, contributing to wealth gap constraints. Geographic mobility is constrained, feeding into labor mobility suppression effects. Each downstream constraint has its own ε value and perspectives, but all are causally coupled to student debt through mechanisms documented in this story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
