% ============================================================================
% CONSTRAINT STORY: student_debt_burden_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_student_debt_burden_uk, []).

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
 *   constraint_id: student_debt_burden_uk
 *   human_readable: Student Debt Burden in the UK Higher Education System
 *   domain: economic/educational_policy
 *
 * SUMMARY:
 *   The UK student debt system, instituted in its current form in 2012
 *   following the Browne Review, represents a fundamental shift from public
 *   education financing (tax-based) to private debt-based financing. Students
 *   now graduate with average debt of £45,000-£50,000, repayable through
 *   income-contingent mechanisms (9% of income above £25,000 threshold). The
 *   constraint exhibits the hallmark signature of tangled rope: genuine
 *   coordination problem (how to fund expansion of higher education capacity)
 *   coexists with asymmetric extraction (burden falls disproportionately on
 *   low-income graduates whose earnings premiums may not justify the debt
 *   cost). The classification varies dramatically across income cohorts:
 *   high-earning graduates experience this as rope (coordination benefit with
 *   trivial extraction cost); low-income graduates experience it as snare
 *   (extraction with minimal coordination benefit); working-class cohorts
 *   organized politically experience it as hybrid tangled rope. The theater
 *   ratio (0.48) reflects that the system maintains legitimacy through human
 *   capital narrative ('education is an investment') while operational
 *   mechanics have shifted from risk-pooling (tax-based funding) to
 *   risk-shifting (individual debt bearing). The extractiveness has increased
 *   over the 14-year interval as: (1) debt levels have risen with tuition
 *   increases, (2) graduate earnings have stagnated or declined in many
 *   fields, (3) real repayment burdens have accumulated as interest
 *   compounds. The constraint is downstream of austerity policy choices and
 *   upstream of observable effects on fertility rates, housing delays, and
 *   brain drain migration patterns.
 *
 * KEY AGENTS:
 *   - Low-Income Graduates: Primary victims (powerless/trapped) — bear full extraction burden; no exit options. Earn £20,000-£28,000 annually; 9% repayment leaves minimal margin for savings or life investment.
 *   - Working-Class Student Cohort: Organized victims (organized/constrained) — first-generation cohort; debt burden suppresses intergenerational mobility. Student unions and Labour party position this as extractive.
 *   - High-Earning Graduates: Secondary victims / effective beneficiaries (powerful/mobile) — experience debt as marginal cost; can arbitrage via emigration or high-income arbitrage. Brain drain enabled by debt escape option.
 *   - State Education System: Primary beneficiary (institutional/arbitrage) — shifted financing burden to students; reduced public expenditure on education while maintaining capacity expansion narrative.
 *   - Financial Services Industry: Beneficiary (institutional/arbitrage) — administers debt, collects interest, securitizes loan portfolios. Risk is transferred to borrowers; revenue is stable and government-backed.
 *   - Universities: Institutional actor (institutional/arbitrage) — dependent on enrollment to fund operations; debt enables demand for places but burden is suppressing working-class applications in some regions.
 *   - Intergenerational Mobility: Primary victim (powerless/trapped) — abstract collective good that cannot organize; debt burden perpetuates class hierarchy across generations.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing debt-based financing as inherent to education rather than recognizing it as a specific policy choice with distributional consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_debt_burden_uk, 0.58).
domain_priors:suppression_score(student_debt_burden_uk, 0.65).
domain_priors:theater_ratio(student_debt_burden_uk, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_debt_burden_uk, extractiveness, 0.58).
narrative_ontology:constraint_metric(student_debt_burden_uk, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(student_debt_burden_uk, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(student_debt_burden_uk, tangled_rope).
narrative_ontology:human_readable(student_debt_burden_uk, "Student Debt Burden in the UK Higher Education System").
narrative_ontology:topic_domain(student_debt_burden_uk, "economic/educational_policy").

domain_priors:requires_active_enforcement(student_debt_burden_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(student_debt_burden_uk, state_education_system).
narrative_ontology:constraint_beneficiary(student_debt_burden_uk, financial_services_industry).
narrative_ontology:constraint_beneficiary(student_debt_burden_uk, high_earning_graduates).
narrative_ontology:constraint_victim(student_debt_burden_uk, low_income_graduates).
narrative_ontology:constraint_victim(student_debt_burden_uk, working_class_students).
narrative_ontology:constraint_victim(student_debt_burden_uk, intergenerational_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME GRADUATE (SNARE) — Trapped by debt service obligations that consume 9% of gross income indefinitely. Cannot exit through non-repayment (credit destruction, wage garnishment). Cannot exit through geographic mobility (debt follows across borders within UK). Minimal coordination benefit — the debt instrument claims to fund education but extraction persists regardless of degree utility or employment outcome. Maximum experienced extraction.
constraint_indexing:constraint_classification(student_debt_burden_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME GRADUATE (TANGLED ROPE) — Faces genuine coordination problem (education finance) alongside asymmetric extraction (debt service curtails savings, housing access, family planning). Can exit through income growth but constrained by debt burden; exit cost is delaying life milestones by 5-10 years. Genuine coordination function (debt enables access) coexists with extraction mechanism (interest accumulation, long repayment horizon).
constraint_indexing:constraint_classification(student_debt_burden_uk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE EDUCATION FUNDING AUTHORITY (ROPE) — Benefits from debt-based financing: reduces up-front public expenditure, shifts cost to students, enables expansion of higher education without matching budget increases. Experiences the constraint as coordination mechanism: student loans solve the collective action problem of education finance. No systemic extraction from state perspective; effective arbitrage (other actors bear the financing cost).
constraint_indexing:constraint_classification(student_debt_burden_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL SERVICES INDUSTRY (ROPE) — Derives coordination benefit from debt servicing infrastructure: loan administration, interest collection, debt securitization create institutional workflows and revenue streams. Experiences constraint as pure coordination: managing student debt is a stable, low-risk revenue source with government backing. No extraction perceived; effective arbitrage (profits are internalized, risk is distributed to borrowers).
constraint_indexing:constraint_classification(student_debt_burden_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: WORKING-CLASS STUDENT COHORT (TANGLED ROPE) — Organized agents (student unions, progressive political parties) see the constraint as hybrid: genuine access function (debt enables first-generation participation) coexists with extraction mechanism (debt burden perpetuates class hierarchy by delaying wealth accumulation and intergenerational mobility). Exit options constrained by political economy: debt forgiveness threatens fiscal credibility; default organizing is legally suppressed. Effective extraction depends on income trajectory — graduates with high salaries experience lower effective burden; graduates with low salaries bear asymmetric cost.
constraint_indexing:constraint_classification(student_debt_burden_uk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HIGH-EARNING GRADUATE (TANGLED ROPE) — Mobile globally; can exit through high-income arbitrage (9% repayment rate is marginal cost of debt service) or geographic migration (brain drain to higher-wage economies). Experiences constraint as minimal extraction: debt service is negligible share of income. Genuine coordination benefit persists (access to credential) with trivial extraction cost. Exit options are real and exercised, enabling selective opt-out from the extraction mechanism.
constraint_indexing:constraint_classification(student_debt_burden_uk, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: UNIVERSITY SYSTEM (PITON) — Debt-funded expansion enabled rapid growth of higher education; universities now depend on student enrollment to fund operations, but debt burden is suppressing demand among working-class cohorts. Universities perform legitimacy theater: framing debt as 'investment in human capital' despite declining graduate earnings in many fields. Primary function (education delivery) persists; coordination rationale (debt enables capacity) has atrophied as per-student costs have risen and graduate outcomes have diverged. Maintained by institutional path dependence, not functional necessity.
constraint_indexing:constraint_classification(student_debt_burden_uk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some education finance mechanism is inevitable — societies must allocate scarce resources for human capital formation. Cost-bearer selection (public vs private, tax-based vs debt-based) is a political choice, not a natural law. The engine's false summit detector identifies this as naturalization of a contingent policy choice. Treating debt-based funding as inherent to education confuses a specific institutional arrangement with a structural necessity.
constraint_indexing:constraint_classification(student_debt_burden_uk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(student_debt_burden_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(student_debt_burden_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(student_debt_burden_uk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(student_debt_burden_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(student_debt_burden_uk, TR),
    TR >= 0.70.

:- end_tests(student_debt_burden_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from low-income graduates (permanent reduction in lifetime savings capacity, delayed life milestones) but coordinates genuine education finance. The value reflects that extraction is asymmetric (concentrated on low-income cohort) rather than universal. High-earning graduates pay the same nominal debt but experience it as negligible extraction; this heterogeneity prevents the extractiveness from reaching snare levels (0.70+) when measured across the full population. Measurement trajectory shows extractiveness increasing over time as debt levels accumulate and graduate earnings stagnate — 0.35 (2012) to 0.58 (2026) — indicating that the extraction component is growing while the coordination component (education access) is stabilizing. Suppression (0.65): Moderate-high. Significant barriers to exit include: (1) legal enforceability of debt — default triggers wage garnishment, credit destruction, asset seizure; (2) career constraints — professional credentials require degree completion despite debt burden; (3) intergenerational obligation — cultural expectation that children pursue degree regardless of family capital to cover cost; (4) geographic immobility — UK debt follows bearer across regions; (5) income volatility — low-wage labor markets with intermittent employment make 9% repayment unaffordable in multiple periods, triggering debt accumulation. Suppression is not complete (some borrowers can negotiate income-contingent forgiveness after 30 years) but is substantial enough to prevent exit for majority of low-income cohort during prime working years. Theater ratio (0.48): Moderate. The legitimacy narrative emphasizes 'investment in human capital' and 'cost-sharing' but operational mechanics are debt-shifting rather than risk-pooling. The income-contingent mechanism performs legitimacy theater: framing repayment as 'affordable' (9% threshold) while masking that many borrowers will never fully repay principal. Universities perform enrollment theater: marketing degrees as 'investment' despite declining average earnings in many fields. The theater ratio is lower than piton constraints because the underlying coordination function (education access) is still operational — the theater is not sustaining a purely inert mechanism. But theater is rising: as earnings stagnate and debt burdens accumulate, more legitimacy work is required to maintain the fiction that debt-based funding is sustainable.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the low-income and high-earning graduate perspectives is the diagnostic heart of this constraint. Same constraint object (UK student debt system); divergent classifications (snare vs rope) driven entirely by exit options and income trajectory. This is the defining characteristic of tangled rope: a single institutional structure that coordinates genuine function (education finance) while extracting asymmetrically from those with limited exit capacity. The state's rope perspective is genuine — from their view, the constraint solves a coordination problem (how to fund education without unlimited public expenditure) with minimal coercive overhead. But the state's perspective is necessarily partial: it omits the extraction experienced by trapped agents. The analytical observer's role is to hold all perspectives simultaneously and recognize that the 'correct' classification is tangled rope because all perspectives are structurally valid — they're measuring different agents' relationships to the same constraint. The perspectival gap is not an error to be resolved but a feature to be documented.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Low-income graduates are declared victims with trapped exit — d approaches 1.0 (full target), f(d) ≈ 1.42 (maximum experienced extractiveness). High-earning graduates are declared secondary victims but have mobile exit options — d ≈ 0.20 (closer to beneficiary position due to arbitrage capability), f(d) ≈ 0.02 (minimal experienced extraction). State and financial services are declared beneficiaries with arbitrage exit — d ≈ 0.05 (full beneficiary position), f(d) ≈ -0.12 (negative extraction from their perspective, meaning coordination benefit flows toward them). Working-class cohorts are declared victims with constrained exit — d ≈ 0.70 (high target position), f(d) ≈ 1.10 (high experienced extraction). The scope modifier σ(S) = 1.0 (national scope) means extractiveness is unscaled for scope. The chi formula produces: χ = 0.58 × f(d) × 1.0, varying from χ ≈ 0.82 (low-income victims) to χ ≈ 0.01 (high-earning beneficiaries) to χ ≈ -0.07 (state/financial services beneficiaries). This heterogeneity in experienced extraction is precisely why the constraint cannot be classified as a uniform snare (would require χ ≥ 0.66 for all agents) but is properly classified as tangled rope (genuine coordination function alongside asymmetric extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for student debt is resolved by recognizing that the constraint exhibits genuine coordination function (education access) alongside genuine extraction (asymmetric burden). The coordination function: debt-based financing enabled expansion of higher education capacity after 2012; working-class and first-generation students accessed credentials that were previously gatekept by class background. The extraction mechanism: low-income graduates bear permanent reduction in lifetime savings and asset accumulation; burden is not proportional to earnings premium (many graduates in humanities, social sciences earn below £30,000, making 9% repayment unaffordable for family planning and housing). The classification is tangled rope, not snare, because: (1) genuine coordination benefit exists (not pure extraction), (2) beneficiaries and victims are distinct groups (state, financial services benefit; low-income graduates are victimized), (3) active enforcement is required (debt collection, wage garnishment, credit penalties). The constraint would be snare if extracted agents received zero coordination benefit and all agents perceived only extraction; it would be rope if extracted agents perceived the extraction as fair price for coordination benefit. The reality is intermediate: some agents (high-earners) get coordination benefit with trivial extraction cost; other agents (low-income) get coordination benefit with substantial extraction cost. This is tangled rope. The measurement trajectory shows extractiveness increasing over time (0.35 → 0.58) while coordination function (education access) remains stable. This indicates the constraint is drifting toward snare as the extraction component dominates the coordination component. If the trajectory continues (extractiveness → 0.70+), mandatrophy resolution would require reclassification as snare with a sunset clause or legitimacy crisis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_contingency_effectiveness,
    'Does the income-contingent repayment threshold (9% of gross income above £25,000) actually prevent debt traps, or does it mask extraction by spreading repayment across multiple decades?',
    'Longitudinal tracking of borrower lifetime earnings and total repayment amounts; analysis of cohorts that will never repay principal during working lifetime; comparison of effective interest rates after income-contingency adjustment',
    'If effective: repayment mechanism is genuine coordination (risk-sharing). If masked: income-contingency is theater disguising indefinite extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_contingency_effectiveness, empirical, 'Whether income-contingent repayment prevents or masks debt traps').

omega_variable(
    intergenerational_mobility_causal_chain,
    'Does the student debt burden directly suppress intergenerational mobility, or are low-income graduates constrained primarily by other factors (family capital, social networks, geographical immobility)?',
    'Causal analysis: comparison of asset accumulation and life milestone timing (housing, family formation, career transitions) between debt-bearing cohorts and hypothetical no-debt counterfactual; isolation of debt burden effect from correlated socioeconomic factors',
    'If debt is primary suppression mechanism: constraint is snare for working-class cohort. If debt is secondary to family capital and networks: constraint is tangled rope at worse, rope or scaffold at best.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_mobility_causal_chain, empirical, 'Causal impact of debt burden on intergenerational mobility').

omega_variable(
    credible_forgiveness_threshold,
    'What proportion of student debt would need to be forgiven before the constraint''s classification shifts from snare/tangled_rope to scaffold (temporary measure with sunset)?',
    'Political economy analysis: fiscal impact modeling; comparison with international debt forgiveness precedents (Canada 1980s, Australia proposed measures); measurement of public support thresholds for forgiveness policies',
    'If threshold < 30% forgiveness: constraint remains extractive (snare/tangled rope). If threshold > 70% forgiveness: constraint is already functionally a scaffold with government backing for debt relief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credible_forgiveness_threshold, preference, 'Debt forgiveness threshold for scaffold classification').

omega_variable(
    degree_utility_dispersion,
    'Are graduates in low-value fields (humanities, social sciences, creative disciplines) experiencing the debt burden as extraction, while graduates in high-value fields (STEM, professional credentials) experience it as coordination?',
    'Cohort analysis by field of study: tracking earnings premiums, employment rates, and debt burden relative to baseline income for each major field; identification of fields with negative lifetime earnings premium',
    'If dispersion is high and systematic: constraint decomposes into separate constraints per field. If dispersion is low: constraint is uniform across cohort.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(degree_utility_dispersion, empirical, 'Degree utility dispersion across fields of study').

omega_variable(
    brain_drain_feedback_loop,
    'Does high debt burden selectively cause emigration of high-earning graduates, creating a feedback loop where remaining UK-employed graduates have lower average earnings and higher effective debt burden?',
    'Tracking of graduate emigration rates by income quintile and field; analysis of income distribution before and after selective emigration; modeling of aggregate debt burden distribution',
    'If feedback loop exists: constraint progressively worsens for non-emigrants (extraction increases over time). This would shift the measurement trajectory upward and require mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_feedback_loop, empirical, 'Brain drain feedback loop amplifying debt burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(student_debt_burden_uk, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stdbt_tr_t0, student_debt_burden_uk, theater_ratio, 0, 0.32).
narrative_ontology:measurement(stdbt_tr_t7, student_debt_burden_uk, theater_ratio, 7, 0.4).
narrative_ontology:measurement(stdbt_tr_t14, student_debt_burden_uk, theater_ratio, 14, 0.48).
narrative_ontology:measurement(stdbt_tr_t21, student_debt_burden_uk, theater_ratio, 21, 0.52).

% Extraction over time
narrative_ontology:measurement(stdbt_be_t0, student_debt_burden_uk, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stdbt_be_t7, student_debt_burden_uk, base_extractiveness, 7, 0.47).
narrative_ontology:measurement(stdbt_be_t14, student_debt_burden_uk, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(stdbt_be_t21, student_debt_burden_uk, base_extractiveness, 21, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(student_debt_burden_uk, resource_allocation).
narrative_ontology:affects_constraint(student_debt_burden_uk, intergenerational_wealth_gap_uk).
narrative_ontology:affects_constraint(student_debt_burden_uk, housing_affordability_uk_generational).
narrative_ontology:affects_constraint(student_debt_burden_uk, fertility_decline_developed_economies).
narrative_ontology:affects_constraint(student_debt_burden_uk, skilled_emigration_brain_drain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(student_debt_burden_uk, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
