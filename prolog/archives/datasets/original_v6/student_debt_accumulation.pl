% ============================================================================
% CONSTRAINT STORY: student_debt_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_student_debt_accumulation, []).

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
 *   constraint_id: student_debt_accumulation
 *   human_readable: Student Debt Accumulation as Extractive Constraint
 *   domain: economic/educational
 *
 * SUMMARY:
 *   Student debt accumulation functions as a structural extraction mechanism
 *   that has progressively tightened since the late 1970s. The constraint
 *   operates through credential necessity (students cannot access viable
 *   labor markets without educational credentials) combined with suppression
 *   mechanisms (limited bankruptcy protections, aggressive collection,
 *   income-based repayment that extends obligation indefinitely) and
 *   theatrical legitimation (financial aid rhetoric, income-driven repayment
 *   plans that create illusion of flexibility). The system extracts from
 *   powerless student borrowers toward lending institutions and education
 *   gatekeepers, while organized debt relief coalitions perceive it as a
 *   temporary problem with political exit pathways. From a civilizational
 *   analytical perspective, the constraint represents a fundamental shift
 *   from state-funded public education to individual debt-financed
 *   credentialing—a degradation of public infrastructure masked by financial
 *   services rhetoric.
 *
 * KEY AGENTS:
 *   - Student Borrowers: Primary victims (powerless/trapped) — require credentials for labor market access; cannot exit debt without abandoning education; bear full extraction cost through decades of repayment
 *   - Lending Institutions: Primary beneficiaries (institutional/arbitrage) — capture risk-adjusted returns, government guarantees, and fee structures; experience constraint as coordination mechanism for monetizing educational futures
 *   - Federal Student Loan Servicers: Secondary beneficiaries (institutional/arbitrage) — extract servicing fees, default collection premiums, and origination fees; maintain extraction infrastructure
 *   - Education Credential Gatekeepers: Beneficiaries (institutional/arbitrage) — universities benefit from tuition extraction and loan-enabled enrollment inflation; have no incentive to reduce credential barriers
 *   - Parent Co-Signers: Secondary victims (moderate/constrained) — bear parental obligation to support borrowing; experience joint liability and credit score damage; cannot easily exit kinship expectations
 *   - Future Cohorts: Systemic victims (powerless/trapped) — early debt accumulation constrains family formation, wealth accumulation, housing access, and intergenerational mobility for younger generations
 *   - Debt Relief Coalition: Organized agents (organized/constrained) — perceive constraint as temporary and contestable through policy change; advocate sunset pathways (debt cancellation, free college, public investment)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals extraction mechanism and systematic wealth transfer from younger to older cohorts; identifies atrophy of public education funding as root cause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_debt_accumulation, 0.68).
domain_priors:suppression_score(student_debt_accumulation, 0.72).
domain_priors:theater_ratio(student_debt_accumulation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_debt_accumulation, extractiveness, 0.68).
narrative_ontology:constraint_metric(student_debt_accumulation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(student_debt_accumulation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(student_debt_accumulation, snare).
narrative_ontology:human_readable(student_debt_accumulation, "Student Debt Accumulation as Extractive Constraint").
narrative_ontology:topic_domain(student_debt_accumulation, "economic/educational").

domain_priors:requires_active_enforcement(student_debt_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(student_debt_accumulation, lending_institutions).
narrative_ontology:constraint_beneficiary(student_debt_accumulation, loan_servicers).
narrative_ontology:constraint_beneficiary(student_debt_accumulation, education_credential_gatekeepers).
narrative_ontology:constraint_victim(student_debt_accumulation, student_borrowers).
narrative_ontology:constraint_victim(student_debt_accumulation, future_economic_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR STUDENT (SNARE) — Structurally trapped. Educational credential is essential for viable labor market entry; debt is the only documented pathway to that credential in the contemporary system. Student cannot exit without abandoning credential pursuit entirely. Suppression is maximal: income-driven repayment creates perpetual obligation; collection mechanisms are aggressive; bankruptcy protection is statutorily unavailable. Extraction runs fully toward lenders. No coordination benefit is perceived from the borrower's position — the debt mechanism purely extracts.
constraint_indexing:constraint_classification(student_debt_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LENDING INSTITUTION (ROPE) — Benefits from the debt accumulation constraint through risk-adjusted returns, government guarantees, and fee structures. Experiences the constraint as a coordination mechanism: loan origination, servicing, and collection are genuinely coordinated functions. From the lender's structural position, the constraint solves the problem of how to monetize educational futures. Exit options are abundant (arbitrage into other credit products). Extraction runs toward this institution.
constraint_indexing:constraint_classification(student_debt_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PARENT CO-SIGNER (TANGLED ROPE) — Structurally constrained by family obligation and implicit expectations. Has genuine incentive to coordinate (child's education) but bears asymmetric extraction (parental income garnishment, credit score damage). Cannot easily exit without violating kinship norms. Experiences both coordination function (enabling child's education) and extraction (parental obligation structure).
constraint_indexing:constraint_classification(student_debt_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEBT RELIEF COALITION (SCAFFOLD) — Organized agents (debt cancellation advocates, student unions, progressive legislators) perceive the student debt constraint as a temporary institutional arrangement with a sunset. Debt forgiveness programs, free college proposals, and public service loan forgiveness represent alternative pathways. Organized agents have agency and exit strategies. Extraction experienced is moderate because the exit pathway is visible and politically contestable. Theater is present (income-driven repayment plans create illusion of flexibility) but not dominant.
constraint_indexing:constraint_classification(student_debt_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EDUCATION FUNDING SYSTEM (PITON) — The shift from state-funded public higher education to individual debt-financing is a degraded institutional arrangement maintained through inertia. The original function — ensuring broad access to education — has atrophied. The system persists because alternative funding mechanisms (progressive taxation, public investment) have been politically defunded, leaving debt as the only remaining pathway. Theater is moderate (financial aid offices, income-driven repayment, public service forgiveness) masking the systematic withdrawal of public support. The piton classification derives from theater_ratio and the atrophy of public funding function.
constraint_indexing:constraint_classification(student_debt_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global view, student debt accumulation functions as a mechanism of wealth extraction and intergenerational constraint. The constraint suppresses career mobility, family formation, housing access, and wealth accumulation for debtor cohorts. Exit is analytically available only through political economy shifts (debt cancellation, public investment, alternative credentialing). The analytical perspective reveals the structure as a snare: borrower populations are trapped by credential necessity, suppressed by collection mechanisms and bankruptcy restrictions, with asymmetric extraction toward lenders and away from debtors.
constraint_indexing:constraint_classification(student_debt_accumulation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(student_debt_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(student_debt_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(student_debt_accumulation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(student_debt_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(student_debt_accumulation, TR),
    TR >= 0.70.

:- end_tests(student_debt_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. Student debt has grown from ~$35 billion (1993) to ~$1.7 trillion (2023), with extractiveness increasing as debt-to-income ratios have risen and repayment timelines have lengthened. The measurement trajectory shows extractiveness rising from 0.35 (1993: basic loan financing) to 0.68 (2023: systemic wealth extraction mechanism). Suppression (0.72): Very high. Multiple mechanisms suppress alternatives: (1) educational credential necessity creates structural necessity for borrowing; (2) bankruptcy law (2005 BAPCPA) effectively eliminated discharge of private student loans, removing fundamental safety valve; (3) income-driven repayment extends obligations indefinitely, trapping borrowers in perpetual extraction; (4) collection mechanisms (wage garnishment, tax offset, social security garnishment for borrowers 65+) are exceptionally aggressive compared to other credit products. Theater ratio (0.58): Moderate-high and increasing. Income-driven repayment plans (PAYE, REPAYE, INCOME-CONTINGENT) create theatrical illusion of flexibility while actually extending repayment periods and increasing interest accrual. Financial aid packaging creates illusion of institutional support masking systematic shift of funding burden to students. Public service loan forgiveness program (PSLF) has 99% rejection rate in early years, functioning as theater rather than genuine relief. Theater has increased as the system has become more extractive—more performative mechanisms are needed to legitimize the constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the sharp perspectival gap between powerless trapped agents (maximum snare perception) and institutional beneficiaries (rope perception) and organized challengers (scaffold perception). The gap reveals that the constraint's classification is observer-relative: the same mechanism appears as immovable snare from the borrower's position but as contestable scaffold from the debt relief coalition's position. This is not because the constraint has two natures, but because exit capacity and agency differ across structural positions. The analytical observer sees the constraint as pure snare because the analytical position can see the full extraction mechanism and its political contingency—the constraint could be unmade through policy, but isn't, which makes the extraction mechanism visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Student borrowers: high d (~0.92) derived from powerless + trapped + victim status → f(d) ≈ 1.38 → high experienced extraction. Lending institutions: low d (~0.08) derived from institutional + arbitrage + beneficiary status → f(d) ≈ -0.08 → negative/subsidy-like experienced extraction (benefit flow). Debt relief coalition: moderate d (~0.58) derived from organized + constrained + partial victim status (representing future borrowers) → f(d) ≈ 0.75 → moderate extraction, but with agency and exit visibility that lowers effective constraint. Analytical observer: moderate-high d (~0.75) derived from analytical position + systemic victim framing → f(d) ≈ 1.15 → high extraction in abstract terms, but mediated through analytical distance. No directionality overrides are needed—the structural data generates appropriate d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION WITH ORGANIZATIONAL QUALIFICATION: The primary classification (snare) is robust across powerless, moderate, and analytical perspectives. However, the 'Dynamic Coalition' extension (corpus update) suggests that the snare classification may be subject to power upgrade if the coalition of powerless agents (student borrowers) achieves sufficient organization. Current student debt activism (debt strikes, loan forgiveness advocacy) shows signs of coalition formation. If the powerless agent power is upgraded to 'organized' based on critical mass of active participants, the effective extraction coefficient (χ) would decrease, potentially shifting the constraint from snare toward tangled_rope. This does not invalidate the snare classification—it reflects the structural reality that snares can degrade when trapped agents achieve sufficient coordination. The mandatrophy is resolved by noting that the snare classification is contingent on the absence of organized borrower resistance; the presence of coalition activity suggests the constraint is actively being contested and may degrade over the measurement interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_necessity_threshold,
    'Is educational credential truly necessary for viable labor market participation, or is this a socially constructed barrier maintained by credentialing institutions?',
    'Labor market analysis comparing wage trajectories and employment access for credentialed vs non-credentialed workers controlling for ability/productivity; examination of credential inflation over time; analysis of alternative certification pathways (apprenticeships, portfolios, demonstrated competence)',
    'If credential is genuinely necessary: exit_options for students are legitimately trapped, supporting snare classification. If credential is socially constructed barrier: the trap is contingent rather than structural, and constraint reclassifies as Tangled Rope with higher leverage for escape through alternative credentialing systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_necessity_threshold, empirical, 'Whether educational credentials are structurally necessary or socially enforced').

omega_variable(
    default_rate_true_incidence,
    'What proportion of student debt defaults represent borrower inability to repay vs deliberate non-payment due to perceived unfairness or predatory lending terms?',
    'Survey data on borrower motivation for default; comparison of default rates across loan terms (federal vs private, fixed vs variable); analysis of default clustering by debt-to-income ratio; examination of borrower perceptions of loan legitimacy and fairness',
    'If defaults are inability-driven: suppression operates through material economic barriers (low income, job loss) and classification remains snare. If defaults are deliberate non-payment: suppression is maintained through enforcement mechanisms (credit scoring, wage garnishment) against borrower resistance, suggesting active extraction enforcement and stronger snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_rate_true_incidence, empirical, 'Whether defaults represent inability to pay or deliberate resistance').

omega_variable(
    intergenerational_wealth_extraction,
    'Does student debt accumulation function as a mechanism that systematically extracts wealth from younger cohorts to older wealth-holders through mechanisms of time-value-of-money and compounding interest?',
    'Cohort wealth analysis comparing net worth trajectories for pre-debt-expansion cohorts vs post-1980s cohorts controlling for education level; interest payment analysis showing cumulative wealth transfer from borrowers to lenders; modeling of lifetime earnings differential lost to debt service',
    'If wealth extraction is systematic and intergenerational: classification is robust snare. If wealth extraction is incidental to legitimate educational financing: classification shifts toward tangled_rope with weaker extraction component. This determines whether the constraint''s primary function is education coordination vs wealth transfer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_wealth_extraction, empirical, 'Whether student debt systematically extracts wealth across generations').

omega_variable(
    lender_surplus_capture,
    'What proportion of student loan profit accrues to lenders as pure surplus (extraction) vs legitimate financing cost and risk compensation?',
    'Financial analysis of lending institution loan portfolios; comparison of student loan profit margins to other credit products; analysis of loan origination costs and risk-adjusted returns; examination of federal guarantees and how much risk is actually borne by private lenders',
    'If profit is primarily surplus: supports high extractiveness (0.68) and snare classification. If profit reflects legitimate cost and risk: extractiveness should be lower (0.35-0.45) and constraint reclassifies as Tangled Rope. The gap between actual and legitimate profit is the rent-seeking component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lender_surplus_capture, empirical, 'What portion of lender profit represents surplus vs legitimate cost recovery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(student_debt_accumulation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stdbt_tr_t0, student_debt_accumulation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(stdbt_tr_t5, student_debt_accumulation, theater_ratio, 5, 0.5).
narrative_ontology:measurement(stdbt_tr_t10, student_debt_accumulation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(stdbt_tr_t15, student_debt_accumulation, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(stdbt_be_t0, student_debt_accumulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stdbt_be_t5, student_debt_accumulation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(stdbt_be_t10, student_debt_accumulation, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(stdbt_be_t15, student_debt_accumulation, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(student_debt_accumulation, resource_allocation).
narrative_ontology:affects_constraint(student_debt_accumulation, wage_stagnation).
narrative_ontology:affects_constraint(student_debt_accumulation, intergenerational_wealth_inequality).
narrative_ontology:affects_constraint(student_debt_accumulation, housing_affordability_crisis).
narrative_ontology:affects_constraint(student_debt_accumulation, credential_inflation).
narrative_ontology:affects_constraint(student_debt_accumulation, family_formation_delay).

% DUAL FORMULATION NOTE:
% Student debt accumulation is downstream of the broader constraint 'state-funded_education_defunding' (higher ε as pure policy choice) and upstream of multiple economic constraints it enables (housing access, wealth inequality, family formation timing). The network reflects causal and structural coupling: debt accumulation is not primarily a financial services constraint, but a policy constraint downstream of the decision to privatize education funding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(student_debt_accumulation, institutional, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
