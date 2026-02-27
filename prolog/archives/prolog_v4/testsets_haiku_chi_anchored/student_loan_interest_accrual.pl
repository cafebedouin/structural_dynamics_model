% ============================================================================
% CONSTRAINT STORY: student_loan_interest_accrual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_student_loan_interest_accrual, []).

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
 *   constraint_id: student_loan_interest_accrual
 *   human_readable: Student Loan Interest Accrual
 *   domain: economic/financial_regulation
 *
 * SUMMARY:
 *   Student loan interest accrual during deferment and forbearance creates a
 *   structural trap for borrowers unable to make full payments. The
 *   constraint operates at the intersection of education access (public
 *   policy goal), financial sustainability (institutional justification), and
 *   individual hardship (borrower experience). From the borrower's
 *   perspective, particularly low-income households, accrual during periods
 *   of zero income and institutionally-permitted non-payment functions as
 *   pure extraction: the borrower bears the cost while having no income to
 *   service the growing debt. From the servicer and Department of Education
 *   perspective, accrual functions as a coordination mechanism compensating
 *   for default risk and administrative costs. The constraint exhibits the
 *   full typology depending on observer position: powerless borrowers see a
 *   Snare, institutional beneficiaries see a Rope, organized advocates see a
 *   Tangled Rope with persistent extraction despite partial protections, and
 *   the analytical observer risks naturalizing a policy choice as an economic
 *   law. Theater ratio (0.48) reflects moderate performative content:
 *   congressional debate about accrual occurs regularly (interest rate cap
 *   discussions, temporary suspensions during COVID-19), but structural
 *   changes to accrual mechanisms remain limited, suggesting the performative
 *   energy exceeds the actual functional reform.
 *
 * KEY AGENTS:
 *   - Student Borrowers: Primary victims (powerless/trapped) — cannot exit credential requirement; accrual continues even during approved deferment/forbearance with zero income
 *   - Low-Income Households: Secondary victims (moderate/constrained) — income-driven repayment provides constrained exit but residual accrual burden remains heavy during economic stress
 *   - Loan Servicers: Primary beneficiaries (institutional/arbitrage) — profit from accrual; fees increase with loan balance; minimal pressure to reduce accrual rates
 *   - Federal Department of Education: Primary beneficiary (institutional/arbitrage) — claims accrual funds program sustainability and compensates for default risk
 *   - Student Debt Advocacy Coalition: Organized challengers (organized/constrained) — perceive both coordination need and asymmetric extraction; push for accrual reform through income-driven repayment modifications
 *   - Congressional Status Quo: Institutional inertia (institutional/arbitrage) — maintains accrual through periodic reauthorization despite documented hardship; occasional temporary suspensions (COVID-19 pause 2020-2023) reveal contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_loan_interest_accrual, 0.58).
domain_priors:suppression_score(student_loan_interest_accrual, 0.62).
domain_priors:theater_ratio(student_loan_interest_accrual, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_loan_interest_accrual, extractiveness, 0.58).
narrative_ontology:constraint_metric(student_loan_interest_accrual, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(student_loan_interest_accrual, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(student_loan_interest_accrual, snare).
narrative_ontology:human_readable(student_loan_interest_accrual, "Student Loan Interest Accrual").
narrative_ontology:topic_domain(student_loan_interest_accrual, "economic/financial_regulation").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(student_loan_interest_accrual, loan_servicers).
narrative_ontology:constraint_beneficiary(student_loan_interest_accrual, federal_education_department).
narrative_ontology:constraint_victim(student_loan_interest_accrual, student_borrowers).
narrative_ontology:constraint_victim(student_loan_interest_accrual, low_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT BORROWER (SNARE) — Trapped by education credential requirement and income constraints. Cannot exit the loan system; accrual continues during deferment/forbearance even when borrower has zero income. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(student_loan_interest_accrual, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME HOUSEHOLD (SNARE) — Constrained exit via income-driven repayment, but accrual burden remains heavy; cannot afford higher payments during periods of economic stress. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(student_loan_interest_accrual, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOAN SERVICERS / FEDERAL EDUCATION DEPARTMENT (ROPE) — Institutional beneficiary. Interest accrual is presented as a coordination mechanism: servicers argue accrual funds loan operations and compensates for default risk. Government claims accrual ensures fiscal sustainability of the federal student loan program. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(student_loan_interest_accrual, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STUDENT DEBT ADVOCACY COALITION (TANGLED ROPE) — Organized agents (progressive legislatures, borrower unions, debt relief organizations) perceive both genuine coordination need (risk pooling, program sustainability) AND asymmetric extraction (accrual mechanisms benefit servicers over borrowers). Sees income-driven repayment as partial solution with residual extraction. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(student_loan_interest_accrual, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE STATUS QUO (PITON) — The accrual mechanism persists through institutional inertia despite documented hardship. Congress has periodically suspended accrual (COVID-19, 2020-2023) yet always allows restart. theater_ratio=0.48 reflects moderate performative content: legislative debate about accrual occurs regularly but produces minimal structural change. Accrual system maintained by vested interest persistence, not functional necessity.
constraint_indexing:constraint_classification(student_loan_interest_accrual, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET LOGIC VIEW — From a civilization-scale perspective, interest accrual on deferred debt may appear as an immutable law of lending markets: capital always charges for time value and default risk. However, the structural data (ε=0.58, suppression=0.62, no natural emergence property) contradicts the mountain classification. This is a false summit: accrual is a policy choice, not a market law. Income-driven repayment plans, partial accrual forgiveness, and zero-accrual alternatives (Germany, Norway) demonstrate contingency.
constraint_indexing:constraint_classification(student_loan_interest_accrual, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(student_loan_interest_accrual_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(student_loan_interest_accrual, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(student_loan_interest_accrual, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(student_loan_interest_accrual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(student_loan_interest_accrual, TR),
    TR >= 0.70.

:- end_tests(student_loan_interest_accrual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Interest accrual transfers wealth from borrowers (particularly low-income, longer repayment timeline) to servicers and federal government. The extractiveness is not maximal (0.66+) because income-driven repayment plans and periodic forbearance provide partial relief, and some accrual genuinely reflects administrative costs and default risk. The 20-year trajectory (0.35→0.58) reflects increasing accrual rates, growing loan balances, and reduced regulatory pressure on servicers post-2015 (when origination shifted from private to federal servicers). Suppression (0.62): Moderate-high. Borrowers face significant barriers to exiting the system: education credential requirement, limited alternative financing, stigma of default, and information asymmetry about accrual mechanisms. The mandatory nature of federal student loans (primary source for low-income borrowers) amplifies suppression. Theater ratio (0.48): Moderate. Congressional debate about interest rates, temporary accrual suspensions, and income-driven repayment modifications create a performative appearance of reform. The actual structural mechanism — accrual during deferment — persists unchanged despite decades of debate. The recent (2023) expansion of income-driven repayment may increase theater ratio by creating perception of reform while maintaining underlying accrual logic.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The powerless borrower sees a Snare (extraction without exit); the low-income household sees a Snare with partial mitigation (constrained exit via income-driven repayment); the institutional beneficiary sees a Rope (coordination mechanism and risk compensation); the organized advocates see a Tangled Rope (genuine coordination need AND extractive asymmetry); the legislative status quo sees a Piton (performative debate, minimal functional change); and the analytical observer risks seeing a Mountain (natural law of lending markets) until the structural contingency becomes apparent. This is a diagnostic exemplar of how indexical classification reveals that the same mechanism — accrual during non-payment — is simultaneously a coordinated risk management tool (from the lender's perspective) and a pure extraction mechanism (from the powerless borrower's perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Student borrower: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction targeting. Low-income household: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction targeting with partial exit relief. Loan servicers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; arbitrage exit means they can exit or modify the constraint without penalty. Federal Education Department: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Institutional beneficiary; claims deficit-neutral operation. Student debt advocacy coalition: Organized + constrained → d≈0.55, f(d)≈0.75. Significant extraction relative to their power, but organized status and constrained (not trapped) exit enable collective action. Legislative status quo: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate, not from directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unsubsidized_accrual_necessity,
    'Is interest accrual on unsubsidized loans during deferment economically necessary to maintain federal loan program sustainability, or does it reflect excessive servicer profit-taking?',
    'Comparative financial analysis: loan servicer profit margins vs administrative costs; comparison with international student loan systems (UK, Germany, Australia); modeling of alternative accrual schedules on program solvency',
    'If necessary: accrual is a coordination mechanism (Rope). If excessive: accrual is pure extraction (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsubsidized_accrual_necessity, empirical, 'Whether accrual is economically necessary or excess profit-taking').

omega_variable(
    income_driven_repayment_sufficiency,
    'Do income-driven repayment plans (SAVE, IBR, PAY-AS-YOU-EARN) adequately protect low-income borrowers from accrual burden, or is residual accrual during forbearance a material harm mechanism?',
    'Longitudinal tracking of borrower outcomes under SAVE vs traditional repayment; analysis of borrowers entering forbearance (medical, economic hardship); comparison of total loan amounts paid vs principal borrowed across income quartiles',
    'If protective: perceived extraction is largely mitigated (Rope from ''moderate'' power perspective). If insufficient: the snare classification holds (Snare remains for low-income cohort).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_driven_repayment_sufficiency, empirical, 'Whether income-driven repayment sufficiently protects borrowers from accrual harm').

omega_variable(
    default_risk_accrual_proportion,
    'What fraction of accrual rates actually reflects compensation for loan default risk vs. servicer profit extraction vs. administrative costs?',
    'Decomposition of federal student loan interest rates: default loss reserves vs servicer fees vs cost-of-capital. Comparison to unsecured personal loan markets with similar default rates.',
    'If default risk > 60% of accrual: stronger case for accrual as coordination/risk mechanism. If < 20%: stronger case for accrual as pure extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(default_risk_accrual_proportion, empirical, 'Proportion of accrual that reflects actual default risk compensation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(student_loan_interest_accrual, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sloan_tr_t0, student_loan_interest_accrual, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sloan_tr_t10, student_loan_interest_accrual, theater_ratio, 10, 0.42).
narrative_ontology:measurement(sloan_tr_t20, student_loan_interest_accrual, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(sloan_be_t0, student_loan_interest_accrual, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sloan_be_t10, student_loan_interest_accrual, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sloan_be_t20, student_loan_interest_accrual, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(student_loan_interest_accrual, resource_allocation).
narrative_ontology:affects_constraint(student_loan_interest_accrual, income_driven_repayment_plan_inadequacy).
narrative_ontology:affects_constraint(student_loan_interest_accrual, student_loan_forgiveness_policy_instability).
narrative_ontology:affects_constraint(student_loan_interest_accrual, federal_student_loan_servicer_monopoly).

% DUAL FORMULATION NOTE:
% Student loan interest accrual is a distinct structural constraint from the broader student debt trap. Accrual operates as the compound mechanism that transforms education access into lifetime extraction for low-income households. It is downstream of education financing policy but represents a specific mechanical constraint about interest calculation during deferment/forbearance. Related constraints (income-driven repayment inadequacy, servicer monopoly, forgiveness policy instability) all operate through the accrual mechanism as their enforcement tool.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(student_loan_interest_accrual, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
