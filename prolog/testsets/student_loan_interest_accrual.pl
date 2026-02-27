% ============================================================================
% CONSTRAINT STORY: student_loan_interest_accrual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Student Loan Interest Accrual During Deferment
 *   domain: economic/debt/higher_education
 *
 * SUMMARY:
 *   Student loan interest accrual during deferment or forbearance is a
 *   structural mechanism embedded in U.S. federal and private student loan
 *   contracts. When a borrower is unable to pay (due to unemployment,
 *   hardship, or continued education), they can request deferment or
 *   forbearance—a temporary suspension of payments. However, interest on the
 *   underlying principal continues to accrue unless the borrower's loan is
 *   classified as 'subsidized' (federal government pays interest) or the
 *   borrower is on an income-driven repayment plan with interest subsidies.
 *   This constraint creates a debt spiral: borrowers who defer because they
 *   cannot afford payments see their total debt burden increase invisibly,
 *   emerging from deferment owing more than they borrowed. The mechanism
 *   operates with minimal coercion—no enforcement action is needed because
 *   the accrual is automatic and incorporated into loan contracts.
 *   Suppression is high because borrowers often lack full awareness of
 *   accrual mechanics and have no exit options while in deferment. The
 *   constraint's extractiveness has increased over the past decade as: (1)
 *   more borrowers enter deferment due to underemployment, (2) private loan
 *   servicers have captured regulatory oversight of the process, (3)
 *   subsidized loan availability has declined, and (4) political willingness
 *   to freeze or subsidize accrual has eroded. The theater ratio remains low
 *   (0.35) because accrual is functionally transparent—the mechanism does
 *   what it appears to do—even though awareness of that function is low.
 *
 * KEY AGENTS:
 *   - Deferred Borrowers: Primary victims (powerless/trapped) — cannot exit deferment; bear full cost of accrual
 *   - Low-Income Graduates: Secondary victims (moderate/constrained) — limited alternatives; face payment shocks when deferment ends
 *   - Loan Servicers: Primary beneficiaries (institutional/arbitrage) — capture servicer fees and interest revenue; operate as monopoly administrators
 *   - Federal Treasury: Secondary beneficiary (institutional/arbitrage) — captures interest revenue on federal loans; subsidies declined after 2012
 *   - Borrower Advocacy: Secondary actor (organized/constrained) — push for reform but constrained by servicer lobbying and legislative capture
 *   - Federal Student Loan Program: Institutional actor (institutional/arbitrage) — maintains accrual policy through inertia and regulatory alignment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_loan_interest_accrual, 0.52).
domain_priors:suppression_score(student_loan_interest_accrual, 0.68).
domain_priors:theater_ratio(student_loan_interest_accrual, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_loan_interest_accrual, extractiveness, 0.52).
narrative_ontology:constraint_metric(student_loan_interest_accrual, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(student_loan_interest_accrual, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(student_loan_interest_accrual, snare).
narrative_ontology:human_readable(student_loan_interest_accrual, "Student Loan Interest Accrual During Deferment").
narrative_ontology:topic_domain(student_loan_interest_accrual, "economic/debt/higher_education").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(student_loan_interest_accrual, loan_servicers).
narrative_ontology:constraint_beneficiary(student_loan_interest_accrual, federal_treasury).
narrative_ontology:constraint_beneficiary(student_loan_interest_accrual, secondary_loan_market).
narrative_ontology:constraint_victim(student_loan_interest_accrual, deferred_borrowers).
narrative_ontology:constraint_victim(student_loan_interest_accrual, forbearance_borrowers).
narrative_ontology:constraint_victim(student_loan_interest_accrual, low_income_graduates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFERRED BORROWER (SNARE) — Borrower placed in deferment due to unemployment, hardship, or enrollment in graduate school. Cannot exit: deferment is often mandatory for certain conditions (e.g., public service loan forgiveness qualification period), and private alternatives are unavailable for federal loans. Interest accrues regardless of payment capacity, creating a principal-growth trap. Maximum extraction experienced — zero agency, zero alternatives, compounding debt burden invisible during deferment period.
constraint_indexing:constraint_classification(student_loan_interest_accrual, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME GRADUATE (SNARE) — Holds federal student loans from undergraduate education. Cannot afford payments during early career phase; eligible for income-driven repayment plans or deferment. Interest continues accruing; capitalized interest appears on statement but borrower has no choice about accrual mechanism. Exit is theoretically possible (private refinancing) but inaccessible due to credit profile. Experiences extraction as structural necessity rather than coercive enforcement — no enforcement needed because the mechanism is locked into the federal loan contract.
constraint_indexing:constraint_classification(student_loan_interest_accrual, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOAN SERVICERS & FEDERAL TREASURY (ROPE) — Servicers manage loan accounting and collections; federal treasury captures revenue from interest. Interest accrual during deferment/forbearance is a coordination mechanism: it preserves the loan's value against inflation and creates incentive for borrowers to resume payments. Servicers experience the constraint as functional—interest accrual enables their business model and aligns with federal revenue objectives. Exit is not needed because this perspective benefits from the accrual mechanism. High f(d) approaching negative values indicates net beneficiary status.
constraint_indexing:constraint_classification(student_loan_interest_accrual, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BORROWER ADVOCACY & CONSUMER PROTECTION (TANGLED ROPE) — Consumer advocacy groups, state attorneys general, and Congress see accrual-during-deferment as both coordination mechanism (prevents principal decay) and extraction (harms vulnerable borrowers). Reform efforts create competing extraction narratives: servicers extract via accrual; advocacy groups extract political pressure. Advocacy has constrained exit—they cannot unilaterally end accrual but can push legislation. Experiences both benefit (forced savings discipline) and cost (debt trap escalation). Suppression is high (servicer information asymmetry, regulatory capture of Education Dept); coordination function exists (prevents moral hazard) but is overshadowed by extraction.
constraint_indexing:constraint_classification(student_loan_interest_accrual, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL STUDENT LOAN PROGRAM (PITON) — The accrual-during-deferment rule persists as institutional inertia. Originally designed (1960s-1980s) to address moral hazard—prevent borrowers from using deferment as indefinite payment avoidance. The theater ratio is moderate (0.35)—accrual has real functional content (prevents principal decay, funds servicer operations), but policy enforcement is largely symbolic: forbearance is available as an alternative, income-driven repayment caps payments regardless of accrual, and periodic policy freezes (COVID-19 pandemic) demonstrate that accrual is not immutable. The constraint maintains itself through servicer lobbying and default departmental procedures, not because the underlying function is essential.
constraint_indexing:constraint_classification(student_loan_interest_accrual, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a mathematical perspective, interest accrual on deferred balances is derived from the fundamental rules of compound interest: A = P(1 + r/n)^(nt). No agent can 'choose' not to accrue interest without violating the definition of interest itself. This perspective risks naturalizing what is actually a policy choice (the interest rate, the accrual mechanism, the deferment eligibility rules). The engine's false summit detector will identify this as a false mountain—compound interest is a mathematical law, but accrual-during-deferment is a policy design choice that could be replaced (e.g., accrual freeze during deferment, or interest subsidy by the federal government as was the case for subsidized loans through 2012).
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
 *   Extractiveness (0.52): High-moderate. Accrual during deferment extracts wealth from borrowers by increasing total debt burden during periods of economic vulnerability. However, extraction is not maximal (≥0.66 for pure snares) because: (1) accrual rates are transparent and visible in loan documents, (2) income-driven repayment plans partially offset accrual burden through payment caps, and (3) some borrowers have access to subsidized or subsidies-available programs. The extractiveness has increased over time as subsidized loan programs have declined and servicer discretion over deferment terms has expanded. Suppression (0.68): High. Borrowers in deferment face multiple suppression mechanisms: (a) lack of awareness that accrual continues, (b) no private loan alternatives for federal loans (monopoly access), (c) career constraints that forced deferment in the first place make exit infeasible, (d) servicer information asymmetry—borrowers often cannot obtain clear accrual calculations until emerging from deferment, (e) regulatory capture—the Education Department primarily works with servicers rather than borrower advocates. Theater ratio (0.35): Low. Accrual is not performative—the mathematical mechanism works exactly as described. The theater is present in regulatory framing (accrual presented as 'default interest cost' rather than 'policy choice') and servicer compliance theater (borrowers receive notices of accrual but rarely understand them), but the underlying mechanism is substantive.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the deferred borrower's perspective (Snare—maximum extraction, no exit, accumulating debt trap) and the servicer's perspective (Rope—revenue mechanism aligned with institutional function) is maximal. The borrower sees a debt trap; the servicer sees a standard business function. The advocacy perspective (Tangled Rope) occupies the middle ground: accrual serves a coordination function (prevents moral hazard through incentivizing repayment) but is overshadowed by extraction (harms vulnerable populations). The piton perspective (Federal Student Loan Program viewed institutionally) reveals that accrual is maintained partly through inertia—the COVID-19 payment pause (2020-2023) demonstrated that accrual can be frozen, and borrowers did not dramatically increase deferment abuse when accrual was suspended. The analytical/natural law perspective risks falsely naturalizing compound interest mathematics as if accrual-during-deferment were inevitable—but subsidized loans (which existed until 2012) proved that accrual can be prevented through policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Servicers and the Treasury derive d from their role as beneficiaries with arbitrage exit options. The pipeline computes their experienced extractiveness as negative or near-zero because they benefit from the accrual mechanism and can, theoretically, choose to end it—but profit-maximization prevents them from doing so. Borrowers derive d from their role as victims with trapped exit options. Maximum d (0.95+) produces maximum f(d) (~1.42), which amplifies their experienced extractiveness χ = ε × f(d) × σ(S). The advocacy coalition derives d from constrained exit—they have organizational power but cannot unilaterally change the mechanism; they are neither victims nor beneficiaries. Overrides are not needed because the structural derivation captures the relationship: accrual flows from borrowers to servicers with no genuine alternatives for deferred borrowers.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (CONCEPTUAL): The constraint avoids the mandatrophy by distinguishing the coordination function (accrual-as-moral-hazard-prevention) from the extraction mechanism (accrual-as-debt-accumulation). Pure coordination (Rope) would require that accrual equally benefit borrower and servicer—it does not; borrowers in deferment uniformly experience extraction. Pure extraction (Snare) would require that servicers had no legitimate coordination rationale—they do; moral hazard is a real coordination problem. The Snare classification (from borrower and low-income graduate perspectives) is correct because the extraction is primary and the coordination is secondary or absent from their viewpoint. The Tangled Rope classification (from advocacy perspective) is correct because reform efforts acknowledge both the moral hazard problem (coordination) and the extraction harm (borrower trap). The constraint does not collapse because each classification is indexed to a specific observer position. The analytical observer's false mountain (naturalizing accrual as inherent to interest) is caught by the engine's false summit detector: accrual-during-deferment is a policy choice, not a mathematical or physical law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferment_necessity_threshold,
    'Is accrual-during-deferment necessary to prevent moral hazard, or does it create worse behavior (strategic default, work avoidance)?',
    'Comparative analysis of default rates and repayment behavior under accrual vs. no-accrual regimes; natural experiments (COVID-19 payment pause, state-level subsidies); longitudinal borrower surveys on deferment decision-making',
    'If moral hazard dominant: accrual serves a coordination function (Rope classification stronger). If perverse incentives dominant: accrual is pure extraction (Snare classification stronger).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferment_necessity_threshold, empirical, 'Whether accrual-during-deferment prevents or encourages default').

omega_variable(
    servicer_revenue_dependency,
    'How much of the loan servicer''s business model depends on interest accrual during deferment? Can servicers be profitable under accrual-free deferment?',
    'Financial analysis of servicer revenue streams; cost-benefit modeling of operations under different accrual policies; comparison to subsidized loan model (pre-2012)',
    'If high dependency: accrual is essential to the servicer''s extraction mechanism (Snare or Tangled Rope classification). If low dependency: accrual is a policy choice that serves other objectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(servicer_revenue_dependency, empirical, 'Servicer profitability under accrual-free deferment').

omega_variable(
    borrower_awareness_completeness,
    'Do borrowers understand that interest accrues during deferment? Does accrual lack transparency function as part of the extraction mechanism?',
    'Surveys of borrowers in deferment; analysis of loan disclosure documents; comparison of stated awareness to actual accrual-related defaults and payment shocks',
    'If awareness is low: accrual functions as hidden extraction (Snare classification stronger, suppression higher). If awareness is high: accrual is transparent mechanism (classification shifts toward Rope or Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(borrower_awareness_completeness, empirical, 'Borrower awareness of accrual-during-deferment').

omega_variable(
    federal_subsidy_capacity,
    'Could the federal government afford to subsidize interest during deferment (as it did for subsidized loans pre-2012)? Is non-accrual politically blocked or fiscally impossible?',
    'Federal budget analysis; cost-benefit of subsidy vs. current default write-offs; comparison of subsidy costs to other education spending',
    'If affordable and blocked: accrual is a policy choice extracting from borrowers to preserve servicer revenue (Snare classification). If fiscally constrained: accrual reflects genuine resource scarcity (Tangled Rope or Scaffold classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_subsidy_capacity, empirical, 'Federal budget capacity for interest subsidies during deferment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(student_loan_interest_accrual, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sloan_tr_t0, student_loan_interest_accrual, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sloan_tr_t5, student_loan_interest_accrual, theater_ratio, 5, 0.33).
narrative_ontology:measurement(sloan_tr_t10, student_loan_interest_accrual, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(sloan_be_t0, student_loan_interest_accrual, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sloan_be_t5, student_loan_interest_accrual, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sloan_be_t10, student_loan_interest_accrual, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(student_loan_interest_accrual, resource_allocation).
narrative_ontology:affects_constraint(student_loan_interest_accrual, income_driven_repayment_cap_evasion).
narrative_ontology:affects_constraint(student_loan_interest_accrual, default_cascade_mechanism).
narrative_ontology:affects_constraint(student_loan_interest_accrual, public_service_loan_forgiveness_gaming).

% DUAL FORMULATION NOTE:
% Student loan interest accrual operates as both a coordination mechanism (preventing moral hazard through incentivizing repayment) and an extraction mechanism (creating debt spirals for deferred borrowers). This constraint family includes: (1) accrual-during-deferment (this story, ε=0.52, Snare), (2) subsidized loan elimination (ε=0.45, Tangled Rope—coordination loss), (3) servicer capture of Education Department (ε=0.55, Snare—institutional extraction). Accrual is downstream of servicer monopoly and upstream of default cascades.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(student_loan_interest_accrual, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
