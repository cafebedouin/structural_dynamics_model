% ============================================================================
% CONSTRAINT STORY: student_loan_default_cliff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_student_loan_default_cliff, []).

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
 *   constraint_id: student_loan_default_cliff
 *   human_readable: Student Loan Default Cliff: Forbearance Expiration and Repayment Shock
 *   domain: economic/financial_regulation
 *
 * SUMMARY:
 *   The student loan default cliff emerges when forbearance programs (payment
 *   suspension, interest freeze) expire and borrowers face immediate
 *   repayment obligations on federal loans totaling ~$1.7 trillion (2024).
 *   Extractiveness surged from 0.15 (forbearance period) to 0.58
 *   (post-expiration) as the policy shock converts a temporary relief
 *   mechanism into a structural repayment crisis. The constraint exhibits
 *   classic snare properties: high suppression (limited exit options),
 *   asymmetric power (borrowers vs. government collectors), and coercive
 *   mechanism (wage garnishment, credit destruction, social exclusion).
 *   However, the constraint also contains organizational fissures:
 *   income-driven repayment plans theoretically soften the cliff for
 *   moderate-income borrowers (tangled rope perspective), debt relief
 *   advocates have organized a political coalition with plausible policy wins
 *   (scaffold perspective), and the government itself faces political costs
 *   from mass defaults (which incentivizes forbearance renewal, suggesting
 *   the constraint is self-limiting). The theater ratio (0.38) is lower than
 *   expected for a snare, reflecting that the default mechanism is
 *   functionally enforced (garnishment, credit reporting) rather than
 *   performative, unlike the loan servicer infrastructure itself (which
 *   remains highly theatrical despite low functional value). The constraint's
 *   lifecycle shows rapid extraction escalation as forbearance ends, then
 *   potential policy intervention (declining theater) if political pressure
 *   succeeds in restructuring repayment terms.
 *
 * KEY AGENTS:
 *   - Low-Income Borrowers: Primary victims (powerless/trapped) — no capacity to service debt; face default, wage garnishment, credit destruction
 *   - Nontraditional Students: Secondary victims (powerless/trapped) — borrowed at life stages when 20–25 year forgiveness exceeds work-life horizon; trapped by temporal asymmetry
 *   - Middle-Income Borrowers: Moderate victims (moderate/constrained) — income-driven repayment provides partial relief; some capacity to pay; benefit from long-term forgiveness structures
 *   - Federal Government: Primary beneficiary (institutional/arbitrage) — collects from those who can pay; absorbs defaults through budget; can arbitrage policy reforms
 *   - Debt Relief Coalition: Organized actors (organized/constrained) — advocates, legal aid, progressive legislators; building scaffold (policy reform path) with sunset clause
 *   - Loan Servicers: Institutional actors (institutional/arbitrage) — degraded infrastructure maintained by regulatory lock-in; theatrical compliance role with minimal functional value
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees tangled rope structure: coordination function (market testing repayment capacity) + persistent extraction (indefinite servitude for trapped borrowers)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_loan_default_cliff, 0.58).
domain_priors:suppression_score(student_loan_default_cliff, 0.72).
domain_priors:theater_ratio(student_loan_default_cliff, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_loan_default_cliff, extractiveness, 0.58).
narrative_ontology:constraint_metric(student_loan_default_cliff, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(student_loan_default_cliff, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(student_loan_default_cliff, snare).
narrative_ontology:human_readable(student_loan_default_cliff, "Student Loan Default Cliff: Forbearance Expiration and Repayment Shock").
narrative_ontology:topic_domain(student_loan_default_cliff, "economic/financial_regulation").

% --- Structural relationships ---
narrative_ontology:constraint_victim(student_loan_default_cliff, low_income_borrowers).
narrative_ontology:constraint_victim(student_loan_default_cliff, nontraditional_students).
narrative_ontology:constraint_victim(student_loan_default_cliff, public_service_workers).
narrative_ontology:constraint_victim(student_loan_default_cliff, borrowers_in_deferment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME BORROWER (SNARE) — Forbearance expires with no viable exit. Income-driven repayment plans exist in theory but require annual certification and offer only modest payment reduction ($0–$200/month for many). Default becomes the only option for those with no discretionary income. Trapped exit + victim status → d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. Maximum extraction: borrower bears full cost of the cliff with no escape.
constraint_indexing:constraint_classification(student_loan_default_cliff, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NONTRADITIONAL STUDENT (SNARE) — Older borrowers, career-switchers, and part-time students borrowed at life stages when forgiveness timelines (20–25 years) exceed their remaining work-life horizon. Forbearance masked this structural trap. Default or perpetual debt are the only outcomes. d≈0.90, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(student_loan_default_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-INCOME BORROWER (TANGLED ROPE) — Income-driven repayment is genuinely available; monthly payments might be $300–$600 instead of $1200+. Extraction is real but mediated. Also benefits from debt forgiveness after 20–25 years (though this date recedes as payments are deferred). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55. Moderate extraction with some institutional safety valve.
constraint_indexing:constraint_classification(student_loan_default_cliff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT / DEBT COLLECTOR (ROPE) — The cliff is a coordination mechanism: it forces borrowers into formal repayment, population-level revenue recovery, and regulatory compliance. The government can arbitrage: collect from those who can pay, defer those who can't, or forgive strategically. No systemic cost — the government absorbs default losses through budget adjustments. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Net beneficiary; experiences constraint as revenue mechanism, not burden.
constraint_indexing:constraint_classification(student_loan_default_cliff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEBT RELIEF COALITION (SCAFFOLD) — Organized actors (student debt forgiveness advocates, legal aid, progressive legislators) view the cliff as a temporary coordination failure solvable by policy reform: income-driven repayment improvements, limited forgiveness programs (e.g., Public Service Loan Forgiveness), or debt cancellation. These actors are building an exit path with a sunset: permanent policy solutions replace the forbearance band-aid. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.20. Low effective extraction because the coalition has agency and sees a legislative path forward. The sunset is policy reform, estimated 5–10 years.
constraint_indexing:constraint_classification(student_loan_default_cliff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LOAN SERVICER SYSTEM (PITON) — The servicer infrastructure (Navient, Mohela, etc.) has atrophied functionally over the forbearance period. Borrower communication systems, income verification workflows, and payment processing are degraded and theatrical. The servicer persists through regulatory lock-in and contractor relationships, not functional necessity. theater_ratio=0.68 reflects that much servicer activity is compliance ritual with minimal value-add. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Piton classification from theater gate; institutional arbitrage exit.
constraint_indexing:constraint_classification(student_loan_default_cliff, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the default cliff represents a structural asymmetry: wage growth stagnation + loan debt inflation + income-driven forgiveness that never actually forgives (25 years of payments exceed principal + interest). The system coordinates debt collection while extracting from those with no real exit. The constraint is neither pure coordination (beneficiaries exist: government, educated workers with income) nor pure extraction (victims have some recourse). The observer sees the tangled hybrid: coordination function (market testing of repayment capacity) + persistent extraction (long-term debt servitude for the trapped). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(student_loan_default_cliff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(student_loan_default_cliff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(student_loan_default_cliff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(student_loan_default_cliff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(student_loan_default_cliff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(student_loan_default_cliff, TR),
    TR >= 0.70.

:- end_tests(student_loan_default_cliff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting asymmetric coercion post-forbearance. Base extraction is 58% because the government enforces repayment through garnishment and credit destruction for defaulters, while borrowers have limited escape routes. The value reflects that extraction occurs, but not absolutely (income-driven repayment and forgiveness programs exist as partial relief valves). Suppression (0.72): High. Borrowers face severe barriers to exit: (a) most cannot refinance federal loans in private markets (credit constraints), (b) private student loans cannot be discharged in bankruptcy, (c) income-driven repayment requires annual recertification and offers minimal payment reduction for low-income borrowers, (d) forbearance is temporary, not permanent. The only real exit is default, which carries severe social costs (credit destruction, wage garnishment, professional license loss). Theater ratio (0.38): Moderate. The default mechanism is functionally enforced (not theatrical), but the loan servicer infrastructure is highly theatrical. Borrower communication, income verification, and payment processing are ritualistic and degraded. The constraint's core function (debt collection) is real; the servicing layer is performative. Mandatrophy resolution: The constraint exhibits genuine extraction (χ ≈ 0.81 for trapped borrowers, computed from d≈0.92, f(d)≈1.40, σ=1.0) without mislabeling coordination as pure extraction. The government's coordination function (market testing via income-driven repayment) is real but subordinate to extraction. The snare classification is justified by suppression (0.72), ε (0.58), and high χ from powerless victims' perspective.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces dramatically different classifications across perspectives. Low-income borrowers see pure extraction (Snare, χ≈0.81) with no exit. Middle-income borrowers see mixed coordination-extraction (Tangled Rope, χ≈0.55) with partial relief available. The federal government sees pure coordination (Rope, χ≈-0.07) — debt collection is a normal governmental function, and defaults are absorbed within budget. The debt relief coalition sees a temporary problem (Scaffold) with a sunset measured in policy reform cycles (5–10 years). The loan servicer system sees its own degraded ritual (Piton, theater=0.68) maintained by contractual lock-in rather than functional necessity. The analytical observer from a civilizational perspective sees tangled rope (χ≈0.67) — the system coordinates borrowers into debt servitude while extracting long-term value. The gap reflects genuine structural asymmetries: borrowers experience different constraints based on income, organizational capacity, and exit options. No single type describes the full reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income borrowers: Victim + trapped exit → d≈0.92, f(d)≈1.40. Maximum extraction. Zero discretionary income means default is the only outcome; no institutional relief mechanism functions. Nontraditional students: Victim + trapped exit + biographical horizon → d≈0.90, f(d)≈1.38. Nearly maximum extraction; temporal mismatch (forgiveness timeline >> remaining work-life) creates structural impossibility. Middle-income borrowers: Victim + constrained exit → d≈0.65, f(d)≈0.95. Moderate extraction; income-driven repayment and forgiveness programs provide partial institutional relief. Federal government: Beneficiary + arbitrage exit → d≈0.05, f(d)≈-0.12. Net beneficiary; government can arbitrage between collection, deferment, and political forgiveness. Debt relief coalition: Organized + constrained exit → d≈0.35, f(d)≈0.35. Low effective extraction; coalition has agency and sees legislative path forward (scaffold sunset). Loan servicers: Institutional + arbitrage exit → d≈0.05, f(d)≈-0.12. Beneficiaries through contractual relationships; can exit if contracts expire. Analytical observer: analytical exit → d≈0.72, f(d)≈1.15. Sees the constraint as structurally embedded hybrid.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing extraction (ε=0.58, χ peaks at 0.81 from trapped borrowers' perspective) from coordination (income-driven repayment plans exist, forgiveness after 20–25 years is offered). The snare classification correctly identifies this as a constraint where coercion dominates coordination: yes, there are formal relief mechanisms, but they require organizational capacity (annual income verification), they extend timelines indefinitely (25-year forgiveness never actually completes because payments accumulate), and they are accessible only to those with stable income (not low-income workers with variable earnings). The mandatrophy is resolved by recognizing that mandatrophy-failing constraints exhibit high χ with low suppression (appearing as coordination when the underlying mechanism is coercive). This constraint has high suppression (0.72), which rules out that inversion. The extraction is real and suppression is structural. The constraint classifies as snare with high confidence (χ≈0.81 from primary victim perspective). The scaffold perspective (policy reform with sunset) is analytically distinct — it represents a potential future state, not the current structural reality. Current reality is snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    idr_plan_sufficiency,
    'Do income-driven repayment plans actually provide affordable access to repayment, or do they merely extend the debt timeline indefinitely?',
    'Longitudinal analysis of borrowers on income-driven plans: comparison of lifetime payments vs. principal + interest; tracking of forgiveness completion rates (borrowers reaching 20–25 year mark with debt actually forgiven)',
    'If sufficient: constraint is tangled rope (extraction mediated by coordination). If insufficient: constraint is closer to snare (IDR is theatrical relief with no real exit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(idr_plan_sufficiency, empirical, 'Whether income-driven repayment plans provide genuine affordability or extend indefinite servitude').

omega_variable(
    forgiveness_implementation_deadline,
    'What policy timeline for genuine debt relief (forgiveness, restructuring, or income-linked repayment) would convert this snare into a scaffold with a real sunset?',
    'Policy scorecard: track enactment of forgiveness programs, income-linked repayment legislation, and sunset deadlines; measure reduction in default cliff severity over 5–10 year period',
    'If < 3 years: scaffold perspective is realistic. If > 15 years: scaffold is aspirational rather than structural; constraint remains snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forgiveness_implementation_deadline, preference, 'Political timeline for policy reform converting snare to sunset scaffold').

omega_variable(
    wage_growth_recalibration,
    'Can nominal wage growth exceed student debt service obligations at the population level, creating a structural escape valve?',
    'Cohort analysis: median wage growth vs. median monthly student loan payment for borrowers entering repayment in 2023–2026; projection of payment-to-income ratios over 10-year period',
    'If wage growth > 3% annually: constraint softens over time (Rope or Scaffold). If wage growth < 1.5%: constraint hardens (Snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_growth_recalibration, empirical, 'Whether wage growth can structurally outpace debt service obligations').

omega_variable(
    default_contagion_threshold,
    'At what default rate does the student loan system trigger cascade failure (servicer insolvency, government bailout, credit market freezing)?',
    'Financial stability analysis: servicer balance sheets; stress testing of cascading default scenarios; measurement of systemically important thresholds',
    'If threshold < 15% default rate: cliff induces immediate systemic crisis (government forced to extend forbearance or cancel). If > 25%: cliff can be absorbed within existing institutional capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(default_contagion_threshold, empirical, 'Default rate threshold for triggering systemic financial crisis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(student_loan_default_cliff, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sldc_tr_t0, student_loan_default_cliff, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sldc_tr_t12, student_loan_default_cliff, theater_ratio, 12, 0.3).
narrative_ontology:measurement(sldc_tr_t24, student_loan_default_cliff, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(sldc_be_t0, student_loan_default_cliff, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sldc_be_t12, student_loan_default_cliff, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(sldc_be_t24, student_loan_default_cliff, base_extractiveness, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(student_loan_default_cliff, resource_allocation).
narrative_ontology:affects_constraint(student_loan_default_cliff, income_driven_repayment_sustainability).
narrative_ontology:affects_constraint(student_loan_default_cliff, public_service_loan_forgiveness).
narrative_ontology:affects_constraint(student_loan_default_cliff, consumer_financial_protection_bureau).

% DUAL FORMULATION NOTE:
% The default cliff decomposes into three structurally distinct constraints: (1) The cliff itself as a temporary shock (this story, ε=0.58, snare), (2) The long-term sustainability of income-driven repayment plans (ε≈0.45, tangled rope — coordination mechanism with extraction overlay), (3) The public service forgiveness program (ε≈0.35, scaffold — genuinely temporary relief with sunset). Each has different temporal dynamics and policy solutions. This story focuses on the immediate cliff; downstream stories address institutional sustainment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
