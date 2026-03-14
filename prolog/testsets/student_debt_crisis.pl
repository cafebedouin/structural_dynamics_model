% ============================================================================
% CONSTRAINT STORY: student_debt_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_student_debt_crisis, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: student_debt_crisis
 *   human_readable: Student Debt as Coordination and Extraction Mechanism
 *   domain: economic/educational/policy
 *
 * SUMMARY:
 *   The student debt crisis represents a structural transformation in how
 *   education access is financed in the United States. What began in the
 *   1990s as a coordination mechanism to enable higher education access for
 *   students lacking family wealth has gradually shifted toward an extraction
 *   mechanism that transfers educational costs from public/institutional
 *   shoulders to individual borrowers, with significant accumulation of
 *   financial burden. The system exhibits properties of a Tangled Rope:
 *   genuine coordination function (financing access when alternative funding
 *   is unavailable), but with embedded asymmetric extraction (borrowers bear
 *   risk and service costs; institutions capture enrollment revenue and
 *   guaranteed repayment). The extractiveness measure has risen from 0.35
 *   (early period, genuine access function) to 0.62 (recent period,
 *   extraction-dominant), while the theater ratio has increased from 0.42 to
 *   0.58, indicating rising performativity: the 'investment in human capital'
 *   narrative masks the actual mechanism (individual debt burden as tuition
 *   funding substitute). Key structural agents are the financial institutions
 *   (beneficiaries), borrowing students (primary victims), universities
 *   (beneficiary-victims in a cost spiral), and the government (beneficiary
 *   through fiscal deferral). The identity-locked perspective reveals a
 *   secondary extraction mechanism: debt anchors borrowers to specific career
 *   trajectories, suppressing alternative life choices and labor market
 *   mobility.
 *
 * KEY AGENTS:
 *   - Borrowing students (powerless/trapped → moderate/constrained): Face the maximum extraction. Lack exit options in immediate term; some mobility in biographical term through salary negotiation, career change, or geographic arbitrage.
 *   - Financial institutions (institutional/arbitrage): Primary beneficiaries. Originate, service, and hold loans as stable guaranteed assets. Full exit capacity.
 *   - Public universities (organized/constrained): Institutional beneficiary trapped in cost spiral. Benefit from tuition revenue but cannot reduce prices; trapped by state disinvestment and enrollment-dependent budget models.
 *   - Government treasury (institutional/arbitrage): Shifts fiscal burden to future workers. Full exit capacity; receives repayment revenue while guaranteeing default risk.
 *   - Career-locked borrowers (moderate/identity_locked): Structurally mobile but identity-fused to high-income career path. Debt service anchors identity and life choices.
 *   - Educational access narrative (institutional/inertial): Piton classification. Legitimating mythology persists despite declining causal connection to actual access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_debt_crisis, 0.62).
domain_priors:suppression_score(student_debt_crisis, 0.68).
domain_priors:theater_ratio(student_debt_crisis, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_debt_crisis, extractiveness, 0.62).
narrative_ontology:constraint_metric(student_debt_crisis, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(student_debt_crisis, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(student_debt_crisis, tangled_rope).
narrative_ontology:human_readable(student_debt_crisis, "Student Debt as Coordination and Extraction Mechanism").
narrative_ontology:topic_domain(student_debt_crisis, "economic/educational/policy").

domain_priors:requires_active_enforcement(student_debt_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(student_debt_crisis, financial_institutions).
narrative_ontology:constraint_beneficiary(student_debt_crisis, government_treasury).
narrative_ontology:constraint_beneficiary(student_debt_crisis, universities).
narrative_ontology:constraint_victim(student_debt_crisis, borrowing_students).
narrative_ontology:constraint_victim(student_debt_crisis, social_mobility).
narrative_ontology:constraint_victim(student_debt_crisis, future_wage_earners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEBTED GRADUATE (SNARE) — Trapped by debt obligation with no material exit. Education is positioned as necessity for labor market participation, but cost structure transfers to individual borrower. Cannot walk away without destroying credit rating, employment prospects, and access to future capital. Suppression is total: legal frameworks enforce repayment; labor market provides no alternative path to equivalent income. Maximum extraction from this position.
constraint_indexing:constraint_classification(student_debt_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME BORROWER (TANGLED ROPE) — Faces surmountable but high-cost exit options. Can relocate for higher-wage work, negotiate salary above debt service, or defer consumption. Benefits from the coordination function: student loans exist because education access needs financing. But bears asymmetric extraction: debt service reduces lifetime wealth accumulation; repayment timeline extends into family formation years. Mixed experience — genuine coordination for access, significant asymmetric extraction.
constraint_indexing:constraint_classification(student_debt_crisis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTIONS (ROPE) — Primary beneficiary. Student loans are lower-risk assets (backed by government guarantee, non-dischargeable in bankruptcy, enforceable across state lines). Experience the system as pure coordination: originating loans, servicing payments, and managing default risk. Arbitrage options abundant (refinance portfolios, securitize, sell to secondary markets). Extraction is one-directional toward these institutions but experienced as legitimate cost recovery and risk compensation.
constraint_indexing:constraint_classification(student_debt_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC UNIVERSITIES (TANGLED ROPE) — Institutional beneficiary with constrained exit. Benefit from stable funding stream (tuition revenue enables operating budgets, construction, research). But also trapped in cost spiral: cannot reduce prices below what students can finance; cannot reduce funding expectations. Coordination function is genuine (funding mechanism for educational access). Extraction is bidirectional: extract from students through tuition inflation; extracted from by state disinvestment (debt shift absorbs what public funding decline creates).
constraint_indexing:constraint_classification(student_debt_crisis, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GOVERNMENT TREASURY (ROPE) — Institutional beneficiary. Student loan program defers public education costs to individual borrowers; shifts fiscal burden from current generation to future workers. Government guarantees loans (absorbs default risk) but receives repayment revenue stream and net reduction in current budget pressure. Experiences system as pure coordination: financing mechanism for educational access that stabilizes public finances. Arbitrage options (refinance terms, program design, forgiveness policies) remain fully available.
constraint_indexing:constraint_classification(student_debt_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EDUCATIONAL ACCESS NARRATIVE (PITON) — The theatrical justification for student debt frames it as a public good (investment in human capital, meritocratic access to higher education). This narrative persists despite declining causal connection: debt has grown while real wages for degree holders stagnated; debt forgiveness is less expensive than traditional grant programs; debt burden suppresses the very social mobility the narrative promises. The ritual of student loans persists through institutional inertia and the legitimating narrative ('borrowing for education is normal and fair'), not because the mechanism achieves its stated function.
constraint_indexing:constraint_classification(student_debt_crisis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CAREER-LOCKED DEBT HOLDER (SNARE) — Structurally mobile (could change careers, relocate, renegotiate) but identity-locked into high-income career path by debt obligation. Debt service anchors them to specific salary requirements and career trajectory. Cannot exercise exit options (entrepreneurship, lower-wage meaningful work, sabbatical, family caregiving) because identity is constituted through career achievement necessary to service debt. The binding is cognitive and relational (self-concept depends on wage sufficiency) even though material barriers to exit are modest.
constraint_indexing:constraint_classification(student_debt_crisis, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks viewing student debt as immutable law: 'someone must pay for education; cost cannot be eliminated; debt is inevitable consequence of financing access.' This perspective naturalizes what the empirical data shows is contingent: debt financing is a policy choice, not a law of nature. Other countries finance higher education through taxation, endowments, or reduced-cost structures without individual debt burden. The mountain classification is a false summit — it misidentifies a contingent institutional arrangement as structural necessity.
constraint_indexing:constraint_classification(student_debt_crisis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(student_debt_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(student_debt_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(student_debt_crisis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(student_debt_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(student_debt_crisis, TR),
    TR >= 0.70.

:- end_tests(student_debt_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The system transfers 80%+ of education costs to individual borrowers, with debt service extending 10-30 years post-graduation. But extractiveness is not maximal because: (a) some borrowers do experience lifetime earnings gains that exceed debt costs; (b) government income-driven repayment programs provide partial relief; (c) periodic forgiveness proposals suggest the mechanism can be challenged. The 0.62 value reflects that extraction is significant but not total — borrowers can sometimes generate sufficient surplus income to service debt and survive. The upward trajectory (0.35 → 0.62 over 30 years) reflects increasing extraction pressure as tuition inflation outpaced wage growth. Suppression (0.68): High. Multiple enforcement mechanisms prevent exit: legal non-dischargeability in bankruptcy; wage garnishment capacity; credit rating destruction for default; employment barriers for federal/professional roles. But suppression is not absolute: income-driven repayment programs provide partial relief valve; state-level forgiveness programs create small exit windows. Suppression magnitude increased over the interval as enforcement mechanisms were strengthened (wage garnishment, tax offset authority) and deferment/forbearance options were restricted. Theater ratio (0.58): Moderate-high. The 'investment in human capital' narrative provides legitimacy, but empirical connection has weakened: real wages for degree holders stagnated (2000-2020) while debt grew exponentially. The theatrical element is the framing ('borrowing for education is normal and fair') rather than the mechanism itself. Theater increased over the interval as the narrative became more divergent from wage reality.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals the system's structural asymmetry. Beneficiaries (institutions, government, lenders) perceive the system as pure coordination (Rope) — a necessary financing mechanism. Borrowers perceive it as extraction with suppression (Snare). The university is trapped between both positions (Tangled Rope) — they benefit from tuition revenue but have become victims of the cost spiral created by the same debt system. The identity-locked borrower reveals a secondary binding mechanism: debt obligation anchors career trajectory, suppressing labor market mobility even for those with formal exit capacity. The piton perspective reveals the theatrical cover story (human capital investment) has decoupled from material reality (stagnant wages) but persists because the narrative legitimates the mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness chi is derived from the agent's power level, exit options, and structural relationship to the debt mechanism. Trapped borrowers with zero exit options experience maximum chi (d ≈ 0.95, f(d) ≈ 1.42). Constrained middle-income borrowers experience moderate chi (d ≈ 0.65, f(d) ≈ 1.00) because they retain options at cost. Identity-locked borrowers at moderate power experience high chi despite structural mobility (d ≈ 0.80) because the cognitive lock prevents exercising options. Institutional beneficiaries (financial institutions, government) experience negative or minimal chi (d ≈ 0.10-0.20) because the extraction flow runs toward them. The chi formula χ = ε × f(d) × σ(S) amplifies extractiveness at national scope (σ = 1.0) and across diffuse agent base, making the constraint's total extraction pressure significant despite moderate base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the Tangled Rope classification, which acknowledges both the genuine coordination function (education access financing) and the asymmetric extraction (individual debt burden, suppressed mobility, wealth transfer to institutions). The mandatrophy is resolved not by choosing between 'is this extraction or coordination?' but by recognizing it as a hybrid mechanism where the coordination function has weakened (as alternative funding models became available) while the extraction mechanism has strengthened (through legal enforcement, wage garnishment, non-dischargeability). The system is not pure extraction (Snare classification would not capture the genuine access function that existed and persists for some borrowers). But it is not pure coordination (Rope classification would erase the asymmetric burden transfer and career anchoring). Tangled Rope is the accurate classification that preserves both dimensions. The identity-locked perspective adds diagnostic depth: borrowers are not merely trapped by material barriers (which would make this a Snare) but by cognitive fusion with career-identity-debt interdependency, revealing a secondary extraction mechanism that Snare alone would not capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_debt_service_threshold,
    'At what debt-to-income ratio does educational investment cease to produce net lifetime wealth gain for borrowers?',
    'Longitudinal wage studies tracking borrower cohorts; calculation of cumulative lifetime earnings minus debt service across education levels and degree types; comparison with counterfactual no-degree trajectories',
    'If threshold is crossed for undergraduate degrees: system has shifted from investment financing to extraction. If threshold maintained: system retains coordination function despite extraction overlay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_debt_service_threshold, empirical, 'Debt-to-income threshold where education investment reverses to wealth extraction').

omega_variable(
    alternative_financing_viability,
    'Could public funding, income-share agreements, or employer-sponsored education reduce total extraction without eliminating access?',
    'Comparative analysis of alternative financing models; cost projections for public funding via progressive taxation; default rates and borrower outcomes under alternative models',
    'If viable: current student debt system is policy choice favoring creditors, not necessity. Classification becomes pure Snare for powerless borrowers. If unviable: extraction is minimally necessary and system may be genuine Tangled Rope coordination-extraction hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financing_viability, empirical, 'Feasibility of alternative education financing models').

omega_variable(
    identity_lock_persistence,
    'Do debt holders who achieve financial security maintain identity-locked career trajectories, or do they exercise previously-constrained exit options?',
    'Longitudinal studies of career changes following debt payoff or forgiveness; behavioral analysis of borrowers who achieve financial security (excess income beyond debt service); comparison with debt-free cohorts',
    'If borrowers remain locked: identity fusion is deeper than debt obligation and persists post-payoff. If borrowers exit: debt was the primary lock mechanism, and payoff enables identity reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Persistence of identity lock after debt obligation removal').

omega_variable(
    coordination_function_residual,
    'What proportion of student loan origination serves genuine educational access coordination versus speculative extraction?',
    'Decompose loan volume into: (a) borrowers who graduate with improved lifetime earnings; (b) borrowers whose lifetime earnings do not exceed debt service; (c) borrowers who default; (d) borrowers whose income would have been sufficient without debt burden. Track shifts in proportion over time.',
    'If coordination portion < 40%: system is primarily extraction with coordination cover story. If coordination portion > 60%: system retains significant hybrid function. Shift in proportions over time indicates whether system is degrading (rising Piton likelihood).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_residual, empirical, 'Proportion of student debt serving access coordination versus extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(student_debt_crisis, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_tr_t0, student_debt_crisis, theater_ratio, 0, 0.42).
narrative_ontology:measurement(debt_tr_t10, student_debt_crisis, theater_ratio, 10, 0.48).
narrative_ontology:measurement(debt_tr_t20, student_debt_crisis, theater_ratio, 20, 0.54).
narrative_ontology:measurement(debt_tr_t30, student_debt_crisis, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(debt_be_t0, student_debt_crisis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(debt_be_t10, student_debt_crisis, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(debt_be_t20, student_debt_crisis, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(debt_be_t30, student_debt_crisis, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(student_debt_crisis, resource_allocation).
narrative_ontology:boltzmann_floor_override(student_debt_crisis, 0.18).
narrative_ontology:affects_constraint(student_debt_crisis, wage_stagnation_labor_market).
narrative_ontology:affects_constraint(student_debt_crisis, university_funding_model).
narrative_ontology:affects_constraint(student_debt_crisis, wealth_inequality_intergenerational).

% DUAL FORMULATION NOTE:
% Student debt is structurally linked to three upstream constraints. Wage stagnation has made debt service more burdensome relative to income (affects this constraint's extractiveness). University funding model shifted costs to students as public disinvestment accelerated (primary driver of debt growth). Wealth inequality is a downstream consequence of debt burden suppressing wealth accumulation for borrower cohorts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(student_debt_crisis, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
