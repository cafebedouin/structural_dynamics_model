% ============================================================================
% CONSTRAINT STORY: student_loan_default_cliff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: student_loan_default_cliff
 *   human_readable: Student Loan Default Cliff
 *   domain: economic/debt_policy
 *
 * SUMMARY:
 *   The student loan default cliff emerges from the structural tension
 *   between forbearance (temporary relief from repayment obligations) and
 *   eventual enforcement (resumption of full debt service). When forbearance
 *   expires, borrowers—particularly those with low incomes, dependent care
 *   responsibilities, or employment in high-cost regions—face immediate
 *   repayment obligations without intermediate adjustment periods. This
 *   creates a cliff rather than a gradient: the day forbearance ends, payment
 *   obligations jump from $0 to the full amortized amount, often exceeding
 *   borrower capacity. The constraint exhibits hybrid coordination and
 *   extraction: the loan servicers and federal education system benefit from
 *   the cliff (it forces repayment and concentrates collection efforts),
 *   while borrowers bear the costs. The constraint is actively enforced
 *   through collection mechanisms (wage garnishment, tax intercept, credit
 *   reporting) and perpetuated through theatrical political narratives
 *   (forbearance extensions framed as compassion, cliff deadlines framed as
 *   fiscal discipline). The core structural question is whether forbearance
 *   is emergency policy requiring reform or a proxy for permanent
 *   income-based repayment that Congress refuses to formalize.
 *
 * KEY AGENTS:
 *   - Low-Income Borrowers: Primary victims (powerless/trapped) — no refinancing options, no wage flexibility to absorb cliff; face full extraction through late fees, interest capitalization, and collection activities
 *   - Middle-Income Borrowers: Secondary victims (moderate/constrained) — can access income-driven plans but face extended repayment timelines and increased total interest paid
 *   - Loan Servicers: Primary beneficiaries (organized/arbitrage) — capture servicing fees, late fees, and collection revenues when cliff triggers; have high agency in collection practices
 *   - Federal Department of Education: Secondary beneficiary (institutional/arbitrage) — controls forbearance duration, repayment rules, and enforcement mechanisms; benefits from cliff as policy lever
 *   - Higher Education Institutions: Tertiary actor (powerful/mobile) — face enrollment pressure from cliff-induced debt aversion but benefit from forbearance-enabled enrollment stability
 *   - Congress: Political performer (institutional/constrained) — constrained by political costs of either forgiving debt or engineering large-scale defaults; uses forbearance cycles for electoral signaling
 *   - Debt Relief Advocacy: Organized alternative (organized/mobile) — building policy infrastructure for structural reform (broad forgiveness, income-share agreements); offers genuine exit pathway with political sunset
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the cliff as inevitable feature of debt systems rather than recognizing it as engineered policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(student_loan_default_cliff, 0.62).
domain_priors:suppression_score(student_loan_default_cliff, 0.68).
domain_priors:theater_ratio(student_loan_default_cliff, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(student_loan_default_cliff, extractiveness, 0.62).
narrative_ontology:constraint_metric(student_loan_default_cliff, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(student_loan_default_cliff, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(student_loan_default_cliff, tangled_rope).
narrative_ontology:human_readable(student_loan_default_cliff, "Student Loan Default Cliff").
narrative_ontology:topic_domain(student_loan_default_cliff, "economic/debt_policy").

domain_priors:requires_active_enforcement(student_loan_default_cliff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(student_loan_default_cliff, loan_servicers).
narrative_ontology:constraint_beneficiary(student_loan_default_cliff, federal_education_department).
narrative_ontology:constraint_victim(student_loan_default_cliff, low_income_borrowers).
narrative_ontology:constraint_victim(student_loan_default_cliff, borrowers_with_dependent_care).
narrative_ontology:constraint_victim(student_loan_default_cliff, borrowers_in_high_cost_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME BORROWER (SNARE) — Trapped between forbearance expiration and income insufficiency. No exit option: cannot refinance (credit too damaged), cannot default without severe penalties, cannot negotiate payment terms. Bears full extraction from servicing fees, accrued interest, and wage garnishment threat. High suppression: no alternatives to repayment system.
constraint_indexing:constraint_classification(student_loan_default_cliff, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME BORROWER (TANGLED ROPE) — Can service debt but only through income-driven repayment plans that extend timeline and increase total interest paid. Constrained exit: forgiveness pathways exist (PSLF, IDR forgiveness) but require sustained employment and documented compliance. Benefits from forbearance window (built financial resilience), but extraction mechanism (capitalized interest, extended repayment) is real. Mixed experience: coordination (access to repayment plans) AND extraction (debt structure tilts toward lenders).
constraint_indexing:constraint_classification(student_loan_default_cliff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL EDUCATION DEPARTMENT (ROPE) — Experiences the forbearance-to-repayment transition as a coordination mechanism: managing the system's restart, redistributing risk through income-driven plans, preventing cascade defaults that would destabilize the program. Arbitrage options available (can adjust rules, extend forbearance, modify repayment structures). Net beneficiary of the constraint: the cliff creates urgency and political leverage for policy action. Low experienced extraction.
constraint_indexing:constraint_classification(student_loan_default_cliff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOAN SERVICERS (ROPE) — Primary beneficiaries. The cliff creates a coordination problem they can solve: restarting payment systems, recapturing dormant accounts, capturing late fees and collection activities. High arbitrage options (can modify servicing practices, adjust collection tactics). Experience the constraint as beneficial coordination that captures value. Low theoretical suppression (borrowers can't organize servicers out of existence), but servicer-perceived suppression is minimal because their interests align with enforcement.
constraint_indexing:constraint_classification(student_loan_default_cliff, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGHER EDUCATION INSTITUTIONS (TANGLED ROPE) — Experience mixed effects. Coordination benefit: forbearance reduces student complaints and enables enrollment stability. Extraction exposure: default cliff threatens enrollment (students defer education due to debt burden) and reduces institutional revenue. Mobile exit options (can reduce tuition, increase aid, shift to online models). Experience both coordination (stable debt system enables enrollment) and extraction (cliff threatens institutional enrollments and prestige metrics tied to student outcomes).
constraint_indexing:constraint_classification(student_loan_default_cliff, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CONGRESSIONAL POLITICS (PITON) — Forbearance extensions and cliff deadlines serve primarily as political theater rather than functional policy. Each extension creates electoral talking points ('I fought for working families'); each cliff deadline creates urgency narratives. The functional goal (managing repayment system) is decoupled from the performative goal (demonstrating compassion vs. fiscal discipline). Theater ratio high: repeated extensions and deferrals signal that the underlying system is not being reformed. Constrained exit: Congress cannot easily exit the forbearance cycle without either forgiving debt or engineering mass defaults, both politically costly.
constraint_indexing:constraint_classification(student_loan_default_cliff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, the default cliff could appear as an immutable feature of any debt system: borrowers who cannot pay will default; lenders must eventually enforce; the cliff is a structural inevitability of repayment mechanics. However, the structural data contradicts this — the cliff is engineered through policy choices (forbearance duration, repayment rules, collection practices), not discovered as a law of nature. This perspective risks naturalizing what is actually a contingent institutional arrangement. Engine classification: false summit.
constraint_indexing:constraint_classification(student_loan_default_cliff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: DEBT RELIEF ADVOCACY (SCAFFOLD) — Organized agents (debt activists, progressive policy advocates) see the cliff as a temporary structural failure with a policy sunset: broad-based forgiveness, free public college, or income-share agreements would replace the debt-cliff system. High agency and clear exit pathway (policy reform). Experience low effective extraction because they have concrete alternatives and escalating political voice. Theater ratio moderate: advocacy performs moral urgency while building alternative policy infrastructure.
constraint_indexing:constraint_classification(student_loan_default_cliff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

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
 *   Extractiveness (0.62): The base rate reflects substantial asymmetry in costs and benefits. Borrowers at the cliff face immediate, non-negotiable extraction: capitalized interest, servicing fees, collection costs, wage garnishment threat, and credit damage. The extraction is not total (income-driven plans exist, some forgiveness pathways available) but it is severe and immediate. Over the interval, extractiveness has increased as forbearance has extended and then begun contracting—the cliff's height has grown as the debt has accrued unpaid interest. Suppression (0.68): Multiple barriers prevent escape: limited refinancing options for low-income or default-history borrowers, income constraints preventing reallocation to repayment, weak organizing capacity among atomized borrower population, collateral enforcement mechanisms (wage garnishment, tax intercept, credit freezing), and policy uncertainty (Congress repeatedly delays cliff deadlines, keeping borrowers in limbo). Theater ratio (0.55): Moderate, increasing. Congressional forbearance extensions are primarily performative—they signal compassion without reforming the underlying system. Each extension and cliff deadline serve electoral purposes more than functional debt management. The actual repayment system (income-driven plans, forgiveness pathways) performs its coordination function, but the cliff itself is theatrical: it exists as a deadline threat rather than as a functional enforcement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Low-Income Borrower (Snare) and Loan Servicer (Rope) is maximal: one experiences pure extraction with no alternatives, the other experiences coordinated revenue management. The gap between Middle-Income Borrower (Tangled Rope) and Department of Education (Rope) reflects different exit capacities: the DoE can modify the system, the borrower is constrained within it. The gap between Debt Relief Advocacy (Scaffold) and Congressional Theater (Piton) reflects different theories of change: one sees policy reform as imminent and designed, the other sees endless deferral through theatrical extensions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the repayment flow. Beneficiaries (servicers, DoE) with arbitrage options (can modify rules, extend forbearance, adjust enforcement) experience low d → negative χ. Victims (low-income borrowers) with trapped exit experience high d → high χ. Middle-income borrowers with constrained exit experience moderate d → moderate χ. Congress with constrained political exit experiences moderate-high d (structurally positioned to enforce, but politically unable to avoid costs). Higher education with mobile options experiences lower d despite secondary victim status. The Tangled Rope classification emerges because the constraint has genuine coordination function (income-driven plans do solve collective action problem of matching repayment to ability) AND asymmetric extraction (beneficiaries capture more than victims regain). The Piton classification for Congress reflects that political theater has become the primary function, decoupled from debt management.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the Tangled Rope classification (not pure Snare) is correct at the system level, but Snare is correct for the low-income subpopulation specifically. The constraint exhibits both real coordination function (income-driven repayment plans do solve the problem of matching obligation to ability) AND real extraction (beneficiaries systematically capture more than victims regain, through interest capitalization, extended timelines, and collection mechanisms). The mandatrophy resolution distinguishes between: (1) the system's nominal function (coordination of repayment), which is real but unevenly experienced, and (2) the system's extractive layer (fees, interest, enforcement), which is also real and systematically asymmetric. The Snare classification from the low-income perspective is not an alternative classification of the same constraint—it's a different constraint (the default cliff for those trapped by income insufficiency) that is downstream of the Tangled Rope (the overall student loan system as a policy coordination mechanism with extraction layer). The false Mountain (natural law view) is rejected because the cliff is engineered through policy choices about forbearance duration, repayment rules, and enforcement mechanisms. If different policy choices (broader forgiveness, income-share agreements, free public college) were made, the cliff would disappear or reshape entirely. It is not a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forbearance_permanence,
    'Is forbearance a temporary emergency measure or a permanent feature of the student loan system?',
    'Congressional voting patterns on extensions; policy analysis of whether forbearance is being designed for permanence or phased expiration',
    'If temporary: cliff is genuine default crisis requiring policy response. If permanent: cliff disappears as a constraint (forbearance becomes the system). If indefinite stalling: classification shifts from Snare to Piton (theater-driven).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forbearance_permanence, preference, 'Whether forbearance is temporary or permanent policy').

omega_variable(
    income_driven_repayment_sufficiency,
    'Do income-driven repayment plans actually allow borrowers to service debt without hardship, or do they primarily defer insolvency?',
    'Longitudinal tracking of borrower outcomes under IDR: debt-to-income ratios at forgiveness, default rates, financial stress indicators, comparison to borrowers without IDR access',
    'If sufficient: cliff is negotiable through plan access (Tangled Rope for middle-income). If inadequate: cliff is irreducible extraction even with IDR (Snare). If plan enrollment is low: suppression mechanism (lack of information/access) is higher than assumed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_driven_repayment_sufficiency, empirical, 'Whether income-driven repayment plans prevent hardship').

omega_variable(
    default_cascade_mechanism,
    'Does individual default trigger systemic cascades (institution defaults, servicer insolvency, program collapse) or do defaults remain isolated?',
    'Historical analysis of previous forbearance expiration cycles; servicer financial stress tests; modeling of large-scale default scenarios',
    'If cascade risk is real: federal government has high enforcement pressure (suppression increases). If isolated: suppression is lower and borrower alternatives expand. If cascade is catastrophic: Snare classification confirmed across all non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_cascade_mechanism, empirical, 'Whether defaults cascade systemically').

omega_variable(
    political_will_for_reform,
    'Will Congress enact structural reform (broad forgiveness, policy reset, new system) before the next forbearance expiration?',
    'Legislative activity tracking, polling on reform support, political feasibility assessment',
    'If reform likely: Scaffold perspective strengthens, sunset clause is real. If reform unlikely: cycles repeat, theater-ratio increases (Piton classification strengthens). If reform is 50-year horizon: current cliff is de facto permanent extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_will_for_reform, preference, 'Political likelihood of structural reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(student_loan_default_cliff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sldc_tr_t0, student_loan_default_cliff, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sldc_tr_t5, student_loan_default_cliff, theater_ratio, 5, 0.45).
narrative_ontology:measurement(sldc_tr_t10, student_loan_default_cliff, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(sldc_be_t0, student_loan_default_cliff, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sldc_be_t5, student_loan_default_cliff, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sldc_be_t10, student_loan_default_cliff, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(student_loan_default_cliff, resource_allocation).
narrative_ontology:affects_constraint(student_loan_default_cliff, higher_education_enrollment_access).
narrative_ontology:affects_constraint(student_loan_default_cliff, wealth_inequality_accumulation).
narrative_ontology:affects_constraint(student_loan_default_cliff, intergenerational_debt_transmission).

% DUAL FORMULATION NOTE:
% The student loan default cliff can be decomposed into two distinct constraints: (1) the overall student loan system as a resource allocation mechanism (moderate ε, genuine coordination function, Tangled Rope), and (2) the forbearance-to-repayment cliff as a temporal enforcement shock (higher ε, pure extraction for trapped borrowers, Snare). This story treats the cliff as a temporal feature of the larger system. Upstream constraints (higher education financing structure, wage stagnation) explain why the cliff is catastrophic; downstream constraints (debt collection, wage garnishment mechanisms) are the enforcement layer. Network links show how cliff-triggered defaults propagate to institutional solvency, college enrollment decisions, and intergenerational wealth transmission.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(student_loan_default_cliff, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
