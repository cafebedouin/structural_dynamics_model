% ============================================================================
% CONSTRAINT STORY: isa_education_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_isa_education_scaffold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: isa_education_scaffold
 *   human_readable: Income Share Agreement (ISA) Funding for Education
 *   domain: economic/educational
 *
 * SUMMARY:
 *   Income Share Agreements (ISAs) present a financing mechanism for
 *   education access that combines genuine coordination benefits (removing
 *   upfront cost barriers) with significant extraction dynamics (income
 *   contingency creates long-term earnings extraction). The mechanism
 *   appeared in the mid-2010s as a response to declining public education
 *   investment and unsustainable student debt burdens. ISAs fund education
 *   directly while deferring payment collection to the post-graduation
 *   period, creating a structural tension: they solve the access problem
 *   (coordination) while creating new lock-in problems (long-term earnings
 *   extraction). The constraint exhibits distinct classifications from six
 *   perspectives, revealing how ISA functions as a temporary solution
 *   (scaffold) from one angle and as a permanent extraction mechanism (snare)
 *   from another. The theater ratio (0.52) reflects that while ISA claims to
 *   solve the education financing crisis through market efficiency, much of
 *   its appeal is performative: it appears to eliminate the 'debt burden'
 *   while actually converting it into an invisible earnings tax. The sunset
 *   clause depends on whether public education investment or alternative
 *   financing mechanisms mature; if they do, ISA becomes unnecessary and
 *   contracts wind down. If they do not, ISA risks degrading into
 *   institutional inertia (piton) or permanent extraction (snare).
 *
 * KEY AGENTS:
 *   - Low-Income Students/Graduates: Primary victim (powerless/trapped) — ISA is often the only financing option; income-share extraction persists even if earnings remain low
 *   - ISA Funding Providers: Primary beneficiary (institutional/arbitrage) — recover capital plus margin through income contingency; can exit by cessation or securitization
 *   - Moderate-Income Graduates: Secondary victim (moderate/constrained) — benefit from access but bear long-term income extraction; have some exit capacity through career changes or relocation
 *   - Education Access Coalition: Organized stakeholders (organized/constrained) — advocate for ISA as temporary solution; expect sunset via public investment increase
 *   - Educational Credentialing System: Institutional (institutional/arbitrage) — ISA patches the symptom (access barrier) while perpetuating the underlying constraint (credential inflation)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing ISA as inevitable market mechanism rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(isa_education_scaffold, 0.38).
domain_priors:suppression_score(isa_education_scaffold, 0.45).
domain_priors:theater_ratio(isa_education_scaffold, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(isa_education_scaffold, extractiveness, 0.38).
narrative_ontology:constraint_metric(isa_education_scaffold, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(isa_education_scaffold, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(isa_education_scaffold, scaffold).
narrative_ontology:human_readable(isa_education_scaffold, "Income Share Agreement (ISA) Funding for Education").
narrative_ontology:topic_domain(isa_education_scaffold, "economic/educational").

domain_priors:requires_active_enforcement(isa_education_scaffold).
narrative_ontology:has_sunset_clause(isa_education_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(isa_education_scaffold, isa_funding_providers).
narrative_ontology:constraint_beneficiary(isa_education_scaffold, high_earning_graduates).
narrative_ontology:constraint_victim(isa_education_scaffold, low_income_students).
narrative_ontology:constraint_victim(isa_education_scaffold, low_earning_graduates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME GRADUATE (SNARE) — A student from a low-income background with no alternative financing mechanism is trapped in the ISA agreement. If earnings remain below contract thresholds or plateauing at lower incomes, the income-share obligation persists indefinitely within the contract term (typically 10-25 years). The graduate cannot renegotiate and has no exit: education is prerequisite, ISA is the only financing option available. Maximum suppression and extraction experienced by this agent.
constraint_indexing:constraint_classification(isa_education_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE-INCOME GRADUATE (TANGLED ROPE) — A graduate earning middle-income to slightly above-average income experiences ISA as a hybrid: they benefit from access to education without upfront cost (coordination function), but the income share is still a significant extraction on earnings (5-8% of gross income typically). They have some exit capacity (could potentially seek other education financing in a counterfactual, could relocate to lower cost-of-living areas, could pursue career changes) but constrained by contract lock-in and informational asymmetries about alternative pathways.
constraint_indexing:constraint_classification(isa_education_scaffold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ISA FUNDING PROVIDER (ROPE) — From the funder's perspective, ISA is pure coordination: the provider offers capital upfront, students get education access, and the provider recovers capital plus margin through income contingency. The funder experiences the mechanism as enabling coordination between capital markets (willing to lend) and students (willing to repay from future earnings). Exit is guaranteed arbitrage — the funder can exit by ceasing new agreements or securitizing existing contracts. This perspective sees the constraint as functional coordination, not extraction.
constraint_indexing:constraint_classification(isa_education_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EDUCATION ACCESS COALITION (SCAFFOLD) — Organized stakeholders (education nonprofits, consumer advocates, policymakers) see ISA as a temporary solution to the education financing crisis. The mechanism has a genuine sunset: as public investment in education increases, as student loan interest rates are capped or eliminated, as federal income-contingent repayment programs mature, ISA becomes less necessary. The coalition views the extraction component as tolerable during the transition period (sunset window: 10-15 years estimated) because the coordination benefit (access without upfront barriers) addresses an acute crisis. Suppression will decline as alternatives proliferate.
constraint_indexing:constraint_classification(isa_education_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EDUCATIONAL CREDENTIALING SYSTEM (PITON) — At the civilizational level, the education-as-prerequisite-for-income structure persists through institutional inertia. ISA is a patch: it maintains the requirement that access to earning potential requires educational credentials while the underlying system (why education signals productivity, why credentials are scarce, why alternative pathways to income are blocked) remains unexamined. The theater ratio is moderate (0.52) because ISA partially hides the underlying credential inflation by making the upfront cost invisible — the extraction is deferred into future earnings. The system appears to be solving the access problem while the actual constraint (credential-lock-in) persists through ISA's mechanism.
constraint_indexing:constraint_classification(isa_education_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of education financing mechanism is inherent to any society where education is scarce and income-generation is individually variable. ISA could be naturalized as an inevitable convergence solution — markets allocating capital across risk profiles must eventually discover income-contingency mechanisms. However, this perspective risks confusing the inevitability of education finance mechanisms with the inevitability of THIS particular extraction architecture. The engine's false summit detector should flag this as naturalization of institutional choice.
constraint_indexing:constraint_classification(isa_education_scaffold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(isa_education_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(isa_education_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(isa_education_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(isa_education_scaffold, TR),
    TR >= 0.70.

:- end_tests(isa_education_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The base extraction reflects the income-contingent mechanism's core dynamic: funders recover capital plus a margin (typically 5-8% of post-tax income, or cap after 10-25 years). This is less aggressive than predatory lending (0.70+) but significantly higher than pure coordination (0.05). The extraction is not symmetric — low earners pay more as percentage of income, high earners pay less. The moderate value reflects that (a) funders do take real risk on outcome uncertainty, (b) extraction is conditional on actual income (not upfront burden), and (c) the cap provides some protection. Suppression (0.45): Moderate. Barriers to exit include: contract lock-in (10-25 years), information asymmetries about alternative financing and career outcomes, credit-market barriers to traditional loans for low-income students, and the prerequisite-ness of education for earning potential. However, suppression is not total — some agents can pursue alternative paths (community college, trade school, public universities with lower ISA burden), and ISA terms are transparent about obligations. Theater ratio (0.52): Moderate. ISA marketing emphasizes access-without-upfront-cost, creating performative appearance of 'market efficiency solving the education crisis.' The reality is more complex: the mechanism defers extraction into future earnings, making the burden invisible until repayment begins. Theater has increased over the interval as ISA providers have expanded marketing and as the public debate has shifted away from questions about fairness toward celebration of 'risk-sharing.'
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the ISA funder (Rope) and the low-income graduate (Snare) is absolute. From the funder's perspective, ISA is functional coordination with risk-appropriate returns. From the graduate's perspective, ISA is inescapable extraction during the repayment period. This gap is not an observational artifact — it is structural: the funder has multiple exit options (cease new agreements, securitize existing contracts, invest in alternatives), while the graduate has zero exit options (the financing is already received, the contract is binding). The moderate-income graduate occupies the middle: they experience coordination benefit (access) plus extraction cost (income share), producing the tangled_rope classification. The education access coalition expects this gap to shrink over time as alternatives proliferate, validating the scaffold classification (temporary extraction during transition).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality is derived from funding providers' structural position: they are institutional actors (power=institutional) with arbitrage exit (can exit by cessation or securitization). This produces low d (around 0.15-0.20), corresponding to beneficiary status. Victim directionality is derived from low-income graduates' structural position: they are powerless actors (power=powerless) with trapped exit (ISA is their only financing option). This produces high d (around 0.85-0.95), corresponding to victim status. Moderate-income graduates have constrained exit (could pursue other paths but with significant friction), producing intermediate d (around 0.55-0.65). The organized coalition has constrained exit with agency (they are working to create alternatives), producing d around 0.45-0.55. Directionality overrides are not necessary — the structural derivation from beneficiary/victim declarations and exit options produces accurate d values. The ISA funding provider experiences negative or minimal chi because the constraint extracts toward them (beneficiary with arbitrage exit). The low-income graduate experiences maximal chi because the constraint extracts from them (victim with trapped exit).
 *
 * MANDATROPHY ANALYSIS:
 *   ISA resolves the mandatrophy by distinguishing between coordination function (access-without-upfront-cost) and extraction mechanism (long-term income contingency). The classification is not 'is ISA coordination or extraction?' but 'from whose perspective?' The funder sees pure coordination (Rope). The low-income graduate sees pure extraction (Snare). The moderate-income graduate sees both (Tangled Rope). The organized coalition expects the extraction to sunset as alternatives mature (Scaffold). The educational credentialing system sees ISA as perpetuating the underlying constraint while appearing to solve it (Piton). The analytical observer risks naturalizing the arrangement as inevitable market evolution (Mountain, false summit). The mandatrophy is resolved by recognizing that ISA IS a genuine coordination mechanism (enabling access that would not otherwise exist) AND a genuine extraction mechanism (deferred income tax on future earnings). Both are structurally real. The tension between them is not an error in the framework — it is the core structural feature of ISA design. The sunset clause hypothesis (that extraction becomes unnecessary once alternatives proliferate) is empirically testable and depends on whether public investment, income-contingent loan programs, and alternative credentials mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_distribution_tail_extraction,
    'For graduates earning in the bottom income quartile, does ISA extract a larger percentage of lifetime earnings than traditional fixed-rate student debt?',
    'Longitudinal income tracking and debt burden analysis comparing ISA cohorts to federal student loan cohorts, stratified by initial earnings percentile',
    'If ISA extracts more from low earners: victim classification confirmed, snare perspective justified. If extraction is comparable or lower: ISA is functional coordination, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_distribution_tail_extraction, empirical, 'Whether ISA extracts more from low-earning graduates than traditional loans').

omega_variable(
    sunset_clause_political_viability,
    'Is the sunset clause (declining ISA prevalence as alternatives proliferate) structurally inevitable, or does ISA become locked-in by political economy (funders defend the mechanism, graduates resist higher public taxation)?',
    'Historical analysis of comparable temporary financing mechanisms (e.g., HEAL Act loans, GSL program evolution); examination of whether private ISA funders lobby against public education investment',
    'If sunset is structural: scaffold classification holds. If lock-in is probable: ISA degrades toward piton or tangled_rope, with permanent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_political_viability, empirical, 'Whether ISA sunset clause is structurally or politically viable').

omega_variable(
    information_asymmetry_career_outcomes,
    'Do students selecting into ISA have accurate information about likely career earnings by field, or are they systematically misled about income prospects?',
    'Analysis of student expectations pre-ISA-enrollment vs actual earnings post-graduation; examination of how field-specific income variation is disclosed in ISA marketing',
    'If information is accurate: suppression is moderate (agents make informed choice). If systematically misleading: suppression increases toward snare levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_career_outcomes, empirical, 'Whether students have accurate information about career earnings when selecting ISA').

omega_variable(
    alternative_funding_availability,
    'For what percentage of potential ISA students does the mechanism represent the ONLY viable financing option vs one option among several?',
    'Survey of ISA enrollees about alternative financing considered; analysis of credit-market access by student demographic; simulation of counterfactual financing without ISA',
    'If ISA is the only option for >60% of users: trapped classification confirmed, snare dominates. If alternatives exist but ISA is optimal: scaffold and tangled_rope classifications dominate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_funding_availability, empirical, 'Whether ISA is the only viable option or one choice among alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(isa_education_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isa_tr_t0, isa_education_scaffold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(isa_tr_t5, isa_education_scaffold, theater_ratio, 5, 0.48).
narrative_ontology:measurement(isa_tr_t10, isa_education_scaffold, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(isa_be_t0, isa_education_scaffold, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(isa_be_t5, isa_education_scaffold, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(isa_be_t10, isa_education_scaffold, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(isa_education_scaffold, resource_allocation).
narrative_ontology:boltzmann_floor_override(isa_education_scaffold, 0.25).
narrative_ontology:affects_constraint(isa_education_scaffold, student_debt_crisis).
narrative_ontology:affects_constraint(isa_education_scaffold, education_credential_inflation).
narrative_ontology:affects_constraint(isa_education_scaffold, income_inequality_accumulation).

% DUAL FORMULATION NOTE:
% ISA is downstream of the student debt crisis (which created demand for alternative financing) and upstream of income inequality dynamics (ISA's income-contingency mechanism amplifies earnings-based stratification). The constraint family includes three distinct stories: (1) student_debt_crisis (ε≈0.65, Snare) — the crisis ISA responds to; (2) isa_education_scaffold (ε≈0.38, Scaffold) — the financing mechanism itself; (3) income_inequality_accumulation (ε≈0.52, Tangled Rope) — the downstream effect of ISA-driven earnings extraction. Each has different ε because they represent different structural claims. The student debt crisis has higher ε because it locks students into predatory terms with no coordination benefit. ISA has moderate ε because it provides access (coordination) while extracting future income (extraction). Income inequality has moderate ε because ISA amplifies existing inequality rather than creating it de novo.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
