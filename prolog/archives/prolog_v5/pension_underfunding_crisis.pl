% ============================================================================
% CONSTRAINT STORY: pension_underfunding_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pension_underfunding_crisis, []).

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
 *   constraint_id: pension_underfunding_crisis
 *   human_readable: Pension Underfunding Crisis
 *   domain: economic/fiscal/social
 *
 * SUMMARY:
 *   The pension underfunding crisis represents a structural tension between
 *   past political commitments (defined-benefit promises funded by current
 *   contributions) and present demographic/economic reality (fewer workers
 *   per retiree, lower asset returns, longer lifespans). This constraint
 *   exhibits multiple classification perspectives reflecting different
 *   stakeholder positions and time horizons. Current retirees and public
 *   sector employers benefit from benefit promises; future retirees bear
 *   extraction cost through delayed retirement or reduced benefits; taxpayers
 *   bear extraction through increased contributions; organized labor occupies
 *   a hybrid position with both institutional coordination benefit and
 *   victimization. The crisis is simultaneously a coordination failure
 *   (pension system intended to secure retirement; underfunding prevents that
 *   function), an extraction mechanism (benefit promises to current
 *   beneficiaries are being financed by reduced future promises to
 *   contributors), and an inertial governance problem (regulatory apparatus
 *   maintains increasingly theatrical compliance rituals rather than
 *   implementing structural reform). Theater ratio (0.68) reflects that
 *   regulatory adjustments, contribution hikes, and actuarial assumption
 *   revisions are increasingly performative — they delay hard choices without
 *   resolving underfunding. Extractiveness (0.58) reflects moderate-high
 *   asymmetric burden: costs are partially shared across cohorts (moderate)
 *   but concentrated on lowest-wage workers and youngest cohorts
 *   (moderate-high targeting).
 *
 * KEY AGENTS:
 *   - Current Retirees: Primary beneficiary (institutional/arbitrage) — receive promised benefits without bearing full contribution cost; benefit from implicit intergenerational transfer
 *   - Future Retirees: Primary victim (powerless/trapped) — must contribute higher rates or accept lower benefits; cannot exit; bear full actuarial underfunding cost
 *   - Public Sector Workers: Secondary victim and moderate beneficiary (moderate/constrained) — receive defined-benefit coordination benefit but face contribution increases and work-life extension
 *   - Public Sector Employers: Secondary beneficiary (institutional/arbitrage) — defer liabilities to future, reducing immediate budget pressure; refinance through contribution adjustments
 *   - Financial Sector: Tertiary beneficiary (institutional/arbitrage) — earn fees managing pension assets and selling liability-hedging products
 *   - Labor Union Coalition: Organized victims (organized/mobile) — represent membership victimized by underfunding but have institutional voice to negotiate burden-sharing
 *   - Fiscal Reformers: Organized agents (organized/constrained) — push structured adjustment (scaffold) but politically constrained by affected constituencies
 *   - Regulatory & Governance Apparatus: Institutional theater maintainers (institutional/arbitrage) — manage performative compliance without resolving crisis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pension_underfunding_crisis, 0.58).
domain_priors:suppression_score(pension_underfunding_crisis, 0.65).
domain_priors:theater_ratio(pension_underfunding_crisis, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pension_underfunding_crisis, extractiveness, 0.58).
narrative_ontology:constraint_metric(pension_underfunding_crisis, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(pension_underfunding_crisis, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pension_underfunding_crisis, tangled_rope).
narrative_ontology:human_readable(pension_underfunding_crisis, "Pension Underfunding Crisis").
narrative_ontology:topic_domain(pension_underfunding_crisis, "economic/fiscal/social").

domain_priors:requires_active_enforcement(pension_underfunding_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pension_underfunding_crisis, current_benefit_receivers).
narrative_ontology:constraint_beneficiary(pension_underfunding_crisis, public_sector_employers).
narrative_ontology:constraint_beneficiary(pension_underfunding_crisis, financial_institutions).
narrative_ontology:constraint_victim(pension_underfunding_crisis, future_retirees).
narrative_ontology:constraint_victim(pension_underfunding_crisis, taxpayers).
narrative_ontology:constraint_victim(pension_underfunding_crisis, public_service_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE RETIREE (SNARE) — Trapped by decades of compressed wage growth, mandatory contributions to underfunded schemes, and vanishing exit options. Cannot leave the pension system; must bear full cost of actuarial underfunding through delayed retirement or benefit cuts. Maximum experienced extraction with no alternative income sources in old age.
constraint_indexing:constraint_classification(pension_underfunding_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SECTOR WORKER (TANGLED ROPE) — Receives genuine coordination benefit (pension defined-benefit guarantees, stable employment, deferred compensation) alongside real extraction (contribution rate increases, benefit caps, work-life extension). Cannot easily exit to private sector (credential lock, seniority loss); constrained by career investment. Moderate extraction with asymmetric cost distribution.
constraint_indexing:constraint_classification(pension_underfunding_crisis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC SECTOR EMPLOYER (ROPE) — Benefits from pension liability deferral (reduces immediate budget pressure), arbitrage access through refinancing and accounting adjustments. Experiences constraint as coordination mechanism: pension promises enable workforce retention and are integral to labor negotiations. Net beneficiary during underfunding window.
constraint_indexing:constraint_classification(pension_underfunding_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNION COALITION (TANGLED ROPE) — Organized agents (union leadership, pension funds, benefit trustees) have partial agency to renegotiate terms and extract concessions via collective bargaining. Can threaten to organize or strike; have institutional voice. Benefits from pension system's coordination function (deferred wage security) but also victimized by underfunding crisis affecting membership. Genuine coordination benefit coexists with extraction burden.
constraint_indexing:constraint_classification(pension_underfunding_crisis, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FINANCIAL SECTOR (ROPE) — Asset managers and insurers benefit from pension fund fees, mortality hedging, liability-driven investment products. Experience constraint as pure coordination benefit — pension system generates steady flows to financial markets. Can arbitrage across funding ratios and actuarial assumptions. Net beneficiary with institutional exit options.
constraint_indexing:constraint_classification(pension_underfunding_crisis, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PENSION GOVERNANCE & REGULATORY APPARATUS (PITON) — Regulatory agencies, pension boards, and oversight bodies maintain complex governance structures and compliance theater with diminishing functional capacity to resolve underfunding. Contribution rate adjustments, assumption revisions, and accounting deferrals are performative responses to a structural crisis. Theater ratio high: regulations appear to address crisis but systematically delay hard choices. Institutional inertia maintains apparatus despite known inadequacy.
constraint_indexing:constraint_classification(pension_underfunding_crisis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: FISCAL REFORM COALITION (SCAFFOLD) — Policy reformers, budget hawks, and actuarial experts pushing parametric adjustments (raising retirement age, means-testing, contribution increases) see underfunding as a temporary coordination failure with structured sunset: phased benefit changes create predictable transition pathways. Genuine coordination benefits coexist with asymmetric burden on lower-wage workers. Constrained by political feasibility but organized enough to propose structured exit from the crisis.
constraint_indexing:constraint_classification(pension_underfunding_crisis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/actuarial perspective, pension underfunding may appear as an immutable feature of pay-as-you-go systems: demographic transition (lower birth rates, longer lifespans) creates structural imbalance between contributors and retirees. This perspective naturalizes what is actually a contingent institutional design (defined-benefit obligations without dedicated funding) as inherent to aging societies. Engine will flag this as a false summit.
constraint_indexing:constraint_classification(pension_underfunding_crisis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pension_underfunding_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pension_underfunding_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pension_underfunding_crisis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pension_underfunding_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pension_underfunding_crisis, TR),
    TR >= 0.70.

:- end_tests(pension_underfunding_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits clear extraction — current beneficiaries receive implicit subsidies from future contributors; lowest-wage workers face disproportionate burden through contribution increases or benefit reductions. However, the value reflects that extraction is partially masked by genuine coordination function (pensions do provide retirement security in principle) and that burden-sharing is not absolute (all cohorts bear some adjustment cost). Theater ratio (0.68): High. Regulatory governance has become increasingly performative: contribution rate adjustments appear to address underfunding but systematically underestimate costs; actuarial assumption revisions (lowering discount rates, raising life expectancy assumptions) are technical adjustments that delay political reform; governance board meetings produce elaborate compliance theater without implementing structural solutions. The trajectory from 0.42 to 0.68 shows theater increasing as crisis has deepened and regulatory solutions have failed. Suppression (0.65): Moderate-high. Significant barriers constrain workers' exit options: pension contributions are mandatory (cannot opt out), job lock (switching to private sector costs seniority and vesting), and political economy (reform proposals face organized opposition from affected constituencies). But suppression is not total — some workers can switch; some policy reform is politically feasible; some alternative retirement mechanisms exist (401k supplementation, though inadequate). Mandatrophy: Not resolved. The constraint exhibits genuine coordination function (pensions provide retirement security) coexisting with real extraction (implicit transfers from future to current beneficiaries). Classification varies by perspective without collapse to one type because the structural positions genuinely differ — beneficiaries and victims experience different mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates strong perspectival divergence reflecting structural position and time horizon. Current retirees and public sector employers see coordination (Rope) — pension system is solving the problem of deferred wage security. Fiscal reformers see a temporary problem with adjustment pathway (Scaffold) — parametric reform can phase in higher contributions and later retirement ages. Pension governance sees degraded ritual (Piton) — regulatory compliance maintains appearance of management without addressing crisis. Labor unions see mixed coordination and extraction (Tangled Rope) — pensions provide genuine security but underfunding forces burden-sharing that falls disproportionately on membership. Future retirees and lowest-wage taxpayers see pure extraction (Snare) — they are trapped by obligation to contribute to underfunded system with no exit. The civilizational analytical perspective risks naturalizing this as a Mountain — pension underfunding as inevitable consequence of demographic aging — but the structural data reveals this as false: underfunding results from deliberate choices about funding mechanisms (pay-as-you-go rather than pre-funding) and benefit guarantees, not from immutable demographic fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation reflects each agent's position in the extraction flow. Current retirees (institutional, arbitrage): benefit from pension promise with no corresponding contribution burden; low d (0.1-0.15) leads to negative or near-zero effective extraction (net benefit). Public sector employers (institutional, arbitrage): defer liability to future, immediate budgetary relief; low d leads to rope classification. Future retirees (powerless, trapped): must pay high contributions with reduced benefits; high d (0.85-0.95) leads to high effective extraction (snare classification). Public sector workers (moderate, constrained): receive coordination benefit (defined-benefit security) but face increasing contributions and later retirement; moderate d (0.50-0.65) leads to tangled rope. Labor unions (organized, mobile): have institutional voice to negotiate but represent victimized workers; moderate-high d (0.55-0.65) with mobility access reduces pure snare classification. Fiscal reformers (organized, constrained): push reform structure but politically constrained; moderate d reflects that reform enables exit from crisis but at cost to current beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The pension underfunding crisis resolves the mandatrophy by showing that all seven indexed perspectives are legitimate. The crisis is not 'which classification is correct?' but 'what does each stakeholder's structural position reveal about the constraint's nature?' Current retirees genuinely experience coordination (Rope) — the system is solving their retirement problem. Future retirees genuinely experience extraction (Snare) — they are trapped funding a system that won't provide equivalent benefits. Fiscal reformers genuinely see a sunset path (Scaffold) — parametric reform can phase in adjustment. Pension governance genuinely maintains theater (Piton) — regulatory apparatus persists despite loss of functional capacity. The analytical mountain perspective is a false summit: demographic aging creates pressure but does not determine outcomes — institutional design choices (funding mechanism, benefit guarantees, contribution rates) determine whether underfunding occurs. The presheaf across the observation site is the answer: each perspective reveals a real structural mechanism operating simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_inevitability,
    'Is pension underfunding an inevitable consequence of demographic aging (fewer workers per retiree) or a contingent outcome of political choices about funding mechanisms?',
    'Comparative analysis: examine systems with full pre-funding (Canada Pension Plan, Norwegian pension fund) vs pay-as-you-go with benefit caps vs hybrid systems; assess whether demographic pressures are equally severe across different institutional designs',
    'If inevitable: mountain classification partially justified; underfunding is structural feature of aging. If contingent: false summit confirmed; underfunding results from deferred contributions and inadequate capitalization — politically changeable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_inevitability, empirical, 'Whether underfunding is demographic necessity or institutional choice').

omega_variable(
    benefit_cut_distribution,
    'Will benefit adjustments and contribution increases be distributed regressively (falling hardest on lowest-wage workers and near-retirees) or progressively (protecting vulnerable groups)?',
    'Analysis of actual reform proposals and policy outcomes; measurement of impact by income quintile, age cohort, and public vs private sector; comparison to stated equity principles',
    'If regressive: scaffold perspective fails (sunset appears structured but creates new extraction); snare classification strengthened (powerless bear disproportionate burden). If progressive: scaffold genuinely redistributes burden; snare classification weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_cut_distribution, empirical, 'Distributive incidence of pension adjustment burden').

omega_variable(
    political_feasibility_of_reform,
    'Can parametric reform (raising retirement age, means-testing, contribution increases) be sustained against political opposition from affected cohorts, or will crisis force sudden cracking?',
    'Historical analysis of pension reform adoption and reversal; polling data on public support for specific reforms; modeling of electoral consequences for parties implementing unpopular changes',
    'If sustained: scaffold perspective accurate (structured phased transition possible). If blocked: piton perspective dominates (regulatory theater unable to implement reform); crisis eventually forces binary outcome (sudden cuts or sudden revenue).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_of_reform, empirical, 'Political sustainability of gradual pension reform').

omega_variable(
    intergenerational_extraction_asymmetry,
    'Are current retirees and near-retirees disproportionately shielded from underfunding costs (extracting from future workers) or are costs genuinely shared across age cohorts?',
    'Actuarial life-cycle analysis: compare lifetime contributions + returns vs lifetime benefits by retirement cohort; measure whether early cohorts receive implicit subsidies from later cohorts',
    'If asymmetric shielding: pure extraction from younger workers confirmed (snare classification for future retirees validated). If costs genuinely shared: tangled rope classification dominates (mixed coordination + extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_extraction_asymmetry, empirical, 'Intergenerational distribution of underfunding burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pension_underfunding_crisis, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pens_tr_t0, pension_underfunding_crisis, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pens_tr_t10, pension_underfunding_crisis, theater_ratio, 10, 0.58).
narrative_ontology:measurement(pens_tr_t20, pension_underfunding_crisis, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(pens_be_t0, pension_underfunding_crisis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pens_be_t10, pension_underfunding_crisis, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(pens_be_t20, pension_underfunding_crisis, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pension_underfunding_crisis, resource_allocation).
narrative_ontology:affects_constraint(pension_underfunding_crisis, public_sector_labor_market_equilibrium).
narrative_ontology:affects_constraint(pension_underfunding_crisis, fiscal_sustainability_government_budgets).
narrative_ontology:affects_constraint(pension_underfunding_crisis, intergenerational_wealth_transfer).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
