% ============================================================================
% CONSTRAINT STORY: sotu_1948_truman_social_security_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1948_truman_social_security_expansion, []).

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
 *   constraint_id: sotu_1948_truman_social_security_expansion
 *   human_readable: SOTU 1948 Truman Social Security Expansion Proposal
 *   domain: social_policy/income_security
 *
 * SUMMARY:
 *   Truman's 1948 proposal expands Social Security to cover millions of
 *   previously uncovered workers—agricultural laborers, domestic workers,
 *   self-employed persons—and increases benefit levels for all participants.
 *   The expansion redistributes income from higher-wage earners and employers
 *   toward low-income workers and their dependents, funded through increased
 *   payroll taxation and general revenue. The constraint demonstrates the
 *   structure of a hybrid coordination-extraction mechanism: the genuine
 *   coordination function (pooling risk across larger population, enabling
 *   income smoothing against unemployment and old age) coexists with
 *   asymmetric extraction (higher-wage workers and employers subsidize
 *   lower-wage workers' participation and higher replacement rates). The
 *   constraint's theater ratio (0.35) reflects the rhetorical framing around
 *   'security' and 'protection' that obscures the underlying redistributive
 *   mechanism. The suppression measure (0.62) captures both the elimination
 *   of alternatives for newly covered workers (who are now mandatorily
 *   enrolled) and the difficulty for employers of exiting payroll tax
 *   obligations. The extractiveness trajectory (0.15→0.38) shows rising
 *   extraction as the scope of the program expands and beneficiary base
 *   deepens, but extractiveness remains moderate because the expansion also
 *   delivers genuine coordination benefits—workers previously without any
 *   protection now have income security.
 *
 * KEY AGENTS:
 *   - Uncovered agricultural workers: Primary beneficiary (powerless/trapped) — gain entry to Social Security system; extraction from their perspective is minimal because they have no exit options and see the constraint as enabling security rather than constraining choice
 *   - Uncovered domestic workers: Primary beneficiary (powerless/trapped) — similar to agricultural workers; gain coverage but are structurally dependent on employment continuity
 *   - Higher-income private sector workers: Primary victim (moderate/constrained) — pay increased payroll taxation; enjoy higher benefits but subsidize lower-wage workers' participation; can consider geographic mobility or sectoral shift but at cost
 *   - Agricultural employers: Secondary victim (powerful/mobile) — lose labor cost advantage from exclusion; forced to internalize Social Security costs; have capital mobility and mechanization options but at capital cost
 *   - Federal government / Treasury: Institutional actor (institutional/constrained) — bears long-term benefit liability; revenue recovery depends on payroll tax compliance and sustained economic growth
 *   - Labor movement and progressive coalition: Organized actors (organized/constrained) — primary advocates for expansion; view it as temporary correction with sunset (once universal coverage achieved)
 *   - Social Security Administration: Institutional actor (institutional/arbitrage) — benefits from expanded mandate and budgetary authority; sees expansion as justification for administrative growth
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing the expansion as economic necessity rather than contingent political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1948_truman_social_security_expansion, 0.38).
domain_priors:suppression_score(sotu_1948_truman_social_security_expansion, 0.62).
domain_priors:theater_ratio(sotu_1948_truman_social_security_expansion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1948_truman_social_security_expansion, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1948_truman_social_security_expansion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1948_truman_social_security_expansion, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1948_truman_social_security_expansion, tangled_rope).
narrative_ontology:human_readable(sotu_1948_truman_social_security_expansion, "SOTU 1948 Truman Social Security Expansion Proposal").
narrative_ontology:topic_domain(sotu_1948_truman_social_security_expansion, "social_policy/income_security").

domain_priors:requires_active_enforcement(sotu_1948_truman_social_security_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1948_truman_social_security_expansion, uncovered_agricultural_workers).
narrative_ontology:constraint_beneficiary(sotu_1948_truman_social_security_expansion, uncovered_domestic_workers).
narrative_ontology:constraint_beneficiary(sotu_1948_truman_social_security_expansion, uncovered_self_employed).
narrative_ontology:constraint_beneficiary(sotu_1948_truman_social_security_expansion, existing_beneficiaries_higher_rates).
narrative_ontology:constraint_beneficiary(sotu_1948_truman_social_security_expansion, low_income_families).
narrative_ontology:constraint_victim(sotu_1948_truman_social_security_expansion, higher_income_earners_increased_taxation).
narrative_ontology:constraint_victim(sotu_1948_truman_social_security_expansion, employers_payroll_tax_increase).
narrative_ontology:constraint_victim(sotu_1948_truman_social_security_expansion, general_taxpayers_revenue_cost).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCOVERED AGRICULTURAL WORKER (ROPE) — Previously excluded from any Social Security protection. The expansion closes a systemic coordination gap: agricultural workers can now participate in income-smoothing mechanism alongside industrial workers. Minimal extraction from this agent's perspective; primarily perceives coordination function. Trapped by economic circumstances (agricultural labor offers no exit), but the constraint enables rather than constrains.
constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HIGHER-INCOME PRIVATE SECTOR WORKER (TANGLED ROPE) — Bears increased payroll taxation but receives higher benefit levels and security. Coordination function is genuine (pooled insurance against unemployment/disability/old age), but asymmetric extraction occurs: this agent subsidizes lower-wage workers' participation and higher replacement rates. Constrained by taxation but not trapped (wage employment alternatives exist, though tax differential may discourage mobility at margins).
constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AGRICULTURAL EMPLOYER LOBBY (SNARE) — Faces direct extraction through expanded payroll tax obligations and loss of labor cost advantage. Previously could avoid Social Security contributions by employing uncovered workers; expansion forces inclusion. Structurally has exit options (capital mobility, shift to mechanization, relocation), but the constraint's enforcement mechanisms (federal mandate, withholding) make exit costly. Experiences maximal extraction relative to their power level — a well-organized actor being forced to internalize externality.
constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT—REVENUE AND LIABILITY PERSPECTIVE (TANGLED ROPE) — Coordination function is substantial: managing national risk pooling across millions of workers. But extraction asymmetry exists: Treasury bears the cost of expanded benefits while revenue recovery depends on sustained payroll taxation compliance. Government is constrained by electoral cycles and budget pressure but not trapped (it can adjust rates, eligibility, or general revenue allocation). The expansion increases long-term liabilities relative to immediate revenue, creating intergenerational extraction.
constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR MOVEMENT AND PROGRESSIVE COALITION (SCAFFOLD) — Organized advocates viewing the expansion as temporary correction of Depression-era institutional gaps. See the constraint as having a generational sunset: once universal coverage and benefit adequacy are achieved, the expansion's redistributive function completes and transitions to pure coordination (steady-state Social Security). Theater ratio reflects political positioning and rhetorical frames around 'crisis' and 'security', but the underlying mechanism is durable. Suppression appears low from this perspective because the coalition supports the expansion and sees exit path (completion of reform agenda).
constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SOCIAL SECURITY ADMINISTRATION—INSTITUTIONAL PERFORMANCE (PITON) — The expansion requires SSA to absorb millions of new claimants, creating administrative theater: eligibility determination, record-keeping, claims processing become performatively complex. The agency's primary function (income security) is genuine, but much of its institutional practice becomes self-justifying bureaucratic maintenance—forms, verification procedures, administrative hierarchies persist because 'that's how Social Security works', not because they optimize for claimant welfare. Theater ratio reflects the performative administrative apparatus.
constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER—ECONOMIC NECESSITY FRAME (MOUNTAIN) — From civilizational distance, income security against unemployment, old age, and loss of wage-earner in complex industrial economies is an irreducible structural necessity. Markets alone cannot handle the magnitude and timing of these risks; pooled insurance is not optional. This perspective risks naturalizing what is actually a contingent policy choice—treating Truman's expansion as discovering a natural economic law rather than constructing a political arrangement. The engine's false summit detector will identify this as naturalization of a contested distributional choice.
constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1948_truman_social_security_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1948_truman_social_security_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1948_truman_social_security_expansion, TR),
    TR >= 0.70.

:- end_tests(sotu_1948_truman_social_security_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The expansion redistributes income from higher-wage earners and employers toward lower-wage workers and previously uncovered populations. This redistribution is genuine—not merely perceived but structural. Higher-wage workers' payroll taxes rise, and the benefit replacement rate is progressive (lower-wage workers get higher replacement percentages). However, extractiveness is not extreme (not 0.70+) because the constraint also delivers coordination benefits to all participants: pooled insurance against unemployment, old-age, and survivor risk. The expansion's primary function is coordination; extraction is the asymmetric distribution of that coordination's cost-benefit. Suppression (0.62): Moderate-high. Newly covered workers face mandatory enrollment with no opt-out (suppression for them is the elimination of alternatives to participation). Employers face mandatory payroll tax obligation with no exit other than capital mobility or sectoral shift (high but not total suppression). Higher-wage workers face taxation they cannot easily avoid without leaving the labor force (high suppression). The suppression reflects the program's mandatory, universal character—it is enforced federal policy, not voluntary coordination. Theater ratio (0.35): Low-moderate. The expansion's justification rhetoric emphasizes 'security', 'protection', and 'social responsibility', but the underlying mechanism is straightforward redistribution of income through mandatory pooled insurance. There is some performative framing (e.g., treating the program as 'insurance' rather than explicitly as redistribution), but the functional mechanism is clear and the administrative apparatus is relatively lean compared to later bureaucratic elaboration. Theater increases slightly over the interval as enrollment and claims processing complexity rise, but remains dominated by functional activity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits substantial perspectival divergence, revealing the distributional tensions embedded in the expansion. The uncovered agricultural worker sees pure coordination (Rope)—the constraint solves their risk exposure problem and enables income smoothing. The higher-income worker sees mixed coordination and extraction (Tangled Rope)—they benefit from coverage but subsidize others. The agricultural employer sees pure extraction (Snare)—they lose a cost advantage and are forced to internalize externality. The federal government sees intergenerational coordination with debt burden (Tangled Rope)—current revenue is adequate but long-term sustainability depends on demographics and wage growth. The labor movement sees temporary correction (Scaffold)—the expansion is a step toward universal coverage with a natural sunset once the goal is achieved. The SSA sees institutional purpose and growth (Piton)—the agency's maintenance and expansion become self-justifying. The analytical observer at civilizational distance risks seeing economic necessity (Mountain)—treating income security against unemployment and old age as a discovered natural law rather than a constructed political choice. The false summit detector identifies this last perspective as naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation (d → f(d) → χ) flows from beneficiary/victim declarations and exit options. Uncovered workers declared as beneficiaries with trapped exit options (d ≈ 0.15) experience low/negative effective extraction f(d) ≈ -0.01 to 0.02—they perceive the constraint as beneficial. Higher-wage workers declared as victims with constrained exit options (d ≈ 0.70) experience moderate-high effective extraction f(d) ≈ 1.00. Employers declared as victims with mobile exit options (d ≈ 0.85) experience high effective extraction f(d) ≈ 1.15, but their high power level and capital mobility options mean they can negotiate exceptions or invest in mechanization to reduce tax exposure. The federal government as institutional actor with constrained exit (d ≈ 0.60) experiences moderate effective extraction f(d) ≈ 0.80, reflecting that government bears benefit liability but can adjust tax rates and revenue sources—constrained but not trapped. The labor movement with organized power and constrained exit (d ≈ 0.40) experiences low-to-moderate effective extraction f(d) ≈ 0.40, reflecting that the coalition supports the expansion and sees it as temporary correction. The piton perspective derives from the institutional maintenance view rather than from high directionality d—the Social Security Administration experiences the constraint as providing institutional purpose and budgetary justification, not as extracting from it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Tangled Rope is the stable equilibrium classification for a successful income-security expansion. The constraint is NOT pure extraction (Snare) because it delivers genuine coordination benefits to all participants—the pooled insurance mechanism is real and valuable. But it is NOT pure coordination (Rope) because it imposes asymmetric costs on higher-wage earners and employers who subsidize lower-wage workers' participation and higher replacement rates. The constraint is also NOT temporary (Scaffold) because Social Security, once established, does not have a designed sunset—the expansion integrates permanently into the institutional structure. The piton perspective (institutional maintenance and theater) is real but secondary—the constraint's primary function is coordination with asymmetric distribution, not pure performance. The mountain perspective (economic necessity) risks naturalizing a political choice. The correct equilibrium is Tangled Rope: genuine coordination function (pooled insurance) with asymmetric extraction (progressive redistribution). This classification is stable because both elements are real and structural, not because observers disagree about the constraint's nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainability_financing_mechanism,
    'Can the expanded program be financed indefinitely through payroll taxation alone, or does it require sustained general revenue subsidy?',
    'Demographic projections combined with benefit formula analysis; comparison of payroll tax revenue to projected benefit payments over 30+ years under varying wage-growth and life-expectancy scenarios',
    'If payroll-tax-sustainable: program is self-financing coordination mechanism (Rope for most actors). If requires general subsidy: intergenerational extraction occurs (Snare for future workers, Tangled Rope for current beneficiaries), and classification shifts toward snare or tangled_rope across longer time horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_financing_mechanism, empirical, 'Whether expanded Social Security can sustain itself through payroll taxation').

omega_variable(
    wage_suppression_incidence,
    'To what extent does payroll tax expansion suppress nominal wage growth for covered workers, shifting extraction burden backward to employees rather than appearing as employer cost?',
    'Wage growth trajectories pre- and post-expansion controlling for productivity, sectoral shifts, and union organizing; econometric incidence analysis of tax burden between employers and workers',
    'If workers bear majority incidence (>60%): extraction from workers is higher than apparent; the constraint appears less redistributive than nominal structure suggests. Classification from moderate/higher-income perspective shifts toward Snare. If employers bear incidence: expansion is more successfully extractive against capital.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_incidence, empirical, 'Wage suppression incidence of payroll tax expansion').

omega_variable(
    behavioral_adaptation_labor_supply,
    'Does expanded unemployment and old-age security reduce labor force participation, and if so, does reduced participation offset redistributive gains through lower tax revenue?',
    'Labor force participation rates by age and sector pre- and post-expansion; correlation analysis between benefit generosity increases and subsequent participation changes, controlling for secular trends',
    'If participation drop >5%: tax base erosion may force rate increases or benefit cuts, transforming expansion into intertemporal extraction (lower participation workers subsidize higher-participation cohorts). Classification remains Tangled Rope but with asymmetry reversed over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_adaptation_labor_supply, empirical, 'Labor supply behavioral response to expanded benefits').

omega_variable(
    coverage_enforcement_gap,
    'What proportion of nominally covered workers actually register with Social Security and pay taxes, versus the proportion of uncovered who work informally despite legal requirement?',
    'Tax compliance audit data; comparison of reported beneficiaries to eligible population; analysis of informal economy labor in nominally covered sectors',
    'If enforcement gap >20%: program''s redistribution reaches only fraction of intended beneficiaries, and actual extraction from compliant workers is higher than designed. Snare classification strengthens for tax-compliant workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_enforcement_gap, empirical, 'Gap between legal coverage and actual enrollment/compliance').

omega_variable(
    political_sustainability_retrenchment,
    'Will organized beneficiary constituencies (labor, elderly, low-income) successfully defend the expanded program against future retrenchment pressures, or will the expansion be treated as temporary and rolled back?',
    'Political economy analysis of benefit defense vs. fiscal pressure; precedent analysis from other New Deal programs; constituency strength measurement (membership numbers, political mobilization capacity)',
    'If defensible (high constituency strength): expansion becomes durable Tangled Rope / steady Rope. If vulnerable (low constituency strength relative to fiscal pressure): expansion is Scaffold with genuine sunset, and theater ratio measures political salience rather than functional performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_sustainability_retrenchment, preference, 'Political sustainability of expanded Social Security against retrenchment pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1948_truman_social_security_expansion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sstu_tr_t0, sotu_1948_truman_social_security_expansion, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sstu_tr_t5, sotu_1948_truman_social_security_expansion, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sstu_tr_t10, sotu_1948_truman_social_security_expansion, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(sstu_be_t0, sotu_1948_truman_social_security_expansion, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sstu_be_t5, sotu_1948_truman_social_security_expansion, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(sstu_be_t10, sotu_1948_truman_social_security_expansion, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1948_truman_social_security_expansion, resource_allocation).
narrative_ontology:affects_constraint(sotu_1948_truman_social_security_expansion, postwar_labor_bargaining_structure).
narrative_ontology:affects_constraint(sotu_1948_truman_social_security_expansion, federal_fiscal_capacity_constraints).
narrative_ontology:affects_constraint(sotu_1948_truman_social_security_expansion, intergenerational_social_contract).

% DUAL FORMULATION NOTE:
% This expansion is downstream of Depression-era Social Security establishment (ε=0.25, Mountain-to-Rope across perspectives) and upstream of post-1970s sustainability debates (ε=0.45-0.55, Tangled Rope-to-Snare as demographics shift). The expansion's ε=0.38 represents the equilibrium point where genuine coordination function is maximized and redistribution is politically sustainable—neither pure Rope (no redistribution) nor pure Snare (redistribution becomes visible and vulnerable to retrenchment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
