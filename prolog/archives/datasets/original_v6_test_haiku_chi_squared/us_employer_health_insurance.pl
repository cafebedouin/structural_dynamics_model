% ============================================================================
% CONSTRAINT STORY: us_employer_health_insurance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_employer_health_insurance, []).

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
 *   constraint_id: us_employer_health_insurance
 *   human_readable: US Employer-Sponsored Insurance (ESI) System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The US employer-sponsored insurance (ESI) system is a path-dependent
 *   artifact of World War II wage controls. During the war, the Treasury
 *   ruled that employer contributions to health insurance were not subject to
 *   wage caps or income tax, creating an incentive for employers to offer
 *   coverage instead of raising wages. This tax treatment persisted after the
 *   war and hardened into institutional reality. ESI now covers ~160 million
 *   Americans and is the dominant source of private insurance. The constraint
 *   operates through employment lock: workers fear losing coverage when
 *   changing jobs, suppressing labor mobility and enabling employers to
 *   extract rents in the form of compressed wages and reduced job
 *   flexibility. The system is extractive for uninsured workers (powerless
 *   victims), constrained for employer-dependent workers (moderate victims),
 *   beneficial for large employers and insurers (institutional
 *   beneficiaries), and performative for the tax code and regulatory
 *   framework (piton). The false natural law view treats this as inevitable,
 *   but the structural metrics reveal it as a contingent institutional
 *   arrangement that has become increasingly dysfunctional as healthcare
 *   costs and employment precarity have grown.
 *
 * KEY AGENTS:
 *   - Large employers (institutional/arbitrage): Primary beneficiary — captures tax subsidies, negotiating leverage, labor compliance beyond productivity
 *   - Uninsured individuals (powerless/trapped): Primary victim — excluded from affordable coverage; cannot exit healthcare necessity
 *   - Employer-dependent workers (moderate/constrained): Secondary victims — locked into jobs due to coverage dependency; face wage suppression
 *   - Insurance companies (institutional/arbitrage): Secondary beneficiary — direct billing to employers creates sticky relationships; pooling at scale
 *   - Self-employed/small business (powerful/mobile): Mixed experience — can exit but face adverse selection; constrained by scale disadvantage
 *   - Uninsurable population (powerless/trapped): Maximal extraction victim — historically excluded from private coverage; trapped by stigmatized alternatives
 *   - Tax code and regulatory framework (institutional/arbitrage): Institutional substrate — 26 U.S.C. § 162(a), § 106 create the formal structure; ERISA safe harbors entrench insurer power
 *   - Public health and labor regulators (organized/constrained): Institutional actors with conflicting mandates — tasked with access but dependent on ESI as primary mechanism
 *   - Analytical observer (analytical/analytical): Risks naturalizing contingent arrangement as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_employer_health_insurance, 0.52).
domain_priors:suppression_score(us_employer_health_insurance, 0.68).
domain_priors:theater_ratio(us_employer_health_insurance, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_employer_health_insurance, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_employer_health_insurance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_employer_health_insurance, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_employer_health_insurance, snare).
narrative_ontology:human_readable(us_employer_health_insurance, "US Employer-Sponsored Insurance (ESI) System").
narrative_ontology:topic_domain(us_employer_health_insurance, "economic/social").

domain_priors:requires_active_enforcement(us_employer_health_insurance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, large_employers).
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, insurance_companies).
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, high_income_workers).
narrative_ontology:constraint_victim(us_employer_health_insurance, non_employed_individuals).
narrative_ontology:constraint_victim(us_employer_health_insurance, low_income_workers).
narrative_ontology:constraint_victim(us_employer_health_insurance, gig_economy_workers).
narrative_ontology:constraint_victim(us_employer_health_insurance, unemployed_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED/PRECARIOUSLY EMPLOYED (SNARE) — Lacks access to employer coverage due to unemployment, gig work, or part-time status. Trapped by healthcare necessity and cost; cannot exit employment without losing coverage. Cannot opt into affordable alternative systems. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(us_employer_health_insurance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYER-DEPENDENT WORKER (SNARE) — Covered by employer plan but constrained by switching costs. Changing jobs risks coverage gap, pre-existing condition exclusions (historically), or plan discontinuity. Extraction mechanism: employer captures labor compliance beyond productivity value; worker pays implicit cost through wage suppression. d≈0.80, f(d)≈1.22, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(us_employer_health_insurance, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE EMPLOYER (ROPE) — Benefits from tax subsidies (employer contributions deductible, employee benefits excluded from taxable income), negotiating leverage with insurers, and labor cost bundling. Experiences constraint as coordination mechanism: offers coverage to attract and retain talent, solve adverse selection by pooling risk. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(us_employer_health_insurance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSURANCE INDUSTRY (ROPE) — Captured both demand (employer mandate for coverage to attract workers) and supply (regulatory entrenchment through tax code; direct billing to employers creates sticky relationships). Benefits from pooling, fee extraction, and administrative complexity. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(us_employer_health_insurance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SELF-EMPLOYED/SMALL BUSINESS (TANGLED ROPE) — Can negotiate coverage and costs (mobile exit), but faces adverse selection risk and price disadvantage vs. large pools (constrained by scale). Experiences both coordination (risk pooling) and extraction (premium loading for small group). requires_active_enforcement: true because small group insurance requires regulatory coordination (state insurance regulators, ERISA exemptions) and ongoing enforcement of coverage standards. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(us_employer_health_insurance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: UNINSURABLE POPULATION (SNARE) — Historically excluded from private coverage due to pre-existing conditions (now legally prohibited, but enforcement remains patchy). Trapped by systemic exclusion; no legitimate exit path within the ESI system. Medicaid (alternative) is state-dependent, income-conditional, and carries stigma. Effective extraction is maximal. d≈0.98, f(d)≈1.50, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(us_employer_health_insurance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: TAX CODE/REGULATORY FRAMEWORK (PITON) — The constraint's institutional substrate. ESI persists because of path-dependent tax treatment (26 U.S.C. § 162(a), § 106: employer contributions deductible, employee benefits excluded from income) and regulatory carve-outs (ERISA safe harbors). The tax structure is largely performative — it creates the illusion of 'employer-provided' coverage while obscuring the economic reality (employer payments are deferred compensation). theater_ratio=0.61 reflects high administrative overhead (claims processing, verification, compliance) with declining functional coupling to actual health outcomes. The framework persists through institutional inertia; reforming it requires Congressional action that accumulated interests block.
constraint_indexing:constraint_classification(us_employer_health_insurance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: PUBLIC HEALTH/LABOR REGULATORS (TANGLED ROPE) — Tasked with ensuring health coverage access and preventing employment discrimination, but structurally dependent on ESI as the dominant coverage mechanism. Sees constraint as requiring active enforcement (ACA mandate enforcement, HIPAA portability rules, pre-existing condition protections). Constrained by Congressional reluctance to dismantle tax subsidies (worth ~$300B/year). Experiences both coordination (risk pooling for large employers) and extraction (regulatory complexity creates barriers for small/medium firms). d≈0.60, f(d)≈0.82, σ=1.0 → χ≈0.50.
constraint_indexing:constraint_classification(us_employer_health_insurance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing ESI as inevitable: 'Healthcare must be tied to employment because employers have the bargaining power and risk pools.' This perspective treats the constraint as immutable — an emergent property of labor markets and insurance economics. However, the structural data (ε=0.52, suppression=0.68, theater=0.61) contradicts true natural law signature (≤0.25 extractiveness, ≤0.05 suppression). The engine identifies this as a false summit: ESI is a contingent institutional arrangement (WWII tax code accident + regulatory path-dependence), not a law of nature. The false natural law framing obscures policy choice.
constraint_indexing:constraint_classification(us_employer_health_insurance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_employer_health_insurance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_employer_health_insurance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_employer_health_insurance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_employer_health_insurance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_employer_health_insurance, TR),
    TR >= 0.70.

:- end_tests(us_employer_health_insurance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts measurably from three victim groups: uninsured workers (denied coverage), employer-dependent workers (wage suppression ~2-3% of compensation), and small businesses (adverse selection loading). However, extractiveness is not maximal because large employers experience net benefits (arbitrage), and the coordination function (risk pooling) is real, even if increasingly dysfunctional. The trajectory shows growth from 0.28 (1945, when ESI was marginal) to 0.52 (2015, when ESI became extractive as uninsured rates rose and healthcare costs accumulated). Suppression (0.68): High. Multiple barriers prevent exit: healthcare necessity (cannot opt out of needing care), switching costs (coverage gaps, pre-existing condition loading until 2014), regulatory entrenchment (tax code makes alternative systems uncompetitive), and labor market structure (most job offers include coverage, making refusal difficult). The Affordable Care Act (2010) reduced suppression by enabling individual market access, but state-level variation and persistent premium gaps maintain suppression. Theater ratio (0.61): Moderate-high. ESI exhibits substantial administrative overhead (~8-12% of premiums) that doesn't correlate with health outcomes. Employer-negotiated plan design (metal tiers, deductibles, network restrictions) creates the appearance of meaningful choice while obscuring the reality that employers design plans around cost management, not patient benefit. The rise in high-deductible plans (2005-2015) reflects employers shifting costs to workers while maintaining the coverage theater. Beneficiaries/Victims: Large employers and insurers benefit from labor lock-in and regulatory entrenchment. Victims are uninsured individuals, employer-dependent workers facing wage suppression, gig economy workers excluded from coverage, and small businesses paying adverse-selection premiums.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival disagreement. Large employers see pure Rope (coordination mechanism for risk pooling and labor competition). Insurers see Rope (pooling benefit, regulatory stability). Uninsured workers see Snare (locked out by cost and structure). Employer-dependent workers see Snare (locked in by switching costs and healthcare necessity). Small businesses see Tangled Rope (both risk pooling and adverse-selection extraction). Regulators see Tangled Rope (mandated to ensure access while dependent on ESI as mechanism). The analytical observer risks seeing Mountain (healthcare must follow employment because of insurance economics), but this naturalizes what is actually a regulatory choice. The perspectival gap reflects that the same structural phenomenon — tying healthcare to employment — creates coordination benefits for some agents and extraction for others, without a coherent mechanism to satisfy all perspectives simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Large employers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through tax subsidies and labor compliance. Insurance industry: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary through regulatory entrenchment and pooling. Uninsured individuals: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction; no legitimate exit within ESI framework. Employer-dependent workers: Victim + constrained → d≈0.80, f(d)≈1.22. High extraction through wage suppression and employment lock; some mobility but at high cost. Self-employed/small business: Mixed + mobile → d≈0.55, f(d)≈0.75. Can exit (mobile) but face adverse selection; balanced between coordination benefit and extraction cost. Uninsurable population: Victim + trapped → d≈0.98, f(d)≈1.50. Maximal extraction; historical exclusion from coverage, trapped by systemic gatekeeping. Public health regulators: Constrained + organized → d≈0.60, f(d)≈0.82. Intermediate extraction; have institutional power but face Congressional constraints on tax code reform.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that ESI is a Snare, not a Rope. The extraction (0.52) exceeds the coordination benefit because the suppression (0.68) prevents exit while the pooling could be achieved through individual market reform (with subsidies). The Tangled Rope classification from self-employed and regulator perspectives is justified by their constrained exit options and need for active enforcement, but the aggregate system is extractive. The false natural law view (Mountain classification from analytical observer) is revealed as such by the base properties: ε=0.52 >> 0.25 (mountain threshold), suppression=0.68 >> 0.05 (mountain threshold). The engine's false summit detector flags this. The functional test: could the coordination function (risk pooling) be achieved through alternative mechanisms (individual market + subsidies, public option, universal coverage)? Yes, empirically demonstrated in other high-income countries. Therefore, the constraint is not a coordination failure requiring ESI to solve — it is a regulatory choice maintaining ESI for beneficiary extraction. This is the mandatrophy resolution: the constraint is a Snare (extractive) with some theaters of coordination (Rope-like experiences for beneficiaries and large pooled workers), not a Rope with some extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employer_exit_feasibility,
    'Could large employers realistically switch to defined-contribution healthcare financing (give workers vouchers rather than coverage) without triggering regulatory resistance or adverse selection collapse?',
    'Modeling of enrollment dynamics if all large employers switched to healthcare voucher systems; measurement of adverse selection effects on individual market; regulatory impact analysis of Congressional action required to eliminate tax subsidies',
    'If feasible: constraint is primarily path-dependent tax code artifact (easier to reform). If infeasible: constraint is locked in by labor market coordination failure (harder to reform).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_exit_feasibility, empirical, 'Whether employer exit from coverage provision is structurally feasible').

omega_variable(
    individual_market_sufficiency,
    'Can the individual health insurance market (including ACA exchanges) achieve the same risk-pooling efficiency and adverse-selection prevention as large employer group plans without regulatory intervention?',
    'Comparative analysis of premium loading, coverage denial rates, and adverse selection magnitude in individual market vs. group plans; assessment of whether individual market would stabilize with long-term subsidies or regulatory mandates',
    'If sufficient: ESI is extractive without coordination benefit (pure Snare). If insufficient: constraint has genuine coordination function (Tangled Rope justified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_market_sufficiency, empirical, 'Whether individual market can achieve group plan efficiency without ESI').

omega_variable(
    political_reform_threshold,
    'What distribution of costs (uninsured rate, per-capita healthcare spending, employer administrative burden) would trigger Congressional action to eliminate ESI tax subsidies and move to universal coverage?',
    'Historical policy reform analysis; survey of reform proposals and Congressional estimates; scenario modeling of trigger points for legislative coalitions',
    'If threshold is near current trajectory: constraint is unstable and could collapse quickly. If threshold is very high: constraint is institutionally locked and will persist unless exogenous shock (pandemic, macroeconomic shift) changes calculus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_reform_threshold, preference, 'Political threshold for ESI tax subsidy elimination').

omega_variable(
    wage_suppression_magnitude,
    'How much of the wage stagnation in real compensation (1980-2023) is attributable to workers bearing implicit costs of ESI employment lock (foregone job mobility, skill misallocation, employer rent-extraction)?',
    'Econometric estimation of wage premiums for jobs offering ESI vs. equivalent jobs without coverage; measurement of switching costs and job-match quality loss from employment lock; comparison of wage trajectories in high vs. low ESI coverage regions',
    'If large (>1% of annual wage growth foregone): constraint is severely extractive. If small (<0.1%): extraction is localized to vulnerable subpopulations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_magnitude, empirical, 'Magnitude of wage suppression from ESI lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_employer_health_insurance, 1945, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esi_tr_t0, us_employer_health_insurance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(esi_tr_t35, us_employer_health_insurance, theater_ratio, 35, 0.48).
narrative_ontology:measurement(esi_tr_t70, us_employer_health_insurance, theater_ratio, 70, 0.61).

% Extraction over time
narrative_ontology:measurement(esi_be_t0, us_employer_health_insurance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(esi_be_t35, us_employer_health_insurance, base_extractiveness, 35, 0.42).
narrative_ontology:measurement(esi_be_t70, us_employer_health_insurance, base_extractiveness, 70, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_employer_health_insurance, resource_allocation).
narrative_ontology:affects_constraint(us_employer_health_insurance, us_healthcare_cost_escalation).
narrative_ontology:affects_constraint(us_employer_health_insurance, employment_mobility_suppression).
narrative_ontology:affects_constraint(us_employer_health_insurance, uninsured_population_trap).
narrative_ontology:affects_constraint(us_employer_health_insurance, small_business_adverse_selection).

% DUAL FORMULATION NOTE:
% ESI is composed of multiple structurally distinct constraints that should ideally be decomposed: (1) the tax code structure (why ESI receives preferential tax treatment), (2) the regulatory entrenchment (why individual market cannot achieve competitive equilibrium), (3) the employment lock mechanism (how wage suppression occurs), (4) the adverse selection dynamics (how uninsured and small-group pools degrade). This story treats them as a unified system constraint; a more refined corpus would separate these into linked stories with individual ε values reflecting their different empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_employer_health_insurance, moderate, 0.8).
constraint_indexing:directionality_override(us_employer_health_insurance, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
