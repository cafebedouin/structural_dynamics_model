% ============================================================================
% CONSTRAINT STORY: sotu_1956_eisenhower_social_security_unemployment_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1956_eisenhower_social_security_unemployment_expansion, []).

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
 *   constraint_id: sotu_1956_eisenhower_social_security_unemployment_expansion
 *   human_readable: Social Security and Unemployment Insurance Expansion (1956)
 *   domain: social_policy/labor_insurance
 *
 * SUMMARY:
 *   The 1956 expansion of Social Security and unemployment insurance
 *   represents a critical moment in the construction of the American social
 *   insurance state. The expansion extended coverage to approximately 10
 *   million previously uncovered workers (primarily agricultural and domestic
 *   workers) and improved benefits for existing recipients. Structurally, the
 *   expansion exhibits the defining feature of Tangled Rope: it solves a
 *   genuine coordination problem (pooling income risk across millions of
 *   workers requires centralized infrastructure that individual market actors
 *   cannot coordinate) while simultaneously extracting costs from employers
 *   and high-income workers through mandatory payroll contributions. The
 *   constraint exhibits asymmetric power distribution: newly covered workers
 *   and their dependents benefit from income security; employers and
 *   high-income taxpayers bear involuntary costs; and the federal government
 *   gains administrative authority over labor-market risk. The mechanism is
 *   presented in insurance language (workers 'earn' benefits through
 *   contributions) but functions partly as wealth redistribution
 *   (contributions do not perfectly correlate with benefits by income). The
 *   theater_ratio (0.38) reflects that the expansion legitimately delivers
 *   income security (functional core) but also maintains significant
 *   performative elements: actuarial justifications that don't change policy,
 *   regulatory compliance overhead, and political rhetoric that emphasizes
 *   individual contribution-benefit linkage while obscuring progressive
 *   redistribution. The extractiveness trajectory (0.15 → 0.35 over the
 *   interval) reflects increasing tension as the program's costs accumulate,
 *   employers absorb payroll taxes, and benefit promises grow relative to
 *   revenue — a pattern that would lead to mandatrophy pressures in later
 *   decades.
 *
 * KEY AGENTS:
 *   - Newly Covered Agricultural Workers: Primary beneficiary (powerless/constrained) — gain income security, face modest contribution costs, exit constrained by labor market mobility
 *   - Small Employers: Secondary actor (moderate/constrained) — bear payroll contribution costs, benefit from reduced labor turnover, experience mixed coordination and extraction
 *   - High-Income Business Owners: Secondary victim (powerful/mobile) — bear higher payroll taxes, experience extraction, have exit options but constrained by enforcement
 *   - Labor Unions: Institutional beneficiary (institutional/constrained) — benefit from strengthened insurance floor, experience mixed coordination (wage focus) and extraction (reduced take-home pay)
 *   - Progressive Reform Coalition: Organized beneficiary (organized/arbitrage) — architects and advocates for expansion, see it as temporary step toward universal coverage
 *   - Federal Government: Institutional coordinator (institutional/arbitrage) — creates and enforces the system, experiences it as coordination mechanism
 *   - Insurance Industry: Institutional actor (institutional/arbitrage) — persists in supplemental roles despite public insurance dominance, experiences constraint as theatrical
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination and asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1956_eisenhower_social_security_unemployment_expansion, 0.35).
domain_priors:suppression_score(sotu_1956_eisenhower_social_security_unemployment_expansion, 0.25).
domain_priors:theater_ratio(sotu_1956_eisenhower_social_security_unemployment_expansion, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1956_eisenhower_social_security_unemployment_expansion, extractiveness, 0.35).
narrative_ontology:constraint_metric(sotu_1956_eisenhower_social_security_unemployment_expansion, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sotu_1956_eisenhower_social_security_unemployment_expansion, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1956_eisenhower_social_security_unemployment_expansion, tangled_rope).
narrative_ontology:human_readable(sotu_1956_eisenhower_social_security_unemployment_expansion, "Social Security and Unemployment Insurance Expansion (1956)").
narrative_ontology:topic_domain(sotu_1956_eisenhower_social_security_unemployment_expansion, "social_policy/labor_insurance").

domain_priors:requires_active_enforcement(sotu_1956_eisenhower_social_security_unemployment_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_social_security_unemployment_expansion, newly_covered_workers).
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_social_security_unemployment_expansion, dependents_of_covered_workers).
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_social_security_unemployment_expansion, labor_market_stabilizers).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_social_security_unemployment_expansion, employers_bearing_payroll_costs).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_social_security_unemployment_expansion, high_income_taxpayers).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_social_security_unemployment_expansion, general_revenue_fund).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEWLY COVERED AGRICULTURAL WORKER (ROPE) — Previously excluded from social insurance; extension provides genuine security coordination benefit. Worker faces modest contribution costs but gains income protection against unemployment, disability, and old age. Exit option constrained by limited labor market mobility and rural location. Experiences this as primarily coordinative — solving the collective action problem of income security across dispersed rural workers. Net beneficiary despite modest costs.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL EMPLOYER IN MANUFACTURING (TANGLED ROPE) — Bears mandatory payroll contributions; experiences both coordination benefit (stable, insured workforce reduces labor turnover costs) and extraction (mandatory contribution burden reduces available capital for wages or investment). Exit constrained by labor law requirements and competitive necessity of participating in national labor pool. Genuine dual nature: coordination function is real (employer benefits from lower turnover), but asymmetric extraction is also real (employer's contribution is involuntary cost). Moderate power reflects ability to organize collectively with other employers but inability to exit the system.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME BUSINESS OWNER (SNARE) — Faces payroll tax increase on earnings above the current contribution cap; experiences extraction without meaningful coordination benefit (their income is already secured). High suppression of tax avoidance options (mandatory participation, enforcement mechanisms, capped deductions). However, classified as snare rather than mountain because: (1) exit options exist (relocate business, restructure income), (2) extraction is targeted but not absolute, and (3) the mechanism is explicitly policy-driven, not natural law. Power is powerful because business owners can lobby, influence, and reorganize around the constraint. Still experiences the constraint as extractive net of benefits.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNION (TANGLED ROPE) — Experiences the expansion as mixed coordination and extraction. Genuine coordination benefit: expansion strengthens the social insurance floor, reducing pressure on union-negotiated pension schemes and enabling focus on wage gains rather than benefit design. But also extraction: union members contribute payroll taxes that reduce take-home wages; the pool expansion dilutes per-worker benefit ratios if the program grows faster than revenue. Union power is institutional but exit is constrained by labor law and competitive necessity of participating in the national insurance system. Both benefits and burdens are real and significant.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE REFORM COALITION (SCAFFOLD) — Organized actors (AFL-CIO, social insurance advocates, Democratic legislators) see the expansion as a temporary solution with sunset logic: the goal is to extend the social insurance model until coverage becomes universal, at which point the expansion mechanism becomes the baseline. Low effective extraction from this perspective because the coalition has agency, political power, and sees a clear exit path (universal coverage making incremental expansions unnecessary). Arbitrage-level exit option reflects ability to organize political alternatives. Time horizon is generational because universal coverage is viewed as the eventual endpoint, with this expansion as a transitional step.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INSURANCE INDUSTRY (PITON) — Maintains private insurance and actuarial infrastructure despite expansion of public insurance. The industry experiences the constraint as theatrical: it must exist and participate in the system but its primary profit mechanism (underwriting risk pricing) is bypassed by the government insurance floor. The private insurance industry persists through inertia and regulatory protection, not because it solves the core insurance problem better than the public system. Theater_ratio is high because the industry continues actuarial and administrative functions that the social insurance system largely supersedes. The industry has arbitrage-level exit options (relocate to underserved markets, specialize in supplemental products) but these are constrained by political resistance to private insurance domination of essential coverage.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: FEDERAL GOVERNMENT — COORDINATION VIEW (ROPE) — From the perspective of the state apparatus itself, the expansion solves a genuine collective action problem: coordinating income security across millions of workers requires a centralized mechanism that individuals and firms cannot coordinate independently. The government's perspective is that this is primarily coordinative. Low effective extraction from the government's viewpoint because the constraint is self-imposed (the government creates and enforces it). The revenue flow is experienced as resource channeling toward a legitimate collective good, not extraction by an external actor.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the expansion exhibits genuine coordination (solving labor-market risk pooling) AND genuine asymmetric extraction (mandatory payroll contributions are involuntary costs to employers and high-income workers, with benefits distributed unequally by income). The mechanism is explicitly policy-driven with identifiable winners and losers. The analytical perspective sees this as a policy choice with real distributive consequences, not as a natural law or pure coordination. Suppression is moderate: workers and employers have some ability to organize, lobby, and modify the terms, though they cannot exit the basic mechanism. Theater is moderate because the expansion is presented in insurance language (contributions earn benefits) but also functions as wealth redistribution.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1956_eisenhower_social_security_unemployment_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1956_eisenhower_social_security_unemployment_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1956_eisenhower_social_security_unemployment_expansion, TR),
    TR >= 0.70.

:- end_tests(sotu_1956_eisenhower_social_security_unemployment_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The expansion solves a genuine collective action problem (income risk pooling) but imposes involuntary costs on employers and high-income workers. The metric reflects that the constraint functions partly as coordination (new workers genuinely benefit from security) and partly as extraction (costs are involuntary and unequally distributed). The trajectory from 0.15 to 0.35 reflects accumulating tensions as employer costs compound and benefit commitments exceed revenue growth — a pattern characteristic of Tangled Rope constraints that begin with strong coordination functions but gradually accumulate extractive overhead. Suppression (0.25): Moderate-low. Employers and workers have significant ability to organize, lobby, and influence terms (unions are powerful, business associations can petition Congress). The constraint cannot be physically avoided but is not locked in by desperation or complete material dependency. Workers can find employment elsewhere (though constrained by market structure); employers can reorganize labor processes (though constrained by competitive necessity). Theater ratio (0.38): Moderate. The expansion delivers genuine insurance benefits (functional core) but maintains performative elements. Actuarial justifications serve legitimacy more than policy determination; regulatory overhead is partly functional (claims processing) and partly theatrical (compliance rituals that don't change outcomes); the insurance framing obscures progressive redistribution. The ratio rises over the interval as administrative complexity grows and the gap between contribution-benefit rhetoric and redistributive reality widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence. The newly covered agricultural worker sees primarily coordination (Rope) — they are solving the security problem their powerlessness created. The small employer sees mixed coordination and extraction (Tangled Rope) — benefits exist but costs are involuntary. The high-income business owner sees extraction (Snare) — high suppression of tax avoidance, no meaningful coordination benefit. The labor union sees mixed effects (Tangled Rope) — coordination benefit (wage negotiation focus) but also extraction (payroll contribution reduces take-home pay). The reform coalition sees a temporary solution with exit (Scaffold) — universal coverage will eventually make this expansion mechanism unnecessary. The insurance industry sees its own degradation (Piton) — the expansion pushes private insurance out of essential coverage; the industry persists through regulatory protection, not market function. The federal government sees pure coordination (Rope) — the state solves the collective action problem it perceives. The analytical observer sees the full structure (Tangled Rope) — genuine coordination function genuinely present, but asymmetric costs and redistributive effects are also genuine and unavoidable. The perspectival gap reflects real differences in how the agents experience the constraint, not measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's power level, exit options, and relationship to the extraction flow. Newly covered workers (powerless/trapped) experience high d (0.75+) — they bear some contribution costs but are the primary beneficiaries; the constraint targets them for protection, not extraction. Small employers (moderate/constrained) experience mid-range d (0.45-0.55) — they bear involuntary payroll costs but also benefit from labor pool stability; costs and benefits are both real. High-income business owners (powerful/mobile) experience high d (0.70+) — they bear payroll taxes with minimal coordination benefit; their power and mobility options are constrained by enforcement and competitive necessity. Labor unions (institutional/constrained) experience moderate d (0.50-0.60) — benefits exist (stronger floor for negotiation) but costs are also real (payroll tax reduces member income). Reform coalition (organized/arbitrage) experience low d (0.20-0.30) — they are the architects and primary beneficiaries; their arbitrage options (political alternatives) are broad. Insurance industry (institutional/arbitrage) experience low-to-negative d (0.05-0.15) — the expansion is a constraint imposed on the industry, pushing it toward supplemental roles; their arbitrage options are limited by political resistance. Federal government (institutional/arbitrage) experience negative d (0.00-0.20) — the government creates the system for its own coordination purposes. Analytical observer (analytical/analytical) experiences canonical d (0.72) — observes the full structure without direct material involvement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits the core mandatrophy structure: at low time horizons (immediate, biographical), the beneficiary and victim perspectives diverge sharply (newly covered worker sees Rope, employer sees Snare), which is correct classification reflecting real asymmetry. At higher time horizons (generational, civilizational), the perspectives converge toward Tangled Rope as both groups recognize that the constraint solves a genuine coordination problem while imposing asymmetric costs. The mandatrophy resolves through time: the constraint's dual nature (coordination + extraction) becomes visible only when the observation period extends long enough for both effects to manifest. Immediate perspective: extraction appears dominant (new costs are salient, benefits accumulate slowly). Biographical perspective: coordination and extraction are both visible (workers recognize security benefit, employers recognize reduced turnover). Generational perspective: the constraint appears as necessary infrastructure (everyone has internalized that income pooling requires collective mechanisms). This trajectory is characteristic of Tangled Rope that resists reclassification as either pure Rope (ignoring real costs) or pure Snare (ignoring genuine benefits). The Tangled Rope classification is stable because both functions are structurally irreducible — you cannot remove the extraction without destroying the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_insurance_framing,
    'Is the payroll contribution experienced primarily as an insurance premium or as a tax? Does the framing determine the agent''s classification?',
    'Historical analysis of worker and employer attitudes toward payroll contributions; survey data on perceived benefit-contribution linkage; comparison of insurance vs tax framing in political rhetoric vs economic reality',
    'If framed as insurance: more perspectives classify as Rope (genuine coordination). If framed as tax: more perspectives classify as Snare/Tangled Rope (extraction mechanism). The constraint''s actual effect may be identical, but perspectival classification depends partly on the culturally constructed meaning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_insurance_framing, conceptual, 'Whether payroll contributions are perceived as insurance or taxation').

omega_variable(
    benefit_adequacy_to_new_covered_workers,
    'Do newly covered workers receive benefits adequate to justify the contribution burden, or are they subsidizing high-benefit incumbent workers?',
    'Actuarial analysis of benefit-to-contribution ratios by cohort; comparison of marginal returns for newly covered vs long-tenure workers; intergenerational accounting of benefit flows',
    'If benefits are adequate and fairly distributed: coordination function is genuine across all cohorts, perspectives shift toward Rope. If newly covered workers are net contributors: they are partially subsidizing incumbent workers, perspectives shift toward Snare for this group (extraction without benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_adequacy_to_new_covered_workers, empirical, 'Whether newly covered workers receive actuarially fair benefits').

omega_variable(
    administrative_theater_overhead,
    'What proportion of expanded program resources goes to genuine benefit delivery vs administrative, compliance, and regulatory theater?',
    'Administrative cost analysis: percentage of payroll tax devoted to claims processing, verification, record-keeping, regulatory compliance; comparison with pure insurance overhead; identification of redundant or performative activities (e.g., actuarial justifications that don''t change policy)',
    'If overhead > 15%: theater_ratio should be higher (0.45+), indicating Piton risk. If overhead < 5%: theater_ratio should be lower, indicating purer Rope/Tangled Rope. High theater indicates the expansion is becoming inertial rather than genuinely functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_theater_overhead, empirical, 'Administrative overhead as proportion of program resources').

omega_variable(
    political_coalition_stability,
    'Will the reform coalition supporting the expansion remain organized and politically powerful enough to defend and extend coverage, or will it fragment and allow entrenchment?',
    'Political history: tracking of union coalition strength, progressive legislative power, public support for social insurance over the next decade; identification of defection points or consolidation moments',
    'If coalition remains strong: scaffold sunset logic is real, the expansion will progress toward universal coverage. If coalition fragments: the expansion becomes Piton (inertial, theatrical, protected by institutional momentum rather than active support). The scaffold perspective''s arbitrage exit option depends on sustained coalition power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_coalition_stability, empirical, 'Political durability of the reform coalition').

omega_variable(
    labor_market_substitution_effects,
    'Does the expansion increase labor force participation among previously uncovered workers, or do payroll costs reduce hiring, wages, or work hours?',
    'Labor market empirical analysis: comparing employment, wage, and hours trends for newly covered vs control groups; identification of employer substitution (e.g., shift from full-time to part-time to avoid coverage)',
    'If participation increases: the constraint genuinely solves a coordination problem (enables labor force entry by providing security). If participation decreases: the constraint is partly extractive (payroll costs reduce employment opportunities, harming the very workers it aims to protect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_substitution_effects, empirical, 'Labor market participation effects of expanded coverage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1956_eisenhower_social_security_unemployment_expansion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu56_tr_t0, sotu_1956_eisenhower_social_security_unemployment_expansion, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sotu56_tr_t5, sotu_1956_eisenhower_social_security_unemployment_expansion, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sotu56_tr_t10, sotu_1956_eisenhower_social_security_unemployment_expansion, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(sotu56_be_t0, sotu_1956_eisenhower_social_security_unemployment_expansion, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sotu56_be_t5, sotu_1956_eisenhower_social_security_unemployment_expansion, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(sotu56_be_t10, sotu_1956_eisenhower_social_security_unemployment_expansion, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1956_eisenhower_social_security_unemployment_expansion, resource_allocation).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_social_security_unemployment_expansion, payroll_tax_incidence_on_wages).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_social_security_unemployment_expansion, social_security_trust_fund_solvency).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_social_security_unemployment_expansion, labor_force_participation_in_covered_sectors).

% DUAL FORMULATION NOTE:
% This constraint is upstream of several sectoral and fiscal constraints: payroll tax incidence mechanisms, trust fund dynamics, and labor market participation effects. The expansion's extractiveness trajectory affects all downstream constraints. This story captures the expansion mechanism itself; sibling stories address the specific empirical effects (wage incidence, trust fund pressure, participation effects) with their own ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1956_eisenhower_social_security_unemployment_expansion, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
