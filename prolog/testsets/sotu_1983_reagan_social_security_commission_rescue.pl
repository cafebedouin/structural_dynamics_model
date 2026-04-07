% ============================================================================
% CONSTRAINT STORY: sotu_1983_reagan_social_security_commission_rescue
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1983_reagan_social_security_commission_rescue, []).

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
 *   constraint_id: sotu_1983_reagan_social_security_commission_rescue
 *   human_readable: 1983 Social Security Commission Rescue: Distributed Sacrifice and Institutional Legitimacy
 *   domain: social_policy/fiscal_governance
 *
 * SUMMARY:
 *   The 1983 Social Security Commission rescue represents a structurally
 *   complex constraint that imposes distributed sacrifice across multiple
 *   constituencies — self-employed workers, younger workers, higher-income
 *   beneficiaries, and government employees — to prevent system bankruptcy
 *   affecting 36 million current beneficiaries. The mechanism achieves
 *   bipartisan consensus by framing the package as 'shared sacrifice,' but
 *   the actual distributional impact is asymmetric: costs are front-loaded on
 *   workers and self-employed while benefits accrue to current beneficiaries.
 *   The constraint exhibits tangled rope structure at the baseline level: it
 *   contains genuine coordination (system solvency, intergenerational
 *   transfer mechanism) alongside asymmetric extraction (burden placement on
 *   low-power agents). The theater ratio increased from 0.35 (pre-commission)
 *   to 0.55 (post-implementation) reflecting the gap between the 'shared
 *   sacrifice' narrative and actual distributional mechanics. Government
 *   employee inclusion (CSRS) demonstrates the hybrid nature: it is
 *   simultaneously a coordination mechanism (unified pension system) and a
 *   coercive incorporation (retroactive mandate without opt-out). The 75-year
 *   solvency horizon creates a scaffold temporal structure, but the
 *   credibility of this sunset is an open question — subsequent commissions
 *   did not substantially revise the 1983 framework, suggesting the sunset
 *   may be a false one.
 *
 * KEY AGENTS:
 *   - Self-employed workers: Primary victims (powerless/trapped) — bear dual payroll tax, increasing from 9.35% to 15.3%, with no exit mechanism
 *   - Higher-income beneficiaries (>$25,000): Secondary victims (powerless/trapped) — subject to means-testing, benefit reduction for earned income
 *   - Younger workers (entry-level labor force): Secondary victims (moderate/constrained) — face higher payroll rates (5.85% to 7.65%) for system solvency
 *   - Government employees (CSRS): Mixed victims/organized (organized/constrained) — forced into Social Security, gaining pension coordination but bearing integration costs
 *   - Social Security beneficiaries (36 million current): Primary beneficiaries (powerless/arbitrage) — protected from system insolvency, benefits preserved
 *   - High-income workers (above wage base): Partial beneficiaries (powerful/arbitrage) — taxable wage base raised, but can arbitrage through income structuring
 *   - Bipartisan institutional coalition (Reagan/O'Neill/Baker): Institutional beneficiary (institutional/arbitrage) — gains legitimacy through cross-party consensus, avoids more radical restructuring
 *   - SSA bureaucracy: Institutional maintainer (institutional/arbitrage) — implements constraints, maintains system through enforcement theater
 *   - Analytical observer: External position (analytical/analytical) — risks naturalizing political choices as demographic necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1983_reagan_social_security_commission_rescue, 0.38).
domain_priors:suppression_score(sotu_1983_reagan_social_security_commission_rescue, 0.48).
domain_priors:theater_ratio(sotu_1983_reagan_social_security_commission_rescue, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1983_reagan_social_security_commission_rescue, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1983_reagan_social_security_commission_rescue, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1983_reagan_social_security_commission_rescue, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1983_reagan_social_security_commission_rescue, tangled_rope).
narrative_ontology:human_readable(sotu_1983_reagan_social_security_commission_rescue, "1983 Social Security Commission Rescue: Distributed Sacrifice and Institutional Legitimacy").
narrative_ontology:topic_domain(sotu_1983_reagan_social_security_commission_rescue, "social_policy/fiscal_governance").

domain_priors:requires_active_enforcement(sotu_1983_reagan_social_security_commission_rescue).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1983_reagan_social_security_commission_rescue, social_security_beneficiaries).
narrative_ontology:constraint_beneficiary(sotu_1983_reagan_social_security_commission_rescue, future_workers).
narrative_ontology:constraint_beneficiary(sotu_1983_reagan_social_security_commission_rescue, institutional_legitimacy_coalition).
narrative_ontology:constraint_victim(sotu_1983_reagan_social_security_commission_rescue, self_employed_workers).
narrative_ontology:constraint_victim(sotu_1983_reagan_social_security_commission_rescue, higher_income_beneficiaries).
narrative_ontology:constraint_victim(sotu_1983_reagan_social_security_commission_rescue, government_employees).
narrative_ontology:constraint_victim(sotu_1983_reagan_social_security_commission_rescue, younger_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SELF-EMPLOYED WORKER (SNARE) — Bears dual payroll tax (employer and employee portions), increasing from 9.35% to 15.3% on self-employment income. No exit mechanism: self-employment is the only viable income path for this agent. Trapped by occupational structure and economic necessity. Maximum extraction experienced — the burden was front-loaded to prevent system insolvency, making it visible and immediate.
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HIGHER-INCOME BENEFICIARY (SNARE) — Benefits subject to means-testing for incomes above $25,000 (couples). High-income retirees lose benefit dollars for additional earnings. Trapped by age and prior contributions; cannot exit or renegotiate. Extraction is asymmetric: lower-income beneficiaries unaffected, upper bracket bears disproportionate cost reduction.
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENT EMPLOYEES (TANGLED ROPE) — Forced into Social Security system after decades of exemption; benefits from expanded pension coverage coordination, but bears immediate cost of dual contributions and payroll integration. Organized labor can negotiate protections (transition periods, dual-benefit grandfather clauses) but cannot exit the mandate. Constrained by regulatory requirement; moderate agency through union representation. Mixed: genuine coordination gain (unified retirement system) alongside asymmetric extraction (retroactive mandate costs).
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: YOUNGER WORKERS (TANGLED ROPE) — Face higher payroll tax rates (increased from 5.85% to 7.65% by 1990) for system solvency. Constrained by labor market entry requirements and pension integration. Gain from system rescue (ensures benefits exist when they retire) but bear cost front-loaded. Exit is theoretically possible (emigration, informal economy) but practically expensive. Extraction is asymmetric but negotiated through intergenerational transfer logic.
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SOCIAL SECURITY BENEFICIARY COALITION (ROPE) — 36 million beneficiaries protected from system insolvency. Net benefit flows to this group: system preservation, benefit continuation, increased tax base. Arbitrage position: beneficiaries can shift burden to workers without organizational cost. The rescue package functions as a coordination mechanism for the beneficiary group — distributed sacrifice across multiple constituencies enables continued benefit delivery. This agent experiences extraction running toward them, not away.
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL LEGITIMACY COALITION (SCAFFOLD) — Reagan, O'Neill, Baker agreement demonstrates bipartisan capacity to address entitlement crisis. The rescue functions as a temporary structural fix with built-in sunset: the trust fund solvency horizon (75 years) creates temporal boundary. Theater ratio is high (0.55) because the 'shared sacrifice' narrative obscures distributional asymmetry, but the sunset is real — the 1983 package was explicitly designed to avoid requiring full restructuring for a generation. Net effect: preserves institutional legitimacy without permanent extraction architecture. Organized actors (political parties, unions) experience this as a coordination win with temporal bounds.
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: HIGH-INCOME WORKERS (TANGLED ROPE) — Payroll tax ceiling raised from $29,700 to $35,700 (with phased increases). High earners below ceiling pay higher taxes; above ceiling, the effective tax rate declines. Powerful agents can arbitrage through: income shifting, timing strategies, pension optimization. Net extraction is moderate because powerful agents can navigate the constraint through financial engineering. Benefits from system preservation accrue universally, but extraction is differentially escapable.
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: SOCIAL SECURITY ADMINISTRATION (PITON) — Implements benefit calculations, means-testing, government employee integration rules, and payroll tax collection. The constraint persists through institutional inertia and legal requirement, not through active functional necessity. Theater ratio is moderate (0.55) — the administrative apparatus is partially performative (compliance theater), partially functional (actual benefits delivery). The SSA maintains the system because it is legislatively mandated and politically sensitive, not because the specific mechanism is the only way to achieve retirement security.
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / DEMOGRAPHIC NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, pay-as-you-go pension systems in aging societies structurally require revenue increases or benefit adjustments. The 1983 commission resolved this immutable demographic logic: declining worker-to-beneficiary ratios force cost redistribution. This perspective naturalizes the constraint as inherent to the demographic transition. However, structural data indicates false summit: the specific distributional choices (payroll tax over income tax, benefit cuts for high earners, CSRS inclusion) are political, not demographic necessities. The mountain classification conceals policy contingencies.
constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1983_reagan_social_security_commission_rescue_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1983_reagan_social_security_commission_rescue, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1983_reagan_social_security_commission_rescue, TR),
    TR >= 0.70.

:- end_tests(sotu_1983_reagan_social_security_commission_rescue_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The package imposes measurable costs (payroll tax increases, benefit adjustments, mandatory inclusion) on multiple constituencies. However, the extraction is not maximal because: (1) system solvency is a genuine coordination problem — without rescue, the system collapses entirely, harming all beneficiaries; (2) burden distribution is negotiated across constituencies rather than unilateral; (3) high-income workers and institutional actors can arbitrage aspects of the constraint. The baseline extractiveness (0.22) represents the pre-commission crisis state, where system insolvency threatened universal harm. The rescue raises extractiveness to 0.38 by concentrating and making visible the costs that were previously diffuse and implicit. Suppression (0.48): Moderate-high. Multiple mechanisms suppress alternatives: (1) political urgency — system bankruptcy is imminent, limiting radical restructuring debate; (2) labor market constraints — workers cannot easily exit payroll system; (3) occupational constraints — self-employed workers have no alternative income path; (4) age constraints — beneficiaries cannot renegotiate or exit. Suppression is not total (0.60+) because some agents retain negotiation capacity (unions for government employees, income optimization for high earners). Theater ratio (0.55): Moderate-high. The 'shared sacrifice' narrative obscures asymmetric distribution: costs fall disproportionately on low-power agents (self-employed, young workers), while benefits flow to beneficiaries and institutional actors gain legitimacy. The bipartisan framing functions as legitimacy theater — the visible consensus obscures that the deal works best for those with exit options and institutional power. Theater increased from 0.35 to 0.55 because the commission's public presentation emphasized consensus and shared burden far more than the actual distributional mechanics warranted.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the self-employed worker (snare) and the beneficiary coalition (rope) is maximum: one group experiences pure extraction, the other experiences coordination benefit. The gap between younger workers (tangled rope) and beneficiaries (rope) is also substantial: workers bear extraction; beneficiaries receive coordination. The gap between the bipartisan coalition's view (scaffold with sunset and legitimacy) and the self-employed worker's view (snare with no exit) is the gap between institutional power and powerlessness. The analytical observer risks collapsing this gap by naturalizing the constraint as demographic necessity, which would erase the political agency of the institutional actors who shaped the distributional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. Self-employed workers and higher-income beneficiaries are victims with trapped exits → d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Younger workers are victims with constrained (not trapped) exits — they can theoretically change occupations, emigrate, work informally, though at high cost → d ≈ 0.75 → f(d) ≈ 1.10. Government employees are organized with constrained exits and mixed beneficiary/victim status → d ≈ 0.55 → f(d) ≈ 0.75. Social Security beneficiaries are beneficiaries with arbitrage exits (can shift burden to workers through political power) → d ≈ 0.15 → f(d) ≈ -0.01 (experienced extraction runs toward this group, not away). High-income workers are partial beneficiaries with arbitrage capacity → d ≈ 0.35 → f(d) ≈ 0.15 (moderate, arbitrageable extraction). Bipartisan coalition gains institutional legitimacy from consensus-building, positioning them as beneficiaries with maximum arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 (institution benefits from visibility of the deal). The directionality chain reveals that extracted value flows from low-d agents (trapped victims) to high-arbitrage agents (beneficiaries and institutional actors).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CANDIDATE: The 1983 rescue package shows evidence of both coordination and extraction, creating genuine ambiguity about its primary function. RESOLUTION: The constraint is tangled rope at the baseline level (ε=0.38, suppression=0.48, active enforcement required, beneficiaries and victims present) because: (1) genuine coordination function exists — system solvency is a coordination problem that the package solves; (2) asymmetric extraction also exists — costs are front-loaded on low-power agents while benefits flow to high-power agents; (3) active enforcement is required — payroll tax collection, benefit means-testing, CSRS integration all require ongoing administration. The mandatrophy is resolved by recognizing that 'shared sacrifice' is the coordination framing that justifies the extraction: agents accept burden redistribution because the alternative (system collapse) is worse for everyone, including victims. This is the defining feature of tangled rope: extraction justified by coordination necessity. The theater ratio (0.55) indicates that the legitimacy gain from bipartisan consensus partially obscures the actual distributional asymmetry, but the coordination function is real enough that it is not pure snare. Future resolution: if the 75-year solvency horizon proves to be a false sunset (rates lock in permanently without revisiting distributional choices), the constraint may harden into snare; if genuine policy flexibility emerges in later commissions, the scaffold view gains credibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributional_intent_vs_outcome,
    'Does the ''shared sacrifice'' framing accurately reflect the actual distributional impact, or does it obscure asymmetric burden placement?',
    'Cohort-level accounting: track lifetime tax contributions and benefit receipts by income quintile, occupational category (self-employed vs employed vs government), and age cohort at implementation',
    'If shared: extraction hypothesis overstated, constraint is genuine coordination (Rope from multiple perspectives). If asymmetric: extraction is real but naturalized through bipartisan consensus theater (Tangled Rope classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_intent_vs_outcome, empirical, 'Whether distributional burden aligns with ''shared sacrifice'' narrative').

omega_variable(
    government_employee_inclusion_coercion,
    'Is CSRS integration a coordination mechanism or a coercive incorporation masquerading as fairness?',
    'Historical analysis of CSRS employee resistance, union negotiation outcomes, and alternative structural arrangements (parallel systems, grandfathering, opt-out mechanisms). Comparison with international pension integration models.',
    'If coordination: forced inclusion is justified redistribution cost-sharing. If coercive: government employees are victims of a snare masquerading as tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_employee_inclusion_coercion, empirical, 'Whether CSRS integration is coordination or coercion').

omega_variable(
    bipartisan_consensus_as_legitimacy_theater,
    'Does bipartisan agreement on the package increase institutional legitimacy (genuine coordination) or function primarily as political theater obscuring extractive distribution?',
    'Public polling on constraint perception by affected groups; measurement of institutional trust pre/post-1983; comparison of Republican vs Democratic voter responses to Reagan vs O''Neill framings of the same policy',
    'If legitimacy is real: scaffold perspective valid, extraction is limited by consensus-building cost. If theater: bipartisan framing is a cover story, and extraction is higher than base metrics indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bipartisan_consensus_as_legitimacy_theater, conceptual, 'Whether bipartisan consensus generates or masks legitimacy').

omega_variable(
    payroll_tax_incidence_shifting,
    'Who actually bears the cost of the payroll tax increase — employees, employers through wage suppression, or consumers through price inflation?',
    'Economic incidence analysis: wage growth differential post-1983 comparing covered vs uncovered workers; wage growth differential between sectors with high vs low employment tax sensitivity; price inflation correlation with payroll tax rates',
    'If employer/wage incidence: worker extraction is lower than statutory tax implies. If employee incidence: extraction is higher. If shifted to consumers: extraction is diffuse and unrecoverable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payroll_tax_incidence_shifting, empirical, 'Economic incidence of payroll tax increase').

omega_variable(
    sunset_clause_credibility,
    'Is the 75-year solvency horizon a genuine sunset clause enabling future policy flexibility, or a false sunset that politically locks in higher tax rates permanently?',
    'Historical tracking: do subsequent commissions revisit tax/benefit tradeoffs, or do 1983 rates become the new baseline? Polling on public understanding of solvency horizon and reform expectations.',
    'If credible sunset: scaffold classification holds, extraction is temporally bounded. If false: the constraint hardens into permanent extraction (reclassify to snare/tangled_rope with indefinite horizon).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_credibility, conceptual, 'Whether 75-year solvency horizon enables genuine policy flexibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1983_reagan_social_security_commission_rescue, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ss_rescue_theater_baseline, sotu_1983_reagan_social_security_commission_rescue, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ss_rescue_theater_immediate, sotu_1983_reagan_social_security_commission_rescue, theater_ratio, 1, 0.55).
narrative_ontology:measurement(ss_rescue_theater_decade, sotu_1983_reagan_social_security_commission_rescue, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(ss_rescue_extr_baseline, sotu_1983_reagan_social_security_commission_rescue, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ss_rescue_extr_immediate, sotu_1983_reagan_social_security_commission_rescue, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(ss_rescue_extr_decade, sotu_1983_reagan_social_security_commission_rescue, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1983_reagan_social_security_commission_rescue, resource_allocation).
narrative_ontology:affects_constraint(sotu_1983_reagan_social_security_commission_rescue, social_security_payroll_tax_ceiling_debate).
narrative_ontology:affects_constraint(sotu_1983_reagan_social_security_commission_rescue, government_employee_pension_integration_politics).
narrative_ontology:affects_constraint(sotu_1983_reagan_social_security_commission_rescue, means_testing_elderly_income_verification).

% DUAL FORMULATION NOTE:
% The 1983 rescue package can be decomposed into three structurally distinct constraints: (1) payroll tax increase (ε ≈ 0.42, snare for self-employed and young workers), (2) benefit adjustment through means-testing (ε ≈ 0.35, snare for higher-income beneficiaries), (3) government employee CSRS inclusion (ε ≈ 0.40, tangled rope for organized government workers). The unified constraint story aggregates these into a single distributional package, appropriate because they were negotiated as a bundle and cannot be independently evaluated without losing the coordination context. If granular analysis is needed, decompose into three linked stories with network.affects_constraints capturing the joint constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1983_reagan_social_security_commission_rescue, powerless, 0.95).
constraint_indexing:directionality_override(sotu_1983_reagan_social_security_commission_rescue, organized, 0.55).
constraint_indexing:directionality_override(sotu_1983_reagan_social_security_commission_rescue, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
