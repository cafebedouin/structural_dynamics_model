% ============================================================================
% CONSTRAINT STORY: sotu_1996_clinton_balanced_budget_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1996_clinton_balanced_budget_requirement, []).

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
 *   constraint_id: sotu_1996_clinton_balanced_budget_requirement
 *   human_readable: 7-Year Balanced Budget Mandate with Deficit Elimination and Modest Tax Cut (SOTU 1996)
 *   domain: fiscal_policy/institutional_economics
 *
 * SUMMARY:
 *   The 1996 Balanced Budget Mandate, enacted through the Balanced Budget Act
 *   of 1997, created a binding institutional constraint requiring the federal
 *   government to eliminate permanent deficit spending within 7 years through
 *   combined spending reductions and modest tax relief. The constraint forces
 *   a tradeoff between immediate spending and long-term debt service costs,
 *   benefiting future taxpayers and creditors while imposing extraction costs
 *   on agencies and program beneficiaries. This constraint exhibits all six
 *   DR types from different perspectives: it appears as an immutable fiscal
 *   law (mountain), a coordination mechanism solving a collective-action
 *   deficit problem (rope), a mixed coordination-extraction hybrid
 *   (tangled_rope), a performative political commitment (piton), a temporary
 *   fiscal correction (scaffold), and pure extraction for those dependent on
 *   reduced spending (snare). The constraint's extractiveness increased from
 *   0.35 to 0.58 over the 7-year interval as the baseline spending cuts
 *   deepened and supplemental cuts accumulated. Theater ratio remained low
 *   (0.32–0.42) because the deficit targets were measurable and objective,
 *   reducing performative cover-up relative to institutional constraints with
 *   vaguer success metrics.
 *
 * KEY AGENTS:
 *   - Future Taxpayers and Debt Market Actors: Primary beneficiaries (institutional/arbitrage) — gain from lower interest costs on Treasury debt and reduced future tax burdens as debt service declines
 *   - Federal Discretionary Spending Agencies: Primary victims (powerless/trapped) — face non-negotiable budget caps and must implement cuts; no exit option
 *   - Working Poor and Means-Tested Program Participants: Secondary victims (moderate/constrained) — experience both benefit (lower long-term interest rates) and cost (reduced program access); constrained by lack of alternatives
 *   - Congressional Budget Process and Fiscal Hawks: Organized agents (organized/constrained) — manage implementation; have agency in prioritizing cuts but constrained by political feasibility
 *   - Executive and Legislative Leadership: Institutional actors (powerful/mobile) — can claim credit for responsibility and exit through blame-shifting; experience constraint as politically beneficial theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating fiscal accounting identity as natural law rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1996_clinton_balanced_budget_requirement, 0.52).
domain_priors:suppression_score(sotu_1996_clinton_balanced_budget_requirement, 0.48).
domain_priors:theater_ratio(sotu_1996_clinton_balanced_budget_requirement, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1996_clinton_balanced_budget_requirement, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1996_clinton_balanced_budget_requirement, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1996_clinton_balanced_budget_requirement, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1996_clinton_balanced_budget_requirement, tangled_rope).
narrative_ontology:human_readable(sotu_1996_clinton_balanced_budget_requirement, "7-Year Balanced Budget Mandate with Deficit Elimination and Modest Tax Cut (SOTU 1996)").
narrative_ontology:topic_domain(sotu_1996_clinton_balanced_budget_requirement, "fiscal_policy/institutional_economics").

domain_priors:requires_active_enforcement(sotu_1996_clinton_balanced_budget_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1996_clinton_balanced_budget_requirement, future_taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1996_clinton_balanced_budget_requirement, debt_service_recipients).
narrative_ontology:constraint_beneficiary(sotu_1996_clinton_balanced_budget_requirement, institutional_credibility).
narrative_ontology:constraint_victim(sotu_1996_clinton_balanced_budget_requirement, discretionary_federal_spending).
narrative_ontology:constraint_victim(sotu_1996_clinton_balanced_budget_requirement, means_tested_programs).
narrative_ontology:constraint_victim(sotu_1996_clinton_balanced_budget_requirement, working_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCRETIONARY PROGRAM BENEFICIARIES (SNARE) — Federal employees, researchers dependent on NIH/NSF grants, infrastructure workers, and vulnerable populations relying on means-tested programs face non-negotiable spending cuts. No exit option; federal budget constraints apply uniformly. Full extraction borne by those dependent on government spending.
constraint_indexing:constraint_classification(sotu_1996_clinton_balanced_budget_requirement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING POOR AND SOCIAL PROGRAM PARTICIPANTS (TANGLED ROPE) — Experience both coordination benefit (deficit reduction lowers long-term interest rates, reducing mortgage and loan costs) and extraction (program cuts reduce immediate aid). Can exit some programs through employment but at high cost; constrained by lack of alternative safety nets. Mixed extraction-coordination dynamic.
constraint_indexing:constraint_classification(sotu_1996_clinton_balanced_budget_requirement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FUTURE TAXPAYERS AND DEBT MARKET ACTORS (ROPE) — Primary beneficiaries of deficit reduction. Institutional actors (pension funds, foreign governments holding Treasury bonds, future tax base) benefit from lower debt service obligations. Can arbitrage between Treasury bonds and alternative investments; experience the constraint as pure coordination of fiscal responsibility. Minimal extraction.
constraint_indexing:constraint_classification(sotu_1996_clinton_balanced_budget_requirement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL BUDGET PROCESS AND FISCAL MONITORS (SCAFFOLD) — Organized agents (CBO, fiscal hawks, budget committees) experience the mandate as a temporary coordination mechanism with built-in sunset: the balanced budget constraint forces prioritization for 7 years, after which the mechanism naturally expires. Modest theater (payroll tax collections, spending caps) but genuine enforcement function. Sunset structure embedded in the 7-year timeline.
constraint_indexing:constraint_classification(sotu_1996_clinton_balanced_budget_requirement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE AND LEGISLATIVE LEADERSHIP (PITON) — Presidents and Congressional leaders can claim credit for fiscal responsibility without bearing implementation costs personally. The constraint is performative for leadership: announcing deficit reduction commitment generates political capital regardless of actual implementation trajectory. Leadership has exits (blame bureaucrats for implementation failures, negotiate exemptions) but chooses to maintain the constraint's theater for political benefit.
constraint_indexing:constraint_classification(sotu_1996_clinton_balanced_budget_requirement, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FISCAL MATHEMATICS VIEW (MOUNTAIN) — From a civilizational perspective, the balanced budget imperative emerges as a natural law: the government cannot spend more than it collects indefinitely without creating fiscal crisis, currency devaluation, or debt spiral. This perspective treats the constraint as mathematically inevitable. However, the structural data contradicts the mountain classification — identifiable beneficiaries exist, enforcement is institutional rather than emergent, and the 7-year timeline is policy-chosen rather than physics-derived. Engine false-summit detection should flag this as naturalization of a contingent institutional choice.
constraint_indexing:constraint_classification(sotu_1996_clinton_balanced_budget_requirement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1996_clinton_balanced_budget_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1996_clinton_balanced_budget_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1996_clinton_balanced_budget_requirement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1996_clinton_balanced_budget_requirement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1996_clinton_balanced_budget_requirement, TR),
    TR >= 0.70.

:- end_tests(sotu_1996_clinton_balanced_budget_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint accumulates extraction over time as deadline approaches and baseline cuts deepen. Initial extractiveness (0.35) reflects modest tax relief and early spending adjustments. By year 7, extractiveness reaches 0.58 as supplemental cuts target programs with high political salience but low enforcement capacity (welfare reform, NIH funding, infrastructure maintenance). The rising trajectory indicates that the constraint functions less as balanced coordination and more as a ratcheting extraction mechanism — each year's success enables tighter constraints in the next year. Suppression (0.48): Moderate. Federal agencies and program recipients face real constraints on exit options — they cannot opt out of budget cuts — but suppression is not total. Congress can pass supplemental appropriations, agencies can shift funds between line items, and political pressure can force exemptions. The restraint is institutional rather than absolute. Theater ratio (0.38): Low-moderate. Deficit measurements are objective and verifiable (CBO estimates), reducing performative cover-up compared to more subjective institutional constraints. However, theater emerges in the timing of counting methodologies (off-budget accounts, Social Security trust fund status) and in the announcement rhetoric relative to actual implementation difficulty.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is primarily distributional rather than perceptual. All perspectives agree that the deficit exists and must be addressed; they disagree on who bears the adjustment cost. Future taxpayers experience rope (pure coordination benefit). Working poor experience tangled rope (mixed extraction and benefit). Federal employees experience snare (pure extraction). Leadership experiences piton (low personal cost, high political benefit). The analytical observer risks mountain classification by treating deficit arithmetic as natural law, but the structural data contradicts this: the balanced budget requirement is a policy choice with identifiable beneficiaries (future creditors) and victims (current program beneficiaries). The constraint is institutional, not emergent from physics or mathematics.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from the base extractiveness (ε), the agent's directionality value (d reflecting their structural position), and the scope modifier (σ=1.0 for national scope). Primary beneficiaries (future taxpayers, institutional creditors) have low d values because they benefit from deficit reduction; χ for these agents is dampened (partially negative), they perceive rope or coordination benefit. Victims dependent on discretionary spending have high d values (they bear full extraction cost); χ for these agents is amplified, producing snare perception. Organized agents with political agency have moderate d values; they perceive tangled rope (mixed cost and ability to influence cuts). Leadership with mobility have very low d values relative to extraction; they perceive piton (performative burden). The deficit itself is structural — the government collects less than it spends — so the constraint cannot be evaded; only the distribution of adjustment burden can be negotiated.
 *
 * MANDATROPHY ANALYSIS:
 *   The balanced budget constraint resolves the mandatrophy through temporal decomposition: in the short term (immediate/biographical horizon), it is an extraction mechanism (Snare) for those dependent on federal spending; in the medium term (generational horizon), it is a coordination mechanism (Rope) for those benefiting from lower debt service; in the long term (civilizational horizon), it is either a natural law (Mountain — if the deficit was truly unsustainable) or a false summit (if lower-deficit alternatives were viable). The constraint's extractiveness (0.52) is above the pure-coordination threshold (0.35) and below the pure-extraction threshold (0.66), placing it correctly as Tangled Rope: it provides genuine coordination benefit (deficit reduction lowers interest costs) alongside asymmetric extraction (benefits concentrated on future creditors, costs concentrated on current program beneficiaries). The false-summit candidate evaluation: does the constraint serve identifiable beneficiaries (future taxpayers, creditors)? Yes. Can these beneficiaries be replaced by alternative beneficiaries if the constraint is removed? Yes — if the deficit were allowed to grow, current program beneficiaries would be the beneficiaries. This confirms Tangled Rope over Mountain: the constraint is institutional choice, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deficit_necessity_empirical,
    'Is persistent federal deficit spending structurally necessary for full employment and economic growth, or can balanced budgets be achieved without unacceptable employment costs?',
    'Comparison of employment trajectories and GDP growth in balanced-budget vs deficit-spending economies; econometric analysis of Okun''s law and fiscal multiplier estimates for 1990s fiscal consolidation',
    'If deficit necessary: the balanced budget constraint is extractive from employment (Snare becomes dominant). If not necessary: constraint is coordination mechanism (Rope becomes dominant). This determines whether the mandate is destructive or optimal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deficit_necessity_empirical, empirical, 'Whether persistent deficits are structurally necessary for employment').

omega_variable(
    distribution_of_burden,
    'Do spending cuts fall equally across income groups, or are they concentrated on means-tested and discretionary programs serving lower-income beneficiaries?',
    'Distributional analysis of actual budget cuts 1996-2002; income-weighted incidence analysis comparing tax and transfer impacts across deciles',
    'If equally distributed: constraint is symmetric coordination (Rope). If concentrated on low-income programs: constraint is regressive extraction mechanism (Snare becomes dominant perspective for affected groups).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distribution_of_burden, empirical, 'Distribution of spending cuts across income groups').

omega_variable(
    credibility_premium_magnitude,
    'How much does the balanced budget commitment reduce federal borrowing costs relative to counterfactual deficit-spending trajectory?',
    'Treasury bond yield analysis; estimation of interest-rate savings attributable to credibility premium from fiscal consolidation announcements vs actual deficit reduction',
    'If premium > 1% of GDP: coordination benefit is substantial (Rope strengthened). If premium < 0.2% of GDP: benefit is marginal relative to distribution costs (Tangled Rope weakens to Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credibility_premium_magnitude, empirical, 'Magnitude of borrowing cost reduction from balanced budget credibility').

omega_variable(
    temporal_irreversibility,
    'Are the spending cuts reversible after the 7-year period, or do they create institutional path dependence that persists even if the deficit target is met?',
    'Historical analysis of federal spending patterns post-2002; tracking of program discontinuations vs temporary suspensions; institutional memory loss in eliminated offices',
    'If reversible: Scaffold classification is accurate (true sunset). If irreversible: the 7-year frame is theater masking permanent extraction (constraint reclassifies to Snare or Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_irreversibility, empirical, 'Reversibility of spending cuts after 7-year period').

omega_variable(
    growth_rate_elasticity,
    'Does the balanced budget constraint reduce economic growth sufficiently to increase debt-to-GDP ratio despite nominal deficit elimination?',
    'Counterfactual simulation of GDP growth under alternative fiscal scenarios; econometric estimation of fiscal multipliers for 1996-2002 period; debt-to-GDP trajectory analysis',
    'If growth reduction is severe: constraint defeats its own purpose (debt ratio rises due to denominator collapse) — reclassifies to Snare extracting from future GDP growth. If growth impact is modest: coordination logic strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_rate_elasticity, empirical, 'Whether balanced budget reduces growth enough to worsen debt-to-GDP ratio').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1996_clinton_balanced_budget_requirement, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bb96_tr_t0, sotu_1996_clinton_balanced_budget_requirement, theater_ratio, 0, 0.32).
narrative_ontology:measurement(bb96_tr_t2, sotu_1996_clinton_balanced_budget_requirement, theater_ratio, 2, 0.35).
narrative_ontology:measurement(bb96_tr_t4, sotu_1996_clinton_balanced_budget_requirement, theater_ratio, 4, 0.38).
narrative_ontology:measurement(bb96_tr_t7, sotu_1996_clinton_balanced_budget_requirement, theater_ratio, 7, 0.42).

% Extraction over time
narrative_ontology:measurement(bb96_be_t0, sotu_1996_clinton_balanced_budget_requirement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bb96_be_t2, sotu_1996_clinton_balanced_budget_requirement, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(bb96_be_t4, sotu_1996_clinton_balanced_budget_requirement, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(bb96_be_t7, sotu_1996_clinton_balanced_budget_requirement, base_extractiveness, 7, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1996_clinton_balanced_budget_requirement, resource_allocation).
narrative_ontology:affects_constraint(sotu_1996_clinton_balanced_budget_requirement, social_security_trust_fund_depletion).
narrative_ontology:affects_constraint(sotu_1996_clinton_balanced_budget_requirement, medicare_solvency_crisis).
narrative_ontology:affects_constraint(sotu_1996_clinton_balanced_budget_requirement, declining_federal_research_investment).

% DUAL FORMULATION NOTE:
% The balanced budget constraint is the upstream institutional mechanism affecting downstream fiscal crises. Social Security trust fund depletion and Medicare solvency reflect long-term consequences of deficit reduction prioritizing discretionary spending cuts over entitlement reform. Federal research investment decline is a direct consequence of the 7-year spending caps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1996_clinton_balanced_budget_requirement, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
