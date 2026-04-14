% ============================================================================
% CONSTRAINT STORY: sotu_1979_carter_voluntary_wage_price_restraint_compact
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1979_carter_voluntary_wage_price_restraint_compact, []).

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
 *   constraint_id: sotu_1979_carter_voluntary_wage_price_restraint_compact
 *   human_readable: Carter's Voluntary Wage-Price Restraint Compact (1979)
 *   domain: economic_policy/inflation_control
 *
 * SUMMARY:
 *   President Carter's 1979 voluntary wage-price restraint compact represents
 *   a deliberate institutional choice to address stagflation through
 *   tripartite voluntary coordination rather than mandatory controls
 *   ('unworkable' wage-price freezes). The compact asks government to reduce
 *   deficits, business to moderate prices, and labor to restrain wages,
 *   framing all three commitments as equal sacrifice for inflation reduction.
 *   In structural reality, the compact is a tangled rope—genuine coordination
 *   function exists (reducing wage-price spiral benefits the economy as a
 *   whole), but the extraction mechanism is asymmetric: wage earners and
 *   business profit margins bear concentrated costs while fixed-income
 *   earners and creditors bear concentrated benefits. The framework relies on
 *   voluntary compliance and patriotic commitment; suppression operates
 *   through social pressure, government leverage (threat of mandatory
 *   controls), and coordination on wage discipline rather than through legal
 *   coercion. The theater ratio rises over the 24-month measurement interval
 *   (0.55 to 0.78) as the compact's performative elements (public commitment
 *   ceremonies, patriotic framing, repeated pledges) accumulate while actual
 *   inflation remains sticky—indicating Piton degradation dynamics.
 *   Extractiveness rises from 0.35 to 0.72 as the gap between promised
 *   restraint and actual inflation widens, with labor bearing the visible
 *   cost through constrained wages while government and business find ways to
 *   exit (government deficit increases, business raises prices selectively).
 *   This constraint demonstrates how voluntary frameworks can become
 *   extractive mechanisms when participation is coercively coordinated
 *   through patriotic narrative and asymmetric pressure.
 *
 * KEY AGENTS:
 *   - Wage Earners: Primary victims (powerless/trapped) — face suppressed nominal wage growth while inflation erodes purchasing power; cannot exit labor market; bear visible extraction
 *   - Labor Organizations: Organized victims (organized/constrained) — formally voluntary commitment; retain bargaining power in principle but face patriotic framing pressure and member discipline; constrained exit due to union leverage concerns
 *   - Business Sector: Mixed beneficiary/victim (institutional/constrained) — benefits from wage restraint (reduced labor costs) but constrained by price ceiling commitments; can exit by raising prices selectively but faces government reputational pressure
 *   - Federal Government: Primary beneficiary/architect (institutional/arbitrage) — frames compact, controls communication, benefits from inflation reduction and appearance of economic competence; can arbitrage sectoral participation
 *   - Fixed-Income Earners & Savers: Primary beneficiaries (powerful/mobile) — direct benefit from inflation reduction through preserved purchasing power; minimal suppression; experience pure coordination
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing wage-price spiral as immutable law rather than contingent feature of specific institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1979_carter_voluntary_wage_price_restraint_compact, 0.58).
domain_priors:suppression_score(sotu_1979_carter_voluntary_wage_price_restraint_compact, 0.48).
domain_priors:theater_ratio(sotu_1979_carter_voluntary_wage_price_restraint_compact, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1979_carter_voluntary_wage_price_restraint_compact, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1979_carter_voluntary_wage_price_restraint_compact, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1979_carter_voluntary_wage_price_restraint_compact, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1979_carter_voluntary_wage_price_restraint_compact, tangled_rope).
narrative_ontology:human_readable(sotu_1979_carter_voluntary_wage_price_restraint_compact, "Carter's Voluntary Wage-Price Restraint Compact (1979)").
narrative_ontology:topic_domain(sotu_1979_carter_voluntary_wage_price_restraint_compact, "economic_policy/inflation_control").

domain_priors:requires_active_enforcement(sotu_1979_carter_voluntary_wage_price_restraint_compact).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1979_carter_voluntary_wage_price_restraint_compact, fixed_income_earners).
narrative_ontology:constraint_beneficiary(sotu_1979_carter_voluntary_wage_price_restraint_compact, savers).
narrative_ontology:constraint_beneficiary(sotu_1979_carter_voluntary_wage_price_restraint_compact, creditors).
narrative_ontology:constraint_victim(sotu_1979_carter_voluntary_wage_price_restraint_compact, wage_earners).
narrative_ontology:constraint_victim(sotu_1979_carter_voluntary_wage_price_restraint_compact, business_profit_margins).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped by labor market conditions and inflation dynamics. Nominal wage growth is constrained by the compact while real purchasing power erodes. Cannot exit the labor market; faces suppression through employer coordination on wage discipline and social pressure framing restraint as patriotic duty. The compact extracts real income.
constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR ORGANIZATIONS (TANGLED ROPE) — Formally voluntary commitment to wage restraint; unions retain bargaining power in principle but face suppression through patriotic framing, government leverage (regulatory threats), and member pressure to 'do their part.' Coordination function exists (reducing runaway wage-price spiral benefits labor as collective), but extraction mechanism is present (leadership bears reputational cost of striking, members bear real income loss). Constrained exit—can leave, but at high cost to union leverage.
constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS SECTOR (TANGLED ROPE) — Voluntary price restraint commitment; coordination function exists (reduced input costs from wage restraint, predictability from government deficit reduction). But extraction mechanism is present (profit margins compressed by price ceiling; forced to absorb wage inflation differential if labor restraint fails unilaterally). Exit is constrained—firms can raise prices, but face public reputational cost and potential government pressure (threat of wage-price controls if compact fails).
constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (ROPE) — Primary architect and orchestrator of the compact. Benefits through restored price stability and perceived economic competence. Experiences the framework as pure coordination—the mechanism by which government signals commitment to fiscal discipline (deficit reduction) in exchange for private sector restraint. Can arbitrage by negotiating sectoral participation and managing the public commitment narrative. Low experienced extraction.
constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FIXED-INCOME EARNERS, SAVERS, CREDITORS (ROPE) — Primary beneficiaries of inflation reduction. Coordination benefit is direct and substantial: lower inflation preserves purchasing power of fixed incomes and savings, increases real returns on debt (reduces effective debt burden). Experience the constraint as pure coordination—it solves the inflation problem that harms them. Mobile exit options (portfolio shifts, savings vehicles) but the constraint removes the need to exercise them. Net benefit.
constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC TRUST / DEMOCRATIC LEGITIMACY (SCAFFOLD) — The compact is a temporary framework relying on voluntary compliance; its sunset is built into its structure—either inflation declines (compact succeeds, sunset by demonstrated efficacy) or it fails and formal controls become necessary (sunset via replacement). Theater ratio is high: the compact is substantially performative commitment and patriotic rhetoric. But the framework explicitly positions itself as temporary alternative to mandatory controls, giving it genuine sunset logic. Constrained exit through erosion of public belief in voluntarism.
constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, wage-price spirals are presented as immutable properties of modern economies with full employment and money supply growth—no institutional arrangement can prevent them without coercion. The compact appears as a futile effort to repeal natural economic law. However, this is a false summit: post-war economies achieved stable inflation without wage-price compacts through different institutional mechanisms (capital controls, monetary restraint, labor corporatism in Europe). The 'natural law' framing naturalizes a specific institutional configuration (US 1970s labor relations + Fed independence + global capital mobility).
constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1979_carter_voluntary_wage_price_restraint_compact_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1979_carter_voluntary_wage_price_restraint_compact, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1979_carter_voluntary_wage_price_restraint_compact, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1979_carter_voluntary_wage_price_restraint_compact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. Initial extractiveness is low (0.35) because the compact is genuinely framed as tripartite sacrifice—all three sectors pledge restraint. But extractiveness rises to 0.58-0.72 as asymmetry emerges: labor restrain wages, business raises prices selectively (food, energy, non-competed sectors), and government deficits increase. The gap between voluntary commitment and actual performance is the extraction mechanism. Suppression (0.48): Moderate. Coercive elements are primarily social and narrative rather than legal: patriotic framing, public pledges (high audience cost for breaking commitment), government threat to impose mandatory controls if voluntary framework fails, and labor leadership pressure on members. Material barriers to exit exist (workers can't easily leave labor market, unions can't break deals without reputational damage) but are not total. Business can exit by raising prices; government can exit by increasing spending. Theater ratio (0.65, rising to 0.78): Substantial performative element. The compact relies on public ceremony and repeated commitment signals; actual inflation control mechanisms are weak (no enforcement structure, no penalty for breaking pledges beyond reputational). The rise in theater over 24 months indicates Piton degradation—the framework becomes increasingly performative as actual inflation persists despite professed restraint. Claimed type (Tangled Rope): Justified by presence of genuine coordination function (all three sectors would benefit from successfully halting wage-price spiral) alongside asymmetric extraction (fixed-income earners benefit, wage earners lose, business margin compressed). Requires active enforcement (government pressure on participants) despite formal voluntarism.
 *
 * PERSPECTIVAL GAP:
 *   The wage earner sees pure extraction (Snare): they restrain wages while inflation erodes real income, with no reciprocal benefit. They have no exit; their only lever is striking, which violates the compact and invites patriotic condemnation. Labor organizations see hybrid coordination-extraction (Tangled Rope): restrain wages reduces labor cost inflation, benefits labor as collective, but individual members bear the cost and leadership bears reputational risk if they strike. Business sees constrained tangled rope: benefit from wage restraint but constrained by price commitments; can exit by raising prices on non-competed goods but faces government pressure. Government sees pure coordination (Rope): orchestrates the deal, benefits from inflation reduction and economic credibility, has arbitrage power over sectoral terms. Fixed-income earners see pure coordination (Rope): direct benefit from inflation reduction with zero cost. The analytical observer risks seeing a mountain (immutable wage-price spiral) but structural data reveals this as false summit—wage-price spirals are contingent on specific labor relations and monetary institutions, not laws of nature. The perspectival gaps reflect real structural differences in extraction flow: from wage earners to creditors, with business as intermediary and government as orchestrator.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from beneficiary/victim status and exit options. Wage earners are victims with trapped exit → high d → high f(d) → high experienced extractiveness (snare experience). Labor organizations are victims with constrained exit (can strike but at high cost) → moderate-high d → moderate-high f(d) → tangled rope experience. Business is mixed (benefits from wage restraint, victim of price ceiling) with constrained exit → d around 0.55 → moderate experienced extractiveness. Government is beneficiary with arbitrage exit (controls participation) → low d → low f(d) → rope experience. Fixed-income earners are beneficiaries with mobile exit (can shift portfolios) → very low d → negative f(d) → rope/institutional experience. The directionality chain captures why different sectors experience the same institutional framework as different constraint types: their structural position relative to extraction flow is different, even though the formal commitments are identical.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled rope classification resolves the apparent paradox that the compact is simultaneously described as 'voluntary coordination' (rope logic) and 'extraction mechanism' (snare logic). The resolution is that both descriptions are structurally true from different positions: from the government and fixed-income perspective, it is pure coordination (voluntary mechanism to reduce a shared problem). From the wage-earner perspective, it is pure extraction (forced restraint with no reciprocal benefit). From the business and labor leadership perspective, it is tangled rope—genuine coordination benefit (stabilizing inflation reduces uncertainty) coupled with asymmetric extraction (wage earners and profit margins bear costs, fixed-income earners bear benefits). The mandatrophy would arise if we tried to assign a single type to the constraint independent of perspective. The resolution is perspectival: the constraint IS tangled rope at the analytical level (has both coordination function and extraction mechanism); different agents experience different portions of the structure depending on their position. The framework prevents the false simplification (either pure coordination or pure extraction) by forcing explicit declaration of beneficiaries (fixed-income earners, savers, creditors) and victims (wage earners, business profit margins) alongside the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coerced_suppression,
    'Is the measured suppression (0.48) structural coercion or internalized patriotic commitment that collapses when institutional pressure is removed?',
    'Post-compact empirical tracking: Do wage negotiations revert to pre-compact patterns when government announces compact failure or withdrawal? Does union wage-setting behavior show persistent restraint after formal compact ends, or return to previous trajectories?',
    'If structural: suppression persists, classification stands. If internalized: suppression is temporary theater, reducing effective extraction and reclassifying some perspectives toward rope. Affects Piton candidacy for the overall framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_suppression, empirical, 'Whether suppression is structural coercion or internalized commitment').

omega_variable(
    asymmetry_of_voluntary_restraint,
    'Which sector (labor, business, government) is most likely to unilaterally break the compact, and what triggers exit?',
    'Historical observation: track inflation trajectory, labor wage settlements, business pricing behavior, and government deficit changes month-by-month. Identify which sector deviates first and under what conditions (recession triggers, inflation acceleration, leadership change).',
    'If labor exits first: snare classification for wage earners confirmed, compact is extraction mechanism masquerading as coordination. If business exits: tangled rope classification confirmed, business uses compact to discipline labor then breaks it. If government exits: scaffold reclassifies to piton (performative control framework).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetry_of_voluntary_restraint, empirical, 'Sectoral asymmetry in exit likelihood and triggers').

omega_variable(
    coordination_vs_cover_story,
    'Does the compact''s coordination function (reducing wage-price spiral) represent genuine mutual benefit, or is it primarily a cover story for extracting wage concessions?',
    'Counterfactual analysis: Model inflation trajectory under three scenarios—(1) no compact (baseline wage-price spiral), (2) compact with full compliance, (3) compact with labor compliance but business/government defection. Compare outcomes to actual 1979-1981 inflation. If scenario 2 produces materially lower inflation than 1, coordination function is real. If outcomes are similar, coordination was cover story.',
    'If genuine coordination: tangled rope classification is stable across perspectives. If cover story: reclassify all institutional perspectives to snare (using compact to extract concessions), leaving only government at rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_cover_story, empirical, 'Whether coordination function is genuine or cover story for extraction').

omega_variable(
    inflation_causality_attribution,
    'What are the true causal drivers of 1970s inflation—wage-price spiral, money supply growth, supply shocks, expectations formation, or some combination—and how much of the problem is addressable through voluntary wage-price restraint?',
    'Econometric decomposition: Vector autoregression analysis of wage, price, money supply, and supply shock variables in 1970s data. Estimate variance contribution of each. Compare to theoretical wage-price spiral models to assess how much restraint would reduce inflation.',
    'If wage-price spiral is dominant (>40% of variance): compact targets the right mechanism, coordination logic is sound. If supply shocks or money growth dominant (>50%): compact targets wrong mechanism, it is theater (Piton reclassification). If mixed: compact addresses partial problem, tangled rope classification is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_causality_attribution, empirical, 'True causal drivers of 1970s inflation').

omega_variable(
    government_fiscal_commitment_credibility,
    'Is the government''s commitment to deficit reduction and fiscal restraint credible, or does the compact ask labor and business to restrain while government backslides on its obligation?',
    'Budget deficit tracking: Compare government spending and revenue changes in 1979-1981 against pre-compact baseline and against public deficit reduction targets. Identify timing and magnitude of any deviations. Assess whether labor/business could have observed government fiscal slippage during their compliance period.',
    'If government deficit remains controlled: government is binding participant, tangled rope classification stable. If government deficits increase despite compact: compact is one-way extraction of labor/business concessions, reclassifies to snare for both labor and business sectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_fiscal_commitment_credibility, empirical, 'Credibility of government fiscal restraint commitment').

omega_variable(
    false_summit_natural_law_candidate,
    'Is the wage-price spiral presented as an immutable natural law inherent to full-employment economies, or is it a contingent feature of specific institutional arrangements (labor market structures, monetary policy, capital mobility) that could be changed?',
    'Comparative institutional analysis: Examine inflation dynamics in other OECD countries with similar full-employment conditions but different labor relations institutions (European corporatism, Japanese enterprise unions). Assess whether wage-price spirals appeared in those contexts despite similar macro conditions.',
    'If spirals are universal: natural law framing is justified, mountain perspective is accurate. If spirals are institution-specific: mountain classification is false summit, naturalizing a specific US institutional configuration. This affects the analytical observer perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'Whether wage-price spiral is natural law or contingent institutional feature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1979_carter_voluntary_wage_price_restraint_compact, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carter79_tr_t0, sotu_1979_carter_voluntary_wage_price_restraint_compact, theater_ratio, 0, 0.55).
narrative_ontology:measurement(carter79_tr_t6, sotu_1979_carter_voluntary_wage_price_restraint_compact, theater_ratio, 6, 0.62).
narrative_ontology:measurement(carter79_tr_t12, sotu_1979_carter_voluntary_wage_price_restraint_compact, theater_ratio, 12, 0.65).
narrative_ontology:measurement(carter79_tr_t24, sotu_1979_carter_voluntary_wage_price_restraint_compact, theater_ratio, 24, 0.78).

% Extraction over time
narrative_ontology:measurement(carter79_be_t0, sotu_1979_carter_voluntary_wage_price_restraint_compact, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(carter79_be_t6, sotu_1979_carter_voluntary_wage_price_restraint_compact, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(carter79_be_t12, sotu_1979_carter_voluntary_wage_price_restraint_compact, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(carter79_be_t24, sotu_1979_carter_voluntary_wage_price_restraint_compact, base_extractiveness, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1979_carter_voluntary_wage_price_restraint_compact, resource_allocation).
narrative_ontology:affects_constraint(sotu_1979_carter_voluntary_wage_price_restraint_compact, stagflation_monetary_policy_constraint).
narrative_ontology:affects_constraint(sotu_1979_carter_voluntary_wage_price_restraint_compact, labor_relations_capital_asymmetry).

% DUAL FORMULATION NOTE:
% The voluntary compact is a distinct constraint from the underlying wage-price spiral it attempts to manage. The spiral has different ε (likely 0.40-0.50, depending on monetary accommodation). The compact has ε=0.58, reflecting the extractive mechanism built into the voluntary framework itself (asymmetric participation pressure). The two constraints form a family where the compact is downstream institutional response to the spiral; both should be modeled in full for complete constraint mapping.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1979_carter_voluntary_wage_price_restraint_compact, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
