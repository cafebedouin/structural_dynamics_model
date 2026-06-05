% ============================================================================
% CONSTRAINT STORY: sotu_1963_kennedy_progressive_income_tax_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1963_kennedy_progressive_income_tax_reduction, []).

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
 *   constraint_id: sotu_1963_kennedy_progressive_income_tax_reduction
 *   human_readable: 1963 Kennedy Progressive Income Tax Reduction (14-65% brackets)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   The 1963 Kennedy proposal for progressive income tax reduction (from
 *   20-91% to 14-65% brackets over 3 years) represents a critical pivot point
 *   in US fiscal policy: a shift from countercyclical spending-based
 *   Keynesianism to demand-side stimulus through tax reduction. The
 *   constraint exhibits structural tension between two coordination functions
 *   that operate in opposite directions: (1) the stimulus mechanism (removing
 *   perceived distortions from obsolete high progressive rates) that benefits
 *   private capital and middle-income consumers, and (2) the extraction
 *   mechanism (reducing federal revenue, constraining public investment
 *   capacity) that harms those dependent on federal programs and future
 *   generations dependent on public capital. This makes it a canonical
 *   Tangled Rope: genuine coordination function (aligning tax policy with
 *   growth incentives) coupled with asymmetric extraction (benefits
 *   concentrated in upper-income and business sectors, costs borne by those
 *   dependent on federal services and public investment). The constraint
 *   exhibits increasing extractiveness over the 3-year phase-in (0.22 → 0.38)
 *   as the gap between promised stimulus effects and actual economic results
 *   widens. The low theater ratio (0.25-0.35) reflects that the mechanism is
 *   structurally straightforward: reduce rates, increase private purchasing
 *   power and investment incentives. Unlike institutional theater (peer
 *   review, regulatory certification), fiscal stimulus is mechanically direct
 *   — the performative content is in the political framing ('obsolete tax
 *   system,' 'growth imperative') rather than in the procedure itself.
 *
 * KEY AGENTS:
 *   - Low-income consumers: Primary intended beneficiary (moderate/constrained) — direct tax reduction increases purchasing power but depends on employment opportunity; extraction risk if public programs shrink
 *   - Business investors: Primary structural beneficiary (institutional/arbitrage) — lower tax rates increase retained earnings and investment returns; experiences constraint as solution to coordination problem
 *   - Federal government: Primary initiator/coordinator (institutional/arbitrage) — voluntarily implements constraint believing it solves growth problem; benefits if stimulus validates predictions
 *   - State and local governments: Secondary victim (moderate/constrained) — federal revenue-sharing declines, forcing difficult choices between tax increases and service cuts; constrained by federal dynamics but not trapped
 *   - Public investment (infrastructure, education, R&D): Structural victim (powerless/trapped) — federal spending on capital formation declines as revenues shrink; no agency to exit the constraint; bears extraction
 *   - Unemployed and marginalized workers: Tertiary victim (powerless/trapped) — tax reduction targets those with income; public safety net shrinks as federal revenues decline; no exit from dependence
 *   - Future generations: Generational victim (powerless/trapped) — reduced public investment in education, infrastructure, research capacity defers growth potential; bear costs of underinvestment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1963_kennedy_progressive_income_tax_reduction, 0.38).
domain_priors:suppression_score(sotu_1963_kennedy_progressive_income_tax_reduction, 0.25).
domain_priors:theater_ratio(sotu_1963_kennedy_progressive_income_tax_reduction, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1963_kennedy_progressive_income_tax_reduction, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1963_kennedy_progressive_income_tax_reduction, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sotu_1963_kennedy_progressive_income_tax_reduction, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1963_kennedy_progressive_income_tax_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1963_kennedy_progressive_income_tax_reduction, "1963 Kennedy Progressive Income Tax Reduction (14-65% brackets)").
narrative_ontology:topic_domain(sotu_1963_kennedy_progressive_income_tax_reduction, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1963_kennedy_progressive_income_tax_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1963_kennedy_progressive_income_tax_reduction, low_income_consumers).
narrative_ontology:constraint_beneficiary(sotu_1963_kennedy_progressive_income_tax_reduction, business_investors).
narrative_ontology:constraint_beneficiary(sotu_1963_kennedy_progressive_income_tax_reduction, aggregate_private_sector).
narrative_ontology:constraint_victim(sotu_1963_kennedy_progressive_income_tax_reduction, federal_government_revenue).
narrative_ontology:constraint_victim(sotu_1963_kennedy_progressive_income_tax_reduction, public_investment_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNEMPLOYED/MARGINALIZED WORKER (SNARE) — Trapped in dependence on public employment programs and welfare infrastructure that shrink as federal revenue declines. Tax reduction targets consumer purchasing power but does not reach those without income. The constraint extracts from the bottom by defunding the public safety net that serves them. No exit from dependency; no benefit from the stimulus if they lack employment.
constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME SALARIED WORKER (TANGLED ROPE) — Benefits directly from tax reduction in their bracket (e.g., 42% down to 32%), increasing take-home pay. Constrained by labor market and employer dependency. Also bears extraction risk if private-sector stimulus fails to materialize — the promised growth may not offset federal program cuts that affect their community (schools, infrastructure). Mixed experience: genuine benefit from tax cut but coupled with systemic extraction through reduced public investment.
constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS CORPORATION (ROPE) — Net beneficiary of the constraint. Lower tax rates increase after-tax profits and retained earnings for capital investment. The mechanism is pure coordination: the constraint removes what business sees as a distortion (high progressive rates) and enables private capital allocation. Corporations have arbitrage options — capital can flow to lower-tax jurisdictions, higher-return sectors. The constraint aligns their preferences directly with policy. Experiences the policy as solving a genuine coordination problem: obsolete tax rates have depressed investment incentives.
constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL FISCAL AUTHORITY (ROPE) — Experiences the tax reduction as solving a coordination problem: the legacy tax system created perverse incentives (high nominal rates encouraging avoidance, real rates uneven across brackets). The constraint resets tax policy toward transparency and growth-aligned rates. The authority voluntarily initiates the constraint and benefits from the anticipated virtuous cycle: lower rates → higher compliance and economic activity → restored revenue and legitimacy. No coercion applied to the authority; it is the primary agent choosing this coordination mechanism.
constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE/LOCAL GOVERNMENT (TANGLED ROPE) — Constrained by federal dynamics but not trapped. Depends on federal revenue-sharing and intergovernmental transfers that depend on federal revenue. If private-sector stimulus fails, state/local entities face revenue shortfalls. But they also benefit from broader economic growth if the stimulus works. The constraint enforces a coordination function (aligning incentives) but extracts through uncertainty and reduced federal support during the transition phase. Exit options exist (state tax increases, revenue alternatives) but are politically constrained.
constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TAX POLICY ORTHODOXY (PITON) — The constraint degrades an older institutional arrangement: high progressive tax rates and their associated complexity were the consensus tool for countercyclical fiscal policy and wealth redistribution. The Kennedy reduction represents the ascendance of a new orthodoxy (growth through private sector stimulus, Keynesian demand-side management via tax cuts rather than spending). The old rate structure persists in rhetoric (defending high marginal rates as moral) but loses functional force. Theater ratio reflects the gap between the formal principle of progressivity and the actual mechanisms of rate reduction — both are presented as compatible, but the rate reduction fundamentally shifts incentives away from the redistributive function.
constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: FISCAL STIMULUS COALITION (SCAFFOLD) — Organized coalition of economists, business groups, and growth-oriented policymakers see this tax reduction as a temporary coordination mechanism with a clear sunset: if the stimulus works and economic growth accelerates, the 3-year phase-in completes and new tax policies can be implemented with higher baseline revenue. The coalition has mobile options (different stimulus mechanisms, spending alternatives) and sees the tax cut as one of several possible approaches. High agency; low experienced extraction. The sunset clause is implicit: the reduction is meant to solve a discrete problem (sluggish growth in early 1960s) with a defined timeline.
constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some tension between revenue needs and growth incentives is inherent to taxation: all tax systems face a tradeoff between progressivity (social goals) and efficiency (growth incentives). This perspective risks naturalizing the specific policy choice (rate reduction) as a necessary law of economics. However, the structural data contradicts the mountain classification — multiple institutional beneficiaries exist, and the constraint is politically chosen, not physically inevitable. The engine will flag this as a false summit, revealing how 'economic necessity' cover stories naturalize contingent political choices.
constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1963_kennedy_progressive_income_tax_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1963_kennedy_progressive_income_tax_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1963_kennedy_progressive_income_tax_reduction, TR),
    TR >= 0.70.

:- end_tests(sotu_1963_kennedy_progressive_income_tax_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint operates as genuine stimulus mechanism (real reduction in effective tax burden for targeted groups) coupled with real extraction (federal revenue loss, public investment opportunity costs). The extractiveness is not high because the stimulus is legitimate — businesses and workers do benefit from the tax reduction. But it is substantial because the extraction vector points clearly: revenue is transferred from public investment capacity to private purchasing power, and if the stimulus fails to generate offsetting growth, the extraction becomes permanent. The trajectory from 0.22 to 0.38 reflects increasing extraction as the 3-year phase-in proceeds and the federal deficit accumulates without yet generating the promised economic acceleration. Suppression (0.25): Low-moderate. Suppression is limited because the constraint operates through market mechanisms (price signals in private consumption and investment) rather than coercion. Those benefiting (businesses, middle-income workers) experience it as choice enablement, not suppression. But suppression is present for those dependent on federal programs — the shrinking public investment capacity and potential program cuts represent real barriers to their alternatives. Theater ratio (0.35): Low. The fiscal mechanism is transparent and mechanically straightforward: reduce rates, increase take-home pay and retained earnings. The performative content is in the political framing ('obsolete tax system,' 'growth imperative,' 'private initiative versus government spending') rather than in the procedure. Both the claim (that this is good policy) and the mechanism (that rate reduction increases private purchasing power) are literally true — the theater is in what is omitted (the public investment costs, the inequality implications) rather than in what is asserted.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The federal fiscal authority and business sector see Rope (coordination solving the 'obsolete rate' problem). The business-aligned stimulus coalition sees Scaffold (temporary mechanism with sunset when growth validates). The unemployed and marginalized see Snare (public program shrinkage without compensating opportunity). The middle-income worker sees Tangled Rope (genuine tax benefit coupled to public service uncertainty). State and local governments see Tangled Rope (benefit from growth if it materializes, extraction from revenue cuts if it doesn't). The future generations perspective sees extraction at a civilizational time horizon — the public investment deferred will reduce long-term productive capacity, making this generation's growth an extraction from future potential. The analytical observer at the civilizational horizon risks seeing a Mountain (tension between revenue and growth as inherent to taxation), but the structural data reveals this as false summit — the specific choice to reduce rates is politically contingent, not physically inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from the beneficiary and victim declarations coupled with exit options. Business investors (beneficiary, arbitrage exit) derive low d (0.15-0.25) → negative f(d) → negative χ (they experience as benefit). Low-income consumers (beneficiary, constrained exit) derive moderate d (0.40-0.50) → moderate f(d) → moderate χ (genuine benefit but constrained by labor market). Federal public investment capacity (victim, trapped) derives high d (0.85-0.95) → high f(d) → high χ (maximum extraction — no options, bears full cost). Middle-income workers (both beneficiary and victim simultaneously) derive mixed d based on whether they are measured relative to the tax benefit (beneficiary side) or to public service cuts (victim side). The perspectival gap emerges because organized agents (businesses, fiscal authority) that initiate and control the constraint experience low extraction; powerless agents (dependent populations, future generations) that have no voice in the constraint experience high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy through empirical outcome data. If economic growth materializes (>3% annually), the stimulus is validated as genuine coordination (Rope), and the federal revenue recovery through growth offsets the initial deficit (Scaffold sunset thesis confirmed). If economic growth fails to materialize, the constraint degrades to Snare (extraction without coordination benefit), and mandatrophy emerges: the policy persists as deficit spending without the promised compensating mechanism. The critical empirical resolution is the federal revenue trajectory: if revenues recover by 1966, the constraint is validated as temporary coordination with self-correction. If revenues remain depressed, the constraint reveals itself as extraction disguised as stimulus. The omega variables document these resolution pathways explicitly: stimulus efficacy threshold, federal revenue recovery path, and incidence distribution across brackets are the core empirical tests. The 1963 proposal is mandatrophy-vulnerable because it makes a strong claim about behavioral response (growth will recover revenues) that can be falsified. If the claim fails, the constraint remains but is reclassified as snare (extraction without the coordination function that justifies it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stimulus_efficacy_threshold,
    'What magnitude of economic growth is required to validate the stimulus mechanism and offset federal revenue losses?',
    'Longitudinal GDP growth data (1963-1966), tax revenue tracking, multiplier effect measurement against counterfactual (no tax cut) estimates',
    'If GDP growth < 2.5% annually: stimulus fails, constraint reclassifies toward snare (extraction without coordination benefit). If GDP growth > 3.5%: stimulus succeeds, constraint validates as rope for most perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stimulus_efficacy_threshold, empirical, 'Economic growth threshold validating stimulus mechanism').

omega_variable(
    bracket_progressivity_retention,
    'Do the reduced brackets (14-65%) retain genuine progressivity or does the reduction amount to de facto proportional taxation masked by bracket rhetoric?',
    'Effective tax rate analysis by income quintile (1963 vs 1966); comparison of rate dispersion; inflation-adjusted marginal vs average rate trajectories',
    'If progressivity retained: constraint is genuine coordination with redistributive function preserved. If progressivity degraded: constraint reclassifies as extractive (snare from lower-income perspective), using stimulus language to mask regressive effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bracket_progressivity_retention, empirical, 'Whether reduced brackets preserve genuine progressivity').

omega_variable(
    federal_revenue_recovery_path,
    'Do federal revenues return to pre-reduction levels by 1966 (end of phase-in) via economic growth, or is the deficit structural?',
    'Federal receipts tracking, nominal vs real revenue comparison, growth-to-revenue ratio analysis (whether growth accelerates sufficiently to restore revenue)',
    'If recovered: virtuous cycle validates rope classification and scaffold sunset thesis. If structural deficit persists: constraints extraction capacity increases (higher χ), reclassifying toward snare, and the policy generates long-term mandatrophy (stimulus without self-correction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_revenue_recovery_path, empirical, 'Federal revenue recovery via economic growth').

omega_variable(
    incidence_distribution_across_brackets,
    'Which income brackets capture the largest share of the tax reduction in absolute dollars, and which capture the largest proportional benefit?',
    'Tax Revenue Act of 1964 implementation data, IRS Statistics of Income analysis, income distribution of tax benefits by quintile',
    'If high earners capture disproportionate absolute benefits: extraction vector points upward (snare). If benefits distributed proportionally: rope coordination validated. If low earners benefit proportionally more: progressive stimulus validated (rope with equity function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incidence_distribution_across_brackets, empirical, 'Distribution of tax reduction benefits across income brackets').

omega_variable(
    public_investment_crowding_out,
    'Does the reduction in federal revenues directly crowd out public investment (infrastructure, education, R&D) or does growth-generated revenue substitute?',
    'Federal spending by category (1960-1970); public capital formation rates; long-term productivity impacts attributable to public investment decline',
    'If crowding-out occurs: constraint extracts from future generations (public investment deferred, long-term growth potential reduced). Reclassifies toward snare at generational horizon. If growth-substitution succeeds: constraint validates as rope (temporary coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_investment_crowding_out, empirical, 'Crowding out of public investment by revenue reduction').

omega_variable(
    political_coalition_decomposition,
    'What distributional coalitions formed to support and oppose the tax reduction, and did the constraint achieve the predicted political equilibrium?',
    'Legislative voting analysis, interest group testimony, economic organization mobilization (chambers of commerce, labor unions, fiscal clubs); comparative analysis of predicted vs actual coalition strength',
    'If coalitions align as predicted: institutional coordination validates rope. If coalitions realign (unexpected opposition from business, unexpected support from labor): constraint reclassifies and mandatrophy emerges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_coalition_decomposition, empirical, 'Political coalition structure and alignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1963_kennedy_progressive_income_tax_reduction, 1963, 1966).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu1963_tr_t0, sotu_1963_kennedy_progressive_income_tax_reduction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sotu1963_tr_t1, sotu_1963_kennedy_progressive_income_tax_reduction, theater_ratio, 1, 0.3).
narrative_ontology:measurement(sotu1963_tr_t2, sotu_1963_kennedy_progressive_income_tax_reduction, theater_ratio, 2, 0.33).
narrative_ontology:measurement(sotu1963_tr_t3, sotu_1963_kennedy_progressive_income_tax_reduction, theater_ratio, 3, 0.35).

% Extraction over time
narrative_ontology:measurement(sotu1963_be_t0, sotu_1963_kennedy_progressive_income_tax_reduction, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sotu1963_be_t1, sotu_1963_kennedy_progressive_income_tax_reduction, base_extractiveness, 1, 0.28).
narrative_ontology:measurement(sotu1963_be_t2, sotu_1963_kennedy_progressive_income_tax_reduction, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(sotu1963_be_t3, sotu_1963_kennedy_progressive_income_tax_reduction, base_extractiveness, 3, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1963_kennedy_progressive_income_tax_reduction, resource_allocation).
narrative_ontology:affects_constraint(sotu_1963_kennedy_progressive_income_tax_reduction, federal_fiscal_sustainability_postwar).
narrative_ontology:affects_constraint(sotu_1963_kennedy_progressive_income_tax_reduction, wealth_inequality_tax_progressivity_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of postwar fiscal consensus (federal revenue needs vs. growth incentives). It affects downstream constraints on federal sustainability (whether revenues remain adequate for public investment and debt service) and on inequality (whether tax progressivity can be maintained as a redistributive mechanism). The ε value (0.38) reflects the specific policy choice; upstream constraints have different ε values reflecting the pre-tax-cut fiscal position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
