% ============================================================================
% CONSTRAINT STORY: sotu_1963_johnson_tax_bill_recession_insurance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1963_johnson_tax_bill_recession_insurance, []).

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
 *   constraint_id: sotu_1963_johnson_tax_bill_recession_insurance
 *   human_readable: Kennedy-Johnson Tax Bill (1963) as Recession Insurance Mechanism
 *   domain: macroeconomic_policy/fiscal_stimulus
 *
 * SUMMARY:
 *   The Kennedy-Johnson tax bill of 1963 represents a landmark application of
 *   Keynesian fiscal policy as recession insurance. President Johnson and his
 *   economic advisors defend the 10% corporate tax cut and individual rate
 *   reductions as mechanisms to stimulate private investment, increase
 *   aggregate demand, and create employment while simultaneously expanding
 *   federal revenues through growth-driven tax base expansion. The constraint
 *   exhibits Tangled Rope structure: it genuinely coordinates labor market
 *   expansion and business investment incentives while simultaneously
 *   extracting through higher tax burdens on the wealthiest taxpayers and
 *   creating suppression through ideological foreclosure (presenting this
 *   mechanism as the only viable countercyclical policy). The theater
 *   component reflects a pervasive mischaracterization: the stimulus effect
 *   derives primarily from the underlying deficit spending (Keynesian
 *   aggregate demand), not from the tax structure itself. The tax-cut framing
 *   provides political cover for deficit spending, which carries different
 *   ideological valences in 1963 than direct government spending would.
 *
 * KEY AGENTS:
 *   - Workers and Job-Seekers: Primary intended beneficiary (moderate/constrained) — gain employment from stimulus-driven growth but constrained by market conditions; bear inflationary costs
 *   - Higher-Income Taxpayers: Primary victim (powerless/trapped) — face involuntary extraction via higher effective tax rates with no exit mechanism; cannot escape national taxation
 *   - Business Sector: Primary beneficiary (institutional/arbitrage) — receives tax cuts on corporate rates and accelerated depreciation; gains capital retention and investment incentive; high exit optionality
 *   - Labor Unions (Organized Workers): Secondary beneficiary with constraints (organized/constrained) — benefit from tight labor markets but suppressed through reduced public-sector expansion and alternative policy foreclosure
 *   - Federal Reserve: Coordinating authority (powerful/mobile) — implements countercyclical monetary policy; retains autonomy to adjust; sees temporary coordination role with implicit sunset
 *   - Treasury Department: Institutional actor (institutional/arbitrage) — maintains performative function; benefits from tax-cut framing that masks deficit-spending stimulus; has exit through policy reversal
 *   - The Broader Economy: Abstract beneficiary (powerless/trapped) — the constraint's framing assumes economy needs recession insurance but forecloses debate over alternative mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1963_johnson_tax_bill_recession_insurance, 0.38).
domain_priors:suppression_score(sotu_1963_johnson_tax_bill_recession_insurance, 0.35).
domain_priors:theater_ratio(sotu_1963_johnson_tax_bill_recession_insurance, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1963_johnson_tax_bill_recession_insurance, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1963_johnson_tax_bill_recession_insurance, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1963_johnson_tax_bill_recession_insurance, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1963_johnson_tax_bill_recession_insurance, tangled_rope).
narrative_ontology:human_readable(sotu_1963_johnson_tax_bill_recession_insurance, "Kennedy-Johnson Tax Bill (1963) as Recession Insurance Mechanism").
narrative_ontology:topic_domain(sotu_1963_johnson_tax_bill_recession_insurance, "macroeconomic_policy/fiscal_stimulus").

domain_priors:requires_active_enforcement(sotu_1963_johnson_tax_bill_recession_insurance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_tax_bill_recession_insurance, workers_job_security).
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_tax_bill_recession_insurance, unemployed_new_employment).
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_tax_bill_recession_insurance, businesses_incentive_structures).
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_tax_bill_recession_insurance, federal_revenue_generation).
narrative_ontology:constraint_victim(sotu_1963_johnson_tax_bill_recession_insurance, higher_income_taxpayers).
narrative_ontology:constraint_victim(sotu_1963_johnson_tax_bill_recession_insurance, tax_burden_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-INCOME TAXPAYER (SNARE) — Faces involuntary extraction via tax rate increases with no exit mechanism. Cannot relocate abroad without citizenship loss; cannot avoid taxation without legal consequence. Suppression is structural (legal obligation, IRS enforcement). Experiences the tax bill as pure extraction with minimal coordination benefit to this agent class. Extraction is masked as growth stimulus, but the burden falls entirely on higher earners.
constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNEMPLOYED JOB-SEEKER (TANGLED ROPE) — Benefits from new employment opportunities created by stimulus-driven growth, but constrained by market conditions, skill requirements, and geographic mobility costs. The tax bill coordinates labor market expansion (genuine benefit) while extracting through inflationary pressures, reduced public spending in other domains, and opportunity cost. Mixed experience: job creation is real coordination; suppression of alternative policy options (direct job guarantee, public works) is extraction component.
constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS SECTOR (ROPE) — Primary beneficiary of tax cuts (accelerated depreciation, corporate rate reduction). Experiences the constraint as pure coordination: lower tax burden creates capital retention and investment incentive. Exit options excellent — can shift investment allocation or restructure to exploit tax incentives. Suppression minimal for this agent — the constraint works in their favor. Theater moderate — policy framed as growth stimulus, functionally operates as tax cut for capital.
constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNIONS (TANGLED ROPE) — Organized agents benefit from job creation and tight labor markets (increases wage bargaining power) but constrained by inflation risk, reduced public sector expansion (union employment base), and suppression of more direct full-employment policies. Genuine coordination function (stimulus creates labor demand); extraction through opportunity cost (stimulus-via-tax-cut vs stimulus-via-public-works debate). Suppression operates through ideological framing — Keynesian demand management is presented as only viable option, foreclosing debate over alternative stimulus mechanisms that might benefit public-sector unions more.
constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL RESERVE (SCAFFOLD) — Sees the tax bill as a temporary coordination mechanism: fiscal stimulus provides near-term recession insurance while monetary policy maintains medium-term stability. Suppression is low — the Fed retains policy autonomy and can adjust interest rates independently. Theater moderate — the 'recession insurance' framing is partially aspirational; actual countercyclical effects depend on coordination between fiscal and monetary policy. Has exit: can tighten policy if inflation emerges, can reverse accommodation if fiscal stimulus proves excessive. Sunset implicit: as economy reaches full employment, stimulus becomes inflationary rather than stabilizing, and both fiscal and monetary tightening become necessary.
constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TREASURY DEPARTMENT (PITON) — The tax bill maintains a performative function: it appears to stimulate growth through tax cuts while the actual mechanism is aggregate demand expansion via deficit spending. Functional degradation: the 'tax cut for growth' narrative masks Keynesian stimulus (which should be characterized as countercyclical spending). Theater high — the constraint operates largely through rhetorical performance: politicians claim tax cuts boost growth; the economic effect is from fiscal deficit, not tax structure change. Treasury maintains this theater because it enables politically palatable stimulus while avoiding explicit demand-management rhetoric.
constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the tax bill appears to solve an inherent constraint of market economies: the business cycle cannot be eliminated, only managed. Recessions are presented as inevitable natural phenomena requiring countercyclical policy. However, this naturalizes what is actually a contingent institutional arrangement: aggregate demand management via deficit spending is a chosen policy mechanism, not a law of nature. The 'natural law' framing prevents questioning whether alternative mechanisms (direct employment, public works, automatic stabilizers) might produce better outcomes. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1963_johnson_tax_bill_recession_insurance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1963_johnson_tax_bill_recession_insurance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1963_johnson_tax_bill_recession_insurance, TR),
    TR >= 0.70.

:- end_tests(sotu_1963_johnson_tax_bill_recession_insurance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The tax bill creates genuine aggregate demand expansion (positive) while extracting through higher-earner burden and inflationary pressure (negative). The extraction is partially masked by economic growth — beneficiaries (workers, businesses) experience net benefit during the stimulus window. Extracted value flows from high-income taxpayers to wage earners and capital owners; the incidence is complex and time-dependent. Over the 6-year interval, extractiveness rises from 0.28 to 0.42 as inflation begins accumulating and the initial stimulus effect plateaus. Suppression (0.35): Moderate. Structural barriers to alternative policies are ideological rather than material — the 'tax-cut stimulus' narrative dominates public discourse, suppressing consideration of direct employment or public works. Suppression on high-income taxpayers is legal (enforced through IRS) but the suppression is not severe — wealthy taxpayers retain substantial wealth and can relocate assets (though not persons). Theater ratio (0.45–0.50): Moderate-rising. The 'tax cut for growth' framing is partially theatrical — the actual mechanism is deficit-driven aggregate demand expansion. Theater increases over the interval as inflation emerges and the actual tax policy structure (lower rates) becomes insufficient to explain the growth effect without invoking Keynesian multipliers. The constraint's claimed mechanism (tax structure) diverges from its actual mechanism (deficit spending), driving theater ratio upward.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The business sector sees pure coordination (Rope) — they receive tax cuts and experience beneficial incentives with no extraction cost perceived. The high-income taxpayer sees pure extraction (Snare) — involuntary burden with no benefit to this agent class. The unemployed job-seeker sees tangled rope — genuine job creation (coordination) but inflationary erosion and lost public sector opportunities (extraction). The labor unions see scaffold — temporary stimulus with an implicit sunset when full employment is reached. The Federal Reserve sees temporary coordination with stabilizing policy tools. The Treasury sees institutional ritual maintaining politically acceptable deficit spending through tax-cut theater. The analytical observer risks seeing an immutable law of economics (Mountain) — recessions cannot be prevented, only managed — but this naturalizes what is actually a contestable institutional choice about stimulus mechanism. The perspectival gaps are severe because the constraint bundles genuine coordination (stimulus creates employment) with extraction (high earners bear tax burden) and masks the actual mechanism (deficit spending) with preferred narratives (tax cuts drive growth).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from its structural position relative to the extraction flow. High-income taxpayers (powerless/trapped) face d ≈ 0.95: they are pure targets with no exit, maximum experienced extraction. Business sector (institutional/arbitrage) faces d ≈ 0.10: they are beneficiaries with excellent exit options, experiencing negative effective extraction (they benefit from the constraint). Workers (moderate/constrained) face d ≈ 0.55: they gain employment but constrained by market conditions and inflation, mixed extraction. Unions (organized/constrained) face d ≈ 0.45: organized enough to negotiate but constrained by overall policy framework, moderate extraction. The Federal Reserve (powerful/mobile) faces d ≈ 0.40: they have policy autonomy and can exit through tightening, low extraction. Treasury (institutional/arbitrage) faces d ≈ 0.15: they benefit from the constraint (deficit spending enabled by tax-cut framing), low extraction. The derived f(d) values produce experienced extractiveness (chi) ranging from high (powerless agents) to negative (beneficiary agents), confirming the tangled rope classification for agents experiencing mixed effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying what coordination vs. extraction actually means in fiscal stimulus context. Coordination function: deficit spending expands aggregate demand, creates labor market tightness, increases employment. Extraction function: higher earners bear involuntary tax burden, while the burden distribution over time becomes ambiguous (does inflation shift burden back to workers?). The mandatrophy is not 'is this stimulus good or bad?' but 'who experiences what, and how does the mechanism work?' The constraint demonstrates that stimulus can simultaneously create jobs (genuine coordination) and extract from high earners (genuine extraction). The snare perspective (high-income taxpayer) is not wrong — they do experience extraction. The rope perspective (business sector) is not wrong — they do experience coordination. Both are true from their positions. The Tangled Rope classification is the analytical resolution: the constraint genuinely serves both functions simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stimulus_mechanism_tax_vs_spending,
    'Does the stimulus effect derive from the tax cut structure itself or from the underlying deficit spending? Are lower tax rates or higher aggregate demand the causal driver of growth?',
    'Empirical analysis of tax cuts with vs. without deficit spending; comparison to direct government spending stimulus at equivalent fiscal cost; econometric isolation of tax elasticity vs. multiplier effects',
    'If tax structure causally drives growth: the tax bill is efficient stimulus (lower theater). If deficit spending drives growth: the tax cut is theater masking Keynesian stimulus (higher theater, reclassify to Piton). If both: mixed mechanism confirms Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stimulus_mechanism_tax_vs_spending, empirical, 'Whether growth derives from tax structure or deficit-driven aggregate demand').

omega_variable(
    recession_insurance_counterfactual,
    'Would the economy have entered recession without the tax bill? Did the stimulus prevent a recession or merely accelerate recovery from a mild downturn?',
    'Retrospective analysis comparing actual 1963-1968 trajectory to forecasted recession baseline; examination of leading economic indicators (unemployment rate, capacity utilization, credit conditions) pre-bill vs. post-bill',
    'If prevents major recession: recession insurance function is real, scaffold perspective gains strength. If accelerates minor recovery: insurance function is aspirational, extraction mechanism (tax burden on high earners) becomes more salient, snare perspective strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recession_insurance_counterfactual, empirical, 'Whether tax bill prevented recession or merely accelerated recovery').

omega_variable(
    distributional_incidence_ambiguity,
    'What is the actual long-term incidence of the tax burden? Do higher earners actually bear the cost, or does the tax burden shift to workers through inflation, wage suppression, or reduced public services?',
    'Incidence analysis: wage growth vs. inflation trajectory 1963-1968; public spending reductions; comparison of ex-ante tax burden distribution (statutory rates) vs. ex-post incidence (actual burden on income, wealth, consumption)',
    'If high earners bear burden: snare perspective is accurate. If burden shifts to workers: workers experience both extraction (inflation) and coordination (employment), confirming tangled rope. If burden dissipates through growth: all perspectives experience rope-like coordination (deficit spending creates growth with no net burden).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_incidence_ambiguity, empirical, 'Long-term incidence of tax burden across income distribution').

omega_variable(
    inflation_accumulation_timing,
    'When does inflationary pressure emerge from the stimulus? Is inflation suppressed during the 1963-1968 recovery period and then accumulate post-1968?',
    'Price level tracking by year; wage growth comparison; analysis of 1968-1973 inflation acceleration in relation to sustained fiscal stimulus',
    'If inflation emerges within stimulus period: suppression is real-time and visible, extraction is more salient. If inflation emerges later (1969+): the tax bill appears to succeed at recession insurance during its interval, but creates conditions for subsequent inflation crisis, suggesting theater increased over time (piton reclassification at longer timescale).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_accumulation_timing, empirical, 'Timing of inflation emergence relative to tax stimulus').

omega_variable(
    alternative_policy_counterfactual,
    'Would direct government spending (public works, public employment) have produced equivalent or superior outcomes at equivalent fiscal cost compared to the tax-cut stimulus mechanism?',
    'Comparative policy analysis: literature on public works multipliers vs. tax-cut multipliers; historical analysis of New Deal programs as baseline for direct spending effectiveness; econometric models of alternative fiscal pathways',
    'If public works superior: the tax-cut constraint represents organized suppression of more effective policy (snare with institutional beneficiary: business sector). If equivalent: tax bill represents genuine policy choice with real tradeoffs (tangled rope confirmed). If tax-cut superior: efficiency claims are validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_policy_counterfactual, empirical, 'Comparative effectiveness of alternative stimulus mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1963_johnson_tax_bill_recession_insurance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu63_tr_t0, sotu_1963_johnson_tax_bill_recession_insurance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sotu63_tr_t2, sotu_1963_johnson_tax_bill_recession_insurance, theater_ratio, 2, 0.42).
narrative_ontology:measurement(sotu63_tr_t4, sotu_1963_johnson_tax_bill_recession_insurance, theater_ratio, 4, 0.46).
narrative_ontology:measurement(sotu63_tr_t6, sotu_1963_johnson_tax_bill_recession_insurance, theater_ratio, 6, 0.5).

% Extraction over time
narrative_ontology:measurement(sotu63_be_t0, sotu_1963_johnson_tax_bill_recession_insurance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sotu63_be_t2, sotu_1963_johnson_tax_bill_recession_insurance, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(sotu63_be_t4, sotu_1963_johnson_tax_bill_recession_insurance, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(sotu63_be_t6, sotu_1963_johnson_tax_bill_recession_insurance, base_extractiveness, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1963_johnson_tax_bill_recession_insurance, resource_allocation).
narrative_ontology:affects_constraint(sotu_1963_johnson_tax_bill_recession_insurance, federal_spending_multiplier_mechanism).
narrative_ontology:affects_constraint(sotu_1963_johnson_tax_bill_recession_insurance, inflation_emergence_1968_1973).
narrative_ontology:affects_constraint(sotu_1963_johnson_tax_bill_recession_insurance, tax_policy_structural_debate).

% DUAL FORMULATION NOTE:
% The tax bill constraint can be decomposed into two structurally distinct claims: (1) the stimulus effect derives from tax structure changes (ε ≈ 0.15, Rope for all agents), and (2) the stimulus effect derives from deficit-driven aggregate demand (ε ≈ 0.50, Tangled Rope). The empirical question of which mechanism dominates determines the constraint's true classification. This story treats the unified policy package and assumes mixed mechanism (hence Tangled Rope). Alternative decompositions would separate tax structure effects from spending effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
