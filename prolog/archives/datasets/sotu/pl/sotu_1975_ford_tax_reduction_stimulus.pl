% ============================================================================
% CONSTRAINT STORY: sotu_1975_ford_tax_reduction_stimulus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1975_ford_tax_reduction_stimulus, []).

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
 *   constraint_id: sotu_1975_ford_tax_reduction_stimulus
 *   human_readable: 1975 Ford Tax Reduction and Stimulus Mechanism
 *   domain: economic/fiscal_policy
 *
 * SUMMARY:
 *   The 1975 Ford tax reduction and stimulus package represents a demand-side
 *   fiscal intervention during the 1974-1975 recession designed to restore
 *   consumption and investment. The mechanism distributes $12 billion in
 *   individual rebates (12% of 1974 tax payments, capped at $1,000 per
 *   return) and liberalizes business investment tax credits to 12%. The
 *   constraint operates as a Tangled Rope: genuine coordination function
 *   (stimulus addresses demand collapse and unused capital capacity) exists
 *   alongside asymmetric extraction (benefits accrue to employed taxpayers
 *   and firms with investment capacity; costs accrue to future taxpayers and
 *   unemployed non-filers excluded from the rebate base). Theater ratio is
 *   moderate (0.42) because the stimulus mechanism is transparent — direct
 *   cash transfers are visible — but the underlying macroeconomic theory
 *   (Keynesian counter-cyclical policy) is increasingly contested by
 *   mid-1970s stagflation evidence. The constraint exhibits all six DR types
 *   from different perspectives, making it diagnostically rich: unemployed
 *   non-filers see Snare (exclusion from benefits), individual recipients see
 *   Rope (direct benefit with consumption choice), firms see Rope (subsidized
 *   investment), future taxpayers see Tangled Rope (mixed coordination and
 *   deferred cost), policymakers see Scaffold (time-limited intervention),
 *   economic doctrine sees Piton (Keynesian theory maintaining the stimulus
 *   despite mounting evidence of inefficacy), and the analytical observer
 *   risks seeing Mountain (natural law of stimulus limitations) but this is a
 *   false summit.
 *
 * KEY AGENTS:
 *   - Individual taxpayers (employed): Primary beneficiary (moderate/mobile) — receive direct rebates; can choose consumption vs. savings timing
 *   - Business firms with investment plans: Primary beneficiary (powerful/arbitrage) — capture 12% investment credit; can reallocate investment timing to maximize benefit
 *   - Unemployed non-filers: Primary victim (powerless/trapped) — excluded from rebate mechanism; bear recession costs without stimulus relief
 *   - Future taxpayers: Secondary victim (moderate/constrained) — obligated to service deficit-financed stimulus through future taxes or reduced services
 *   - Federal Reserve and fiscal policymakers: Organized coordinators (organized/constrained) — design stimulus as time-limited intervention with clear sunset; perceive mechanism as temporary solution
 *   - Keynesian macroeconomic doctrine: Institutional framework (institutional/arbitrage) — legitimizes stimulus as counter-cyclical policy; maintains intellectual coherence despite stagflation contradictions
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional policy as economic law; False Summit detection reveals naturalization of contingent doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1975_ford_tax_reduction_stimulus, 0.38).
domain_priors:suppression_score(sotu_1975_ford_tax_reduction_stimulus, 0.35).
domain_priors:theater_ratio(sotu_1975_ford_tax_reduction_stimulus, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1975_ford_tax_reduction_stimulus, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1975_ford_tax_reduction_stimulus, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1975_ford_tax_reduction_stimulus, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1975_ford_tax_reduction_stimulus, tangled_rope).
narrative_ontology:human_readable(sotu_1975_ford_tax_reduction_stimulus, "1975 Ford Tax Reduction and Stimulus Mechanism").
narrative_ontology:topic_domain(sotu_1975_ford_tax_reduction_stimulus, "economic/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1975_ford_tax_reduction_stimulus).
narrative_ontology:has_sunset_clause(sotu_1975_ford_tax_reduction_stimulus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1975_ford_tax_reduction_stimulus, individual_taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1975_ford_tax_reduction_stimulus, business_investment_firms).
narrative_ontology:constraint_beneficiary(sotu_1975_ford_tax_reduction_stimulus, treasury_short_term_deficit_reduction).
narrative_ontology:constraint_victim(sotu_1975_ford_tax_reduction_stimulus, future_fiscal_capacity).
narrative_ontology:constraint_victim(sotu_1975_ford_tax_reduction_stimulus, low_income_nonworking_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNEMPLOYED NON-FILER (SNARE) — Excluded from rebate mechanism because they filed no tax return in 1974. Bears costs of inflation/recession (unemployment, reduced services) but receives no direct stimulus. Trapped in economic downturn with no exit option. Maximum extraction — the constraint explicitly excludes this agent while distributing benefits to employed taxpayers.
constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL REBATE RECIPIENT (ROPE) — Receives direct cash transfer (up to $1,000 per return). Limited by one-time nature of rebate and modest rebate amount relative to annual income. Experiences stimulus as coordination mechanism: government and household solve consumption timing problem together. Mobile — can spend or save rebate. Net benefit but constrained by program design.
constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS INVESTMENT FIRM (ROPE) — Liberalized investment tax credit (12%) directly subsidizes capital equipment purchases. Arbitrage option: can reallocate investment timing to capture the credit. Experiences constraint as beneficial coordination — government absorbs cost, firms make investment decisions. Strong net benefit with agency.
constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FUTURE TAXPAYER (TANGLED ROPE) — Bears deferred cost of deficit-financed stimulus through higher future tax burdens or reduced services. Constrained by institutional fiscal structure — cannot exit the federal tax system. Genuine coordination benefit exists (economic recovery reduces unemployment and raises tax base), but asymmetric distribution of costs (future) and benefits (present) creates extraction. Suppression via institutional inevitability of future fiscal obligation.
constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FISCAL POLICYMAKERS (SCAFFOLD) — See stimulus as temporary coordinating intervention with explicit sunset: one-year duration, specific dollar magnitude, time-limited business credit. Organized agents (Congress, Executive) design it as non-permanent. Theater low because the mechanism is transparent: direct cash transfer, identifiable credit. Perceive constraint as soluble — once recovery proceeds, rebates and credits phase out and normal fiscal structure resumes. Exit path clear.
constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: KEYNESIAN STABILIZATION DOCTRINE (PITON) — The stimulus mechanism is an instantiation of counter-cyclical fiscal policy (Keynesian framework). The institutional doctrine persists through economic theory and policy training, but its functional relationship to actual economic recovery is contested and increasingly questioned. By 1975, the doctrine's credibility is eroding (stagflation contradicts Phillips curve; velocity assumptions challenged). Theater ratio reflects that the stimulus is largely performative expression of a doctrine rather than a mechanism with high causal efficacy. High institutional inertia — policymakers continue stimulus because Keynesian theory legitimizes it, not because empirical evidence demonstrates effectiveness. Piton classification derives from theater (0.42) and doctrinal degradation, not from high extractiveness.
constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MONETARY NEUTRALITY (MOUNTAIN) — From a civilizational/global analytical perspective, the constraint appears as a natural law of fiscal stimulus: real economic effects require relative price changes or supply-side shifts; nominal redistribution via rebates and credits leaves real constraints unchanged. This perspective sees the stimulus as inherently limited by monetary theory — you cannot solve real unemployment (reduced capital stock, skill mismatches, sectoral shifts) with nominal transfer mechanisms. However, empirical evidence contradicts this mountain classification, revealing it as a false summit: the constraint is an institutional arrangement (Keynesian policy doctrine) presented as economic necessity.
constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1975_ford_tax_reduction_stimulus_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1975_ford_tax_reduction_stimulus, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1975_ford_tax_reduction_stimulus, TR),
    TR >= 0.70.

:- end_tests(sotu_1975_ford_tax_reduction_stimulus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The stimulus mechanism does deliver genuine direct benefits to recipients (rebates, investment credits) and addresses a real coordination problem (demand collapse in recession). However, extractiveness is elevated above zero by: (1) exclusion of non-filers from rebate base, concentrating benefits on employed population; (2) asymmetric temporal distribution (immediate benefits to present agents, deferred costs to future taxpayers); (3) distributional regressivity of flat-cap rebate structure ($1,000 cap benefits lower-income filers more in relative terms but concentrates absolute benefits on median-income filers); (4) uncertainty whether business investment credit stimulates net new capital or merely accelerates existing plans. The extractiveness trajectory shows rise to 0.38 at mid-interval (peak stimulus intensity and uncertainty about effectiveness) then decline to 0.32 at end of interval (stimulus phase-out, recovery beginning to reduce deficit urgency). Theater ratio (0.42): Moderate-low. Direct cash transfers and explicit tax credits are mechanically transparent — recipients see the benefit, IRS administers the mechanism straightforwardly. Theater does not reflect hidden extraction. Rather, theater reflects that underlying macroeconomic justification (Keynesian stimulus doctrine) is increasingly performative by 1975 — the theory maintains institutional authority despite stagflation evidence that contradicts Phillips curve assumptions and multiplier expectations. Suppression (0.35): Moderate. Barriers to exit from the constraint's effects are institutional (future taxpayers cannot exit the federal fiscal system) and structural (unemployed cannot retrospectively file tax returns to claim rebates). However, suppression is not total — individual recipients choose spending timing, firms choose investment timing, future fiscal effects are diffuse and long-delayed, and the stimulus is explicitly temporary. Beneficiaries and victims are not trapped; they are constrained by institutional structure and temporal asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a sharp perspectival gap between beneficiaries (see coordination and benefit) and victims (see extraction or exclusion). Individual rebate recipients see Rope — straightforward coordination mechanism where government transfers cash and households solve consumption timing. Business firms see Rope — investment credit directly subsidizes capital formation at clear marginal rate. Both beneficiary perspectives are veridical: the coordination function is real. Unemployed non-filers see Snare — the constraint explicitly excludes them from benefits while recession imposes costs. Future taxpayers see Tangled Rope — mixed benefit (economic recovery reduces unemployment and raises future tax base) and cost (deficit-financed stimulus creates fiscal obligation). The perspectival gap is bridged by temporal distribution: immediate beneficiaries have high power and mobile/arbitrage exit options; victim groups are temporally diffuse (future) or politically organized (unemployed) but lack direct participation in stimulus design. Policymakers see Scaffold — they perceive the stimulus as temporary, time-limited coordination mechanism with clear sunset (one-year duration, specific dollar magnitude). The Piton perspective (degradation of Keynesian doctrine) emerges when comparing 1975 policy environment to pre-stagflation 1960s: the theory maintained authority through inertia and institutional credibility despite mounting empirical contradictions. The analytical observer risks a false-summit Mountain (stimulus is naturally limited by monetary theory and real economy structure) — but this naturalizes what is actually a contingent policy choice grounded in increasingly questioned doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: who benefits, who bears costs, what exit options constrain each agent. Individual rebate recipients: beneficiary status + mobile exit (can choose consumption timing) → low-to-moderate d → low chi → Rope experience. Business firms: beneficiary status + arbitrage exit (can reallocate investment timing) → very low d → negative/near-zero chi → strong Rope experience. Unemployed non-filers: victim status + trapped exit (cannot retroactively file; cannot exit recession) → very high d → high chi → Snare experience (maximum experienced extraction). Future taxpayers: mixed victim status (bear deferred costs) + constrained exit (cannot exit federal fiscal system) → moderate-high d → moderate chi → Tangled Rope experience. Policymakers: beneficiary status (doctrine credits them with solving crisis) + constrained exit (institutional obligation to implement monetary-fiscal coordination) → low-moderate d → low chi → Scaffold experience (perceive sunset, not extraction). Keynesian doctrine: beneficiary status (institutional authority and research funding) + arbitrage exit (can modify doctrine, incorporate new evidence) → very low d → near-zero chi → Piton experience (institutionally maintained despite functional degradation). Analytical observer: observer status (external to political-economic structure) → moderate d → moderate chi via canonical fallback (0.73). The engine derives d from beneficiary/victim declarations and exit options; the sigmoid f(d) then computes experienced extractiveness chi relative to base extractiveness ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that extractiveness (0.38) is legitimate: the constraint genuinely coordinates (addresses demand collapse, utilizes unused capital capacity) AND exhibits real asymmetric extraction (benefits to employed/investing agents, costs to unemployed/future agents). The Tangled Rope classification reconciles apparent contradiction: the constraint is not pure coordination (rope) because distributional asymmetry and temporal distribution of costs create extraction; it is not pure extraction (snare) because genuine coordination benefit exists. The constraint's theatrical character (Keynesian doctrine maintaining intellectual authority despite stagflation) explains why the constraint persists despite questionable empirical efficacy. The Piton perspective (economic doctrine degradation) and false-summit Mountain (naturalization of institutional choice) are diagnostic signals that the policy's credibility rests on institutional inertia and doctrinal authority rather than robust empirical validation. Mandatrophy is resolved: the constraint is correctly classified as Tangled Rope when base properties, perspectives, and beneficiary/victim declarations are integrated. The classification is not contradicted by any single perspective — it is the coherent summary of the structure across all perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stimulus_multiplier_uncertainty,
    'What is the actual short-run fiscal multiplier for 1975 household rebates and business investment credits? Does it exceed 1.0, justifying the stimulus as counter-cyclical?',
    'Econometric analysis comparing counties and states with differential rebate take-up rates; comparison of 1975 consumption growth vs. baseline recession trajectory; assessment of crowding-out effects on private investment',
    'If multiplier < 0.8: stimulus is net deficit-increasing without significant output recovery (snare perspective dominates). If multiplier > 1.2: stimulus is genuinely counter-cyclical coordination (rope perspective dominates). If 0.8-1.2: tangled rope assessment correct — coordination mixed with asymmetric fiscal cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stimulus_multiplier_uncertainty, empirical, 'Magnitude of fiscal multiplier for 1975 stimulus mechanisms').

omega_variable(
    business_investment_timing_vs_underlying_demand,
    'Does the liberalized investment tax credit stimulate net new capital formation, or primarily accelerate investment that would have occurred anyway?',
    'Comparison of 1975 capital formation trends vs. 1974 baseline; analysis of whether credit-induced investments have positive NPV absent credit; sector-level analysis of investment timing bunching around credit implementation',
    'If net new capital: business investment credit is genuine coordination (reduces intertemporal distortion). If primarily timing acceleration: credit is extraction mechanism disguised as stimulus (benefits present firms, costs future treasury). Classification shift from rope to snare for business perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(business_investment_timing_vs_underlying_demand, empirical, 'Whether investment credit creates net new capital or merely accelerates existing plans').

omega_variable(
    rebate_propensity_to_consume_distribution,
    'What proportion of rebate income is consumed vs. saved? Does propensity vary by income level, age, and expectation of permanence?',
    'Household surveys on rebate spending plans; panel data on consumption changes of rebate recipients; comparison of high-MPC (marginal propensity to consume) groups vs. low-MPC groups',
    'If high consumption (MPC > 0.7): stimulus transmits to demand, validating tangled_rope coordination function. If low consumption (MPC < 0.4): rebate is saved/used for debt reduction, reducing stimulus effect and shifting classification toward snare (extraction without coordination benefit). Psychological expectations (one-time vs. permanent income) are critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebate_propensity_to_consume_distribution, empirical, 'Propensity to consume out of one-time tax rebate').

omega_variable(
    distributional exclusion_mechanism,
    'Is the exclusion of non-filers (unemployed, disabled, elderly with no income) a structural feature of tax-based targeting or a policy choice?',
    'Historical analysis of rebate design: were alternatives with broader coverage (refundable credits, negative income tax, direct payments) considered and rejected? Analysis of coverage rates by income decile.',
    'If structural: snare perspective reflects inherent limitation of tax-based transfer (trapment for excluded populations is unavoidable). If policy choice: snare perspective reflects extractive design (rebate could have included non-filers but didn''t). Classification of the constraint depends on whether exclusion is mechanism-necessary or contingent-institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional exclusion_mechanism, conceptual, 'Whether tax-rebate exclusion of non-filers is structural or contingent').

omega_variable(
    long_term_fiscal_sustainability,
    'Does the deficit-financed stimulus reduce long-term fiscal capacity, or is recovery-driven tax base growth sufficient to offset the cost?',
    'Fiscal accounting from 1975 baseline through 1980; comparison of debt-to-GDP trajectory under stimulus vs. no-stimulus scenario; analysis of whether 1975-1980 recovery generated additional tax revenue exceeding rebate and credit costs',
    'If deficit is self-correcting through growth: tangled_rope assessment correct (legitimate coordination with deferred cost). If deficit accumulates and constrains future policy: extraction is more severe (future_taxpayer victim perspective validated). Impact on mandatrophy resolution for high-extractiveness constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_fiscal_sustainability, empirical, 'Long-term fiscal sustainability of 1975 deficit-financed stimulus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1975_ford_tax_reduction_stimulus, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ford_tax_tr_t0, sotu_1975_ford_tax_reduction_stimulus, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ford_tax_tr_t6, sotu_1975_ford_tax_reduction_stimulus, theater_ratio, 6, 0.42).
narrative_ontology:measurement(ford_tax_tr_t12, sotu_1975_ford_tax_reduction_stimulus, theater_ratio, 12, 0.38).

% Extraction over time
narrative_ontology:measurement(ford_tax_be_t0, sotu_1975_ford_tax_reduction_stimulus, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ford_tax_be_t6, sotu_1975_ford_tax_reduction_stimulus, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(ford_tax_be_t12, sotu_1975_ford_tax_reduction_stimulus, base_extractiveness, 12, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1975_ford_tax_reduction_stimulus, resource_allocation).
narrative_ontology:affects_constraint(sotu_1975_ford_tax_reduction_stimulus, inflation_expectations_1975).
narrative_ontology:affects_constraint(sotu_1975_ford_tax_reduction_stimulus, federal_deficit_accumulation_1975_1980).
narrative_ontology:affects_constraint(sotu_1975_ford_tax_reduction_stimulus, keynesian_doctrine_credibility_crisis).

% DUAL FORMULATION NOTE:
% The 1975 Ford tax stimulus is structurally upstream of multiple constraint families: inflation expectations (stimulus signals demand-side policy, affects price-setting), deficit accumulation (fiscal cost compounds over recovery period), and Keynesian doctrine (stimulus is institutional expression of theory whose empirical credibility is declining). Separate stories track the constraint's downstream institutional and macroeconomic effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1975_ford_tax_reduction_stimulus, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
