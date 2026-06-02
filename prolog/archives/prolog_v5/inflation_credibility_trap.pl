% ============================================================================
% CONSTRAINT STORY: inflation_credibility_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inflation_credibility_trap, []).

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
 *   constraint_id: inflation_credibility_trap
 *   human_readable: Inflation Credibility Trap in Central Banking
 *   domain: macroeconomic_policy/monetary_governance
 *
 * SUMMARY:
 *   The inflation credibility trap represents a structural dynamic in which a
 *   central bank's mandate to maintain price stability becomes incompatible
 *   with fiscal and political pressures to tolerate inflation. Once inflation
 *   expectations drift upward and become embedded in wage-setting and
 *   financial markets, the central bank faces a choice: impose painful
 *   disinflation (unemployment, real wage cuts, asset price declines) to
 *   restore credibility, or accommodate inflation through a combination of
 *   explicit policy adjustment (raising inflation targets) and implicit
 *   theater (redefining measurement, emphasizing 'transitory' factors,
 *   gradually shifting the goalposts of the formal target). The trap emerges
 *   because organized actors (government, financial sector) benefit from
 *   inflation tolerance and can pressure the central bank to choose
 *   accommodation, while powerless actors (wage earners, savers) bear the
 *   costs of eroding purchasing power and lack the political voice to demand
 *   disinflation. The constraint combines genuine coordination (all agents
 *   benefit from price stability in principle) with asymmetric extraction
 *   (the path to stability imposes concentrated costs on powerless agents
 *   while allowing organized interests to escape via inflation hedging). This
 *   makes the inflation credibility trap a canonical tangled rope: it solves
 *   a real coordination problem (how to finance government deficits and clear
 *   labor markets during growth slowdowns) through mechanisms that extract
 *   disproportionately from those without exit options.
 *
 * KEY AGENTS:
 *   - Wage Earners: Primary victims (powerless/trapped) — real wages erode as nominal wages adjust slowly while prices accelerate; cannot organize coordinated wage demands without employer retaliation or job loss
 *   - Savers: Primary victims (powerless/trapped) — nominal asset holdings (savings, bonds, cash) lose purchasing power; barriers to alternative assets (real estate access, foreign currency restrictions, transaction costs) prevent exit
 *   - Government Fiscal Authority: Primary beneficiary (institutional/arbitrage) — real debt burden shrinks through inflation erosion; nominal growth appears strong; can arbitrage short-term inflation tolerance with long-term credibility restoration
 *   - Central Bank Incumbent: Primary beneficiary (institutional/arbitrage) — maintains political accommodation option; can shift inflation target, revise measurement, articulate 'transitory' narratives; preserves institutional continuity
 *   - Financial Institutions: Secondary beneficiary (moderate/constrained) — benefit from debt erosion and inflation as hedge mechanism; constrained by deposit flight and margin compression risks
 *   - Organized Opposition (Inflation Hawks, Bond Markets, Currency Unions): Organized agents (organized/constrained) — pushing for credibility restoration through disinflation commitment; have constrained exit (policy pressure, capital flight threats, competing currencies) but can force regime change over generational timescale
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both the genuine coordination problem (government financing, labor market adjustment) and the asymmetric extraction mechanism (distributional burden concentrated on powerless agents)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inflation_credibility_trap, 0.58).
domain_priors:suppression_score(inflation_credibility_trap, 0.62).
domain_priors:theater_ratio(inflation_credibility_trap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inflation_credibility_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(inflation_credibility_trap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(inflation_credibility_trap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inflation_credibility_trap, tangled_rope).
narrative_ontology:human_readable(inflation_credibility_trap, "Inflation Credibility Trap in Central Banking").
narrative_ontology:topic_domain(inflation_credibility_trap, "macroeconomic_policy/monetary_governance").

domain_priors:requires_active_enforcement(inflation_credibility_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inflation_credibility_trap, incumbent_central_bank).
narrative_ontology:constraint_beneficiary(inflation_credibility_trap, government_fiscal_actors).
narrative_ontology:constraint_beneficiary(inflation_credibility_trap, debt_holders).
narrative_ontology:constraint_victim(inflation_credibility_trap, wage_earners).
narrative_ontology:constraint_victim(inflation_credibility_trap, savers).
narrative_ontology:constraint_victim(inflation_credibility_trap, currency_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped in nominal wage rigidity while prices rise. Real wages erode despite wage agreements. Exit is structural: cannot opt out of currency, cannot flee nation's inflation, cannot coordinate wage increases without employer retaliation or job loss. Bears full extraction cost with no escape route. Suppression is maximal because labor market power asymmetry prevents wage-price spiral even as inflation persists.
constraint_indexing:constraint_classification(inflation_credibility_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAVER (SNARE) — Trapped in nominal asset denominations while purchasing power decays. Bank deposits, bonds, cash holdings lose real value. Exit options are severely constrained: alternative assets (real estate, commodities, foreign currency) face legal barriers, transaction costs, or are inaccessible to low-income savers. Faces extraction without organized alternative.
constraint_indexing:constraint_classification(inflation_credibility_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTION (TANGLED ROPE) — Benefits from inflation erosion (debt burdens shrink in real terms for borrowers; banks capture spread compression). Also constrained by coordination requirement: must maintain deposit flows and lending trust. Faces both asymmetric extraction (margin compression as interest rate ceilings bind) and coordination benefits (inflation simplifies debt rollovers). Moderate agent with constrained exit: can switch regulatory regimes or business models at cost.
constraint_indexing:constraint_classification(inflation_credibility_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT FISCAL AUTHORITY (ROPE) — Experiences inflation as pure coordination benefit. Real debt burden shrinks; tax brackets drift nominally upward (fiscal drag); nominal growth appears strong. Exit via alternative funding mechanisms (real debt market discipline, tax enforcement, spending cuts) is available but costly. Government can arbitrage: ignore inflation consequences short-term, manage expectations through rhetoric, coordinate with central bank on implicit inflation tolerance.
constraint_indexing:constraint_classification(inflation_credibility_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CENTRAL BANK - INCUMBENT (ROPE) — Experiences credibility trap as coordination mechanism for managing political pressure. Can articulate price stability mandate while tolerating inflation through: (a) revised inflation targets (shifting goalposts), (b) measurement changes (hedonics, base effects), (c) communication of 'transitory' narratives. Arbitrage exit available: resign mandate, pivot to flexible inflation targeting, coordinate with government on implicit tolerance. Net beneficiary: maintains political cover while accommodating fiscal expansion.
constraint_indexing:constraint_classification(inflation_credibility_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED OPPOSITION (SCAFFOLD) — Central bank independence advocates, inflation hawks, currency unions (ECB independence framework) see the credibility trap as temporary institutional failure. Organized actors (institutional economists, bond market vigilantes, competing currency zones) have constrained exit: can shift policy pressure, demand governance reforms, threaten capital flight. Time horizon is generational because credibility restoration requires 5-10 year commitment to disinflation. Theater is high during transition period (recession rhetoric masking deliberate demand destruction). This perspective sees a sunset: if inflation credibility is restored, the trap dissolves.
constraint_indexing:constraint_classification(inflation_credibility_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INFLATION TARGETING REGIME - DEGRADED (PITON) — The formal 2% inflation target is largely theater. Central banks have revised definitions, measurement methodologies, and expectations frameworks so extensively that the original target is unrecognizable. The ritual persists (central bank communications still invoke the target) but the functional meaning has atrophied. Piton classification reflects theater_ratio > 0.65: central banks maintain credibility performance (inflation reports, policy statements) while the substantive discipline has eroded through institutional inertia and political accommodation.
constraint_indexing:constraint_classification(inflation_credibility_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational standpoint, inflation targeting represents a hybrid: genuine coordination (price stability benefits all agents; reducing inflation volatility is real public good) coupled with asymmetric extraction (institutional actors benefit more than powerless agents from inflation tolerance, and organized interests (financial sector, government) profit from the trap while wage earners bear costs). The constraint persists because dismantling it requires painful adjustment (unemployment, real wage cuts) that creates political opposition, which the incumbent regime exploits. Central classification of tangled_rope reflects ε=0.58, suppression=0.62, and the requirement that beneficiaries (government, banks) + victims (workers, savers) + enforcement (central bank policy + political pressure) all exist.
constraint_indexing:constraint_classification(inflation_credibility_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inflation_credibility_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inflation_credibility_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inflation_credibility_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inflation_credibility_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inflation_credibility_trap, TR),
    TR >= 0.70.

:- end_tests(inflation_credibility_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The inflation credibility trap extracts from wage earners and savers in the form of purchasing power loss. The extraction is not as extreme as a pure Snare (ε ≥ 0.66) because: (a) some inflation benefits are diffuse (debt erosion benefits all debtors, which includes some wage earners with mortgages), (b) organized labor can partially recover through coordinated wage claims (constrained rather than fully trapped), and (c) long-term credibility restoration remains theoretically possible. The 0.58 value reflects substantial asymmetric extraction coupled with meaningful exit options for organized actors. Suppression (0.62): High. Multiple barriers prevent exit: legal currency monopolies prevent denomination switching, labor market power asymmetry prevents unilateral wage increases, capital controls and transaction costs block forex access for small savers, and organized labor organizing faces employer retaliation. Suppression is not maximal (0.85) because: (a) some worker sectors (public sector, unionized) do achieve partial wage indexation, (b) financial institutions and wealthy actors can hedge inflation, and (c) political opposition can eventually force policy change. Theater ratio (0.68): High. The measurement of inflation has drifted substantially from original definitions through methodological revisions (hedonic adjustment, substitution weighting, base effect management). Central banks maintain elaborate communications around 'transitory' inflation factors, 'core' vs 'headline' distinctions, and revised target frameworks that were intended to be rigid but have become adjustable. The theater reflects the gap between the formal inflation target (2%) and the revealed tolerance for inflation (3-5%+). Theater has increased over the measurement interval as communication theater has substituted for actual policy commitment.
 *
 * PERSPECTIVAL GAP:
 *   The inflation credibility trap creates one of the largest perspectival gaps in macroeconomic policy. Government and incumbent central bank see coordination or manageable politics (Rope, Piton, Scaffold depending on time horizon). Wage earners see irreversible extraction (Snare). The gap reflects real structural differences: government can influence policy, central bank can adjust targets/measurement, financial institutions can hedge; wage earners cannot. The gap is not a measurement error — it is the credibility trap's defining feature. Resolution would require either: (a) credibility restoration (organized opposition's Scaffold path) reducing the extraction pressure, or (b) organized labor coordination powerful enough to shift wage-setting dynamics (converting Snare to Tangled Rope), or (c) currency exit becoming realistic for savers (reducing trapped to constrained classification). Current trajectory suggests gradual theater degradation (Piton) combined with stable Snare for powerless agents — the trap is self-sustaining because the actors who benefit most (government, central bank) are also the actors with power to define the problem and the terms of resolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to the extraction flow. Government (institutional/arbitrage) derives d ≈ 0.05 (beneficiary with exit option) → f(d) ≈ -0.12 → negative or minimal χ (benefits from inflation). Wage earners (powerless/trapped) derive d ≈ 0.95 (victim with no exit) → f(d) ≈ 1.42 → high χ (bears extraction). Central bank incumbent (institutional/arbitrage) derives d ≈ 0.15 (moderate beneficiary; politically accommodates but nominally maintains mandate) → f(d) ≈ 0.08 → moderate χ. Organized opposition (organized/constrained) derives d ≈ 0.50 (symmetric position: benefits from eventual price stability, bears costs of disinflation pressure) → f(d) ≈ 0.65 → moderate χ. The spread in d values (from 0.05 to 0.95) across perspectives explains why some agents see coordination (low d → low/negative χ) while others see extraction (high d → high χ). The constraint's defining property is that the same ε (base extraction 0.58) is distributed asymmetrically across agents with different d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The inflation credibility trap resolves the mandatrophy by demonstrating how a single coordination problem (government financing in a low-growth environment) becomes a constraint that segregates agents by power level. The mandatrophy is the temptation to classify the entire constraint as Rope (it does solve the coordination problem of fiscal sustainability and labor market adjustment) and miss the asymmetric extraction mechanism. Classification as pure Rope would hide that wage earners experience Snare while government experiences benefit. The tangled_rope classification forces visibility of both: the genuine coordination function (inflation does simplify macroeconomic adjustment) AND the asymmetric extraction mechanism (costs concentrated on powerless agents while benefits flow to organized actors). The mandatrophy resolution requires declaring beneficiaries (government, financial institutions, incumbent central bank) and victims (wage earners, savers, currency users) explicitly. Without this declaration, the constraint collapses to either Rope (missing the extraction) or Snare (missing the coordination). The presheaf of perspectives reveals the true structure: a hybrid mechanism optimized for incumbent institutional actors at the cost of powerless agents' purchasing power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_expectations_formation,
    'Are inflation expectations formed adaptively (based on observed history) or rationally (incorporating future policy commitment)? If adaptive, the trap deepens; if rational, credibility can be restored through announced policy.',
    'Empirical econometric analysis of household and firm inflation expectations; comparison of expectations revisions following explicit central bank communication about disinflation commitment; quasi-experimental evidence from policy regime changes (e.g., Volcker disinflation, ECB independence framework).',
    'If adaptive: the trap is structural and requires years of demonstrated low inflation to reset expectations. If rational: credible commitment to disinflation can restore anchoring quickly, reducing suppression and shifting classification toward Rope (pure coordination). Current evidence suggests a mix — moderate rationality with sticky adaptive components — implying ε remains moderate rather than converging to either extreme.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_expectations_formation, empirical, 'Mechanism of inflation expectation formation').

omega_variable(
    fiscal_dominance_reversibility,
    'Is the credibility trap irreversible once fiscal dominance (government deficit financing via inflation) becomes institutionalized, or can a central bank restore independence through explicit commitment even against political opposition?',
    'Historical case analysis: Volcker disinflation (success under political fire), ECB independence (sustained credibility despite member state pressure), contemporary emerging markets (loss and attempted restoration of central bank autonomy). Structural: can independence be restored if the government has become dependent on seigniorage revenue?',
    'If irreversible: classification locks into Snare (for wage earners) and sustained Rope (for government) — the trap persists indefinitely unless political collapse. If reversible: Scaffold classification is correct — organized opposition (inflation hawks, bond markets, competing currencies) can force regime change with long enough time horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_dominance_reversibility, empirical, 'Reversibility of fiscal-monetary dominance once established').

omega_variable(
    real_wage_recovery_threshold,
    'What inflation rate or disinflation timeline would trigger widespread wage-earner pressure for nominal wage escalation that could re-accelerate inflation? Is there a threshold of perceived unfairness that breaks the suppression mechanism?',
    'Labor unrest data during high-inflation periods; correlation between real wage losses and strike activity or union organizing; historical wages-inflation feedback loops; contemporary data on wage-setting demands during moderate inflation phases (2-4%).',
    'If threshold is low (labor organizes quickly): suppression weakens and classification shifts from Snare to Tangled Rope for wage earners — they acquire constrained exit (organized labor). If threshold is high (labor accepts real losses indefinitely): suppression persists, Snare classification confirmed, and the trap is stable. Current evidence suggests moderate threshold (~2-3 years of real wage losses) — suppression is durable but not indefinite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_wage_recovery_threshold, empirical, 'Threshold for wage-earner escalation of nominal demands').

omega_variable(
    measurement_inflation_definitional_capture,
    'As central banks have revised CPI methodology (hedonics, substitution effects, base effects), have these technical changes genuinely captured improving quality of life, or have they systematically understated inflation to preserve credibility narratives?',
    'Comparison of official CPI vs. alternative inflation measures (trimmed-mean, owner-equivalent-rent models, international measurement standards); analysis of timing of methodological revisions relative to political pressure for lower inflation reporting; randomized household surveys asking about actual price experiences vs. official statistics.',
    'If revisions are genuine: theater_ratio should be lower (~0.45) and ε should decline. If revisions are definitional capture: theater_ratio rises and ε is understated — true extractiveness is higher than reported. Current suspicion suggests moderate bias (some genuine measurement improvement, some definitional drift) — theater_ratio estimate of 0.68 reflects this mixed mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_inflation_definitional_capture, empirical, 'Whether CPI revisions reflect genuine measurement improvement or credibility theater').

omega_variable(
    currency_substitution_exit_availability,
    'Can savers and wage earners realistically exit the inflation trap by adopting alternative currencies (foreign currency savings, cryptocurrencies, barter-adjacent informal economies), or are legal/institutional barriers sufficiently strong to trap users in the domestic currency?',
    'Empirical data on dollarization in high-inflation economies; cryptocurrency adoption during periods of currency debasement; legal barriers to foreign currency transactions; transaction costs (spreads, conversion fees) for small savers. Does realistic alternative exist for median wage-earner, or only for wealthy actors with access to capital markets?',
    'If realistic alternatives exist: classification shifts from trapped to constrained (high-cost exit available); Snare becomes Tangled Rope for wage earners. If barriers are high: trapped classification confirmed. Current evidence suggests barriers are significant for small savers and wage earners — legal currency monopolies, low access to forex markets, transaction costs. Cryptocurrency has reduced barriers in some jurisdictions but is unstable store of value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(currency_substitution_exit_availability, empirical, 'Availability of realistic alternative currency exit for ordinary savers and wage earners').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inflation_credibility_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infl_tr_t0, inflation_credibility_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(infl_tr_t3, inflation_credibility_trap, theater_ratio, 3, 0.52).
narrative_ontology:measurement(infl_tr_t6, inflation_credibility_trap, theater_ratio, 6, 0.64).
narrative_ontology:measurement(infl_tr_t10, inflation_credibility_trap, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(infl_be_t0, inflation_credibility_trap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(infl_be_t3, inflation_credibility_trap, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(infl_be_t6, inflation_credibility_trap, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(infl_be_t10, inflation_credibility_trap, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inflation_credibility_trap, resource_allocation).
narrative_ontology:affects_constraint(inflation_credibility_trap, wage_price_spiral_dynamics).
narrative_ontology:affects_constraint(inflation_credibility_trap, central_bank_independence_erosion).
narrative_ontology:affects_constraint(inflation_credibility_trap, government_debt_sustainability).

% DUAL FORMULATION NOTE:
% The inflation credibility trap is downstream of two distinct structural claims: (1) fiscal dominance — government deficit financing creates political pressure for monetary accommodation; (2) expectation dynamics — inflation expectations, once elevated, are sticky and require costly disinflation to reset. These are separate constraints with different ε values. The credibility trap story models the hybrid effect where both operate simultaneously. The upstream constraints in the network address the fiscal and expectations components separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
