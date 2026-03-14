% ============================================================================
% CONSTRAINT STORY: financial_repression_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financial_repression_mechanism, []).

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
 *   constraint_id: financial_repression_mechanism
 *   human_readable: Financial Repression Mechanism
 *   domain: economic_policy/monetary_governance
 *
 * SUMMARY:
 *   Financial repression is a set of policy tools — interest rate suppression
 *   below inflation, mandatory holding of government debt, capital controls,
 *   currency monopoly enforcement, and regulatory barriers to non-bank
 *   financial intermediation — that together extract resources from savers
 *   (primarily retail investors and small businesses with limited arbitrage
 *   options) and transfer them to governments and incumbent financial
 *   institutions. The constraint exhibits genuine coordination elements
 *   (directing credit to government priority projects, stabilizing
 *   debt-to-GDP ratios, maintaining currency monopoly that enables monetary
 *   policy) alongside asymmetric extraction (negative real returns imposed on
 *   captive savers, redistribution from creditors to debtors). The mechanism
 *   is maintained through active enforcement (capital controls, reserve
 *   requirements, administered interest rates, regulatory licensing). Theater
 *   ratio has increased over the interval as central banks have developed
 *   more sophisticated transmission mechanisms and explanatory narratives,
 *   disguising the extractive component as technical necessity ('financial
 *   stability,' 'macroprudential policy,' 'emergency measures'). The
 *   constraint's classification as Tangled Rope reflects that no single
 *   perspective views it as pure coordination (Rope) or pure extraction
 *   (Snare) — different agents experience it differently based on their exit
 *   options and structural position.
 *
 * KEY AGENTS:
 *   - Government Treasury: Primary beneficiary (institutional/arbitrage) — extracts real resources through seigniorage, debt-to-GDP reduction, and deficit financing at below-market rates
 *   - Savers (Retail & Small Investors): Primary victim (powerless/trapped) — trapped by currency monopoly, capital controls, and administered interest rates; experience systematic negative real returns
 *   - Incumbent Banking Sector: Secondary beneficiary (institutional/arbitrage) — benefits from spread protection, capital requirements forcing debt demand, and barriers to non-bank competition
 *   - Small Business Owners: Secondary victim (moderate/constrained) — constrained by credit rationing, borrowing requirements, and currency debasement; also benefit from access to cheap credit
 *   - International Investors: Mixed (organized/constrained) — organized agents seeking yields but constrained by currency depreciation and capital flight risk; benefit from access to repressed-regime debt yields
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financial_repression_mechanism, 0.58).
domain_priors:suppression_score(financial_repression_mechanism, 0.62).
domain_priors:theater_ratio(financial_repression_mechanism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financial_repression_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(financial_repression_mechanism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(financial_repression_mechanism, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financial_repression_mechanism, tangled_rope).
narrative_ontology:human_readable(financial_repression_mechanism, "Financial Repression Mechanism").
narrative_ontology:topic_domain(financial_repression_mechanism, "economic_policy/monetary_governance").

domain_priors:requires_active_enforcement(financial_repression_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financial_repression_mechanism, government_treasury).
narrative_ontology:constraint_beneficiary(financial_repression_mechanism, large_incumbent_financial_institutions).
narrative_ontology:constraint_victim(financial_repression_mechanism, savers_retail_investors).
narrative_ontology:constraint_victim(financial_repression_mechanism, currency_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL SAVER (SNARE) — Trapped by currency monopoly and capital controls. Real interest rates held below inflation (negative real returns). Cannot exit national currency system; domestic savings systematically extracted via financial repression. No arbitrage available without violating capital controls or incurring severe tax penalties. Maximum experienced extraction.
constraint_indexing:constraint_classification(financial_repression_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Constrained by borrowing requirements and currency exposure. Benefits from access to artificially cheap credit (coordination function: credit allocation), but bears costs through inflation, negative real returns on savings, and currency debasement. High exit cost (relocation, foreign currency exposure, regulatory friction). Mixed extraction and coordination.
constraint_indexing:constraint_classification(financial_repression_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENT TREASURY (ROPE) — Primary beneficiary. Experiences financial repression as pure coordination: eroding the real value of debt, reducing debt service burden, and directing credit to government-favored projects. Can arbitrage through seigniorage (central bank money creation), debt restructuring, and deficit financing. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(financial_repression_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT BANKING SECTOR (ROPE) — Secondary beneficiary. Benefits from financial repression through: (a) guaranteed demand for government debt (capital requirements), (b) spreads between administered rates and market rates, (c) protection from non-bank financial competition through capital controls and regulatory barriers. Can arbitrage by extracting spread differential. Experiences as coordination mechanism for credit supply.
constraint_indexing:constraint_classification(financial_repression_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL INVESTMENT COMMUNITY (TANGLED ROPE) — Organized but constrained by currency devaluation and capital flight restrictions. Benefits from access to yield-hungry international capital seeking returns above repressed domestic rates (coordination function: cross-border capital allocation). Bears costs through currency depreciation, default risk, and political capital controls. Can exit via divestment but faces significant losses. Mixed extraction and coordination at the border.
constraint_indexing:constraint_classification(financial_repression_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT) — Civilizational perspective risks classifying financial repression as a natural law: 'government must manage debt; interest rate suppression is inevitable for fiscal sustainability; savers must bear the burden.' This natural law framing naturalizes what is actually a political choice enforced through legal restrictions on capital mobility, interest rate administered-pricing, and monetary policy. The structural data reveals this is not a mountain but a tangled rope sustained through active enforcement.
constraint_indexing:constraint_classification(financial_repression_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_repression_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financial_repression_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_repression_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financial_repression_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(financial_repression_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Retail savers experience negative real returns averaging 2-5% annually in repressive regimes (nominal rates 2-4% while inflation runs 5-8%). This is a direct extraction mechanism. However, the figure is lower than pure extraction (0.70+) because the government's debt servicing constraint is genuine (fiscal sustainability is a real coordination problem) and some benefits (currency stability, credit availability) are distributed to moderate-power actors (small business). The trajectory shows extractiveness growing from 0.32 to 0.58 over the interval, reflecting deepening of repressive measures as fiscal pressures mount. Suppression (0.62): Moderate-high. Significant barriers to exit include legal capital controls (penalties up to 50% of transfers in some regimes), currency monopoly enforcement, reserve requirements, licensing restrictions on foreign currency accounts, and tax penalties on foreign investment. But suppression is not total (0.90+) because some arbitrage is available to wealthy agents (smuggling, foreign property, political capital for exemptions) and suppression varies across national regimes. Theater ratio (0.55): Moderate. Central banks have developed increasingly sophisticated framing (macroprudential policy, financial stability, countercyclical policy) to justify what are fundamentally administered interest rates and capital rationing. But the theater is not yet high (0.70+) because the basic extraction mechanism (negative real returns) is viscerally obvious to savers. Theater has been rising as transmission mechanisms have become more complex.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from identical base properties. The government treasury sees Rope: interest rate suppression and debt-to-GDP reduction are coordination solutions to genuine fiscal sustainability problems. The banking sector sees Rope: regulatory protection and spread benefits from the system are valuable coordination. Small business owners see Tangled Rope: they benefit from credit access but suffer inflation erosion. International investors see Tangled Rope: yield opportunities attract capital but currency depreciation and capital controls constrain exit. Retail savers see Snare: trapped by currency monopoly, systematic extraction via negative real returns, no meaningful exit. The civilizational analytical observer risks seeing Mountain: 'government must manage debt; interest rate suppression is necessary for fiscal stability; savers are bearing their fair share of burden.' The structural data reveals this Mountain perspective as a false summit — financial repression is a contingent political choice enforced through legal mechanisms (capital controls, administered pricing), not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) derives from their structural position in the constraint. Government and incumbent banks derive d ≈ 0.05-0.20 (beneficiaries with arbitrage options): they experience low or negative chi — the constraint subsidizes them. Retail savers derive d ≈ 0.90-0.95 (trapped victims with no exit): they experience maximum chi — the constraint directly extracts from them. Small business owners derive d ≈ 0.50-0.65 (moderate agents, mixed position): they benefit from credit access but bear inflation and currency costs — experienced chi is moderate, placing them in Tangled Rope rather than Snare. International investors derive d ≈ 0.60-0.70 (organized but constrained): they can exit via divestment but face currency losses, placing them in Tangled Rope. The directionality chain produces a perspectival spread from Rope (beneficiaries) through Tangled Rope (moderate/organized agents) to Snare (trapped savers), all from the same base properties. This spread is the signature of a successfully operative extraction regime.
 *
 * MANDATROPHY ANALYSIS:
 *   Financial repression resolves the mandatrophy by showing that Tangled Rope classification is correct precisely because the constraint contains genuine coordination (fiscal stability, credit direction) AND asymmetric extraction (negative real returns). The constraint cannot be classified as pure Rope because savers systematically lose (victimization is structural). It cannot be classified as pure Snare because governments genuinely solve a fiscal coordination problem and some moderate-power agents benefit from credit access. The mandatrophy is resolved by recognizing that the 'necessity' framing (fiscal sustainability requires repression) is partial truth: repression is ONE solution to fiscal problems, not the ONLY solution. Alternative paths (spending constraint, tax reform, default restructuring) exist but involve different distributional consequences. Financial repression is chosen because it benefits government and incumbent institutions while imposing costs on diffuse, powerless savers. The Tangled Rope classification captures this exactly: coordination function (fiscal stabilization, credit allocation) co-exists with extraction (savers bear negative real returns).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coercive_participation,
    'What proportion of saver participation in the financial repression regime is voluntary (accepting negative real returns for stability or convenience) versus coercive (legal prohibition on alternatives)?',
    'Comparative analysis of savings behavior when capital controls are relaxed or eliminated; measurement of actual versus latent demand for foreign currency holdings; survey data on saver awareness of negative real returns',
    'If mostly voluntary: classification shifts toward Rope (savers see coordination benefits). If mostly coercive: classification remains Snare (coercive extraction). If mixed: confirms Tangled Rope for moderate-power agents (constrained choice) and Snare for powerless agents (trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coercive_participation, empirical, 'Degree of voluntary vs coercive participation in negative real returns').

omega_variable(
    fiscal_sustainability_necessity,
    'Is financial repression a necessary instrument for fiscal sustainability in a given context, or is it primarily a rent-extraction mechanism benefiting incumbent actors?',
    'Counterfactual analysis: modeling alternative fiscal paths (spending restraint, tax reform, default) and comparing their distributional consequences to the financial repression path; analysis of whether repression revenue exceeds what would be generated by market-rate borrowing plus spending adjustment',
    'If necessary: suppression should be reclassified as lower (legitimate fiscal constraint rather than coercive extraction). If primarily extractive: suppression remains high and confirmed as rent-seeking. This directly affects the claimed_type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_necessity, conceptual, 'Whether financial repression is fiscally necessary or primarily extractive').

omega_variable(
    inflation_channel_ambiguity,
    'Is the extraction mechanism primarily interest rate suppression, inflation tax (seigniorage), or the combination of both? How much extraction flows through each channel?',
    'Decomposition of real return erosion into components: (1) nominal rate below inflation (interest rate channel), (2) unexpected inflation above forecasts (surprise inflation tax), (3) expected inflation priced into rates (no extraction, just redistribution). Measurement of ex-post vs ex-ante returns.',
    'If mostly unexpected inflation: extraction is maximal and highly salient (powerless agents experience Snare). If mostly expected: agents adjust expectations and experience lower effective extraction (Tangled Rope even for moderate-power agents). Changes the actual chi values experienced across perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_channel_ambiguity, empirical, 'Decomposition of extraction between interest rate suppression and inflation tax channels').

omega_variable(
    alternative_equilibrium_stability,
    'If financial repression were removed (capital controls lifted, interest rates liberalized), what equilibrium would emerge? Would it be more or less stable than the current repressed equilibrium?',
    'Historical comparisons with countries that have ended financial repression (Chile 1973-1989, Poland 1989-1991, South Korea 1998 post-crisis liberalization); modeling of currency stability, inflation dynamics, and savings allocation under liberalization; measurement of whether initial capital flight stabilizes into new equilibrium',
    'If stable liberalized equilibrium exists: the mountain perspective (repression is necessary) is false — removal is feasible. Supports Tangled Rope classification over Mountain. If liberalization triggers currency collapse or hyperinflation: mountain perspective gains plausibility, and repression appears more structurally necessary. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_equilibrium_stability, empirical, 'Whether liberalized equilibrium is stable and more efficient than repressed equilibrium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financial_repression_mechanism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(finrep_tr_t0, financial_repression_mechanism, theater_ratio, 0, 0.38).
narrative_ontology:measurement(finrep_tr_t10, financial_repression_mechanism, theater_ratio, 10, 0.48).
narrative_ontology:measurement(finrep_tr_t20, financial_repression_mechanism, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(finrep_be_t0, financial_repression_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(finrep_be_t10, financial_repression_mechanism, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(finrep_be_t20, financial_repression_mechanism, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financial_repression_mechanism, resource_allocation).
narrative_ontology:affects_constraint(financial_repression_mechanism, currency_monopoly_enforcement).
narrative_ontology:affects_constraint(financial_repression_mechanism, capital_controls_regime).
narrative_ontology:affects_constraint(financial_repression_mechanism, inflation_targeting_framework).

% DUAL FORMULATION NOTE:
% Financial repression operates as a bundle of policy instruments (interest rate suppression, capital controls, inflation taxation, mandatory debt holdings). These instruments are often analyzed separately, but they form a single structural constraint when viewed from the saver's perspective — the extraction mechanism requires enforcement across all channels. Decomposition into separate constraints by instrument (interest_rate_suppression, capital_controls, seigniorage) would artificially separate mechanisms that are strategically linked and mutually reinforcing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financial_repression_mechanism, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
