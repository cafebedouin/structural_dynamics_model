% ============================================================================
% CONSTRAINT STORY: sotu_1990_bush_family_savings_plan
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1990_bush_family_savings_plan, []).

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
 *   constraint_id: sotu_1990_bush_family_savings_plan
 *   human_readable: Family Savings Plan Tax Incentive Mechanism (1990 Bush Administration)
 *   domain: economic_policy/capital_formation
 *
 * SUMMARY:
 *   The Family Savings Plan, introduced in the 1990 State of the Union
 *   address by President George H.W. Bush, is a tax incentive mechanism
 *   designed to increase household savings by making savings tax-advantaged
 *   relative to consumption. The policy's stated goal is capital formation:
 *   by increasing the supply of capital available for business expansion and
 *   investment, the plan aims to stimulate job creation and wage growth. The
 *   structural mechanics are straightforward: tax deferrals and exclusions on
 *   capital gains and savings reduce the cost of saving relative to
 *   consumption, creating incentives for households to redirect income into
 *   productive assets. The plan is classified as Tangled Rope because it
 *   combines a genuine coordination function (aligning private savings
 *   incentives with national capital formation goals) with asymmetric
 *   extraction (disproportionately benefiting high-income savers while
 *   imposing costs on the federal treasury and future taxpayers). The key
 *   structural tension is between the plan's stated function (broad capital
 *   formation for job creation) and its primary documented effect (wealth
 *   concentration among households with capacity to save). The extractiveness
 *   value (0.58) reflects that the revenue cost (estimated $5-15 billion
 *   annually) is substantial, but a portion of the extraction is offset by
 *   genuine coordination benefit and by future revenue generation through
 *   business growth. However, the theater ratio (0.52) indicates that the gap
 *   between stated goal and primary effect is significant — the plan
 *   functions more as a wealth concentration mechanism than as a broad
 *   job-creation tool. The rising extractiveness over the measurement
 *   interval (0.35 to 0.63) reflects accumulating insights: the plan's
 *   initially modest distributional effects become more pronounced over time
 *   as high-income savers compound returns and as the gap between
 *   job-creation claims and documented effects becomes clearer.
 *
 * KEY AGENTS:
 *   - High-income households: Primary beneficiaries (institutional/arbitrage) — capture disproportionate tax subsidy through capital gains and savings deferrals; can arbitrage between saving and consumption/investment strategies.
 *   - Small business owners and entrepreneurs: Secondary beneficiaries (institutional/arbitrage) — benefit from access to expanded capital pools and improved financing conditions; experience the constraint as coordination.
 *   - Low-income households: Primary victims (powerless/trapped) — cannot participate in tax-advantaged savings due to lack of discretionary capital; bear extraction through reduced public services or higher future tax rates.
 *   - Federal treasury and revenue system: Secondary victim (institutional/constrained) — bears direct revenue loss; constrained by political commitment to plan and need for tax competitiveness.
 *   - Middle-income savers: Mixed beneficiary-victim (moderate/constrained) — can participate marginally but capture less benefit per-capita than high-income savers; constrained by tax complexity and limited discretionary capital.
 *   - Fiscal policy reformers: Organized agents (organized/constrained) — see plan as temporary intervention with sunset potential; constrained by political inertia and legacy beneficiary commitments.
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing the constructed tax preference as an immutable feature of efficient markets.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1990_bush_family_savings_plan, 0.58).
domain_priors:suppression_score(sotu_1990_bush_family_savings_plan, 0.45).
domain_priors:theater_ratio(sotu_1990_bush_family_savings_plan, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1990_bush_family_savings_plan, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1990_bush_family_savings_plan, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sotu_1990_bush_family_savings_plan, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1990_bush_family_savings_plan, tangled_rope).
narrative_ontology:human_readable(sotu_1990_bush_family_savings_plan, "Family Savings Plan Tax Incentive Mechanism (1990 Bush Administration)").
narrative_ontology:topic_domain(sotu_1990_bush_family_savings_plan, "economic_policy/capital_formation").

domain_priors:requires_active_enforcement(sotu_1990_bush_family_savings_plan).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1990_bush_family_savings_plan, small_business_owners).
narrative_ontology:constraint_beneficiary(sotu_1990_bush_family_savings_plan, high_income_savers).
narrative_ontology:constraint_beneficiary(sotu_1990_bush_family_savings_plan, venture_capital_ecosystem).
narrative_ontology:constraint_victim(sotu_1990_bush_family_savings_plan, federal_treasury).
narrative_ontology:constraint_victim(sotu_1990_bush_family_savings_plan, low_income_non_savers).
narrative_ontology:constraint_victim(sotu_1990_bush_family_savings_plan, future_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME HOUSEHOLDS (SNARE) — Cannot participate in the tax incentive because they lack the capital to save. Trapped by immediate consumption requirements and wage constraints. Bears the extraction burden through higher future tax rates or reduced public services to offset revenue loss. No exit option.
constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME SAVERS (TANGLED ROPE) — Can participate marginally in tax-advantaged savings (IRAs, capital gains treatment) but faces constraints: limited discretionary capital, opportunity cost of foregone consumption, complexity of navigating rules. Genuine coordination benefit (incentive alignment increases savings) combined with asymmetric distribution of gains (high-income savers capture disproportionate tax subsidy). Constrained exit because the tax structure is mandatory; participation is optional but exit from the broader fiscal system is not.
constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS OWNERS (ROPE) — Primary beneficiaries who experience the constraint as pure coordination: tax incentives align their personal savings incentives with capital formation for expansion. Direct benefit from access to expanded capital pool. Arbitrage exit: can choose to save or invest domestically or internationally; the tax incentive makes domestic saving attractive relative to alternatives. Net benefit outweighs any perceived cost.
constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTH CONCENTRATION MECHANISM (PITON) — The plan's stated functional goal is capital formation and job creation. Its actual documented effect is significant wealth concentration: tax-advantaged savings disproportionately benefit high-income households who have discretionary capital. The mechanism persists not because it efficiently achieves its stated goal but because the beneficiary coalition (wealthy individuals, financial services industry) maintains it through political inertia. Theater ratio reflects gap between stated goal (broad job creation and wage growth) and primary effect (wealth concentration). High-income actors see the constraint as outdated (piton) rather than functional (rope) because its job-creation impact is modest relative to the wealth concentration effect.
constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FISCAL POLICY REFORMERS (SCAFFOLD) — Organized agents (tax reformers, budget-deficit hawks, progressive coalitions) see the plan as a temporary intervention with built-in obsolescence. The constraint could be sunset: eliminating the tax incentive and using revenue for direct infrastructure or education investment achieves capital formation without wealth concentration. Constrained by political capital and legacy beneficiary commitments, but sees a clear exit path (base tax rate reform). The constraint is performative (theater) rather than functionally necessary — alternative capital formation mechanisms exist.
constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL TREASURY (TANGLED ROPE) — Bears the direct extraction (foregone tax revenue, estimated $5-15B annually depending on plan scope). But also benefits from the coordination function: increased capital formation can generate future tax revenue through business growth and employment. Constrained by political commitment to the plan and by the need to maintain tax competitiveness (cannot simply reverse incentives without affecting investment migration). Extraction and benefit coexist within the same institutional perspective.
constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capital formation through incentivized savings can appear as an immutable feature of market economics: if agents are rational wealth-maximizers, they will allocate capital to its highest-return uses, and tax incentives merely reveal natural patterns of investment efficiency. However, the structural beneficiary data (specific high-income groups, not all savers) reveals this as a false summit — the 'natural' capital allocation is actually a constructed preference set shaped by tax policy design.
constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1990_bush_family_savings_plan_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1990_bush_family_savings_plan, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1990_bush_family_savings_plan, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1990_bush_family_savings_plan, TR),
    TR >= 0.70.

:- end_tests(sotu_1990_bush_family_savings_plan_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The plan extracts significant revenue from the federal treasury (estimated $5-15B annually at 1990 levels, growing over the decade). This extraction is genuine and measurable. However, it is partially offset by coordination benefit: the increased capital formation does produce some genuine business expansion and, in principle, future tax revenue through growth. The 0.58 value reflects that the coordination benefit is real but incomplete — the plan's capital formation effect is modest relative to the revenue cost, and much of the benefit accrues to high-income savers rather than to broad job creation. The rising trajectory (0.35 to 0.63) reflects that the coordination benefit declines over time as the plan matures and concentrates wealth rather than broadening capital access. Suppression (0.45): Moderate. Low-income households are suppressed from participation by lack of capital, but the suppression is structural (poverty) rather than policy-enforced. Middle-income savers face suppression through complexity and tax code navigation barriers. The suppression is real but not total — alternative savings vehicles exist. Theater ratio (0.52): Moderate-high. The stated goal (broad job creation and wage growth) is theater relative to the primary documented effect (wealth concentration among high savers). The policy's performative element is the job-creation narrative, which oversells the causal connection between the tax incentive and employment. The rising theater (0.38 to 0.58) reflects that over the measurement interval, the gap between rhetorical promise and documented effect widens.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the beneficiary perspective (Rope — the constraint aligns incentives and produces capital) and the low-income household perspective (Snare — the constraint extracts without providing benefit). A secondary gap exists between the institutional treasury perspective (Tangled Rope — mixed benefit and cost) and the analytical observer perspective (Mountain — treating tax incentives as natural market coordination). The piton perspective (Wealth Concentration Mechanism) observes that the plan's stated function (job creation) has degraded relative to its actual function (wealth concentration), and that it persists through inertia rather than effectiveness. The scaffold perspective (Fiscal Reformers) sees a sunset path: replacing the tax incentive with direct government investment or regulatory deregulation could achieve equivalent capital formation without wealth concentration. The perspectival gap reflects the range from genuine coordination (small business owner view) to pure extraction (low-income household view), mediated by institutional constraints and alternative mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality pipeline: High-income savers/small business owners are declared beneficiaries with institutional power and arbitrage exit options. The derivation chain computes d ≈ 0.10-0.20 (full beneficiary + arbitrage → low directionality). Applied to the sigmoid f(d), this produces negative or near-zero f(d), meaning their experienced effective extraction is negative (they experience subsidy, not extraction). The federal treasury is implicitly a victim (bears revenue cost) with no exit option, yielding d ≈ 0.85-0.95 and high f(d) ≈ 1.35+, meaning their experienced extraction is amplified. Low-income non-savers are explicitly victims (trapped by poverty) with d ≈ 0.95, producing maximum f(d) ≈ 1.42+. These differentials produce the perspectival gap: beneficiary sees Rope (low chi, coordination), victim sees Snare (high chi, extraction), institutional observer sees Tangled Rope (mixed chi, both present).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy — 'is this a coordination mechanism (Rope) or an extraction mechanism (Snare)?' — is resolved by indexical specificity. The constraint IS both. From the small business owner's perspective, it is genuine coordination: the tax incentive aligns their personal savings incentives with capital formation that benefits business expansion. From the low-income household's perspective, it is pure extraction: they cannot participate and bear the cost through reduced public goods. From the institutional treasury perspective, it is Tangled Rope: the extracted revenue is partially recovered through future tax revenue from business growth, but the net fiscal effect is negative in the short term and uncertain long-term. The mandatrophy resolution is to recognize that all three classifications are structurally correct — they describe the same constraint from different structural positions. The constraint is not 'really' one type masquerading as another; it is genuinely a coordination mechanism for some agents and an extraction mechanism for others, with the distribution of benefit and cost determining the classification from each perspective. The piton perspective adds that the constraint is also, objectively, theater relative to its stated goal — the documented job-creation effect is modest relative to the wealth concentration effect. The scaffold perspective indicates that the constraint could be sunset and replaced with alternatives that achieve equivalent capital formation without the wealth concentration cost, making it a temporary intervention rather than a necessary feature of capital markets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_formation_counterfactual,
    'Would the observed increase in savings and business capital formation have occurred without the tax incentive, driven by other economic factors (interest rates, business climate, technology cycles)?',
    'Econometric analysis comparing savings and capital formation rates in policy-treated and control regions; instrumental variable estimation using policy implementation timing variation',
    'If counterfactual savings were high regardless: plan extracts revenue with minimal coordination benefit (reclassify toward Snare). If savings were low absent incentive: plan is genuine coordination mechanism (confirm Rope/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_formation_counterfactual, empirical, 'Counterfactual capital formation without tax incentive').

omega_variable(
    wealth_concentration_intention,
    'Is the disproportionate wealth concentration in high-income savers a recognized design feature or an unintended consequence of the tax structure?',
    'Analysis of legislative intent, CBO distributional analysis at time of enactment, internal economic modeling; comparison to alternative designs (e.g., per-account caps, means-testing) that were considered but rejected',
    'If intentional design: plan is redistributive extraction masquerading as coordination (stronger Snare reading). If unintended: plan is Tangled Rope with an exploitable design flaw (sunset potential). If designed but unstated: plan exhibits false summit characteristics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_concentration_intention, conceptual, 'Whether wealth concentration is intentional policy design').

omega_variable(
    job_creation_attribution,
    'What fraction of observed job creation in the interval 1990-2000 is attributable to the capital formation effect of the Family Savings Plan versus other sources (trade policy, technology adoption, demographics, monetary policy)?',
    'Causal decomposition using policy variation, regional heterogeneity, and dynamic macroeconomic models; comparison of employment growth in regions with different plan take-up rates',
    'If job creation is substantial and attributable: plan functions as stated (Rope for beneficiary perspective). If minimal or not attributable: stated function is theater, actual function is revenue extraction and wealth concentration (Piton/Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(job_creation_attribution, empirical, 'Attributable job creation from capital formation mechanism').

omega_variable(
    fiscal_sustainability_horizon,
    'At what point does cumulative revenue loss from the tax incentive exceed the discounted future tax revenue generated by plan-induced capital formation and economic growth?',
    'Long-term fiscal projection models; dynamic scoring of plan effects on future revenue; comparison to baseline without plan',
    'If net fiscal effect becomes negative: plan transitions from Tangled Rope (mixed benefit) to Snare (pure extraction) at future time horizon. Determines whether scaffold sunset should be triggered.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_sustainability_horizon, empirical, 'Fiscal break-even horizon for plan sustainability').

omega_variable(
    alternative_capital_formation_mechanisms,
    'Could equivalent capital formation be achieved through direct government investment, public-private partnerships, or regulatory deregulation, avoiding the wealth concentration effect?',
    'Comparative institutional analysis of capital formation mechanisms in peer economies; analysis of alternative policy designs with equivalent cost but different distributional properties',
    'If alternatives exist: plan is not functionally necessary (strongly supports Scaffold classification and sunset logic). If alternatives are weaker: plan has genuine irreplaceable function (supports Rope reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capital_formation_mechanisms, conceptual, 'Existence and effectiveness of alternative capital formation mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1990_bush_family_savings_plan, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fsp_tr_t0, sotu_1990_bush_family_savings_plan, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fsp_tr_t3, sotu_1990_bush_family_savings_plan, theater_ratio, 3, 0.45).
narrative_ontology:measurement(fsp_tr_t6, sotu_1990_bush_family_savings_plan, theater_ratio, 6, 0.52).
narrative_ontology:measurement(fsp_tr_t9, sotu_1990_bush_family_savings_plan, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(fsp_be_t0, sotu_1990_bush_family_savings_plan, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fsp_be_t3, sotu_1990_bush_family_savings_plan, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(fsp_be_t6, sotu_1990_bush_family_savings_plan, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(fsp_be_t9, sotu_1990_bush_family_savings_plan, base_extractiveness, 9, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1990_bush_family_savings_plan, resource_allocation).
narrative_ontology:affects_constraint(sotu_1990_bush_family_savings_plan, capital_gains_tax_structure).
narrative_ontology:affects_constraint(sotu_1990_bush_family_savings_plan, wealth_concentration_dynamics).
narrative_ontology:affects_constraint(sotu_1990_bush_family_savings_plan, federal_budget_deficit_constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
