% ============================================================================
% CONSTRAINT STORY: us_debt_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_debt_ceiling, []).

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
 *   constraint_id: us_debt_ceiling
 *   human_readable: US Debt Ceiling
 *   domain: political/fiscal_policy
 *
 * SUMMARY:
 *   The US debt ceiling is a legally binding limit on federal borrowing
 *   established in 1917 and repeatedly raised or suspended since 1960.
 *   Structurally, it creates a periodic negotiation crisis in which Congress
 *   must vote to increase the ceiling to authorize payment of
 *   already-incurred obligations. This creates a leverage point: whichever
 *   faction controls the ceiling vote can demand policy concessions. The
 *   constraint exhibits strong tangled rope characteristics (coordination
 *   bundled with extraction), but with increasing theater ratio (0.81) as the
 *   ceiling has never actually constrained spending — every ceiling crisis
 *   ends in authorization increase, yet the negotiation process forces fiscal
 *   reckoning and bundles spending debate. The extractiveness score (0.62)
 *   reflects moderate-to-significant extraction: creditors experience
 *   volatility risk, federal agencies experience payment uncertainty, and
 *   fiscal conservatives extract policy wins. The suppression is high (0.68)
 *   because alternatives to the current mechanism (multi-year budgeting,
 *   automatic authorization, independent fiscal commission) face
 *   institutional resistance, and Congress has no practical incentive to
 *   eliminate a tool that gives political leverage.
 *
 * KEY AGENTS:
 *   - Congressional Majority with Leverage: Institutional/arbitrage actors who control ceiling votes and demand policy concessions during negotiations. Primary beneficiary (extraction flows toward them in form of policy wins). Organized ability to coordinate demands.
 *   - Treasury and Executive Branch: Institutional/constrained actors who must manage payment prioritization during ceiling-imposed delays and uncertainty. Secondary victim during crises; benefit from coordination function when ceiling is normal.
 *   - Federal Benefit Recipients and Federal Employees: Powerless/trapped actors who face payment delays when Treasury must prioritize under ceiling constraints. Bear extraction cost of delayed benefits and paychecks. Cannot exit.
 *   - Bondholder and Creditor Class: Powerful/mobile but internationally constrained actors who experience volatility risk and spread widening during ceiling crises. Trapped in US treasury market during negotiation windows (exit is costly). Secondary victims of negotiation-induced volatility.
 *   - Fiscal Constraint Advocates: Institutional/arbitrage actors who benefit from the ceiling's theoretical constraint on spending, even though it is never binding. See coordination benefit (forces fiscal debate) without experiencing extraction.
 *   - Reform Coalition: Organized/constrained actors developing alternative fiscal mechanisms (automatic appropriations, multi-year budgeting) to replace the ceiling. See it as temporary problem with structural sunset.
 *   - International Financial System: Powerful/mobile actors experiencing tangled rope dynamics — genuine coordination benefit (predictable annual fiscal debate signals market stability between crises) plus extraction (volatility tax and spread adjustments during crisis windows).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_debt_ceiling, 0.62).
domain_priors:suppression_score(us_debt_ceiling, 0.68).
domain_priors:theater_ratio(us_debt_ceiling, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_debt_ceiling, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_debt_ceiling, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_debt_ceiling, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_debt_ceiling, tangled_rope).
narrative_ontology:human_readable(us_debt_ceiling, "US Debt Ceiling").
narrative_ontology:topic_domain(us_debt_ceiling, "political/fiscal_policy").

domain_priors:requires_active_enforcement(us_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_debt_ceiling, executive_branch_discretion).
narrative_ontology:constraint_beneficiary(us_debt_ceiling, congressional_leverage_holders).
narrative_ontology:constraint_victim(us_debt_ceiling, fiscal_constraint_seekers).
narrative_ontology:constraint_victim(us_debt_ceiling, economic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BONDHOLDER EXTRACTION (SNARE) — Trapped in a system where debt ceiling crises create existential uncertainty about repayment. Cannot exit US treasury markets without massive cost. Experiences pure extraction via repeated brinksmanship that depresses asset values during negotiation windows and forces acceptance of deteriorating credit terms. Maximum suppression: alternatives (foreign treasuries, private bonds) require capital reallocation that itself triggers losses.
constraint_indexing:constraint_classification(us_debt_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL AGENCIES (TANGLED ROPE) — Constrained by uncertainty about payment timing and appropriation levels; cannot plan procurement or benefit distribution during ceiling negotiations. But also benefits from the coordination function: the ceiling forces periodic fiscal reckoning and bundles spending authorizations with debt capacity. Significant extraction (delayed payments, budget uncertainty) but genuine coordination benefit (prevents unlimited spending without debate). Active enforcement required — the Treasury must manage payment prioritization during crisis windows.
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL LEVERAGE HOLDERS (ROPE) — Institutional actors with procedural control over ceiling votes can demand policy concessions (spending cuts, deregulation, authorization reductions) during crisis. Experience the constraint as a coordination mechanism for forcing fiscal negotiation. Net beneficiaries — extract policy wins during leverage windows. Arbitrage exit: can walk away from ceiling crisis if external event (market shock, electoral pressure) forces compromise. Low experienced extraction — they design and control the negotiation.
constraint_indexing:constraint_classification(us_debt_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FISCAL CONSTRAINT MYTHOLOGY (PITON) — The debt ceiling functions as a performative artifact of fiscal responsibility without actual constraint. Federal government always raises the ceiling when it hits; the constraint is never binding on actual borrowing or spending. Theater ratio (0.81) reflects that 100% of ceiling crises are 'resolved' through ceiling increases, yet the discourse treats each negotiation as a genuine constraint on federal spending. The ceiling persists through institutional inertia (Congress inherited it, changing it requires legislation) despite its function having atrophied. The theatrical component: annual/biennial negotiation crises create appearance of fiscal discipline without reducing deficits or debt levels.
constraint_indexing:constraint_classification(us_debt_ceiling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Progressive reformers see the debt ceiling as a temporary institutional artifact with a structural sunset. Proposals to eliminate the ceiling, replace it with automatic appropriations authorization, or link it to GDP growth are building alternative fiscal coordination mechanisms. Organized but constrained by institutional resistance; perceive the current ceiling as a transitional problem being solved by replacing the mechanism entirely. Sunset logic: as automatic appropriations and multi-year budgeting mature, the ceiling's enforcement mechanism loses force. Theater remains high but functional dependency declines.
constraint_indexing:constraint_classification(us_debt_ceiling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL FINANCIAL SYSTEM (TANGLED ROPE) — International investors experience the ceiling as both coordination device (signals US fiscal debate, enables orderly debt markets during normal periods) and extraction mechanism (repeated crisis brinksmanship creates volatility, forces risk premium adjustments). Powerful actors with significant exit options (can reallocate capital to other sovereigns) but constrained within the window of each negotiation. Effective extraction rises when ceiling negotiations create systemic risk; falls when resolution appears certain. Experience genuine coordination benefit (predictable annual reckoning) plus extraction (volatility tax on holdings during crisis windows).
constraint_indexing:constraint_classification(us_debt_ceiling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN?) — At civilizational scale, the constraint appears as a natural law of sovereign debt: any government that borrows must eventually confront the limits of creditor tolerance. From this view, the debt ceiling is merely a formal expression of an inherent constraint on sovereign borrowing capacity. However, structural evidence contradicts the mountain classification: the US ceiling has zero binding capacity (always raised); other sovereigns function without explicit ceilings; and the coordination function depends entirely on legislative will, not physical law. The engine's false summit detector will flag this — the 'natural law' reading naturalizes what is actually a contingent institutional and political arrangement.
constraint_indexing:constraint_classification(us_debt_ceiling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_debt_ceiling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_debt_ceiling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_debt_ceiling, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_debt_ceiling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_debt_ceiling, TR),
    TR >= 0.70.

:- end_tests(us_debt_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): The ceiling creates moderate-to-significant extraction because (1) creditors experience volatility and spread-widening costs during negotiation windows, (2) federal agencies and beneficiaries face payment uncertainty and delayed obligations, and (3) Congressional majority can extract policy concessions (spending cuts, authorization reductions, deregulation). The value is not extreme (0.85+) because the ceiling never actually binds — every crisis resolves in ceiling increase, limiting the extraction threat's credibility. Base extractiveness started at 0.48 in 2010 and has risen to 0.62 by 2020 due to increased frequency of crises (2011, 2013, 2015, 2017, 2021) and higher policy stakes extracted. Suppression (0.68): High but not total. Barriers to exiting the ceiling include congressional inertia (changing requires legislation), institutional path-dependency (inherited mechanism), and active benefit to Congressional leverage-holders (they benefit from the extraction tool). However, alternatives exist and are being developed — multi-year budgeting, automatic authorization, fiscal commission proposals. International creditors have mobile options but face significant costs to exit US treasury markets. Trapped agents (federal beneficiaries, employees) face absolute suppression. Theater ratio (0.81): Very high and increasing. The ceiling is performative — 100% of ceiling crises resolve in ceiling increase, yet the negotiation process creates appearance of fiscal constraint. The theatrical component has risen as crises have become more frequent and more explicitly tied to policy demands unrelated to spending (appropriations riders, regulatory rollbacks). The theater is central to the extraction mechanism: the credible threat of default (performance of willingness to breach treaty obligations) is what generates leverage, even though breaching is never actually attempted. Federal agencies play along with payment prioritization plans that never execute.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates full perspectival divergence. The Congressional majority sees Rope (pure coordination and leverage). Federal agencies see Tangled Rope (coordination bundled with extraction uncertainty). Bondholders see Snare (pure extraction via default threat and volatility). Federal beneficiaries see Snare (pure extraction via delayed payments). Fiscal reformers see Scaffold (temporary problem with sunset toward auto-appropriations). The Treasury/inertial perspective sees Piton (performative ritual). The false summit perspective (civilizational/natural law) sees Mountain but the engine detects it as false — the constraint naturalizes what is a contingent political mechanism. This full spectrum is the diagnostic value of the constraint: it reveals how the same structural mechanism can be experienced as coordination, extraction, temporary problem, degraded theater, or natural law depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position, beneficiary/victim status, and exit options. Congressional leverage-holders: beneficiaries with arbitrage exit → low d (around 0.10-0.20) → negative or minimal f(d) → minimal experienced extraction (they control the mechanism). Treasury/agencies: constrained victims → moderate d (around 0.55-0.65) → f(d) ≈ 0.75 → moderate-high experienced extraction. Bondholders: trapped in US markets + secondary victims of volatility → high d (around 0.80-0.85) → f(d) ≈ 1.20-1.30 → high experienced extraction. Federal beneficiaries/employees: powerless + trapped → maximum d (around 0.90-0.95) → f(d) ≈ 1.35-1.42 → maximum experienced extraction. Reform coalition: organized + constrained + beneficiaries of sunset → moderate d (around 0.50-0.60) → f(d) ≈ 0.65-0.75 → moderate extraction that declines with reform progress. No directionality overrides needed — structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The debt ceiling resolves the mandatrophy at extractiveness 0.62 by revealing that the tangled rope classification is stable across multiple observables: (1) measuring via creditor volatility cost, (2) measuring via federal payment delays, (3) measuring via extracted policy concessions all yield consistent ε ≈ 0.60-0.65. The constraint is genuinely hybrid (coordination + extraction), not pure extraction masquerading as coordination. However, the increasing theater ratio (0.65 → 0.81) and stable-despite-crises ceiling increases signal that the extraction mechanism relies entirely on performative threat (default threat that is never executed), whereas the coordination function (forcing fiscal reckoning) is real. This suggests the constraint may be degrading toward Piton as the threat becomes less credible with repeated (non-)use. The constraint is not yet mandatrophy-resolved because it remains genuinely tangled — removing either the coordination component (auto-appropriations) or the extraction component (limiting leverage-extraction) would destabilize the political equilibrium. The mandatrophy is resolved by recognizing that tangled rope can be stable when the coordination and extraction components serve different political constituencies and both have structural inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_binding_threshold,
    'At what debt-to-GDP ratio does the ceiling become genuinely binding (creditors refuse to extend) rather than merely performative (Congress raises it)?',
    'Historical comparison with other sovereigns at high debt ratios (Japan, Italy, Greece during crisis); modeling of creditor behavior as spreads rise; analysis of whether ceiling-raising would be halted by market access loss rather than political negotiation',
    'If currently non-binding: entire constraint is theater (pushes classification toward Piton). If binding threshold is 150% debt-to-GDP: ceiling may still be binding within 10-15 years. If binding threshold > 250% debt-to-GDP: ceiling is never binding for US under normal conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_binding_threshold, empirical, 'Threshold at which debt ceiling becomes binding rather than performative').

omega_variable(
    extraction_mechanism_clarity,
    'Is the primary extraction mechanism (1) debt service cost increases from negotiation volatility, (2) policy concessions demanded during leverage windows, or (3) distributional harm from delayed payments to beneficiaries?',
    'Econometric analysis of treasury yield spreads during ceiling crises vs normal periods; policy outcome tracking during ceiling negotiations; timing analysis of payment delays and their distributional effects',
    'If (1) dominates: bondholder extraction is moderate and direct. If (2) dominates: extraction is to Congressional majority and less economically measurable. If (3) dominates: extraction flows to fiscal conservatives via delayed benefits; tangled rope classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_mechanism_clarity, empirical, 'Which mechanism drives primary extraction under debt ceiling constraint').

omega_variable(
    coordination_function_necessity,
    'Does the debt ceiling serve any genuine coordination function (forcing periodic fiscal reckoning, bundling spending authorization with debt capacity) that could not be achieved through alternative mechanisms (multi-year budgeting, automatic authorization, independent fiscal commission)?',
    'Comparative institutional analysis of non-ceiling countries'' fiscal processes; longitudinal tracking of fiscal outcomes pre- and post-ceiling in countries that eliminated their ceilings; experimental policy design research on alternative bundling mechanisms',
    'If coordination function is essential: tangled rope classification holds. If function is redundant: constraint degrades to pure snare (extraction without coordination). If function is net negative (binds coordination to extraction): reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Whether debt ceiling serves essential coordination function or merely couples extraction to funding').

omega_variable(
    congressional_leverage_distribution,
    'Is the leverage to demand policy concessions (the rope/rope-beneficiary dynamic) concentrated in a narrow faction or distributed across coalition members?',
    'Analysis of policy wins extracted during recent ceiling crises (2011, 2013, 2015, 2017, 2021); power-distribution modeling of voting coalitions; tracking of who demanded what during each negotiation',
    'If leverage is concentrated: tangled rope from majority faction perspective, snare from others. If leverage is distributed: constraint looks more like rope to multiple factions. If leverage is contested (flip-flopping between parties): constraint is unstable tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_leverage_distribution, empirical, 'Distribution of leverage and extracted policy concessions in ceiling negotiations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_debt_ceiling, 2010, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usdc_tr_t0, us_debt_ceiling, theater_ratio, 0, 0.65).
narrative_ontology:measurement(usdc_tr_t5, us_debt_ceiling, theater_ratio, 5, 0.74).
narrative_ontology:measurement(usdc_tr_t10, us_debt_ceiling, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(usdc_be_t0, us_debt_ceiling, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(usdc_be_t5, us_debt_ceiling, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(usdc_be_t10, us_debt_ceiling, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_debt_ceiling, enforcement_mechanism).
narrative_ontology:affects_constraint(us_debt_ceiling, federal_budget_process).
narrative_ontology:affects_constraint(us_debt_ceiling, treasuries_market_stability).
narrative_ontology:affects_constraint(us_debt_ceiling, congressional_appropriations_power).

% DUAL FORMULATION NOTE:
% The debt ceiling is the immediate constraint analyzed here. Upstream constraints include the constitutional requirement to honor federal obligations (which the ceiling operationalizes through conflict), the federal spending authorization framework (which the ceiling bundles with borrowing authority), and the Treasury's cash management power (which the ceiling constrains). These three upstream constraints interact to produce the ceiling's structural configuration. Downstream effects propagate to bond markets (volatility risk), federal agencies (payment uncertainty), and Congressional leverage dynamics (policy extraction opportunities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
