% ============================================================================
% CONSTRAINT STORY: sotu_2005_bush_social_security_reform_trajectory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2005_bush_social_security_reform_trajectory, []).

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
 *   constraint_id: sotu_2005_bush_social_security_reform_trajectory
 *   human_readable: Social Security Structural Reform: Transition to Market-Linked Accounts (2005)
 *   domain: social_policy/pension_reform
 *
 * SUMMARY:
 *   The 2005 Social Security restructuring proposal represents a fundamental
 *   shift from collective intergenerational risk pooling (PAYGO) to
 *   individualized market-based retirement accounts. The constraint exhibits
 *   competing structural logics: genuine actuarial problem (PAYGO insolvency
 *   under demographic pressure) justifies some reform; but the specific
 *   design (market-linked for younger workers, protected benefits for current
 *   beneficiaries 55+) redistributes longevity risk from collective pool onto
 *   individual workers while creating fee-capture opportunities for financial
 *   services. The constraint operates across all six DR types depending on
 *   observer position: snare for low-income workers with no exit from
 *   mandatory market exposure; tangled rope for moderate-income workers with
 *   genuine upside but substantial volatility risk; rope for financial
 *   services industry capturing AUM growth; mountain from the civilizational
 *   perspective that 'longevity risk must go somewhere'; piton for the
 *   degraded PAYGO system sustained by political inertia and beneficiary
 *   protection; and scaffold only if the transition includes genuine
 *   protections and sunset provisions (which the 2005 proposal lacked). The
 *   theater ratio (0.65) reflects that much public debate conflated actuarial
 *   solvency questions with ideological preferences for market mechanisms,
 *   inflating perceived urgency beyond what demographic models strictly
 *   justified.
 *
 * KEY AGENTS:
 *   - Low-income workers (ages 25-45): Primary victims (powerless/trapped) — mandatory participation in market-linked system without capital buffer to absorb volatility; bear full risk transfer from collective pool
 *   - Financial services industry: Primary beneficiaries (institutional/arbitrage) — gain direct fee flow and AUM expansion; experience reform as pure coordination solution
 *   - Middle-income workers: Secondary victims/partial beneficiaries (moderate/constrained) — gain market upside exposure but face substantial volatility risk and behavioral barriers to optimal portfolio management
 *   - Current beneficiaries (ages 55+): Protected cohort (institutional/arbitrage) — explicitly shielded from restructuring; benefit from status quo PAYGO while younger cohorts shift to market risk
 *   - PAYGO actuarial system: Institutional actor (institutional/arbitrage) — degraded coordination mechanism sustained by political protection of beneficiaries; original function (intergenerational risk pooling) eroding
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy choice (risk redistribution) as immutable law (longevity risk must migrate somewhere)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2005_bush_social_security_reform_trajectory, 0.58).
domain_priors:suppression_score(sotu_2005_bush_social_security_reform_trajectory, 0.68).
domain_priors:theater_ratio(sotu_2005_bush_social_security_reform_trajectory, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2005_bush_social_security_reform_trajectory, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2005_bush_social_security_reform_trajectory, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_2005_bush_social_security_reform_trajectory, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2005_bush_social_security_reform_trajectory, tangled_rope).
narrative_ontology:human_readable(sotu_2005_bush_social_security_reform_trajectory, "Social Security Structural Reform: Transition to Market-Linked Accounts (2005)").
narrative_ontology:topic_domain(sotu_2005_bush_social_security_reform_trajectory, "social_policy/pension_reform").

domain_priors:requires_active_enforcement(sotu_2005_bush_social_security_reform_trajectory).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2005_bush_social_security_reform_trajectory, financial_services_industry).
narrative_ontology:constraint_beneficiary(sotu_2005_bush_social_security_reform_trajectory, younger_high_earners_with_market_confidence).
narrative_ontology:constraint_victim(sotu_2005_bush_social_security_reform_trajectory, actuarial_commons_intergenerational_transfer).
narrative_ontology:constraint_victim(sotu_2005_bush_social_security_reform_trajectory, risk_averse_workers_and_low_income_cohorts).
narrative_ontology:constraint_victim(sotu_2005_bush_social_security_reform_trajectory, future_elderly_in_market_downturns).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME WORKERS (SNARE) — Trapped by mandatory participation and labor market necessity. Bears full longevity risk transfer without capital cushion to absorb market volatility. No exit: cannot opt into traditional DB system, cannot afford individual investment management, cannot delay retirement if markets crash near retirement date. Market downturns hit hardest those least able to bear losses. Maximum extraction from trapped position.
constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME WORKERS (TANGLED ROPE) — Constrained but not trapped; can partially self-insure through supplemental savings. Benefits from market upside exposure and investment control, but bears volatility risk and faces behavioral barriers to optimal portfolio management. Experiences genuine coordination benefit (retirement security through growth) alongside extraction (risk transfer from collective to individual). Mixed exit cost: professional financial advice is expensive; switching between accounts has transaction costs.
constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME WORKERS & FINANCIAL SECTOR (ROPE) — Net beneficiary with full arbitrage capability. Gains from market-linked accounts include direct fee flow to financial services, expanded AUM (assets under management), and career opportunities in account management. Experiences the reform as pure coordination: solving the 'retirement security problem' through market mechanisms. Can opt out via robust supplemental savings if market-linked system underperforms.
constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL SERVICES INDUSTRY (TANGLED ROPE) — Institutional beneficiary with genuine coordination function (asset pooling, professional management, liquidity provision) alongside substantial extraction (fee capture, customer dependency, AUM growth). Constrained exit: competitive pressure limits ability to raise fees, but oligopolistic structure and regulatory capture enable sustained extraction. Active enforcement required to maintain account segregation and fee structures.
constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL PAYGO SYSTEM (PITON) — Degraded collective insurance mechanism. The original PAYGO system (1935) solved genuine coordination problem: pooled longevity risk across cohorts, smoothed lifetime income, eliminated poverty-in-old-age. But by 2005, the system is insolvent, benefits structure is rigid, and actuarial commons is narratively delegitimized. The theater ratio (0.65) reflects that much political debate about PAYGO 'crisis' conflates genuine actuarial problems (rising life expectancy, declining worker/beneficiary ratio) with manufactured urgency (timing, magnitude of shortfall, behavioral assumptions). The piton persists through inertia — protected beneficiaries (55+) have political power to block phase-out, but the system's functional integrity has already eroded.
constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LONGEVITY RISK (MOUNTAIN) — From a civilizational perspective, longevity risk must be borne by someone. The mountain classification captures the 'immutable' aspect: risk doesn't disappear, it migrates. PAYGO collective pools risk; market-linked systems shift it to individual workers. This perspective risks naturalizing what is a policy choice as an inevitable physical/mathematical law. However, the structural data suggests false summit: beneficiaries exist (financial services, high-income workers), suggesting the 'inevitable' risk transfer is actually contingent institutional design.
constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: TRANSITIONAL REFORM COMPROMISE (SCAFFOLD) — If reform includes genuine sunset clause and staged transition with protections for low-income workers, the constraint becomes a temporary coordination mechanism. But 2005 proposal lacked clear sunset: no specified date for full transition, no mandatory reversion if market-linked system underperforms, no wealth floor for low-income beneficiaries. Scaffold logic requires that suppression declines over horizon; 2005 design shows rising suppression (younger cohorts take on more risk as transition progresses). This perspective is aspirational rather than structural — a true scaffold would include legislative triggers for reconvergence.
constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2005_bush_social_security_reform_trajectory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2005_bush_social_security_reform_trajectory, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2005_bush_social_security_reform_trajectory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2005_bush_social_security_reform_trajectory, TR),
    TR >= 0.70.

:- end_tests(sotu_2005_bush_social_security_reform_trajectory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reform transfers longevity risk from collective PAYGO pool (where it was distributed across cohorts and absorbing capacity varies with labor force growth) onto individual workers (where it concentrates by market timing and individual financial capacity). For low-income workers, this is substantial extraction: they lose guaranteed benefit structure and gain market volatility with no insurance mechanism. For high-income workers, extraction is lower: market upside plus diversification benefit partly offsets risk transfer. The aggregate extractiveness (0.58) reflects weighted average: concentration of extraction on powerless cohorts amplifies the per-capita effect. Suppression (0.68): High. Multiple barriers to rejecting or exiting the system: mandatory participation (legal), labor market necessity (economic), political protection of status quo (institutional), industry lobbying against regulation (structural). Low-income workers cannot opt out, cannot afford individual investment management, face career risk if they reduce payroll contributions. Behavioral suppression also substantial: cognitive biases in financial decision-making, limited financial literacy among lower-income cohorts, and information asymmetry between workers and financial industry. Theater ratio (0.65): Moderate-high. Political debate surrounding 'Social Security crisis' inflated urgency beyond actuarial baseline. Demographic projections showed insolvency 2042-2052 (decades away); political rhetoric framed imminent emergency. Much public discussion focused on market-mechanism ideology rather than comparative risk-bearing outcomes. The reform's packaging as 'ownership' and 'personal accounts' obscured the risk transfer mechanism. However, some genuine functional theater: PAYGO system's benefit structure is increasingly rigid, and modernizing it (payroll tax adjustment, means-testing, or alternative pooling mechanisms) requires political will that isn't purely technical.
 *
 * PERSPECTIVAL GAP:
 *   Wide perspectival gap between low-income worker (snare: trapped, bears all extraction) and financial services (rope: benefits, exits easily). Both experience the same structural reform but classify as opposite types. Middle-income workers split between snare-adjacent (constrained, faces volatility) and rope-adjacent (moderate gains from market exposure) depending on time horizon: at biographical horizon, volatility risk dominates (tangled rope); at generational horizon, upside exposure becomes salient (rope). The piton and mountain perspectives diagnose the reform as inevitable or degradation respectively, but both obscure the extraction mechanism. The scaffold perspective is aspirational rather than structural in the 2005 proposal — a genuine scaffold would include wealth floors for low-income beneficiaries, mandatory reversion if market returns underperform, and sunset triggers for reconvergence to pooled system.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chains beneficiary/victim declarations through exit options to produce d values. Low-income workers declared as victims with trapped exit → d ≈ 0.95. Financial services declared as beneficiaries with arbitrage exit → d ≈ 0.10. The chi formula then scales extractiveness by f(d) and scope modifier σ(national ≈ 1.0). For low-income workers: χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (snare threshold χ ≥ 0.66 clearly met). For financial services: χ ≈ 0.58 × (-0.05) × 1.0 ≈ -0.03 (negative extraction, rope classification). The perspectival gap is encoded in d values derived from structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy emerges between PAYGO coordination function (legitimate: collective risk pooling solves retirement income problem) and market-linked extraction (concentration of longevity risk on least able to bear it). The reformer sees coordination: markets solve the 'retirement security' problem more efficiently than PAYGO bureaucracy, enabling growth, choice, and intergenerational burden-sharing. The worker sees extraction: risk transfer without equivalent protection, fees, and behavioral barriers. The classical mandatrophy resolution would require that both framings are partially correct: PAYGO is degraded (true: demographic ratio decline is real problem), but market-linked structure doesn't fully solve it (also true: fee drag, volatility risk, behavioral barriers are substantial). A mandatrophy-resolving design would combine elements: (1) genuine market option WITH (2) guaranteed floor return at PAYGO-equivalent purchasing power, (3) fee regulation to cap extraction, (4) mandatory annuitization at retirement to restore lifetime income guarantee, (5) intergenerational rebalancing mechanism if market-linked system diverges from expected returns. The 2005 proposal lacked most of these — it was largely one-way transfer of risk and return to markets, which is why mandatrophy is unresolved: the constraint exhibits genuine coordination function (retirement income security) alongside genuine extraction (risk concentration on powerless cohorts), and the design doesn't adequately balance both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_insolvency_magnitude,
    'Is the Social Security actuarial insolvency a genuine crisis requiring structural reform, or a manageable problem solvable through contribution rate adjustment or benefit indexing?',
    'Comparison of Social Security trust fund depletion scenarios under different reform pathways (payroll tax increase, benefit adjustment, market-linked diversification); sensitivity analysis to demographic assumptions (fertility, migration, longevity gains)',
    'If crisis: market-linked reform is necessary coordination response (reduces extraction narrative). If manageable: reform is primarily redistributive (increases extraction narrative, strengthens snare classification for low-income workers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actuarial_insolvency_magnitude, empirical, 'Whether insolvency magnitude justifies structural reform vs marginal adjustment').

omega_variable(
    market_volatility_absorption_capacity,
    'Do individual workers have sufficient behavioral capacity and financial sophistication to manage market-linked retirement accounts without catastrophic losses from panic selling or poor allocation?',
    'Behavioral finance studies on household investment decisions during market downturns; international comparison of funded pension systems with worker choice (Chile, Australia); cohort analysis of investment performance by education and income level',
    'If high capacity: middle-income perspective (constrained/beneficial) correct — workers genuinely coordinate their retirement through markets. If low capacity: risk concentration on those least able to bear it (snare classification dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_volatility_absorption_capacity, empirical, 'Worker behavioral capacity to manage market risk without losses').

omega_variable(
    intergenerational_risk_equity,
    'Is the shift from collective PAYGO pooling to individual market-linked accounts ethically neutral (just different risk-bearing mechanisms) or ethically problematic (abandons risk pooling across cohorts)?',
    'Intergenerational equity analysis: simulation of retirement income outcomes across multiple market scenarios by cohort; comparison of intergenerational wealth transfer under PAYGO vs market-linked; implicit intergenerational contract analysis',
    'If neutral: framework treats reform as coordination problem with technical solution. If problematic: framework recognizes extraction from younger cohorts (breaking intergenerational contract) — strengthens snare/tangled_rope classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_risk_equity, preference, 'Ethical status of intergenerational risk transfer from collective to individual').

omega_variable(
    financial_sector_fee_capture,
    'What proportion of market-linked account returns would be captured by financial services fees, reducing effective retirement income below what PAYGO system would deliver?',
    'Fee structure analysis: comparison of aggregate fee burdens (administrative, advisory, transaction, mutual fund) across proposed account types; simulation of lifetime fee drag on retirement income by account size and asset allocation; international comparison of fee structures in privatized pension systems',
    'If fee drag < 10% returns: extraction is partial (tangled rope for financial sector). If fee drag > 15%: extraction is severe (snare classification for workers, piton for PAYGO coordination mechanism that''s being replaced by higher-cost system).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financial_sector_fee_capture, empirical, 'Fee capture impact on retirement income adequacy').

omega_variable(
    market_timing_generational_cohort_risk,
    'Are younger cohorts forced to time market entry in their 20s-40s, bearing concentration risk if they happen to retire during or after a major market downturn?',
    'Cohort retirement income simulation across market history (1980-2005 timeframe): retirement adequacy for cohorts retiring in 2000 (post-bubble), 2008 (financial crisis), etc.; comparison to PAYGO guaranteed benefits; analysis of sequence-of-returns risk',
    'If concentration risk is severe: snare classification for risk-averse workers (no exit from mandatory participation, no protection from cohort-level market timing). If adequately smoothed: tangled rope classification (some workers benefit, some lose).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_timing_generational_cohort_risk, empirical, 'Cohort-level market timing risk concentration for mandatory participation').

omega_variable(
    political_enforcement_sustainability,
    'Can market-linked account system be politically sustained if early cohorts experience large losses relative to PAYGO counterfactual (e.g., if market tanks shortly after implementation)?',
    'Historical analysis of pension privatization reversals (Argentina, Bolivia); political risk assessment of benefit revision coalitions; baseline of intergenerational bargaining breakdown',
    'If low sustainability: system contains hidden sunset clause (political reversal likely under adverse conditions) — might reclassify as scaffold with endogenous sunset trigger. If high sustainability: system is locked-in snare/tangled_rope with little exit option even if underperforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_enforcement_sustainability, empirical, 'Political sustainability of market-linked system under adverse cohort outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2005_bush_social_security_reform_trajectory, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ss_reform_tr_t0, sotu_2005_bush_social_security_reform_trajectory, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ss_reform_tr_t10, sotu_2005_bush_social_security_reform_trajectory, theater_ratio, 10, 0.62).
narrative_ontology:measurement(ss_reform_tr_t20, sotu_2005_bush_social_security_reform_trajectory, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(ss_reform_be_t0, sotu_2005_bush_social_security_reform_trajectory, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ss_reform_be_t10, sotu_2005_bush_social_security_reform_trajectory, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ss_reform_be_t20, sotu_2005_bush_social_security_reform_trajectory, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2005_bush_social_security_reform_trajectory, resource_allocation).
narrative_ontology:affects_constraint(sotu_2005_bush_social_security_reform_trajectory, payroll_tax_system_intergenerational_contract).
narrative_ontology:affects_constraint(sotu_2005_bush_social_security_reform_trajectory, defined_benefit_pension_erosion_private_sector).
narrative_ontology:affects_constraint(sotu_2005_bush_social_security_reform_trajectory, financial_industry_regulatory_capture).

% DUAL FORMULATION NOTE:
% Social Security reform decomposes into three related constraints: (1) actuarial insolvency of PAYGO system (genuine coordination problem ε ≈ 0.25); (2) structural risk redistribution via market-linked accounts (extraction mechanism ε ≈ 0.58); (3) fee capture and industry benefits (beneficiary coordination ε ≈ 0.35). This story focuses on the risk redistribution constraint (constraint #2). The actuarial insolvency constraint is upstream (cited as justification); the industry benefit constraint is embedded as beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2005_bush_social_security_reform_trajectory, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
