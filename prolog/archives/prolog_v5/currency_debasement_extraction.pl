% ============================================================================
% CONSTRAINT STORY: currency_debasement_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_debasement_extraction, []).

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
 *   constraint_id: currency_debasement_extraction
 *   human_readable: Currency Debasement Extraction via Monetary Inflation
 *   domain: monetary_policy/political_economy
 *
 * SUMMARY:
 *   Currency debasement represents a structural constraint where central
 *   monetary authorities systematically reduce the purchasing power of a
 *   currency through monetary expansion, creating a transfer of real value
 *   from savers, wage earners, and fixed-income recipients to debt holders,
 *   asset owners, and the fiscal authority. This constraint exhibits
 *   asymmetric power distribution: those who control the monetary system
 *   (government, central banks, large asset holders) benefit from debasement
 *   while those dependent on nominal wages and savings bear the costs. The
 *   constraint operates through a dual mechanism — legitimate monetary
 *   accommodation (expanding money supply to support real economic growth)
 *   and extractive redistribution (debasement rate exceeding real economic
 *   growth, systematically eroding non-asset-holding populations' purchasing
 *   power). The opacity of the mechanism (inflation is diffuse,
 *   non-personified, and often attributed to external causes) enables high
 *   suppression despite being a policy choice. The theater ratio (0.68)
 *   reflects the extensive institutional performance around inflation
 *   management: central bank communications, inflation targeting frameworks,
 *   and forward guidance that signal control and legitimacy while the
 *   underlying extraction mechanism persists. The constraint has intensified
 *   over the measurement interval as monetary authorities have deployed
 *   expanded balance sheets and low-rate regimes, increasing the divergence
 *   between inflation and wage growth while asset values appreciate.
 *
 * KEY AGENTS:
 *   - Wage Earners: Primary victims (powerless/trapped) — receive compensation in units whose purchasing power erodes; cannot exit without bearing massive legal and social costs; no representation in monetary policy decisions
 *   - Savers: Primary victims (powerless/trapped) — purchasing power of accumulated savings systematically reduced; low-yield alternatives offer no escape; trapped in domestic currency system
 *   - Fixed-Income Recipients: Primary victims (powerless/constrained) — pensioners and annuitants with nominal-fixed payments see real income decline; cannot renegotiate; geographic mobility limited
 *   - Government Fiscal Authority: Primary beneficiary (institutional/arbitrage) — benefits from inflation as implicit taxation mechanism; enables deficit spending and reduces real debt burden; full policy discretion
 *   - Asset Owners: Primary beneficiary (powerful/arbitrage) — real assets and equities appreciate in nominal terms during inflation; portfolio diversification enables hedging; maximum exit option
 *   - Debt Holders: Primary beneficiary (powerful/arbitrage) — real value of debt contracts shrinks; can arbitrage through derivatives and currency markets; benefits from nominal price appreciation of collateral
 *   - Central Banking Institution: Institutional actor (institutional/arbitrage) — maintains performative inflation management regime; institutional identity fused with monetary policy tool; theater high, function degraded (Piton perspective)
 *   - International Trading Community: Secondary actor (organized/constrained) — both benefits (export competitiveness via weak currency) and bears costs (currency volatility, transaction costs); constrained exit from global trade network
 *   - Cryptocurrency / Alternative Currency Movement: Counter-institutional actor (organized/mobile) — building alternative payment systems as exit pathway; sees debasement as temporary problem with technological sunset
 *   - Analytical Observer: Perspectival view (analytical/analytical) — risks naturalizing policy discretion as physical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_debasement_extraction, 0.58).
domain_priors:suppression_score(currency_debasement_extraction, 0.65).
domain_priors:theater_ratio(currency_debasement_extraction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_debasement_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_debasement_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(currency_debasement_extraction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_debasement_extraction, snare).
narrative_ontology:human_readable(currency_debasement_extraction, "Currency Debasement Extraction via Monetary Inflation").
narrative_ontology:topic_domain(currency_debasement_extraction, "monetary_policy/political_economy").

domain_priors:requires_active_enforcement(currency_debasement_extraction).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_debasement_extraction, debt_holders).
narrative_ontology:constraint_beneficiary(currency_debasement_extraction, asset_owners).
narrative_ontology:constraint_beneficiary(currency_debasement_extraction, government_fiscal_authority).
narrative_ontology:constraint_victim(currency_debasement_extraction, wage_earners).
narrative_ontology:constraint_victim(currency_debasement_extraction, savers).
narrative_ontology:constraint_victim(currency_debasement_extraction, fixed_income_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped within a national currency system with minimal exit options. Receives compensation in nominal units whose purchasing power erodes via monetary expansion. Cannot arbitrage into alternative currencies without legal/logistical barriers and social cost. Experiences maximum extraction: real wages decline, purchasing power shrinks, labor bargaining power weakens. Zero degrees of freedom.
constraint_indexing:constraint_classification(currency_debasement_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAVER (SNARE) — Trapped holding currency balances or low-yielding domestic savings. Monetary expansion systematically reduces real value of accumulated savings. Cannot exit without forgoing liquidity or accepting foreign currency/commodity volatility. Extraction is coercive — the mechanism (inflation) is structural to the monetary system itself, not avoidable through market choice. Suppression is high: savings rates decline, alternatives are risky, currency controls may exist.
constraint_indexing:constraint_classification(currency_debasement_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FIXED-INCOME RECIPIENT (SNARE) — Pensioners, annuitants, and contract holders with payments fixed in nominal terms. Monetary expansion directly erodes real purchasing power of payments. Constrained exit: can supplement income but at high cost; cannot renegotiate contracts; geographic mobility is limited. Extraction is structural and relentless — passive victim of monetary policy decisions made without their consent or input.
constraint_indexing:constraint_classification(currency_debasement_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT FISCAL AUTHORITY (ROPE) — Benefits from monetary expansion as a financing mechanism: inflation reduces real value of government debt, erodes tax bracket thresholds, and enables deficit spending. Experiences the constraint as pure coordination — expanding the money supply is a mechanism for managing fiscal obligations and economic stimulus. From this perspective, inflation is a policy tool, not an extraction mechanism. Net beneficiary with full exit option (can choose inflation rate via monetary authority delegation).
constraint_indexing:constraint_classification(currency_debasement_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ASSET OWNER (ROPE) — Real assets, equity, and hard property preserve value through inflation or appreciate as nominal prices rise. Benefits from debasement through asset price appreciation and real debt reduction. Can arbitrage into alternative assets; portfolio flexibility allows hedging. Experiences the constraint as beneficial coordination — the monetary system enables asset appreciation while nominal debt burdens shrink. High exit option via diversification.
constraint_indexing:constraint_classification(currency_debasement_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DEBT HOLDER (ROPE) — Creditors benefit from inflation expectations via higher nominal interest rates, but existing debt contracts are repaid in cheaper currency. Large debt holders and banks can arbitrage through currency markets, derivatives, and portfolio rebalancing. Can exit via debt restructuring or currency diversification. Experiences constraint as leverageable opportunity — inflation reduces real debt service burden for those holding debts.
constraint_indexing:constraint_classification(currency_debasement_extraction, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL TRADING COMMUNITY (TANGLED ROPE) — Both benefits and bears costs from currency debasement. Export-oriented firms benefit from weaker home currency (improved competitiveness); import-dependent firms bear costs. Currency volatility creates coordination problems for cross-border transactions and price-setting. Constrained exit: cannot leave the global trade network; can hedge currency risk at cost. Extraction exists (volatility imposes real transaction costs) alongside coordination function (inflation can improve trade competitiveness). Organized agents can lobby for stabilization or protection, but cannot unilaterally escape currency dynamics.
constraint_indexing:constraint_classification(currency_debasement_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: CENTRAL BANKING INSTITUTION (PITON) — The monetary policy apparatus maintains inflation as a dual mechanism: nominal tool for economic stimulus and real extraction mechanism that funds government and redistributes wealth upward. Central banks perform extensive theater (inflation targeting frameworks, forward guidance, transparency committees) that signals control and legitimacy while the underlying mechanism (currency debasement) remains structurally unchanged. Theater ratio is high because the performative ritual (central bank communications) obscures the extraction function. The institutional identity has fused with the policy tool (inflation management becomes the bank's primary signal of competence) even as the original coordination function (price stability) has atrophied. Piton classification reflects degraded function maintained by institutional inertia.
constraint_indexing:constraint_classification(currency_debasement_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: CRYPTOCURRENCY / ALTERNATIVE CURRENCY MOVEMENT (SCAFFOLD) — Emerging organized response creating alternative currency systems (Bitcoin, stablecoins, community currencies) as workarounds to fiat currency debasement. Sees traditional currency extraction as temporary problem with technological sunset: distributed ledger systems can provide transparent, non-debaseable money supply and enable exit from centralized monetary authority. Mobile exit option: can migrate savings and transactions to alternative systems. Suppression exists (regulatory barriers, adoption costs, volatility risk) but declining as infrastructure matures. Scaffold classification reflects that alternative systems constitute a real exit pathway, even though deployment is incomplete and risks remain.
constraint_indexing:constraint_classification(currency_debasement_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of monetary expansion appears structurally inherent to modern economies: population growth, economic expansion, and increased transaction volume require base money growth. This perspective sees currency debasement as an unavoidable feature of scaling monetary systems, not an extractive policy choice. However, the structural data contradicts this naturalization — the base extraction value (0.58) and victim declarations reveal that debasement magnitude, timing, and distribution are contingent institutional choices, not physical limits. The mountain perspective represents a false summit: it naturalizes policy discretion as natural law.
constraint_indexing:constraint_classification(currency_debasement_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_debasement_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_debasement_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_debasement_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_debasement_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(currency_debasement_extraction, TR),
    TR >= 0.70.

:- end_tests(currency_debasement_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Currency debasement systematically transfers real purchasing power from savers and wage earners to debt holders and asset owners. The magnitude (0.58) reflects that while the mechanism is structural, the debasement rate is policy-chosen and can vary; extraction is not absolute (wage growth and savers can partially compensate through market mechanisms) but is consistent and coercive. The value increased from 0.35 to 0.58 over the interval as monetary expansion accelerated without corresponding wage growth. Suppression (0.65): High. Multiple suppression mechanisms operate simultaneously: (1) Mechanism opacity — inflation is diffuse and often attributed to external shocks rather than policy choice; (2) Legal barriers — citizens cannot easily exit the national currency system; (3) Coordination failure — savers are dispersed and unorganized while debt holders are concentrated; (4) Institutional capture — central banks are delegated authority without wage-earner representation; (5) Psychological adaptation — populations gradually adjust expectations rather than organizing resistance. Theater ratio (0.68): High. Central banks employ extensive performative apparatus: inflation targeting frameworks (0–2% targets that are regularly breached), forward guidance communications, transparency committees, and committee meeting rituals. This theater signals competence and control while the underlying extraction mechanism (debasement magnitude, timing, distribution) remains structurally unchanged. The theater increased from 0.48 to 0.68 over the interval as central bank communications became more elaborate while inflation targeting credibility declined, indicating Goodhart drift (metric substitution).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence between beneficiaries and victims. Wage earners and savers see pure extraction (Snare) — they bear 100% of the cost with no coordination benefit. Asset owners and debt holders see pure benefit (Rope) — they experience the constraint as enabling and beneficial. The government fiscal authority sees a policy tool (Rope) — inflation is a mechanism for managing fiscal obligations and stimulus. The central banking institution sees a managed process (Piton) — the ritual of inflation management becomes the institution's identity even as the underlying function (price stability) has degraded. Organized groups see temporary problems with exits (Scaffold for alternative currency movements; Tangled Rope for trading communities). The civilizational analytical observer risks seeing physical necessity (Mountain) but the structural data reveals this is a false summit — debasement magnitude and distribution are contingent policy choices. The perspectival gap reveals that the same monetary expansion process is simultaneously extraction to some agents and coordination/benefit to others, with the distribution determined by asset ownership and debt status, not by general economic needs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the debasement flow. Wage earners and savers: d ≈ 0.95 (trapped victims with no arbitrage option) → f(d) ≈ 1.42 (maximum experienced extraction). Debt holders and asset owners: d ≈ 0.08 (beneficiaries with full arbitrage option) → f(d) ≈ -0.10 (negative extraction, i.e., net benefit). Government fiscal authority: d ≈ 0.05 (institutional beneficiary) → f(d) ≈ -0.12 (net benefit). Organized groups with constrained exit (international traders, alternative currency communities): d ≈ 0.50–0.55 (mixed position) → f(d) ≈ 0.65–0.75 (moderate extraction experienced). The directionality computation from beneficiary/victim declarations and exit options produces the perspectival gap: high-d victims experience maximum extraction; low-d beneficiaries experience net benefit; mixed-d organized actors experience tangled coordination-extraction. The scope modifier σ(S) = 1.0 (national scope) does not amplify or dampen in this case.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION (ε = 0.58, suppression = 0.65, χ ≥ 0.66 at high-d perspectives): Currency debasement is classified as Snare because the primary mechanism is extraction (wealth transfer from nominal-income holders to real-asset and debt holders) with minimal coordination function. The coordination narrative ('inflation enables economic growth and stimulus') is secondary; the primary function is extraction of seigniorage and implicit taxation. Mandatrophy is resolved by distinguishing legitimate monetary accommodation (low extractiveness, genuine coordination function) from extractive debasement (high extractiveness, suppressed alternatives). Modern monetary regimes exceed the coordination threshold — debasement rates outpace real economic growth, and the beneficiaries are concentrated rather than diffuse, indicating extraction rather than coordination. The theater high (0.68) confirms Goodhart drift: central bank communications become increasingly performative as the gap between inflation targets and actual inflation widens, and as public trust in the institution declines. The Piton perspective (central banking institution viewing its own process as degraded) directly instantiates the mandatrophy resolution: when the institutional actor managing the constraint sees its own function as theater-heavy and coordin-ationally weak, the constraint has crossed from Rope or Scaffold into Snare + Piton degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_necessity_threshold,
    'What inflation rate constitutes legitimate monetary accommodation versus deliberate currency debasement extraction?',
    'Historical comparison of inflation rates across regimes; correlation between inflation and real economic growth, wage growth, and asset appreciation; counterfactual analysis of low-inflation monetary alternatives',
    'If threshold < 2%: most modern inflation regimes are extractive (Snare from more perspectives). If threshold > 5%: significant extraction is naturalised as necessary accommodation (more Mountain perspectives). If no clear threshold: extraction is policy-choice dependent and varies by political context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_necessity_threshold, conceptual, 'Threshold for distinguishing legitimate monetary accommodation from extractive debasement').

omega_variable(
    distributional_mechanism_opacity,
    'Is currency debasement experienced as constraint extraction, or is the causal mechanism between monetary expansion and individual purchasing power loss sufficiently opaque that victims do not perceive the extraction?',
    'Survey-based perception studies: do wage earners and savers attribute income/savings loss to monetary policy or other causes? Analysis of political salience: do populations organized around anti-inflation demands when inflation becomes perceptually obvious (vs. slow steady erosion)? Historical episodes of high-inflation regime transitions.',
    'If mechanism is opaque: suppression is higher than measured (victims don''t perceive the constraint) and victims lack organized exit options (can''t fight what they don''t see). If mechanism becomes salient (hyperinflation episodes): organized resistance emerges and suppression collapses. This determines whether the constraint''s stability depends on perception management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_mechanism_opacity, empirical, 'Whether debasement extraction mechanism is perceptually opaque to victims').

omega_variable(
    alternative_currency_sufficiency,
    'Can cryptocurrency or alternative currency systems provide genuine exit from fiat currency debasement, or do they remain marginal/volatile substitutes insufficient for mainstream economic participation?',
    'Longitudinal adoption rates; correlation between cryptocurrency adoption and traditional currency abandonment; stability analysis of alternative currency purchasing power over decadal timescales; regulatory barriers to alternative currency use; transaction cost comparison with fiat systems',
    'If sufficiently mature: scaffold perspective confirmed — technological sunset is real and exit option becomes mobile rather than trapped. If remains marginal: scaffold is aspirational; mainstream populations remain trapped in fiat systems. This determines whether the constraint''s long-term classification shifts from Snare toward Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_currency_sufficiency, empirical, 'Whether alternative currency systems provide sufficient exit option').

omega_variable(
    debt_reduction_versus_real_extraction,
    'For government and large corporate debt holders, does inflation reduction of real debt burden constitute genuine benefit (net-positive extraction prevention) or merely redistribution that masks continued extraction from wage earners?',
    'Comparative analysis of total factor productivity, wage growth, and real debt service across high-inflation and low-inflation regimes; measurement of whether nominal debt reduction translates to improved service capacity for debtors or merely transfers wealth upward via asset appreciation',
    'If debt reduction is net positive: inflation serves coordination function for fiscal sustainability (more Rope perspectives). If debt reduction masks upward redistribution: extraction is merely hidden by debt financialization (Snare classification holds). This determines whether government debtor benefits from inflation represent real relief or extractive theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_reduction_versus_real_extraction, empirical, 'Whether inflation benefits to debt holders represent genuine sustainability gains or extractive redistribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_debasement_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(currdb_tr_t0, currency_debasement_extraction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(currdb_tr_t5, currency_debasement_extraction, theater_ratio, 5, 0.6).
narrative_ontology:measurement(currdb_tr_t10, currency_debasement_extraction, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(currdb_be_t0, currency_debasement_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(currdb_be_t5, currency_debasement_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(currdb_be_t10, currency_debasement_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_debasement_extraction, resource_allocation).
narrative_ontology:affects_constraint(currency_debasement_extraction, wage_stagnation_trap).
narrative_ontology:affects_constraint(currency_debasement_extraction, debt_financialization_escalation).
narrative_ontology:affects_constraint(currency_debasement_extraction, asset_price_inflation_inequality).

% DUAL FORMULATION NOTE:
% Currency debasement is upstream of wage stagnation and debt financialization dynamics. The monetary constraint creates structural conditions for wage-income decoupling (workers receive nominal raises that lag inflation) and for debt-dependent consumption (consumers borrow to maintain purchasing power). All three constraints are linked via the monetary expansion mechanism but represent distinct structural dynamics with their own ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(currency_debasement_extraction, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
