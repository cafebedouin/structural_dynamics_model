% ============================================================================
% CONSTRAINT STORY: capital_flight_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capital_flight_suppression, []).

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
 *   constraint_id: capital_flight_suppression
 *   human_readable: Capital Flight Suppression: Exchange Controls and Financial Restrictions
 *   domain: political_economy/financial_regulation
 *
 * SUMMARY:
 *   Capital flight suppression through exchange controls and financial
 *   restrictions represents a structural tension between the government's
 *   need for monetary policy autonomy and macroeconomic stability
 *   (coordination benefit) and the distributional extraction from individual
 *   savers, exporters, and cross-border investors who bear the suppression
 *   cost. The constraint exhibits hybrid characteristics: genuine
 *   coordination function (preventing currency crisis, enabling seigniorage
 *   capture) coexists with asymmetric extraction through forced conversions,
 *   negative real returns, and restricted access to global capital markets.
 *   The constraint's theater_ratio (0.45) reflects that enforcement
 *   mechanisms often become ritualized: licensing procedures, documentation
 *   requirements, and approval processes consume resources without
 *   effectively preventing motivated evasion through informal channels, trade
 *   misinvoicing, or cryptocurrency. Measurement data shows theater
 *   increasing over the interval (0.25 to 0.48) as enforcement becomes more
 *   procedural and less functionally effective against sophisticated evasion.
 *   The extractiveness trajectory (0.35 to 0.62) reflects escalating
 *   enforcement intensity and widening effectiveness gap between formal rules
 *   and actual capital flows.
 *
 * KEY AGENTS:
 *   - Individual Savers: Primary victims (powerless/trapped) — face legal prohibition on currency conversion and account restrictions; trapped within national system with no legitimate exit; bear extraction through real returns erosion
 *   - Export-Oriented Firms: Secondary victims (powerful/constrained) — benefit from exchange rate stability but constrained by repatriation requirements and conversion mandates; experience both coordination benefits and extraction costs
 *   - Domestic Government: Primary beneficiary (institutional/arbitrage) — captures seigniorage, maintains monetary autonomy, prevents reserve depletion; experiences constraint as low-cost infrastructure for fiscal spending
 *   - Central Bank: Enforcement institution (institutional/constrained) — implements controls as coordination tool but politically pressured to liberalize; constrained exit through institutional dependency
 *   - Regional Trade Bloc: Organized actors (organized/mobile) — perceive capital controls as temporary crisis measure with sunset as regional integration deepens; can negotiate exit terms
 *   - Legacy Enforcement Apparatus: Institutional inertia (institutional/arbitrage) — maintains procedural rituals that reduce functional effectiveness while persisting through bureaucratic entrenchment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes genuine hybrid of coordination and extraction without naturalizing either
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_flight_suppression, 0.58).
domain_priors:suppression_score(capital_flight_suppression, 0.72).
domain_priors:theater_ratio(capital_flight_suppression, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_flight_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(capital_flight_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(capital_flight_suppression, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_flight_suppression, tangled_rope).
narrative_ontology:human_readable(capital_flight_suppression, "Capital Flight Suppression: Exchange Controls and Financial Restrictions").
narrative_ontology:topic_domain(capital_flight_suppression, "political_economy/financial_regulation").

domain_priors:requires_active_enforcement(capital_flight_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_flight_suppression, domestic_government).
narrative_ontology:constraint_beneficiary(capital_flight_suppression, protected_domestic_industries).
narrative_ontology:constraint_victim(capital_flight_suppression, individual_savers).
narrative_ontology:constraint_victim(capital_flight_suppression, export_oriented_firms).
narrative_ontology:constraint_victim(capital_flight_suppression, cross_border_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL SAVER (SNARE) — Faces legal prohibition on currency conversion and account freezes for unauthorized transfers. Trapped within national financial system with no legitimate exit mechanism. Bears full extraction cost through currency devaluation and negative real returns while unable to protect savings through diversification. Maximum suppression: legal barriers, enforcement infrastructure, and information asymmetry prevent escape.
constraint_indexing:constraint_classification(capital_flight_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXPORT-ORIENTED FIRM (TANGLED ROPE) — Genuine coordination function: capital controls prevent currency volatility that disrupts export pricing. But simultaneously experiences extraction through forced repatriation requirements, mandatory conversion at unfavorable rates, and restricted dividend distribution. Constrained exit: legal requirements and transaction costs prevent capital relocation despite having organizational resources. Both benefits and costs are structural.
constraint_indexing:constraint_classification(capital_flight_suppression, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC GOVERNMENT (ROPE) — Primary beneficiary experiencing capital controls as pure coordination mechanism: stabilizes currency, prevents reserve depletion, maintains monetary policy autonomy, and enables public spending without interest-rate spiral. Experiences the constraint as infrastructure with low cost. Net flows run toward this agent through seigniorage capture and retained foreign exchange reserves.
constraint_indexing:constraint_classification(capital_flight_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL TRADE BLOC (SCAFFOLD) — Organized actors (IMF coordination, ASEAN harmonization, EU-style monetary union) see capital controls as temporary crisis measures with sunset clauses. The constraint is perceived as having limited duration — as regional integration deepens and macroeconomic stabilization improves, capital account liberalization becomes both feasible and coordinated. Sunset timeline: 5-15 years depending on stabilization success. Low effective extraction because organized agents see exit pathway and can negotiate terms.
constraint_indexing:constraint_classification(capital_flight_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: CENTRAL BANK (TANGLED ROPE) — Enforcement institution experiencing dual role: benefits from capital controls (tools for monetary policy and reserve management) but constrained by political pressure to liberalize and by coordination costs of maintaining the enforcement apparatus. Active enforcement required; genuine coordination function (currency stability) coexists with asymmetric extraction from restricted agents. Constrained exit through political dependency and institutional path-dependence.
constraint_indexing:constraint_classification(capital_flight_suppression, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY APPARATUS (PITON) — Historical capital controls from post-colonial independence or Cold War containment persist through institutional inertia long after their original justification has eroded. Theater ratio high (0.45): regulations require documentation, licensing, and approval procedures that consume resources but don't effectively prevent motivated capital flight through informal channels, trade misinvoicing, or cryptocurrency. The ritual persists because removing it signals weakness; the apparatus persists because it's bureaucratically entrenched.
constraint_indexing:constraint_classification(capital_flight_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, capital controls are a genuine hybrid: they provide real coordination benefits (monetary autonomy, exchange rate stability, prevention of boom-bust cycles) alongside real extraction (wealth taxation through inflation, forced conversions, opportunity costs). No perspective naturalizes this as immutable law — all agents recognize the constraint as contingent policy choice. The extractiveness value reflects true structural mix: neither pure coordination nor pure predation.
constraint_indexing:constraint_classification(capital_flight_suppression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_flight_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_flight_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_flight_suppression, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_flight_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capital_flight_suppression, TR),
    TR >= 0.70.

:- end_tests(capital_flight_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Capital controls extract wealth from individual savers through seigniorage capture (government spending financed by suppressed capital outflows) and forced conversions at unfavorable rates. However, extractiveness is not maximal because some domestic agents benefit from currency stability and because export-oriented firms experience genuine coordination benefits alongside extraction costs. The trajectory from 0.35 to 0.62 reflects escalation: controls become more severe as macroeconomic pressure builds (currency depreciation, inflation acceleration), forcing government to tighten enforcement. Suppression (0.72): High. Legal barriers (prohibition on currency conversion, account freezes, mandatory licensing) create structural impossibility of exit for individual savers. But sophisticated agents can evade through informal channels, trade misinvoicing, or capital relocation, so effective suppression is heterogeneous by agent capability. Theater ratio (0.45): Moderate. Enforcement procedures (foreign exchange licenses, documentation requirements, approval delays) are partly functional (information gathering, fraud detection) and partly performative (ritual compliance, bureaucratic gatekeeping). The increasing theater (0.25 to 0.48) reflects degradation: as evasion becomes more sophisticated, procedures become more elaborate while effectiveness declines, creating a hallmark pattern of theater substitution for functional control.
 *
 * PERSPECTIVAL GAP:
 *   The constraint classifies as Snare from the powerless individual saver perspective (maximum extraction, no exit, full suppression) but as Rope from the government perspective (pure coordination, net benefit, low-cost infrastructure). Export-oriented firms see Tangled Rope (mixed coordination and extraction, constrained but not trapped). This perspectival gap reflects genuine distributional asymmetry: the constraint benefits government and some protected sectors while harming individual savers and capital-intensive exporters. The regional organized actors see Scaffold (temporary measure with sunset as stabilization succeeds), while the analytical observer sees Tangled Rope (genuine hybrid at civilizational scale). The key insight: no perspective perceives capital controls as immutable natural law. All recognize them as contingent policy choice — the disagreement is whether the choice benefits or harms them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from structural relationship to the constraint. The government is a beneficiary with arbitrage-level exit options (can liberalize capital account at will) — derives low d value (beneficial position) producing negative χ in analysis. Individual savers are victims with trapped exit options (no legal mechanism to escape) — derive high d values (maximum targeting) producing high χ (high experienced extraction). Export-oriented firms are mixed: benefit from exchange rate stability coordination but are victimized by repatriation mandates; constrained exit (high costs to relocate) moderates d value below full-victim status. The Central Bank is an enforcement institution constrained by political pressure — derives moderate d value reflecting mixed institutional position. The scaffold perspective's organized regional actors have mobile exit options (can coordinate multilateral liberalization) — derive low d values despite some victim-like characteristics because they have genuine agency and negotiating power.
 *
 * MANDATROPHY ANALYSIS:
 *   CAPITAL CONTROLS AS MANDATROPHY EXEMPLAR: The mandate (suppress capital flight to preserve macroeconomic stability and government fiscal space) creates the trophy (individuals trapped in losing currencies, firms unable to optimize capital allocation, wealth destruction through seigniorage). This constraint demonstrates why mandatrophy matters: governments implementing capital controls believe they are creating coordination benefits (preventing runs on reserves, maintaining currency value, enabling counter-cyclical policy). But the mechanism for preventing capital flight is extraction from trapped savers, and this extraction reveals the structural hybrid: coordination and extraction are not separable. The government stabilizes the macroeconomy partly by genuine coordination (preventing boom-bust cycles, managing current account flows) and partly by predation (capturing inflation tax from savers who cannot exit). Removing the mandate (liberalizing capital account) risks trophy loss (currency volatility, inflation acceleration) if macroeconomic fundamentals are weak. The resolution lies in the scaffold perspective: the constraint should carry a sunset clause contingent on achieving macroeconomic stabilization (inflation target, current account balance). Once stabilization is real, coordinated regional liberalization becomes feasible, and the extraction mechanism loses justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_vs_evasion_equilibrium,
    'What is the true compliance rate of capital controls versus the reported legal compliance rate?',
    'Analysis of informal capital flows (hawala transfers, trade misinvoicing, cryptocurrency adoption, cash smuggling) compared to measured capital account deficits; cross-border transaction tracing',
    'If true evasion rate > 40 percent: suppression metric is overestimated (many agents are not actually trapped); reclassify from Snare to Tangled Rope or Rope for significant victim cohorts. If evasion < 20 percent: suppression is real and underestimated; extraction mechanism is more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_evasion_equilibrium, empirical, 'True compliance rate of capital controls versus reported rates').

omega_variable(
    macroeconomic_benefit_realization,
    'Do capital controls actually achieve their stated macroeconomic objectives (exchange rate stability, reserve accumulation, monetary policy autonomy) or do they simply delay adjustment while creating distortions?',
    'Time-series comparison of countries with vs without capital controls; impulse response analysis of shocks; measurement of exchange rate volatility, reserve sustainability, and inflation differentials pre/post implementation',
    'If benefits realized: extractiveness may be justified as legitimate coordination cost. If benefits are illusory: extractiveness represents pure extraction disguised as necessity. Classification may shift from Tangled Rope to Snare if coordination function disappears.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(macroeconomic_benefit_realization, empirical, 'Whether capital controls achieve stated macroeconomic objectives').

omega_variable(
    distributional_incidence_heterogeneity,
    'Do capital controls fall uniformly on all agents or do wealthy/connected agents escape through informal channels while powerless agents bear full suppression?',
    'Cross-sectional analysis of capital flight patterns by wealth quintile, sector, and political connection; comparison of effective rates of extraction across agent types',
    'If highly heterogeneous: effective suppression is lower for beneficiaries and institutional actors, higher for individuals; this implies separate constraint stories (one for each agent class) rather than a single uniform constraint. If uniform: suppression metric is accurate across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_incidence_heterogeneity, empirical, 'Distributional heterogeneity of capital control incidence').

omega_variable(
    alternative_policy_counterfactual,
    'Would the macroeconomic outcomes sought by capital controls be achievable through alternative instruments (fiscal consolidation, monetary credibility, sectoral protection, targeted infant-industry support) without the suppression cost?',
    'Historical comparison of countries that achieved similar outcomes through different policy mixes; structural econometric modeling of policy substitution elasticity',
    'If alternatives are feasible: capital controls are revealed as politically easier rent-seeking mechanism than genuine stabilization tool. Extractiveness-to-coordination ratio shifts downward. If alternatives are infeasible: coordination benefit is higher than skeptics claim. Classification stability increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_policy_counterfactual, conceptual, 'Feasibility of alternative policy instruments for achieving control objectives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_flight_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capflight_tr_t0, capital_flight_suppression, theater_ratio, 0, 0.25).
narrative_ontology:measurement(capflight_tr_t3, capital_flight_suppression, theater_ratio, 3, 0.38).
narrative_ontology:measurement(capflight_tr_t6, capital_flight_suppression, theater_ratio, 6, 0.45).
narrative_ontology:measurement(capflight_tr_t9, capital_flight_suppression, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(capflight_be_t0, capital_flight_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(capflight_be_t3, capital_flight_suppression, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(capflight_be_t6, capital_flight_suppression, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(capflight_be_t9, capital_flight_suppression, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_flight_suppression, resource_allocation).
narrative_ontology:affects_constraint(capital_flight_suppression, currency_crisis_prevention).
narrative_ontology:affects_constraint(capital_flight_suppression, seigniorage_dependency).
narrative_ontology:affects_constraint(capital_flight_suppression, financial_market_fragmentation).

% DUAL FORMULATION NOTE:
% Capital flight suppression is downstream of currency crises (upstream: constraint on currency stability) and enables government fiscal dependency on seigniorage (downstream: constraint on monetary policy autonomy). These form a constraint family where each story has distinct ε values reflecting their structural position: currency stability constraint has lower ε (genuine coordination), seigniorage dependency has higher ε (extraction wrapped in macro necessity), capital suppression sits between (hybrid Tangled Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capital_flight_suppression, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
