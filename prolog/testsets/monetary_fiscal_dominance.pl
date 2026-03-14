% ============================================================================
% CONSTRAINT STORY: monetary_fiscal_dominance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_fiscal_dominance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: monetary_fiscal_dominance
 *   human_readable: Monetary-Fiscal Dominance in Sovereign Debt Management
 *   domain: macroeconomic_policy/monetary_fiscal_coordination
 *
 * SUMMARY:
 *   Monetary-fiscal dominance describes a structural constraint where the
 *   fiscal authority (treasury/government) gains de facto control over
 *   monetary policy by accumulating debt levels that force the central bank
 *   to monetize (accommodate) deficits to prevent default or currency
 *   collapse. The constraint exhibits all six DR types depending on
 *   structural position. The same phenomenon—central bank absorption of
 *   government debt—appears as an extractive snare on savers and CB
 *   technicians, a coordination rope for fiscal authorities managing
 *   multi-period budgets, a degraded institutional piton masking dominance
 *   under independence rhetoric, a temporary scaffold with possible sunset
 *   through credibility restoration, a tangled hybrid for international
 *   creditors caught between yield-seeking and default risk, and possibly a
 *   false-summit natural law to observers who treat fiat currency tension as
 *   inevitable. The extractiveness trajectory (0.35→0.62 over 15 periods)
 *   reflects dominance onset and accumulation: early monetary accommodation
 *   (legitimate coordination) gradually hardens into systematic suppression
 *   of CB autonomy as debt levels rise. Theater ratio increases (0.32→0.55)
 *   as central bank independence rhetoric diverges from operational
 *   subordination—performative language about independence persists while
 *   actual behavior becomes determined by fiscal needs.
 *
 * KEY AGENTS:
 *   - Fiscal Authority (Finance Ministry): Primary beneficiary (powerful/arbitrage) — can issue debt at lower rates via CB accommodation; has exit options including tax increases, spending cuts, or institutional reform
 *   - Household Savers: Primary victim (powerless/trapped) — trapped in currency and financial system; bear inflation and depreciation costs through purchasing power loss
 *   - Central Bank Technocrat: Secondary victim (moderate/constrained) — structurally constrained by price stability mandate but operationally coerced by fiscal dominance; cannot exit without political crisis
 *   - International Creditor Coalition: Secondary victim (organized/constrained) — benefits from high initial yields but constrained by network effects; cannot coordinate collective exit without signaling loss of confidence or realizing losses
 *   - Central Bank Independence Norm: Institutional doctrine (institutional/constrained) — persists performatively even as functional substance erodes; maintained through rhetoric despite behavioral subordination
 *   - Fiscal Rules Framework: Institutional scaffolding (organized/constrained) — coordinates monetary-fiscal authority with sunset logic; effectiveness depends on credibility restoration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing fiat currency tension as inevitable law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_fiscal_dominance, 0.62).
domain_priors:suppression_score(monetary_fiscal_dominance, 0.68).
domain_priors:theater_ratio(monetary_fiscal_dominance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_fiscal_dominance, extractiveness, 0.62).
narrative_ontology:constraint_metric(monetary_fiscal_dominance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(monetary_fiscal_dominance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_fiscal_dominance, tangled_rope).
narrative_ontology:human_readable(monetary_fiscal_dominance, "Monetary-Fiscal Dominance in Sovereign Debt Management").
narrative_ontology:topic_domain(monetary_fiscal_dominance, "macroeconomic_policy/monetary_fiscal_coordination").

domain_priors:requires_active_enforcement(monetary_fiscal_dominance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_fiscal_dominance, fiscal_authority).
narrative_ontology:constraint_beneficiary(monetary_fiscal_dominance, debt_servicing_government).
narrative_ontology:constraint_victim(monetary_fiscal_dominance, central_bank_independence).
narrative_ontology:constraint_victim(monetary_fiscal_dominance, price_stability_objective).
narrative_ontology:constraint_victim(monetary_fiscal_dominance, household_savers).
narrative_ontology:constraint_victim(monetary_fiscal_dominance, currency_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLD SAVER (SNARE) — Trapped in currency and financial system with no exit. Bears inflation and currency debasement costs as fiscal authority forces central bank to monetize debt. No choice but to absorb purchasing power loss through savers' effective tax on nominal wealth. Maximum extraction from structural position.
constraint_indexing:constraint_classification(monetary_fiscal_dominance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CENTRAL BANK TECHNOCRAT (SNARE) — Structurally constrained by mandate to maintain price stability but operationally coerced by fiscal dominance. Cannot exit the constraint without political crisis. Experiences the squeeze: forced to monetize debt while blamed for inflation outcomes. High extraction despite institutional position — the constraint inverts institutional hierarchy.
constraint_indexing:constraint_classification(monetary_fiscal_dominance, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL AUTHORITY (ROPE) — Coordinates legitimate public spending needs with central bank accommodation. Experiences constraint as coordination mechanism solving multi-period budget smoothing: issue debt, central bank provides liquidity, fiscal authority can fund essential services without immediate taxation. Net beneficiary with mobile exit options (can raise taxes, reduce spending, seek alternative financing).
constraint_indexing:constraint_classification(monetary_fiscal_dominance, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL CREDITOR COALITION (TANGLED ROPE) — Organized actors (foreign central banks, institutional investors, currency speculators) benefit from initial debt accumulation and high yields but face loss if fiscal dominance deteriorates into hyperinflation or default. Constrained by network effects: cannot exit individually without signaling loss of confidence; must coordinate collective exit or accept write-downs. Mixed experience of coordination (debt market maturity transformation) and extraction (realized through currency depreciation if dominance persists).
constraint_indexing:constraint_classification(monetary_fiscal_dominance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCE MINISTRY (TANGLED ROPE) — Primary beneficiary with arbitrage exit (can shift to alternative financing, austerity, or institutional reform). Coordinates legitimate fiscal needs while benefiting from central bank accommodation that reduces borrowing costs. Active enforcement required: finance ministry must convince central bank to absorb debt. Asymmetric extraction flows toward fiscal authority; central bank bears suppressed agency cost.
constraint_indexing:constraint_classification(monetary_fiscal_dominance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CENTRAL BANK INDEPENDENCE NORM (PITON) — The institutional doctrine of central bank independence persists even as monetary-fiscal dominance undermines its functional meaning. The norm survives through theater: central banks declare independence while monetizing deficits, claiming technical necessity rather than subordination. Independence rhetoric is maintained (theater) while substance dissolves (functional degradation). High theater_ratio reflects this performative decoupling.
constraint_indexing:constraint_classification(monetary_fiscal_dominance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: FISCAL RULES & COORDINATION FRAMEWORK (SCAFFOLD) — Temporary institutional scaffolding (debt brakes, fiscal pacts, inflation targeting with credibility clauses) attempts to coordinate monetary and fiscal authority with sunset logic: as institutional trust rebuilds, these constraints should relax. Coordination function is genuine: rules enable market access at lower rates. But enforcement is weak and often circumvented, creating theater. Sunset depends on whether credibility restoration succeeds.
constraint_indexing:constraint_classification(monetary_fiscal_dominance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of fiscal-monetary tension may be inherent to fiat currency systems: the sovereign's power to create money and to issue debt creates an inescapable conflict between price stability and budget flexibility. This view sees dominance as a structural inevitability of sovereign currency design. However, the structural data contradicts the mountain classification — central bank independence, multi-pillar mandates, and international coordination frameworks demonstrate that dominance is contingent on institutional choice, not natural law. Engine will detect this as false summit.
constraint_indexing:constraint_classification(monetary_fiscal_dominance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_fiscal_dominance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_fiscal_dominance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_fiscal_dominance, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_fiscal_dominance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monetary_fiscal_dominance, TR),
    TR >= 0.70.

:- end_tests(monetary_fiscal_dominance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. Fiscal authority captures sustained benefit from debt accommodation (lower borrowing costs, fiscal flexibility). Extraction is not maximal (≥0.75) because central banks retain technical capacity to resist dominance through credibility (inflation targeting credibility, institutional independence) and because households can partially shield themselves through asset diversification or capital mobility. Suppression (0.68): High. Central bank independence is nominally protected by law and international norms, but fiscal dominance suppresses this protection through debt accumulation that makes resistance politically costly. Households have limited exit (cannot flee currency entirely without extreme costs). Theater ratio (0.55): Moderate-high. Central bank independence rhetoric persists (theater) while operational subordination to fiscal needs (reality) increases over time. The constraint requires performance of independence alongside actual accommodation. The theater ratio increases with dominance severity because the gap between stated mandate (price stability) and revealed behavior (debt monetization) widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification from a single set of structural parameters. The fiscal authority (powerful/arbitrage) sees coordination (rope)—they are solving a legitimate fiscal problem. The household saver (powerless/trapped) sees pure extraction (snare)—they bear all costs with no exit. The central bank (moderate/constrained) sees constraint inversion (snare)—their institutional position should protect them but dominance reverses the hierarchy. International creditors (organized/constrained) see mixed extraction and coordination (tangled rope)—the debt market is genuine coordination but dominance creates hidden extraction through inflation/depreciation. The central bank independence norm (institutional/constrained) appears as performative piton—the doctrine persists rhetorically while operational meaning erodes. The fiscal rules framework (organized/constrained) appears as sunset-constrained scaffold—temporary coordination with possible exit as credibility rebuilds. The civilizational analytical observer risks a false-summit mountain—seeing fiat currency tension as inevitable natural law rather than contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Fiscal authority: beneficiary + arbitrage exit → low d → negative χ (experienced as coordination rope). Household saver: victim + trapped exit → high d → high χ (experienced as extraction snare). Central bank: structurally institutional but operationally coerced + constrained exit → medium-high d → moderate-high χ (experienced as constrained extraction snare). International creditor: victim + constrained-by-coordination exit → high d but with organization modifier → moderate χ (experienced as tangled rope with escape-path uncertainty). The directionality values are derived from structural position (beneficiary vs victim) and exit capacity (arbitrage vs trapped vs constrained) per the framework derivation chain. The piton classification derives from the theater gate (theater_ratio 0.55 is above 0.70 threshold for piton confirmation) rather than from high experienced extraction—central bank independence persists through institutional inertia and rhetoric despite eroded functional substance.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC RESOLUTION: The mandatrophy is resolved by recognizing that monetary-fiscal dominance is a genuine tangled rope with institutional piton overlay. The tangled rope is the baseline: fiscal authority genuinely coordinates public spending (rope function) while extracting from savers and constraining CB (snare function). The piton is the secondary process: central bank independence doctrine persists through theater even as functional substance erodes. The false-summit mountain (naturalizing dominance as inherent fiat currency property) is analytically detected and rejected. The scaffold perspective is real but subsidiary: fiscal rules frameworks provide temporary coordination with possible sunset, but they are applied inconsistently and often breached, suggesting they are aspirational rather than operative in high-dominance regimes. The snare perspectives (household savers, constrained CB technicians) reveal that dominance creates fundamental asymmetries: savers cannot exit their currency, central banks cannot credibly resist without political crisis, and international creditors cannot coordinate individual exit. No single type captures the constraint—the presheaf over institutional positions (fiscal authority, central bank, savers, creditors) shows how the same structural phenomenon produces distinct experiential classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dominance_causality_direction,
    'Does fiscal dominance cause inflation and currency depreciation, or does expected inflation and currency weakness force fiscal dominance as a response to currency mismatch and capital flight?',
    'Vector autoregression and Granger causality analysis; identification of monetary policy shocks vs fiscal shocks; comparison of dominance onset timing across countries with different fiscal-monetary timing sequences',
    'If fiscal→inflation: snare classification confirmed for households and CB technicians. If inflation→fiscal dominance: classification shifts toward scaffold/rope (dominance is response to external shock, not pure extraction). If bidirectional: tangled rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dominance_causality_direction, empirical, 'Causal direction between fiscal dominance and inflation outcomes').

omega_variable(
    central_bank_autonomy_vs_subordination,
    'Is central bank accommodation of fiscal deficits a loss of independence or a legitimate coordination function within a democratic fiscal system?',
    'Comparative institutional analysis: central bank mandate texts and legislative history; interviews with CB governors on perceived constraint vs coordination; measurement of central bank policy rate divergence from inflation expectations vs fiscal need',
    'If subordination: snare for CB, extraction flow is clear. If coordination: rope for both actors, extraction is coordination cost. If ambiguous: tangled rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_bank_autonomy_vs_subordination, conceptual, 'Whether CB accommodation is subordination or legitimate fiscal coordination').

omega_variable(
    seigniorage_distribution_legitimacy,
    'Is the distribution of seigniorage (inflation tax) revenue across fiscal authority, creditors, and households legitimate public finance or illegitimate wealth transfer?',
    'Historical analysis of seigniorage-GDP ratios and revenue distribution; measurement of real wealth transfer through inflation by income cohort; comparison with alternative tax structures that could achieve same fiscal adjustment',
    'If legitimate: extractiveness score reduces to 0.35-0.45 (scaffold/rope). If illegitimate: extractiveness confirmed at 0.62+ (tangled rope/snare). Classification hinges on whether inflation is seen as policy choice or necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_distribution_legitimacy, preference, 'Legitimacy of seigniorage distribution mechanism').

omega_variable(
    international_coordination_constraint,
    'To what extent does international creditor coordination (capital flight, currency pressure, capital controls) externally constrain dominance, converting it from internal extraction into constrained coordination?',
    'Measurement of foreign creditor asset composition and exit sensitivity; modeling of capital flight thresholds; comparison of dominance persistence in capital-open vs capital-controlled economies',
    'If strong external constraint: dominance is bounded rope/scaffold (creditors have exit, which disciplines fiscal authority). If weak constraint: dominance is unbounded snare/tangled rope (creditors cannot coordinate exit, trapped by systemic risk). Shifts analysis from internal extraction to network game.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_coordination_constraint, empirical, 'International creditor coordination capacity and exit options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_fiscal_dominance, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfd_tr_t0, monetary_fiscal_dominance, theater_ratio, 0, 0.32).
narrative_ontology:measurement(mfd_tr_t5, monetary_fiscal_dominance, theater_ratio, 5, 0.43).
narrative_ontology:measurement(mfd_tr_t10, monetary_fiscal_dominance, theater_ratio, 10, 0.55).
narrative_ontology:measurement(mfd_tr_t15, monetary_fiscal_dominance, theater_ratio, 15, 0.61).

% Extraction over time
narrative_ontology:measurement(mfd_be_t0, monetary_fiscal_dominance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mfd_be_t5, monetary_fiscal_dominance, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mfd_be_t10, monetary_fiscal_dominance, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(mfd_be_t15, monetary_fiscal_dominance, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_fiscal_dominance, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_fiscal_dominance, 0.18).
narrative_ontology:affects_constraint(monetary_fiscal_dominance, central_bank_independence_doctrine).
narrative_ontology:affects_constraint(monetary_fiscal_dominance, currency_regime_credibility).
narrative_ontology:affects_constraint(monetary_fiscal_dominance, sovereign_debt_sustainability).

% DUAL FORMULATION NOTE:
% Monetary-fiscal dominance decomposes into three structurally distinct constraints: (1) institutional dominance of fiscal over monetary authority (political economy), (2) currency credibility loss through inflation expectations (macroeconomic), (3) debt sustainability across generations (fiscal). This story models the coordination-extraction hybrid at the institutional level; upstream stories capture the macroeconomic transmission and downstream stories model intergenerational fiscal extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_fiscal_dominance, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
