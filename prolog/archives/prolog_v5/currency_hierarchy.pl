% ============================================================================
% CONSTRAINT STORY: currency_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_hierarchy, []).

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
 *   constraint_id: currency_hierarchy
 *   human_readable: Currency Hierarchy and Asymmetric Monetary Extraction
 *   domain: economic/monetary_systems/geopolitics
 *
 * SUMMARY:
 *   The currency hierarchy represents a global structural constraint on
 *   monetary autonomy and capital flows, centered on the dominance of reserve
 *   currencies (primarily the US dollar, secondarily the euro and yen) in
 *   international settlement, pricing, and debt denomination. This constraint
 *   exhibits genuine coordination functions—standardized media of exchange
 *   reduce transaction costs and enable global trade—while simultaneously
 *   functioning as an extraction mechanism that concentrates monetary
 *   seigniorage and geopolitical leverage in reserve currency issuers and
 *   penalizes peripheral economies through currency risk, capital flight
 *   during crises, and debt trap cycles. The constraint has evolved over 60
 *   years from a formally fixed system (Bretton Woods) through managed
 *   flexibility to the current floating regime, yet the hierarchy has
 *   actually intensified: dollar dominance in settlement has grown, not
 *   declined; peripheral states have become more integrated into
 *   dollar-denominated debt; and alternative currencies have repeatedly
 *   failed to achieve meaningful substitution. The rise of emerging
 *   alternatives (BRICS currency initiatives, blockchain settlement, Chinese
 *   digital yuan) represents the first genuine organized challenge to dollar
 *   hegemony, creating a perspectival split: scaffold agents see a temporary
 *   constraint with a sunset clause; institutional beneficiaries see an
 *   immutable coordination system; peripheral states see an inescapable trap.
 *
 * KEY AGENTS:
 *   - Peripheral States: Primary victims (powerless/trapped) — lack currency reserves, face capital flight during crises, trapped in foreign-denominated debt; no exit from import dependency on reserve currency zones
 *   - Commodity Exporters: Organized victims (organized/constrained) — export prices set in foreign currency, subject to dollar appreciation cycles; can negotiate but cannot exit
 *   - Developing Country Populations: Powerless victims (powerless/trapped) — bear inflation costs during currency crises, asset losses from capital flight, wage compression from currency devaluation
 *   - Reserve Currency Issuer: Primary beneficiary (institutional/arbitrage) — central issuer captures seigniorage, leverages currency for geopolitical coercion, provides coordination function
 *   - High-Credit Institutional Actors: Secondary beneficiaries (institutional/constrained) — multinational corporations, elite central banks benefit from capital access and favorable rates; also participate in extraction
 *   - Emerging Currency Coalitions: Organized challengers (organized/mobile) — BRICS nations, digital currency initiatives, bilateral settlement schemes building alternative pathways; see sunset clause
 *   - Bretton Woods Institutions: Institutional enforcers (institutional/arbitrage) — IMF, World Bank maintain hierarchy through structural adjustment and surveillance; power degrading (piton classification)
 *   - Analytical Observer: Neutral analyst (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to monetary systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_hierarchy, 0.58).
domain_priors:suppression_score(currency_hierarchy, 0.62).
domain_priors:theater_ratio(currency_hierarchy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_hierarchy, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_hierarchy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(currency_hierarchy, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_hierarchy, tangled_rope).
narrative_ontology:human_readable(currency_hierarchy, "Currency Hierarchy and Asymmetric Monetary Extraction").
narrative_ontology:topic_domain(currency_hierarchy, "economic/monetary_systems/geopolitics").

domain_priors:requires_active_enforcement(currency_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_hierarchy, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(currency_hierarchy, high_credit_rating_states).
narrative_ontology:constraint_beneficiary(currency_hierarchy, multinational_financial_institutions).
narrative_ontology:constraint_victim(currency_hierarchy, peripheral_states).
narrative_ontology:constraint_victim(currency_hierarchy, commodity_exporters).
narrative_ontology:constraint_victim(currency_hierarchy, developing_country_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL STATE POPULATIONS (SNARE) — Trapped in foreign currency debt traps. Local currency collapses during capital flight; dollar/euro denominated debt becomes unpayable; no exit from import dependency on reserve currency zones. Maximum extraction with minimal coordination function visible.
constraint_indexing:constraint_classification(currency_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMODITY EXPORTING STATES (TANGLED ROPE) — Coordinate global trade through dollar pricing, but face extraction: commodity prices set in foreign currency, subject to dollar appreciation cycles. Organized agents (OPEC, etc.) can negotiate but exit costs remain high. Genuine coordination function (standardized pricing) coexists with asymmetric extraction.
constraint_indexing:constraint_classification(currency_hierarchy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RESERVE CURRENCY ISSUER (ROPE) — Experiences the constraint as pure coordination: dollar system enables global settlement, reduces transaction costs. Issuer benefits from seigniorage and geopolitical leverage but also provides genuine settlement function. Net beneficiary with exit optionality via currency substitution threats.
constraint_indexing:constraint_classification(currency_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-CREDIT INSTITUTIONAL ACTORS (TANGLED ROPE) — Central banks, multinational corporations operating in reserve currency zones. Benefit from capital access and favorable borrowing rates (coordination function) while also extracting from others through financial market positioning. Both beneficiaries and participants in extraction mechanism.
constraint_indexing:constraint_classification(currency_hierarchy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EMERGING ALTERNATIVE CURRENCY COALITIONS (SCAFFOLD) — BRICS, digital currency initiatives, bilateral trade settlement mechanisms represent temporary scaffolding to coordinate around alternatives. These agents see the hierarchy as a temporary feature with a sunset clause: as alternatives mature (blockchain settlement, regional currency unions), the dollar's enforcement power declines. Low extraction because coalition agents have exit pathways.
constraint_indexing:constraint_classification(currency_hierarchy, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY BRETTON WOODS INSTITUTIONS (PITON) — IMF, World Bank maintain the currency hierarchy through structural adjustment conditions, surveillance, and normative authority. But their functional power has degraded: emerging central banks operate independent forex markets, capital controls persist despite IMF pressure, and alternative settlement mechanisms bypass IMF oversight. Theater ratio high because surveillance rituals continue despite reduced enforcement capacity. Institution persists through inertia and because no unified alternative enforcement mechanism exists yet.
constraint_indexing:constraint_classification(currency_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL NECESSITY VIEW (MOUNTAIN) — From a systems perspective, some currency hierarchy appears inevitable: coordinated settlement requires some medium of exchange; network effects create winner-take-most dynamics; foreign exchange markets have genuine coordination costs. This perspective risks naturalizing contingent institutional arrangements (dollar dominance, capital account liberalization, unilateral monetary policy power) as inherent to monetary systems. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(currency_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_hierarchy, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(currency_hierarchy, TR),
    TR >= 0.70.

:- end_tests(currency_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The hierarchy extracts significant value through seigniorage (revenue from money creation), currency crisis cascades, and capital flight mechanisms. But extraction is not total because genuine coordination functions exist—settlement efficiency, reduced transaction costs, standardized pricing—that deliver real benefits to all participants including peripheral economies. The measure reflects that extraction coexists with coordination; the hierarchy is not a pure Snare because it does solve coordination problems. Suppression (0.62): High. Significant structural barriers to exit include: (1) network effects make alternative currencies harder to reach critical mass, (2) foreign exchange reserves are locked into reserve currencies, (3) capital controls face IMF pressure and market discipline, (4) debt in foreign currency is sticky, (5) switching costs for trading systems are high. These are not total barriers (some states use capital controls; some alternative mechanisms gain traction) but they are substantial. Theater ratio (0.45): Moderate. Bretton Woods institutions maintain substantial ritual activity (surveillance missions, structural adjustment programs, peer reviews) whose functional effect on actual enforcement is disputed. But the hierarchy relies less on theatrical enforcement than on genuine network effects and structural incentives—the theater is real but not dominant.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is geographic and structural: the same institution (say, Mexico's central bank) experiences the constraint as Tangled Rope because it both coordinates with the global dollar system and faces extraction through currency cycles. A peripheral state government experiences Snare. A Mexican multinational corporation experiences Rope (can arbitrage currency differentials). A US central bank experiences Rope (pure coordination benefit). The gap reveals that 'the currency hierarchy' is not one constraint but a presheaf of constraints over different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: power level, exit options, and relationship to extraction flows. Reserve currency issuers with arbitrage options experience low d (beneficiaries with exit); peripheral states with trapped status experience high d (victims without exit); organized commodity exporters with constrained exit experience moderate-high d (victims with limited options). The pipeline applies sigmoid f(d) to convert position to experienced chi. Institutional actors in high-credit positions benefit from the hierarchy (low d) while also extracting from others through financial positioning (they appear as both beneficiary and extractor). This is captured by the Tangled Rope classification: they experience mixed directionality depending on which relationship within the constraint is being measured.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the hierarchy is a genuine Tangled Rope at the analytical level: it coordinates global settlement (requiring beneficiary + victim declarations, active enforcement) while asymmetrically extracting from peripheral economies (requiring organized victims who experience non-maximum extraction, suggesting constrained rather than trapped exit). The false summit (mountain view) is particularly tempting here because monetary coordination genuinely has limits (you need *some* medium, network effects are real) and because dominant institutions naturalize their specific arrangement as inevitable. The mandatrophy resolution is: network effects are real and create winner-take-most dynamics, BUT the winner could be different (euro, yuan, or multilateral basket) and the current winner captures additional extraction through policy choices (capital account liberalization requirements, unilateral sanctions power, favorable interest rates). The hierarchy is not a law of nature; it is an institutional arrangement with genuine coordination functions plus contingent extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_source,
    'Is the currency hierarchy enforced primarily by military/geopolitical coercion, by network effects and coordination convenience, or by structural economic dependency?',
    'Historical counterfactual analysis: would the hierarchy persist if military enforcement were removed? How much substitution occurs when alternatives reach coordination threshold (e.g., crypto, CIPS, SWIFT alternatives)?',
    'If coercion-dominant: Snare classification extends further up power spectrum. If network-effects-dominant: Rope classification strengthens; alternative currency adoption is harder than it appears. If dependency-dominant: Tangled Rope is correct; agents are trapped partly by their own structural position, not just by external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_source, empirical, 'Primary enforcement mechanism for currency hierarchy').

omega_variable(
    alternative_currency_scalability,
    'Can decentralized or multilateral currency alternatives (blockchain, digital yuan, CIPS) scale to replace reserve currency functions without creating new hierarchies?',
    'Technical analysis of settlement speed, transaction costs, and liquidity for alternative systems; measurement of substitution rate as alternatives mature; analysis of power concentration in alternative systems.',
    'If alternatives can scale without new hierarchy: Scaffold sunset is real and accelerating. If alternatives create new hierarchy: cycle repeats; current peripheral states exit to new extraction scheme. If neither: currency hierarchy is structural equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_currency_scalability, empirical, 'Whether alternatives can replace hierarchy without recreating it').

omega_variable(
    seigniorage_vs_coordination_decomposition,
    'What proportion of reserve currency benefits flow from actual coordination functions (settlement, transaction cost reduction) versus extractive seigniorage (monetary expansion benefits)?',
    'Accounting for net seigniorage flows; measurement of settlement efficiency gains from standardization; comparison of transaction costs to counterfactual multi-currency system.',
    'If seigniorage-dominant (>70%): pure Snare even for nominal beneficiaries. If coordination-dominant (>60%): Rope classification is correct and hierarchy is legitimate coordination. If balanced: Tangled Rope is accurate representation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_vs_coordination_decomposition, empirical, 'Decomposition of seigniorage benefits from genuine coordination').

omega_variable(
    capital_controls_effectiveness,
    'Do peripheral states retain effective exit options through capital controls and local currency preservation, or are these strategies futile against hierarchy?',
    'Empirical analysis of growth outcomes for countries with active capital controls vs open capital accounts; measurement of currency stability for countries maintaining local money markets.',
    'If effective: exit is constrained but not trapped; classification upgrades from Snare to Tangled Rope or Scaffold for these agents. If futile: trapped classification confirmed; integration into hierarchy is inescapable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_controls_effectiveness, empirical, 'Whether capital controls provide effective exit strategy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_hierarchy, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curr_hier_tr_t0, currency_hierarchy, theater_ratio, 0, 0.32).
narrative_ontology:measurement(curr_hier_tr_t20, currency_hierarchy, theater_ratio, 20, 0.4).
narrative_ontology:measurement(curr_hier_tr_t40, currency_hierarchy, theater_ratio, 40, 0.45).
narrative_ontology:measurement(curr_hier_tr_t60, currency_hierarchy, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(curr_hier_be_t0, currency_hierarchy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(curr_hier_be_t20, currency_hierarchy, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(curr_hier_be_t40, currency_hierarchy, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(curr_hier_be_t60, currency_hierarchy, base_extractiveness, 60, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_hierarchy, resource_allocation).
narrative_ontology:boltzmann_floor_override(currency_hierarchy, 0.18).
narrative_ontology:affects_constraint(currency_hierarchy, sovereign_debt_crisis_cycle).
narrative_ontology:affects_constraint(currency_hierarchy, capital_flight_dynamics).
narrative_ontology:affects_constraint(currency_hierarchy, forex_market_efficiency).
narrative_ontology:affects_constraint(currency_hierarchy, monetary_policy_autonomy).

% DUAL FORMULATION NOTE:
% Currency hierarchy as global monetary coordination mechanism (Rope perspective) vs. currency hierarchy as extraction machine (Snare perspective) represent genuinely different structurally-analyzed constraints. The rope view measures the coordination function (settlement efficiency, transaction cost reduction); the snare view measures the extraction function (seigniorage asymmetry, capital flight amplification). Both are true from their respective measurement positions. The tangled_rope classification at the analytical level integrates both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(currency_hierarchy, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
