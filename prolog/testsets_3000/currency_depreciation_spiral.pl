% ============================================================================
% CONSTRAINT STORY: currency_depreciation_spiral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_depreciation_spiral, []).

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
 *   constraint_id: currency_depreciation_spiral
 *   human_readable: Currency Depreciation Spiral
 *   domain: economic/monetary/international
 *
 * SUMMARY:
 *   A currency depreciation spiral occurs when a country's exchange rate
 *   depreciates, raising import prices, which increases inflation and capital
 *   flight expectations, which drives further depreciation, creating a
 *   self-reinforcing cycle that traps domestic agents in collapsing
 *   purchasing power while benefiting external creditors and export-oriented
 *   actors. The spiral represents a structural extraction mechanism where
 *   macroeconomic dynamics enforce transfer of real wealth from wage earners
 *   and import-dependent sectors to foreign creditors and export-competitive
 *   producers. The constraint exhibits genuine coordination functions
 *   (managing trade competitiveness, allocating foreign exchange) alongside
 *   severe asymmetric extraction, making it a canonical tangled_rope. The
 *   theater ratio (0.38) reflects that central bank policy responses (raising
 *   interest rates, intervention in FX markets, forward guidance) operate at
 *   a meaningful level but cannot override fundamental macroeconomic
 *   imbalances — the performance of monetary control is high relative to
 *   actual control capacity.
 *
 * KEY AGENTS:
 *   - Domestic wage earners: Primary victims (powerless/trapped) — real wages collapse as import prices rise; no exit from national currency zone
 *   - Import-dependent manufacturers: Secondary victims (moderate/constrained) — face rising input costs; relocation possible but costly
 *   - Export-oriented corporations: Primary beneficiaries (institutional/arbitrage) — gain cost competitiveness; can arbitrage between depreciating home and strong foreign currencies
 *   - Foreign-currency debt holders (domestic): Victims when debt is foreign-denominated; beneficiaries when debt can be redenominated or restructured
 *   - International creditors and financial institutions: Secondary beneficiaries (organized/mobile) — real debt burdens deflate in depreciating currency; can exit through lending freezes
 *   - Currency speculators: Tertiary beneficiaries (institutional/arbitrage) — profit from timing depreciation; amplify spiral through forward positioning
 *   - Central bank: Institutional actor (institutional/arbitrage) — manages visible policy responses; theater persists as performative response to constraints it cannot fully control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_depreciation_spiral, 0.58).
domain_priors:suppression_score(currency_depreciation_spiral, 0.72).
domain_priors:theater_ratio(currency_depreciation_spiral, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_depreciation_spiral, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_depreciation_spiral, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(currency_depreciation_spiral, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_depreciation_spiral, tangled_rope).
narrative_ontology:human_readable(currency_depreciation_spiral, "Currency Depreciation Spiral").
narrative_ontology:topic_domain(currency_depreciation_spiral, "economic/monetary/international").

domain_priors:requires_active_enforcement(currency_depreciation_spiral).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_depreciation_spiral, external_creditors).
narrative_ontology:constraint_beneficiary(currency_depreciation_spiral, export_corporations).
narrative_ontology:constraint_beneficiary(currency_depreciation_spiral, currency_speculators).
narrative_ontology:constraint_victim(currency_depreciation_spiral, domestic_wage_earners).
narrative_ontology:constraint_victim(currency_depreciation_spiral, import_dependent_sectors).
narrative_ontology:constraint_victim(currency_depreciation_spiral, foreign_currency_debt_holders_domestic).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Trapped by currency depreciation with no exit option. Real wages collapse as import prices rise. Cannot switch currencies, cannot access foreign exchange, cannot exit the national monetary zone. Experiences maximum extraction as purchasing power erodes rapidly.
constraint_indexing:constraint_classification(currency_depreciation_spiral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Constrained by currency depreciation but also participates in coordination. Benefits from access to domestic markets and supply chains during depreciation, but faces rising costs for imported inputs. Partial exit through relocation is costly but possible. Experiences mixed extraction and coordination.
constraint_indexing:constraint_classification(currency_depreciation_spiral, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Benefits from currency depreciation through competitiveness gains. Can arbitrage between depreciated home currency and strong foreign currency. Coordinates export supply chains. Experiences the constraint as pure coordination benefit with minimal extraction.
constraint_indexing:constraint_classification(currency_depreciation_spiral, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized actors (IMF, World Bank, foreign creditors) see depreciation as both enforcement mechanism and coordination challenge. They benefit from deflation of real debt burdens in depreciating currency but must coordinate policy responses. Mobile exit through capital flight or lending freezes. Complex extraction-coordination mix.
constraint_indexing:constraint_classification(currency_depreciation_spiral, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Central bank policy response (raising rates, FX intervention, tightening money supply) is largely performative theater. Structural drivers of depreciation (capital flight, trade deficits, debt dynamics) override policy levers. Theater ratio high because the visible policy actions (reserve management, forward guidance) cannot stop the spiral in many contexts. Piton emerges from degraded monetary policy function.
constraint_indexing:constraint_classification(currency_depreciation_spiral, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From civilizational/universal perspective, currency depreciation spiral appears as an immutable law of macroeconomics: unsustainable debt dynamics, capital flow imbalances, and inflation differentials create mathematical inevitability. However, the structural data reveals this as false naturalization. Policy choices (debt restructuring, capital controls, import substitution) can interrupt the spiral. The mountain framing obscures contingent institutional arrangements as natural laws.
constraint_indexing:constraint_classification(currency_depreciation_spiral, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_depreciation_spiral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_depreciation_spiral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_depreciation_spiral, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_depreciation_spiral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(currency_depreciation_spiral, TR),
    TR >= 0.70.

:- end_tests(currency_depreciation_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Significant and accelerating. The spiral transfers real wealth from domestic agents to external creditors through multiple mechanisms: (1) wage erosion as import prices rise faster than nominal wages; (2) deflation of debt burdens for foreign-currency borrowers who benefit from depreciation; (3) profitability increases for exporters whose revenues are now priced in strong foreign currency. The extractiveness increases over the 6-year interval from 0.22 to 0.58 as the spiral deepens and capital flight accelerates. Suppression (0.72): High. Barriers to escape include: capital controls that may exist but are often porous; debt obligations that trap countries in spiral-intensifying policies (fiscal austerity, high interest rates); inability of wage earners to access foreign exchange; coordination failure among creditors to restructure debt. The psychological component is substantial — once depreciation expectations form, they become self-fulfilling through capital flight. Theater ratio (0.38): Moderate. Central bank policy is not purely theatrical — interest rate hikes have real effects on demand and inflation. But policy capacity is fundamentally limited by the underlying macro imbalances (current account deficits, debt dynamics, capital flight pressures). The theater has increased slightly over the interval as policy responses become more desperate and less effective.
 *
 * PERSPECTIVAL GAP:
 *   The wage earner sees a snare with no exit and no coordination benefit — pure extraction of purchasing power. The export manufacturer sees a rope with pure coordination benefit — exchange rate depreciation solves their competitiveness problem perfectly. The import manufacturer sees a tangled rope with mixed benefit and cost — they benefit from tariff protection and domestic market access but suffer from input cost increases. The central bank sees a piton — its policy levers (interest rates, FX intervention) have become ritualized responses that cannot arrest the spiral's structural drivers. The international creditor sees tangled rope with net benefit — coordinating with other creditors on restructuring terms while capturing currency appreciation arbitrage. The analytical observer at civilizational scope risks seeing a mountain (inevitable mathematical consequence of macro imbalance) but the structural data reveals this as false naturalization — policy choices (debt restructuring, capital controls, import substitution) can interrupt the spiral, making it a contingent institutional arrangement rather than natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by each agent's structural position relative to the depreciation spiral. External creditors and exporters benefit from depreciation, placing them at low d (0.05–0.20): their currencies and debt burdens are favorable. Domestic wage earners bear full cost with no countervailing benefit, placing them at high d (0.90–0.98): maximum extraction. Import manufacturers are intermediate (d ≈ 0.60), bearing some costs but gaining market protection. Central banks occupy an institutional position with arbitrage options (capital account management, FX reserves) placing them at low-to-moderate d (0.25–0.40). The sigmoid function f(d) converts these structural positions into experienced extractiveness values chi, which vary by agent perspective. An export manufacturer's analytical observer (institutional/arbitrage) computes low chi because d is low and their power is high. A wage earner's perspective (powerless/trapped) computes high chi because d is high and their power is low.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival multiplicity: all six types are legitimate readings of the same macroeconomic data. The snare classification (from powerless wage earners) reflects their genuine structural experience. The rope classification (from beneficiary exporters) reflects their genuine experience of pure coordination benefit. The tangled rope (from moderate import manufacturers) reflects genuine mixed extraction and coordination. The piton classification (from institutional central banks) reflects genuine degradation of policy function. The mountain classification (from analytical observers) is a FALSE SUMMIT — it naturalizes contingent policy choices (debt structure, capital account openness, exchange rate regime) as inevitable mathematical laws. The constraint's extractiveness has increased from 0.22 to 0.58 over the measurement interval, and this trajectory is not predetermined: policy restructuring (debt reduction, capital controls, import substitution) can interrupt the spiral. The theater ratio's stability (0.28→0.38) indicates that central bank performance is not improving despite increased effort, confirming the piton diagnosis of degraded institutional function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_denominated_currency_asymmetry,
    'Is the spiral driven by the mathematical impossibility of servicing foreign-currency debt in a depreciating currency, or by reversible policy and market psychology?',
    'Comparative analysis of debt restructuring outcomes; case studies where debt was redenominated or restructured successfully vs cases where depreciation spiraled',
    'If mathematical impossibility: mountain constraint. If reversible through restructuring: tangled_rope or snare constraint determined by political will and power asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_denominated_currency_asymmetry, empirical, 'Whether debt-currency mismatch creates mathematical inevitability').

omega_variable(
    capital_flight_endogeneity,
    'Does capital flight cause depreciation or does expected depreciation cause capital flight? Which direction is primary?',
    'Time-series analysis of capital flow timing relative to exchange rate movements; forward guidance experiments tracking asset outflows',
    'If depreciation-causes-flight: positive feedback loop (snare dynamics). If flight-causes-depreciation: policy intervention on capital controls can interrupt the spiral (tangled_rope/scaffold dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_flight_endogeneity, empirical, 'Causal direction of capital flight and currency depreciation').

omega_variable(
    international_creditor_enforcement_capacity,
    'Do external creditors actively enforce debt servicing through pressure, or does the constraint emerge autonomously from financial market dynamics?',
    'Documentary evidence of IMF/creditor conditionality; comparison of depreciation spirals with vs without formal creditor coordination',
    'If active enforcement: snare with institutional targeting (tangled_rope from creditor perspective). If autonomous market: snare is self-organizing extraction without visible enforcer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_creditor_enforcement_capacity, empirical, 'Whether external creditors actively enforce via conditionality or constraint emerges autonomously').

omega_variable(
    alternative_monetary_regime_feasibility,
    'Could currency boards, dollarization, or other institutional regimes prevent depreciation spirals?',
    'Analysis of countries with currency boards or fixed regimes; assessment of whether they face alternative binding constraints instead',
    'If feasible: depreciation spiral is contingent institutional choice (tangled_rope, not mountain). If substitutes create equivalent constraints: mountain constraint takes different form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_monetary_regime_feasibility, conceptual, 'Feasibility of alternative monetary regimes that prevent depreciation spirals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_depreciation_spiral, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(currdep_tr_t0, currency_depreciation_spiral, theater_ratio, 0, 0.28).
narrative_ontology:measurement(currdep_tr_t2, currency_depreciation_spiral, theater_ratio, 2, 0.32).
narrative_ontology:measurement(currdep_tr_t4, currency_depreciation_spiral, theater_ratio, 4, 0.36).
narrative_ontology:measurement(currdep_tr_t6, currency_depreciation_spiral, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(currdep_be_t0, currency_depreciation_spiral, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(currdep_be_t2, currency_depreciation_spiral, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(currdep_be_t4, currency_depreciation_spiral, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(currdep_be_t6, currency_depreciation_spiral, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_depreciation_spiral, resource_allocation).
narrative_ontology:affects_constraint(currency_depreciation_spiral, foreign_debt_trap).
narrative_ontology:affects_constraint(currency_depreciation_spiral, trade_deficit_persistence).
narrative_ontology:affects_constraint(currency_depreciation_spiral, capital_flight_mechanism).

% DUAL FORMULATION NOTE:
% Currency depreciation spiral is downstream of debt denomination and current account imbalances but represents a distinct structural constraint. Upstream constraints (foreign_debt_trap, trade_deficit_persistence) create conditions that trigger the spiral; the spiral itself is the extraction mechanism by which those upstream imbalances are forced onto domestic agents. Decomposition: (1) debt_servicing_constraint ε=0.35 (tangled rope — coordination of creditor relations + extraction of resources) (2) currency_depreciation_spiral ε=0.58 (tangled rope — coordination of trade adjustment + extraction of purchasing power) (3) capital_flight_mechanism ε=0.65 (snare — pure extraction from policy space).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(currency_depreciation_spiral, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
