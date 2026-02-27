% ============================================================================
% CONSTRAINT STORY: arg_ev_tariff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arg_ev_tariff, []).

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
 *   constraint_id: arg_ev_tariff
 *   human_readable: Argentine Tariff on Chinese Electric Vehicles
 *   domain: economic/political
 *
 * SUMMARY:
 *   Argentina's contemplated tariff on Chinese electric vehicles represents a
 *   constraint that embodies tension between industrial protection and
 *   economic liberalization. Under Javier Milei's administration, which has
 *   rhetorically committed to free-market policies, the tariff proposal
 *   appears contradictory yet follows a logic common to developing economies
 *   seeking to build domestic manufacturing capacity. The constraint exhibits
 *   mixed coordination and extraction functions: it coordinates domestic
 *   automakers and labor unions around price floors and market protection,
 *   but extracts from consumers through reduced choice and elevated prices.
 *   The tariff's duration and intent are ambiguous — framed as transitional
 *   industrial policy supporting the EV transition, but potentially permanent
 *   rent-seeking disguised as climate policy. Chinese manufacturers face
 *   barriers but retain arbitrage options through local investment. Regional
 *   MERCOSUR partners experience both coordination pressure (harmonization
 *   toward common standards) and extraction (tariff-segmented market access).
 *   The theater ratio reflects the gap between stated climate/development
 *   justifications and actual protectionist function.
 *
 * KEY AGENTS:
 *   - Argentine Domestic Automakers (SEVEL, Renault): Primary beneficiaries (institutional/arbitrage) — capture price protection and market share preservation during tariff period
 *   - Argentine EV Consumers: Primary victims (powerless/trapped) — bear extraction through tariff-elevated prices and reduced model choice; constrained by limited domestic alternatives
 *   - Chinese EV Manufacturers (BYD, Li Auto, others): Powerful institutional actor (powerful/arbitrage) — can arbitrage through investment in local assembly; face tariff barriers but retain strategic options
 *   - Argentine Labor Unions (Auto Sector): Secondary beneficiary (organized/constrained) — job protection through tariff-preserved domestic production; constrained by MERCOSUR obligations
 *   - Competing South American Markets (Brazil, Chile): Regional victim (moderate/constrained) — face tariff walls to Argentine market; constrained by MERCOSUR rules; can lobby for harmonization
 *   - Argentine Government (Milei Administration): Institutional enforcer (institutional/arbitrage) — sets tariff rate and duration; arbitrage through revenue collection and political favor allocation
 *   - MERCOSUR Trade Bloc: Institutional coordination (organized/constrained) — experiences tariff as friction in regional integration; constrained by member sovereignty but able to negotiate harmonization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arg_ev_tariff, 0.52).
domain_priors:suppression_score(arg_ev_tariff, 0.65).
domain_priors:theater_ratio(arg_ev_tariff, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arg_ev_tariff, extractiveness, 0.52).
narrative_ontology:constraint_metric(arg_ev_tariff, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(arg_ev_tariff, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arg_ev_tariff, tangled_rope).
narrative_ontology:human_readable(arg_ev_tariff, "Argentine Tariff on Chinese Electric Vehicles").
narrative_ontology:topic_domain(arg_ev_tariff, "economic/political").

domain_priors:requires_active_enforcement(arg_ev_tariff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arg_ev_tariff, argentine_domestic_automakers).
narrative_ontology:constraint_beneficiary(arg_ev_tariff, local_battery_producers).
narrative_ontology:constraint_beneficiary(arg_ev_tariff, labor_unions_in_auto_sector).
narrative_ontology:constraint_victim(arg_ev_tariff, argentine_ev_consumers).
narrative_ontology:constraint_victim(arg_ev_tariff, regional_trade_integration).
narrative_ontology:constraint_victim(arg_ev_tariff, competing_south_american_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARGENTINE EV CONSUMER (SNARE) — Trapped by tariff barriers. Domestic alternatives are limited and expensive. Cannot exit the constraint through import options without absorbing 35-40% tariff premiums. Bears extraction through reduced choice and elevated prices. No meaningful alternatives within local market.
constraint_indexing:constraint_classification(arg_ev_tariff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMPETING SOUTH AMERICAN MARKETS (TANGLED ROPE) — Countries like Brazil and Chile have different tariff strategies and domestic EV policies. Argentina's tariffs create both coordination pressure (tariff harmonization would reduce arbitrage) and extraction (Chilean/Brazilian firms face barriers to Argentine market access). Constrained exit through MERCOSUR agreements and regional trade rules.
constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ARGENTINE DOMESTIC AUTOMAKERS (ROPE) — Primary beneficiaries. Tariff protection solves a collective action problem: without tariffs, Chinese EV competition would force simultaneous price cuts across domestic producers. Tariffs coordinate on a higher price equilibrium and protect market share. Institutional actors with arbitrage options (can lobby for tariff rates, exemptions, or duration). Experience the constraint as coordination function.
constraint_indexing:constraint_classification(arg_ev_tariff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CHINESE EV MANUFACTURERS (ROPE) — Powerful institutional actor. Tariff is a coordination mechanism for them as well: it segments the Latin American market and prevents undercutting. Chinese firms can arbitrage by investing in Argentine manufacturing (tariff-exempt local production) or selling through joint ventures. This converts the tariff from extraction into coordination with arbitrage options.
constraint_indexing:constraint_classification(arg_ev_tariff, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MERCOSUR INTEGRATION FRAMEWORK (SCAFFOLD) — Regional trade bloc sees tariff barriers as temporary friction in the path toward continental EV supply chain integration. Current tariffs are expected to sunset as: (a) Argentine domestic EV capacity scales, (b) regional harmonization agreements mature (targeting 2028-2032), (c) cross-border battery supply chains develop. Tariff framed as transitional protection while regional champions emerge. High suppression (tariff walls) but declining over horizon due to sunset expectations.
constraint_indexing:constraint_classification(arg_ev_tariff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: GLOBAL FREE TRADE IDEOLOGY (PITON) — The WTO framework and neoliberal economic consensus treat tariffs as inefficient distortions. Yet Argentina's tariff is justified through climate/industrial policy rhetoric (EV transition support). The performative element: tariff is framed as temporary climate policy but functions as permanent protectionism. Theater ratio reflects the gap between stated (climate/development) and actual (protection from competition) goals. This perspective sees the tariff as an inertial remnant of pre-globalization trade policy maintained by political will despite ideological opposition.
constraint_indexing:constraint_classification(arg_ev_tariff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (GLOBAL VALUE CHAIN) (TANGLED ROPE) — From a global supply chain perspective, Argentina's tariff both facilitates and extracts. It facilitates: incentivizes Chinese and other manufacturers to invest in Argentine assembly (creating local jobs, building supply chain links). It extracts: prevents consumers from accessing cheaper global alternatives, locks in price floors that benefit domestic and protected foreign producers, creates tariff rents that flow to government and incumbent firms. Effective extraction is substantial but mixed with genuine coordination functions (supply chain localization). Scope is global; exit is mobile but costly (manufacturing relocation has switching costs).
constraint_indexing:constraint_classification(arg_ev_tariff, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arg_ev_tariff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arg_ev_tariff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arg_ev_tariff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arg_ev_tariff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arg_ev_tariff, TR),
    TR >= 0.70.

:- end_tests(arg_ev_tariff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The tariff imposes direct costs on consumers (35-40% price premiums on imported EVs) while concentrating benefits on a narrow producer coalition. However, extractiveness is not maximal (0.66+) because: (1) domestic alternatives exist at lower quality/price (not zero exit), (2) the tariff has stated sunset expectations (MERCOSUR harmonization pressure), (3) Chinese manufacturers retain FDI arbitrage options that could supply the market locally, (4) the tariff serves a legitimate (if controversial) industrial policy function. Suppression (0.65): Moderate-high. Tariff walls constrain consumer choice significantly. Non-tariff barriers (local content rules, certification delays) add suppression. But suppression is not total because smuggling and gray market options exist, and tariff rates are negotiable (not legally immovable). Theater ratio (0.58): Moderate. The stated function is EV transition support and domestic capacity building (real policy goal), but the actual effect is price protection for incumbent producers and revenue for government (actual function). The gap between rhetoric and reality is real but not maximal — the domestic capacity story is not entirely theater, but it is partially aspirational.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a sharp perspectival gap between beneficiaries and victims. Domestic automakers (institutional/arbitrage) classify the tariff as pure coordination (Rope) — it solves their prisoner's dilemma of undercutting and stabilizes prices. Argentine consumers (powerless/trapped) classify it as pure extraction (Snare) — they have no alternative but to pay higher prices. Chinese manufacturers (powerful/mobile) classify it as mixed (Tangled Rope or even Rope) — tariff creates pricing power but also incentivizes efficient local manufacturing. The MERCOSUR perspective (organized/constrained) sees temporary friction with sunset logic (Scaffold). The global trade ideology perspective sees inertial protectionism disguised as climate policy (Piton). The analytical/supply-chain perspective sees genuine mixed coordination and extraction (Tangled Rope). No single classification captures all structural relationships; the perspectival gap is the phenomenon itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (domestic automakers, unions) are institutional actors with arbitrage exit options — they can lobby for tariff rates, exemptions, duration. Their derived d is low (~0.15-0.25), producing negative effective extraction: the tariff subsidizes them. Victims (consumers) are powerless and trapped — they have no political influence on tariff policy and cannot exit the market without bearing full tariff cost. Their derived d is high (~0.90-0.95), producing high effective extraction. Chinese manufacturers are powerful with mobile exit options (local investment), deriving moderate d (~0.50-0.60) and moderate effective extraction — they can arbitrage. MERCOSUR partners are moderate institutional actors with constrained exit (regional rules), deriving d ~0.65-0.70. The presheaf shows clear perspectival differentiation: beneficiaries see coordination (Rope), consumers see extraction (Snare), analytical observers see tangled hybrid function (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES COORDINATION VS EXTRACTION AMBIGUITY: The tariff appears at first glance to be either pure coordination (stabilizing prices for producers) or pure extraction (raising consumer costs). The mandatrophy is resolved by decomposing the structural relationships: For domestic automakers, it is coordination (they benefit from price floor, solution to collective action problem). For consumers, it is extraction (they bear costs with no exit). For Chinese manufacturers, it is arbitrage-enabled coordination (can invest locally, converting tariff from barrier into FDI incentive). The constraint is Tangled Rope because it exhibits BOTH coordination (producer price stabilization, MERCOSUR harmonization incentives) AND extraction (consumer surplus loss, tariff rents captured by government and incumbents), with active enforcement required (tariff collection, local content verification). The mandatrophy does not collapse into a single type because different agents experience genuinely different structural functions. The beneficiaries' 'this is coordination' and the victims' 'this is extraction' are both structurally correct perspectives on the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tariff_duration_intent,
    'Is Argentina''s tariff intended as permanent protection or transitional industrial policy?',
    'Analysis of policy statements, legislative language, renewal thresholds, and comparative tariff rates over next 5-10 years. Cross-reference with similar tariffs in Brazil (temporary) vs Mexico (persistent).',
    'If permanent: classification shifts from Scaffold toward Tangled Rope or Snare (extraction dominates). If transitional: Scaffold classification holds (sunset is real). Determines whether tariff is coordination or pure rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tariff_duration_intent, empirical, 'Whether tariff is transitional or permanent protectionism').

omega_variable(
    domestic_ev_capacity_buildout,
    'Will Argentine domestic EV manufacturing capacity actually scale to justify the tariff''s stated purpose?',
    'Tracking announced investments by domestic automakers (SEVEL, Renault) and new entrants. Measurement: production units, capital expenditure, timeline to capacity. Cross-check against regional demand forecasts.',
    'If capacity scales: tariff functions as transitional protection (Scaffold becomes primary). If capacity stalls: tariff functions purely as rent extraction (Snare dominates for consumers). Determines whether beneficiaries'' coordination story is structural or theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_ev_capacity_buildout, empirical, 'Whether domestic EV capacity will scale as claimed').

omega_variable(
    consumer_welfare_incidence,
    'Does tariff-induced price increase exceed willingness-to-pay for quality/availability improvements in domestic offerings?',
    'Demand elasticity analysis; survey of consumer purchase intent at tariff-inclusive prices; market share tracking (domestic EVs vs imported vs substitutes like ICE vehicles).',
    'If consumer surplus loss > producer surplus gain: extraction is unambiguous (Snare/Tangled Rope). If balanced: coordination narrative is credible. Determines the sign of directionality for consumers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_welfare_incidence, empirical, 'Whether tariff price increases exceed consumer willingness to pay').

omega_variable(
    chinese_investment_substitution,
    'Will Chinese manufacturers invest in Argentine local assembly, transforming tariff from import barrier into FDI incentive?',
    'Tracking announced Chinese manufacturing investments in Argentina, joint venture announcements, assembly plant construction timelines. Compare tariff level to estimated FDI breakeven point.',
    'If significant Chinese investment: tariff converts from Snare (consumers trapped) to mixed Rope/Tangled Rope (local assembly enables access, tariff rents flow to government/Chinese investors). If no investment: tariff remains pure import barrier (Snare). Affects experienced extraction for consumers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chinese_investment_substitution, empirical, 'Whether Chinese firms invest in Argentine manufacturing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arg_ev_tariff, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arg_ev_tr_t0, arg_ev_tariff, theater_ratio, 0, 0.52).
narrative_ontology:measurement(arg_ev_tr_t3, arg_ev_tariff, theater_ratio, 3, 0.55).
narrative_ontology:measurement(arg_ev_tr_t6, arg_ev_tariff, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(arg_ev_be_t0, arg_ev_tariff, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(arg_ev_be_t3, arg_ev_tariff, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(arg_ev_be_t6, arg_ev_tariff, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arg_ev_tariff, resource_allocation).
narrative_ontology:affects_constraint(arg_ev_tariff, mercosur_tariff_harmonization).
narrative_ontology:affects_constraint(arg_ev_tariff, chinese_ev_manufacturing_fdi).
narrative_ontology:affects_constraint(arg_ev_tariff, latin_american_supply_chain_segmentation).

% DUAL FORMULATION NOTE:
% The Argentine EV tariff is downstream of the broader US/EU tariff movement on Chinese EVs (2023-2024) and represents regional response. It also feeds upstream into MERCOSUR harmonization negotiations and Chinese FDI decisions. The tariff's extractiveness value (0.52) reflects the local consumer welfare incidence; if measured from a supply-chain perspective, extractiveness would be lower (0.35-0.40) due to FDI arbitrage options. Constraint family includes mercosur_tariff_harmonization (higher coordination intent) and chinese_ev_manufacturing_fdi (tracking the investment substitution omega).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arg_ev_tariff, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
