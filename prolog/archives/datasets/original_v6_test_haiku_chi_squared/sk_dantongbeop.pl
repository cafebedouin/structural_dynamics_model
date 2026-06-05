% ============================================================================
% CONSTRAINT STORY: sk_dantongbeop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sk_dantongbeop, []).

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
 *   constraint_id: sk_dantongbeop
 *   human_readable: South Korea's Mobile Device Distribution Improvement Act (Dantongbeop)
 *   domain: economic/technological/regulatory
 *
 * SUMMARY:
 *   South Korea's Mobile Device Distribution Improvement Act (Dantongbeop),
 *   enacted in 2014, was officially intended to stabilize the mobile device
 *   market by mandating transparent and non-discriminatory subsidies across
 *   carriers. The law emerged from concerns about unfair subsidy allocation
 *   practices and small retailer exclusion. However, the constraint exhibits
 *   a fundamental tension between its stated coordination function
 *   (transparent subsidy allocation) and its actual extraction mechanism
 *   (locking distribution into carrier-controlled channels, suppressing
 *   independent retailers and MVNOs, constraining consumer choice). The core
 *   structural question is whether the act genuinely solves a coordination
 *   problem (carriers had chaotic, inefficient subsidy competition that
 *   benefited no one) or merely formalizes carrier oligopoly extraction
 *   (using transparency theater to mask distribution control). The
 *   perspectival evidence strongly suggests both are true simultaneously —
 *   making this a canonical tangled rope. The theater_ratio (0.64) reflects
 *   that the act's compliance apparatus (KCC transparency filings,
 *   anti-discrimination audits) is heavily performative: subsidy transparency
 *   filings dominate actual enforcement capacity, and carriers maintain
 *   control over device allocation through contracts that are nominally
 *   transparent but functionally opaque to consumers and retailers.
 *
 * KEY AGENTS:
 *   - Major Mobile Carriers (SK Telecom, KT, LG U+): Primary beneficiaries (institutional/arbitrage) — consolidate control over device distribution, reduce subsidy competition chaos, stabilize margins through predictable allocation
 *   - Device Manufacturers (Samsung, Apple): Secondary beneficiaries (institutional/arbitrage) — benefit from stable carrier relationships and predictable subsidy flows; reduce distributor margin volatility
 *   - Small Mobile Device Retailers: Primary victims (powerless/trapped) — excluded from subsidy allocation, face margin compression, lack exit options; cannot operate outside carrier channels
 *   - Mobile Virtual Network Operators (MVNOs): Secondary victims (moderate/constrained) — constrained access to subsidized device markets; face structural disadvantage vs major carriers
 *   - Consumers: Tertiary victims (powerless/trapped) — reduced device choice diversity, locked into carrier-controlled distribution, limited price transparency despite official transparency mandate
 *   - Government Industrial Policy: Tertiary beneficiary (institutional/arbitrage) — uses act to stabilize strategic tech sector, concentrate market control, reduce foreign competition
 *   - Enforcement Bureaucracy (KCC): Institutional actor (institutional/arbitrage) — maintains compliance theater through filings and audits; enforcement capacity degraded relative to compliance burden
 *   - Consumer Advocacy Coalition: Organized agents (organized/constrained) — provide coordination function through transparency pressure; voice without direct market power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_dantongbeop, 0.52).
domain_priors:suppression_score(sk_dantongbeop, 0.68).
domain_priors:theater_ratio(sk_dantongbeop, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_dantongbeop, extractiveness, 0.52).
narrative_ontology:constraint_metric(sk_dantongbeop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sk_dantongbeop, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sk_dantongbeop, tangled_rope).
narrative_ontology:human_readable(sk_dantongbeop, "South Korea's Mobile Device Distribution Improvement Act (Dantongbeop)").
narrative_ontology:topic_domain(sk_dantongbeop, "economic/technological/regulatory").

domain_priors:requires_active_enforcement(sk_dantongbeop).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sk_dantongbeop, major_carriers).
narrative_ontology:constraint_beneficiary(sk_dantongbeop, device_manufacturers).
narrative_ontology:constraint_beneficiary(sk_dantongbeop, government_industrial_policy).
narrative_ontology:constraint_victim(sk_dantongbeop, small_distribution_retailers).
narrative_ontology:constraint_victim(sk_dantongbeop, consumer_choice_diversity).
narrative_ontology:constraint_victim(sk_dantongbeop, mvno_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL DISTRIBUTION RETAILERS (SNARE) — Trapped in the subsidy distribution system with no exit option. Cannot operate outside the carrier-mandated channels; face margin compression and inventory constraints. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(sk_dantongbeop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSUMER CHOICE AND MARKET DIVERSITY (SNARE) — Cannot exit the centralized subsidy regime; bears the cost of reduced device options and limited competitive pricing. No voice in allocation decisions. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(sk_dantongbeop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MVNO OPERATORS (TANGLED ROPE) — Constrained by subsidy regime but benefit from infrastructure/network access. Partially dependent on carrier networks for operation. d≈0.68, f(d)≈1.03, σ=1.0 → χ≈0.54.
constraint_indexing:constraint_classification(sk_dantongbeop, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSUMER ADVOCACY COALITION (ROPE) — Organized agents (consumer groups, tech journalists) provide coordination function through transparency advocacy and public pressure. Exit options are constrained but coalition has voice and organizational capacity. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.25.
constraint_indexing:constraint_classification(sk_dantongbeop, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY REFORMERS (SCAFFOLD) — Institutional actors (policy analysts, progressive regulators) frame the act as a temporary coordination mechanism with a sunset path: gradual liberalization of device distribution, expansion of MVNO market share, EU-style subsidy transparency rules. d≈0.20, f(d)≈0.05, σ=1.0 → χ≈0.03. Low extraction because reform pathway is visible. has_sunset_clause_rationale: International regulatory harmonization (EU Device Transparency Directive adoption, US FCC unlocking requirements) creates pressure to decouple device subsidies from carrier lock-in. Estimated sunset: 10-15 years as digital market competition forces subsidy model reform.
constraint_indexing:constraint_classification(sk_dantongbeop, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ENFORCEMENT BUREAUCRACY (PITON) — The regulatory apparatus (Korean Communications Commission) maintains compliance theater: subsidy transparency filings, anti-discrimination audits, fair-dealing reports. Theater ratio=0.64 reflects high performative compliance burden relative to actual market enforcement capacity. The act's enforcement has largely degraded into ritual compliance rather than effective anti-cartel function. d≈0.15, f(d)≈-0.02, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(sk_dantongbeop, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: MAJOR MOBILE CARRIERS (ROPE) — SK Telecom, KT, LG U+ benefit from subsidy coordination mechanism that allocates device-cost burden transparently and predictably. The act reduces chaotic competition and stabilizes market structure. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary with modest effective extraction because coordination reduces uncertainty.
constraint_indexing:constraint_classification(sk_dantongbeop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: DEVICE MANUFACTURERS (ROPE) — Samsung and Apple benefit from predictable carrier subsidy flows and guaranteed device placement through regulated distribution. Market coordination reduces distributor margin volatility. d≈0.12, f(d)≈-0.06, σ=1.0 → χ≈-0.03. Net beneficiary; coordination reduces distribution chaos.
constraint_indexing:constraint_classification(sk_dantongbeop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — Global regulatory view sees Dantongbeop as a hybrid: provides coordination (transparent subsidy allocation) AND extraction (locks consumers into carrier-controlled distribution, suppresses alternative retail channels, restricts MVNO growth). The act produces both benefits and harms at different scales. d≈0.58, f(d)≈0.70, σ=1.0 → χ≈0.36. This is the true measured perspective: tangled rope, not pure rope or snare.
constraint_indexing:constraint_classification(sk_dantongbeop, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_dantongbeop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sk_dantongbeop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sk_dantongbeop, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sk_dantongbeop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sk_dantongbeop, TR),
    TR >= 0.70.

:- end_tests(sk_dantongbeop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The act generates meaningful asymmetric costs and benefits. Small retailers face margin compression and exclusion (high extraction from them); consumers face reduced choice diversity (moderate extraction); carriers gain stable predictable subsidy flows and distribution control (moderate benefit). The extractiveness is not as severe as a pure snare (0.70+) because the transparency mandate provides some real functional benefit — subsidy allocation is more predictable than the pre-2014 chaos. But the extraction is substantial enough (carriers consolidate distribution control, competitors excluded) to justify a moderate-high score. Suppression (0.68): High. Multiple barriers exist to alternative distribution channels: legal subsidy allocation is carrier-controlled, MVNOs lack subsidy participation pathways, independent retailers cannot negotiate carrier inventory terms, consumer price comparison tools are limited by opaque contract terms. These are structural suppressions, not merely high switching costs. Theater ratio (0.64): Moderate-high. The act's compliance burden (transparency filings, anti-discrimination audits, fair-dealing reports) is substantial relative to actual enforcement capacity. The KCC audits subsidy allocation but has limited ability to verify fairness of actual device inventory distribution, contract term negotiation, or service tier discrimination. Carriers maintain effective control despite formal transparency. Theater has increased over the interval as compliance bureaucracy expanded (2014-2024) without proportional enforcement capacity growth.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp divergence between beneficiary and victim perspectives. Major carriers and device manufacturers see genuine coordination benefit (Rope) — predictable subsidy allocation solved the pre-2014 chaos and reduced competition costs. Small retailers and MVNOs see pure extraction (Snare) — the act systematized their exclusion from viable distribution channels. Consumers experience mixed extraction and coordination: they benefit from some stabilized pricing but lose choice diversity and face opacity despite transparency mandates (Tangled Rope). The enforcement bureaucracy sees its own degraded ritual (Piton perspective) — compliance theater grows while actual enforcement capacity remains limited. The analytical observer sees tangled rope — genuine coordination function (subsidy predictability) bundled with genuine extraction (distribution control, competitor suppression). The pivotal gap is between carriers' rope perspective (coordination achieved) and retailers' snare perspective (extraction systematized). This gap is NOT resolvable by claiming one perspective is 'wrong' — it reflects genuine structural asymmetry: the act really does coordinate carrier behavior AND really does extract from retailers. Both are true.
 *
 * DIRECTIONALITY LOGIC:
 *   Major carriers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Device manufacturers: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.06. Net beneficiary. Small retailers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — no exit option. MVNOs: Victim + constrained → d≈0.68, f(d)≈1.03. Significant extraction; structural disadvantage relative to major carriers. Consumers: Victim + trapped → d≈0.95, f(d)≈1.42. High extraction; reduced choice diversity, opaque contracts despite transparency mandate. Enforcement bureaucracy: Institutional + arbitrage → d≈0.15, f(d)≈-0.02. Piton classification comes from theater gate, not from high chi. Consumer advocacy: Organized + constrained → d≈0.45, f(d)≈0.48. Low extraction because coalition has voice and can pressure for transparency. Analytical observer: analytical → d≈0.58, f(d)≈0.70. Captures the true mixed structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The Dantongbeop resolves the mandatrophy by declaring explicit beneficiaries (major carriers, device manufacturers, government industrial policy) and victims (small retailers, MVNOs, consumer choice diversity). This is NOT a false summit (mountain misclassification): the act is neither an immutable law of nature nor a pure coordination mechanism. The coordination function is real (stable carrier subsidy allocation) but bundled with extraction (distribution control, competitor suppression). The act also satisfies the tangled rope gate: requires_active_enforcement=true (KCC audits and compliance filings), beneficiaries declared (major carriers), victims declared (small retailers, MVNOs). The theater_ratio (0.64) reflects that enforcement is substantial but largely performative relative to actual market control. The act is a classic tangled rope: coordination bundled with asymmetric extraction, maintained through active enforcement (compliance filings), with hidden victims (small retailers) and visible beneficiaries (carriers). The mandate resolves by accepting this duality: the act genuinely solves a coordination problem AND genuinely extracts from competitors. The perspectival gap (carriers see rope, retailers see snare, analytical observers see tangled rope) is NOT a failure of the framework but evidence of structural reality. Different agents genuinely experience the same constraint differently because it produces different costs and benefits from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_transparency_effectiveness,
    'Does mandated subsidy transparency actually prevent carrier discrimination, or does it merely move discrimination into non-transparent forms (service tiers, contract terms, device inventory allocation)?',
    'Comparative analysis of device price variation across carriers pre- and post-2014; examination of compliance filing data vs actual market pricing; MVNO operator interviews on access and subsidy parity',
    'If transparency is effective: tangled rope classification stands — genuine coordination achieved. If discrimination persists in opaque forms: snare classification gains support — the act is regulatory theater masking extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_transparency_effectiveness, empirical, 'Whether subsidy transparency prevents carrier discrimination').

omega_variable(
    retail_consolidation_causation,
    'Did the Dantongbeop cause the documented consolidation of mobile device retailers and reduction of independent shops, or was this consolidation a broader market trend independent of the act?',
    'Time-series analysis of retailer count, market concentration (HHI) of distribution channels, and independent shop survival rates. Comparison with non-regulated markets (Japan, Singapore) over same period. Retailer interviews on subsidy regime impact on viability.',
    'If Dantongbeop-caused: supports snare classification for small retailers — the act directly suppressed their exit options. If exogenous trend: reduces victim status — retailers faced headwinds from broader e-commerce and consolidation forces unrelated to regulation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retail_consolidation_causation, empirical, 'Whether the act caused retail consolidation or reflected broader trends').

omega_variable(
    mvno_market_suppression_mechanism,
    'Does the Dantongbeop''s carrier-controlled subsidy distribution mechanically exclude MVNOs, or do MVNOs have genuine paths to compete in subsidized device markets?',
    'MVNO market share trends post-2014 vs comparable regulated markets (EU). Analysis of MVNO access to device subsidy programs and carrier cross-subsidy requirements. Legal examination of subsidy distribution contracts.',
    'If mechanically excluded: victim classification for MVNOs is strong — regulation suppresses structural alternative. If market-accessible but MVNOs choose not to compete in subsidies: victim status is weaker — constraint is less severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mvno_market_suppression_mechanism, empirical, 'Whether Dantongbeop mechanically excludes MVNOs from subsidy participation').

omega_variable(
    international_regulatory_convergence,
    'Are global trends (EU Device Transparency Directive, US FCC unlocking requirements, WTO digital trade rules) creating genuine pressure for Dantongbeop liberalization, or will regulatory capture prevent reform?',
    'Comparison of Korean subsidy regulations with EU, US, Japan standards over next 5 years. Monitoring of government reform proposals and carrier industry positions. Assessment of international trade pressure.',
    'If convergence pressure is genuine: scaffold classification is correct — sunset is real. If capture dominates: the scaffold is aspirational — tangled rope with no actual exit path is the stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_regulatory_convergence, empirical, 'Whether international regulatory convergence will pressure Dantongbeop liberalization').

omega_variable(
    consumer_benefit_measurement,
    'Did the Dantongbeop increase or decrease consumer welfare through price stability, or did transparency-as-theater mask ongoing extraction through hidden contract terms and subsidy reallocation?',
    'Total cost of ownership (TCO) analysis for consumers across carrier tiers and devices. Comparison of device price volatility, upgrade availability, and contract transparency pre- and post-2014. Consumer surveys on perceived subsidy fairness.',
    'If welfare increased: rope classification gains support — genuine coordination benefit. If welfare declined or was redistributed: snare classification gains support — the act is net-negative extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_benefit_measurement, empirical, 'Whether Dantongbeop increased or decreased consumer welfare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sk_dantongbeop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dant_tr_t0, sk_dantongbeop, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dant_tr_t5, sk_dantongbeop, theater_ratio, 5, 0.51).
narrative_ontology:measurement(dant_tr_t10, sk_dantongbeop, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(dant_be_t0, sk_dantongbeop, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dant_be_t5, sk_dantongbeop, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(dant_be_t10, sk_dantongbeop, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sk_dantongbeop, resource_allocation).
narrative_ontology:affects_constraint(sk_dantongbeop, korean_smartphone_market_concentration).
narrative_ontology:affects_constraint(sk_dantongbeop, mvno_market_suppression_korea).
narrative_ontology:affects_constraint(sk_dantongbeop, retail_distribution_consolidation_tech).

% DUAL FORMULATION NOTE:
% The Dantongbeop can be decomposed into two related but structurally distinct constraints: (1) Subsidy Transparency Coordination (ε≈0.25, Rope) — the genuine coordination benefit of predictable subsidy allocation, and (2) Distribution Channel Lock-in (ε≈0.68, Snare) — the extraction mechanism that suppresses alternative distribution. However, these are operationally inseparable in the actual regulatory regime, justifying a single tangled rope story with both effects. The network relationships show how the act propagates effects into downstream constraints: market concentration (carriers gain market share), MVNO suppression (market barrier effects), and retail consolidation (exclusion effects).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sk_dantongbeop, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
