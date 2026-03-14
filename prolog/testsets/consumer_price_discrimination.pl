% ============================================================================
% CONSTRAINT STORY: consumer_price_discrimination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumer_price_discrimination, []).

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
 *   constraint_id: consumer_price_discrimination
 *   human_readable: Consumer Price Discrimination through Data-Driven Targeting
 *   domain: economic/digital_markets
 *
 * SUMMARY:
 *   Consumer price discrimination through data-driven targeting represents a
 *   hybrid coordination-extraction constraint operating across digital
 *   platforms. Platforms collect behavioral data (purchase history, browsing
 *   patterns, device identifiers, location) to implement dynamic pricing that
 *   allocates limited inventory across heterogeneous consumer demand. This
 *   mechanism produces genuine coordination benefits: reduced deadweight loss
 *   from rigid pricing, improved inventory turnover, and allocative
 *   efficiency matching supply to demand elasticity. Simultaneously, the
 *   opacity of behavioral targeting and the suppression of consumer price
 *   comparison enable systematic extraction: platforms charge price-sensitive
 *   consumers more based on inferred willingness-to-pay derived from
 *   behavioral data. The constraint exhibits all hallmarks of Tangled Rope:
 *   genuine coordination function (dynamic allocation), asymmetric extraction
 *   (behavioral targeting), and high suppression (consumers cannot see or
 *   compare prices). The theater ratio (0.48) reflects that price
 *   discrimination operates with relatively low performative overhead — the
 *   extraction mechanism is structural (data collection and algorithmic
 *   optimization) rather than ritual (unlike formal price parity clauses,
 *   which are increasingly theatrical). The extractiveness trajectory
 *   (0.32→0.58 over the interval) shows accumulation: as data collection has
 *   become more granular and behavioral targeting more sophisticated, the
 *   extraction component has grown relative to the coordination component.
 *   Suppression (0.62) is high because the discrimination mechanism is
 *   intentionally opaque — consumers cannot see that they are being
 *   discriminated against, and algorithmic opacity prevents market-level
 *   price comparison.
 *
 * KEY AGENTS:
 *   - Price-Sensitive Consumers: Primary victims (powerless/trapped) — lack behavioral data arbitrage capacity, face platform concentration, unable to comparison shop across dynamic prices
 *   - Price-Aware Shoppers: Secondary victims (moderate/constrained) — retain some agency via multi-platform access and comparison tools, but data collection and switching costs significantly constrain exit
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — design and operate discrimination mechanism; see pricing as pure coordination with optional extraction upside
 *   - Data Brokers: Secondary beneficiaries (institutional/arbitrage) — monetize behavioral data; feed targeting signals back to platforms
 *   - Consumer Protection Regulators: Organized actors (organized/constrained) — possess enforcement authority but face technical complexity, capture risk, and jurisdictional limits
 *   - MFN Clause Framework: Institutional artifact (institutional/arbitrage) — designed to prevent price arbitrage but now largely theater as dynamic pricing makes enforcement impossible
 *   - Analytical Observer: Structural perspective (analytical/analytical) — sees constraint as existing on spectrum between pure coordination and pure extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_price_discrimination, 0.58).
domain_priors:suppression_score(consumer_price_discrimination, 0.62).
domain_priors:theater_ratio(consumer_price_discrimination, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_price_discrimination, extractiveness, 0.58).
narrative_ontology:constraint_metric(consumer_price_discrimination, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(consumer_price_discrimination, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_price_discrimination, tangled_rope).
narrative_ontology:human_readable(consumer_price_discrimination, "Consumer Price Discrimination through Data-Driven Targeting").
narrative_ontology:topic_domain(consumer_price_discrimination, "economic/digital_markets").

domain_priors:requires_active_enforcement(consumer_price_discrimination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_price_discrimination, platform_operators).
narrative_ontology:constraint_beneficiary(consumer_price_discrimination, data_brokers).
narrative_ontology:constraint_beneficiary(consumer_price_discrimination, targeted_sellers).
narrative_ontology:constraint_victim(consumer_price_discrimination, price_sensitive_consumers).
narrative_ontology:constraint_victim(consumer_price_discrimination, behavioral_targeting_targets).
narrative_ontology:constraint_victim(consumer_price_discrimination, market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRICE-SENSITIVE CONSUMER (SNARE) — Digitally trapped in price discrimination without exit capacity. Behavioral tracking via cookies, device fingerprints, and purchase history enables personalized price extraction. No meaningful alternative (all major platforms employ similar targeting). High suppression: consumer cannot see the discrimination mechanism; comparison shopping is defeated by dynamic pricing. Zero coordination benefit — pure extraction.
constraint_indexing:constraint_classification(consumer_price_discrimination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRICE-AWARE SHOPPER (TANGLED ROPE) — Moderate power through platform switching and price comparison tools (Honey, CamelCamelCamel), but constrained by switching costs, data re-collection, and incomplete price visibility. Coordination benefit exists: dynamic pricing allocates inventory to high-demand periods. Extraction exists: browsers' shopping history is monetized; comparison tool data feeds back to platforms. Mixed experience — some agency but significant asymmetric extraction.
constraint_indexing:constraint_classification(consumer_price_discrimination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences price discrimination as pure coordination: allocating limited inventory across price-sensitive and price-insensitive consumers, matching demand to supply dynamically, reducing deadweight loss from fixed pricing. Net beneficiary of the extraction flow. Arbitrage capacity is maximal — can enter/exit pricing strategies, adjust algorithms, move to new markets. Sees no suppression; the mechanism is opaque only to consumers, not to the platform.
constraint_indexing:constraint_classification(consumer_price_discrimination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER PROTECTION REGULATOR (TANGLED ROPE) — Organized state actors (FTC, EU DPA) experience price discrimination as both coordination and extraction problem. Coordination benefit: dynamic pricing can improve allocative efficiency IF algorithmic transparency and consumer consent exist. Extraction problem: platforms use opacity and behavioral targeting to suppress consumer welfare gains. Constrained by jurisdictional limits, technical complexity, and regulatory capture risk. Active enforcement (algorithmic audits, consent mechanisms) required to shift toward coordination and away from extraction.
constraint_indexing:constraint_classification(consumer_price_discrimination, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MFN CLAUSE INSTITUTIONAL ARTIFACT (PITON) — Price parity clauses (MFN: 'you shall not charge less elsewhere') were designed to prevent arbitrage and maintain price coordination across channels. Modern MFN is largely theater — dynamic pricing and personalization have made strict price parity impossible to enforce and verify. The institutional commitment persists (Amazon, Airbnb still claim price parity) while the functional mechanism has atrophied. Theater ratio high because MFN invocation has become a legitimation ritual rather than an enforcement mechanism. No longer coordinates effectively; persists through institutional inertia.
constraint_indexing:constraint_classification(consumer_price_discrimination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, price discrimination is inherently hybrid. Perfect price discrimination would be pure coordination (optimal allocation); opacity-driven discrimination is pure extraction. Empirically, the mechanism operates between these poles: platforms extract significant rents through behavioral targeting (suppression via opacity) while also providing coordination services (dynamic inventory allocation). The constraint requires active enforcement to shift the balance from extraction toward coordination. Current equilibrium favors extraction because suppression mechanisms (behavioral targeting, data opacity, switching costs) are stronger than coordination benefits.
constraint_indexing:constraint_classification(consumer_price_discrimination, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_price_discrimination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_price_discrimination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_price_discrimination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumer_price_discrimination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumer_price_discrimination, TR),
    TR >= 0.70.

:- end_tests(consumer_price_discrimination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated moderate. Price discrimination extracts meaningful consumer surplus through behavioral targeting and opacity. The value reflects that extraction is significant but not maximal — platforms retain coordination incentives (inventory efficiency matters for their operations) and some consumer welfare is improved by demand-responsive pricing. If discrimination were pure extraction with no coordination benefit, extractiveness would be ≥0.70. Suppression (0.62): Moderate-high. Behavioral targeting is intentionally opaque; consumers cannot see discrimination occurring. Switching costs (app-based habits, data collection friction, platform ecosystem lock-in) raise the barriers to exit. Legal and technical complexity (algorithmic decision-making) further suppress consumer understanding. However, suppression is not absolute: price comparison tools exist (Honey browser extension, CamelCamelCamel), educated consumers can observe price variance, and some platforms have begun transparency initiatives. Theater ratio (0.48): Moderate-low. Price discrimination operates with relatively little performative activity. The MFN clause (Perspective 5) is increasingly theatrical — invoked for legitimacy but not enforced. The core discrimination mechanism is structural: data feeds optimization algorithms with minimal ritual overlay. The value reflects that extraction here is primarily structural (data asymmetry, algorithmic optimization) rather than theatrical (formal compliance rituals).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates fundamental disagreement about classification across structural positions. The price-sensitive consumer (powerless/trapped) experiences pure extraction (Snare) — they pay more without understanding why or having alternatives. The price-aware shopper (moderate/constrained) experiences mixed coordination and extraction (Tangled Rope) — they benefit from dynamic allocation but also bear extraction through data collection and switching costs. The platform (institutional/arbitrage) experiences coordination (Rope) — they see dynamic pricing as inventory allocation and consumer surplus exists in equilibrium. The regulator (organized/constrained) also sees Tangled Rope but emphasizes the enforcement challenge: the coordination benefits are achievable only if behavioral targeting is bounded and consumer transparency is mandated. The MFN institutional framework (piton) shows how coordination mechanisms degrade over time — the price parity clause was designed to coordinate pricing across channels but has become theater as dynamic pricing renders it unenforceable. The analytical observer (Tangled Rope) sees the constraint as fundamentally hybrid with an empirical question: what combination of transparency, switching cost reduction, and enforcement would shift the balance from extraction-dominant to coordination-dominant? The perspectival gap reveals that 'price discrimination' is not a single phenomenon but a coordination mechanism with embedded extraction that is visible only from certain positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps structural position to experienced extraction via power level, exit options, and beneficiary/victim status. Price-sensitive consumers are victims with trapped exit — derived d approaches 0.95 (maximum target status), producing f(d) ≈ 1.42 and high experienced extraction chi. Price-aware shoppers are partial victims with constrained exit — derived d ≈ 0.65, producing f(d) ≈ 1.00 and moderate extraction. Platform operators are beneficiaries with arbitrage exit — derived d ≈ 0.15, producing f(d) ≈ -0.01 and negative chi (they experience the constraint as beneficial). Regulators are organized partial victims with constrained exit (authority without full enforcement capacity) — derived d ≈ 0.60, producing moderate f(d). The directionality pipeline reveals why the same mechanism produces such different classifications: the structural positions (beneficiary vs victim, trapped vs arbitrage) determine d, which determines f(d), which scales base extraction into experienced extraction chi. No override is necessary — the structural data is sufficient.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that price discrimination is genuinely both coordination AND extraction, not one mislabeled as the other. The resolution hinges on answering the omegas: (1) Is behavioral targeting necessary for coordination benefits, or is it parasitic? If parasitic, extraction can be reduced without losing efficiency. (2) Can transparency be implemented without destroying coordination? If yes, the constraint can shift toward Scaffold (temporary enforcement + sunset into transparent dynamic pricing). (3) What is the capture risk? If regulators lack auditing capacity, the Tangled Rope classification is unstable — the enforcement required to maintain balance cannot occur, and the constraint drifts toward Snare. The mandatrophy is resolved not by choosing between Rope/Snare but by specifying the conditions under which the hybrid nature can be sustained versus the conditions under which it collapses toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transparency_vs_extraction_tradeoff,
    'Can algorithmic transparency (revealing discrimination to consumers) be implemented without destroying the coordination benefits of dynamic pricing?',
    'Natural experiments from transparency mandates (EU Platform-to-Business Regulation, algorithmic auditing pilots); measurement of price variance and allocation efficiency before/after disclosure',
    'If transparency preserves efficiency: constraint shifts toward Rope/Scaffold (coordination dominant). If transparency degrades efficiency: constraint remains Tangled Rope or Snare (extraction remains necessary for performance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_vs_extraction_tradeoff, empirical, 'Whether algorithmic transparency can preserve coordination benefits').

omega_variable(
    behavioral_targeting_necessity,
    'Is behavioral targeting (purchase history, browsing data, device fingerprinting) necessary for dynamic pricing''s coordination function, or is it parasitic extraction layered onto price optimization?',
    'Comparative analysis: platforms using behavior-neutral dynamic pricing (time/inventory-based only) vs behavior-targeted pricing; measurement of price variance, allocative efficiency, and consumer surplus',
    'If necessary: suppression value is coordination cost, not pure extraction overhead. If parasitic: suppression component (0.62) could be reduced to ~0.15 without losing efficiency, collapsing the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_targeting_necessity, empirical, 'Whether behavioral targeting is necessary for dynamic pricing function').

omega_variable(
    switching_cost_equilibrium,
    'What switching cost threshold converts constrained exit into trapped exit? At what platform concentration does the moderate consumer''s constrained status degrade into powerless trapped status?',
    'Market concentration index (HHI) correlated with switching rate measurements; estimation of willingness-to-pay to switch platforms as function of HHI',
    'If threshold is low (HHI > 2000): current market structure has collapsed moderate/constrained into powerless/trapped for most consumers. If threshold is high: constrained status is accurate and coalition power (organized multiple consumers) remains viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_equilibrium, empirical, 'Switching cost threshold for trapped vs constrained status').

omega_variable(
    regulator_capture_risk,
    'To what extent has algorithmic complexity enabled regulatory capture? Do regulators lack technical capacity to audit price discrimination, allowing platforms to appear compliant while extraction continues?',
    'Historical analysis of FTC/DPA enforcement actions; comparison of disclosed algorithms vs actual price discrimination measured by mystery shopping and data analysis; regulatory hiring and budget trends for algorithmic auditing',
    'If high capture: the organized/constrained regulator (Perspective 4) is misclassified — effectively organized/arbitrage (benefits from opacity). Constraint would remain Snare even with enforcement attempted. If low capture: Tangled Rope classification holds and enforcement can shift balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulator_capture_risk, conceptual, 'Extent of regulatory capture via algorithmic complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_price_discrimination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cpd_tr_t0, consumer_price_discrimination, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cpd_tr_t5, consumer_price_discrimination, theater_ratio, 5, 0.38).
narrative_ontology:measurement(cpd_tr_t10, consumer_price_discrimination, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cpd_be_t0, consumer_price_discrimination, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cpd_be_t5, consumer_price_discrimination, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cpd_be_t10, consumer_price_discrimination, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_price_discrimination, resource_allocation).
narrative_ontology:affects_constraint(consumer_price_discrimination, algorithmic_opacity).
narrative_ontology:affects_constraint(consumer_price_discrimination, platform_data_concentration).
narrative_ontology:affects_constraint(consumer_price_discrimination, consumer_switching_costs).

% DUAL FORMULATION NOTE:
% Price discrimination decomposes into multiple constraint stories: (1) consumer_price_discrimination (this story) — the hybrid coordination-extraction mechanism; (2) algorithmic_opacity — the information asymmetry enabling discrimination; (3) platform_data_concentration — the data accumulation enabling behavioral targeting. Each has distinct ε values and structural properties. This story links upstream to data concentration (data is input to discrimination algorithm) and downstream to opacity (opacity is output mechanism that suppresses consumer awareness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
