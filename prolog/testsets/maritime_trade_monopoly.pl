% ============================================================================
% CONSTRAINT STORY: maritime_trade_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maritime_trade_monopoly, []).

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
 *   constraint_id: maritime_trade_monopoly
 *   human_readable: Maritime Trade Monopoly: Extraction Through Shipping Control
 *   domain: economic/trade/logistics
 *
 * SUMMARY:
 *   The maritime trade monopoly represents a pure extraction constraint where
 *   a shipping consortium controls access to global trade routes through
 *   combination of capital concentration, regulatory capture, and port
 *   infrastructure control. Dependent trading nations, small merchants, and
 *   consumer economies bear extraction costs through inflated freight rates,
 *   supply chain delays, and compelled use of monopoly-adjacent services. The
 *   monopoly maintains suppression through legal barriers (cabotage laws,
 *   flag-state regulations, port access restrictions) and structural
 *   immobility (prohibitive capital costs for alternative shipping
 *   infrastructure). The constraint exhibits accumulated extractiveness
 *   (rising from 0.48 to 0.68 over the 100-year interval) and rising theater
 *   ratio (0.38 to 0.55), indicating growing reliance on performative
 *   compliance with outdated regulatory frameworks rather than genuine
 *   coordination functions. Alternative shipping technologies
 *   (containerization, autonomous vessels, regional logistics networks) are
 *   eroding the structural necessity for monopoly control but have not yet
 *   achieved scale sufficient to break the constraint.
 *
 * KEY AGENTS:
 *   - Monopoly Shipping Consortium: Primary beneficiary (institutional/arbitrage) — captures monopoly rents through rate-setting, port priority, and forced auxiliary service use
 *   - Dependent Trading Nations: Primary victim (powerless/trapped) — bear maximum extraction through freight cost multiplication and supply chain dependence; no geographic exit available
 *   - Small Merchant Operators: Secondary victim (moderate/constrained) — face high capital barriers to fleet ownership and forced use of monopoly services; cannot exit without catastrophic cost
 *   - Consumer Economies: Tertiary victim (moderate/constrained) — experience extraction through elevated import costs and supply volatility
 *   - Merchant Coalition: Organized agent (organized/constrained) — benefits from some coordination (standardized routes, port infrastructure) while bearing asymmetric extraction through artificial rate ceilings
 *   - Legacy Regulation Framework: Institutional system (institutional/arbitrage) — maintains monopoly control through flag-state capture and cabotage laws; sees own function as degraded (piton perspective)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — identifies systematic extraction masked as natural consequence of geographic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maritime_trade_monopoly, 0.68).
domain_priors:suppression_score(maritime_trade_monopoly, 0.72).
domain_priors:theater_ratio(maritime_trade_monopoly, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maritime_trade_monopoly, extractiveness, 0.68).
narrative_ontology:constraint_metric(maritime_trade_monopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(maritime_trade_monopoly, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maritime_trade_monopoly, snare).
narrative_ontology:human_readable(maritime_trade_monopoly, "Maritime Trade Monopoly: Extraction Through Shipping Control").
narrative_ontology:topic_domain(maritime_trade_monopoly, "economic/trade/logistics").

domain_priors:requires_active_enforcement(maritime_trade_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maritime_trade_monopoly, monopoly_shipping_consortium).
narrative_ontology:constraint_victim(maritime_trade_monopoly, dependent_trading_nations).
narrative_ontology:constraint_victim(maritime_trade_monopoly, small_merchant_operators).
narrative_ontology:constraint_victim(maritime_trade_monopoly, consumer_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT TRADING NATION (SNARE) — Trapped by geographic isolation and reliance on maritime routes. Cannot build parallel shipping infrastructure. Bears maximum extraction through inflated freight costs, supply chain delays, and dependence on monopoly-controlled schedules. No exit option.
constraint_indexing:constraint_classification(maritime_trade_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL MERCHANT OPERATOR (SNARE) — Constrained by high capital barriers to fleet ownership and monopoly control of port access. Extraction through freight rate manipulation, priority queuing, and forced use of subsidiary services. Can theoretically exit but at prohibitive cost — effectively trapped.
constraint_indexing:constraint_classification(maritime_trade_monopoly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONOPOLY SHIPPING CONSORTIUM (ROPE) — Benefits from coordination of global shipping routes and standardized logistics. Experiences the constraint as pure coordination: managing fleet utilization, port scheduling, and route optimization. Net beneficiary with exit through regulatory arbitrage.
constraint_indexing:constraint_classification(maritime_trade_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MERCHANT COALITION (TANGLED ROPE) — Organized agent (collective shipping agreements, port authority committees) sees the monopoly as both enabling coordination and extracting through rate-setting. Coordination benefits exist (standardized shipping lanes, port infrastructure) alongside asymmetric extraction (artificially inflated rates). Constrained exit through regulatory and contractual lock-in.
constraint_indexing:constraint_classification(maritime_trade_monopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY REGULATION FRAMEWORK (PITON) — Historical maritime regulations (cabotage laws, flag-state monopolies, colonial trade routes) persist through institutional inertia despite reduced functional necessity. Alternative shipping technologies (containerization, autonomous vessels, regional logistics networks) are eroding the rationale for monopoly control. Theater ratio reflects performative compliance with outdated regulatory architecture.
constraint_indexing:constraint_classification(maritime_trade_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational scope, the monopoly appears as systematic extraction with suppression mechanisms embedded in maritime law, port infrastructure control, and regulatory capture of shipping standards. The constraint persists because suppression mechanisms (legal barriers, capital concentration, information asymmetry) make alternatives structurally unavailable at scale.
constraint_indexing:constraint_classification(maritime_trade_monopoly, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maritime_trade_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maritime_trade_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maritime_trade_monopoly, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maritime_trade_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(maritime_trade_monopoly, TR),
    TR >= 0.70.

:- end_tests(maritime_trade_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting significant rent extraction from dependent actors. The monopoly captures disproportionate value from freight rate-setting, port access control, and forced auxiliary service use. The value is not maximal (0.90+) because some genuine coordination functions persist (route standardization, port infrastructure, logistics optimization) and because competitive margins exist in certain commodity segments where alternatives are viable. Suppression (0.72): High, indicating substantial barriers to exit through combination of legal mechanisms (cabotage laws, flag-state regulations) and economic barriers (capital concentration, port infrastructure control, information asymmetry). Small merchants and dependent nations cannot realistically build alternative shipping infrastructure. Theater ratio (0.55): Moderate-high, reflecting significant performative content in regulatory compliance. Cabotage laws and flag-state requirements persist despite reduced technological necessity — they maintain monopoly control while appearing to serve national maritime interests. Rising theater ratio over time (0.38 → 0.55) indicates increasing gap between formal regulatory justification and actual coordination function.
 *
 * PERSPECTIVAL GAP:
 *   Maximal gap between beneficiary and victim perspectives. The monopoly consortium sees coordination (Rope) — managing global shipping is a genuine coordination problem. Dependent nations see pure extraction (Snare) — they cannot exit and bear full cost of inflated rates. The merchant coalition sees hybrid (Tangled Rope) — some coordination benefits (standardized routes, infrastructure) alongside significant extraction (rate manipulation). The legacy regulation framework sees its own degradation (Piton) — cabotage laws persist through inertia as alternative technologies (containerization, autonomous vessels) erode their functional necessity. The analytical observer sees systematic extraction (Snare) — the constraint persists because suppression mechanisms prevent alternative shipping infrastructure from achieving scale. The perspectival gap reflects the fundamental asymmetry: coordination benefits flow to the monopoly operator and some large merchants; extraction costs flow to dependent nations and small operators.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (monopoly consortium) have institutional power and arbitrage exit options, yielding low directionality (d ≈ 0.10-0.20) and negative or near-zero effective extraction from their perspective. Victims (dependent nations, small merchants) have powerless/moderate power and trapped/constrained exit options, yielding high directionality (d ≈ 0.85-0.95) and maximum or near-maximum effective extraction. The organized merchant coalition has constrained exit and mixed benefit/burden relationship, yielding moderate directionality (d ≈ 0.50-0.60) and moderate effective extraction. The legacy regulation framework benefits from status quo through institutional inertia, yielding low directionality from the regulatory perspective. The analytical observer, standing outside the constraint, derives d from the structural dominance of extraction over coordination (d ≈ 0.75-0.85), confirming snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Snare classification is robust across multiple victim perspectives despite the monopoly operator's honest experience of coordination problems. The mandatrophy question 'Is this coordination or extraction?' is answered: it is extraction enabled by coordination mechanisms. The monopoly genuinely solves routing, scheduling, and port logistics problems (coordination function), but it captures the rent from solving those problems by preventing alternatives (extraction mechanism). The piton perspective reveals that performative regulatory compliance (theater ratio 0.55) increasingly masks reduced functional necessity — maritime regulations originally justified by genuine collective action problems are now used to maintain monopoly control. The analytical observer's snare classification confirms: suppression mechanisms (legal + economic barriers) and high extraction (0.68) override the coordination function. The constraint persists not because coordination is impossible without monopoly control but because suppression prevents the emergence of competitive alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'What portion of the monopoly''s rate-setting reflects legitimate coordination costs versus extractive overhead?',
    'Comparative analysis of shipping rates in competitive markets (feeder routes, regional corridors) versus monopoly-controlled routes; cost accounting by tonnage and distance; margin analysis across commodity types',
    'If coordination costs > 40% of rate premium: reclassify as Tangled Rope from more perspectives. If < 15%: confirm Snare classification. Current estimate splits the difference, supporting Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Boundary between coordination costs and extractive overhead in freight rates').

omega_variable(
    suppression_mechanism_externality,
    'Is suppression maintained through active enforcement (legal, violent, contractual) or through structural immobility (no alternatives exist)?',
    'Historical analysis of resistance attempts; identification of cases where merchants successfully built alternatives or switched providers; assessment of legal barriers versus economic barriers',
    'If active enforcement dominates: snare classification confirmed by coercive mechanism. If structural immobility dominates: suppression is economic (high transaction costs to exit) rather than legal; reclassify exit_options to mobile for certain high-capital actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_externality, empirical, 'Whether suppression operates through active enforcement or structural immobility').

omega_variable(
    alternative_shipping_viability,
    'Could autonomous vessels, drone shipping, or regional containerization networks at scale reduce the monopoly''s control within a generation?',
    'Technology readiness assessment; cost comparison of alternative logistics pathways; pilot program outcomes in competitive markets; regulatory barriers to alternative technologies',
    'If viable within 15-20 years: the piton perspective is accurate — monopoly is degraded ritual. If viable within 5 years: scaffold classification applies (sunset visible in near term). If not viable: snare classification persists indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_shipping_viability, empirical, 'Viability of alternative shipping technologies to break monopoly control').

omega_variable(
    flag_state_capture_degree,
    'To what extent is monopoly control maintained through capture of flag-state regulations versus genuine market dominance?',
    'Comparative analysis of shipping rates under different flag-state regulatory regimes; assessment of whether open-registry flags reduce monopoly extractiveness; historical correlation between regulatory capture and rate setting',
    'If capture > 60%: suppression classification is primarily regulatory (legal barriers) rather than market-structural. Reclassify from Snare to institutional capture dynamic (Tangled Rope with institutional identity_locked perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flag_state_capture_degree, empirical, 'Degree to which flag-state regulatory capture enables monopoly extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maritime_trade_monopoly, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mtm_tr_t0, maritime_trade_monopoly, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mtm_tr_t25, maritime_trade_monopoly, theater_ratio, 25, 0.48).
narrative_ontology:measurement(mtm_tr_t50, maritime_trade_monopoly, theater_ratio, 50, 0.55).
narrative_ontology:measurement(mtm_tr_t75, maritime_trade_monopoly, theater_ratio, 75, 0.59).

% Extraction over time
narrative_ontology:measurement(mtm_be_t0, maritime_trade_monopoly, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mtm_be_t25, maritime_trade_monopoly, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(mtm_be_t50, maritime_trade_monopoly, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(mtm_be_t75, maritime_trade_monopoly, base_extractiveness, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maritime_trade_monopoly, resource_allocation).
narrative_ontology:affects_constraint(maritime_trade_monopoly, colonial_trade_dependency).
narrative_ontology:affects_constraint(maritime_trade_monopoly, port_infrastructure_lock_in).
narrative_ontology:affects_constraint(maritime_trade_monopoly, containerization_standard_adoption).

% DUAL FORMULATION NOTE:
% Maritime monopoly control operates through multiple linked constraints: legal/regulatory capture (flag-state monopoly), infrastructure control (port gatekeeping), and rate-setting power (freight cartel). Each component has distinct epsilon and operates through different suppression mechanisms. The unified story treats the monopoly as a single constraint because the extraction mechanism is unified across all components. Decomposition would be warranted if one component (e.g., containerization standards) showed structural independence with distinct epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maritime_trade_monopoly, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
