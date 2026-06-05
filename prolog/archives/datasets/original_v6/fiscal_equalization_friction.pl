% ============================================================================
% CONSTRAINT STORY: fiscal_equalization_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiscal_equalization_friction, []).

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
 *   constraint_id: fiscal_equalization_friction
 *   human_readable: The Equalization Conflict (Net Transfer Friction)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Canada's equalization program represents a fiscal federalism coordination
 *   mechanism designed to enable all provinces to deliver comparable public
 *   services despite unequal tax bases. The program transfers federal revenue
 *   to lower-capacity provinces, funding education, healthcare, and
 *   infrastructure. However, the mechanism generates persistent friction:
 *   contributing provinces (Ontario, Alberta, British Columbia) experience
 *   net extraction; receiving provinces benefit from transfers; the federal
 *   government administers the redistribution; and the equalization formula
 *   itself has become a site of chronic political contestation. The
 *   constraint exhibits the classic tangled_rope signature: genuine
 *   coordination function (enabling national market coherence and territorial
 *   equity) layered with asymmetric extraction (net transfer friction creates
 *   political resentment and constitutional instability). The theater ratio
 *   has increased over four decades as debate shifted from 'should we have
 *   equalization' to 'what formula is fair' — the performative dimension of
 *   formula disputes now dominates political discourse while the actual
 *   redistributive function remains unchanged.
 *
 * KEY AGENTS:
 *   - Receiving Provinces: Primary beneficiary (powerful/arbitrage) — Quebec, Manitoba, Saskatchewan, Atlantic provinces receive net transfers; benefit from public service capacity
 *   - Contributing Provinces: Primary victim (powerful/mobile) — Ontario, Alberta, British Columbia bear net transfer burden; experience extraction but retain political power to negotiate
 *   - Federal Government: Administrator (institutional/arbitrage) — controls transfer mechanism; can adjust formula, modify revenue-sharing, restructure equalization entirely
 *   - Low-Income Taxpayers in Contributing Provinces: Secondary victim (powerless/trapped) — bear tax burden with limited exit options; cannot relocate without significant cost
 *   - Fiscal Federalism Reform Coalition: Organized reformers (organized/constrained) — academics, policy institutes, provincial governments proposing improved formula designs with sunset/adjustment mechanisms
 *   - Equalization Formula Apparatus: Performative infrastructure (institutional/constrained) — technical calculation system, administrative machinery, political debate framework; persists through inertia despite chronic contestation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — assesses whether equalization is fundamentally necessary coordination or institutional artifact that could be restructured
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiscal_equalization_friction, 0.38).
domain_priors:suppression_score(fiscal_equalization_friction, 0.48).
domain_priors:theater_ratio(fiscal_equalization_friction, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiscal_equalization_friction, extractiveness, 0.38).
narrative_ontology:constraint_metric(fiscal_equalization_friction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fiscal_equalization_friction, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiscal_equalization_friction, tangled_rope).
narrative_ontology:human_readable(fiscal_equalization_friction, "The Equalization Conflict (Net Transfer Friction)").
narrative_ontology:topic_domain(fiscal_equalization_friction, "economic/political").

domain_priors:requires_active_enforcement(fiscal_equalization_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiscal_equalization_friction, receiving_provinces).
narrative_ontology:constraint_victim(fiscal_equalization_friction, contributing_provinces).
narrative_ontology:constraint_victim(fiscal_equalization_friction, federal_fiscal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EQUALIZATION-PAYING PROVINCE TAXPAYER (SNARE) — Cannot exit without relocating. Bears extraction through provincial revenue loss and federal tax burden. No meaningful exit option beyond personal migration. Maximum experienced extraction relative to coordination benefit.
constraint_indexing:constraint_classification(fiscal_equalization_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTRIBUTING PROVINCE (TANGLED ROPE) — Benefits from national federation and market integration (rope function) but extracted from via equalization mechanism (asymmetric cost). Political power enables exit via constitutional renegotiation and tax competition, but such exit is costly. Experiences both coordination benefit and extraction cost.
constraint_indexing:constraint_classification(fiscal_equalization_friction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT (ROPE) — Administers equalization as coordination mechanism enabling national market and fiscal coherence. Has arbitrage exit: can adjust federal transfers, modify revenue sharing, or restructure equalization formula. Experiences constraint as coordination problem solved by redistribution infrastructure.
constraint_indexing:constraint_classification(fiscal_equalization_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RECEIVING PROVINCE (ROPE) — Benefits directly from equalization transfers. Has political power and arbitrage exit via renegotiation or reduced resource development dependence. Experiences constraint primarily as coordination benefit: access to federal revenue-sharing enables public service delivery in lower-capacity provinces.
constraint_indexing:constraint_classification(fiscal_equalization_friction, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FISCAL FEDERALISM REFORM COALITION (SCAFFOLD) — Organized academic, policy, and provincial actors proposing structural reforms: adjusted formula, sunset triggers, efficiency incentives. See equalization as temporary institutional design with improvable mechanisms. View current friction as solvable by recalibrating parameters and transparency. Constrained exit via political process; organized engagement with sunset-compatible reforms.
constraint_indexing:constraint_classification(fiscal_equalization_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EQUALIZATION FORMULA MACHINERY (PITON) — The technical formula and administrative apparatus (revenue per capita calculations, five-province standard, cap mechanisms) persist through institutional inertia despite chronic contestation. The mechanism is substantially performative: debate over 'fairness' of the formula dominates political discussion while the actual redistributive function remains unchanged. Theater ratio reflects that much activity is formula-tweaking rather than structural reform.
constraint_indexing:constraint_classification(fiscal_equalization_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scale, equalization represents a genuine coordination function (enabling national market and fiscal coherence) layered with asymmetric extraction (net transfer friction creates persistent resentment and constitutional instability). The constraint is hybrid: coordination mechanism generating extraction side-effects that destabilize the original function.
constraint_indexing:constraint_classification(fiscal_equalization_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_equalization_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fiscal_equalization_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_equalization_friction, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fiscal_equalization_friction, TR),
    TR >= 0.70.

:- end_tests(fiscal_equalization_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The equalization program does represent genuine coordination function (enabling national market, reducing fiscal disparities), but extraction is significant. Contributing provinces lose tax capacity relative to federal/provincial revenue splits; receiving provinces gain capacity. The extraction is neither minimal (like a pure coordination mechanism such as a technical standard) nor severe (like predatory debt): it reflects a deliberate redistributive choice layered onto coordination infrastructure. Theater ratio (0.62): Moderately high. Political debate over equalization is substantially performative. The core redistributive function — moving resources from high-capacity to low-capacity provinces — is invariant to formula adjustments within observed bounds. Yet decades of political attention focus on 'fairness' of calculation (five-province standard, cap mechanisms, per-capita adjustments) rather than structural redesign. The performance has increased over time as technical formula disputes displaced fundamental design questions. Suppression (0.48): Moderate. Barriers to exit include constitutional complexity, political coalition requirements, market integration benefits of federation, and unequal information between provinces on true tax bases and fiscal needs. Contributing provinces have higher political power and genuine exit paths (constitutional renegotiation, aggressive tax competition), reducing suppression below what a 'powerless' population would experience. The mechanism is not maximally coercive because powerful actors retain meaningful voice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a three-way perspectival gap: (1) Receiving provinces and federal administrators see equalization as legitimate coordination enabling national fiscal coherence (Rope). (2) Contributing provinces experience mixed coordination and extraction — they benefit from federation but bear net transfer burden (Tangled Rope). (3) Low-income taxpayers in contributing provinces experience pure extraction with no meaningful exit (Snare). The analytical observer must integrate these perspectives and recognize that equalization is structurally a tangled_rope: it solves a genuine coordination problem (enabling comparable public services across provinces) while simultaneously extracting from contributing provinces (creating fiscal resentment and constitutional friction). The piton observation — that formula disputes dominate while core redistributive function remains unchanged — reflects the constraint's drift toward performative activity as the original coordination function has become institutionalized.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to extraction flow. Contributing provinces occupy d ~0.55-0.65 (powerful agents bearing costs, but with mobile exit options and political leverage): they derive negative f(d) but moderate suppression limits effective extraction. Receiving provinces occupy d ~0.15-0.25 (institutional beneficiaries with arbitrage options): they derive positive f(d) and coordination benefit. Low-income taxpayers in contributing provinces occupy d ~0.85-0.95 (powerless victims with trapped exit): they derive maximum f(d) and maximum experienced extraction. Federal government occupies d ~0.05-0.15 (institutional beneficiary administering the mechanism): derives negative f(d) and sees constraint as coordination tool. The perspectival gap emerges from this directionality variance: beneficiaries see rope, administrators see rope, but victims see snare and contributors see tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing equalization as genuinely hybrid: (1) Coordination function is real and substantial — federalism without equalization would fragment fiscal capacity and destabilize the common market. (2) Extraction is real and substantial — net transfer friction creates persistent constitutional tension and resentment in paying provinces. The constraint cannot be cleanly classified as either pure coordination (Rope) or pure extraction (Snare) because both functions are integral to its structure. The tangled_rope classification prevents mislabeling equalization as 'just coordination for mutual benefit' (which ignores redistribution) or 'just extraction by low-capacity provinces' (which ignores genuine public goods coordination). The theater ratio increase over time suggests some drift toward performative activity — the formula disputes have become decoupled from fundamental design choices — but the underlying coordination function persists. The constraint is not degrading into a Piton; the theater increase reflects political contestation of the mechanism, not atrophy of its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_resource_volatility_coupling,
    'Does equalization''s dependence on provincial resource wealth represent a legitimate coordination need or an artificial extractive coupling?',
    'Comparative analysis of resource-rich vs resource-poor federal systems (US, Australia, Germany); modeling of equalization without resource dependency; historical analysis of equalization design choices',
    'If legitimate: equalization is fundamental coordination mechanism. If artificial: current design could be reformed to decouple extraction from resource volatility, reducing friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_resource_volatility_coupling, empirical, 'Whether resource wealth coupling is structural necessity or design choice').

omega_variable(
    interprovincial_mobility_counterfactual,
    'Would interprovincial labor mobility increase significantly if equalization were eliminated or restructured?',
    'Analysis of internal migration patterns relative to equalization phase-in; comparison of interprovincial movement rates across different equalization regimes; survey of migration decision factors',
    'If mobility increases substantially: equalization is functionally necessary coordination. If mobility unchanged: equalization extraction is pure transfer with minimal coordination justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interprovincial_mobility_counterfactual, empirical, 'Counterfactual mobility impact of equalization absence').

omega_variable(
    provincial_exit_credibility,
    'Is the threat of constitutional renegotiation or province-wide tax resistance a credible exit mechanism, or rhetorical theater?',
    'Historical analysis of equalization disputes in constitutional negotiations (Meech Lake, Charlottetown, modern disputes); institutional modeling of constitutional amendment feasibility; political economy of provincial coalition formation',
    'If credible exit: contributing provinces experience tangled_rope (not snare). If theater: extraction is closer to snare, suppression higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(provincial_exit_credibility, empirical, 'Credibility of provincial constitutional exit threat').

omega_variable(
    formula_optimization_frontier,
    'Is the current equalization formula near an optimized balance between redistribution and efficiency incentives, or substantially suboptimal?',
    'Economic modeling comparing current formula performance against alternative designs (partial equalization, efficiency incentives, dynamic adjustment); longitudinal tracking of provincial fiscal capacity trends under current formula',
    'If near-optimal: friction is inherent to redistribution. If suboptimal: friction represents policy failure correctable by design reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formula_optimization_frontier, empirical, 'Optimality of current equalization formula design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiscal_equalization_friction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiscal_eq_tr_t0, fiscal_equalization_friction, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fiscal_eq_tr_t20, fiscal_equalization_friction, theater_ratio, 20, 0.55).
narrative_ontology:measurement(fiscal_eq_tr_t40, fiscal_equalization_friction, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(fiscal_eq_be_t0, fiscal_equalization_friction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fiscal_eq_be_t20, fiscal_equalization_friction, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(fiscal_eq_be_t40, fiscal_equalization_friction, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiscal_equalization_friction, resource_allocation).
narrative_ontology:affects_constraint(fiscal_equalization_friction, interprovincial_trade_friction).
narrative_ontology:affects_constraint(fiscal_equalization_friction, provincial_constitutional_stability).

% DUAL FORMULATION NOTE:
% Equalization friction is downstream of the federalism constraint but represents a distinct structural coordination-extraction hybrid. Upstream constraint is the federalism constitutional design; equalization is a specific mechanism within that design. The fiscal transfer mechanism itself generates secondary constraints (trade friction from tax rate competition, constitutional instability from redistribution disputes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fiscal_equalization_friction, powerful, 0.6).
constraint_indexing:directionality_override(fiscal_equalization_friction, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
