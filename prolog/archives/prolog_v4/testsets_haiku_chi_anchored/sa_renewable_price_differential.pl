% ============================================================================
% CONSTRAINT STORY: sa_renewable_price_differential
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sa_renewable_price_differential, []).

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
 *   constraint_id: sa_renewable_price_differential
 *   human_readable: SA Renewable Price Arbitrage Proxy
 *   domain: economic/technological
 *
 * SUMMARY:
 *   South Australia's renewable penetration reached 84% as of February 2026,
 *   driving wholesale electricity prices to $37/MWh — the lowest in the
 *   National Electricity Market (NEM). NSW wholesale prices remain at
 *   approximately $75/MWh, creating a sustained price differential that
 *   operates as an arbitrage proxy for deeper structural tensions in the NEM.
 *   The constraint exhibits both genuine coordination function (NEM
 *   interconnection enables economic dispatch of lowest-cost renewable
 *   generation across state lines) and extractive asymmetry (coal-dependent
 *   regions bear stranded asset costs while renewable beneficiaries and
 *   financial arbitrageurs capture rents). The tension between these
 *   functions makes it a Tangled Rope: the NEM coordination mechanism is real
 *   and socially beneficial (lower average wholesale costs, faster
 *   decarbonization), but it accompanies forced redistribution of capital
 *   from coal interests to renewable interests. The fundamental question is
 *   whether this redistribution reflects market-efficient signaling or
 *   policy-enforced externalization of transition costs.
 *
 * KEY AGENTS:
 *   - SA Renewable Generators: Primary beneficiary (institutional/arbitrage) — achieve cost-of-fuel-zero dispatch at scale; capture wholesale rents during SA leadership period
 *   - NSW Coal Generators: Primary victim (powerless/trapped) — face depreciation and closure decisions forced by wholesale price floor; cannot relocate assets; cannot arbitrage across markets
 *   - NSW Industrial Consumers: Secondary victim (moderate/constrained) — benefit from NEM wholesale cost reduction but pay higher retail prices due to network tariffs and retailer hedging; constrained by inability to relocate production to SA
 *   - NEM Arbitrage Traders: Secondary beneficiary (powerful/arbitrage) — extract value from SA/NSW spread via interconnect capacity trading; faces suppression from congestion limits
 *   - NEM Decarbonization Coalition: Organized agent (organized/constrained) — includes renewable investors, climate advocates, grid operators; sees constraint as beneficial coordination mechanism with natural sunset as NSW renewables scale
 *   - AEMO (NEM Balancing Authority): Institutional steward (institutional/constrained) — maintains system stability; increasingly manages renewable variability; sees balancing role as threatened by architectural obsolescence
 *   - Energy Transition Systems Analyst: Analytical observer (analytical/analytical) — sees full structure: coordination function genuine but accompanied by asymmetric extraction; classification depends on whether transition proceeds at market-rational or policy-forced pace
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sa_renewable_price_differential, 0.52).
domain_priors:suppression_score(sa_renewable_price_differential, 0.48).
domain_priors:theater_ratio(sa_renewable_price_differential, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sa_renewable_price_differential, extractiveness, 0.52).
narrative_ontology:constraint_metric(sa_renewable_price_differential, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sa_renewable_price_differential, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sa_renewable_price_differential, tangled_rope).
narrative_ontology:human_readable(sa_renewable_price_differential, "SA Renewable Price Arbitrage Proxy").
narrative_ontology:topic_domain(sa_renewable_price_differential, "economic/technological").

domain_priors:requires_active_enforcement(sa_renewable_price_differential).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sa_renewable_price_differential, sa_renewable_generators).
narrative_ontology:constraint_beneficiary(sa_renewable_price_differential, sa_industrial_consumers).
narrative_ontology:constraint_beneficiary(sa_renewable_price_differential, nem_arbitrage_traders).
narrative_ontology:constraint_victim(sa_renewable_price_differential, nsw_coal_generators).
narrative_ontology:constraint_victim(sa_renewable_price_differential, nsw_industrial_consumers).
narrative_ontology:constraint_victim(sa_renewable_price_differential, retail_price_pressure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NSW COAL GENERATOR (SNARE) — Trapped in depreciation cycle as SA's renewables force wholesale prices below operating costs. No exit without capital loss. Cannot arbitrage across states; trapped by location and asset lock. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.65.
constraint_indexing:constraint_classification(sa_renewable_price_differential, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NSW INDUSTRIAL CONSUMER (TANGLED ROPE) — Benefits from coordination: NEM integrates SA's cheap generation, reducing NSW wholesale costs. Constrained by inability to relocate production to SA (labor, logistics, land costs). Pays higher retail price than SA equivalent due to network tariffs + retailer hedging. d≈0.58, f(d)≈0.72, σ=0.9 → χ≈0.19.
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SA RENEWABLE OPERATOR (ROPE) — Primary beneficiary. Achieves record generation volumes at cost-of-fuel-zero. Experiences constraint as pure coordination: wholesale market pricing enables dispatch; NEM integration creates demand. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Net beneficiary via arbitrage exit (can expand, relocate capacity, optimize grid-facing contracts).
constraint_indexing:constraint_classification(sa_renewable_price_differential, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NEM ARBITRAGE TRADER (TANGLED ROPE) — Extracts value from SA/NSW price spread via interconnect capacity arbitrage: long SA, short NSW. Benefits from volatility and structural price floor ($37 SA) as anchor for spread bets. Faces suppression: interconnect congestion limits arbitrage capacity; AEMO intervention constrains dispatch; future renewable adequacy may compress spread. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NEM DECARBONIZATION COALITION (SCAFFOLD) — Organized agents (renewable investors, grid operators, climate advocates) see price arbitrage as transient coordination mechanism enabling fast renewable deployment. Theater ≤ 0.35 (substantive economic arbitrage, not pure performance). Structural sunset: as coal retires and NSW renewables scale, the SA/NSW spread compresses toward renewable parity pricing. d≈0.25, f(d)≈0.18, σ=1.0 → χ≈0.06. Low extraction because mechanism has genuine sunset and solves real coordination problem.
constraint_indexing:constraint_classification(sa_renewable_price_differential, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: AEMO / NEM BALANCING AUTHORITY (PITON) — Maintains system stability through interconnect management and constraint equations. Theater_ratio ≈ 0.38: much institutional activity around 'balancing' is adaptive response to renewable variability, but system stability measures (frequency control ancillary services = FCAS) increasingly performative as storage/demand response mature. Sees itself as solving imminent grid problems; actually maintaining inertia-dependent system architecture that newer tech is making obsolete. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(sa_renewable_price_differential, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ENERGY TRANSITION SYSTEMS ANALYST (TANGLED ROPE from analytical position) — Sees the price differential as both enabler (coordinates rapid SA renewable deployment) and extraction mechanism (captures rents from geographically arbitrary coal stranding, redistributes to renewable owners and traders). Recognizes coordination function (NEM integration, interconnect capacity) is genuine but accompanies asymmetric extraction (coal bearer loses capital, renewable beneficiary gains). d≈0.62, f(d)≈0.88, σ=1.2 → χ≈0.48. This is the classification the engine should compute as `constraint_claim` for the story.
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sa_renewable_price_differential_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sa_renewable_price_differential, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sa_renewable_price_differential, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sa_renewable_price_differential, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sa_renewable_price_differential, TR),
    TR >= 0.70.

:- end_tests(sa_renewable_price_differential_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The price differential ($75 NSW vs $37 SA) represents real economic rent capture by SA generators and a real cost imposed on NSW coal operators. However, this is not maximum extraction (0.75+) because the rent is open-value creation from renewable deployment, not purely redistributive: NEM-wide wholesale costs decline due to SA's low-cost generation. The extractiveness value reflects that ~half the price differential is pure rents (captured by renewable operators and traders) and ~half is genuine efficiency gain (dispersed across all NEM consumers). Suppression (0.48): Moderate. Significant barriers to escape: coal operators cannot immediately retire or relocate; NSW consumers face retail price stickiness; AEMO maintains inertia-dependent balancing requirements that slow institutional adaptation. However, suppression is not total — NSW renewable investment is proceeding rapidly, interconnect capacity provides partial arbitrage relief, and storage/demand response are eroding AEMO's monopoly on balancing. Theater ratio (0.35): Low. The constraint's mechanism is substantively economic — it reflects real physics (renewable cost curve, network physics, wholesale market pricing) with minimal performative overlay. Wholesale market pricing is functional, not theatrical; interconnect management is responsive to real congestion, not ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival split between NSW coal operators (who see a Snare with d≈0.92) and SA renewable operators (who see a Rope with d≈0.08). The gap reveals that the same price differential is experienced as catastrophic extraction by the stranded coal interest and as benign coordination by the renewable beneficiary. The NEM analyst sees the truth: it is Tangled Rope — coordination function (lower NEM costs, efficient dispatch) coupled with asymmetric extraction (coal depreciation, renewable rents). The key disagreement is whether this asymmetry is market-justified or policy-enforced. The coal generator would argue for Snare (unjust expropriation by renewable subsidies or carbon policy). The renewable operator would argue for Rope (legitimate market reward for capital-efficient technology). The analyst sees both: the mechanism is genuinely coordinating, but it is also genuinely extractive from the coal interest. The scaffold perspective (decarbonization coalition) anticipates sunset — as NSW renewable penetration rises toward SA levels, the spread compresses and extraction window closes. This temporal structure is what makes it Scaffold-like in the long view, even though current classification is Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   SA renewable generators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Negative effective extraction; net beneficiaries. NSW coal generators: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum directionality; maximum extraction. Cannot exit asset base or market position. NSW industrial consumers: Victim + constrained → d≈0.58, f(d)≈0.72. Moderate extraction; benefit from wholesale cost reduction but constrained from capturing full savings due to retail price stickiness and network tariff separation. NEM arbitrage traders: Mixed beneficiary-victim + arbitrage → d≈0.35, f(d)≈0.32. Lower extraction because arbitrage exit option is available; traders can rebalance portfolios or exit spread positions. AEMO: Institutional + constrained (not arbitrage) → d≈0.45, f(d)≈0.48. Moderate extraction derived from institutional constraint (legally mandated stability role limits flexibility) despite institutional power level. Energy analyst: Analytical position → d≈0.62, f(d)≈0.88. Sees the system from outside; observes coordination + extraction symmetrically; applies global scope σ=1.2 to account for NEM-wide impacts.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by explicitly declaring the dual structure: it is genuinely a Rope (NEM coordination, efficient dispatch) AND genuinely extractive (coal stranding, uncompensated transition costs). The classification is Tangled Rope, which permits exactly this: a constraint with both coordination function and asymmetric extraction, where both are structurally real. The mandatrophy would arise if one tried to classify it as ONLY Rope (ignoring coal depreciation) or ONLY Snare (ignoring genuine efficiency gains). The Tangled Rope classification is mandatrophy-resolving precisely because it acknowledges both: 'the constraint coordinates efficient generation dispatch (coordination benefit = real) while imposing uncompensated costs on coal-dependent regions (extraction = real).' The theater_ratio (0.35) is low because the mechanism is substantive, not performative — this distinguishes it from a Piton (degraded ritual). The beneficiaries (SA renewables, traders) are real and generate real coordination function. The victims (NSW coal, some consumers) experience real extraction, not theatrical pressure. The constraint passes the mandatrophy gate: both functions are present, suppression is substantial but not total, and the system retains capacity for policy-driven resolution (sunset via NSW renewable scaling, equity mechanisms via transition funding).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coal_exit_speed_threshold,
    'Does the SA/NSW price differential accelerate coal retirement at economically rational pace or force premature stranded assets?',
    'Longitudinal tracking of coal plant closure announcements and capex decisions; comparison of economic IRR for retained coal vs retirement date; correlation with wholesale price dynamics',
    'If rational: constraint is coordination mechanism (Rope/Scaffold). If accelerated: constraint is extractive (Snare/Tangled Rope). Classification hinges on whether coal retirement reflects market signals or policy-forced externalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_exit_speed_threshold, empirical, 'Whether price differential forces rational or premature coal exit').

omega_variable(
    retail_price_passthrough_asymmetry,
    'Do SA consumers realize wholesale price savings in retail pricing, or are they captured by retailer hedging and network tariff arbitrage?',
    'Price decomposition: wholesale (AEMO) vs network tariff (SA Power Networks) vs retailer margin. Longitudinal retail price tracking SA vs NSW controlled for consumption profile. Survey of retailer hedging contract terms.',
    'If passthrough complete: SA constraint benefits extend to end consumers (coordination). If captured: SA retail prices remain high despite wholesale collapse (extraction by retailers/network operators masked by wholesale visibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retail_price_passthrough_asymmetry, empirical, 'Degree of wholesale savings passthrough to SA retail consumers').

omega_variable(
    interconnect_capacity_constraint_origin,
    'Is the SA/NSW interconnect capacity constraint a technical reality (cable physics) or an institutional constraint (regulatory/investment choice)?',
    'Engineering audit of Heywood and Darlington interconnects: thermal limits, cable age, upgrade costs. Comparison of capital expenditure allocation for interconnect expansion vs other NEM upgrades. AEMO planning decisions analysis.',
    'If technical: arbitrage spread is immutable (Mountain-like constraint). If institutional: spread can be compressed by capex decisions (Tangled Rope with policy levers). Classification of financial extraction hinges on whether spread is structural or policy-enforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interconnect_capacity_constraint_origin, empirical, 'Technical vs institutional nature of interconnect capacity limits').

omega_variable(
    renewable_adequacy_convergence_timeline,
    'When will NSW renewable deployment reach SA-equivalent penetration, compressing the price arbitrage floor?',
    'Scenario modeling: NEM renewable build-out trajectory, capacity factors, storage additions. Comparison with historical SA deployment curves. Timeline to wholesale price convergence ($37 SA → $55+NSW).',
    'If < 5 years: scaffold sunset is imminent, extraction window limited (Scaffold classification confirmed). If > 15 years: extraction mechanism persists long-term (Snare/Tangled Rope). Timing determines whether constraint is transitional or locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_adequacy_convergence_timeline, empirical, 'Timeline for NSW renewable adequacy to compress SA/NSW price spread').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sa_renewable_price_differential, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_renew_tr_t0, sa_renewable_price_differential, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sa_renew_tr_t3, sa_renewable_price_differential, theater_ratio, 3, 0.28).
narrative_ontology:measurement(sa_renew_tr_t6, sa_renewable_price_differential, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(sa_renew_be_t0, sa_renewable_price_differential, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sa_renew_be_t3, sa_renewable_price_differential, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(sa_renew_be_t6, sa_renewable_price_differential, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sa_renewable_price_differential, resource_allocation).
narrative_ontology:affects_constraint(sa_renewable_price_differential, nsw_coal_depreciation_cycle).
narrative_ontology:affects_constraint(sa_renewable_price_differential, nem_interconnect_congestion).
narrative_ontology:affects_constraint(sa_renewable_price_differential, renewable_storage_investment_asymmetry).

% DUAL FORMULATION NOTE:
% The SA renewable price arbitrage is structurally decomposable into three linked constraints: (1) physical wholesale price floor set by SA renewable cost curves (high ε), (2) NSW coal asset depreciation cascade driven by wholesale price floor (high ε, high suppression), (3) interconnect congestion limiting arbitrage escape valve (moderate ε, coordination function). This story focuses on the integrated system (Tangled Rope, ε=0.52); the downstream constraints have higher ε values reflecting their more severe extraction mechanisms. All three are linked via network: the price differential drives depreciation decisions, which increases urgency for interconnect upgrade investment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sa_renewable_price_differential, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
