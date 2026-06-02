% ============================================================================
% CONSTRAINT STORY: electricity_grid_reliability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electricity_grid_reliability, []).

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
 *   constraint_id: electricity_grid_reliability
 *   human_readable: Electricity Grid Reliability as Extraction and Coordination
 *   domain: infrastructure/energy_systems
 *
 * SUMMARY:
 *   Electricity grid reliability represents a constraint that generates
 *   genuine coordination benefits (preventing cascading blackouts, optimizing
 *   load distribution) while simultaneously enabling extractive rent-seeking
 *   by utility monopolies. The constraint exhibits tangled rope structure:
 *   utilities coordinate on grid stability while extracting consumer surplus
 *   through rate structures, regulatory capture, and barriers to distributed
 *   generation. The extractiveness value (0.52) reflects the accumulation of
 *   small extraction mechanisms (rate markups, interconnection delays, demand
 *   response penalties, stranded cost recovery) rather than a single coercive
 *   mechanism. Suppression (0.68) stems from residential consumers' trapped
 *   dependency on the grid with no practical exit option. The theater ratio
 *   (0.55) indicates moderate performative content — reliability metrics are
 *   maintained but increasingly disconnected from actual failure modes in
 *   modern grids with high renewable penetration. The constraint is
 *   downstream of technological change (declining solar/battery costs, smart
 *   metering) that creates genuine alternatives but is architecturally
 *   entrenched through regulatory and contractual mechanisms.
 *
 * KEY AGENTS:
 *   - Residential Consumers: Primary victims (powerless/trapped) — dependent on grid with no exit; bear extraction through rates and service degradation
 *   - Utility Companies: Primary beneficiaries (institutional/arbitrage) — capture monopoly rents; design standards that entrench their control
 *   - Grid Operators: Secondary beneficiary (institutional/arbitrage) — coordinate grid operations; benefit from current architectural assumptions
 *   - Small Businesses: Secondary victim (moderate/constrained) — face high barrier to off-grid alternatives; experience extraction through reliability premiums
 *   - Large Industrial Consumers: Powerful player (powerful/arbitrage) — can negotiate preferential terms and alternative arrangements; not typical beneficiary of consumer-facing extraction
 *   - Renewable Energy Sector: Emerging actor (organized/constrained) — experiencing extraction through regulatory gatekeeping while contributing to coordination through grid services
 *   - Regulatory Agencies: Institutional actor (institutional/arbitrage) — maintain reliability standards that may be increasingly performative; enable utility rate recovery mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electricity_grid_reliability, 0.52).
domain_priors:suppression_score(electricity_grid_reliability, 0.68).
domain_priors:theater_ratio(electricity_grid_reliability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electricity_grid_reliability, extractiveness, 0.52).
narrative_ontology:constraint_metric(electricity_grid_reliability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(electricity_grid_reliability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electricity_grid_reliability, tangled_rope).
narrative_ontology:human_readable(electricity_grid_reliability, "Electricity Grid Reliability as Extraction and Coordination").
narrative_ontology:topic_domain(electricity_grid_reliability, "infrastructure/energy_systems").

domain_priors:requires_active_enforcement(electricity_grid_reliability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electricity_grid_reliability, utility_companies).
narrative_ontology:constraint_beneficiary(electricity_grid_reliability, large_industrial_consumers).
narrative_ontology:constraint_beneficiary(electricity_grid_reliability, grid_operators).
narrative_ontology:constraint_victim(electricity_grid_reliability, residential_consumers).
narrative_ontology:constraint_victim(electricity_grid_reliability, small_businesses).
narrative_ontology:constraint_victim(electricity_grid_reliability, grid_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESIDENTIAL CONSUMER (SNARE) — Trapped within the grid with no exit option. Dependent on continuous electricity supply for basic needs (heating, refrigeration, medical devices). Bears cost of reliability infrastructure through rates while having no influence over grid management or investment decisions. Grid failures impose sudden, severe costs; cannot substitute or escape.
constraint_indexing:constraint_classification(electricity_grid_reliability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS (TANGLED ROPE) — Constrained by high cost of distributed generation (solar, batteries, backup generators) but experiences coordination benefit from grid stability. Outages create catastrophic losses (spoilage, equipment damage, lost sales); grid investment reduces these risks. High suppression (exit cost via off-grid systems is prohibitive) combined with genuine coordination function (reliable supply enables commerce).
constraint_indexing:constraint_classification(electricity_grid_reliability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UTILITY COMPANY (ROPE) — Experiences the constraint as coordination mechanism. Grid reliability enables their core business; investment in redundancy and monitoring is a coordination function that they benefit from directly. Rate structures allow cost recovery; regulatory arbitrage options exist (shifting costs to consumers, jurisdictional variation). Net beneficiary with flexible exit capacity.
constraint_indexing:constraint_classification(electricity_grid_reliability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: LARGE INDUSTRIAL CONSUMER (ROPE) — Can negotiate preferential grid access and reliability guarantees. May invest in on-site generation or microgrids as backup. Experiences grid as coordination mechanism for baseline power; invests in alternatives for peak/backup needs. Powerful exit options and arbitrage capacity.
constraint_indexing:constraint_classification(electricity_grid_reliability, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: RENEWABLE ENERGY SECTOR (TANGLED ROPE) — Experiences dual constraint: grid reliability standards create coordination requirements (synchronization, frequency regulation) that benefit system stability, but also create barrier-to-entry requirements (interconnection standards, reliability certifications) that concentrate power with incumbent utilities. Coordinating on renewable integration while being extracted through regulatory gatekeeping.
constraint_indexing:constraint_classification(electricity_grid_reliability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — Traditional reliability standards (n-1 contingency, spinning reserve requirements) are increasingly performative as generation becomes distributed and demand more dynamic. Regulations persist through institutional inertia despite unclear effectiveness in modern grid contexts. Theater ratio reflects that reliability metrics are maintained ceremonially while actual grid failure modes have shifted.
constraint_indexing:constraint_classification(electricity_grid_reliability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL LIMITS VIEW (MOUNTAIN) — From civilizational/universal view, some grid reliability constraint is immutable: power must flow; demand and supply must balance instantaneously; physics imposes hard limits on transmission capacity and response speed. However, the base properties contradict mountain classification — the high extractiveness (0.52) and suppression (0.68) reveal that observed 'limits' are largely institutional choices rather than physical necessities.
constraint_indexing:constraint_classification(electricity_grid_reliability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electricity_grid_reliability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electricity_grid_reliability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electricity_grid_reliability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(electricity_grid_reliability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(electricity_grid_reliability, TR),
    TR >= 0.70.

:- end_tests(electricity_grid_reliability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint embodies multiple extraction mechanisms: (1) monopoly rate structures with limited consumer price elasticity (~0.15 of extractiveness); (2) regulatory cost-recovery allowing utilities to pass infrastructure costs to consumers while capturing returns (~0.20); (3) barriers to distributed generation and microgrids that preserve utility dependency (~0.12); (4) demand response penalties that extract behavioral compliance (~0.05). The value reflects that this is not maximal extraction — consumers receive reliable service and utilities genuinely invest in grid maintenance. However, the extracted surplus flows primarily to utility shareholders rather than being retained as consumer benefit or reinvested in grid modernization. Suppression (0.68): High. Residential consumers face multiple barriers to exit: (1) no practical off-grid alternative for urban/suburban populations (cost, zoning, weather dependency); (2) regulatory prohibition on certain distributed generation configurations; (3) grid dependency is existential (heating, refrigeration, medical devices — no substitute exists). Small businesses face somewhat lower suppression (constrained rather than trapped) because solar + battery systems are technically viable but economically prohibitive. Theater ratio (0.55): Moderate. Traditional reliability standards (n-1 contingency, 99.99% uptime) are maintained through regulatory requirement, but their relevance is declining. Modern grid challenges (renewable ramp rates, demand volatility) are not addressed by traditional metrics. Smart grid investments are partly genuine coordination (enabling distributed generation management) and partly theater (IoT deployments that enable utility data extraction without improving consumer visibility). The theater has increased over the interval as utilities adopt smart grid language without fundamentally changing customer relationship or control architecture.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same underlying mechanism (centralized grid control with monopoly rate structures) produces opposite experienced extractiveness for different agents. A utility company describing 'grid reliability' emphasizes the coordination problem (synchronization, frequency regulation, cascade prevention) and the solution (their technical expertise and capital investment). A residential consumer experiencing 'grid reliability' emphasizes their trapped dependency and the extract revenue through rates. Both descriptions are structurally accurate — the same constraint is genuinely a coordination mechanism for utilities and genuinely an extraction mechanism for consumers. The perspectival gap is not bridged by better description; it reveals structural asymmetry: the utility benefits from coordination while consumers bear the cost of maintaining the coordination infrastructure without having voted on its design or profiting from it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. Utility companies benefit from the constraint (low d, ~0.10-0.15) with arbitrage options (can shift costs, access regulatory mechanisms, invest in alternatives). They experience the constraint as coordination (rope). Residential consumers are victims (high d, ~0.85-0.95) with trapped exit options. They experience maximum extraction given their powerless status. Small businesses are secondary victims (d ~0.70) with constrained exit — they can theoretically exit through distributed generation but at prohibitive cost. Large industrial consumers are structurally on the beneficiary side (d ~0.35) despite powerful status — they can negotiate preferential rates and have genuine alternatives, making their relationship more one of leverage than extraction. Renewable energy sector appears as victim (d ~0.60) through regulatory gatekeeping but as partial beneficiary through grid service compensation — the high extractiveness reflects this ambiguity. Grid operators derive d from institutional status with beneficiary positioning (~0.15). The pipeline's sigmoid f(d) amplifies the gap: powerless trapped consumers get f(d) ≈ 1.42, creating maximum effective extraction; institutional beneficiaries get f(d) ≈ -0.12, experiencing negative extraction (they are subsidized by the constraint structure).
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL DECOMPOSITION NEEDED: The constraint exhibits high theater ratio (0.55) combined with genuine coordination function (synchronization, cascade prevention) and genuine extraction (monopoly rents, rate structures). This is tangled rope by definition, but the theater component is rising and the composition is shifting. The omega variables suggest that if distributed generation penetration exceeds ~40%, the constraint may decompose: (1) physical grid coordination remains (mountain: synchronization requires coordination) but becomes peer-to-peer rather than centralized; (2) monopoly extraction mechanism disappears (utilities lose gatekeeping power); (3) new coordination mechanisms emerge (local balancing, frequency support from distributed resources). The mandatrophy is resolved by recognizing that the current classification is time-dependent: the constraint is tangled rope NOW because utilities retain gatekeeping power, but is transitioning toward EITHER (a) rope (if utilities successfully rebrand as coordination platform providers for distributed generation) or (b) multiple constraints (if the system decomposes into local microgrids with peer-to-peer coordination, each with its own reliability constraint at lower ε). The high theater and rising extractiveness suggest the constraint is approaching a bifurcation point where the classification must be re-evaluated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_generation_threshold,
    'At what penetration level does distributed renewable generation eliminate the extractive monopoly structure of centralized grid control?',
    'Time-series analysis of grid architecture transitions; comparison of extraction metrics (residential rates vs system costs) at 20%, 50%, 80% renewable penetration thresholds',
    'If threshold < 40%: current grid structure''s extraction is contingent and could dissolve soon. If threshold > 70%: extraction may persist even with high renewable penetration due to entrenched control mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_generation_threshold, empirical, 'Distributed generation penetration level that disrupts centralized grid extraction').

omega_variable(
    reliability_vs_resilience_ambiguity,
    'Is ''grid reliability'' primarily about preventing rare catastrophic failures (n-1 contingency logic) or about rapid recovery from frequent minor disruptions (resilience)?',
    'Historical failure data analysis; comparison of cost-benefit for hardening (preventing failures) vs. adaptability (managing failures); survey of residential consumer preferences for reliability vs. cost',
    'If primarily prevention: current centralized architecture is efficient (mountain perspective). If primarily resilience: distributed generation with local storage is superior (snare becomes rope for consumers, extraction mechanism dissolves).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliability_vs_resilience_ambiguity, conceptual, 'Conceptual ambiguity between preventing failures vs. managing recovery').

omega_variable(
    regulatory_gatekeeping_necessity,
    'Are interconnection standards and reliability certifications necessary for grid stability or are they extractive barriers protecting utility monopolies?',
    'Comparative analysis of grid stability metrics in deregulated vs. regulated jurisdictions; empirical testing of grid stability with simplified interconnection rules; case studies of successful peer-to-peer microgrids without utility gatekeeping',
    'If necessary: regulatory framework is genuine coordination (rope classification confirmed). If extractive: regulations are piton/snare (consumers experience extraction through artificial barriers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_gatekeeping_necessity, empirical, 'Whether regulatory gatekeeping is necessary for grid stability or purely extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electricity_grid_reliability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grid_rel_tr_t0, electricity_grid_reliability, theater_ratio, 0, 0.38).
narrative_ontology:measurement(grid_rel_tr_t15, electricity_grid_reliability, theater_ratio, 15, 0.47).
narrative_ontology:measurement(grid_rel_tr_t30, electricity_grid_reliability, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(grid_rel_be_t0, electricity_grid_reliability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(grid_rel_be_t15, electricity_grid_reliability, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(grid_rel_be_t30, electricity_grid_reliability, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electricity_grid_reliability, global_infrastructure).
narrative_ontology:boltzmann_floor_override(electricity_grid_reliability, 0.22).
narrative_ontology:affects_constraint(electricity_grid_reliability, electricity_market_concentration).
narrative_ontology:affects_constraint(electricity_grid_reliability, distributed_generation_gatekeeping).
narrative_ontology:affects_constraint(electricity_grid_reliability, demand_response_pricing).

% DUAL FORMULATION NOTE:
% Grid reliability as a constraint decomposes into three structurally distinct components: (1) physical synchronization requirement (mountain-like, ε ≤ 0.10); (2) utility monopoly structure enabling extraction (tangled rope, ε ≈ 0.52); (3) regulatory gatekeeping of distributed generation (snare for renewables sector, ε ≈ 0.65). The current JSON story treats these as a single tangled rope by aggregating the mechanisms. Alternative decomposition into three separate stories with network links would enable more precise classification of each component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electricity_grid_reliability, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
