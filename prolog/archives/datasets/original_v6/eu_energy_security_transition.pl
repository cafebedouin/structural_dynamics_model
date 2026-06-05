% ============================================================================
% CONSTRAINT STORY: eu_energy_security_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_energy_security_transition, []).

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
 *   constraint_id: eu_energy_security_transition
 *   human_readable: EU Energy Security Transition: Renewable Coordination with Asymmetric Cost Distribution
 *   domain: energy_policy/geopolitics
 *
 * SUMMARY:
 *   The EU energy security transition from Russian gas dependency to
 *   renewable energy represents a hybrid coordination-extraction constraint.
 *   The constraint solves a genuine collective action problem (reducing
 *   geopolitical vulnerability to energy coercion) and coordinates complex
 *   technical infrastructure (grid modernization, renewable deployment,
 *   storage systems). However, this coordination mechanism simultaneously
 *   extracts from coal-dependent regions, energy-intensive industries, and
 *   less-developed southern EU states through concentrated costs, suppressed
 *   exit options, and debt-financed transitions. The constraint exhibits
 *   genuine coordination function (preventing grid collapse, enabling
 *   distributed renewable integration) alongside asymmetric cost distribution
 *   (coal workers trapped without retraining funding, southern states
 *   constrained by fiscal rules, energy-intensive sectors facing carbon
 *   pricing). The theater ratio is moderate and rising, reflecting increasing
 *   regulatory complexity (ETS reporting, carbon accounting, grid
 *   coordination protocols) that justifies institutional maintenance
 *   independent of whether efficiency gains justify the overhead. The
 *   extractiveness metric rises over the interval (0.32 → 0.62) as transition
 *   costs accumulate and coal plant closures accelerate without corresponding
 *   compensation mechanisms maturing.
 *
 * KEY AGENTS:
 *   - Coal-Dependent Region Workers: Primary victim (powerless/trapped) — located in Silesia, Ruhr Valley, eastern Europe; face geographic immobility and skill-specific unemployment; retraining programs severely underfunded relative to transition speed
 *   - Renewable Energy Producers & Grid Technology Firms: Primary beneficiary (institutional/arbitrage) — including Orsted, Siemens Energy, grid modernization contractors; capture investment flows, subsidies, and new market creation
 *   - EU Climate Policy Coalition: Organized beneficiary (organized/constrained) — EU Commission climate directorate, Nordic governments, environmental NGOs; maintain transition as temporary constraint with sunset (2050 neutrality target); have agency to modify terms
 *   - Energy-Intensive Manufacturing: Secondary victim (moderate/constrained) — steel, chemicals, cement, automotive manufacturing; constrained by carbon pricing and energy cost increases; benefit from industrial coordination and long-term supply security
 *   - Southern EU States: Institutional victim (institutional/constrained) — Italy, Spain, Greece, Portugal; face asymmetric extraction through debt-financed infrastructure spending under fiscal constraints; constrained by EU fiscal rules and ECB policy
 *   - Incumbent Gas Infrastructure: Inert beneficiary (institutional/arbitrage) — LNG terminals, pipeline networks; persist through regulatory authority; see declining functional value but maintain institutional roles through elaborate monitoring regimes
 *   - Analytical Observer: Geopolitical perspective (analytical/analytical) — sees constraint as genuine security coordination with embedded new dependencies; recognizes both liberation from Russian coercion and vulnerability to critical minerals concentration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_energy_security_transition, 0.58).
domain_priors:suppression_score(eu_energy_security_transition, 0.62).
domain_priors:theater_ratio(eu_energy_security_transition, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_energy_security_transition, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_energy_security_transition, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eu_energy_security_transition, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_energy_security_transition, tangled_rope).
narrative_ontology:human_readable(eu_energy_security_transition, "EU Energy Security Transition: Renewable Coordination with Asymmetric Cost Distribution").
narrative_ontology:topic_domain(eu_energy_security_transition, "energy_policy/geopolitics").

domain_priors:requires_active_enforcement(eu_energy_security_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_energy_security_transition, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(eu_energy_security_transition, northern_eu_states).
narrative_ontology:constraint_beneficiary(eu_energy_security_transition, advanced_grid_technology_firms).
narrative_ontology:constraint_victim(eu_energy_security_transition, coal_dependent_regions).
narrative_ontology:constraint_victim(eu_energy_security_transition, energy_intensive_industries).
narrative_ontology:constraint_victim(eu_energy_security_transition, southern_eu_states).
narrative_ontology:constraint_victim(eu_energy_security_transition, low_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL-DEPENDENT REGION WORKERS (SNARE) — Trapped by geographic location and specialized skills with no alternative employment. Structural suppression is total: coal plants are closing, retraining programs are underfunded, relocation is economically impossible. Bears full extraction cost with no exit option or compensation pathway. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(eu_energy_security_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ENERGY-INTENSIVE MANUFACTURING (TANGLED ROPE) — Constrained by high transition costs and carbon pricing. Benefits from reliable grid and industrial coordination through green transition, but faces significant extraction through energy cost increases and carbon border adjustment mechanisms. Genuine coordination function (grid stability, supply chain integration) exists alongside asymmetric cost distribution.
constraint_indexing:constraint_classification(eu_energy_security_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY PRODUCERS & GRID TECH FIRMS (ROPE) — Primary beneficiaries with substantial arbitrage options. Transition creates new market opportunities, subsidies, and investment flows. Experience the constraint as pure coordination: renewable integration requires network coordination, but this coordination generates their profit. Net positive position with exit options through geographic diversification and market positioning.
constraint_indexing:constraint_classification(eu_energy_security_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EU CLIMATE POLICY COALITION (SCAFFOLD) — Organized agents (environmental NGOs, Nordic governments, EU Commission climate directorate) see the transition as temporary coordination challenge with built-in sunset: the 2050 climate neutrality target and renewable deployment targets create hard deadline for transition completion. Low effective extraction because the coalition has agency, see the goal as legitimate, and maintain that the constraint dissolves once renewable capacity reaches critical mass (est. 80% penetration in 15-20 years).
constraint_indexing:constraint_classification(eu_energy_security_transition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: SOUTHERN EU STATES (TANGLED ROPE) — Constrained by EU fiscal rules and infrastructure funding dependencies. Nominally benefit from renewable potential (high solar/wind resources) and green transition funding, but face asymmetric extraction through: (1) forced debt-financed transition spending under tight fiscal constraints; (2) climate migration pressures; (3) grid infrastructure costs concentrated on less wealthy states. Experience genuine coordination benefits (energy independence from Russian gas) alongside significant extraction of fiscal and social capacity.
constraint_indexing:constraint_classification(eu_energy_security_transition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INCUMBENT GAS INFRASTRUCTURE & REGULATORY APPARATUS (PITON) — LNG import facilities, gas pipeline networks, and regulatory oversight bodies designed for fossil fuel markets persist through institutional inertia despite declining strategic value. The EU Emissions Trading System and grid regulation create elaborate performative compliance mechanisms that justify their own existence independent of whether they efficiently drive transition. Theater ratio rises as regulatory complexity increases (reporting, monitoring, carbon accounting) while actual emissions reduction accelerates through market forces. These institutions maintain themselves through regulatory authority rather than functional necessity.
constraint_indexing:constraint_classification(eu_energy_security_transition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL LENS (TANGLED ROPE) — From civilizational perspective, the energy transition solves a genuine geopolitical vulnerability (Russian gas dependency) through coordinated renewable deployment. However, it simultaneously extracts through: (1) concentration of critical minerals (lithium, cobalt) in geopolitically unreliable sources; (2) extreme weather vulnerability of renewable-dependent grids; (3) technological lock-in to renewable supply chains. Pure coordination view (independence from Russian coercion) coexists with new dependencies and asymmetric vulnerability distribution across EU members. The constraint is genuinely hybrid — solves one extraction mechanism while embedding others.
constraint_indexing:constraint_classification(eu_energy_security_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_energy_security_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_energy_security_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_energy_security_transition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_energy_security_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_energy_security_transition, TR),
    TR >= 0.70.

:- end_tests(eu_energy_security_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, rising over interval. The constraint solves the genuine geopolitical problem of Russian gas dependency through coordinated renewable deployment, but this coordination mechanism simultaneously extracts from powerless and constrained agents. The extractiveness rises as (1) coal plant closure dates accelerate without proportional retraining funding, (2) carbon pricing mechanisms layer onto the transition, (3) renewable deployment creates winners (energy producers, northern states with grid capacity) and losers (coal workers, southern states with debt constraints). The 0.32 → 0.62 trajectory reflects transition acceleration without corresponding distribution mechanism maturation. Suppression (0.62): High. Coal workers face near-total suppression: geographic immobility, skill-specific unemployment, relocation costs, and inadequate retraining programs. Energy-intensive industries face high suppression through carbon pricing and energy cost increases. Southern EU states face fiscal suppression (debt requirements for grid modernization). However, suppression is not absolute for all agents — renewable energy producers and northern EU states with grid capacity have exit options and arbitrage potential. Theater ratio (0.48): Moderate and rising. ETS reporting, carbon accounting, grid coordination protocols create elaborate regulatory theater that justifies institutional roles (gas infrastructure operators, grid regulators) independent of whether they efficiently drive transition. The theater is rising because regulatory complexity increases while actual transition accelerates through market forces (renewable costs falling, stranded gas assets, investor capital reallocation).
 *
 * PERSPECTIVAL GAP:
 *   Coal workers (powerless/trapped) classify the constraint as snare — maximum extraction with no exit. Renewable energy producers (institutional/arbitrage) classify it as rope — pure coordination enabling their market. Climate coalitions (organized/constrained) classify it as scaffold — temporary problem with sunset. Southern EU states (institutional/constrained) classify it as tangled rope — genuine coordination benefits (Russian independence) with asymmetric extraction (debt-financed transition). Energy-intensive industries (moderate/constrained) classify it as tangled rope — industrial coordination plus carbon pricing extraction. Incumbent gas operators (institutional/arbitrage) classify it as piton — persisting through regulatory theater despite functional decline. The analytical observer classifies it as tangled rope — genuine security coordination with embedded new dependencies. The perspectival gap reveals the constraint's fundamental tension: it solves one extraction mechanism (Russian coercion) while potentially embedding others (critical minerals dependency, grid fragility, fiscal debt). The gap is not resolvable through single classification — it is diagnostic of structural hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) derives from their base relationship to the constraint. Coal workers have high d (full targets of extraction) + trapped exit → f(d) ≈ 1.42 → χ amplified. Renewable producers have low d (beneficiaries with arbitrage) → f(d) ≈ -0.12 → χ dampened or negative. Energy-intensive industries are symmetric (both targets and partial beneficiaries) + constrained exit → d ≈ 0.55, f(d) ≈ 0.75 → moderate chi. Southern EU states are partial victims + constrained exit → d ≈ 0.60, f(d) ≈ 0.85 → moderate-high chi. The constraint operates at continental scope (σ(S) = 1.1), amplifying chi for all agents. No directionality override is needed — the structural data (beneficiary/victim declarations + exit options) captures the real directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is UNRESOLVED. The constraint genuinely coordinates (solves Russian dependency problem) and genuinely extracts (asymmetric cost distribution). The false summits are: (1) the mountain view that energy transition is immutable natural law (it is contingent institutional choice, though solving a real problem), (2) the rope-only view that it is pure coordination (extraction is real and asymmetric). The true classification is tangled_rope — genuine coordination function + asymmetric extraction + active enforcement (mandatory targets, carbon pricing). However, mandatrophy resolution requires determining whether the extraction is a necessary coordination cost or exploitative overhead. The just-transition funding omega variable is critical: if actual funding is adequate, extraction is justified as coordination cost; if funding is inadequate, extraction is unjustified and the constraint should downgrade to snare at certain perspectives. Current evidence (coal region transitions globally underfunded, southern EU fiscal constraints not eased, retraining programs 60-70% underfunded relative to need) suggests extraction exceeds justified coordination cost — indicating mandatrophy failure at the practical level even if the constraint is theoretically tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_minerals_dependency_substitution,
    'Does the transition from Russian gas dependency to renewable energy represent genuine liberation or substitution of one extractive dependency for another (critical minerals, rare earths from China, lithium from geopolitically unstable regions)?',
    'Supply chain analysis of lithium, cobalt, rare earth sourcing; geopolitical risk assessment of primary suppliers; technological substitution pathways (sodium-ion, solid-state batteries reducing lithium concentration)',
    'If genuine liberation: snare and tangled_rope classifications are justified (exit from Russian extraction achieved). If substitution: the constraint may be rope-only at analytical level (coordinates vulnerability reduction even if imperfectly). If worse substitution: reclassify as snare at civilizational level (new locked-in dependency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_minerals_dependency_substitution, empirical, 'Whether energy transition solves or substitutes extraction dependencies').

omega_variable(
    renewable_grid_fragility_nonlinear_risk,
    'Does high renewable penetration (70%+) create nonlinear grid fragility risks (cascading blackouts, weather-driven supply shocks) that concentrate new forms of suppression on population segments unable to secure backup power?',
    'Grid simulation studies of extreme weather scenarios; modeling of blackout probability and duration at various renewable penetration levels; analysis of backup power access correlation with wealth/geography',
    'If high fragility risk: the constraint is snare from the perspective of communities without backup power (new trapped status despite nominal energy independence). If manageable through storage/interconnection: tangled_rope holds (coordination cost is legitimate). If fragility is mitigated regionally: reclassify southern EU perspective from tangled_rope to rope (coordination without extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_grid_fragility_nonlinear_risk, empirical, 'Nonlinear fragility risks in high-renewable grids').

omega_variable(
    just_transition_funding_feasibility,
    'Is the actual (not pledged) funding available for coal-region retraining, worker compensation, and economic diversification sufficient to prevent multi-generational poverty traps in affected areas?',
    'Comparison of historical precedent (post-industrial region transitions: Ruhr Valley, Appalachia, Polish Silesia); gap analysis between pledged funds and economic modeling of transition costs; longitudinal tracking of coal-region employment and income in early transition cohorts',
    'If funding is adequate: snare classification may be too severe for coal workers (constrained or mobile becomes realistic). If funding is inadequate (likely): snare classification confirmed — workers remain trapped without meaningful exit options or compensation. Mandatrophy resolution depends on this factual determination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(just_transition_funding_feasibility, empirical, 'Actual funding adequacy for just transition').

omega_variable(
    coordination_vs_coercion_threshold,
    'At what point does the EU''s mandatory renewable targets and coal shutdown timelines transition from coordination (solving genuine collective action problem) to coercion (imposing costs asymmetrically on powerless groups to solve beneficiary problem)?',
    'Comparative analysis: regions where transition is locally chosen vs. EU-mandated; agent autonomy in transition pathway selection; presence/absence of alternative pathways with same climate outcome',
    'If coercion threshold is crossed before technical completion: reclassify from tangled_rope to snare at EU institutional level (extraction mechanism disguised as coordination). If coordination framing holds: tangled_rope is justified, and mandatrophy is resolved through hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_coercion_threshold, conceptual, 'Coordination vs coercion threshold in mandatory transition').

omega_variable(
    european_technological_autonomy_feasibility,
    'Can the EU develop independent manufacturing and supply chains for batteries, grid storage, and renewable components, or will lock-in to non-EU suppliers (China for batteries, Middle East for rare earths, non-EU for semiconductor manufacturing) create structural vulnerability?',
    'Technology roadmapping for EU-autonomous production; cost/capacity projections for domestic manufacturing; geopolitical risk assessment of supplier diversification',
    'If EU autonomy is feasible: the transition genuinely reduces extraction (independence from Russian gas + European tech sovereignty). If not feasible: new asymmetric extraction emerges (European technology firms dependent on non-EU supply), and civilizational-level classification downgrades from tangled_rope to snare at global scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_technological_autonomy_feasibility, empirical, 'EU technological autonomy in renewable supply chains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_energy_security_transition, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_energy_tr_t0, eu_energy_security_transition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eu_energy_tr_t5, eu_energy_security_transition, theater_ratio, 5, 0.42).
narrative_ontology:measurement(eu_energy_tr_t10, eu_energy_security_transition, theater_ratio, 10, 0.48).
narrative_ontology:measurement(eu_energy_tr_t15, eu_energy_security_transition, theater_ratio, 15, 0.51).

% Extraction over time
narrative_ontology:measurement(eu_energy_be_t0, eu_energy_security_transition, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eu_energy_be_t5, eu_energy_security_transition, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(eu_energy_be_t10, eu_energy_security_transition, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(eu_energy_be_t15, eu_energy_security_transition, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_energy_security_transition, global_infrastructure).
narrative_ontology:affects_constraint(eu_energy_security_transition, european_critical_minerals_supply_security).
narrative_ontology:affects_constraint(eu_energy_security_transition, grid_stability_extreme_weather).
narrative_ontology:affects_constraint(eu_energy_security_transition, coal_worker_regional_collapse).
narrative_ontology:affects_constraint(eu_energy_security_transition, russian_energy_coercion).

% DUAL FORMULATION NOTE:
% The energy transition is downstream of the Russian energy coercion constraint (solving that extraction mechanism) but upstream of critical minerals dependency (embedding new extraction). Decomposition: (1) renewable_grid_coordination (high ε, rope-focused), (2) coal_region_transition_funding (high ε, snare-focused), (3) critical_minerals_lock_in (medium ε, snare-focused). All three are linked — the coordination requires the funding requires the minerals sourcing. Each has distinct ε based on observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_energy_security_transition, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
