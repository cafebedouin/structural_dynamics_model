% ============================================================================
% CONSTRAINT STORY: ukrainian_energy_security_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ukrainian_energy_security_dependency, []).

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
 *   constraint_id: ukrainian_energy_security_dependency
 *   human_readable: Ukrainian Energy Security Dependency on Russian Gas and Coal Supplies
 *   domain: geopolitical/energy_security
 *
 * SUMMARY:
 *   Ukrainian energy security dependency on Russian gas and coal supplies
 *   represents a classic snare constraint: a geopolitical-economic extraction
 *   mechanism maintained through infrastructure lock-in, capital constraints,
 *   and explicit supply weaponization. From 2000 through 2022, this
 *   constraint operated as a pure extraction mechanism (high χ, high
 *   suppression, minimal coordination benefit) through which Russia captured
 *   economic rents and maintained geopolitical leverage. The constraint
 *   exhibits the full DR typology depending on observer position: Ukrainian
 *   consumers and the state experience it as a snare (trapped, powerless);
 *   Russian energy exporters experience it as rope (beneficial coordination);
 *   European actors experience tangled rope (mixed benefits and extraction
 *   risk); organized EU energy transition actors frame it as a temporary
 *   scaffold with sunset through renewable deployment; the legacy Soviet
 *   pipeline infrastructure itself has become increasingly piton-like
 *   (maintained through inertia rather than necessity); and the analytical
 *   observer risks naturalizing what is a contingent infrastructure and power
 *   arrangement as an immutable geographic law. The 2022 invasion and
 *   subsequent energy pivot have accelerated European exit pathways (LNG
 *   imports, renewable acceleration) but Ukraine remains structurally
 *   trapped. The measuring interval (0-10 years) represents the period from
 *   roughly 2015 through 2025, capturing the escalation of the constraint
 *   (extractiveness rising from 0.42 to 0.68) as geopolitical weaponization
 *   became explicit and Ukraine's exit options contracted.
 *
 * KEY AGENTS:
 *   - Ukrainian Households and Industry: Primary victims (powerless/trapped) — bear fuel costs, risk supply disruption, endure voltage rationing. No exit capacity.
 *   - Ukrainian State and Energy Infrastructure: Secondary victim (powerless/constrained) — budget extracted through high energy costs; infrastructure locked into Soviet-era design; debt constraints prevent transition investment.
 *   - Russian Energy State: Primary beneficiary (institutional/arbitrage) — captures economic rents through monopoly pricing; maintains geopolitical leverage; can arbitrage between markets and pipelines.
 *   - European Energy Consumers and States: Secondary beneficiary/victim (powerful/mobile) — benefit from low energy costs maintained by Russian supply; bear geopolitical risk of supply disruption; have high-cost exit options (LNG, renewables).
 *   - EU Energy Transition Coalition: Organized actor (organized/constrained) — building alternative supply pathways with explicit decarbonization mandates; faces resource constraints but has political and financial leverage.
 *   - Soviet Pipeline Infrastructure: Institutional actor (institutional/arbitrage) — maintains operational status through legacy inertia; transit agreements with declining functional necessity.
 *   - Analytical Observer: External perspective (analytical/analytical) — risks naturalizing power asymmetry as geographic immutability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukrainian_energy_security_dependency, 0.68).
domain_priors:suppression_score(ukrainian_energy_security_dependency, 0.72).
domain_priors:theater_ratio(ukrainian_energy_security_dependency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukrainian_energy_security_dependency, extractiveness, 0.68).
narrative_ontology:constraint_metric(ukrainian_energy_security_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ukrainian_energy_security_dependency, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ukrainian_energy_security_dependency, snare).
narrative_ontology:human_readable(ukrainian_energy_security_dependency, "Ukrainian Energy Security Dependency on Russian Gas and Coal Supplies").
narrative_ontology:topic_domain(ukrainian_energy_security_dependency, "geopolitical/energy_security").

domain_priors:requires_active_enforcement(ukrainian_energy_security_dependency).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ukrainian_energy_security_dependency, russian_energy_exporters).
narrative_ontology:constraint_beneficiary(ukrainian_energy_security_dependency, russian_state_revenues).
narrative_ontology:constraint_victim(ukrainian_energy_security_dependency, ukrainian_consumers).
narrative_ontology:constraint_victim(ukrainian_energy_security_dependency, ukrainian_industrial_base).
narrative_ontology:constraint_victim(ukrainian_energy_security_dependency, ukrainian_state_budget).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UKRAINIAN CONSUMERS (SNARE) — Trapped in energy dependency by geography, infrastructure design, and lack of capital for alternative sources. No meaningful exit options. Bears full cost of supply disruption, price volatility, and political leverage. Maximum experienced extraction from the powerless position.
constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UKRAINIAN STATE INFRASTRUCTURE (SNARE) — Constrained by debt service requirements, infrastructure lock-in, and the generation-scale investment horizon for energy transition. Exit is technically possible but costs exceed available capital. Structural extraction through rent capture by pipeline operators and energy traders.
constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RUSSIAN ENERGY STATE (ROPE) — Experiences the constraint as a coordination mechanism for maintaining export markets and state revenue. Can arbitrage between supply routes (Europe, Asia), production levels, and price targeting. Net beneficiary with high agency. Extraction runs toward this agent.
constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN ENERGY ACTORS (TANGLED ROPE) — Powerful but energy-dependent on Russian supply routes that pass through Ukraine. Benefit from low energy costs maintained by Russian monopoly pricing; bear risk of supply disruption. Have exit options (LNG, renewables, conservation) but at significant short-term cost. Mixed coordination (reliable supply) and extraction (price leverage, geopolitical vulnerability).
constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU ENERGY TRANSITION COALITION (SCAFFOLD) — Organized actors (EU Green Deal, renewable energy mandates, LNG import agreements) are building alternative pathways with explicit sunset: renewable capacity expansion, liquified natural gas terminals, energy efficiency. High theater in political commitment; genuine structural changes reduce extraction over time. Sunset clause is approximately 10-15 years for energy independence targets.
constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: SOVIET INFRASTRUCTURE LEGACY (PITON) — The pipeline network is largely performative now, maintained through inertia and contractual obligation rather than necessity. Transit agreements, capacity utilization, and pipeline maintenance have become increasingly theatrical as energy flows shift and alternatives emerge. The infrastructure persists not because it efficiently solves a problem but because replacement has not yet occurred. Theater ratio high because maintenance and operation continue despite declining functional necessity.
constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOGRAPHIC DETERMINISM VIEW (MOUNTAIN) — From the civilizational perspective, energy dependency appears as an immutable feature of geography: Ukraine's location between energy reserves (east) and consuming markets (west) makes it structurally a transit zone. This perspective risks naturalizing what is actually a contingent infrastructure choice and geopolitical power asymmetry.
constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukrainian_energy_security_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukrainian_energy_security_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ukrainian_energy_security_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ukrainian_energy_security_dependency, TR),
    TR >= 0.70.

:- end_tests(ukrainian_energy_security_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High, reflecting the magnitude of economic rent extraction through monopoly pricing and supply weaponization. The constraint generates measurable transfers: Ukraine paid approximately 2-3x market prices for Russian gas during peak extraction periods; this represents a direct wealth transfer to Russian exporters and the Russian state. The extractiveness value reflects that this transfer is substantial and non-consensual (Ukraine lacks alternatives). Suppression (0.72): Very high. Multiple barriers prevent exit: (1) Infrastructure lock-in — 90%+ of Soviet-era pipeline infrastructure runs from east to west through Ukrainian territory, designed for one-way flow; (2) Capital constraints — energy transition investment ($100B+ over 10 years) exceeds available Ukrainian financing and requires external funding; (3) Time horizon mismatch — renewable buildout requires 5-10 years while demand is immediate; (4) Political constraints — EU integration pathway creates path-dependence on EU-aligned energy policy. Theater ratio (0.55): Moderate. While the constraint operates through real economic mechanisms, a significant portion of the extraction mechanism is theatrical: pipeline maintenance agreements with declining necessity, capacity utilization protocols that don't reflect actual flows, invoicing disputes that are partially performance rather than substantive. The theater increased during the measurement interval as political disputes came to dominate technical negotiations.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the Ukrainian trapped perspective (snare: 'we cannot escape') and the EU transition coalition perspective (scaffold: 'this is a temporary problem we are solving'). These agents are looking at the same constraint structure but with radically different time horizons, exit capacities, and resource access. The gap is not in analysis but in structural position. To close the gap requires: (1) capital transfer (EU funds Ukrainian energy transition), (2) supply diversification (LNG terminals, alternative pipeline routes), and (3) time (5-10 years for renewable buildout). Without these, the gap is unbridgeable — Ukraine remains in snare while Europe transitions toward rope. The Russian perspective (rope: 'this is a profitable supply network') is incompatible with the Ukrainian perspective (snare: 'this is extraction') but both are accurate descriptions of structural relationships. The incompatibility is the point — it reveals the asymmetry that defines the snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent power and exit options. Ukrainian powerless agents with trapped exit options derive d ≈ 0.92-0.95 (maximum target), producing χ magnification through the sigmoid f(d) ≈ 1.42. These agents bear maximum effective extraction. Russian institutional agents with arbitrage exit derive d ≈ 0.08-0.15 (strong beneficiary), producing negative or minimal f(d) ≈ -0.10 to 0.02. The energy flow runs toward Russian actors. European powerful agents with mobile exit derive d ≈ 0.55-0.65 (near-symmetric but slightly burdened), producing f(d) ≈ 0.75-1.00, moderate experienced extraction. The directionality asymmetry captures the real structural difference: Ukrainian consumers cannot leave, Russian exporters can redirect supply, European states can develop alternatives. The scaffold perspective reflects organized actors with constrained exit (high cost but possible), deriving d ≈ 0.45-0.55, producing moderate χ ≈ 0.15-0.25 because these actors see the constraint as temporary and solvable. No directionality overrides are necessary — the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how mandatrophy is resolved through perspectival decomposition. The apparent contradiction — 'Is this an immutable geographic constraint (mountain) or a fixable geopolitical extraction mechanism (snare)?' — is not a logical error but an observational asymmetry. The Ukrainian powerless trapped agent perceives immutability (cannot change their energy source within their time horizon). The analytical observer at civilizational scale risks projecting this local immutability onto geography itself. But the Russian beneficiary, the European transition coalition, and the organized energy actors all perceive mutability (change is possible through different mechanisms and time horizons). The mandatrophy dissolves when we recognize that immutability is local to position: Ukrainian consumers at biographical scale experience the constraint as unchangeable (mountain-like); the same constraint at generational scale with external support (EU funding, alternative suppliers) becomes mutable (tangled rope → scaffold → rope as alternatives mature). The false summit (analytical mountain) is diagnosed through the structural data: if the constraint were truly immutable, Russia would not need to threaten supply disruption (the threat would be meaningless); the threat itself reveals that the constraint is maintained through active extraction, not natural law. The mandatrophy resolves to: the constraint appears as mountain from the trapped powerless perspective at biographical scale; it is actually snare/tangled rope/scaffold depending on power and horizon; the analytical mountain is naturalization of what is a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_supply_feasibility,
    'Can Ukraine achieve energy independence through renewable capacity and alternative imports within 10 years, or are capital constraints binding?',
    'Capital requirement analysis; comparison of available financing vs required investment; timeline for renewable deployment and LNG terminal construction',
    'If feasible: scaffold sunset is real, extraction window closes. If constrained: escape from snare requires external actors (EU, US) to fund transition; Ukraine remains trapped in dependency during implementation period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_feasibility, empirical, 'Feasibility of Ukrainian energy independence timeline').

omega_variable(
    supply_weaponization_mechanism,
    'Is the extraction mechanism primarily economic rent extraction (monopsony pricing) or geopolitical leverage (threat of supply disruption), or are they structurally inseparable?',
    'Correlation analysis: price movements vs political events; counterfactual pricing under pure commercial terms; comparison to alternative supplier pricing in non-dependent markets',
    'If primarily economic: renegotiation and diversification reduce extraction. If primarily geopolitical: diversification alone insufficient without political realignment. If inseparable: snare classification is robust even under alternative supply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_weaponization_mechanism, empirical, 'Decomposition of economic vs geopolitical extraction mechanisms').

omega_variable(
    transit_rent_capture_stability,
    'Are Ukraine''s gas transit revenues from mediating European imports structurally stable, or are they subject to sudden elimination through pipeline rerouting?',
    'Analysis of pipeline route optimization; feasibility and timeline for alternative routes; historical precedent for rerouting decisions',
    'If unstable: Ukraine''s budget dependency on transit revenues is unstable, creating secondary snare. If stable: transit revenues provide legitimate coordination function. Piton classification of pipeline infrastructure depends on this assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transit_rent_capture_stability, empirical, 'Stability of Ukrainian transit revenue model').

omega_variable(
    eu_energy_transition_credibility,
    'Is the EU energy transition mandate a genuine structural commitment with binding capacity requirements, or is it aspirational policy that collapses under price pressure?',
    'Analysis of legislative mandate strength; budget allocation; comparison of stated targets vs actual implementation rates; correlation between energy prices and policy pressure',
    'If genuine: scaffold sunset is credible, creates exit pathway for European actors and spillover support for Ukrainian alternatives. If aspirational: European energy actors remain partially trapped in Russian dependency, reducing their ability to fund Ukrainian transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_energy_transition_credibility, empirical, 'EU energy transition mandate credibility and binding nature').

omega_variable(
    identity_locked_policy_dependence,
    'Has Ukrainian energy policy become identity-locked to European integration frameworks in ways that prevent pragmatic negotiation with alternative suppliers or energy independence pathways?',
    'Policy analysis of Ukrainian energy strategy alignment with EU directives; assessment of whether EU membership requirements prevent otherwise viable energy partnerships; historical precedent for policy shifts when EU frameworks change',
    'If identity-locked: Ukraine''s exit options are further constrained than trapped classification suggests — the state cannot imagine alternatives to EU-aligned energy policy. If structurally constrained: policy flexibility exists but carries political cost (EU relations). Affects whether state actors can exercise arbitrage options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_policy_dependence, conceptual, 'Institutional identity-lock in Ukrainian energy policy to EU frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukrainian_energy_security_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukr_energy_tr_t0, ukrainian_energy_security_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ukr_energy_tr_t5, ukrainian_energy_security_dependency, theater_ratio, 5, 0.48).
narrative_ontology:measurement(ukr_energy_tr_t10, ukrainian_energy_security_dependency, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(ukr_energy_be_t0, ukrainian_energy_security_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ukr_energy_be_t5, ukrainian_energy_security_dependency, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ukr_energy_be_t10, ukrainian_energy_security_dependency, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ukrainian_energy_security_dependency, resource_allocation).
narrative_ontology:affects_constraint(ukrainian_energy_security_dependency, european_energy_security).
narrative_ontology:affects_constraint(ukrainian_energy_security_dependency, russian_state_budget_dependency).
narrative_ontology:affects_constraint(ukrainian_energy_security_dependency, ukrainian_political_sovereignty).

% DUAL FORMULATION NOTE:
% Ukrainian energy dependency decomposes into three structurally distinct constraints: (1) supply security (pipelines, price volatility) — extractiveness ≈ 0.68 (this story); (2) infrastructure modernization (Soviet legacy capital replacement) — extractiveness ≈ 0.35, higher theater from performative maintenance; (3) geopolitical leverage (threat of cutoff as political weapon) — extractiveness ≈ 0.81, lower theater because threat is explicit. These three stories share the same interval and some agents but have distinct ε values and measurement trajectories. This story focuses on the supply constraint; the others examine infrastructure debt and political leverage separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ukrainian_energy_security_dependency, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
