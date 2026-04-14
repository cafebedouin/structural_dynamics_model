% ============================================================================
% CONSTRAINT STORY: critical_infrastructure_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_critical_infrastructure_dependency, []).

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
 *   constraint_id: critical_infrastructure_dependency
 *   human_readable: Critical Infrastructure Dependency Lock-In
 *   domain: political_economy/infrastructure
 *
 * SUMMARY:
 *   Critical infrastructure dependency creates a structural lock-in where
 *   societies become trapped within systems they cannot easily exit. The
 *   constraint exhibits the full spectrum of DR types from different vantage
 *   points: dependent societies experience pure extraction (snare); operators
 *   perceive coordination solutions (rope); vendors combine coordination with
 *   lock-in (tangled rope); alternative suppliers face insurmountable
 *   barriers (snare); regulators maintain performative oversight while
 *   captured (piton); essential workers combine genuine coordination function
 *   with identity-based wage suppression (tangled rope with identity_locked
 *   exit); decentralization advocates see a temporal solution with sunset
 *   clauses (scaffold); and civilizational observers risk naturalizing
 *   contingent institutional arrangements as laws of physics (false summit
 *   mountain). The constraint's theater ratio (0.55) reflects that regulatory
 *   oversight persists as ritual despite regulatory capture — safety
 *   inspections and rate reviews occur but do not materially constrain
 *   operator behavior. Extractiveness has increased from 0.42 to 0.68 over
 *   the twenty-year interval, indicating that operators have successfully
 *   layered additional extraction mechanisms (privatization of gains,
 *   socialization of maintenance costs, deferral of system upgrades) onto the
 *   original coordination function. The scaffold perspective's feasibility
 *   timeline (distributed generation and storage maturation) represents a
 *   genuine structural exit path, but adoption barriers created by incumbent
 *   control remain high.
 *
 * KEY AGENTS:
 *   - Dependent Society: Primary victim (powerless/trapped/national) — cannot exit power grid, water, or telecommunications systems without existential harm. Bears full extraction cost through monopoly pricing and deferred maintenance.
 *   - Infrastructure Operator: Primary beneficiary (institutional/arbitrage/national) — captures coordination rents and monopoly extraction. Experiences constraint as enabling their core function (grid stability). Full exit optionality (can sell to competitor or public authority).
 *   - Equipment Vendor: Secondary beneficiary (powerful/mobile/continental) — captures rents from proprietary lock-in and switching costs. Provides genuine technical coordination (SCADA, control systems) alongside extractive licensing.
 *   - Alternative Supplier: Secondary victim (organized/constrained/continental) — renewable energy producers, municipal utilities, competing operators face regulatory and technical barriers to entry. Experiencing the constraint as monopoly gatekeeping.
 *   - Regulatory Authority: Institutional actor (institutional/arbitrage/national) — created to prevent monopoly abuse but captured by operator through revolving doors, technical dependence, and political influence. Maintains performative regulation with low enforcement.
 *   - Essential Worker Population: Mixed actor (moderate/identity_locked/national) — power technicians, water system operators whose professional identity is constituted through infrastructure maintenance. Structurally mobile but identity-locked; experience mixed coordination function (genuine technical skill) and wage extraction (suppressed by identity lock preventing exit).
 *   - Decentralization Movement: Organized challengers (organized/constrained/global) — grid modernization advocates, solar/battery advocates, municipal utility movements building alternative pathways. See constraint as temporary with 10-30 year sunset.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical/universal) — risks naturalizing the dependency lock as inherent to electrification physics, obscuring that centralized topology and proprietary control are contingent institutional choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(critical_infrastructure_dependency, 0.68).
domain_priors:suppression_score(critical_infrastructure_dependency, 0.75).
domain_priors:theater_ratio(critical_infrastructure_dependency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(critical_infrastructure_dependency, extractiveness, 0.68).
narrative_ontology:constraint_metric(critical_infrastructure_dependency, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(critical_infrastructure_dependency, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(critical_infrastructure_dependency, snare).
narrative_ontology:human_readable(critical_infrastructure_dependency, "Critical Infrastructure Dependency Lock-In").
narrative_ontology:topic_domain(critical_infrastructure_dependency, "political_economy/infrastructure").

domain_priors:requires_active_enforcement(critical_infrastructure_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(critical_infrastructure_dependency, infrastructure_operator).
narrative_ontology:constraint_beneficiary(critical_infrastructure_dependency, equipment_vendor).
narrative_ontology:constraint_victim(critical_infrastructure_dependency, dependent_society).
narrative_ontology:constraint_victim(critical_infrastructure_dependency, alternative_suppliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SOCIETY (SNARE) — Society cannot exit electrical grid, water systems, or telecommunications without existential risk. Trapped by sunk infrastructure costs and lack of viable alternatives. Suppression is maximal: exit creates immediate harm (no power = death in winter; no water = disease). Extraction runs continuous — operators extract rent through monopoly pricing, deferred maintenance, and service degradation. No coordination function perceived; constraint exists purely to capture value.
constraint_indexing:constraint_classification(critical_infrastructure_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFRASTRUCTURE OPERATOR (ROPE) — Perceives the constraint as coordination mechanism: integrating power generation, distribution, and consumption into unified system. Genuine coordination problem exists (frequency matching, load balancing, resilience against cascading failure). Beneficiary with full arbitrage optionality (can exit by transferring system to competitor or public authority). Experiences the constraint as enabling rather than extractive — their profit derives from solving coordination, not from monopoly suppression.
constraint_indexing:constraint_classification(critical_infrastructure_dependency, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: EQUIPMENT VENDOR (TANGLED ROPE) — Powerful actor (energy multinational, semiconductor fab) with genuine coordination function (SCADA protocols, proprietary control systems enable grid reliability). Also benefits from lock-in: switching costs make operator dependent on vendor's upgrades, support, and licensing. Mixed extraction and coordination — vendor solves real technical problems AND captures rents from switching costs. Mobile exit options (can sell to different operator or withdraw) but high-value extant relationships limit actual mobility.
constraint_indexing:constraint_classification(critical_infrastructure_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: ALTERNATIVE SUPPLIER (SNARE) — Renewable energy producer, competing infrastructure operator, or technology provider attempting to enter market. Faces massive suppression: incumbent control of regulatory approvals, interconnection standards designed to prevent entry, capital barriers, and grid access control by monopolist. Organized (can mobilize capital and advocacy) but constrained (exit via bankruptcy or acquired by incumbent is real threat). Experiences the constraint as pure extraction mechanism preventing market competition.
constraint_indexing:constraint_classification(critical_infrastructure_dependency, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (PITON) — Created to oversee operator coordination and prevent monopoly abuse. Maintains performative regulation: tariff reviews, safety inspections, competitive procurement requirements. Functionally degraded: regulatory capture means operator effectively writes its own rules. Theater ratio high because regulation persists as ritual despite low enforcement. Authority has arbitrage (can impose penalties) but captures itself through revolving doors, technical dependencies, and political capture. Sees own process as partially theatrical but continues for institutional inertia.
constraint_indexing:constraint_classification(critical_infrastructure_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ESSENTIAL WORKER POPULATION (TANGLED ROPE) — Power grid operators, water system technicians, network engineers whose professional identity is constituted through maintaining critical infrastructure. Structurally mobile (could retrain, migrate sectors) but identity-locked: their competence, status, and self-concept depend on being 'essential.' Experiences mixed coordination (genuine technical skill required to maintain system reliability) and extraction (wages suppressed because identity lock prevents exit despite labor shortage). Cannot imagine leaving the role even when offered better compensation in other sectors.
constraint_indexing:constraint_classification(critical_infrastructure_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: DECENTRALIZATION MOVEMENT (SCAFFOLD) — Grid modernization advocates, renewable energy coalition, municipal utility movements see the centralized infrastructure lock-in as temporary. Distributed generation (rooftop solar, local batteries, microgrids) creates alternative verification pathways and exit routes. Suppression present (incumbent lobby, interconnection barriers) but organized advocates see a sunset: as storage costs decline and smart grid protocols mature, the centralized dependency loses value. Constraint appears as solvable coordination failure with 10-30 year timeline.
constraint_indexing:constraint_classification(critical_infrastructure_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, energy flows require coordination: the laws of thermodynamics and electrical physics mandate system-level optimization. Grid stability, frequency matching, and demand-supply balancing are inherent constraints on energy distribution. This perspective naturalizes the dependency as an immutable property of large-scale electrification. However, the structural data reveals this as a false summit: the *dependency lock* is contingent on centralized topology, proprietary control, and regulatory capture, not on physics. Physics mandates coordination; it does not mandate this specific operator or this specific lock-in mechanism.
constraint_indexing:constraint_classification(critical_infrastructure_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_infrastructure_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(critical_infrastructure_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_infrastructure_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(critical_infrastructure_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(critical_infrastructure_dependency, TR),
    TR >= 0.70.

:- end_tests(critical_infrastructure_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Infrastructure operators have successfully established monopoly control, suppressed competition, deferred maintenance, captured regulation, and increased tariffs over the measurement interval. The extractiveness value reflects accumulated extraction mechanisms layered onto legitimate coordination function. Initial value (0.42) represented primarily coordination costs; final value (0.68) includes coordination costs plus accumulated monopoly rents. Suppression (0.75): Very high. Dependent populations face existential barriers to exit (no power = death, no water = disease). Alternative suppliers face regulatory barriers, capital barriers, and incumbent control of interconnection standards. Essential workers face labor market barriers and identity-lock barriers. The suppression is multi-layered and nearly total for trapped agents. Theater ratio (0.55): Moderate. Regulatory oversight persists as ritual (rate reviews, safety inspections) but does not materially constrain operator extraction. The theater is lower than in piton constraints because genuine technical coordination challenges do require some authentic operational complexity, but regulation's impotence creates the performative dimension.
 *
 * PERSPECTIVAL GAP:
 *   The operator sees rope (coordination) while the dependent society sees snare (pure extraction). This gap reveals that the constraint's classification depends entirely on structural position. From the operator's vantage point, managing grid frequency, balancing demand and supply, and preventing cascading failures are genuine coordination problems — the constraint solves real technical challenges. From the dependent society's vantage point, these same coordination functions create monopoly opportunity — operators capture rents from the coordination requirement itself. The vendor sees tangled rope (genuine technical contribution plus switching cost extraction) while the alternative supplier sees snare (pure gatekeeping). The regulator sees piton (its own performative oversight) while the decentralization movement sees scaffold (temporary constraint with sunset). The civilizational observer risks seeing mountain (physics-inherent dependency) but structural data reveals false summit — physics mandates coordination, not this operator, not this lock-in, not this level of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position. The dependent society (powerless/trapped) experiences maximum extraction — they bear the full constraint cost with no exit options or agency. The operator (institutional/arbitrage) experiences net beneficiary flow — their ability to arbitrage between entering and exiting the system means their effective extraction is negative (they benefit). The vendor (powerful/mobile) experiences intermediate extraction flow reversed toward them through switching costs and licensing; their mobility option prevents total capture but their embedded position in the system keeps d elevated (~0.45). The alternative supplier (organized/constrained) experiences high extraction; organization prevents the powerless classification, but constraint options prevent mobile classification. The regulatory authority experiences beneficiary flow when captured (they receive appropriated budget and authority legitimacy) but victim flow when attempting independence (they lose appropriations and political cover). The essential worker experiences extracted flow modulated by identity lock — structurally they could exit (mobile-level material barriers) but identity-lock prevents them from exercising mobility, so they experience constrained-level effective extraction. The decentralization movement (organized/constrained) experiences extraction from incumbent barriers but sees exit pathway, moderating experienced d.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY STRUCTURE: Critical infrastructure dependency decomposes into multiple structurally distinct constraints with different ε values: (1) Energy physics coordination (ε ≈ 0.08, Mountain) — thermodynamics and electrical physics mandate system-level optimization. (2) Grid reliability coordination (ε ≈ 0.25, Rope) — frequency matching, demand balancing, cascading failure prevention require integration. (3) Operator monopoly lock-in (ε ≈ 0.68, Snare/Tangled Rope) — incumbent control creates extractive barriers to alternative supply. These should be separate stories, but are analytically collapsed here to preserve the exemplar's pedagogical value. The mandatrophy is resolved by recognizing that the dependent society's snare experience and the operator's rope experience are both correct — they are measuring different constraints. The physics-level constraint is low-extraction rope. The institutional monopoly constraint is high-extraction snare. The vault-into-mandatory mountain classification happens when observers naturalize (3) onto (1) — claiming that operator monopoly is inherent to physics, when only coordination is inherent to physics. The false summit detection occurs when the claimed type (snare) has been observed to change when institutional arrangements change (e.g., public ownership, municipal utilities, competitive generation), proving the snare is contingent, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralization_feasibility_threshold,
    'At what battery storage cost and smart grid maturity level does distributed generation become a functionally equivalent alternative to centralized grid dependency?',
    'Technical benchmarking of grid stability, frequency management, and resilience in hybrid centralized-decentralized systems; cost-benefit analysis of microgrids vs centralized supply at various technology cost points',
    'If feasible within 10 years: scaffold perspective confirmed and sunset is real. If infeasible within 30 years: sunset is aspirational and snare/tangled rope dynamics persist through planning horizon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_feasibility_threshold, empirical, 'Timeline and feasibility threshold for grid decentralization').

omega_variable(
    regulatory_capture_reversibility,
    'Can regulatory authority recover independence from operator capture, or is the capture mechanism structurally irreversible without external intervention?',
    'Comparative institutional analysis of regulatory recoveries in other monopoly sectors (telecommunications, airlines post-deregulation); examination of structural incentives that bind regulator to incumbent',
    'If reversible: organized supplier perspective becomes mobile and snare could downgrade to tangled rope. If irreversible: regulatory capture is permanent unless regime change occurs, stabilizing snare/piton equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_reversibility, conceptual, 'Reversibility of regulatory capture mechanism').

omega_variable(
    identity_lock_labor_supply_elasticity,
    'How much of the essential worker wage suppression derives from identity lock (workers cannot imagine leaving) versus material barriers (no alternative employment)?',
    'Survey and longitudinal tracking of worker career transitions; wage comparison with similar-skill non-essential sectors; analysis of worker retention after crisis periods (COVID staffing, natural disasters) when identity lock weakens',
    'If primarily identity-locked: worker population is classified as experiencing moderate extraction from cognitive capture, not structural poverty. If primarily material barriers: classification should upgrade to trapped. Determines policy intervention (cultural narrative change vs wage floors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_labor_supply_elasticity, empirical, 'Proportion of wage suppression from identity lock vs material barriers').

omega_variable(
    systemic_fragility_extraction_coupling,
    'Does deferring maintenance and operating the grid at lower safety margins (extraction strategy) increase systemic fragility, creating cascading failure risk that exceeds operator''s capture capacity?',
    'Risk analysis of extreme weather events, cyber attacks, and component failures under maintenance deferral; modeling of cascading failure probability as function of operator extraction rate; tracking of near-miss incidents and infrastructure age',
    'If coupled: snare is unstable — oversupply of extraction destroys coordination function, triggers crisis, and forces regulatory intervention (breaking piton). If decoupled: snare is stable and can persist indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_fragility_extraction_coupling, empirical, 'Whether extraction-driven maintenance deferral increases systemic fragility').

omega_variable(
    vendor_lock_technology_alternative_emergence,
    'Are open-source and interoperable grid control technologies (Linux-based SCADA, open protocols) creating genuine alternatives to proprietary vendor lock, or do they remain marginal?',
    'Adoption rate tracking of open alternatives; operator perception surveys; cost-benefit comparison of open vs proprietary systems; security and reliability track records of deployed open systems',
    'If emerging as genuine alternative: equipment vendor extraction mechanism weakens, tangled rope could downgrade to rope, and operator gains true arbitrage options. If marginal: vendor lock remains and tangled rope stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_lock_technology_alternative_emergence, empirical, 'Feasibility and adoption of open-source grid control alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(critical_infrastructure_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cid_tr_t0, critical_infrastructure_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cid_tr_t10, critical_infrastructure_dependency, theater_ratio, 10, 0.48).
narrative_ontology:measurement(cid_tr_t20, critical_infrastructure_dependency, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(cid_be_t0, critical_infrastructure_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cid_be_t10, critical_infrastructure_dependency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cid_be_t20, critical_infrastructure_dependency, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(critical_infrastructure_dependency, global_infrastructure).
narrative_ontology:boltzmann_floor_override(critical_infrastructure_dependency, 0.25).
narrative_ontology:affects_constraint(critical_infrastructure_dependency, regulatory_capture_cycle).
narrative_ontology:affects_constraint(critical_infrastructure_dependency, essential_worker_wage_suppression).
narrative_ontology:affects_constraint(critical_infrastructure_dependency, renewable_energy_integration_barriers).

% DUAL FORMULATION NOTE:
% Critical infrastructure dependency is downstream of fundamental physics (energy coordination constraint, ε ≈ 0.08, Mountain) and technical coordination requirements (grid reliability, ε ≈ 0.25, Rope), but represents a distinct institutional constraint (monopoly lock-in, ε ≈ 0.68, Snare). The three constraints share the same physical system but have different ε values, different beneficiaries/victims, and different classification trajectories. Network links show how institutional constraints propagate effects to related extraction mechanisms (regulatory capture, labor suppression, alternative supply barriers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(critical_infrastructure_dependency, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
