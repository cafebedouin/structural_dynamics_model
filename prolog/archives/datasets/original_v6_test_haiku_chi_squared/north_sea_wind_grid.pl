% ============================================================================
% CONSTRAINT STORY: north_sea_wind_grid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_north_sea_wind_grid, []).

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
 *   constraint_id: north_sea_wind_grid
 *   human_readable: The North Sea 100GW Multinational Wind Power Grid Initiative
 *   domain: geopolitical/economic/energy_infrastructure
 *
 * SUMMARY:
 *   The North Sea 100GW Multinational Wind Power Grid Initiative presents a
 *   large-scale infrastructure coordination problem with substantial
 *   asymmetric extraction embedded within genuinely beneficial cooperation.
 *   Ten riparian states commit to shared offshore wind development, grid
 *   interconnection, and power trading infrastructure. The coordination
 *   benefits are real: economies of scale, load balancing across diverse
 *   demand profiles, and reduced per-megawatt deployment costs. However, the
 *   grid structure embeds asymmetries that extract from peripheral states and
 *   coastal communities while concentrating benefits in the northern
 *   industrial core. Northern industrial nations (Germany, Netherlands,
 *   Denmark) have higher electricity demand, greater grid control through
 *   transmission operator roles, and stronger bargaining positions.
 *   Peripheral states (Poland, Lithuania, Cyprus representatives) gain grid
 *   access but lose fossil fuel export revenues and face governance
 *   asymmetries. Coastal fishing communities lose traditional fishing grounds
 *   to exclusion zones without sufficient compensation mechanisms. The grid
 *   is theatrically presented as a climate solution without explicit coupling
 *   to fossil fuel baseload decommissioning, creating the appearance of
 *   transformation while maintaining fossil dependency. This is Tangled Rope
 *   at its structural core: genuine coordination function (renewable
 *   integration, efficiency gains) hybridized with asymmetric extraction
 *   (power concentration, rents to vendors, community displacement).
 *
 * KEY AGENTS:
 *   - Northern Industrial Core Nations (Germany, Netherlands, Denmark, others): Primary beneficiaries (institutional/arbitrage) — high demand, transmission control, cost advantages, strategic energy security
 *   - Peripheral Energy-Exporting States (Poland, Hungary, Baltic states, others): Secondary victim and beneficiary (moderate/constrained) — gain grid access but lose fossil fuel revenues and have limited governance voice
 *   - Coastal Fishing Communities: Primary victim (powerless/trapped) — face exclusion from traditional grounds with inadequate compensation mechanisms
 *   - Offshore Wind Technology Vendors (Siemens, GE Renewable, MHI Vestas, others): Secondary beneficiary (institutional/arbitrage) — long-term supply contracts and technology lock-in rents
 *   - Transmission Grid Operators (TSOs): Institutional actor (institutional/constrained) — bear integration costs and operational risk while profits accrue to government shareholders and technology vendors
 *   - EU Climate Governance (European Commission, Parliament): Institutional actor (institutional/arbitrage) — maintains performative climate commitment; actual decarbonization depends on unspoken parallel policies
 *   - Decarbonization Coalition (NGOs, climate-forward governments): Organized actor (organized/mobile) — sees grid as temporary coordination structure with sunset as renewable markets mature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_sea_wind_grid, 0.52).
domain_priors:suppression_score(north_sea_wind_grid, 0.58).
domain_priors:theater_ratio(north_sea_wind_grid, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_sea_wind_grid, extractiveness, 0.52).
narrative_ontology:constraint_metric(north_sea_wind_grid, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(north_sea_wind_grid, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(north_sea_wind_grid, tangled_rope).
narrative_ontology:human_readable(north_sea_wind_grid, "The North Sea 100GW Multinational Wind Power Grid Initiative").
narrative_ontology:topic_domain(north_sea_wind_grid, "geopolitical/economic/energy_infrastructure").

domain_priors:requires_active_enforcement(north_sea_wind_grid).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(north_sea_wind_grid, northern_european_industrial_bloc).
narrative_ontology:constraint_beneficiary(north_sea_wind_grid, offshore_wind_technology_vendors).
narrative_ontology:constraint_beneficiary(north_sea_wind_grid, transmission_grid_operators).
narrative_ontology:constraint_victim(north_sea_wind_grid, coal_and_gas_energy_producers).
narrative_ontology:constraint_victim(north_sea_wind_grid, smaller_peripheral_states).
narrative_ontology:constraint_victim(north_sea_wind_grid, coastal_fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL FISHING COMMUNITIES (SNARE) — Trapped in regional waters; cannot relocate livelihoods. Extraction: loss of traditional fishing grounds to wind farm exclusion zones without compensation or alternative income. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(north_sea_wind_grid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PERIPHERAL ENERGY-EXPORTING NATIONS (TANGLED ROPE) — Constrained by geopolitical dependency and energy export revenues at risk. Coordination benefit: access to shared grid infrastructure reduces isolation. Extraction: forced transition away from fossil fuel revenues; lower bargaining power in grid governance despite equal formal membership. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(north_sea_wind_grid, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NORTHERN INDUSTRIAL CORE NATIONS (ROPE) — High industrial electricity demand; arbitrage exit through grid switching and bilateral power purchases. Experiences constraint as genuine coordination: shared infrastructure reduces individual investment costs and multiplies market reach. d≈0.12, f(d)≈0.08, σ=1.1 → χ≈0.04.
constraint_indexing:constraint_classification(north_sea_wind_grid, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: TRANSMISSION GRID OPERATORS (TANGLED ROPE) — Institutional actors constrained by regulatory mandate to integrate the shared grid; cannot fully exit due to government directives. Coordination benefit: operational efficiency and load balancing across 10 nations. Extraction: bear operational risk and integration costs while profits are distributed to vendor shareholders; asymmetric liability for cross-border faults. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.40.
constraint_indexing:constraint_classification(north_sea_wind_grid, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: OFFSHORE WIND TECHNOLOGY VENDORS (ROPE) — High-mobility institutional actors with arbitrage exit (sell globally). Coordination benefit: EU-wide infrastructure contracts guarantee demand stream. Extraction is minimal because vendors can exit to other markets and skim technology transfer rents from the grid's dependency on their systems. d≈0.18, f(d)≈0.15, σ=1.1 → χ≈0.08.
constraint_indexing:constraint_classification(north_sea_wind_grid, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EU CLIMATE GOVERNANCE APPARATUS (PITON) — Maintains performative commitment to decarbonization. The grid initiative is theatrically presented as transformational, but actual emissions reduction depends on decommissioning fossil fuel baseload capacity (which is not mandated by the grid treaty). Theater ratio 0.68: the infrastructure is real, but the causal link between 100GW of intermittent renewable capacity and carbon neutrality targets is presented without parallel management of fossil fuel exit. d≈0.05, f(d)≈-0.10, σ=1.1 → χ≈-0.07.
constraint_indexing:constraint_classification(north_sea_wind_grid, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: DECARBONIZATION COALITION (SCAFFOLD) — Organized NGOs and climate-forward governments see the grid as a temporary coordination structure with a sunset: as renewable capacity matures and becomes cheaper, the grid becomes self-sustaining through market incentives alone, and formal governance can devolve. Has sunset logic: 15-20 year mandate, after which operators transition to private market coordination. d≈0.35, f(d)≈0.35, σ=1.1 → χ≈0.13.
constraint_indexing:constraint_classification(north_sea_wind_grid, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational timescale, the grid is a mechanism for locking peripheral states into energy dependency on the northern industrial core while creating the appearance of shared governance. Coordination genuine: enables economies of scale and reduces renewable deployment costs regionally. Extraction asymmetric: core nations gain sovereign grid control and lock in long-term demand dependency. d≈0.62, f(d)≈0.90, σ=1.1 → χ≈0.52.
constraint_indexing:constraint_classification(north_sea_wind_grid, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_sea_wind_grid_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(north_sea_wind_grid, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_sea_wind_grid, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(north_sea_wind_grid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(north_sea_wind_grid, TR),
    TR >= 0.70.

:- end_tests(north_sea_wind_grid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts asymmetrically from peripheral states and coastal communities, but the extraction is not total because: (1) genuine coordination benefits flow to all members (renewable economics are superior); (2) peripheral states gain grid access and long-term energy security; (3) compensation mechanisms exist, even if inadequate. The value reflects that the grid is neither pure cooperation nor pure predation. Suppression (0.58): Moderate-high. Multiple barriers prevent exit or renegotiation: (1) geopolitical lock-in (peripheral states cannot easily redirect energy policy without grid partnership); (2) sunk costs in grid infrastructure make exit costly; (3) EU regulatory framework makes unilateral withdrawal diplomatically expensive; (4) fishing communities have no alternative income sources in coastal regions. Theater ratio (0.68): High and rising. The grid is theatrically presented as climate transformation, but actual decarbonization depends on parallel decommissioning of fossil baseload (not mandated by grid treaty). The performance aspect increases over time as actual emissions reduction fails to match promised climate impact, creating pressure to maintain the performance narrative of grid success.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival divide between beneficiaries and victims. The northern industrial core (institutional/arbitrage) experiences it as Rope: genuine coordination for mutual benefit with profitable exit options. Peripheral states (moderate/constrained) experience it as Tangled Rope: forced participation in infrastructure they depend on but cannot fully control. Coastal fishing communities (powerless/trapped) experience it as Snare: pure extraction with no exit. The transmission grid operators (institutional/constrained) experience it as Tangled Rope: obligatory integration with asymmetric liability. Technology vendors (institutional/arbitrage) experience it as Rope: guaranteed long-term contracts with vendor lock-in. The climate governance apparatus (institutional/arbitrage) experiences it as Piton: maintains a performative commitment to decarbonization while actual emissions reduction is contingent on separate policies. The decarbonization coalition (organized/mobile) experiences it as Scaffold: sees the grid as a temporary coordination structure with a sunset clause as renewable economics improve. The analytical observer (analytical/analytical) from a civilizational timescale sees Tangled Rope with geopolitical lock-in: the grid enables energy efficiency but establishes long-term dependency relationships that concentrate power in the industrial core.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal fishing communities: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; cannot exit regional waters, no alternative income sources, zero voice in governance. Peripheral energy states: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction; geopolitically constrained, cannot easily pivot away from grid, but have some institutional leverage and long-term benefits. Northern industrial core: Beneficiary + arbitrage → d≈0.12, f(d)≈0.08. Net beneficiary; can exit to bilateral power trading, have multiple options. Transmission operators: Victim + constrained → d≈0.55, f(d)≈0.75. Moderate-high extraction; forced to bear integration costs but mandated by governments, cannot easily exit. Technology vendors: Beneficiary + arbitrage → d≈0.18, f(d)≈0.15. Net beneficiary with exit option (global markets); lock-in is temporary if suppliers remain competitive. EU climate apparatus: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.10. Institutional beneficiary; maintains narrative control and can exit by shifting climate priorities. Decarbonization coalition: Weak victim + mobile → d≈0.35, f(d)≈0.35. Low-moderate extraction because coalition has exit option (can switch support to alternative decarbonization pathways) and can see the sunset.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through structural decomposition. The question is not 'is this Rope or Snare?' but 'for whom?' The grid genuinely solves a coordination problem (renewable integration, load balancing, cost reduction) — Rope function is real. Simultaneously, it embeds asymmetric extraction favoring the industrial core and disadvantaging peripheral states and fishing communities — Snare function is real. Both are structural features, not measurement errors. The Tangled Rope classification captures this duality: the constraint exists because coordination is necessary, but it persists in its current form because the asymmetric extraction benefits those with power to maintain it. The rising theater ratio (0.50 → 0.68) tracks Goodhart drift: as actual climate impact lags promised targets, political pressure to maintain the grid's narrative success increases. The grid must perform as a climate solution even if decarbonization depends on decoupled policies. This is classic institutional theater: the perceived function (climate transformation) diverges from the actual function (renewable infrastructure + geopolitical lock-in). Mandatrophy is resolved by recognizing that both Rope and Snare functions are simultaneously true, and that the constraint's persistence depends on maintaining both the coordination benefit AND the extraction asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grid_governance_capture,
    'Will grid governance remain symmetric (all 10 states equal voice) or concentrate control in core industrial nations through regulatory capture?',
    'Analysis of voting structures, veto rights, and actual decision-making patterns in the joint governance board; monitoring of technical standard-setting authority; tracking of whose grid balancing preferences drive operational priorities',
    'If symmetric: Tangled Rope with moderate χ across perspectives. If captured: escalates to Snare from peripheral nations'' views; core nations move toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_governance_capture, empirical, 'Whether grid governance remains symmetric or concentrates power').

omega_variable(
    fossil_fuel_phase_out_coupling,
    'Is the 100GW wind grid legally coupled to fossil fuel baseload decommissioning, or are they independent infrastructure paths?',
    'Treaty text analysis; monitoring whether coal/gas plants are actually closed proportional to wind capacity added; assessment of whether ''climate neutrality'' target is achievable without explicit baseload exit mandate',
    'If coupled: grid enables genuine decarbonization (Scaffold/Rope perspectives validated). If decoupled: grid becomes theater masking continued fossil dependency (Piton perspective confirmed; all other perspectives see higher effective extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fossil_fuel_phase_out_coupling, empirical, 'Whether wind grid is coupled to fossil fuel phase-out').

omega_variable(
    peripheral_state_renegotiation_capability,
    'Can peripheral states renegotiate grid terms (cost-sharing, governance votes, grid access priority) if initial conditions become too extractive, or are terms locked for the project duration?',
    'Treaty exit clauses and renegotiation provisions; historical precedent from other multinational infrastructure (Nord Stream, Three Gorges, ITAIPU); monitoring for first-mover exit attempts and outcomes',
    'If renegotiable: Tangled Rope with constrained exit moderating extraction. If locked: escalates to Snare as peripheral states are trapped. Theater ratio rises if renegotiation is formally permitted but politically impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peripheral_state_renegotiation_capability, empirical, 'Whether peripheral states can renegotiate grid terms').

omega_variable(
    fishing_community_compensation_adequacy,
    'Is economic compensation (or job displacement support) for fishing communities calculated independently or derived from energy sector benefit estimates?',
    'Comparison of actual fishing losses (catch value, quota displacement, vessel resale value) vs compensation awarded; longitudinal tracking of fishing community employment and income 5-10 years post-exclusion; analysis of whether compensation formulas were negotiated with fishing representatives',
    'If independently valued: reduces Snare severity (fishing communities move toward Tangled Rope with mobile exit). If derivative: Snare classification confirmed (compensation systematically insufficient).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fishing_community_compensation_adequacy, empirical, 'Whether fishing community compensation is independently negotiated').

omega_variable(
    technology_vendor_lock_in_duration,
    'Can grid operators source renewable turbines and grid equipment from global suppliers, or do initial vendor contracts create lock-in preventing later supplier switching?',
    'Analysis of turbine procurement contracts; technical compatibility of different vendors'' equipment; monitoring of cost escalation when renewal/maintenance contracts occur; tracking of efforts to create vendor-agnostic interface standards',
    'If competitive sourcing maintained: vendor Rope classification holds (arbitrage available). If lock-in occurs: vendors escalate toward Snare position; other perspectives see higher effective extraction as technology rents concentrate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_vendor_lock_in_duration, empirical, 'Whether grid design maintains competitive vendor sourcing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(north_sea_wind_grid, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsw_tr_t0, north_sea_wind_grid, theater_ratio, 0, 0.5).
narrative_ontology:measurement(nsw_tr_t5, north_sea_wind_grid, theater_ratio, 5, 0.62).
narrative_ontology:measurement(nsw_tr_t10, north_sea_wind_grid, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(nsw_be_t0, north_sea_wind_grid, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nsw_be_t5, north_sea_wind_grid, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(nsw_be_t10, north_sea_wind_grid, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(north_sea_wind_grid, resource_allocation).
narrative_ontology:affects_constraint(north_sea_wind_grid, eu_energy_security_dependency).
narrative_ontology:affects_constraint(north_sea_wind_grid, north_sea_marine_ecosystem_protection).
narrative_ontology:affects_constraint(north_sea_wind_grid, european_industrial_competitiveness).

% DUAL FORMULATION NOTE:
% The North Sea grid can be decomposed into distinct structural claims: (1) renewable integration coordination (ε≈0.15, Rope); (2) geopolitical lock-in for peripheral states (ε≈0.68, Snare); (3) coastal displacement (ε≈0.80, Snare). This JSON presents the hybrid (ε=0.52, Tangled Rope) as the constraint's primary identity because the grid's functional form requires all three mechanisms. Downstream constraints include EU energy dependency (which the grid creates), marine ecosystem impacts (which the grid generates), and European industrial competitiveness (which the grid enables).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(north_sea_wind_grid, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
