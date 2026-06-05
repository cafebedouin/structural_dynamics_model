% ============================================================================
% CONSTRAINT STORY: taiwan_strait_hegemony_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_strait_hegemony_shift, []).

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
 *   constraint_id: taiwan_strait_hegemony_shift
 *   human_readable: The Taiwan Strait Energy & Logistics Chokepoint
 *   domain: political_economy/geopolitics
 *
 * SUMMARY:
 *   The Taiwan Strait represents one of the world's most critical maritime
 *   chokepoints: 30% of global maritime trade (~$5 trillion annually) and
 *   ~80% of global LNG shipments transit its waters. A hegemonic power
 *   controlling Taiwan would gain the capacity to extract significant
 *   economic rent, enforce political concessions, and weaponize shipping
 *   access. This constraint models the structural lock created by geographic
 *   dependency. It exhibits snare properties (high extraction, high
 *   suppression, constrained exit for dependent states) from multiple
 *   perspectives, but also contains shadow mechanisms of resistance:
 *   alternative routes, energy transition, coalition dynamics, and
 *   great-power deterrence. The theater ratio (0.38) is low because the
 *   extraction mechanism is primarily military/physical (blockade
 *   credibility) rather than institutional theater; the coercive capacity is
 *   real and observable. The constraint's extractiveness has grown from 0.35
 *   (pre-hegemonic equilibrium) to 0.68 (post-hegemonic assumption),
 *   reflecting both increased capability and increased willingness to use it
 *   once established. The suppression level (0.72) reflects the
 *   irreversibility of hegemonic position once achieved and the military
 *   barriers to coalitional reversal.
 *
 * KEY AGENTS:
 *   - Hegemon Controlling Strait (institutional/arbitrage): Primary beneficiary — gains $500B+ annual extraction capacity, geopolitical leverage, veto power over regional development
 *   - Dependent Shipping States: Japan, South Korea, Taiwan, Philippines (powerless/trapped) — primary victims; face toll extraction, strategic vulnerability, forced political alignment
 *   - Global Energy Importers: EU, India, East Asia (powerless/trapped) — secondary victims; dependent on 80% of LNG traffic; face supply disruption risk and cost inflation
 *   - Non-Hegemon Great Power (US, if China hegemon): powerful/constrained — faces strategic chokehold; military options are escalatory; diplomacy is forfeited
 *   - Regional Powers (ASEAN, India): organized/constrained — benefit from freedom of navigation (coordination function) but pay costs of hegemonic control; constrained exit
 *   - Supply Chain Decoupling Coalition: powerful/mobile — states and corporations investing in alternatives; sees chokepoint as temporary; sunset horizon 15-25 years
 *   - Analytical Observer: analytical/analytical — risks naturalizing contingent hegemonic configuration as geographic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_strait_hegemony_shift, 0.68).
domain_priors:suppression_score(taiwan_strait_hegemony_shift, 0.72).
domain_priors:theater_ratio(taiwan_strait_hegemony_shift, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, extractiveness, 0.68).
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_strait_hegemony_shift, snare).
narrative_ontology:human_readable(taiwan_strait_hegemony_shift, "The Taiwan Strait Energy & Logistics Chokepoint").
narrative_ontology:topic_domain(taiwan_strait_hegemony_shift, "political_economy/geopolitics").

domain_priors:requires_active_enforcement(taiwan_strait_hegemony_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_strait_hegemony_shift, hegemon_controlling_strait).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, dependent_shipping_states).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, global_energy_importers).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, non_hegemon_regional_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SHIPPING STATES (SNARE) — Japan, South Korea, and global LNG importers have no viable alternative to Taiwan Strait transit. Rerouting via Lombok Strait adds 3-5 weeks and significant cost. Geographic constraint is absolute. Suppression ≥0.72: military interdiction, toll extraction, and route denial are all credible coercive mechanisms. d≈0.92, f(d)≈1.40, σ=1.2 (global scope) → χ≈0.71.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL POWERS (TANGLED ROPE) — ASEAN nations and India benefit from strait stability and freedom of navigation (coordination function), but also bear cost of dependence and risk of power shifts. Organized actors see both coordination gains and asymmetric extraction. Hegemonic control disrupts the multilateral coordination equilibrium. d≈0.62, f(d)≈0.90, σ=1.1 → χ≈0.55.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: NON-HEGEMON GREAT POWER (SNARE) — If China gains hegemonic control, the US faces strategic chokehold and cannot easily reverse it through military means without catastrophic war. Constrained exit: military options are existential-risk escalatory; diplomatic options are forfeited once hegemony is established. Trapped in reactive posture. d≈0.85, f(d)≈1.28, σ=1.2 → χ≈0.61.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: HEGEMON CONTROLLING STRAIT (ROPE) — From the hegemonic power's view, the strait represents a coordination mechanism: enforcing safe passage at scale generates legitimate revenue, geopolitical leverage, and network control benefits. The hegemonic power sees the constraint as fundamentally cooperative infrastructure with asymmetric benefits. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.08. Net beneficiary; negative effective extraction because the hegemonic power sees itself as providing a service.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: SUPPLY CHAIN DECOUPLING COALITION (SCAFFOLD) — States and corporations investing in alternative supply chains, renewable energy, and nearshoring see the strait constraint as temporary. Diversified energy sourcing (LNG terminals in Europe, Middle East, Africa) and rail/pipeline alternatives reduce chokepoint leverage over time. Sunset mechanism: 15-25 year horizon for alternative infrastructure maturity. d≈0.48, f(d)≈0.65, σ=1.1 → χ≈0.31. Mobile exit and visible sunset clause enable scaffold classification.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOGRAPHIC INEVITABILITY (MOUNTAIN) — From a civilizational timescale, geographic chokepoints are immutable features of global trade topology. The Taiwan Strait is one of ~12 critical maritime passages (Suez, Panama, Hormuz, Malacca) that will constrain geopolitics indefinitely. This perspective risks naturalizing what is actually a contingent hegemonic configuration. Structural data (ε=0.68, suppression=0.72, theater=0.38, requires_active_enforcement=true) contradicts mountain classification — the engine will compute false summit.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_strait_hegemony_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_strait_hegemony_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(taiwan_strait_hegemony_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. A hegemonic power controlling Taiwan can impose shipping tolls, selective access denial, currency manipulation, and strategic concessions. The 30% of global trade figure is not hyperbolic — it represents real chokepoint leverage. However, the extraction is not as absolute as pure monopoly (ε=0.90) because alternative routes exist (expensive but viable) and energy transition is underway. The measurement trajectory (0.35 → 0.68 over 10 years) reflects the ramp-up of hegemonic enforcement capacity as the controlling power consolidates position and dependent states realize alternatives are limited. Suppression (0.72): High. Once a hegemonic power controls Taiwan, military interdiction, blockade, and denial are credible coercive mechanisms. The suppression level is not maximal (0.90+) because large coalitions can theoretically challenge the hegemon (US Navy, QUAD alliance, NATO logistics), but the suppression is sufficient to prevent unilateral exit by any single dependent state. Theater ratio (0.38): Low. The extraction mechanism is primarily physical (military/naval control) rather than institutional theater. Chokepoint enforcement is not performative — blockade threats are real and visible. However, some theater persists in the political-diplomatic framing (freedom of navigation narratives, legitimacy claims) and in the institutional mechanisms of toll collection.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival gap between the hegemon and the trapped. The hegemon sees the strait as coordinated infrastructure (Rope) — enforcing safe passage, collecting tolls, providing security services. Dependent states see pure extraction (Snare) — they have no meaningful exit and cannot refuse the hegemon's terms. The non-hegemon great power sees a strategic trap (Snare) — constrained by the reality that reversing hegemony requires war. The supply-chain coalition sees a temporary problem (Scaffold) — renewable energy and nearshoring will reduce chokepoint leverage over 15-25 years. Regional powers see mixed coordination and extraction (Tangled Rope) — they benefit from stability but pay the hegemon's rent. The analytical observer risks seeing a natural law (Mountain) — geographic chokepoints are eternal constraints — but the structural data (high suppression, active enforcement, beneficiary/victim asymmetry) reveals this as a false summit: the chokepoint lock is contingent on a specific hegemonic configuration, not on physics or geography alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Dependent shipping states (Japan, South Korea): Victim + trapped → d≈0.92, f(d)≈1.40, σ=1.2 (global scope) → χ≈0.71. Nearly maximal extraction because these states cannot avoid the strait and have no credible military capacity to challenge the hegemon. Global energy importers: Victim + trapped → similar d≈0.92. Non-hegemon great power (US if China hegemon): Victim + constrained (not trapped, because military options exist, but escalatory) → d≈0.85, f(d)≈1.28, σ=1.2 → χ≈0.61. Powerful but constrained because reversing hegemony requires unacceptable war risk. Regional powers (ASEAN, India): Mixed (partial victims, partial beneficiaries of stability) + constrained → d≈0.62, f(d)≈0.90, σ=1.1 → χ≈0.55. Tangled rope classification reflects dual status. Supply chain coalition: Partial victims (near-term) + mobile (can build alternatives) → d≈0.48, f(d)≈0.65, σ=1.1 → χ≈0.31. Low effective extraction because coalition has agency and visible exit path (energy transition, nearshoring). Hegemon: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.08. Net beneficiary; sees itself as providing secure infrastructure service.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's high extractiveness (0.68 > 0.70 gate not triggered, but close) is legitimate and reflects the structural reality that a hegemonic power controlling a critical chokepoint can extract significant value while still providing coordination services (safe passage, rule predictability, regional stability). The mandatrophy is resolved by noting that this is a true Snare (not mislabeled Rope or Mountain), and the beneficiary/victim asymmetry is real and structural. The hegemon genuinely benefits from providing coordination services (Rope aspect), but the distribution of benefits is so skewed that the constraint functions primarily as extraction (Snare aspect). This is not a conceptual error but a real feature of chokepoint politics: the person who controls the gate both coordinates traffic AND extracts rents. The framework's resolution: measure both the coordination function (is safe passage provided? yes) and the extraction asymmetry (is the hegemon's benefit 100x larger than dependent states' benefit? yes). Both are true, and Snare is the appropriate type when extraction overwhelmingly dominates coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hegemonic_stability_durability,
    'If China or another power achieves hegemonic control of Taiwan, how durable is the coercive capability? Can it be maintained against sustained coalition resistance or will it degrade within 20-30 years?',
    'Historical analysis of chokepoint hegemonies: British Suez control (1882-1956), Japanese Strait control (1938-1945), Soviet Bosphorus leverage (1945-1991). Comparative analysis of how long coercive maritime control persists against organized resistance.',
    'If durable (>30 years): snare classification holds; no realistic exit path except coalition war. If degradable (<20 years): reclassifies as scaffold; exit mechanisms become visible and reachable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemonic_stability_durability, empirical, 'Durability of hegemonic chokepoint control against coalition resistance').

omega_variable(
    alternative_route_feasibility,
    'Are Lombok Strait, Sunda Strait, and Malacca Strait alternatives actually viable at scale, or do geographic/political constraints make them equally vulnerable to interdiction?',
    'Engineering analysis of reroute costs, transit times, and throughput limits. Geopolitical analysis of Indonesia''s and Malaysia''s exposure to pressure (ASEAN is internally divided; Indonesia faces economic dependency). Scenario modeling of simultaneous strait closures.',
    'If alternatives are viable: dependent states have constrained but real exit options; classification shifts from snare toward tangled_rope or scaffold. If alternatives are equally vulnerable: trap is more complete; snare classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_route_feasibility, empirical, 'Feasibility of alternative maritime routes as genuine exit options').

omega_variable(
    energy_transition_chokepoint_obsolescence,
    'As global energy transitions to renewables and nuclear, does the strategic value of LNG chokepoints decline faster than coal/oil dependencies did?',
    'Forecast modeling of energy demand by fuel type through 2050 (IEA, IRENA scenarios). Correlation analysis of historical chokepoint leverage with commodity dependency. Sensitivity analysis to renewable transition acceleration.',
    'If transition is rapid (≤15 years to 40% renewable baseload): scaffold sunset becomes real and reachable; hegemon''s extraction capability decays predictably. If transition stalls (>30 years): energy chokepoint remains potent; snare properties persist longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_transition_chokepoint_obsolescence, empirical, 'Energy transition pace and its impact on chokepoint extraction durability').

omega_variable(
    coalition_coordination_threshold,
    'At what cost level (% of GDP for trade disruption, military mobilization requirements) does a coalition of dependent states overcome coordination problems and mount joint resistance?',
    'Game-theoretic analysis of coalition payoffs under different toll regimes. Historical case studies: NATO burden-sharing, OPEC countermeasures to sanctions, submarine cable interdiction response. Threshold estimation from political economy models.',
    'If threshold is low (<2% GDP cost to coalition): organized resistance becomes likely within 5-10 years; snare degrades to tangled_rope. If threshold is high (>5% GDP): hegemon can extract for decades; snare properties harden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_coordination_threshold, empirical, 'Coalition mobilization threshold for joint resistance to chokepoint extraction').

omega_variable(
    us_military_capacity_for_denial,
    'If the US commits to strait denormalization (SLOC interdiction of hegemon''s traffic), can it enforce a counter-embargo, and for how long before mutual exhaustion or escalation?',
    'Military capability analysis: carrier fleet rotation rates, submarine logistics, air sortie sustainability over 2+ year campaigns. Cost-benefit analysis of mutual SLOC denial (both sides suffer ~$1-2T annual trade loss). Escalation pathway analysis to nuclear brink.',
    'If US capacity is credible and sustainable: hegemon knows chokepoint leverage is reversible; changes snare to tangled_rope (enforcer always has exit option). If US capacity is limited: hegemon dominance is nearly irrevocable; snare hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_military_capacity_for_denial, empirical, 'US military capacity to contest hegemonic strait control through counter-interdiction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_strait_hegemony_shift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twsh_tr_t0, taiwan_strait_hegemony_shift, theater_ratio, 0, 0.28).
narrative_ontology:measurement(twsh_tr_t5, taiwan_strait_hegemony_shift, theater_ratio, 5, 0.33).
narrative_ontology:measurement(twsh_tr_t10, taiwan_strait_hegemony_shift, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(twsh_be_t0, taiwan_strait_hegemony_shift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(twsh_be_t5, taiwan_strait_hegemony_shift, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(twsh_be_t10, taiwan_strait_hegemony_shift, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_strait_hegemony_shift, global_infrastructure).
narrative_ontology:affects_constraint(taiwan_strait_hegemony_shift, semiconductor_supply_chain).
narrative_ontology:affects_constraint(taiwan_strait_hegemony_shift, east_asian_power_balance).
narrative_ontology:affects_constraint(taiwan_strait_hegemony_shift, global_energy_security).
narrative_ontology:affects_constraint(taiwan_strait_hegemony_shift, us_alliance_credibility).

% DUAL FORMULATION NOTE:
% The Taiwan Strait chokepoint is downstream of the Taiwan political status question (independent vs PRC control) and upstream of regional power balance, energy security, and semiconductor supply. The constraint's ε value (0.68) reflects a hegemonic configuration; alternative ε values could be estimated for different political outcomes (US-Japan maintenance of status quo: ε≈0.25 (Rope), PRC blockade scenario: ε≈0.85 (pure Snare)). This story models the hegemonic-shift scenario; sibling stories would model status-quo maintenance and blockade escalation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_strait_hegemony_shift, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
