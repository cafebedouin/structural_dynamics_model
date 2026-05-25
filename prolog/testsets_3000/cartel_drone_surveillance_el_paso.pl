% ============================================================================
% CONSTRAINT STORY: cartel_drone_surveillance_el_paso
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cartel_drone_surveillance_el_paso, []).

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
 *   constraint_id: cartel_drone_surveillance_el_paso
 *   human_readable: Cartel Drone Surveillance Monopoly over El Paso Border Area
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   Mexican cartel organizations have established persistent drone
 *   surveillance networks over the El Paso, Texas border region, creating a
 *   structural monopoly on aerial intelligence asymmetry. This constraint
 *   arises from the convergence of three structural conditions: (1) cartels
 *   possess superior financial capacity and asset-loss tolerance to field and
 *   maintain drone networks; (2) US enforcement agencies operate under
 *   legal/jurisdictional constraints that prevent kinetic counter-drone
 *   response; (3) Mexican government border authority lacks equivalent drone
 *   capacity or institutional will to interdict cartel operations on its own
 *   territory. The surveillance network enables cartel optimization of
 *   smuggling timing, route selection, pricing power, and targeting of
 *   enforcement vulnerabilities. Simultaneously, it extracts from civilian
 *   populations through informant recruitment, extortion surveillance, and
 *   violence spillover. The constraint exhibits all six DR types from
 *   different perspectives, revealing how a technological asymmetry
 *   intersects with institutional constraint structures to produce pure
 *   extraction for powerless agents and coordination benefit for cartels.
 *
 * KEY AGENTS:
 *   - Mexican Cartel Organizations: Primary beneficiary (institutional/arbitrage) — captures intelligence advantage, pricing power, route optimization, enforcement evasion capability
 *   - El Paso Civilian Population: Primary victim (powerless/trapped) — subject of continuous surveillance, informant recruitment pressure, extortion targeting, violence spillover risk
 *   - US Border Enforcement Agencies (CBP, DEA, ICE): Secondary victim (organized/constrained) — experience intelligence disadvantage, surveillance asymmetry, reduced interdiction capacity
 *   - Legitimate Cross-Border Commerce: Secondary victim (moderate/trapped) — subject to cartel targeting, extortion, route interdiction, security cost imposition
 *   - Mexican Government Border Authority: Mixed (institutional/constrained) — experiences extraction from cartel surveillance but also forced cooperation requirement
 *   - US Military/Intelligence Apparatus: Institutional observer (institutional/arbitrage) — possesses mitigation capacity but operates under performative legal constraint framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cartel_drone_surveillance_el_paso, 0.68).
domain_priors:suppression_score(cartel_drone_surveillance_el_paso, 0.78).
domain_priors:theater_ratio(cartel_drone_surveillance_el_paso, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, extractiveness, 0.68).
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(cartel_drone_surveillance_el_paso, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cartel_drone_surveillance_el_paso, snare).
narrative_ontology:human_readable(cartel_drone_surveillance_el_paso, "Cartel Drone Surveillance Monopoly over El Paso Border Area").
narrative_ontology:topic_domain(cartel_drone_surveillance_el_paso, "geopolitical/technological").

domain_priors:requires_active_enforcement(cartel_drone_surveillance_el_paso).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cartel_drone_surveillance_el_paso, mexican_cartel_organizations).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, us_border_enforcement_capacity).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, civilian_population_el_paso_region).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, legitimate_cross_border_commerce).
narrative_ontology:constraint_victim(cartel_drone_surveillance_el_paso, border_community_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EL PASO CIVILIAN POPULATION (SNARE) — Residents cannot exit the surveillance zone; carry the cost of cartel intelligence advantage, increased violence spillover risk, and informant vulnerability. No organized resistance capacity. Maximal experienced extraction — trapped subject of continuous monitoring with no exit option or recourse mechanism.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: US BORDER ENFORCEMENT AGENCIES (TANGLED ROPE) — CBP, DEA, ICE experience both extraction (intelligence disadvantage, surveillance asymmetry) and coordination benefit (identifying incursion vectors, coordinating countermeasures). Constrained by budget, legal authority limitations, and technological disadvantage. Active enforcement required to maintain counter-surveillance. Mixed extraction and legitimate defensive coordination.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGITIMATE CROSS-BORDER COMMERCE OPERATORS (SNARE) — Cannot exit the region; face targeting risk from cartel surveillance (theft, extortion, route interdiction). Pay implicit extraction tax through security precautions and loss of commerce efficiency. Trapped between cartel demand for tribute and law enforcement scrutiny. High suppression — limited ability to organize or report without retaliation.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: MEXICAN CARTEL ORGANIZATIONS (ROPE) — Primary beneficiary. Drone network solves critical coordination problem: timing shipments, avoiding enforcement, identifying opportunities. Experiences constraint as enabling mechanism. Extraction is positive from this perspective — the system works for them. Institutional power, high arbitrage (can shift operations, scale, or change tactics).
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MEXICAN GOVERNMENT BORDER AUTHORITY (TANGLED ROPE) — Experiences both extraction (cartel surveillance undermines state capacity, sovereignty violated on their territory) and coordination failure (must cooperate with US to address asymmetric threat). Constrained by cartel violence, corruption penetration, and limited resources. Active enforcement of counter-surveillance is required but produces limited extraction reduction because cartels have institutional-level resources.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: US MILITARY/INTELLIGENCE APPARATUS (PITON) — Possesses technological capability to neutralize the drone network completely but operates under legal/jurisdictional constraints and diplomatic theater. Maintains theater of 'not intervening in foreign security matters' while cartel surveillance creates direct national security threat. Capacity has degraded relative to threat due to institutional inertia and jurisdictional constraints. Maintains restrictive posture through performative policy framework rather than functional limitation.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a systems analysis perspective, drone surveillance asymmetry follows from differential resource access and legal constraint structures. Appears immutable: cartels have financial capacity and willingness to lose assets; enforcement agencies have legal/political constraints. However, this naturalizes a contingent institutional choice (legal constraints are policy-settable, resource concentration is not immutable). Engine will classify as false summit.
constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cartel_drone_surveillance_el_paso_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cartel_drone_surveillance_el_paso, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cartel_drone_surveillance_el_paso, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cartel_drone_surveillance_el_paso, TR),
    TR >= 0.70.

:- end_tests(cartel_drone_surveillance_el_paso_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The cartel network extracts significant value from enforcement agencies (intelligence advantage, interdiction cost reduction) and from civilian populations (informant networks, extortion capacity, violence targeting ability). The extractiveness has increased over the measurement interval (0.42 → 0.68) as drone technology matured, cartel operational sophistication increased, and enforcement adaptation lagged. Suppression (0.78): Very high. Suppression mechanisms include: cartel violence deterrent against counter-surveillance attempts, informant networks preventing civilian cooperation with enforcement, legal constraints preventing US kinetic response, corruption penetration in Mexican authorities, and geographic/technological barriers to detection and interdiction. Escape from the surveillance regime requires exit from the region entirely — there is no constrained-but-functional mode. Theater ratio (0.35): Low. This is a functional, non-performative extraction mechanism. Drones provide direct operational intelligence; the constraint has minimal theater component. The low theater reflects that cartel operations are genuinely optimized by surveillance, not maintained through performance. The declining theater trajectory (0.50 → 0.35) indicates increasing functional integration of surveillance into cartel operations — early drone deployment had experimental/uncertain function; mature deployment is purely operational.
 *
 * PERSPECTIVAL GAP:
 *   The original research group (cartels) sees pure coordination (Rope) — the drone network solves logistics, timing, and opportunity identification. US enforcement agencies see tangled rope — they must coordinate counter-surveillance while bearing intelligence extraction costs. Civilian populations see pure snare — they cannot exit and have no benefit from the system. The Mexican government sees tangled rope — forced to coordinate against a threat that violates sovereignty while bearing extraction from cartel presence. The military apparatus sees degraded capacity (Piton) — possesses mitigation capability but operates under legal theater (jurisdictional constraints framed as immutable when they are policy-settable). The analytical observer risks naturalizing the constraint as immutable (Mountain) — 'asymmetric resources are inherent' — but the structural data reveals that enforcement capacity is constrained by policy choices, not physical law. The perspectival gap between cartel (Rope) and civilian (Snare) is maximum — they experience the identical structure completely differently because exit options differ fundamentally.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from agent structural position relative to extraction flow. Mexican cartels are beneficiaries with high arbitrage capacity: they can scale operations, shift tactics, or relocate surveillance networks with minimal constraint. Their d → 0.0 (full beneficiary), producing negative effective extraction (the system works for them). El Paso civilians are trapped victims with no exit option: d → 0.95 (full target), producing maximum f(d) and maximum experienced extraction chi. US enforcement agencies have institutional power but constrained exit (mobile only within legal bounds): d → 0.65 (victim with some agency), producing strong experienced extraction. Cross-border commerce operators are trapped but with some organizational capacity: d → 0.70 (trapped victim), producing strong extraction. Mexican government is constrained institutional actor: d → 0.50-0.60 (symmetric but with power asymmetry favoring extraction), producing moderate extraction. The military apparatus appears as institutional/arbitrage but with artificial constraints: d override may be needed to reflect that legal constraints reduce their effective arbitrage capacity to constrained-equivalent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy confusion through clear structural decomposition. The cartel sees Rope (pure coordination) because the drone network genuinely solves their logistics problem. The civilian population sees Snare (pure extraction) because they bear costs with no corresponding benefit. Neither perspective is wrong — they describe different structural relationships to the same physical system. The enforcement agencies see Tangled Rope (mixed) because they must coordinate counter-surveillance while bearing extraction costs. The potential false positive would be to classify this as 'one constraint with multiple readings' when actually the structural relationship differs fundamentally: for cartels, it's a functional coordination tool; for civilians, it's a surveillance-extraction mechanism. The decomposition clarifies that this is a single constraint with genuinely different roles occupied by different agents. The mountain perspective (natural law framing) is a false summit: the constraint persists not because asymmetric resources are inherent, but because institutional policy choices (legal constraints, resource allocation to enforcement) are set to preserve the status quo. The piton perspective correctly identifies that the military apparatus maintains theater (jurisdictional deference framed as immutable legal doctrine) to avoid institutional disruption, not because mitigation is technically impossible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_financial_capacity_threshold,
    'What is the critical financial threshold at which cartel drone network maintenance becomes economically unsustainable versus worth the strategic advantage?',
    'Economic analysis of cartel operational budgets, drone fleet replacement costs, operator training requirements, and comparison to estimated smuggling revenue protection and pricing power gains from surveillance advantage',
    'If threshold is achievable through enforcement action: snare can be partially degraded to tangled_rope through attrition. If threshold exceeds enforcement capacity: snare classification is robust and only political constraint removal enables mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_financial_capacity_threshold, empirical, 'Economic threshold for cartel drone network maintenance').

omega_variable(
    enforcement_legal_constraint_removability,
    'Are the legal/jurisdictional constraints preventing kinetic US response to cartel drone incursions structural policy choices or immutable legal doctrine?',
    'Constitutional and international law analysis; comparison to historical precedents of US military action in response to direct cross-border threats; policy analysis of sovereignty doctrine evolution',
    'If removable through policy: mountain perspective is false summit; snare can be degraded through institutional will. If immutable: mountain perspective partially correct; snare persists due to legal structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_legal_constraint_removability, conceptual, 'Whether legal constraints on enforcement response are removable through policy').

omega_variable(
    mexican_state_counter_capacity_trajectory,
    'Is the degradation of Mexican state counter-drone capacity an intrinsic structural feature or a reversible institutional failure?',
    'Longitudinal analysis of Mexican military drone assets, training capacity, and anti-drone technology procurement; assessment of corruption penetration in border security apparatus; correlation with federal law enforcement investment cycles',
    'If reversible: binational coordination can transition from US-centric enforcement to shared burden (tangled_rope from Mexican perspective becomes rope). If intrinsic: cartel advantage is structural until cartel financial capacity declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mexican_state_counter_capacity_trajectory, empirical, 'Whether Mexican state counter-drone capacity degradation is reversible').

omega_variable(
    civilian_cost_asymmetry_irreversibility,
    'Do the informant networks and cartel-civilian penetration created by surveillance asymmetry constitute reversible institutional damage or permanent structural change?',
    'Intelligence assessment of cartel informant penetration depth and civilian collaboration extent; comparison to historical post-conflict reconciliation in regions with similar surveillance states; analysis of informant safety and reintegration capacity',
    'If reversible: suppression metrics can decline post-enforcement. If irreversible: civilian population remains trapped even if drone network is neutralized; snare persists in modified form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_cost_asymmetry_irreversibility, empirical, 'Reversibility of civilian informant network damage from surveillance regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cartel_drone_surveillance_el_paso, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cdse_tr_t0, cartel_drone_surveillance_el_paso, theater_ratio, 0, 0.5).
narrative_ontology:measurement(cdse_tr_t3, cartel_drone_surveillance_el_paso, theater_ratio, 3, 0.42).
narrative_ontology:measurement(cdse_tr_t6, cartel_drone_surveillance_el_paso, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(cdse_be_t0, cartel_drone_surveillance_el_paso, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cdse_be_t3, cartel_drone_surveillance_el_paso, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(cdse_be_t6, cartel_drone_surveillance_el_paso, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cartel_drone_surveillance_el_paso, enforcement_mechanism).
narrative_ontology:affects_constraint(cartel_drone_surveillance_el_paso, drug_trafficking_supply_chain_fragmentation).
narrative_ontology:affects_constraint(cartel_drone_surveillance_el_paso, us_mexico_border_sovereignty_asymmetry).

% DUAL FORMULATION NOTE:
% The cartel drone surveillance monopoly is downstream of technological capability asymmetry and upstream of enforcement coordination failure. Decomposition would separate (1) technological capability gap (which agent can afford/deploy drones) from (2) enforcement legal constraint structure (which agent is permitted to conduct counter-measures). Current story treats them as unified, but epsilon would shift substantially if US military authority constraints were removed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cartel_drone_surveillance_el_paso, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
