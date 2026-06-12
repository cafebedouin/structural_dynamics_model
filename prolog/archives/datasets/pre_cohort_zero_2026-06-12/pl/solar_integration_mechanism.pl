% ============================================================================
% CONSTRAINT STORY: solar_integration_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_solar_integration_mechanism, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: solar_integration_mechanism
 *   human_readable: Passive Solar Priority via Blocking Diode
 *   domain: electrical_engineering/power_systems/off_grid_infrastructure
 *
 * SUMMARY:
 *   The blocking diode on a MeanWell AC-DC converter output enables passive
 *   solar priority in off-grid power systems through a simple physical
 *   mechanism: when the solar array's maximum power point voltage (Vmp)
 *   exceeds the MeanWell's output voltage, the diode reverse-biases and the
 *   AC converter idles, drawing minimal power. When solar voltage drops below
 *   the threshold (clouds, night, high load), the diode forward-biases and
 *   the MeanWell resumes supplying power. This constraint is a diagnostic
 *   exemplar of pure coordination (rope): it solves a genuine
 *   collective-action problem (prioritizing renewable energy without complex
 *   control systems) with minimal extractive overhead, alternatives are not
 *   suppressed (active switching solutions exist and are used where
 *   appropriate), and no party collects rents from the mechanism's operation.
 *   The diode is a commodity component; the pattern is openly documented in
 *   DIY solar forums and off-grid communities; the coordination function is
 *   transparent and verifiable by measuring AC power consumption and diode
 *   bias state.
 *
 * KEY AGENTS:
 *   - Off-Grid System Operators: Primary beneficiary (powerless to moderate / mobile) — gain automatic solar priority without programming or maintenance
 *   - Solar Array Owners: Primary beneficiary (moderate / mobile) — maximize solar utilization through passive priority mechanism
 *   - Battery Bank Longevity: Secondary beneficiary (abstract good, not an agent) — reduced cycling from grid AC when solar is available extends battery life
 *   - System Integrators: Coordination facilitator (moderate / mobile) — specify and install the diode as a standard design pattern
 *   - Solar Equipment Manufacturers: Coordination beneficiary (institutional / arbitrage) — benefit from interoperable design standard that enables product compatibility
 *   - Off-Grid Community Networks: Coordination propagator (organized / constrained) — document and teach the pattern as open knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(solar_integration_mechanism, 0.08).
domain_priors:suppression_score(solar_integration_mechanism, 0.12).
domain_priors:theater_ratio(solar_integration_mechanism, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solar_integration_mechanism, extractiveness, 0.08).
narrative_ontology:constraint_metric(solar_integration_mechanism, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(solar_integration_mechanism, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(solar_integration_mechanism, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(solar_integration_mechanism, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(solar_integration_mechanism, rope).
narrative_ontology:human_readable(solar_integration_mechanism, "Passive Solar Priority via Blocking Diode").
narrative_ontology:topic_domain(solar_integration_mechanism, "electrical_engineering/power_systems/off_grid_infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(solar_integration_mechanism, off_grid_system_operators).
narrative_ontology:constraint_beneficiary(solar_integration_mechanism, solar_array_owners).
narrative_ontology:constraint_beneficiary(solar_integration_mechanism, battery_bank_longevity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(solar_integration_mechanism, off_grid_homesteader).
narrative_ontology:constraint_beneficiary(solar_integration_mechanism, solar_equipment_manufacturer).
narrative_ontology:constraint_beneficiary(solar_integration_mechanism, off_grid_community_network).
narrative_ontology:constraint_vindicates(solar_integration_mechanism, passive_component_superiority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lives off-grid with solar array and AC backup power. Wants solar to take priority automatically when available. The blocking diode gives them this without programming or maintenance — when solar voltage is high enough, the diode blocks AC power and the converter idles. When solar drops, AC resumes automatically. Can remove the diode and wire direct if desired, but has no reason to — the mechanism solves their priority problem with a $10 component.
narrative_ontology:constraint_stakeholder(solar_integration_mechanism, off_grid_homesteader, beneficiary,
    powerless, immediate, mobile, local).

% Designs and installs off-grid power systems. Specifies the blocking diode as a standard design pattern for solar integration because it is simple, reliable, and requires no firmware. Could specify active switching instead, but the passive solution has fewer failure modes and lower cost for most residential applications. Sets the agenda by choosing which integration method to use, but is not locked in — can switch to active solutions when system requirements justify the added complexity.
narrative_ontology:constraint_stakeholder(solar_integration_mechanism, system_integrator, agenda_setter,
    moderate, biographical, mobile, regional).

% Manufactures solar charge controllers, inverters, and AC-DC converters. Benefits from the blocking diode pattern as an interoperable design standard — customers can mix and match equipment from different vendors because the integration method is not proprietary. Could push proprietary active switching to create vendor lock-in, but the passive solution is a coordination win that expands the market. Has arbitrage exit (can pivot to proprietary solutions) but chooses not to because open standards drive adoption.
narrative_ontology:constraint_stakeholder(solar_integration_mechanism, solar_equipment_manufacturer, beneficiary,
    institutional, generational, arbitrage, global).

% DIY solar forums, off-grid cooperatives, and open-source energy projects that document and teach the blocking diode pattern. Benefits from a simple, teachable coordination mechanism that enables mutual aid and knowledge sharing. Constrained exit (community norms favor passive, open solutions over proprietary active switching) but the constraint is coordination, not extraction — the diode pattern is propagated because it works and is accessible, not because anyone collects from it.
narrative_ontology:constraint_stakeholder(solar_integration_mechanism, off_grid_community_network, beneficiary,
    organized, generational, constrained, national).

% Observes the blocking diode mechanism from a civilizational perspective. Sees a coordination solution that solves the solar priority problem with minimal complexity and no rent extraction. The diode's forward voltage drop is inherent coordination cost (the physical price of passive switching), not extractive overhead. Alternatives exist and are used where appropriate (active switching for efficiency-critical applications), so the mechanism does not suppress competing solutions. No party collects rents from the diode's operation — it is a commodity component in an open design pattern.
narrative_ontology:constraint_stakeholder(solar_integration_mechanism, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Automatic solar priority in off-grid power systems: when solar array voltage exceeds AC converter output voltage, the blocking diode reverse-biases and the AC converter idles, allowing solar to supply the load. When solar voltage drops (clouds, night, high load), the diode forward-biases and AC power resumes. Solves the coordination problem of prioritizing renewable energy without complex control systems.
% TRANSFER_FUNCTION: The blocking diode transfers control authority from active switching logic to passive voltage differential. When solar Vmp > MeanWell output, the diode blocks AC power flow and the converter draws minimal idle current. The 'transfer' is not of money or resources but of priority: solar gets first claim on the load, AC fills in when solar is insufficient. The diode's forward voltage drop (0.3-0.7V) is the coordination cost — energy lost to the switching mechanism.
% ABSENT_VOICES: Potential absent voice: active switching controller manufacturers who might prefer proprietary solutions over open passive designs. However, this voice is not structurally excluded — active switching solutions exist in the market and are used where their benefits justify their costs. The blocking diode pattern coexists with active solutions rather than suppressing them. No evidence of systematically excluded stakeholders.
% DISAPPEARANCE_RATIONALE: If the blocking diode disappeared overnight (removed from all off-grid systems using this pattern), system operators would need to choose: (1) wire solar and AC in parallel without priority logic, risking backfeed and inefficient operation, (2) install active switching controllers, adding cost and complexity, (3) manually switch between solar and AC, adding labor, or (4) abandon solar integration and run AC-only. The world rearranges — arrangements depend on the diode's coordination function. This is not a natural fact (the diode must be installed), and its absence would force alternative coordination mechanisms.
% FOUNDING_PROBLEM: Off-grid power systems with both solar and AC backup needed a way to prioritize solar (to maximize renewable energy use and minimize generator runtime or grid draw) without complex control systems, firmware, or active switching that could fail. The founding problem: how to make solar take priority automatically, reliably, and cheaply.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live and is corroborated by: (1) ongoing adoption of the blocking diode pattern in new off-grid installations documented in DIY solar forums and integrator case studies, (2) continued demand for passive solar priority solutions in reliability-critical applications (remote telecom sites, off-grid medical facilities) where firmware failures are unacceptable, (3) persistence of the pattern alongside newer active switching solutions, indicating that the passive approach still solves a real problem for a significant user base. Corroboration comes from system integrators (moderate power, outside the beneficiary set of equipment manufacturers) and off-grid operators (powerless, direct users of the mechanism).
narrative_ontology:disappearance_verdict(solar_integration_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(solar_integration_mechanism, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OFF-GRID HOMESTEADER (ROPE) — Experiences the blocking diode as pure coordination: solar takes priority when available, grid AC idles automatically, no programming required. Mobile exit (can remove diode and wire direct) but no reason to — the mechanism solves the priority problem with minimal overhead. Low extraction, genuine coordination benefit.
constraint_indexing:constraint_classification(solar_integration_mechanism, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: SYSTEM INTEGRATOR (ROPE) — Sees the diode as an elegant coordination solution: one passive component replaces firmware, switching logic, and failure modes. Mobile exit (could specify active switching) but the passive solution is preferable. Coordination function is clear: automatic solar priority without active control. Minimal extraction — the diode costs $3-15 depending on current rating.
constraint_indexing:constraint_classification(solar_integration_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: SOLAR EQUIPMENT MANUFACTURER (ROPE) — Benefits from the blocking diode pattern as a design standard that enables simple solar integration across product lines. Arbitrage exit (could push proprietary active switching) but the passive solution is a coordination win: interoperability, no vendor lock-in, no firmware updates. Low extraction — the diode is a commodity component with no rent-collection mechanism.
constraint_indexing:constraint_classification(solar_integration_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OFF-GRID COMMUNITY NETWORK (ROPE) — Organized agents (DIY solar forums, off-grid cooperatives, open-source energy projects) see the blocking diode as a coordination standard that enables knowledge sharing and mutual aid. Constrained exit (community norms favor passive solutions) but the constraint is coordination, not extraction. The diode pattern is taught, documented, and replicated because it works, not because anyone collects from it.
constraint_indexing:constraint_classification(solar_integration_mechanism, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the blocking diode is a coordination mechanism that solves the solar priority problem with minimal complexity. The mechanism is not a natural law (it requires human design and installation) but it exhibits rope characteristics: low extraction, genuine coordination function, alternatives are not suppressed (active switching exists and is used where appropriate), and no party collects rents from its operation. The diode's forward voltage drop (0.3-0.7V) is inherent coordination cost, not extractive overhead.
constraint_indexing:constraint_classification(solar_integration_mechanism, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(solar_integration_mechanism_tests).
:- end_tests(solar_integration_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The blocking diode's forward voltage drop (0.3-0.7V for Schottky diodes, the typical choice for this application) represents inherent coordination cost, not extractive overhead. At 12V system voltage, a 0.5V drop is 4.2% efficiency loss, but this is the physical cost of the passive switching mechanism, not rent extraction. The diode itself is a commodity component ($3-15 depending on current rating) with no vendor lock-in or proprietary control. The pattern is openly documented and can be implemented with any suitable diode. Suppression (0.12): Very low. Alternatives are not suppressed — active switching solutions (MOSFET controllers, microcontroller-based priority logic, commercial hybrid inverters with built-in solar integration) exist and are used where their benefits (lower voltage drop, more complex priority logic, data logging) justify their costs (higher price, firmware complexity, additional failure modes). The blocking diode pattern coexists with these alternatives rather than suppressing them. The modest suppression value reflects only the coordination cost of learning the pattern and selecting appropriate components. Theater ratio (0.05): Negligible. The mechanism is purely functional — the diode either conducts or blocks based on voltage differential, with no performative overlay. There is no certification requirement, no compliance ritual, no gatekeeping. The only 'theater' is the minimal documentation and component selection process, which serves a genuine coordination function (ensuring the diode is rated for the system's voltage and current). Accessibility collapse (0.35): Moderate. Once the blocking diode pattern is understood, alternative approaches (always-on AC converter, manual switching, no solar integration) become less attractive for the specific use case of passive solar priority. However, alternatives remain viable for different use cases (active switching for efficiency-critical applications, no integration for AC-only systems), so collapse is partial rather than total. Resistance (0.15): Low. The mechanism meets minimal resistance because it solves a real problem (solar priority) with a simple, low-cost solution. Resistance comes primarily from unfamiliarity (installers who haven't encountered the pattern) and from contexts where active switching is genuinely preferable (large systems where the diode's voltage drop becomes significant, or systems requiring data logging and complex priority logic).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all agents classify it as rope. The off-grid homesteader sees coordination (automatic solar priority). The system integrator sees coordination (elegant passive solution). The manufacturer sees coordination (interoperable standard). The community network sees coordination (shared knowledge). The analytical observer sees coordination (genuine problem solved with minimal overhead). The uniformity across perspectives is diagnostic of pure coordination: when a mechanism solves a collective-action problem without creating victims, all structural positions perceive it as coordination. The only potential gap is between experts (who see the diode as obviously correct) and novices (who may not know the pattern exists), but this is a knowledge gap, not a structural extraction gap. Once the pattern is learned, the coordination function is transparent.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this constraint are beneficiaries or neutral — there are no victims. The off-grid system operator benefits from automatic solar priority without programming. The solar array owner benefits from maximized solar utilization. The system integrator benefits from a simple, reliable design pattern. The equipment manufacturer benefits from an interoperable standard. The off-grid community benefits from shared knowledge. The battery bank (an abstract good, not an agent) benefits from reduced cycling. The blocking diode's forward voltage drop is a coordination cost borne by the system as a whole, not extraction from a specific victim group. Directionality values are low across all agents (d ≈ 0.1-0.2), reflecting that the constraint's operation benefits participants rather than extracting from them. The engine will compute low or negative effective extraction (chi) for all perspectives, consistent with rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that rope classification is stable across all perspectives when the coordination function is genuine and extraction is minimal. There is no mandate that has outlived its function — the blocking diode continues to solve the solar priority problem it was designed for. The mechanism has not degraded into theater (piton) because it remains functional. It has not accumulated extraction (tangled_rope or snare) because no party collects rents from its operation. It is not a temporary solution (scaffold) because there is no sunset — passive priority will remain useful as long as off-grid solar systems exist, even if active switching solutions become more common for specific use cases. The constraint is not a natural law (mountain) because it requires human design and installation, but it exhibits mountain-like stability: the physics of diode operation (reverse bias at higher voltage, forward bias at lower voltage) is invariant, and the coordination function follows directly from this physical behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voltage_drop_threshold,
    'At what forward voltage drop does the diode''s coordination cost exceed the benefit of passive priority switching?',
    'Empirical measurement of system efficiency loss vs active switching overhead (firmware complexity, failure modes, maintenance burden) across different load profiles and solar array sizes',
    'If threshold is lower than typical Schottky diode drop (~0.3V): active switching becomes preferable for efficiency-critical applications. If threshold is higher: passive diode remains coordination optimum for broader range of systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voltage_drop_threshold, empirical, 'Voltage drop threshold where passive coordination cost exceeds active switching benefit').

omega_variable(
    ideal_diode_displacement,
    'Do ideal diode controllers (MOSFET-based active rectification with near-zero forward drop) displace the blocking diode pattern, or do they introduce new failure modes that preserve the passive solution''s niche?',
    'Longitudinal tracking of off-grid system designs: adoption rates of ideal diode controllers vs traditional blocking diodes; failure mode analysis (firmware bugs, MOSFET gate drive failures, EMI susceptibility)',
    'If ideal diodes displace blocking diodes without new failure modes: the passive coordination mechanism sunsets (becomes scaffold). If new failure modes emerge: blocking diode remains rope for reliability-critical applications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideal_diode_displacement, empirical, 'Whether ideal diode controllers displace blocking diodes or introduce offsetting failure modes').

omega_variable(
    tacit_knowledge_barrier,
    'Is the blocking diode pattern''s simplicity accessible to non-experts, or does it require tacit knowledge (diode selection, heat sinking, reverse voltage rating) that creates a hidden coordination cost?',
    'Survey of DIY off-grid installations: success rates of blocking diode implementations by installer experience level; failure analysis of undersized or incorrectly rated diodes',
    'If tacit knowledge barrier is low: rope classification confirmed across all power levels. If barrier is significant: the mechanism may be rope for experts but tangled_rope for novices (coordination function exists but access requires specialized knowledge).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_barrier, empirical, 'Whether blocking diode implementation requires tacit knowledge that limits accessibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(solar_integration_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(solar_integ_tr_t0, solar_integration_mechanism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(solar_integ_tr_t3, solar_integration_mechanism, theater_ratio, 3, 0.05).
narrative_ontology:measurement(solar_integ_tr_t6, solar_integration_mechanism, theater_ratio, 6, 0.05).
narrative_ontology:measurement(solar_integ_tr_t10, solar_integration_mechanism, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(solar_integ_be_t0, solar_integration_mechanism, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(solar_integ_be_t3, solar_integration_mechanism, base_extractiveness, 3, 0.08).
narrative_ontology:measurement(solar_integ_be_t6, solar_integration_mechanism, base_extractiveness, 6, 0.08).
narrative_ontology:measurement(solar_integ_be_t10, solar_integration_mechanism, base_extractiveness, 10, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(solar_integration_mechanism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(solar_integration_mechanism, information_standard).

% DUAL FORMULATION NOTE:
% The blocking diode mechanism is a single constraint with a single extractiveness value. It does not decompose into multiple constraints because the observable (AC power consumption during solar-active state, diode bias state vs solar voltage) is stable across measurement contexts. Alternative integration methods (active switching, hybrid inverters) are different constraints with their own extractiveness profiles, not alternative observables of the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
