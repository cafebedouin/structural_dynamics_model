% ============================================================================
% CONSTRAINT STORY: voltage_regulation_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_voltage_regulation_tradeoff, []).

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
 *   constraint_id: voltage_regulation_tradeoff
 *   human_readable: Voltage Regulation Tradeoff in Off-Grid DC Power Systems
 *   domain: electrical_engineering/power_systems/off_grid_infrastructure
 *
 * SUMMARY:
 *   Off-grid DC power systems face a fundamental coordination problem:
 *   battery voltage varies with state of charge (LiFePO4 cells swing from
 *   ~3.0V at 0% SOC to ~3.65V at 100% SOC, yielding 48-58.4V for a 16S pack),
 *   while downstream loads have finite input voltage tolerance ranges. Two
 *   architectural patterns have emerged to coordinate this mismatch. The
 *   Hammerhead configuration accepts the full battery voltage swing
 *   (typically 48-54V in practice, as systems rarely discharge to 0% SOC),
 *   delivering maximum peak current availability at low SOC and avoiding MPPT
 *   regulation losses, but requiring loads with wide input tolerance. The
 *   Smooth Operator configuration inserts Victron MPPT regulation to maintain
 *   fixed bus voltage (e.g., 41.5V), protecting voltage-sensitive equipment
 *   and simplifying downstream DC-DC converter design, but accepting 2-5%
 *   efficiency losses and reduced peak current availability. This constraint
 *   coordinates the inherent tension between battery electrochemistry, power
 *   electronics specifications, and system efficiency optimization. Neither
 *   configuration extracts from the other — both solve the same underlying
 *   problem with different priority weightings. The constraint is a genuine
 *   coordination mechanism (Rope) rather than an extraction mechanism, as
 *   evidenced by: (1) both configurations have legitimate use cases and
 *   active user communities, (2) exit between configurations is
 *   straightforward (mobile exit options), (3) no party collects rents from
 *   the constraint's operation, and (4) the tradeoff arises from physical
 *   properties of batteries and power electronics rather than from
 *   institutional arrangements.
 *
 * KEY AGENTS:
 *   - Off-Grid System Designer: Moderate power, mobile exit — faces genuine coordination problem between battery chemistry, load requirements, and efficiency optimization; chooses configuration based on application priorities
 *   - Hammerhead Configuration Users: Moderate power, mobile exit — prioritize peak power availability and efficiency; accept voltage swing as chosen tradeoff
 *   - Smooth Operator Configuration Users: Moderate power, mobile exit — prioritize voltage stability and equipment protection; accept regulation losses as chosen tradeoff
 *   - Power Electronics Manufacturers: Institutional power, arbitrage exit — design equipment with input voltage tolerance ranges that serve both configurations; benefit from legitimate product differentiation
 *   - Battery Longevity Optimizers: Moderate power, mobile exit — may prefer Smooth Operator's narrower SOC window if cycle life impact of voltage swing is significant (omega variable)
 *   - Peak Power Users: Moderate power, mobile exit — prefer Hammerhead for maximum current availability during high-load events (inverter startup, compressor cycling)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(voltage_regulation_tradeoff, 0.15).
domain_priors:suppression_score(voltage_regulation_tradeoff, 0.2).
domain_priors:theater_ratio(voltage_regulation_tradeoff, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(voltage_regulation_tradeoff, extractiveness, 0.15).
narrative_ontology:constraint_metric(voltage_regulation_tradeoff, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(voltage_regulation_tradeoff, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(voltage_regulation_tradeoff, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(voltage_regulation_tradeoff, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(voltage_regulation_tradeoff, rope).
narrative_ontology:human_readable(voltage_regulation_tradeoff, "Voltage Regulation Tradeoff in Off-Grid DC Power Systems").
narrative_ontology:topic_domain(voltage_regulation_tradeoff, "electrical_engineering/power_systems/off_grid_infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, hammerhead_configuration_users).
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, battery_longevity_optimizers).
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, peak_power_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, off_grid_system_designer).
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, hammerhead_configuration_user).
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, smooth_operator_configuration_user).
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, power_electronics_manufacturer).
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, battery_longevity_optimizer).
narrative_ontology:constraint_beneficiary(voltage_regulation_tradeoff, peak_power_user).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces genuine coordination problem between battery voltage swing (48-54V with SOC), load input tolerance ranges, and efficiency optimization. Has mobile exit — can choose Hammerhead (accept voltage swing, maximize peak power), Smooth Operator (regulate to fixed voltage, accept efficiency losses), or alternative topologies (AC-coupled, different battery chemistry, hybrid). Benefits from having two viable configuration patterns that solve different use cases. The constraint coordinates rather than constrains.
narrative_ontology:constraint_stakeholder(voltage_regulation_tradeoff, off_grid_system_designer, beneficiary,
    moderate, biographical, mobile, local).

% Accepts 48-54V bus voltage swing to maximize battery utilization and peak current availability at low SOC. Benefits from simpler topology (no MPPT regulation stage), higher efficiency (avoids 2-5% MPPT losses), and full battery capacity utilization. Requires loads with wide input tolerance (e.g., HDPLEX 40-60V range). Mobile exit — can switch to Smooth Operator configuration if voltage-sensitive loads are added or if peak power is less critical than voltage stability.
narrative_ontology:constraint_stakeholder(voltage_regulation_tradeoff, hammerhead_configuration_user, beneficiary,
    moderate, immediate, mobile, local).

% Maintains fixed 41.5V bus via Victron MPPT regulation to protect voltage-sensitive loads and simplify downstream DC-DC converter design. Accepts reduced peak current availability (MPPT current limit) and 2-5% efficiency losses as cost of regulation. Benefits from equipment protection, operational simplicity, and compatibility with narrow-input-range loads. Mobile exit — can switch to Hammerhead if efficiency or peak power becomes more critical than voltage stability.
narrative_ontology:constraint_stakeholder(voltage_regulation_tradeoff, smooth_operator_configuration_user, beneficiary,
    moderate, biographical, mobile, local).

% Designs equipment with input voltage tolerance ranges that accommodate both configurations. Wide-input-range products (e.g., HDPLEX 40-60V) serve Hammerhead users; narrow-input-range products serve Smooth Operator users. Benefits from market serving both camps — the constraint creates legitimate product differentiation rather than artificial lock-in. Arbitrage exit — can design for narrow or wide input ranges based on market demand and manufacturing cost tradeoffs.
narrative_ontology:constraint_stakeholder(voltage_regulation_tradeoff, power_electronics_manufacturer, beneficiary,
    institutional, generational, arbitrage, global).

% May prefer Smooth Operator configuration if Hammerhead's deeper voltage swing (corresponding to wider SOC range utilization) materially affects LiFePO4 cycle life. Benefits from narrower effective SOC window if cycle life impact is significant (omega variable). Mobile exit — can choose configuration based on battery longevity vs peak power priority. If cycle life impact is negligible, this agent collapses into the general Hammerhead or Smooth Operator user categories.
narrative_ontology:constraint_stakeholder(voltage_regulation_tradeoff, battery_longevity_optimizer, beneficiary,
    moderate, biographical, mobile, local).

% Prefers Hammerhead configuration for maximum current availability during high-load events (inverter startup, compressor cycling, power tool operation). Benefits from direct battery connection without MPPT current limiting. Accepts voltage swing as necessary tradeoff for peak power capability. Mobile exit — can switch to Smooth Operator if peak loads are reduced or if battery capacity is increased to reduce SOC swing during peak events.
narrative_ontology:constraint_stakeholder(voltage_regulation_tradeoff, peak_power_user, beneficiary,
    moderate, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the inherent mismatch between battery voltage behavior (varies with state of charge) and load equipment input specifications (finite tolerance ranges). Solves the problem: how do you deliver power from a voltage-varying source to voltage-sensitive loads without either over-volting the loads or under-utilizing the battery capacity?
% TRANSFER_FUNCTION: Transfers electrical power from battery to loads. In Hammerhead configuration: direct transfer with full voltage swing, maximizing peak current and efficiency. In Smooth Operator configuration: regulated transfer via MPPT, trading 2-5% efficiency for voltage stability. No wealth or status transfer between parties — both configurations serve the same function with different tradeoff priorities.
% ABSENT_VOICES: No systematically excluded voices. Both configuration camps are represented in off-grid power system design communities. Equipment manufacturers serve both markets. The constraint does not silence dissent — it accommodates multiple solution patterns.
% DISAPPEARANCE_RATIONALE: If the voltage regulation tradeoff disappeared (e.g., if batteries had perfectly flat voltage curves across SOC, or if all loads had infinite input voltage tolerance), off-grid system designers would rearrange their architectures. MPPT regulation stages would become unnecessary. Equipment manufacturers would redesign products. The constraint's disappearance would eliminate a genuine coordination problem, not merely remove a label. The world depends on this constraint to coordinate battery electrochemistry with load requirements.
% FOUNDING_PROBLEM: The founding problem is the inherent voltage variance of battery chemistries (particularly lithium-ion and LiFePO4) across state of charge, combined with the finite input voltage tolerance ranges of power electronics and loads. This problem emerged with the adoption of lithium battery chemistries in off-grid systems (2000s-2010s) — earlier lead-acid systems had narrower voltage swings and different coordination requirements. The problem is: how do you extract maximum energy from a battery whose voltage varies by 15-20% across its discharge curve, while protecting loads that may tolerate only ±10% voltage variation?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by: (1) battery manufacturers' voltage-vs-SOC discharge curves (public datasheets from CATL, LG Chem, BYD, etc. showing 15-20% voltage swing for lithium chemistries), (2) power electronics manufacturers' input voltage specifications (e.g., HDPLEX, MeanWell, Vicor datasheets specifying input tolerance ranges), (3) off-grid system design literature (Solar Energy International textbooks, Victron Energy application notes, DIY off-grid forums documenting the tradeoff), and (4) independent engineering analysis (IEEE papers on DC microgrid voltage regulation, NREL reports on off-grid system optimization). Corroboration comes from battery manufacturers (neutral — they sell to both configurations), load equipment manufacturers (neutral — they design for both), and independent engineering sources (analytical observers with no stake in configuration choice).
narrative_ontology:disappearance_verdict(voltage_regulation_tradeoff, world_rearranges).
narrative_ontology:founding_problem_status(voltage_regulation_tradeoff, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OFF-GRID SYSTEM DESIGNER (ROPE) — Faces genuine coordination problem: battery chemistry dictates voltage swing, load equipment has input tolerance ranges, and MPPT regulation has efficiency costs. The tradeoff coordinates these constraints. Designer has mobile exit (can choose alternative topologies, different battery chemistries, or AC-coupled systems). Experiences low extraction — the constraint solves a real problem with minimal coercive overhead.
constraint_indexing:constraint_classification(voltage_regulation_tradeoff, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: HAMMERHEAD USER (ROPE) — Accepts 48-54V swing to maximize battery utilization and available peak current at low SOC. Benefits from simpler topology (no MPPT regulation losses), higher peak power availability, and full battery capacity utilization. Mobile exit — can switch to Smooth Operator configuration or alternative power architectures. Low extraction — the voltage swing is a chosen tradeoff for performance benefits, not an imposed cost.
constraint_indexing:constraint_classification(voltage_regulation_tradeoff, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: SMOOTH OPERATOR USER (ROPE) — Maintains fixed 41.5V via Victron MPPT regulation to protect voltage-sensitive loads and simplify downstream DC-DC converter design. Accepts reduced peak current availability and MPPT efficiency losses (typically 2-5%) as cost of regulation. Mobile exit — can switch to Hammerhead or hybrid configurations. Low extraction — the regulation cost is a chosen tradeoff for equipment protection and operational simplicity.
constraint_indexing:constraint_classification(voltage_regulation_tradeoff, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: POWER ELECTRONICS MANUFACTURER (ROPE) — Designs equipment with input voltage tolerance ranges (e.g., HDPLEX 40-60V input range) that accommodate both configurations. Benefits from market serving both camps. Arbitrage exit — can design for narrow or wide input ranges based on market demand. Minimal extraction — the constraint creates legitimate product differentiation rather than artificial lock-in.
constraint_indexing:constraint_classification(voltage_regulation_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — The voltage regulation tradeoff is a genuine coordination problem arising from the intersection of battery electrochemistry (voltage varies with SOC), power electronics input specifications (finite tolerance ranges), and system efficiency optimization (regulation has losses). No party is extracting from another — both configurations solve the same underlying problem with different priority weightings (peak power vs voltage stability). The constraint coordinates legitimate engineering tradeoffs rather than enabling extraction.
constraint_indexing:constraint_classification(voltage_regulation_tradeoff, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(voltage_regulation_tradeoff_tests).
:- end_tests(voltage_regulation_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The constraint imposes minimal extraction. Both configurations solve a genuine coordination problem (battery voltage variance vs load input requirements) with different tradeoff priorities. The efficiency penalty of MPPT regulation (2-5%) is a real cost, but it is a chosen cost for voltage stability benefits, not an imposed extraction. The Hammerhead configuration's voltage swing is similarly a chosen tradeoff for peak power benefits. No party is extracting rents — manufacturers serve both configurations, users can switch freely, and the constraint arises from physical properties rather than institutional arrangements. The slight extractiveness reflects only the inherent efficiency losses in any power conversion topology. Suppression (0.20): Low. Alternatives are readily available: AC-coupled systems, different battery chemistries (e.g., lead-acid with narrower voltage swing), hybrid configurations, or simply choosing the other configuration. Exit costs are moderate (equipment replacement, system reconfiguration) but not prohibitive. No institutional barriers prevent switching. Theater ratio (0.10): Very low. The constraint is functional, not performative. Voltage regulation serves a real purpose (protecting equipment, enabling peak power, or optimizing efficiency depending on configuration). There is minimal ritual or ceremony — the engineering tradeoff is straightforward and well-understood. Accessibility collapse (0.35): Low-moderate. Once the tradeoff is understood, some alternatives do collapse (e.g., attempting to run voltage-sensitive loads directly from battery without regulation, or attempting to achieve both maximum peak power and perfect voltage stability simultaneously), but many alternatives remain viable (different configurations, different battery chemistries, AC coupling, hybrid approaches). Resistance (0.25): Low. The constraint meets minimal resistance because it coordinates a genuine problem. Some users resist the efficiency losses of MPPT regulation (preferring Hammerhead), others resist voltage swing (preferring Smooth Operator), but this is preference-based selection among viable alternatives rather than resistance to an imposed constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all five perspectives classify as Rope. The off-grid system designer, both configuration users, the manufacturer, and the analytical observer all see the same structure: a genuine coordination problem with two viable solution patterns, minimal extraction, and mobile exit options. The uniformity of classification reflects the constraint's nature as pure coordination rather than extraction. The slight variation in experienced extraction (Hammerhead users experience slightly lower chi due to avoiding MPPT losses; Smooth Operator users experience slightly higher chi due to accepting those losses) does not change the type classification — both remain well within Rope territory. The omega variables identify the empirical uncertainties (MPPT efficiency magnitude, battery cycle life impact, load equipment failure correlation) that could shift the tradeoff calculus but are unlikely to change the fundamental Rope classification unless one configuration is revealed to impose significant hidden costs.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this constraint are beneficiaries or neutral parties — there are no victims. The off-grid system designer benefits from having two viable configuration patterns that solve different use cases. Hammerhead users benefit from peak power availability and efficiency. Smooth Operator users benefit from voltage stability and equipment protection. Power electronics manufacturers benefit from serving both markets. Battery longevity optimizers and peak power users are specialized beneficiary subgroups. The constraint's low extractiveness reflects that it coordinates rather than extracts — the directionality values for all agents are low (near the beneficiary end of the spectrum), producing low or negative effective extraction chi. The engine will derive d values from beneficiary declarations and mobile/arbitrage exit options, yielding chi values that confirm the Rope classification across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all constraints involve extraction. The voltage regulation tradeoff is a coordination mechanism that enables off-grid power systems to function despite the inherent mismatch between battery voltage behavior and load input requirements. Both configurations coordinate this mismatch successfully with different priority weightings. There is no mandate that has outlived its function — the constraint remains necessary as long as batteries exhibit voltage variance and loads have finite input tolerance. The low theater ratio confirms that the constraint is functional rather than performative. The absence of victims and the presence of multiple beneficiary groups confirm that the constraint coordinates rather than extracts. This is a canonical Rope: a genuine collective-action problem solved with minimal coercive overhead, where participants are net beneficiaries and alternatives are not suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mppt_efficiency_loss_magnitude,
    'What is the true efficiency penalty of Victron MPPT regulation in the Smooth Operator configuration across realistic load profiles?',
    'Empirical measurement of round-trip efficiency (solar → battery → MPPT → load) vs direct battery-to-load efficiency in Hammerhead configuration, measured across representative daily load cycles and seasonal solar availability patterns',
    'If MPPT losses < 2%: Smooth Operator cost is negligible, configuration choice is purely about voltage tolerance preferences. If MPPT losses > 5%: Smooth Operator pays significant efficiency penalty, strengthening the case that Hammerhead''s voltage swing is a worthwhile tradeoff for users who can tolerate it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mppt_efficiency_loss_magnitude, empirical, 'Magnitude of MPPT regulation efficiency penalty').

omega_variable(
    battery_cycle_life_impact,
    'Does the Hammerhead configuration''s deeper voltage swing (48-54V, corresponding to wider SOC range utilization) materially affect LiFePO4 battery cycle life compared to Smooth Operator''s narrower effective SOC window?',
    'Long-term cycle testing of LiFePO4 cells under both voltage profiles; comparison of capacity fade rates and internal resistance growth over 2000+ cycles',
    'If cycle life impact is significant (>10% reduction): Hammerhead''s peak power advantage comes at battery longevity cost, shifting the tradeoff calculus. If cycle life impact is negligible: the voltage swing is purely a coordination choice with no hidden degradation cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(battery_cycle_life_impact, empirical, 'Battery longevity impact of voltage swing magnitude').

omega_variable(
    load_equipment_failure_correlation,
    'Do voltage-sensitive loads (computing equipment, LED drivers, telecommunications gear) exhibit higher failure rates or reduced lifespan under Hammerhead''s 48-54V swing compared to Smooth Operator''s regulated 41.5V?',
    'Field failure rate analysis across deployed systems; controlled testing of representative loads under both voltage profiles; manufacturer warranty claim correlation with bus voltage variance',
    'If failure correlation exists: Hammerhead imposes hidden costs (equipment replacement, downtime) that shift the tradeoff toward Smooth Operator for reliability-critical applications. If no correlation: the voltage swing is within equipment design margins and the tradeoff is purely about peak power vs regulation losses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(load_equipment_failure_correlation, empirical, 'Load equipment reliability under voltage swing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(voltage_regulation_tradeoff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vreg_tr_t0, voltage_regulation_tradeoff, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vreg_tr_t3, voltage_regulation_tradeoff, theater_ratio, 3, 0.1).
narrative_ontology:measurement(vreg_tr_t6, voltage_regulation_tradeoff, theater_ratio, 6, 0.1).
narrative_ontology:measurement(vreg_tr_t10, voltage_regulation_tradeoff, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vreg_be_t0, voltage_regulation_tradeoff, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(vreg_be_t3, voltage_regulation_tradeoff, base_extractiveness, 3, 0.14).
narrative_ontology:measurement(vreg_be_t6, voltage_regulation_tradeoff, base_extractiveness, 6, 0.15).
narrative_ontology:measurement(vreg_be_t10, voltage_regulation_tradeoff, base_extractiveness, 10, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(voltage_regulation_tradeoff, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(voltage_regulation_tradeoff, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is self-contained within off-grid DC power system design. It does not decompose into multiple constraints with different epsilon values — the voltage regulation tradeoff is a single structural phenomenon with a single extractiveness value regardless of which configuration is chosen or which observable is measured (HDPLEX input voltage variance, available peak current, MPPT efficiency losses, or battery cycle life all measure the same underlying tradeoff).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
