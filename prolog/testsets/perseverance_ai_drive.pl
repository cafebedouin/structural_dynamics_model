% ============================================================================
% CONSTRAINT STORY: perseverance_ai_drive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perseverance_ai_drive, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perseverance_ai_drive
 *   human_readable: AI-Driven Martian Rover Autonomy
 *   domain: technological/space_exploration
 *
 * SUMMARY:
 *   NASA's Perseverance rover autonomously planned and executed a 200-foot
 *   drive on Mars, marking a major milestone in remote robotic exploration.
 *   This achievement demonstrates a pure coordination mechanism: the 20+
 *   minute light-delay between Earth and Mars makes real-time remote control
 *   impossible, forcing a structural choice between limited pre-planned
 *   routes (low capability) or autonomous decision-making (high capability).
 *   The constraint is the latency itself — unavoidable by physics — and the
 *   solution (AI autonomy) solves a genuine coordination problem without
 *   extracting value from any participant. All stakeholders benefit: mission
 *   planners achieve exploration objectives, future programs inherit
 *   validated autonomy infrastructure, algorithm researchers gain
 *   field-tested benchmarks, and commercial robotics industries reduce
 *   technical risk. The theater ratio remains low because the system
 *   prioritizes function over performance: autonomy evaluation focuses on
 *   mission success metrics (distance covered, hazard avoidance accuracy)
 *   rather than ritual compliance or institutional reputation maintenance.
 *
 * KEY AGENTS:
 *   - NASA Mission Control: Institutional beneficiary (immediate horizon, arbitrage exit) — gains capability to execute complex exploration missions despite communication latency
 *   - Mission Planners: Institutional beneficiary (immediate horizon, arbitrage exit) — can delegate real-time navigation decisions to autonomous systems
 *   - Future Exploration Programs: Organized beneficiaries (generational horizon, mobile exit) — inherit validated autonomy standards and reduce R&D costs
 *   - AI Algorithm Community: Institutional beneficiary (biographical horizon, arbitrage exit) — generate performance data and field-tested methodologies
 *   - Commercial Space Industry: Powerful beneficiary (biographical horizon, mobile exit) — reduce development risk for autonomous systems in other domains
 *   - Analytical Observer: Universal perspective (civilizational horizon, analytical exit) — sees constraint as solution to unavoidable latency coordination problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perseverance_ai_drive, 0.28).
domain_priors:suppression_score(perseverance_ai_drive, 0.42).
domain_priors:theater_ratio(perseverance_ai_drive, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perseverance_ai_drive, extractiveness, 0.28).
narrative_ontology:constraint_metric(perseverance_ai_drive, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(perseverance_ai_drive, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perseverance_ai_drive, rope).
narrative_ontology:human_readable(perseverance_ai_drive, "AI-Driven Martian Rover Autonomy").
narrative_ontology:topic_domain(perseverance_ai_drive, "technological/space_exploration").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, mission_objectives).
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, human_mission_planners).
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, future_exploration_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NASA MISSION CONTROL (ROPE) — Institutional beneficiary. AI autonomy solves the fundamental coordination problem of 20+ minute light-delay communication: planners cannot command the rover in real-time, so autonomous navigation enables mission objectives. No meaningful extraction — the system aligns incentives between rover capability and mission success. Exit options are arbitrage: NASA can choose alternative rovers or suspend missions, but AI autonomy is increasingly attractive relative to alternatives.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE EXPLORATION PROGRAMS (ROPE) — Organized beneficiaries across multiple agencies and private entities. AI autonomy becomes a coordination standard enabling interoperable rover missions, payload design, and collaborative exploration architecture. The constraint is purely coordinative: all parties benefit from standardized autonomy capabilities. Mobile exit options (programs can modify or abandon autonomy strategies), but coordination benefits exceed costs.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: AI ALGORITHM DEVELOPMENT COMMUNITY (ROPE) — Institutional beneficiary. Perseverance's autonomous drive generates validated performance data, benchmarks, and field-tested pathfinding algorithms. This feeds back into academic research and commercial AI systems. Low suppression (researchers can publish, collaborate, or pursue alternative platforms). Arbitrage exit (algorithms can be applied to other robotic domains). Pure coordination: knowledge standard for autonomous navigation in high-latency environments.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL ROBOTICS INDUSTRY (ROPE) — Powerful agent that benefits from NASA's autonomy R&D and validation. The Perseverance demonstration reduces technical risk and development costs for commercial autonomous systems (lunar landers, asteroid miners, deep-ocean rovers). No extraction — spillover effects are genuine public goods. Mobile exit options (industry can develop alternative autonomy approaches) but the NASA validation is valuable coordination infrastructure.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational view, AI autonomy for rovers is a pure coordination mechanism solving the unavoidable latency constraint of planetary exploration. The 20+ minute light-delay creates a fundamental coordination problem that only autonomous decision-making can solve. No agent extracts value from others — all benefit from capability that would be impossible without AI. This is coordination infrastructure, not an extraction system. The constraint is a natural solution to a structural problem.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perseverance_ai_drive_tests).
:- end_tests(perseverance_ai_drive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint's base extractiveness reflects minimal asymmetric value capture. NASA benefits from the autonomy capability, but all parties benefit from the same solution (more autonomous rovers enable better exploration). There is no extraction flow — no agent gains relative advantage from others' constrained options. The value is created by solving the latency coordination problem, not redistributed from one party to another. Theater ratio (0.35): Low. Perseverance's autonomy system emphasizes functional performance (hazard detection, path planning accuracy) over ritual or institutional signaling. Success is measured in mission outcomes (scientific data, distance covered, obstacle avoidance), not in review cycles or publication cycles. The modest theater increase (0.25 to 0.35 over the interval) reflects minor institutional reporting overhead, not a shift toward performative goals. Suppression (0.42): Moderate. Suppression exists in the form of barriers to entry for alternative autonomous systems: NASA's R&D investment, proprietary algorithm libraries, classified safety protocols, and institutional lock-in to existing rover architectures create meaningful barriers. However, suppression is not coercive — academic groups and commercial entities can develop alternative autonomy systems. Exit options are mobile (other agencies can build different rovers) rather than trapped.
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify this constraint as Rope because the underlying structure is genuinely coordinative. The latency constraint creates a coordination problem that only autonomous AI can solve. No agent experiences this as extraction because the solution benefits everyone symmetrically. The analytical observer's civilizational view confirms this — from a universal perspective, AI autonomy is infrastructure enabling exploration that would be impossible otherwise. The perspectival gaps are minimal because there are no asymmetric beneficiaries or victims. If gaps were to emerge, they would come from second-order effects: proprietary algorithm enclosure, standardization lock-in, or failure propagation creating institutional liability shifts. But the primary constraint remains coordinative.
 *
 * MANDATROPHY ANALYSIS:
 *   PURE COORDINATION EXEMPLAR: This constraint resolves the mandatrophy by presenting as genuinely pure coordination across all perspectives. There is no hidden extraction being masked as coordination, and no coordination function being masked as extraction. The latency constraint is real and unavoidable; the autonomy solution is objectively better than the alternative (pre-planned routes); and all parties benefit. The coordination is not enforced by suppression of alternatives — it is chosen because it works better. The theater ratio is low because the system optimizes for function, not institutional ritual. This is a rope constraint without the risk of degradation into tangled rope or snare — provided that future institutional decisions (algorithm licensing, standards lock-in, failure liability) do not introduce asymmetric extraction. The omegas track these potential shifts, but the current constraint is robustly coordinative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_failure_propagation,
    'If autonomous navigation fails catastrophically on Mars, does this constraint shift from Rope (coordination) to Snare (extraction of safety risk onto future missions)?',
    'Historical analysis of autonomous system failures in remote robotics; assessment of whether mission failures create institutional inertia or genuine learning',
    'If failures are absorbed as learning: Rope classification holds. If failures produce regulatory capture or liability shifting: constraint becomes Snare from some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_failure_propagation, empirical, 'Whether autonomous system failures create coordination learning or extraction of risk').

omega_variable(
    algorithm_proprietary_enclosure,
    'Will NASA''s autonomy algorithms remain open-source knowledge infrastructure, or will commercial licensing and IP protection convert the coordination benefit into extraction?',
    'Tracking of algorithm publication practices, open-source contributions, and commercial licensing terms; comparison with historical NASA technology transfer models',
    'If open: Rope classification is robust. If proprietary enclosure occurs: constraint becomes Tangled Rope (coordination function + asymmetric IP extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_proprietary_enclosure, preference, 'Whether autonomy algorithms remain open or become proprietary').

omega_variable(
    standards_lock_in_risk,
    'Does standardization of Mars rover autonomy create path-dependent lock-in that prevents superior alternative approaches from emerging?',
    'Assessment of standardization processes; identification of alternative architectures (centralized vs distributed, symbolic vs neural-network based); evaluation of lock-in costs vs coordination benefits',
    'If lock-in minimal: Rope holds. If standardization prevents beneficial alternatives: constraint becomes Tangled Rope (coordination structure + asymmetric cost of innovation constraints).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standards_lock_in_risk, conceptual, 'Whether standardization creates beneficial coordination or problematic path-dependent lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perseverance_ai_drive, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(persev_tr_t0, perseverance_ai_drive, theater_ratio, 0, 0.25).
narrative_ontology:measurement(persev_tr_t3, perseverance_ai_drive, theater_ratio, 3, 0.32).
narrative_ontology:measurement(persev_tr_t6, perseverance_ai_drive, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(persev_be_t0, perseverance_ai_drive, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(persev_be_t3, perseverance_ai_drive, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(persev_be_t6, perseverance_ai_drive, base_extractiveness, 6, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perseverance_ai_drive, global_infrastructure).
narrative_ontology:affects_constraint(perseverance_ai_drive, lunar_rover_autonomy).
narrative_ontology:affects_constraint(perseverance_ai_drive, asteroid_mining_autonomy).
narrative_ontology:affects_constraint(perseverance_ai_drive, autonomous_spacecraft_navigation).

% DUAL FORMULATION NOTE:
% Perseverance's autonomy is downstream of fundamental latency constraints in space communication (unavoidable light-delay physics). The constraint story focuses on the institutional solution structure, not on the physics. If decomposition were required, the latency physics would be a separate Mountain constraint; Perseverance's institutional response would be a separate Rope constraint. However, they are not separately analyzed here because the institutional solution is inseparable from the physical constraint it addresses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
