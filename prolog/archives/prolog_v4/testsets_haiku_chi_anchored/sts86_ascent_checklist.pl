% ============================================================================
% CONSTRAINT STORY: sts86_ascent_checklist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sts86_ascent_checklist, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sts86_ascent_checklist
 *   human_readable: Space Shuttle Ascent/Abort Procedural Matrix
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   The Space Shuttle ascent checklist (JSC-48005) represents the ultimate
 *   procedural constraint in human spaceflight: a set of mandatory decision
 *   points, abort criteria, and system checks that bind crew, mission
 *   control, and vehicle into a single executable sequence. The constraint
 *   appears to be purely institutional — NASA's procedure, codified by
 *   engineers, enforced by training and authority. Yet structural analysis
 *   reveals that the checklist is fundamentally a transcription of natural
 *   law. The ascent profile is constrained by gravity, atmospheric density,
 *   structural load limits, fuel consumption law, and orbital mechanics. The
 *   checklist records where these physical constraints intersect with human
 *   decision-making: the points at which ignoring the procedure would violate
 *   physics, kill the crew, or waste the vehicle. The checklist's authority
 *   is neither arbitrary nor merely institutional — it is the authority of
 *   nature expressed through procedural language.
 *
 * KEY AGENTS:
 *   - Flight Crew (Pilot, Mission Specialists): Powerless/trapped within immediate flight envelope — must execute checklist without physically-safe alternatives
 *   - Mission Control / Flight Dynamics: Organized actors with constrained exit — can modify ascent profile only within physics-defined bounds
 *   - NASA / Flight Safety Authority: Institutional actor with design arbitrage — could build different vehicle but for STS, checklist constraints are immutable
 *   - Analytical Observer: Civilizational perspective recognizing orbital mechanics as foundation — sees checklist as phenomenological record of natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sts86_ascent_checklist, 0.08).
domain_priors:suppression_score(sts86_ascent_checklist, 0.02).
domain_priors:theater_ratio(sts86_ascent_checklist, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sts86_ascent_checklist, extractiveness, 0.08).
narrative_ontology:constraint_metric(sts86_ascent_checklist, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sts86_ascent_checklist, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sts86_ascent_checklist, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(sts86_ascent_checklist, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sts86_ascent_checklist, mountain).
narrative_ontology:human_readable(sts86_ascent_checklist, "Space Shuttle Ascent/Abort Procedural Matrix").
narrative_ontology:topic_domain(sts86_ascent_checklist, "technological/institutional").

domain_priors:emerges_naturally(sts86_ascent_checklist).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLIGHT CREW (MOUNTAIN) — Pilots and mission specialists are bound by physics and thermodynamic reality. The checklist encodes non-negotiable constraints: Mach envelope, structural loads, fuel consumption law, engine ignition sequencing, main engine cutoff timing. No exit option exists within the flight envelope. The crew cannot override the constraint without violating the laws of orbital mechanics. d≈0.85, f(d)≈1.15, σ=0.8 → χ≈0.09 (low effective extraction because the constraint is impersonal natural law, not institutional asymmetry).
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MISSION CONTROL / FLIGHT DYNAMICS (MOUNTAIN) — The ascent profile is constrained by physics: gravity losses, aerodynamic heating, structural design limits, fuel management, abort mode boundaries. These are not institutional choices but natural law. Mission Control has constrained exit — they can modify the ascent profile only within narrow bounds defined by vehicle design and atmospheric properties. The checklist records these boundaries. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.06.
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NASA / FLIGHT SAFETY (MOUNTAIN) — The checklist (JSC-48005) codifies irreducible physical constraints discovered through design iteration, testing, and flight history. NASA has design choice (arbitrage) — they could build a different vehicle with different constraints — but for THIS vehicle, the checklist constraints are immutable. The authority sees the checklist as a record of natural law, not a policy lever. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01 (institutional beneficiary perspective; the constraint enables their authority).
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / ORBITAL MECHANICS (MOUNTAIN) — From the perspective of orbital mechanics and thermodynamics, the ascent profile is governed by Kepler's laws, rocket equation, atmospheric density model, vehicle design load limits, and fuel consumption. These are mathematical and physical necessities, not institutional choices. The checklist is a phenomenological summary of these laws applied to a specific vehicle. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sts86_ascent_checklist_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sts86_ascent_checklist, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sts86_ascent_checklist, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sts86_ascent_checklist, ExtMetricName, E),
    domain_priors:suppression_score(sts86_ascent_checklist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sts86_ascent_checklist),
    narrative_ontology:constraint_metric(sts86_ascent_checklist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sts86_ascent_checklist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sts86_ascent_checklist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The checklist does not extract value from crew or mission control. It constrains both equally, applying force from natural law rather than institutional asymmetry. The constraint is impersonal — it does not redistribute resources, privilege one agent over another, or create asymmetric burden. Suppression (0.02): Minimal. Alternatives to following the checklist do exist (crash, explode, fail to reach orbit) but they are not suppressed by institutional power — they are physically catastrophic. The crew's exit options are not suppressed by NASA; they are eliminated by physics. Theater ratio (0.15): Low. The checklist has functional content throughout. Call-outs and system checks have direct safety function — they catch failures before they cascade. Abort decision points encode real physical boundaries, not performative ritual. The small theater component (0.15) reflects that some procedural elements are workload management or communication ritual, but the bulk is functional verification. Accessibility collapse (0.92): Very high. The constraint is a mathematical and physical necessity. No amount of institutional power, technological development within the current vehicle design, or creative interpretation can escape the rocket equation, gravity, atmospheric density, or structural limits. Resistance (0.08): Very low. No organized resistance exists to the constraint — crew, mission control, and NASA all accept it as natural law. Unlike institutional constraints that may face pushback, the ascent checklist faces zero resistance because its authority comes from physics, not politics.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on Mountain classification, confirming that the constraint is uniform across observables. The flight crew, mission control, NASA, and the analytical observer all see the same fundamental reality: the checklist records irreducible physical constraints. There is no perspectival gap (no disagreement about type) because the constraint is not institutional — it is natural law. The absence of perspectival conflict is itself diagnostic: Mountain constraints should show uniformity across all reasonable observables. The crew cannot reframe the ascent profile as coordination (Rope) because the boundaries are physics-determined, not consensus-based. Mission control cannot see it as temporary (Scaffold) because the structural limits do not sunset. NASA cannot experience extraction (Snare) because the constraint does not privilege institutional power over physical reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Flight Crew: d≈0.85 (high). Powerless within the immediate flight envelope; no arbitrage, no exit. But d is not derived from institutional targeting — it is structural position relative to immutable natural law. All agents occupy similarly constrained positions; the crew is not uniquely victimized. Mission Control: d≈0.55 (symmetric). Constrained exit; can modify ascent profile only within physics bounds. The constraint is impersonal — affects mission control and crew symmetrically. NASA: d≈0.05 (institutional beneficiary position). NASA has design arbitrage at the vehicle level. For the STS vehicle, they accept the constraints as natural law. The authority to manage the checklist is an institutional function (beneficiary position), but this does not create extraction because the constraint itself is not extractive. Analytical Observer: d≈0.72 (analytical perspective on structural reality). Views the constraint as mathematical necessity — no beneficiary or victim, only lawful structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is Mountain-only and thus avoids mandatrophy entirely. The base metrics (ε=0.08, suppression=0.02) are well below the Snare threshold (ε≥0.46, suppression≥0.60). The constraint does not create extractive asymmetry that could be mislabeled as coordination. There is no beneficiary/victim asymmetry to resolve — the constraint applies impersonal natural law to all agents equally. The checklist is pure coordination in service of safety, not coordination masking extraction. The unified Mountain classification across all perspectives confirms that no mislabeling risk exists. The small theater component (0.15) reflects procedural workload management but does not rise to the level where Piton gates would activate (theater ≥ 0.70). The constraint is a Mountain without controversy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_vs_physical_boundary,
    'Where does the checklist''s authority transition from encoding physical law to encoding institutional procedure?',
    'Forensic analysis of each checklist item: separation into physical constraints (main engine cutoff velocity, dynamic pressure limits, structural load envelopes) vs procedural choices (call-out sequences, abort decision timing, crew workload scheduling). Items without physical justification are institutional, not natural law.',
    'If 80%+ of items are physical law: Mountain classification is robust. If 40%+ are procedural/institutional: the constraint is a Tangled Rope (coordination via checklist + institutional pressure) misclassified as Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_vs_physical_boundary, empirical, 'Proportion of checklist items that are physical law vs institutional procedure').

omega_variable(
    abort_mode_optimality,
    'Are the defined abort modes (RTLS, TAL, AOA) physical necessities or policy choices?',
    'Comparison of Space Shuttle abort boundaries with other launch vehicles (Soyuz, Falcon 9, SLS). Analysis of whether different abort strategies are physically possible for STS but chosen not to be implemented.',
    'If abort modes are universal physical limits: Mountain confirmed. If other vehicles use different abort strategies for equivalent physics: the constraint is partly institutional (Tangled Rope, not Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abort_mode_optimality, empirical, 'Whether abort modes are physical necessities or institutional choices').

omega_variable(
    crew_discretion_within_bounds,
    'How much discretion do pilots have to deviate from the checklist within the physically-safe envelope?',
    'Review of JSC-48005 authority structure, procedure for in-flight deviations, analysis of historical cases where crews modified checklist execution. Comparison with cockpit authority in military or commercial aviation.',
    'If crews have significant discretion: constraint has institutional overlay (Tangled Rope). If checklist compliance is absolute: constraint approaches pure natural law (Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crew_discretion_within_bounds, empirical, 'Extent of crew discretion to modify checklist execution within safe bounds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sts86_ascent_checklist, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sts86_tr_t0, sts86_ascent_checklist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sts86_tr_t30, sts86_ascent_checklist, theater_ratio, 30, 0.15).
narrative_ontology:measurement(sts86_tr_t60, sts86_ascent_checklist, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(sts86_be_t0, sts86_ascent_checklist, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(sts86_be_t30, sts86_ascent_checklist, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(sts86_be_t60, sts86_ascent_checklist, base_extractiveness, 60, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sts86_ascent_checklist, enforcement_mechanism).
narrative_ontology:affects_constraint(sts86_ascent_checklist, challenger_thermal_protection_failure).
narrative_ontology:affects_constraint(sts86_ascent_checklist, sts_main_engine_cutoff_precision).

% DUAL FORMULATION NOTE:
% The ascent checklist is upstream of specific technical constraints (main engine performance, thermal protection design) that determine abort boundaries. Those constraints have their own ε values reflecting empirical uncertainties; the checklist has ε=0.08 reflecting that it encodes immutable physical law without extractive institutional overlay.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
