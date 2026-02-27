% ============================================================================
% CONSTRAINT STORY: sts86_ascent_checklist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Space Shuttle Ascent/Abort Procedural Matrix (JSC-48005)
 *   domain: aerospace_engineering/institutional_safety
 *
 * SUMMARY:
 *   The Space Shuttle Ascent Checklist (JSC-48005) is the definitive
 *   procedural constraint governing vehicle preparation, launch sequence, and
 *   abort decision logic for the Space Transportation System. Spanning
 *   T-minus 5 minutes through Main Engine Cutoff, the checklist encodes the
 *   irreducible sequence of system verifications, propellant loading
 *   sequences, aerodynamic envelope constraints, and emergency response
 *   procedures. The constraint exhibits characteristics of natural law: it
 *   cannot be negotiated, alternatives are inaccessible, and deviation
 *   triggers either hardware interlocks or mission failure. Extractiveness is
 *   minimal (0.08) because no agent benefits asymmetrically from the
 *   constraint; it distributes risk symmetrically across crew and mission
 *   stakeholders. Theater ratio is low (0.15) because the checklist is
 *   predominantly functional — each item serves a direct purpose in
 *   maintaining vehicle integrity and crew safety. The constraint's
 *   accessibility collapse (0.92) reflects that the checklist represents the
 *   only viable path to orbit; all alternatives (unprepared ascent,
 *   abbreviated procedures, deviation from safety margins) result in
 *   catastrophic failure. The resistance (0.08) is minimal because compliance
 *   is self-enforcing through hardware interlocks and regulatory oversight.
 *
 * KEY AGENTS:
 *   - Flight Crew (Orbiter Commander, Pilot, Mission Specialists): Powerless/trapped — bound by the checklist with zero authorized deviation during ascent
 *   - Mission Control / Flight Directors: Organized/constrained — have abort authority but are bound by the procedural sequence; cannot authorize deviation
 *   - NASA Flight Operations: Institutional/constrained — maintains the checklist, distributes cognitive load, enables coordination between ground and flight
 *   - Aerospace Engineering Community: Analytical observer — analyzes the constraint as emergent from orbital mechanics and human cognitive limits
 *   - Regulatory Agencies (FAA, Congress): Institutional/analytical — oversee safety compliance but do not author the technical checklist
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
narrative_ontology:human_readable(sts86_ascent_checklist, "Space Shuttle Ascent/Abort Procedural Matrix (JSC-48005)").
narrative_ontology:topic_domain(sts86_ascent_checklist, "aerospace_engineering/institutional_safety").

domain_priors:emerges_naturally(sts86_ascent_checklist).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLIGHT CREW (MOUNTAIN) — Pilots and mission specialists are bound by the ascent checklist with zero degrees of freedom during launch sequence. The procedural matrix is experienced as immutable natural law — deviation is physically impossible (systems lock out unauthorized commands) or catastrophic (loss of vehicle and crew). No exit option exists; the constraint emerges from the irreducible requirements of orbital mechanics, propellant sequencing, and fail-safe design. Accessible only through perfect compliance.
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MISSION CONTROL / FLIGHT DIRECTORS (MOUNTAIN) — Mission control has procedural authority but is similarly bound by the checklist matrix. They can delay launch or abort, but they cannot authorize deviation from the sequence without triggering hard engineering constraints. The checklist is generational — it encodes lessons from decades of spaceflight and accident investigations (Apollo 1, Challenger, Columbia). Changing the sequence requires generational timescale evidence and validation. From the flight director's perspective, the constraint is natural law: attempt to skip steps and the shuttle's avionics refuse the command.
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational/universal view of aerospace systems theory, the ascent checklist emerges from irreducible physical constraints: orbital mechanics, thermodynamics, materials limits, and human cognitive capacity during emergency scenarios. The checklist is not policy — it is encoded physics. It cannot be negotiated, traded away, or substituted with alternatives. The accessibility collapse reflects that no meaningful alternative to sequential verification exists; the resistance is minimal because compliance is self-enforcing through hardware interlocks. This constraint exhibits the deepest natural law signature in human-machine systems.
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: NASA FLIGHT OPERATIONS AUTHORITY (ROPE) — At the biographical/national level, NASA Flight Operations sees the checklist as coordination mechanism: it solves the collective action problem of sequencing hundreds of interdependent systems and verifying their state. The institutional actor experiences the checklist as enabling rather than constraining — it provides clarity, reduces communication overhead, and distributes cognitive load. This perspective recognizes that without the procedural matrix, coordination between launch control, mission control, and crew would be impossible. From this view, the constraint is a pure coordination tool (Rope), not an extractive mechanism.
constraint_indexing:constraint_classification(sts86_ascent_checklist, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sts86_ascent_checklist_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sts86_ascent_checklist, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sts86_ascent_checklist, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

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
 *   Extractiveness (0.08): Minimal. No agent benefits asymmetrically from the checklist. The flight crew, mission control, and NASA all share identical safety incentives — successful ascent and crew return. The checklist distributes work (cognitive load, verification tasks) but does not extract value from one party to another. The low extractiveness is the signature of a pure coordination mechanism (or in this case, a mountain that enables coordination). Suppression (0.02): Negligible. The checklist does not suppress alternatives through coercion — alternatives are inaccessible because they are physically infeasible. There is no coercive apparatus; there is only irreducible physics and engineering. Theater ratio (0.15): Low. The checklist is predominantly functional. Each item — cryogenic loading sequences, main engine start checks, flight control system verifications — serves a direct purpose. Some procedural elements (standardized call-out formats, crew briefings) have a small theatrical component for redundancy and error-catching, but the majority is technical substance. Accessibility collapse (0.92): High. The checklist represents the only viable ascent path. All alternatives (abbreviated procedures, parallel sequencing, deviation from thermal/propellant margins) result in vehicle loss. The accessibility threshold for entering orbit without the checklist is effectively infinite — it is not a choice available to any agent. Resistance (0.08): Low. The checklist is self-enforcing through avionics interlocks, propellant system sequencing, and hardware design. No external coercive force is required to maintain compliance; the vehicle's systems refuse the alternative.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on the mountain classification, but they experience the constraint from different structural positions. The flight crew experiences the checklist as immutable law imposed from above (powerless/trapped). Mission control experiences it as both constraint and tool — they maintain procedural authority but are themselves bound by the same sequence (organized/constrained). NASA Flight Operations experiences the checklist as enabling — it solves the coordination problem (institutional/constrained). The analytical observer sees the constraint as emerging from irreducible orbital mechanics and human cognitive limits (analytical/analytical). The convergence across perspectives is the signature of a true mountain: there is no structured perspectival gap because the constraint is invariant to observer position. Even agents with high institutional power cannot authorize deviation; they can only delay or abort.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy (false natural law masquerading as coordination, or vice versa). The mountain classification is validated by the convergence of all perspectives and the minimal theater ratio. The constraint is either natural law or extremely high-quality institutional design — the evidence cannot distinguish, which is the essence of a mountain in human systems. The key omega variables address the boundary: if comparative analysis across space agencies reveals significant procedural divergence, the mountain classification downgrades to institutional scaffolding. If cognitive science analysis shows that the checklist is designed with overcautious margins that exceed human cognitive limits, extractiveness may increase (to maintain procedural control) and classification may shift to tangled_rope. Until those omegas are resolved, the constraint stands as a mountain — an immutable procedural law of spaceflight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hardware_vs_procedure_boundary,
    'Is the checklist constraint inherent to orbital mechanics/thermodynamics (mountain), or is it a contingent institutional procedure (scaffold/rope)?',
    'Comparative analysis across space agencies: do all launch systems converge on structurally identical procedural sequences, or do they vary significantly? Analysis of regulatory vs. engineering origins of each checklist item.',
    'If convergent across agencies: mountain classification is validated. If divergent: the constraint is institutional scaffolding (scaffold/rope), not natural law, and classification downgrades to coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hardware_vs_procedure_boundary, empirical, 'Whether checklist is inherent physical law or contingent institutional procedure').

omega_variable(
    human_cognitive_load_floor,
    'Is the checklist length/complexity driven by irreducible human cognitive limits during emergency response, or by overcautious institutional design?',
    'Cognitive science analysis of pilot decision-making under stress; simulation studies of abbreviated vs. full checklist performance; comparison to commercial aerospace practices (which operate with fewer/shorter checklists).',
    'If cognitive floor is real: mountain classification stands (accessibility collapse remains ≥0.85). If overcautious: extractiveness may increase (institutional actors are preserving complexity to maintain procedural control), reclassifying to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_cognitive_load_floor, empirical, 'Whether checklist complexity is driven by cognitive limits or institutional overcaution').

omega_variable(
    automation_substitution_feasibility,
    'Could automated systems (AI flight envelope monitors, autonomous abort sequencing) fully replace the human-readable checklist without loss of safety margin?',
    'Technical analysis of automation sufficiency for launch abort scenarios; review of automation failures in aviation/aerospace; assessment of regulatory barriers vs. technical barriers to full automation.',
    'If automation is feasible: the checklist is institutional/human-specific rather than universal. Classification may downgrade. If automation is insufficient: mountain classification is confirmed (irreducible to non-human systems).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_substitution_feasibility, conceptual, 'Whether automation could replace human checklist procedures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sts86_ascent_checklist, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sts86_tr_t0, sts86_ascent_checklist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sts86_tr_t30, sts86_ascent_checklist, theater_ratio, 30, 0.15).
narrative_ontology:measurement(sts86_tr_t60, sts86_ascent_checklist, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(sts86_be_t0, sts86_ascent_checklist, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(sts86_be_t30, sts86_ascent_checklist, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(sts86_be_t60, sts86_ascent_checklist, base_extractiveness, 60, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sts86_ascent_checklist, enforcement_mechanism).
narrative_ontology:affects_constraint(sts86_ascent_checklist, challenger_thermal_protection_failure).
narrative_ontology:affects_constraint(sts86_ascent_checklist, columbia_foam_shedding_hazard).

% DUAL FORMULATION NOTE:
% The ascent checklist is downstream of physical constraints (orbital mechanics, materials limits) but upstream of specific failure modes (Challenger O-ring failure, Columbia foam loss). The checklist encodes lessons from accident investigations and serves as the institutional mechanism for preventing known failure modes. Upstream constraints are the irreducible physical laws; downstream constraints are specific failure scenarios that the checklist is designed to prevent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
