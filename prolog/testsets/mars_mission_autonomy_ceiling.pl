% ============================================================================
% CONSTRAINT STORY: mars_mission_autonomy_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mars_mission_autonomy_ceiling, []).

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
 *   constraint_id: mars_mission_autonomy_ceiling
 *   human_readable: Mars Mission Autonomy Ceiling: Communication Lag and Decision Authority
 *   domain: space_operations/human_autonomy
 *
 * SUMMARY:
 *   The autonomy ceiling in Mars missions represents a structural tension
 *   between the physical reality of light-speed communication delay
 *   (22-minute one-way lag) and the institutional choice to enforce
 *   centralized Earth-based decision authority. This constraint operates at
 *   multiple levels: it is simultaneously a physical limit (immutable), a
 *   coordination mechanism (preventing mission-fracturing conflicts), an
 *   extraction mechanism (concentrating decision authority and liability
 *   protection in Earth-based agencies), a degraded institutional ritual
 *   (safety protocols that provide reassurance without functional
 *   improvement), and a temporary technical constraint being addressed by
 *   autonomous systems research. The constraint's extractiveness (0.58)
 *   reflects that mission control authority captures career and institutional
 *   benefits while Mars crew face accountability for deviations and
 *   operational paralysis during time-critical events. The theater ratio
 *   (0.65) reflects that safety procedures around Earth-Mars command have
 *   grown beyond their functional core — risk documentation and approval
 *   chains provide institutional legitimacy and liability protection rather
 *   than improving actual mission outcomes.
 *
 * KEY AGENTS:
 *   - Mars Crew: Primary victim (powerless/trapped) — cannot execute time-critical decisions; faces career accountability and mission failure risk from communication lag constraints
 *   - Mission Control Authority: Primary beneficiary (institutional/arbitrage) — maintains decision authority, liability protection, and career advancement through control preservation; can delegate if needed
 *   - Mars Operations Team: Secondary victim (moderate/constrained) — experiences both genuine coordination function and extraction through protocol enforcement; faces barriers to requesting authority transfer
 *   - International Space Agencies: Institutional actors (powerful/constrained) — coordination function genuine (unified protocol prevents national conflicts); extraction function real (larger agencies preserve decision authority over smaller partners)
 *   - Autonomous Operations Research Community: Organized agents (organized/mobile) — see autonomy ceiling as temporary technical/policy constraint with visible sunset; advancing AI/ML can justify decentralization
 *   - Mission Safety Protocols: Institutional structure (institutional/arbitrage) — performative apparatus providing reassurance of control; maintained through inertia and liability aversion rather than functional safety improvement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice (centralized control) as physical necessity (communication lag)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mars_mission_autonomy_ceiling, 0.58).
domain_priors:suppression_score(mars_mission_autonomy_ceiling, 0.72).
domain_priors:theater_ratio(mars_mission_autonomy_ceiling, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mars_mission_autonomy_ceiling, extractiveness, 0.58).
narrative_ontology:constraint_metric(mars_mission_autonomy_ceiling, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mars_mission_autonomy_ceiling, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mars_mission_autonomy_ceiling, tangled_rope).
narrative_ontology:human_readable(mars_mission_autonomy_ceiling, "Mars Mission Autonomy Ceiling: Communication Lag and Decision Authority").
narrative_ontology:topic_domain(mars_mission_autonomy_ceiling, "space_operations/human_autonomy").

domain_priors:requires_active_enforcement(mars_mission_autonomy_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mars_mission_autonomy_ceiling, mission_control_authority).
narrative_ontology:constraint_beneficiary(mars_mission_autonomy_ceiling, earth_based_agencies).
narrative_ontology:constraint_victim(mars_mission_autonomy_ceiling, mars_crew_agency).
narrative_ontology:constraint_victim(mars_mission_autonomy_ceiling, mission_objectives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARS CREW (SNARE) — Trapped by distance (22-minute one-way light lag). Cannot execute time-critical decisions without violating protocol. Faces extraction: career accountability for deviations, emergency response delays that may cause mission failure or crew harm. No exit option — the communication physics is immutable. Maximum experienced constraint.
constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARS OPERATIONS TEAM (TANGLED ROPE) — Experiences genuine coordination function: Earth-Mars protocol coordination prevents catastrophic misalignment. Also experiences extraction: protocol enforcement prevents crew from optimizing for local conditions; decision authority concentrated in Earth-based control. Constrained exit — teams could theoretically request authority transfer but face institutional and career barriers.
constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MISSION CONTROL AUTHORITY (ROPE) — Primary beneficiary (institutional/arbitrage). Maintains decision authority and liability protection. Experiences constraint as pure coordination: Earth control communicates mission priorities and safety protocols. Can exit arbitrage if needed — delegation decisions are available. Net beneficiary from current arrangement.
constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL SPACE AGENCIES (TANGLED ROPE) — Coordination function: unified mission protocol prevents conflicting national priorities from fracturing Mars operations. Extraction function: agencies with larger Earth infrastructure (NASA, ESA, Roscosmos) preserve decision authority over smaller partners. Constrained exit — agencies could theoretically build independent Mars programs but face cost barriers and coordination failure risks.
constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: AUTONOMOUS OPERATIONS RESEARCH (SCAFFOLD) — Sees the autonomy ceiling as a temporary technical and policy constraint. AI/ML systems for autonomous Mars operations are advancing rapidly; distributed decision trees and local optimization protocols can reduce Earth dependency. Sunset logic: within 10-15 years, sufficient autonomous capacity may be available to justify decentralized decision authority. Currently constrained by policy and validation requirements, but exit is visible.
constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: MISSION SAFETY PROTOCOLS (PITON) — The procedural apparatus around Earth-Mars command authority has grown substantially beyond its functional core. Risk assessments, contingency hierarchies, and escalation procedures create theater — extensive documentation and approval chains that provide reassurance of control rather than increasing actual safety margins. Theater ratio high because: (1) many procedures are redundant with others, (2) Earth cannot actually respond to emergencies (lag precludes real-time intervention), (3) procedures are maintained for institutional legitimacy rather than functional safety. Piton classification: maintained through inertia and liability aversion, not robust safety function.
constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL LIMIT VIEW (MOUNTAIN) — From a civilizational perspective focused on physics, the light-speed delay (22 minutes one-way) creates an immutable constraint on real-time control. No technology can overcome relativistic communication limits. However, this perspective naturalizes what is actually a policy choice — whether to enforce Earth-centered decision authority or permit local Mars autonomy within delegated parameters. The physical constraint (communication lag) is real; the institutional constraint (centralized control) is contingent.
constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mars_mission_autonomy_ceiling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mars_mission_autonomy_ceiling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mars_mission_autonomy_ceiling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mars_mission_autonomy_ceiling, TR),
    TR >= 0.70.

:- end_tests(mars_mission_autonomy_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Mission control captures institutional and career benefits from centralized authority — preserves liability protection, career advancement opportunities, and prestige through decision control. The crew faces offsetting harms: accountability for deviations, operational paralysis, reduced autonomy. However, the extraction is not maximal because some genuine coordination benefits exist (preventing conflicting national missions), and the gap itself creates legitimate first-mover safety concerns. Measurement trajectory shows increasing extractiveness (0.42→0.58) as international missions have layered more authorization procedures and complexity. Suppression (0.72): High. Barriers to crew autonomy are substantial: communication physics (immutable 44-minute round-trip lag), institutional policy (Earth-centered authority), liability structures (mission control bears formal responsibility), and technological constraints (current autonomous systems cannot match human decision-making for all mission classes). These barriers are not total — crew can execute pre-authorized responses and some local optimization — but they are severe enough that crew cannot effectively override control protocols. Theater ratio (0.65): Moderate-high. Safety procedures have expanded significantly beyond their functional core. Many contingency protocols are redundant; escalation procedures create reassurance theater because Earth cannot actually execute real-time intervention (lag precludes response). Procedures are maintained primarily for institutional legitimacy (boards, oversight, liability protection) rather than functional safety improvement. Unlike piton (theater ≥0.70), the constraint still retains significant coordination function, preventing classification as pure theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon yields different classifications depending on observer position. Mission control sees rope — they are solving the legitimate coordination problem of preventing conflicting national Mars agendas and ensuring mission safety. Crew see snare — they bear maximum constraint without meaningful benefit, facing accountability for response delays and operational paralysis. Operations teams see tangled rope — genuine coordination benefits mixed with extraction through protocol enforcement. Autonomous research community sees scaffold — the constraint is technical/policy temporary with visible sunset as AI autonomy improves. The institutional safety apparatus sees itself as piton — degraded ritual maintained through inertia. The analytical observer risks seeing mountain — naturalizing the policy choice (centralized control) as a physical necessity (communication lag). The perspectival gap reveals that the constraint's type is genuinely observer-relative; no single classification is 'correct' independent of position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position within the constraint. Mission Control (institutional/arbitrage) experiences d ≈ 0.10 (beneficiary with high exit options) producing low effective extraction. Mars crew (powerless/trapped) experience d ≈ 0.95 (victim with no exit) producing maximum effective extraction per f(d). Operations teams (moderate/constrained) experience d ≈ 0.65 (victim with high but surmountable exit costs) producing moderate-high extraction. International agencies (powerful/constrained) experience d ≈ 0.40-0.50 depending on agency size and partnership role — larger agencies have lower d (more benefit from coordination control), smaller agencies higher d (more extraction from authority concentration). The gap between beneficiary d and victim d is the perspectival signal: large gap indicates asymmetric extraction; small gap indicates genuine coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE: The autonomy ceiling resolves apparent mandatrophy (how can this be both coordination and extraction?) by showing that it IS genuinely both. Mission control coordination function is real: Earth-based authority does prevent conflicting national missions and ensures unified safety protocols. Extraction function is also real: the coordination could be achieved through looser authority structures with pre-authorized crew autonomy, but current structure concentrates decision authority and benefits in Earth agencies. The mandatrophy dissolves when you recognize that coordination and extraction are not mutually exclusive — the constraint coordinates while extracting. This is precisely the tangled rope structure. The false summit detector (mountain perspective) correctly flags that the 'physical limit' framing naturalizes a policy choice. The communication lag IS immutable; centralized decision authority is NOT. The analytical observer's error is treating the physical constraint as the entire explanation for the institutional constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_threshold_definition,
    'At what level of autonomous capability does delegation of Earth control become justified rather than reckless?',
    'Comparative analysis of Mars mission failure modes with/without Earth control; validation data from Earth-orbit autonomous operations; AI system reliability metrics correlated with mission-critical decision rates',
    'If threshold is reachable with current tech: scaffold perspective validated; policy change is feasible. If threshold requires breakthrough: piton perspective confirmed; control structure persists through technical necessity, not institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_threshold_definition, empirical, 'Operational threshold for justifying autonomous Mars decision authority').

omega_variable(
    liability_vs_safety_gap,
    'Does Earth-centered decision authority improve actual mission safety outcomes, or primarily reduce institutional liability exposure for mission control?',
    'Regression analysis: mission outcomes correlated with decision lag; comparison of failure rates under full Earth control vs. pre-authorized local autonomy; analysis of historical decisions where Earth authority prevented vs. enabled mission-critical failures',
    'If authority improves safety: snare classification is overstated; tangled rope is more accurate. If authority primarily protects liability: snare classification confirmed; safety theater is driving piton classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liability_vs_safety_gap, empirical, 'Whether Earth control improves safety or primarily reduces institutional liability').

omega_variable(
    political_coordination_necessity,
    'Does centralized Earth control serve genuine international coordination of Mars activities, or is it primarily a legacy of Cold War space competition protocols?',
    'Historical comparison of international Mars cooperation under centralized control vs. decentralized models; analysis of actual conflicts prevented by centralized authority; examination of institutional incentives for maintaining control independent of coordination function',
    'If coordination is genuine: tangled rope classification validated; both extraction and coordination functions are real. If control is vestigial Cold War structure: piton classification shifted; institution maintains authority through path dependence rather than functional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_coordination_necessity, conceptual, 'Whether Earth-centered control serves genuine international coordination or vestigial protocols').

omega_variable(
    crew_identity_lock_vs_external_barriers,
    'To what extent is crew acceptance of Earth control due to internalized hierarchical identity (identity_locked) versus external barriers to delegation (trapped)?',
    'Qualitative analysis of crew interviews pre- and post-mission; examination of crew proposals for expanded autonomy; comparison of crew risk tolerance with actual physical constraints; simulation of expanded authority scenarios to detect identity-based vs. practical objections',
    'If identity_locked dominates: crew perception of constraint as immutable exceeds structural reality; reframing could increase effective agency without physical changes. If trapped dominates: crew perception is accurate; only technical solutions (better autonomous systems) enable real agency expansion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crew_identity_lock_vs_external_barriers, empirical, 'Whether crew constraint is identity-based or materially structural').

omega_variable(
    autonomous_system_failure_mode_parity,
    'Do autonomous Mars decision systems have failure modes symmetrical to or substantially worse than Earth-based control failure modes?',
    'Failure analysis: types of errors produced by centralized Earth control (slow response, incomplete information, policy rigidity) vs. autonomous systems (optimization errors, environmental blindness, goal-misalignment); simulation comparison of outcome distributions',
    'If failure modes are parity or autonomous advantage: scaffold perspective strengthened; autonomous delegation can be justified. If autonomous systems have systemic advantages in specific failure modes: piton theater tag shifts — Earth control persists to avoid asymmetric risks, not to improve outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomous_system_failure_mode_parity, empirical, 'Comparative failure mode analysis of autonomous vs. Earth-controlled Mars operations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mars_mission_autonomy_ceiling, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marsaut_tr_t0, mars_mission_autonomy_ceiling, theater_ratio, 0, 0.45).
narrative_ontology:measurement(marsaut_tr_t3, mars_mission_autonomy_ceiling, theater_ratio, 3, 0.58).
narrative_ontology:measurement(marsaut_tr_t6, mars_mission_autonomy_ceiling, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(marsaut_be_t0, mars_mission_autonomy_ceiling, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(marsaut_be_t3, mars_mission_autonomy_ceiling, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(marsaut_be_t6, mars_mission_autonomy_ceiling, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mars_mission_autonomy_ceiling, enforcement_mechanism).
narrative_ontology:affects_constraint(mars_mission_autonomy_ceiling, mars_habitat_self_repair_authority).
narrative_ontology:affects_constraint(mars_mission_autonomy_ceiling, emergency_medical_autonomy).

% DUAL FORMULATION NOTE:
% The autonomy ceiling constraint family includes three distinct structural claims decomposed by domain: (1) mars_mission_autonomy_ceiling — overall decision authority and communication lag; (2) mars_habitat_self_repair_authority — operational maintenance and repair decisions; (3) emergency_medical_autonomy — crew health decisions under communication lag. Each has different ε values reflecting domain-specific extractiveness. The master constraint affects both subordinate constraints through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mars_mission_autonomy_ceiling, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
