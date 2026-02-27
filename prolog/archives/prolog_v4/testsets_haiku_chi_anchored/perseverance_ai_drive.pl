% ============================================================================
% CONSTRAINT STORY: perseverance_ai_drive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perseverance_ai_drive
 *   human_readable: AI-Driven Martian Rover Autonomy Constraint
 *   domain: technological/space_exploration
 *
 * SUMMARY:
 *   NASA's Perseverance rover successfully executed autonomous AI-planned
 *   navigation for the first time on Mars in March 2021, autonomously
 *   traversing 200 feet (61 meters) without direct human teleoperation. This
 *   milestone represents the solution to a fundamental constraint: the 11-22
 *   minute round-trip light delay between Earth and Mars makes real-time
 *   remote control impossible. The autonomy constraint emerges from
 *   communication physics, not from institutional design. However, the
 *   constraint also creates secondary extraction opportunities: vendor
 *   dependency on NASA/JPL's autonomy algorithms, potential lock-in of other
 *   space agencies to existing platforms, and concentration of decision
 *   authority during the planning-execution cycle. The constraint
 *   classification varies sharply depending on the observer's structural
 *   position. For NASA JPL, autonomy is a pure coordination mechanism solving
 *   an unavoidable problem. For the planetary science community, it is the
 *   same. For international space agencies with constrained exit options, it
 *   is a mandatory framework they must adopt. For future missions with better
 *   communication infrastructure, it is a temporary scaffold. The analytical
 *   observer risks naturalizing contingent institutional implementation
 *   details as inherent to the physics. The core constraint is physical
 *   (communication delay), but the autonomy *implementation* contains
 *   extractive and theatrical elements that deserve scrutiny.
 *
 * KEY AGENTS:
 *   - NASA JPL Mission Control: Institutional beneficiary (institutional/arbitrage) — designs and implements autonomy system; benefits from reduced operational overhead and sole proprietor of the solution
 *   - Planetary Science Community: Organized beneficiary (organized/mobile) — benefits from increased mission throughput and daily planning cycles
 *   - Perseverance Rover: Technical artifact (analytical/analytical) — the locus of autonomous decision-making; makes localized navigation choices within human-set constraints
 *   - International Space Agencies: Moderate power actors (moderate/constrained) — must adopt similar autonomy strategies; constrained by technical/export control barriers to developing alternatives
 *   - Future Mars Infrastructure: Powerful future actor (powerful/mobile) — will have exit option via relay satellites and reduced light-delay; experiences current autonomy constraint as temporary
 *   - Earth-Based Human Operators: Secondary institutional actor (institutional/arbitrage) — lose direct teleoperation capability but gain planning authority; experience shift in decision locus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perseverance_ai_drive, 0.32).
domain_priors:suppression_score(perseverance_ai_drive, 0.28).
domain_priors:theater_ratio(perseverance_ai_drive, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perseverance_ai_drive, extractiveness, 0.32).
narrative_ontology:constraint_metric(perseverance_ai_drive, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(perseverance_ai_drive, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perseverance_ai_drive, rope).
narrative_ontology:human_readable(perseverance_ai_drive, "AI-Driven Martian Rover Autonomy Constraint").
narrative_ontology:topic_domain(perseverance_ai_drive, "technological/space_exploration").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, nasa_jpl_mission_control).
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, planetary_science_community).
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, earth_based_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NASA JPL MISSION CONTROL (ROPE) — Primary beneficiary. Experiences AI autonomy as a pure coordination mechanism solving the light-delay problem (11-22 minute round-trip communication lag). Direct human teleoperation is impossible; autonomous planning is the only viable solution. Benefits from reduced operational overhead and mission flexibility. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.04. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: PLANETARY SCIENCE COMMUNITY (ROPE) — Secondary beneficiary. Organized scientists benefit from increased rover throughput and mission longevity. Autonomy enables daily drive plans without multi-day planning cycles. Experience no extraction — this is pure coordination gain. d≈0.20, f(d)≈0.05, σ=1.2 → χ≈0.02. Near-zero effective extraction; coordination benefit is clear.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / NATURAL LAW VIEW (ROPE) — The light-delay constraint is a fundamental property of planetary distances. No agent can exit or arbitrage this physical limit. The autonomy requirement emerges necessarily from the constraint. From this view, autonomy is not extraction but a coordination solution to an immutable physical fact. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.21. Moderate effective extraction reflects the constraint's structural inevitability — the 'cost' is the necessity of autonomous planning, which all agents must bear equally.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: FUTURE MARS EXPLORATION MISSIONS (SCAFFOLD) — Powerful actors with mobile exit options see AI autonomy as a temporary coordination framework with a sunset. As communication infrastructure improves (relay satellites, local communication networks), the need for onboard autonomy will decrease. Initial autonomy constraints are tolerated because they are time-limited. Sunset horizon: 15-25 years as Mars infrastructure matures. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.14. Low effective extraction because the constraint is recognized as temporary.
constraint_indexing:constraint_classification(perseverance_ai_drive, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL SPACE AGENCIES (ROPE) — Moderate power actors with constrained exit options. Must adopt similar autonomy strategies because no alternative exists for Mars operations. Experience the constraint as mandatory coordination rather than extraction. Benefit from NASA's demonstrated solution and can adopt/adapt it. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.19. Moderate effective extraction reflects that they must follow NASA's lead but gain genuine coordination benefits.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

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
 *   Extractiveness (0.32): Moderate-low. The autonomy constraint solves a genuine coordination problem (light delay) that all agents face equally. However, extractiveness is not zero because: (1) NASA/JPL maintains proprietary control of the autonomy algorithms, creating vendor dependency; (2) the implementation concentrates decision authority in closed-loop onboard systems, reducing transparency of failure modes; (3) other space agencies cannot easily replicate or adapt the system due to technical and export control barriers. The rising trajectory (0.15 → 0.32) reflects that as the autonomy system matures and becomes mission-critical, the vendor lock-in and decision concentration effects accumulate. Suppression (0.28): Moderate-low. Barriers to alternatives include technical complexity, capital requirements for developing competing systems, and export control restrictions on space-qualified AI/robotics. However, suppression is not high because international space agencies retain theoretical capacity to develop alternatives (ESA, CNSA, Roscosmos have comparable engineering capability), and the constraint is recognized as legitimate by all parties. Theater ratio (0.35): Low-moderate. The autonomy process includes some performative elements: daily planning briefings for stakeholder engagement, media coverage of autonomous decisions, communication protocols that involve human sign-off even for pre-planned routes. However, the core function (safe navigation in communication-delayed environment) is genuine and not theatrical. The theater ratio has risen slightly over the interval (0.22 → 0.35) as the autonomy system became more prominent in mission narratives and public engagement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a perspectival gap between institutional beneficiary (NASA) and constrained secondary actors (international agencies). NASA JPL sees pure rope — they designed the solution and benefit from its elegance and mission success. The planetary science community also sees rope — pure coordination gain. But international space agencies see the same constraint differently: it looks like rope in its coordination function, but the vendor dependency and export control barriers impose extraction costs. The analytical observer risks seeing the constraint as a natural law (communication physics) and missing the extractive implementation choices layered on top (proprietary algorithms, centralized decision authority). The scaffold perspective (future missions with better infrastructure) is partially aspirational — it assumes that relay networks will be deployed within 15-25 years, which is credible but not certain.
 *
 * DIRECTIONALITY LOGIC:
 *   NASA JPL: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; they designed the solution. Planetary science community: Beneficiary + mobile → d≈0.20, f(d)≈0.05. Nearly zero extraction; genuine coordination gain with high mobility. International space agencies: Victim (constrained exit) + mobile aspirationally → d≈0.50, f(d)≈0.65. Moderate extraction because they must adopt the system but retain some theoretical capacity to develop alternatives. Future missions with infrastructure: Mobile + temporary → d≈0.45, f(d)≈0.45. Low extraction because the constraint has a sunset and the actors have exit options. Analytical observer: d≈0.50, f(d)≈0.65. The constraint's physical basis means no agent can exit entirely, but the implementation choices (not the physics) create asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by separating the *physical constraint* (communication delay requiring autonomy) from the *implementation constraint* (NASA's proprietary autonomy system). The physical constraint is a rope — pure coordination solution to an inevitable problem. The implementation constraint is potentially a snare or tangled rope — vendor dependency, export control restrictions, and decision authority concentration create extraction opportunities. The classification should track the implementation, not the physics. If NASA released the autonomy algorithms as open-source and international agencies could freely adapt them, the constraint would shift firmly toward pure rope. The current classification (rope with moderate extractiveness) reflects that the coordination function is genuine but the implementation includes extractive elements. The mandatrophy is resolved by recognizing that a rope can have extractive components without becoming a snare — the coordination function must be real and broadly beneficial, which it is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_sufficiency_threshold,
    'What level of onboard AI autonomy is necessary and sufficient for safe Mars rover navigation, and is Perseverance''s current implementation at that threshold or overshooting it?',
    'Comparative analysis of mission success rates, distance covered per planning cycle, and navigation error rates across autonomy levels; correlation with terrain complexity and hazard density',
    'If current autonomy is minimal necessary: rope classification confirmed across all perspectives. If overshooting with unnecessary sophistication: suggests extraction (vendor lock-in, capabilities beyond mission need) and shifts toward snare/tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_sufficiency_threshold, empirical, 'Whether current autonomy level is necessary-sufficient or excessive').

omega_variable(
    communication_infrastructure_roadmap,
    'What is the realistic timeline for Mars relay satellite networks to reduce light-delay constraints, and would that enable return to direct teleoperation?',
    'NASA Artemis/Mars architecture roadmaps; feasibility studies for Mars communication constellation; technical analysis of light-delay reduction via relay networks',
    'If infrastructure timeline is 10-15 years: scaffold sunset is credible, constrains classification upward. If infrastructure timeline > 50 years: autonomy is not temporary, shifts toward rope as permanent solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communication_infrastructure_roadmap, empirical, 'Timeline for communication infrastructure reducing light-delay constraints').

omega_variable(
    closed_loop_decision_authority,
    'In the current autonomy constraint, where does decision authority actually reside: with the rover''s AI, with human planners setting constraints, or distributed between them?',
    'Analysis of drive-planning procedure; identification of which decisions are algorithmic vs human-vetted; observation of failure modes (who overrides whom)',
    'If authority is genuinely distributed: tangled rope (mixed coordination-extraction). If authority is actually with AI: snare for human operators (loss of control). If authority remains with humans and AI is purely advisory: pure rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(closed_loop_decision_authority, conceptual, 'Locus of decision authority between rover AI and human planners').

omega_variable(
    vendor_dependency_lock_in,
    'Does the autonomy constraint create vendor lock-in to specific AI/robotics platforms, limiting other space agencies'' ability to develop alternatives?',
    'Analysis of Perseverance''s autonomy software architecture; assessment of portability to other rover designs; review of technology licensing and export control restrictions',
    'If lock-in is significant: suggests extraction mechanism (NASA/JPL benefits from dependency), shifts toward tangled rope or snare. If architecturally open: pure rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_dependency_lock_in, empirical, 'Degree of vendor lock-in to specific autonomy platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perseverance_ai_drive, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(persev_tr_t0, perseverance_ai_drive, theater_ratio, 0, 0.22).
narrative_ontology:measurement(persev_tr_t3, perseverance_ai_drive, theater_ratio, 3, 0.29).
narrative_ontology:measurement(persev_tr_t6, perseverance_ai_drive, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(persev_be_t0, perseverance_ai_drive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(persev_be_t3, perseverance_ai_drive, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(persev_be_t6, perseverance_ai_drive, base_extractiveness, 6, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perseverance_ai_drive, enforcement_mechanism).
narrative_ontology:affects_constraint(perseverance_ai_drive, mars_communication_infrastructure).
narrative_ontology:affects_constraint(perseverance_ai_drive, space_exploration_technology_export_control).

% DUAL FORMULATION NOTE:
% The autonomy constraint decomposes into two structurally distinct claims: (1) the physical necessity of onboard autonomy due to light-delay (pure coordination, rope), and (2) the institutional implementation via proprietary NASA/JPL systems (potential extraction/snare). The first claim is robust across all observables; the second claim varies with observer position and access to alternative implementations. Both are linked here because the implementation dependencies affect whether the coordination solution remains open or becomes captured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(perseverance_ai_drive, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
