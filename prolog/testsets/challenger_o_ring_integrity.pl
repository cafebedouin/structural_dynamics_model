% ============================================================================
% CONSTRAINT STORY: challenger_o_ring_integrity
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Rogers Commission Report / Engineering Analysis
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_challenger_o_ring_integrity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: challenger_o_ring_integrity
 * human_readable: Challenger SRB O-Ring Integrity Failure
 * domain: technological/institutional
 * temporal_scope: 1986-01-28
 * spatial_scope: Kennedy Space Center, FL
 * 
 * SUMMARY:
 * The disaster was caused by the failure of O-ring seals in the Solid Rocket
 * Boosters due to record-low temperatures on launch day. While the physical
 * limitation of the O-rings was a known 'Mountain', the institutional pressure
 * to maintain the launch schedule transformed this risk into a catastrophic 'Snare'.
 * 
 * KEY AGENTS:
 * - The O-Rings (Analytical): The physical component subject to thermodynamic law.
 * - NASA Management (Institutional): Pushing to maintain the launch schedule.
 * - The STS-51-L Crew (Individual Powerless): Trapped by the decision to launch.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(challenger_o_ring_integrity, 0, 10).
narrative_ontology:constraint_claim(challenger_o_ring_integrity, snare).

% Base extractiveness: 0.8. 
% Rationale: Extreme extraction of safety margins and engineering best-practices
% to fulfill institutional mandates (launch schedule), leading to loss of life.
domain_priors:base_extractiveness(challenger_o_ring_integrity, 0.8).

% Suppression score: 0.7.
% Rationale: Concerns from engineers about O-ring performance in the cold were
% actively suppressed and overridden by management in the lead-up to launch.
domain_priors:suppression_score(challenger_o_ring_integrity, 0.7).

% Enforcement: Requires active enforcement of schedule over safety objections.
domain_priors:requires_active_enforcement(challenger_o_ring_integrity).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(challenger_o_ring_integrity, institutional_schedule).
constraint_victim(challenger_o_ring_integrity, sts_51_l_crew).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE O-RINGS (ENGINEERING PHYSICS) - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (The physical laws of thermodynamics)
   WHEN: immediate (The moment of ignition at low temperature)
   WHERE: trapped (The O-rings cannot change their physical properties)
   
   WHY THIS CLASSIFICATION:
   For the O-rings themselves, the relationship between temperature and elasticity
   is an immutable 'Mountain'. Below a certain temperature, they will fail. This
   is a non-negotiable law of physics.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    challenger_o_ring_integrity, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NASA MANAGEMENT - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Driven by schedule and political pressure)
   WHEN: biographical (The lifespan of the Shuttle program)
   WHERE: mobile (Had the choice to postpone the launch)
   
   WHY THIS CLASSIFICATION:
   For NASA management, the known O-ring issue was a risk to be managed—a 'Rope'.
   They saw it as one of many technical challenges to be coordinated and balanced
   against the immense pressure to maintain a frequent launch cadence for political
   and budgetary reasons.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    challenger_o_ring_integrity, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE STS-51-L CREW - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Astronauts with no say in the final launch decision)
   WHEN: immediate (The 73 seconds of flight)
   WHERE: trapped (Physically inside the vehicle with no escape system)
   
   WHY THIS CLASSIFICATION:
   For the crew, the management decision to launch despite the cold transformed the
   technical risk into a 'Snare'. They were trapped in a system where their safety
   was extracted in favor of the schedule, with a fatal outcome.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    challenger_o_ring_integrity, 
    snare, 
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(challenger_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(challenger_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. MANDATROPHY STATUS: The 0.8 extractiveness score is justified because the
 *    decision to launch extracted the entire safety margin from the system,
 *    and ultimately the lives of the crew, to serve an institutional goal.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Physics (Mountain): The material properties are unchangeable.
 *    - Management (Rope): A risk to be managed against a schedule.
 *    - Crew (Snare): The fatal, inescapable outcome of the management decision.
 *
 * 3. CORE INSIGHT: This story is a classic example of how a known 'Mountain'
 *    (physical limit) is transformed into a deadly 'Snare' by institutional
 *    pressures overriding safety protocols.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */
omega_variable(
    normalization_of_deviance_threshold,
    "At what point does an incremental acceptance of risk (a 'Rope' adjustment) become a catastrophic normalization of deviance that guarantees failure (a 'Snare')?",
    resolution_mechanism("Post-hoc analysis of safety waiver trends versus hardware failure rates across multiple missions."),
    impact("If a clear threshold can be identified, it could prevent future systemic failures. If it's unknowable beforehand, it implies all complex systems risk surprise catastrophe."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Launch Postponement
 *    Viability: This was the explicit recommendation of the Thiokol engineers.
 *    Suppression: Actively suppressed by NASA management due to political pressure
 *    and the desire to maintain the launch schedule, including the "Teacher in Space" PR goal.
 *
 * CONCLUSION:
 * The active suppression of the postponement alternative is the central action that
 * converted the physical risk ('Mountain') into a fatal 'Snare' for the crew.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */
/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/challenger_o_ring_integrity].
 * 2. Multi-perspective: ?- multi_index_report(challenger_o_ring_integrity).
 * 3. Run tests: ?- run_tests(challenger_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */