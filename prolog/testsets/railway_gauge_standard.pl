% ============================================================================ 
% CONSTRAINT STORY: railway_gauge_standard
% ============================================================================ 
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: George Stephenson (1825) / Path Dependence / Economic History
% ============================================================================ 

:- module(constraint_railway_gauge_standard, []).

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
 * constraint_id: railway_gauge_standard
 * human_readable: The Standard Railway Gauge (4 ft 8.5 in)
 * domain: technological/economic
 * temporal_scope: 1820s - Present
 * spatial_scope: Global (approx. 60% of world tracks)
 * 
 * SUMMARY:
 * The Standard Gauge (1435 mm) is a classic example of technological lock-in 
 * and path dependence. It was formally set by George Stephenson based on existing 
 * horse-drawn coal wagons. Despite "Broad Gauge" alternatives offering better 
 * stability and speed, the Standard Gauge became a global invariant due to 
 * network effects, leading to an immutable 'Mountain' of infrastructure.
 * 
 * KEY AGENTS:
 * - The Passenger / Freight Loader (Individual Powerless): Uses the trains as presented.
 * - The National Transport Authority (Institutional): Enforces the standard for interoperability.
 * - Isambard Kingdom Brunel (Individual Moderate): Innovator whose superior alternative was rejected.
 */

/* ========================================================================== 
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(railway_gauge_standard, 0, 10).
narrative_ontology:constraint_claim(railway_gauge_standard, piton).

% Base extractiveness: 0.3 (Low-Moderate). 
% It extracts "optimal technical performance." By forcing a sub-optimal width, 
% it limits the size and speed of trains compared to what could be achieved 
% with broader tracks.
domain_priors:base_extractiveness(railway_gauge_standard, 0.3).

% Suppression: 0.2 (Low).
% The standard persists due to massive network effects and sunk capital
% investment (inertia), not active enforcement against alternatives.
domain_priors:suppression_score(railway_gauge_standard, 0.2).

% Resistance: 0.6 (High).
% The sub-optimal nature causes friction for engineers and planners seeking
% efficiency and performance, indicating active resistance.
domain_priors:resistance_score(railway_gauge_standard, 0.6).

% Enforcement: Persists through network effects and sunk capital (inertia).
narrative_ontology:constraint_metric(railway_gauge_standard, extractiveness, 0.3).
narrative_ontology:constraint_metric(railway_gauge_standard, suppression_requirement, 0.2).

% Make the constraint "evolve" by showing resistance increasing over time
narrative_ontology:constraint_metric(railway_gauge_standard, resistance, 0.1, 0). % Low resistance initially (when functional)
narrative_ontology:constraint_metric(railway_gauge_standard, resistance, 0.6, 10). % High resistance currently (due to sub-optimality)

% Formally model the viable alternative
narrative_ontology:intent_viable_alternative(railway_gauge_standard,
    'Brunel\'s Broad Gauge (7 ft)',
    'Technically superior, but suppressed by economic and political factors during the Gauge Wars.').

% BENEFICIARIES & VICTIMS
constraint_beneficiary(railway_gauge_standard, interconnected_logistics).
constraint_victim(railway_gauge_standard, technical_optimality).


/* ========================================================================== 
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* -------------------------------------------------------------------------- 
   PERSPECTIVE 1: THE PASSENGER / FREIGHT LOADER - Mountain
   -------------------------------------------------------------------------- 
   WHO: individual_powerless (No control over infrastructure)
   WHEN: immediate (Using the railway system daily)
   WHERE: trapped (Must use the trains as they are presented) 
   
   WHY THIS CLASSIFICATION:
   For the ordinary user, the railway gauge is an immutable 'Mountain' of
   infrastructure. They simply use the trains as they are presented,
   unaware of gauge wars or technical debates. It's a fixed reality of
   their transportation system.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    railway_gauge_standard,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* -------------------------------------------------------------------------- 
   PERSPECTIVE 2: THE NATIONAL TRANSPORT AUTHORITY - Rope
   -------------------------------------------------------------------------- 
   WHO: institutional (Enforces standards for national infrastructure)
   WHEN: biographical (Managing a career's worth of infrastructure development)
   WHERE: arbitrage (Can fund "gauge-changing" tech or hybrid tracks)
   
   WHY THIS CLASSIFICATION:
   For the institutional regulator, the standard is a 'Rope'. It is the 
   ultimate coordination mechanism, allowing trains to cross state or national
   borders without stopping. They use the constraint to pull a unified economy
   together, prioritizing "connectivity" over "optimal width."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    railway_gauge_standard,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(continental)
    )
).

/* -------------------------------------------------------------------------- 
   PERSPECTIVE 3: ISAMBARD KINGDOM BRUNEL (INNOVATOR) - Snare
   -------------------------------------------------------------------------- 
   WHO: individual_moderate (Power to build, but subject to the network)
   WHEN: immediate (The "Gauge War" of the 1840s)
   WHERE: constrained (His Broad Gauge (7ft) is technically superior but incompatible)
   
   WHY THIS CLASSIFICATION:
   For the innovator, the standard is a 'Snare'. He has a faster, safer, and 
   more stable design, but the "Standard Gauge" has already captured the 
   majority of the network. The requirement to connect to the "Standard" 
   strangles his Broad Gauge, forcing its extinction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    railway_gauge_standard,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE INSTITUTIONAL ANALYST - PITON
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of institutional decay)
   WHEN: biographical (Seeing the standard's function degrade over a career)
   WHERE: analytical (Comparing stated goals vs. actual outcomes)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   The analyst sees the standard as a Piton. It was created as a Rope to
   ensure interoperability, but has decayed into a liability that persists 
   through inertia. It no longer serves its coordinating function optimally
   and now creates more friction than it resolves, but it's too embedded to
   easily remove.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    railway_gauge_standard,
    piton,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(analytical),
        spatial_scope(national)
    )
).

/* ========================================================================== 
   4. TESTS
   ========================================================================== */

:- begin_tests(railway_gauge_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(railway_gauge_standard, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(railway_gauge_standard, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(railway_gauge_standard, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(railway_gauge_tests).

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
 * 1. PERSPECTIVE SELECTION: Added the 'Passenger / Freight Loader' as the
 *    'individual_powerless' agent. This highlights how a technological standard,
 *    once established, becomes an immutable 'Mountain' for the end-user.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Passenger (Mountain): An unquestioned fact of infrastructure.
 *    - Authority (Rope): A tool for national/continental coordination.
 *    - Innovator (Snare): A superior alternative suppressed by network effects.
 * 
 * 3. CORE INSIGHT: This is a classic example of path dependence where initial
 *    contingencies (Stephenson's horse-drawn wagons) lead to a global 'Mountain'
 *    that suppresses technically superior alternatives through network effects.
 */

/* ========================================================================== 
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether the economic and social costs of network effects
 * can ever be overcome by technological innovation.
 */

omega_variable(
    transshipment_innovation,
    "Can digital 'automatic gauge-changing' wheels truly untie the 'Snare' of the Standard Gauge, or is the cost of retrofitting always an insurmountable 'Mountain'?",
    resolution_mechanism("Monitor the adoption rate and economic viability of variable-gauge axles in cross-border corridors (e.g., Spain/France)."),
    impact("If successful: Different gauges can coexist more flexibly ('Rope'). If too costly: Incompatibility remains a permanent 'Mountain' of friction."),
    confidence_without_resolution(medium)
).

/* ========================================================================== 
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Brunel's Broad Gauge (7 ft)
 *    Viability: Physically superior, offering smoother, faster rides and greater load capacity.
 *    Suppression: High. Extinguished by the Railway Regulation (Gauge) Act 1846, which legally
 *    mandated the Standard Gauge and made non-standard gauges economically unviable due to transshipment costs.
 *
 * CONCLUSION:
 * The existence of a physically superior alternative that was actively suppressed
 * confirms that the Standard Gauge is a 'Snare' for engineering innovation,
 * even if it facilitates trade (a 'Rope' for logistics).
 */

/* ========================================================================== 
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE: 
 * 
 * 1. Load: ?- [constraints/railway_gauge_standard].
 * 2. Multi-perspective: ?- multi_index_report(railway_gauge_standard).
 * 3. Run tests: ?- run_tests(railway_gauge_tests).
 */

/* ========================================================================== 
   END OF CONSTRAINT STORY
   ========================================================================== */
