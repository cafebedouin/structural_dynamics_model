% ============================================================================
% CONSTRAINT STORY: railway_gauge_standard
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: George Stephenson (1825) / Path Dependence / Economic History
% ============================================================================

:- module(constraint_railway_gauge, []).

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
 * * constraint_id: railway_gauge_standard
 * human_readable: Standard Railway Gauge (4 ft 8.5 in)
 * domain: technological/economic
 * temporal_scope: 1820s - Present
 * spatial_scope: Global (approx. 60% of world tracks)
 * * SUMMARY:
 * The Standard Gauge (1435 mm) is a classic example of technological lock-in 
 * and path dependence. Legend holds it traces back to Roman chariot ruts, 
 * but it was formally set by George Stephenson based on existing horse-drawn 
 * coal wagons. Despite "Broad Gauge" alternatives offering better stability 
 * and speed, the Standard Gauge became a global invariant due to network effects.
 * * KEY AGENTS:
 * - The Railway Historian: Analytical observer of "frozen accidents" in engineering.
 * - The National Transport Authority: Institutional agent enforcing the standard 
 * to ensure interstate/international interoperability.
 * - The Broad Gauge Innovator (e.g., Brunel): Individual moderate whose 
 * superior technical alternative was strangled by the existing network.
 * * NARRATIVE ARC:
 * What began as a local "Rope" (matching coal wagons) hardened into a 
 * "Mountain" of physical infrastructure. For the technical innovator, it 
 * became a "Noose," as the cost of "breaking gauge" at borders extracted 
 * the viability of better designs.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural identification
narrative_ontology:interval(railway_gauge_interval, 0, 10).
narrative_ontology:constraint_claim(railway_gauge_standard, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.3 (Low-Moderate). It extracts "optimal technical performance." 
% By forcing a sub-optimal width, it limits the size and speed of trains 
% compared to what could be achieved with broader tracks.
domain_priors:base_extractiveness(railway_gauge_standard, 0.3).

% Suppression score (0.0-1.0)
% Rationale: 0.8 (High). Alternatives (Broad or Narrow gauge) are physically 
% suppressed. A train built for one cannot run on the other without 
% expensive "transshipment," making the alternatives economically invisible.
domain_priors:suppression_score(railway_gauge_standard, 0.8).

% Enforcement requirements
% Emerges naturally via network effects; maintained by massive capital investment.
domain_priors:emerges_naturally(railway_gauge_standard).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(railway_gauge_standard, extractiveness, 0.3).
narrative_ontology:constraint_metric(railway_gauge_standard, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(railway_gauge_standard, [standard_rolling_stock_manufacturers, interconnected_logistics]).
constraint_victim(railway_gauge_standard, [high_speed_innovators, technical_perfectionists]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LOGISTICS ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of global trade corridors.
   WHEN: civilizational - Viewing the rail network as a permanent crust of the earth.
   WHERE: trapped - The tracks are already laid in steel and concrete.
   SCOPE: global - Continental rail networks.
   
   WHY THIS CLASSIFICATION:
   To the analyst, the Gauge is a Mountain. It is an immovable fact of 
   civilizational hardware. The cost of ripping up and relaying millions 
   of miles of track to a "better" width is so high that the current 
   width is effectively a law of nature.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    railway_gauge_standard,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE EUROPEAN UNION (Interoperability Dept) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to regulate international standards.
   WHEN: biographical - Managing a single career's worth of infrastructure.
   WHERE: arbitrage - Can fund "gauge-changing" tech or hybrid tracks.
   SCOPE: continental - Trans-European networks.
   
   WHY THIS CLASSIFICATION:
   For the institutional regulator, the standard is a Rope. It is the 
   ultimate coordination mechanism. It allows a train from Paris to 
   reach Warsaw without stopping. They use the constraint to pull a 
   unified economy together, prioritizing "connectivity" over "optimal width."
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: ISAMBARD KINGDOM BRUNEL (Innovator) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Power to build, but subject to the network.
   WHEN: immediate - The "Gauge War" of the 1840s.
   WHERE: constrained - His Broad Gauge (7ft) is technically superior but incompatible.
   SCOPE: national - The UK railway market.
   
   WHY THIS CLASSIFICATION:
   For the innovator, the standard is a Noose. He has a faster, safer, and 
   more stable design, but the "Standard Gauge" has already captured the 
   majority of the network. The requirement to connect to the "Standard" 
   strangles his Broad Gauge, eventually extracting all its capital and 
   forcing its extinction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    railway_gauge_standard,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(railway_gauge_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Innovator sees Noose
    constraint_indexing:constraint_classification(railway_gauge_standard, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(railway_gauge_standard, rope, context(institutional, biographical, arbitrage, continental)),
    constraint_indexing:constraint_classification(railway_gauge_standard, noose, context(individual_moderate, immediate, constrained, national)).

test(power_extractiveness_incompatibility) :-
    % The "victim" (Innovator) feels the extraction of their better tech (Noose).
    % The "institution" uses the lock-in for coordination (Rope).
    domain_priors:base_extractiveness(railway_gauge_standard, E),
    E >= 0.3.

test(time_immutability_lockin) :-
    % Historical/Civilizational view = Mountain.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(railway_gauge_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.3): Chose lower-moderate because while the standard 
 * is technically sub-optimal, the "theft" is of potential performance 
 * rather than active labor or health (unlike QWERTY's RSI).
 * 2. PERSPECTIVE SELECTION: Brunel is the perfect "Noose" subject—he knew 
 * he had a better product, but the network effect strangled it.
 * 3. SUPPRESSION (0.8): Very high. You cannot just "try" a different gauge 
 * without building a whole separate world; the physics of the rail 
 * suppress the alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transshipment_innovation,
    "Can digital 'automatic gauge-changing' wheels untie the Noose of the 
    Standard Gauge (Rope), or is the cost of retrofitting always a 
    Mountain?",
    resolution_mechanism("Monitor the adoption rate of variable-gauge 
    axles in cross-border corridors like Spain/France"),
    impact("If Rope: Different gauges can coexist. If Mountain: Incompatibility 
    remains a permanent friction point."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Brunel's Broad Gauge (7 ft)
 * Viability: High. Provided smoother, faster rides and could carry 
 * much heavier loads.
 * Suppression: High. Extinguished by the Railway Regulation (Gauge) 
 * Act 1846, which mandated the Standard Gauge.
 * * CONCLUSION:
 * The existence of a physically superior alternative that was legally and 
 * economically suppressed confirms that the Standard Gauge is a Noose 
 * for engineering, even if it is a Rope for trade.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * ?- [constraint_railway_gauge].
 * ?- multi_index_report(railway_gauge_standard).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
