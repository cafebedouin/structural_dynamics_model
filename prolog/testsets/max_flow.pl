% ============================================================================
% CONSTRAINT STORY: max_flow_min_cut
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Ford-Fulkerson Algorithm / Network Flow Theory
% ============================================================================

:- module(constraint_max_flow_min_cut, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: max_flow_min_cut
 * human_readable: The Max-Flow Min-Cut Theorem
 * domain: technological/mathematical
 * temporal_scope: Immediate to Historical
 * spatial_scope: Regional to Global
 * * SUMMARY:
 * The Max-Flow Min-Cut theorem states that the maximum amount of flow passing 
 * from a source to a sink in a network is exactly equal to the total weight 
 * of the edges in the minimum cut (the smallest set of edges that, if removed, 
 * would disconnect the source from the sink).
 * * KEY AGENTS:
 * - The Network Architect: Designs the pipes/capacities (institutional power).
 * - The Flow: The data, water, or traffic moving through the system (passive).
 * - The Bottleneck: The specific "Min-Cut" edges that dictate the limit.
 * * NARRATIVE ARC:
 * The constraint acts as an invisible ceiling. No matter how much pressure or 
 * "flow" is added at the source, the system's throughput is hard-capped by the 
 * narrowest part of the network architecture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(max_flow_min_cut, 0, 10).
narrative_ontology:constraint_claim(max_flow_min_cut, mountain).

% Base extractiveness: Moderate (0.4).
% While it's a math law, in human systems (logistics/telecom), the bottleneck 
% often "extracts" value by forcing congestion or fees at the narrowest point.
domain_priors:base_extractiveness(max_flow_min_cut, 0.4).

% Suppression: Low (0.2).
% The bottleneck is usually highly visible to those looking for it (monitoring tools).
domain_priors:suppression_score(max_flow_min_cut, 0.2).

domain_priors:emerges_naturally(max_flow_min_cut).

% Beneficiaries: The Bottleneck Owner (e.g., the bridge with the toll).
constraint_beneficiary(max_flow_min_cut, bottleneck_controller).

% Victims: The Source/Sink Users (limited by the narrowest path).
constraint_victim(max_flow_min_cut, end_users).

narrative_ontology:constraint_metric(max_flow_min_cut, extractiveness, 0.4).
narrative_ontology:constraint_metric(max_flow_min_cut, suppression_requirement, 0.2).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DATA PACKET - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a single unit of flow, the capacity of the edge it is currently on is 
   an immutable law. It cannot "push" harder to move faster than the 
   bandwidth allows.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    max_flow_min_cut,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:emerges_naturally(max_flow_min_cut),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NETWORK ENGINEER - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: biographical
   WHERE: mobile
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   The engineer sees the bottleneck as a "Rope"—a tool for coordination. 
   By identifying the Min-Cut, they can strategically upgrade specific edges 
   to improve total flow efficiently.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    max_flow_min_cut,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MONOPOLIST - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerful
   WHEN: generational
   WHERE: arbitrage
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   A powerful agent who owns the bottleneck uses the Max-Flow limit as a 
   "Noose" to squeeze users. They purposefully keep capacity low to maintain 
   scarcity and high extraction rates.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    max_flow_min_cut,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(max_flow_min_cut, E),
    E >= 0.4,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(max_flow_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(max_flow_min_cut, Mountain, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(max_flow_min_cut, Rope, context(agent_power(institutional), _, _, _)),
    Mountain \= Rope.

test(bottleneck_awareness) :-
    % Testing if "mobile" agents (engineers) see it as a coordination rope.
    constraint_indexing:constraint_classification(max_flow_min_cut, rope, context(_, _, mobile, _)).

:- end_tests(max_flow_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * The "Max-Flow" problem is fundamentally about bottlenecks. 
 * While the math is a Mountain, the *application* of that math in 
 * socio-technical systems is often a Noose (extraction) or a Rope (design).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    dynamic_capacity,
    "Do edge capacities fluctuate based on flow history?",
    resolution_mechanism("Analysis of non-linear network dynamics / congestion collapse"),
    impact("If Capacities=Fixed: Max-Flow/Min-Cut holds. If Capacities=Dynamic: The 'Mountain' becomes a 'Sea' (shifting ground)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(max_flow, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(max_flow, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(max_flow, noose, agent_power(individual_powerless)).
