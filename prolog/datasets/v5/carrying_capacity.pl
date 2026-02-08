% ============================================================================
% CONSTRAINT STORY: carrying_capacity
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ecological Economics / Population Biology / Malthus (1798)
% ============================================================================

:- module(constraint_carrying_capacity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: carrying_capacity
 * human_readable: Carrying Capacity (K)
 * domain: economic/technological/social
 * temporal_scope: Permanent (Biological and Physical Reality)
 * spatial_scope: Global (Ecosystemic)
 * * SUMMARY:
 * Carrying capacity represents the maximum population size of a species that 
 * a specific environment can sustain indefinitely without degrading the 
 * resource base. It acts as a structural limit where the extraction of 
 * resources meets the biological rate of renewal, defining the boundary 
 * between sustainable existence and ecological collapse.
 * * KEY AGENTS:
 * - The Population Biologist (Analytical): Observes the logistic growth 
 * curve and the inevitable leveling of the population as it approaches K.
 * - The Resource Manager (Institutional): Uses carrying capacity as a "Rope" 
 * to coordinate extraction limits (e.g., fishing quotas, urban planning).
 * - The Subsistence Inhabitant (Individual Powerless): Experiences K as a 
 * "Snare" that strangles growth and extracts life quality when 
 * resources are over-appropriated by others.
 * * NARRATIVE ARC:
 * Carrying capacity functions as a "Mountain" of physical reality—the 
 * hardware limits of the local energy and matter budget. For the 
 * conservationist, it is a "Rope" for collective coordination (preserving 
 * the future). However, in a state of overshoot, it becomes a "Snare" 
 * for the individual, where the scarcity of food or space extracts 
 * survival capacity until the population is forcibly corrected.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for extraction
narrative_ontology:interval(carrying_capacity_interval, 0, 10).
narrative_ontology:constraint_claim(carrying_capacity, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: High (0.7). Approaching or exceeding carrying capacity extracts 
% the health of the ecosystem and the future viability of the population. 
% It extracts current metabolic energy to pay for the "debt" of overpopulation.
domain_priors:base_extractiveness(carrying_capacity, 0.7).

% Suppression score (0.0-1.0)
% Rationale: High (0.6). The physical limit suppresses the alternative of 
% "Infinite Growth." While the growth fantasy is visible, the biophysical 
% reality punishes those who ignore it, suppressing long-term non-compliance.
domain_priors:suppression_score(carrying_capacity, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(carrying_capacity, extractiveness, 0.7).
narrative_ontology:constraint_metric(carrying_capacity, suppression_requirement, 0.6).

% Enforcement requirements
% The limit emerges naturally from thermodynamics; institutionalizing it 
% requires active political enforcement (e.g., quotas).
domain_priors:emerges_naturally(carrying_capacity).
domain_priors:requires_active_enforcement(carrying_capacity).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(carrying_capacity, [long_term_survivors, ecosystem_stability]).
constraint_victim(carrying_capacity, [over_expanding_populations, marginalized_resource_users]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ECOLOGIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the thermodynamic and biological substrate.
   WHEN: civilizational - Viewing species survival over thousands of years.
   WHERE: trapped - Logic and biology cannot bypass the caloric limits of land.
   SCOPE: global - Universal to all life-support systems.
   
   WHY THIS CLASSIFICATION:
   To the scientist, carrying capacity is a Mountain. It is an unchangeable 
   feature of the environment's hardware. You cannot "innovate" past the 
   photosynthetic limit of a plot of land or the freshwater recharge rate of 
   an aquifer; they are fixed peaks in the topography of what is possible.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    carrying_capacity,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE FISHERIES MANAGER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define and enforce catch limits.
   WHEN: biographical - Managing a specific industry over a 30-year career.
   WHERE: arbitrage - Can shift between different resource pools or technologies.
   SCOPE: regional - Specific coastal or national waters.
   
   WHY THIS CLASSIFICATION:
   For the institutional manager, carrying capacity is a Rope. It is a 
   functional coordination mechanism. By quantifying "Maximum Sustainable 
   Yield," they pull the industry toward a stable future, using the 
   constraint as a tether to prevent a "Tragedy of the Commons."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    carrying_capacity,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(carrying_capacity, E),
    E > 0.4, % Managed extraction of resource surplus
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE URBAN SLUM DWELLER - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to high density and resource scarcity.
   WHEN: immediate - Today's access to clean water and food.
   WHERE: constrained - Limited by poverty and lack of land ownership.
   SCOPE: local - Immediate neighborhood/slum.
   
   WHY THIS CLASSIFICATION:
   For the person living in an over-capacity urban environment, K is a Snare. 
   The extraction of space and clean air has turned their environment into 
   a trap. As more people compete for the same fixed resources, the Snare of 
   scarcity tightens, strangling their health and economic survival.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    carrying_capacity,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(carrying_capacity, E),
    E > 0.6, % Severe extraction of life quality
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(carrying_capacity_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Snare
    constraint_indexing:constraint_classification(carrying_capacity, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(carrying_capacity, rope, context(institutional, biographical, arbitrage, regional)),
    constraint_indexing:constraint_classification(carrying_capacity, snare, context(powerless, immediate, constrained, local)).

test(power_extractiveness_resource) :-
    ContextPowerless = context(powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, regional),
    constraint_indexing:extractiveness_for_agent(carrying_capacity, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(carrying_capacity, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(carrying_capacity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.7):
 * Carrying capacity is fundamentally about the limits of extraction. In 
 * overshoot, the environment extracts "life" to return to equilibrium. 
 * This is an asymmetric process where those with power often outsource 
 * the extraction to the powerless.
 * * 2. PERSPECTIVE SELECTION:
 * Selected the Ecologist (Law), the Resource Manager (Tool), and the 
 * Slum Dweller (Victim) to highlight the indexical shift from "Math" 
 * to "Policy" to "Suffering."
 * * 3. SNARE LOGIC:
 * Applied to the "Urban Slum" context, where density and resource limits 
 * create a literal physical trap for those who cannot afford to leave.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_k_expansion,
    "Can technology permanently expand the planetary carrying capacity 
    (Mountain), or does it merely create a 'Scaffold' that eventually 
    collapses?",
    resolution_mechanism("Long-term tracking of resource replenishment rates 
    vs. technological throughput"),
    impact("If Mountain: We can grow indefinitely. If Scaffold: The Snare is 
    just hidden for now."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Circular Economy
 * Viability: Theoretically lowers the throughput required to sustain 
 * a population, effectively raising the functional K.
 * Suppression: Moderate. Often suppressed by "Linear Growth" interests 
 * that profit from rapid resource extraction and waste.
 * * CONCLUSION:
 * The existence of circular alternatives that are socially suppressed 
 * confirms that for the individual, Carrying Capacity is often a Snare 
 * that could be loosened through better systemic design.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_carrying_capacity].
 * 2. Multi-perspective: ?- multi_index_report(carrying_capacity).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
