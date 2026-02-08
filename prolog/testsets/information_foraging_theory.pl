% ============================================================================
% CONSTRAINT STORY: information_foraging_theory
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Pirolli, P., & Card, S. (1999). Information foraging.
% ============================================================================

:- module(constraint_info_foraging, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: information_foraging_theory
 * human_readable: Information Foraging (Optimal Search Logic)
 * domain: technological/biological/social
 * temporal_scope: Immediate to Biographical
 * spatial_scope: Global
 * * SUMMARY:
 * IFT posits that humans use built-in biological heuristics to maximize their 
 * "Information Gain" per unit of "Interaction Cost." It explains why we 
 * skim, why we click certain links, and when we decide to abandon a 
 * "patch" (like a long article) to find a new one.
 * * KEY AGENTS:
 * - The Forager: The individual seeking information to solve a problem.
 * - The Information Patch: A concentrated source of data (a web page, a library).
 * - The Information Scent: The cues (links, keywords) that hint at the value 
 * of a potential path.
 * * NARRATIVE ARC:
 * IFT functions as a "Metabolic Rope." It coordinates human attention by 
 * calculating the "return on investment" for every click. It acts as an 
 * invisible Mountain that dictates the design of the entire internet.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(information_foraging_theory, 0, 10).
narrative_ontology:constraint_claim(information_foraging_theory, rope).

% Base extractiveness score (0.2)
% Low extraction; it is a theory of efficiency, though platforms may 
% "hijack" scent to extract attention.
domain_priors:base_extractiveness(information_foraging_theory, 0.2).

% Suppression score (0.1)
% Information "scent" is the opposite of suppression; it is a strategy 
% of high-visibility signaling.
domain_priors:suppression_score(information_foraging_theory, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(information_foraging_theory, extractiveness, 0.2).
narrative_ontology:constraint_metric(information_foraging_theory, suppression_requirement, 0.1).

% Enforcement: Emerges naturally from evolutionary energy conservation.
domain_priors:emerges_naturally(information_foraging_theory).

% Metrics
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE USER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: immediate
   WHERE: mobile (high navigation freedom)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the user, IFT is a "Rope." It is the mental coordination tool that 
   allows them to navigate the infinite "Mountain" of the web. By following 
   "Information Scent," they find the shortest path to their goal, 
   minimizing their cognitive load.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    information_foraging_theory,
    rope,
    context(agent_power(individual_moderate), time_horizon(immediate), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UX DESIGNER - Mountain
   --------------------------------------------------------------------------
   WHO: analytical/institutional
   WHEN: biographical
   WHERE: analytical
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   To a designer, IFT is a "Mountain." You cannot ignore it. If a website 
   has poor "Information Scent" (unclear navigation), users will 
   predictably "evaporate" (The Dead Sea Effect) to a better patch. 
   The theory is an immutable law of user behavior.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    information_foraging_theory,
    mountain,
    context(agent_power(institutional), time_horizon(biographical), exit_options(analytical), spatial_scope(national))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ATTENTION FARMER - Snare
   --------------------------------------------------------------------------
   WHO: powerless (the user being farmed)
   WHEN: immediate
   WHERE: trapped (in a 'sticky' platform)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   When a platform uses IFT to maximize "Time on Site" rather than 
   "Goal Completion," the theory becomes a "Snare." By providing 
   continuous "micro-scents" (infinite scrolls, notifications), they 
   trap the forager in a loop of low-value exploration, strangling 
   their productivity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    information_foraging_theory,
    snare,
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(global))
) :- !.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2.

% --- Theater ratio (missing from base properties) ---
% IFT is grounded in evolutionary biology and validated by empirical HCI
% research. The theory itself has minimal theater — functional predictions
% match observed behavior closely. Low theater (0.08) accounts for the gap
% between idealized foraging models and real-world platform manipulation.
domain_priors:theater_ratio(information_foraging_theory, 0.08).
narrative_ontology:constraint_metric(information_foraging_theory, theater_ratio, 0.08).

% --- Structural properties (missing) ---
% Beneficiary: UX designers and platform architects who use IFT's
% "information scent" predictions to optimize navigation and engagement.
narrative_ontology:constraint_beneficiary(information_foraging_theory, ux_designers_and_platform_architects).

% Victim: Users trapped in attention-farming loops where IFT is weaponized
% to maximize "time on site" via continuous micro-scents (Perspective 3).
narrative_ontology:constraint_victim(information_foraging_theory, attention_farmed_users).

% NOTE: requires_active_enforcement is NOT declared. IFT describes an
% evolved biological heuristic — it emerges naturally from evolutionary
% energy conservation (line 72: domain_priors:emerges_naturally).

% --- Analytical perspective classification (missing) ---
% χ = 0.20 × 1.15 (analytical) × 1.2 (global) = 0.276
% ε = 0.20 ≤ 0.45, χ = 0.276 ≤ 0.35, theater = 0.08 → Rope
% The analytical observer sees IFT as a coordination mechanism: an
% evolutionary cognitive tool that enables efficient information navigation.
constraint_indexing:constraint_classification(information_foraging_theory, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- Omega variable (missing) ---
omega_variable(
    omega_ift_weaponization_threshold,
    'At what point does platform optimization of information scent cross from coordination (Rope) to extraction (Snare)?',
    'Compare user goal-completion rates vs time-on-site metrics across optimized vs non-optimized platforms',
    'If scent serves goals: permanent Rope. If scent traps attention: Snare for powerless users, warranting reclassification.',
    confidence_without_resolution(medium)
).
