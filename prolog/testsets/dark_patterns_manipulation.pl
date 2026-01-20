% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: dark_patterns_manipulation
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Brignull, H. (2010). Dark Patterns. / FTC Enforcement Guidelines.
% ============================================================================

:- module(constraint_dark_patterns, []).

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
 * * constraint_id: dark_patterns_manipulation
 * human_readable: Dark Patterns (Interface Coercion)
 * domain: technological/economic/psychological
 * temporal_scope: Immediate
 * spatial_scope: Global
 * * SUMMARY:
 * Dark patterns exploit cognitive biases (like loss aversion or the default 
 * effect) to steer user behavior toward outcomes that benefit the platform 
 * but harm the user. They are the "shadow side" of Information Foraging Theory.
 * * KEY AGENTS:
 * - The User: The "forager" whose cognitive heuristics are being weaponized.
 * - The Growth Hacker: The designer tasked with maximizing "conversion" at any cost.
 * - The Regulator: The agent attempting to ban deceptive design (e.g., California Privacy Rights Act).
 * * NARRATIVE ARC:
 * Dark patterns function as "Fake Information Scent." They lure the user 
 * into a matching market where the "Match" is involuntary. It turns the 
 * interface from a transparent tool into a psychological Noose.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(dark_patterns_manipulation, 0, 10).
narrative_ontology:constraint_claim(dark_patterns_manipulation, noose).

% Base extractiveness score (0.85)
% High; they are purely designed to extract value (money, data, attention) 
% through deception rather than exchange.
domain_priors:base_extractiveness(dark_patterns_manipulation, 0.85).

% Suppression score (0.9)
% High; the "Exit Option" (e.g., the 'Cancel Subscription' button) is 
% intentionally hidden or made difficult to find.
domain_priors:suppression_score(dark_patterns_manipulation, 0.9).

% Enforcement: Requires active enforcement (Corporate deceptive practices).
domain_priors:requires_active_enforcement(dark_patterns_manipulation).

% Metrics
narrative_ontology:constraint_metric(dark_patterns_manipulation, extractiveness, 0.85).
narrative_ontology:constraint_metric(dark_patterns_manipulation, suppression_requirement, 0.9).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(dark_patterns_manipulation, predatory_platforms).
constraint_victim(dark_patterns_manipulation, vulnerable_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CAPTIVE CUSTOMER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped (in a 'Roach Motel' design)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a user trying to delete an account, the interface is a "Noose." 
   The "Confirm Deletion" button is grayed out, hidden behind five menus, 
   or requires a physical phone call. The platform strangles their 
   exit option to keep their "Match" metric high.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dark_patterns_manipulation,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PRODUCT MANAGER - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: biographical (quarterly targets)
   WHERE: mobile
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   To the product manager, these patterns are a "Rope." They are the 
   coordination tools used to "nudge" users toward the "desired" 
   business outcome. They see it as a necessary strategy for survival 
   in a hyper-competitive attention economy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dark_patterns_manipulation,
    rope,
    context(agent_power(institutional), time_horizon(biographical), exit_options(mobile), spatial_scope(regional))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ETHICAL DESIGNER - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The ethical observer sees these patterns as a "Mountain" of 
   psychological vulnerability. Human cognitive biases are immutable. 
   The fact that they *can* be exploited is a feature of our biological 
   nature that designers must either respect or exploit.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dark_patterns_manipulation,
    mountain,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
) :- !.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
