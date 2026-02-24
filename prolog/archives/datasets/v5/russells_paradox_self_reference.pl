% ============================================================================
% CONSTRAINT STORY: russells_paradox_self_reference
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Bertrand Russell (1901) / Principles of Mathematics
% ============================================================================

:- module(constraint_russells_paradox, []).

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
 * * constraint_id: russells_paradox_self_reference
 * human_readable: Russell's Paradox (Naive Set Theory Collapse)
 * domain: mathematical/philosophical
 * temporal_scope: 1901 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Logical Foundations)
 * * SUMMARY:
 * Russell's Paradox involves the set of all sets that do not contain themselves. 
 * If such a set exists, it must contain itself if and only if it does not. 
 * This contradiction proved that "Unrestricted Comprehension" in naive set 
 * theory is an impossible foundation, forcing the creation of more complex 
 * axiomatic systems like ZFC or Type Theory.
 * * KEY AGENTS:
 * - The Naive Set (Subject): A powerless logical entity bound by a 
 * self-contradictory definition.
 * - The Foundational Architect (Institutional): An agent (like Russell or Zermelo) 
 * using the paradox as a "Rope" to coordinate the transition to rigorous axioms.
 * - The Classical Logician (Victim): An agent (like Gottlob Frege) whose 
 * lifelong work is "strangled" by the discovery of the paradox.
 * * NARRATIVE ARC:
 * Russell's Paradox is the "Mountain" of logical reality; it is an unyielding 
 * feature of self-referential systems. In mathematical history, it acted as a 
 * "Snare" for Frege's Basic Law V, extracting the possibility of simple, 
 * intuitive foundations (extraction). However, it became a "Rope" for the 
 * next century of logic, allowing researchers to coordinate a "standard of 
 * achievement" for consistency in set theory.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural extraction and indexer coordination
narrative_ontology:interval(russell_era, 1901, 2026).
narrative_ontology:constraint_claim(russells_paradox_self_reference, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.7. The paradox is highly extractive; it "takes" the 
% consistency of any naive system, forcing a massive reinvestment in 
% complex, less intuitive axioms to regain logical "solvency."
domain_priors:base_extractiveness(russells_paradox_self_reference, 0.7).

% Suppression score (0.0-1.0)
% Rationale: 0.4. It suppresses the visibility of "unrestricted comprehension," 
% rendering the idea that "any property can define a set" functionally 
% fraudulent in formal logic.
domain_priors:suppression_score(russells_paradox_self_reference, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(russells_paradox_self_reference, extractiveness, 0.7).
narrative_ontology:constraint_metric(russells_paradox_self_reference, suppression_requirement, 0.4).

% Enforcement: Emerges naturally from the interaction of set membership and negation.
domain_priors:emerges_naturally(russells_paradox_self_reference).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(russells_paradox_self_reference, axiomatic_set_theorists).
constraint_beneficiary(russells_paradox_self_reference, type_theory_developers).
constraint_victim(russells_paradox_self_reference, gottlob_frege). % Famous historical victim.
constraint_victim(russells_paradox_self_reference, naive_intuition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PARADOXICAL SET - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - The set is defined by the rule; it has no agency.
   WHEN: immediate - The contradiction is inherent the moment it is defined.
   WHERE: trapped - Bound within the laws of logic.
   SCOPE: local - Immediate membership property.
   
   WHY THIS CLASSIFICATION:
   For the set itself, the paradox is an absolute Mountain. It cannot 
   "choose" to contain or not contain itself. The arithmetic of its 
   identity is an unyielding law that results in a logical nullity or 
   stasis.
   
   NARRATIVE EVIDENCE:
   "If it is not a member of itself, then by definition it must be a member 
   of itself." (Russell's original letter to Frege).
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    russells_paradox_self_reference,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE FOUNDATION BUILDER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to establish new axioms (ZFC, Type Theory).
   WHEN: biographical - Spanning the multi-decade effort to rebuild logic.
   WHERE: mobile - Can choose different "exits" like stratified types or axioms of specification.
   SCOPE: global - Universal foundation for all mathematics.
   
   WHY THIS CLASSIFICATION:
   For the institutional actor, the paradox is a "Rope"—a functional 
   coordination tool. By recognizing the "Mountain," they coordinate 
   a "standard of achievement" for what constitutes a valid set, pulling 
   the field away from the "Snare" of inconsistency toward a stable, 
   usable framework.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    russells_paradox_self_reference,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CLASSICAL FREGEAN - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the tools of logic but is bound by the contradiction.
   WHEN: historical - Realizing the collapse of a lifelong philosophical project.
   WHERE: constrained - The "exit" requires abandoning the core of their logic.
   SCOPE: global - Affecting the interpretation of all formal logic.
   
   WHY THIS CLASSIFICATION:
   For the logician who built upon unrestricted comprehension, the paradox is 
   a "Snare." It "strangles" their hope for a simple, arithmetic foundation 
   for the world. It extracts the "certainty of reason" (extraction), 
   choking the project just as it reached completion.
   
   NARRATIVE EVIDENCE:
   "Arithmetic totters." (Frege's reaction to Russell's discovery).
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    russells_paradox_self_reference,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(russells_paradox_self_reference, E),
    E >= 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(russells_paradox_tests).

test(multi_perspective_variance) :-
    % Set -> Mountain
    constraint_indexing:constraint_classification(russells_paradox_self_reference, Type1, context(powerless, immediate, trapped, local)),
    % Builder -> Rope
    constraint_indexing:constraint_classification(russells_paradox_self_reference, Type2, context(institutional, biographical, mobile, global)),
    Type1 \= Type2.

test(logic_extraction_penalty) :-
    % Classical logicians experience the "Snare" of high extraction (0.7).
    Context = context(individual_moderate, historical, constrained, global),
    constraint_indexing:extractiveness_for_agent(russells_paradox_self_reference, Context, Score),
    Score >= 0.6.

test(natural_emergence) :-
    domain_priors:emerges_naturally(russells_paradox_self_reference).

:- end_tests(russells_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.7): 
 * I chose 0.7 because Russell's Paradox is the ultimate foundational tax. 
 * It extracts the possibility of an intuitive, self-evident logic and 
 * replaces it with a "tax" of complexity (axioms, types).
 * * 2. PERSPECTIVE SELECTION: 
 * Chose Set (Subject), Builder (User), and Frege (Victim) to show how 
 * a "Mountain" of formal reality becomes a "Snare" for those seeking 
 * simple completeness.
 * * 3. OMEGA IDENTIFICATION: 
 * Formalized the uncertainty of "True Sets"—is there a natural Mountain 
 * or just a series of useful Scaffolds?
 */

% YOUR OMEGAS HERE:
omega_variable(
    set_theoretic_realism,
    "Is there a unique 'True' universe of sets (Mountain) or only a pluralism of consistent Scaffolds (Scaffold)?",
    resolution_mechanism("Investigation into whether large cardinal axioms eventually settle independence results like CH."),
    impact("If Mountain: Logic is objective. If Scaffold: Logic is a tool of coordination (Rope)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Non-well-founded Set Theory
 * Viability: Allowing sets to contain themselves (AFA axiom). 
 * Status: A "Rope" that avoids the specific contradiction by shifting 
 * the "Mountain" of membership.
 * * ALTERNATIVE 2: Quine's New Foundations (NF)
 * Viability: Stratified comprehension. 
 * Suppression: Suppressed by the dominance of ZFC, making it an 
 * "invisible Rope" for most mathematicians.
 * * CONCLUSION:
 * The existence of ZFC as the dominant "Rope" proves that we have 
 * navigated the "Mountain" by actively suppressing simpler, intuitive 
 * (but broken) alternatives.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_russells_paradox].
 * 2. Multi-perspective: ?- multi_index_report(russells_paradox_self_reference).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
