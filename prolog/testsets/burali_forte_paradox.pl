% ============================================================================
% CONSTRAINT STORY: burali_forti_paradox
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Set Theory / Cesare Burali-Forti (1897)
% ============================================================================

:- module(constraint_burali_forti_paradox, []).

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
 * * constraint_id: burali_forti_paradox
 * human_readable: Burali-Forti Paradox
 * domain: technological/mathematics/logic
 * temporal_scope: Permanent (Universal Laws of Formal Systems)
 * spatial_scope: Global (Set-Theoretic Foundations)
 * * SUMMARY:
 * The Burali-Forti paradox demonstrates that the collection of all ordinal numbers 
 * cannot itself be a set. If it were, it would have an ordinal number greater 
 * than every ordinal in the collection, leading to a contradiction. It 
 * represents the first formal proof that "totalities" in naive set theory 
 * are logically impossible.
 * * KEY AGENTS:
 * - The Set Theorist: Seeking a consistent foundation for all of mathematics.
 * - The Formal System (ZFC/NBG): The administrative structure that prevents 
 * "Set" status for such totalities.
 * - The Naive Mathematician: An agent attempting to work with unrestricted 
 * comprehension.
 * * NARRATIVE ARC:
 * The paradox acts as a hard boundary (Mountain) for the reach of sets. To 
 * maintain consistency, formal systems treat the "All-Ordinals" as a Proper 
 * Class (Rope), a coordination tool that allows use without membership. 
 * For those seeking a single unified "Set of All," the paradox is a Snare 
 * that collapses their foundational system.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural anchor
narrative_ontology:interval(burali_forti_interval, 0, 10).
narrative_ontology:constraint_claim(burali_forti_paradox, mountain).

% Base extractiveness score: 0.2 (Low)
% Rationale: It extracts "semantic closure" from naive systems but does not 
% extract physical resources.
domain_priors:base_extractiveness(burali_forti_paradox, 0.2).

% Suppression score: 0.3 (Low-Moderate)
% Rationale: Alternatives like New Foundations (NF) exist and are visible, 
% though the "Proper Class" distinction is the dominant enforcement.
domain_priors:suppression_score(burali_forti_paradox, 0.3).

% Enforcement requirements: Emerges naturally from the definition of ordinals.
domain_priors:emerges_naturally(burali_forti_paradox).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(burali_forti_paradox, extractiveness, 0.2).
narrative_ontology:constraint_metric(burali_forti_paradox, suppression_requirement, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(burali_forti_paradox, axiomatic_set_theorists).
constraint_victim(burali_forti_paradox, naive_comprehension_proponents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SET THEORIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal logical laws.
   WHEN: civilizational - Operates on the scale of eternal mathematical truth.
   WHERE: trapped - Logic cannot bypass the definitions of well-ordering.
   SCOPE: global - Universal applicability to all formal systems.
   
   WHY THIS CLASSIFICATION:
   To the theorist, the paradox is a Mountain. It is an unchangeable feature 
   of the landscape of truth. You cannot "fix" it; you can only map it and 
   build your axioms around it to avoid falling into the contradiction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burali_forti_paradox,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE AXIOMATIC SYSTEM (ZFC) - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power that defines what counts as a "Set."
   WHEN: historical - Maintaining mathematical stability over centuries.
   WHERE: arbitrage - Can define hierarchies (Proper Classes vs Sets) to 
   coordinate information.
   SCOPE: national - The reach of the specific formal logic.
   
   WHY THIS CLASSIFICATION:
   For the formal system, the paradox is a Rope. It is a coordination 
   mechanism that forces the distinction between "Sets" (which can be members) 
   and "Proper Classes" (which cannot). This distinction is a functional 
   tool that prevents the system from "exploding" into triviality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burali_forti_paradox,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE NAIVE MATHEMATICIAN - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the rules of logic without 
   meta-systemic power.
   WHEN: immediate - Today's calculation or proof attempt.
   WHERE: trapped - Once the contradiction is reached, the proof is dead.
   SCOPE: local - Immediate workspace.
   
   WHY THIS CLASSIFICATION:
   For the naive actor, the paradox is a Snare. They attempt to use the 
   seemingly "free" rule of unrestricted comprehension (the ability to form 
   a set from any property). However, as they include more ordinals, the 
   paradoxical logic tightens until their entire foundational proof 
   is strangled by contradiction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burali_forti_paradox,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(burali_forti_paradox_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(burali_forti_paradox, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(burali_forti_paradox, T2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(burali_forti_paradox, T3, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(power_semantic_extraction) :-
    % Powerless naive actors suffer total extraction of proof validity (Snare).
    % Institutional systems manage the extraction via Proper Classes (Rope).
    domain_priors:base_extractiveness(burali_forti_paradox, Score),
    Score >= 0.2.

test(time_immutability_scale) :-
    % Long-term civilizational logic = Mountain.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(burali_forti_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.2):
 * Reasoning: Low. The "extraction" is purely semantic—the loss of 
 * unrestricted comprehension. It doesn't extract material labor.
 * * 2. PERSPECTIVE SELECTION:
 * Analytical (The Scientist), Institutional (The Formal System), and 
 * Individual Powerless (The Naive Actor) were chosen to reflect the 
 * standard "Set vs Class" resolution in mathematics.
 * * 3. CLASSIFICATION RATIONALE:
 * Individual Powerless → Snare: Because naive comprehension leads 
 * unavoidably to a "trap" where all statements become provable (Trivialism).
 * * 4. AMBIGUITIES:
 * - Resolved the "trap" by acknowledging that while one can exit ZFC 
 * to another logic, one cannot exit the Burali-Forti result within 
 * any system that respects standard ordinal definitions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    class_vs_set_metaphysics,
    "Are 'Proper Classes' a real mathematical landscape (Mountain) or merely a linguistic scaffold (Rope) to avoid the Snare?",
    resolution_mechanism("Development of a grand unified theory of categories vs sets"),
    impact("If Mountain: The hierarchy of size is a physical limit. If Rope: It's just a bug in our current notation."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Quine's New Foundations (NF)
 * Viability: High. It allows a universal set but restricts the types of 
 * formulas used to define sets (stratification).
 * Suppression: Moderate. NF is well-known but rarely used in "mainstream" 
 * math because stratification is cognitively more "expensive" than ZFC.
 * * CONCLUSION:
 * The existence of NF as a "Rope" alternative suggests that the ZFC 
 * "Mountain" is partially a choice of convenience, though the underlying 
 * paradox remains a structural limit for naive systems.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * * 1. Load: ?- [constraint_burali_forti_paradox].
 * 2. Multi-perspective: ?- multi_index_report(burali_forti_paradox).
 * 3. Run tests: ?- run_tests(burali_forti_paradox_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(burali_forte_paradox, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(burali_forte_paradox, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(burali_forte_paradox, snare, agent_power(individual_powerless)).
