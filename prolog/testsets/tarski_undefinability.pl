% ============================================================================
% CONSTRAINT STORY: tarski_undefinability
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Alfred Tarski (1933) / Philosophy of Language and Logic
% ============================================================================

:- module(constraint_tarski_undefinability, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tarski_undefinability
 * human_readable: Tarski's Undefinability Theorem
 * domain: technological/logic/mathematics
 * temporal_scope: Permanent (Universal Laws of Information)
 * spatial_scope: Global (Formal Languages)
 * * SUMMARY:
 * Tarski's Undefinability Theorem states that "truth" in a sufficiently powerful 
 * formal language cannot be defined within that same language. Any attempt to 
 * create a universal "Truth Predicate" for a language L within L itself results 
 * in a Liar-style paradox. This forces a hierarchy of languages where "Truth" 
 * for Language L can only be defined in a more expressive Meta-Language M.
 * * KEY AGENTS:
 * - The Foundationalist: Seeking a single, self-contained system of total truth.
 * - The System Architect: Designing multi-layered software or logical stacks.
 * - The Infinite Climber: An agent forced to move to higher meta-levels to 
 * achieve semantic clarity.
 * * NARRATIVE ARC:
 * The theorem functions as a hard ceiling (Mountain) for self-contained logic. 
 * To move forward, agents must build a "Rope" of meta-languages (Tarski's 
 * hierarchy). However, for those trapped in a single layer of reality with 
 * no "Meta" access, the theorem acts as a "Snare," making certain truths 
 * fundamentally unspeakable.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration and script extraction
narrative_ontology:interval(tarski_interval, 0, 10).
narrative_ontology:constraint_claim(tarski_undefinability, tangled_rope).
domain_priors:requires_active_enforcement(tarski_undefinability).

% Base extractiveness: 0.2 (Low)
% Rationale: It extracts "semantic closure" but doesn't steal resources.
% It forces energy into the creation of Meta-Levels.
domain_priors:base_extractiveness(tarski_undefinability, 0.2).

% Suppression score: 0.3 (Low-Moderate)
% Rationale: Alternatives (like paraconsistent logic) are visible but 
% the theorem remains a dominant boundary in classical logic.
domain_priors:suppression_score(tarski_undefinability, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(tarski_undefinability, extractiveness, 0.2).
narrative_ontology:constraint_metric(tarski_undefinability, suppression_requirement, 0.3).

% Enforcement: Emerges naturally from the structure of formal syntax.
domain_priors:emerges_naturally(tarski_undefinability).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(tarski_undefinability, meta_language_architects).
narrative_ontology:constraint_victim(tarski_undefinability, [monistic_philosophers, self_contained_ai]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MATHEMATICIAN - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The objective observer of formal systems)
   WHEN: civilizational (Permanent mathematical reality)
   WHERE: trapped (Logic cannot bypass its own syntactic limits)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the mathematician, the theorem is a Mountain. It is an unchangeable peak 
   in the landscape of logic. You cannot "legislate" a truth predicate into a 
   language without breaking the law of non-contradiction. It is a physical 
   limit of information processing.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tarski_undefinability,
    tangled_rope,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COMPILER ARCHITECT - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power over a software stack)
   WHEN: biographical (Achieving goals within a project lifetime)
   WHERE: arbitrage (Can move between language levels/stacks)
   SCOPE: national (The reach of their specific ecosystem)
   
   WHY THIS CLASSIFICATION:
   For the architect, the theorem is a Rope. It provides a coordination 
   mechanism for building secure systems. By separating the "Object Language" 
   (the code) from the "Meta-Language" (the compiler/type checker), they 
   can safely define correctness without risk of infinite recursion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tarski_undefinability,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE TRAPPED SUBJECT - SNARE
   --------------------------------------------------------------------------
   
   WHO: powerless (A subject inside a closed system)
   WHEN: immediate (Living within a single discursive frame)
   WHERE: trapped (Has no access to the "Meta" level)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For an agent trapped in a single system (like an AI with no external 
   sensors or a human in a totalizing ideology), the theorem is a Snare. 
   They can sense that "truth" exists but are syntactically barred from 
   defining it. The harder they try to ground their reality, the tighter 
   the paradoxes pull, leading to cognitive dissonance or systemic collapse.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tarski_undefinability,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(tarski_undefinability_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(tarski_undefinability, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(tarski_undefinability, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(tarski_undefinability, Type3, context(agent_power(powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_semantic_extraction) :-
    % Institutional actors (architects) extract utility by building meta-levels.
    % Powerless subjects suffer the inability to name their truth.
    domain_priors:base_extractiveness(tarski_undefinability, Score),
    Score < 0.3.

test(meta_level_arbitrage) :-
    % If you have arbitrage (access to Meta-Language), it's a Rope.
    constraint_indexing:constraint_classification(tarski_undefinability, rope, context(_, _, arbitrage, _)).

:- end_tests(tarski_undefinability_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.2):
 * Reasoning: Semantic undefinability doesn't "steal" wealth, but it does 
 * extract "Closure." It prevents a system from being self-contained, 
 * which is a low-level structural extraction.
 * * 2. SUPPRESSION SCORE (0.3):
 * Reasoning: Alternatives like Kripke's theory of truth or paraconsistent 
 * logics are known and studied, but Tarski's "hierarchy" remains the 
 * default "Mountain" for classical formalists.
 * * 3. PERSPECTIVE SELECTION:
 * The Analyst sees a Mountain (fact of logic).
 * The Architect sees a Rope (method for building levels).
 * The Subject sees a Snare (inability to resolve their own truth).
 * * 4. AMBIGUITIES:
 * - Resolved the "Snare" aspect by framing it as a linguistic trap for 
 * agents without "Meta-Expressivity."
 * * 5. CONFIDENCE:
 * High: Mathematical basis.
 * Medium: Extent to which "trapped subjects" feel this as a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_language_meta_closure,
    "Is human 'Natural Language' the ultimate Meta-Language that can define its own truth, or is it merely one layer in an infinite hierarchy we cannot see?",
    resolution_mechanism("Discovery of a definitive Truth Predicate in linguistics that does not lead to paradox"),
    impact("If Closure: Tarski's Law is a Rope for AI but not for Humans. If Infinite: We are all trapped in a Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Paraconsistent Logic (Dialetheism)
 * Viability: Allows for some true contradictions, potentially defining 
 * truth within the same language.
 * Suppression: High. Mainstream mathematics views contradictions as 
 * systemic failure.
 * * ALTERNATIVE 2: Kripkean Truth (Fixed-point semantics)
 * Viability: Allows a truth predicate to be "partially" defined.
 * * CONCLUSION:
 * The existence of paraconsistent alternatives suggests Tarski's Theorem 
 * is a "Snare" used by classical logicians to enforce a specific 
 * hierarchical order of reality.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_tarski_undefinability].
 * 2. Multi-perspective: ?- multi_index_report(tarski_undefinability).
 * 3. Run tests: ?- run_tests(tarski_undefinability_tests).
 */

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
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Technical constraint — mostly substantive, minimal implementation theater
domain_priors:theater_ratio(tarski_undefinability, 0.01).
narrative_ontology:constraint_metric(tarski_undefinability, theater_ratio, 0.01).
