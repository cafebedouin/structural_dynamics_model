% ============================================================================
% CONSTRAINT STORY: liar_paradox
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Classical Logic / Epistemology
% ============================================================================

:- module(constraint_liar_paradox, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_metric/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: liar_paradox
 * human_readable: The Liar Paradox (Self-Referential Inconsistency)
 * domain: analytical/logic
 * temporal_scope: Permanent (Historical to Civilizational)
 * spatial_scope: Universal (Information Systems)
 * * SUMMARY:
 * The constraint emerges from the sentence "This statement is false." If true, it is 
 * false; if false, it is true. This creates a logical "no-go zone" or a structural 
 * limit on formal systems that allow self-reference without hierarchies.
 * * KEY AGENTS:
 * - The Logician: Seeking a consistent, "all-true" foundation for reality.
 * - The Natural Language Speaker: Encountering the "glitch" as a curiosity or error.
 * - The Formal System (e.g., Peano Arithmetic): Subject to Goedel's incompleteness.
 * * NARRATIVE ARC:
 * The paradox functions as an immovable boundary in logic. Attempts to "break" it 
 * lead to the development of higher-order logics (Tarski's hierarchy) or the 
 * acceptance of incompleteness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(liar_paradox_interval, 0, 10).
narrative_ontology:constraint_claim(liar_paradox, mountain).

% Base extractiveness: 0.1 (Low)
% Rationale: The paradox doesn't "steal" value, but it does consume cognitive 
% processing time and stalls automated theorem provers.
domain_priors:base_extractiveness(liar_paradox, 0.1).

% Suppression score: 0.2 (Low)
% Rationale: Alternatives (like paraconsistent logic) are widely known but 
% the paradox itself is impossible to "suppress" out of existence; it is a feature 
% of any sufficiently expressive system.
domain_priors:suppression_score(liar_paradox, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(liar_paradox, extractiveness, 0.1).
narrative_ontology:constraint_metric(liar_paradox, suppression_requirement, 0.2).

% Enforcement requirements: Emerges naturally from the structure of language/logic.
domain_priors:emerges_naturally(liar_paradox).

% Metrics for Executive Summary
% Beneficiaries: Meta-mathematicians (provides a career/domain of study).
constraint_beneficiary(liar_paradox, analytical_observers).

% Victims: Consistentists (those who require a single, universal, non-contradictory foundation).
constraint_victim(liar_paradox, institutional_logicians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LOGICIAN (Institutional) - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: Institutional/Analytical power.
   WHEN: Civilizational (the laws of logic are seen as eternal).
   WHERE: Trapped (cannot simply "ignore" the law of non-contradiction).
   
   WHY THIS CLASSIFICATION:
   For the institutional logician, the paradox is a Mountain. It is an unchangeable 
   feature of the landscape of truth. You cannot "legislative" it away; you can 
   only build around it or map its peaks.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    liar_paradox,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NATURAL SPEAKER (Individual Powerless) - ROPE
   --------------------------------------------------------------------------
   WHO: Individual powerless (subject to the "rules" of grammar).
   WHEN: Immediate/Biographical.
   WHERE: Mobile (can simply stop saying the sentence).
   
   WHY THIS CLASSIFICATION:
   To a normal person, the paradox is a Rope. It is a coordination mechanism 
   of language that occasionally gets tangled. It is useful for understanding 
   limits but doesn't stop them from speaking. They can "exit" the paradox 
   by choosing different words.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    liar_paradox,
    rope,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE REBEL LOGICIAN / DIALETHEIST - SNARE
   --------------------------------------------------------------------------
   WHO: Analytical/Individual Moderate.
   WHEN: Historical.
   WHERE: Constrained (by the refusal of others to accept contradictions).
   
   WHY THIS CLASSIFICATION:
   For one seeking a truly unified theory that includes self-reference, the 
   Paradox acts as a Snare. It is used by the "Institutional Logicians" to 
   strangle alternative logics (like Dialetheism) by claiming they are "nonsensical" 
   or "extractive" of truth value.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    liar_paradox,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(liar_paradox_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(liar_paradox, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(liar_paradox, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liar_paradox, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(immutability_scaling) :-
    % Institutional view sees it as eternal/unchangeable (Mountain)
    constraint_indexing:constraint_classification(liar_paradox, mountain, context(agent_power(institutional), civilizational, trapped, _)).

test(exit_option_shift) :-
    % If you have exit options (can ignore the logic), it's just a Rope.
    constraint_indexing:constraint_classification(liar_paradox, rope, context(_, _, mobile, _)).

:- end_tests(liar_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.1): Low, because a paradox doesn't "own" anything, 
 * though it extracts "compute" and "certainty."
 * 2. PERSPECTIVES: Chose Institutional (Logic as Law), Individual (Logic as Tool), 
 * and Analytical (Logic as Conflict).
 * 3. SNARE ARGUMENT: Usually Snares involve coercion. Here, the "coercion" 
 * is the social and academic exclusion of those who suggest logic can 
 * be contradictory (Dialetheism).
 * * CONFIDENCE: High on the classification variance; Medium on the "Snare" 
 * categorization for the rebel logician.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    is_logic_natural,
    "Is logic a natural law (Mountain) or a human-encoded coordination system (Rope)?",
    resolution_mechanism("Discovery of non-human intelligence with fundamentally different cognitive processing"),
    impact("If Mountain: The Liar is a physical limit. If Rope: The Liar is a bug in our specific code."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Tarski's Truth Hierarchy
 * Viability: High (Standard in formal semantics).
 * Suppression: Moderate (Makes language cumbersome).
 * * ALTERNATIVE 2: Dialetheism (True Contradictions)
 * Viability: Low-Moderate (Graham Priest).
 * Suppression: High (Rejected by mainstream mathematics/science).
 * * CONCLUSION: 
 * The existence of Dialetheism shifts the Liar from a "natural law" (Mountain) 
 * to a "Snare" for those who want to use it to invalidate non-classical systems.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_liar_paradox].
 * 2. Run tests: ?- run_tests(liar_paradox_tests).
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
% Structural constraint in analytical domain — low theater, high substance
domain_priors:theater_ratio(liar_paradox, 0.0).
narrative_ontology:constraint_metric(liar_paradox, theater_ratio, 0.0).
