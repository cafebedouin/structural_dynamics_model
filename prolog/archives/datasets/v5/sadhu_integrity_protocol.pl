% ============================================================================
% CONSTRAINT STORY: sadhu_integrity_protocol
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Sadhu’s Sugar" (Adapted from Gandhi Story)
% ============================================================================

:- module(sadhu_integrity_protocol, []).

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
 * * constraint_id: sadhu_integrity_protocol
 * human_readable: The Integrity Requirement (Sadhu's Sugar)
 * domain: social/psychological/ethical
 * temporal_scope: Perennial / Folk Narrative
 * spatial_scope: Local (Village / Interpersonal)
 * * SUMMARY:
 * This constraint mandates that an advisor must embody their own counsel before 
 * delivering it to others. In the story, a Sadhu delays giving 
 * advice to a boy for two weeks because he was still consuming sugar himself. 
 * It posits that effective social coordination (advice) requires internal synchronization (integrity).
 * * KEY AGENTS:
 * - The Sadhu (Institutional/Analytical): The authority figure who enforces a delay 
 * on himself to ensure congruent delivery.
 * - The Mother (Individual Powerless): Seeking to use external authority to solve 
 * a domestic health issue; subject to the Sadhu's two-week delay.
 * - The Son (Individual Powerless): The subject of the advice who promises to 
 * change based on the weight of the Sadhu's character.
 * * NARRATIVE ARC:
 * A mother seeks to "fix" her son's behavior through the Sadhu's authority. 
 * The Sadhu introduces a temporal constraint (two weeks), which functions as a 
 * "Mountain" to the mother but a "Rope" for his own spiritual and social 
 * efficacy. The resolution reveals that the delay was a 
 * necessary extraction of time to ensure the advice was not hypocritical.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% The Structural Anchor
narrative_ontology:interval(sadhu_integrity_protocol, 0, 10).
narrative_ontology:constraint_claim([sadhu_integrity_protocol], [social_synchronization]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Low-Moderate (0.3). The protocol extracts two weeks of time 
% and effort from the mother and son. However, it is not 
% predatory; it is an investment in the "quality" of the resulting change.
domain_priors:base_extractiveness(sadhu_integrity_protocol, 0.3).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.4). It suppresses the "easy path" of immediate, 
% hypocritical advice, making the "quick fix" invisible or invalid.
domain_priors:suppression_score(sadhu_integrity_protocol, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(sadhu_integrity_protocol, extractiveness, 0.3).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, suppression_requirement, 0.4).

% Enforcement requirements
% Emerges naturally from the Sadhu's internal ethics.
domain_priors:emerges_naturally(sadhu_integrity_protocol).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(sadhu_integrity_protocol, authentic_authority). % The Sadhu's reputation and efficacy.
constraint_victim(sadhu_integrity_protocol, immediate_gratification).   % The mother's desire for an instant solution.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SADHU - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Respected community elder/holy man)
   WHEN: immediate (Two-week period of personal change)
   WHERE: mobile (Has the agency to delay or speak)
   SCOPE: local (Interpersonal advice)
   
   WHY THIS CLASSIFICATION:
   To the Sadhu, integrity is a "Rope"—a functional coordination mechanism. 
   He uses the two-week delay as a tool to align his internal state with his 
   external words, ensuring that his influence is "grounded" in reality.
   
   NARRATIVE EVIDENCE:
   "two weeks ago, I was still eating sugar myself".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    sadhu_integrity_protocol,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MOTHER - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless (Frustrated and seeking help)
   WHEN: immediate (Short-term health concern for her son)
   WHERE: trapped (Bounded by the Sadhu's command to "go home")
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   To the mother, the Sadhu's delay is a "Mountain"—an unchangeable, zero-degree-of-freedom 
   requirement. She is "perplexed" and has no choice but to wait if she wants the 
   holy man's specific blessing for her son.
   
   NARRATIVE EVIDENCE:
   "The woman was perplexed. She took the boy by the hand and went home".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sadhu_integrity_protocol,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SON - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (The subject being "chided" and "hauled")
   WHEN: immediate (The moment of the promise)
   WHERE: trapped (Under the dual pressure of mother and holy man)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the boy, the Sadhu's integrity creates a "Snare." Because the advice is 
   now congruent and backed by the Sadhu's demonstrated sacrifice, the boy's 
   freedom to continue his habit is extracted. The authority 
   is now "inescapable" because it is true.
   
   NARRATIVE EVIDENCE:
   "The boy nodded and promised he would not continue this habit any longer".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sadhu_integrity_protocol,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(sadhu_integrity_protocol, E),
    E > 0.2,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(sadhu_integrity_protocol_tests).

test(multi_perspective_integrity) :-
    % Sadhu sees a Rope (Coordination)
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, rope, context(institutional, immediate, mobile, local)),
    % Mother sees a Mountain (Unchangeable delay)
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, mountain, context(powerless, immediate, trapped, local)),
    % Verify they differ
    Type1 \= Type2.

test(power_extractiveness_integrity) :-
    % The powerless son experiences the "Snare" of authoritative change.
    ContextPowerless = context(powerless, immediate, trapped, local),
    % The Sadhu (Institutional) benefits from the authentic status.
    ContextPowerful = context(institutional, immediate, mobile, local),
    constraint_indexing:extractiveness_for_agent(sadhu_integrity_protocol, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(sadhu_integrity_protocol, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(sadhu_integrity_protocol_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.3):
 * Reasoning: While integrity is a "gift," the protocol involves a mandatory 
 * extraction of time and patience from the seeker. 
 * The son also loses his "habit," which is a personal extraction of pleasure.
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the Sadhu (Rope) with the Mother (Mountain) to show how an 
 * internal ethical tool for one is an external, unyielding law for another.
 * * 3. CLASSIFICATION RATIONALE:
 * Sadhu -> Rope: He uses the delay to coordinate his self-identity.
 * Mother -> Mountain: She lacks the "exit options" to skip the delay.
 * Son -> Snare: The "truth" of the Sadhu's integrity tightens the social 
 * demand on the boy to change.
 * * 4. AMBIGUITIES:
 * The primary ambiguity is whether the Sadhu's internal change *causally* * impacted the boy, or if the boy was just impressed by the story of the delay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sadhu_influence_causality,
    "Does the boy's compliance stem from the Sadhu's internal change (Mountain) 
     or from the *story* of the Sadhu's change (Rope)?",
    resolution_mechanism("Comparison of change-rates in subjects receiving advice from congruent advisors vs. hypocritical advisors whose hypocrisy is hidden"),
    impact("If internal change matters: Integrity is a biological law. If only the story matters: Integrity is a narrative Scaffold."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Hypocritical Advice
 * Viability: Very common; tell the boy to stop while you keep eating.
 * Suppression: Explicitly rejected by the Sadhu as ineffective or unethical 
 * for a "holy man".
 * * ALTERNATIVE 2: Pure Authority
 * Viability: Demand obedience based on social rank regardless of behavior.
 * Suppression: Shunted in the moral of the story: "If you are going to give 
 * advice, take it yourself first".
 * * CONCLUSION:
 * The existence of Alternative 1 (Hypocrisy) is what makes the protocol 
 * a "Rope." Without the integrity requirement, the advice becomes a "Snare" 
 * of potentially hollow social control.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * * 1. Load: ?- [sadhu_integrity_protocol].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(sadhu_integrity_protocol).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
