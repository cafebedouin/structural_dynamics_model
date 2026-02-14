% ============================================================================
% CONSTRAINT STORY: constitutional_supremacy
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Marbury v. Madison (1803)
% ============================================================================

:- module(constraint_constitutional_supremacy, []).

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
 * 
 * constraint_id: constitutional_supremacy
 * human_readable: The Supremacy of Written Constitutions
 * domain: legal/political
 * temporal_scope: 1803 - Present
 * spatial_scope: National (United States / Constitutional Democracies)
 * 
 * SUMMARY:
 * Established by Chief Justice John Marshall in Marbury v. Madison (1803), 
 * this constraint posits that the Constitution is the "paramount law of the nation,"
 * and any act of the legislature repugnant to it is void. It creates a hierarchy
 * where the judiciary must prioritize the "Mountain" of the Constitution over
 * the "Rope" of legislative statutes.
 * 
 * KEY AGENTS:
 * - John Marshall (Institutional): The Architect who defines judicial review.
 * - The Citizen (Individual Powerless): Subject to the laws and constitutional protections.
 * - The Legislature (Individual Powerful): The rule-making body whose acts can be invalidated.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(constitutional_supremacy, 0, 10).
narrative_ontology:constraint_claim(constitutional_supremacy, tangled_rope).

% Base extractiveness: 0.3.
% The constraint primarily coordinates power rather than extracting it, though
% it "extracts" finality from the legislative branch.
domain_priors:base_extractiveness(constitutional_supremacy, 0.3).

% Suppression score: 0.95.
% Marshall argues that the Constitution is either the paramount law or it is
% "solemn mockery." There is no middle ground allowed, strongly suppressing alternatives.
domain_priors:suppression_score(constitutional_supremacy, 0.95).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(constitutional_supremacy, extractiveness, 0.3).
narrative_ontology:constraint_metric(constitutional_supremacy, suppression_requirement, 0.95).

% Enforcement: Requires judicial vigilance and "active enforcement" of the hierarchy of laws.
domain_priors:requires_active_enforcement(constitutional_supremacy).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(constitutional_supremacy, judicial_authority).
narrative_ontology:constraint_victim(constitutional_supremacy, legislative_supremacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CITIZEN - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Subject to the laws and constitutional protections)
   WHEN: biographical (Relies on the Constitution for long-term stability and rights)
   WHERE: trapped (Bound by the "supreme Law of the Land")
   
   WHY THIS CLASSIFICATION:
   To the ordinary citizen, the Constitution is an unchangeable 'Mountain'. 
   It represents the fundamental law that provides a stable framework for their
   rights and duties, protecting them from arbitrary legislative or executive
   action. Its supremacy is a fixed, immutable fact of the legal landscape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_supremacy,
    tangled_rope,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE JUDICIARY (JOHN MARSHALL) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The department that "must of necessity expound and interpret")
   WHEN: historical (Building a "government of laws, and not of men")
   WHERE: arbitrage (Balancing the different departments of government)
   
   WHY THIS CLASSIFICATION:
   To the Judge (e.g., John Marshall), the Constitution is a 'Rope'—a functional
   mechanism for coordinating the republic. It is a tool they must use to
   "discharge their duties agreeably to the constitution" and to define the
   boundaries of governmental power.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_supremacy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LEGISLATURE (CONGRESS) - Snare
   --------------------------------------------------------------------------
   WHO: powerful (The rule-making body whose acts are invalidated)
   WHEN: immediate (A specific statute is struck down)
   WHERE: constrained (Prevented from overstepping constitutional bounds)
   
   WHY THIS CLASSIFICATION:
   From the perspective of a legislature that believes it has the power to 
   act, judicial review is a 'Snare.' It is a coercive mechanism that 
   extracts their finality and subjects them to an unelected "analytical" 
   oversight, strangling their perceived legislative autonomy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_supremacy,
    snare,
    context(
        agent_power(powerful),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(constitutional_supremacy_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(constitutional_supremacy, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_supremacy, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_supremacy, Type3, context(agent_power(powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(constitutional_supremacy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INDIVIDUAL POWERLESS PERSPECTIVE: Added 'The Citizen' as the individual
 *    powerless agent, for whom the Constitution acts as an immutable 'Mountain'
 *    of rights and stability.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Citizen (Mountain): Immutable legal framework for rights.
 *    - Judiciary (Rope): A tool for interpreting and enforcing the law.
 *    - Legislature (Snare): Legislative power constrained by judicial review.
 * 
 * 3. CORE INSIGHT: Constitutional supremacy, as established by Marbury v. Madison,
 *    creates a hierarchical legal structure. What is an immutable 'Mountain' for
 *    the citizen becomes a powerful 'Rope' for the judiciary, but a constraining
 *    'Snare' for the legislature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the long-term balance of power within the constitutional framework.
 */

omega_variable(
    judicial_neutrality,
    "Is the Court truly a neutral arbiter of the Constitution, or does judicial review inherently transform into a 'Rope' of judicial policy-making, potentially becoming a 'Snare' for democratic self-governance?",
    resolution_mechanism("Requires evaluation of future case law to see if review is used primarily for coordination (interpreting) or partisan extraction (legislating from the bench)."),
    impact("If truly neutral: Judicial Review is a 'Mountain' of stability. If partisan: It becomes a 'Snare' for other branches."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Legislative Supremacy (e.g., British Parliamentary System)
 *    Viability: Historically viable in many democratic nations, where the legislature is the ultimate legal authority.
 *    Suppression: Rejected by Marshall, who argued that without judicial review, written constitutions are "solemn mockery," effectively suppressing this alternative in the US context.
 *
 * CONCLUSION:
 * The establishment of constitutional supremacy, through the rejection of
 * legislative supremacy, fundamentally shifted the political landscape. What
 * might be seen as a 'Rope' of democratic action in other systems becomes
 * a 'Snare' for an unchecked legislature in the U.S.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/constitutional_supremacy].
 * 2. Multi-perspective: ?- multi_index_report(constitutional_supremacy).
 * 3. Run tests: ?- run_tests(constitutional_supremacy_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */