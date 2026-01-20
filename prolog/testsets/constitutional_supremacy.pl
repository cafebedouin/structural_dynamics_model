% ============================================================================
% CONSTRAINT STORY: constitutional_supremacy
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: constitutional_supremacy
 * human_readable: The Supremacy of Written Constitutions
 * domain: legal/political
 * temporal_scope: 1803 - Present
 * spatial_scope: National (United States / Constitutional Democracies)
 * * SUMMARY:
 * Established by Chief Justice John Marshall, this constraint posits that the 
 * Constitution is the "paramount law of the nation," and any act of the 
 * legislature repugnant to it is void. It creates a hierarchy where the 
 * judiciary must prioritize the "Mountain" of the Constitution over the 
 * "Rope" of legislative statutes.
 * * KEY AGENTS:
 * - John Marshall: The Institutional Architect who defines the "duty of the 
 * judicial department to say what the law is."
 * - William Marbury: The "Individual Moderate" seeking a legal remedy for 
 * a vested right.
 * - James Madison/The Executive: The powerful agent whose actions are 
 * constrained by the judicial interpretation of the law.
 * * NARRATIVE ARC:
 * The case arises from a political dispute over judicial commissions. Marshall 
 * avoids a direct power struggle with the Executive by declaring a specific 
 * law (Section 13 of the Judiciary Act) unconstitutional. This act of 
 * "self-restraint" actually cements the Judiciary's power to constrain 
 * all future legislative acts.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(marbury_v_madison_analysis, 0, 10).
narrative_ontology:constraint_claim(constitutional_supremacy, mountain).

% Base extractiveness score: 0.3
% Rationale: The constraint primarily coordinates power rather than 
% extracting it, though it "extracts" finality from the legislative branch.
domain_priors:base_extractiveness(constitutional_supremacy, 0.3).

% Suppression score: 0.95
% Rationale: Marshall argues that the Constitution is either the paramount 
% law or it is "solemn mockery." There is no middle ground allowed.
domain_priors:suppression_score(constitutional_supremacy, 0.95).

% Enforcement requirements: Requires judicial vigilance and "active 
% enforcement" of the hierarchy of laws.
domain_priors:requires_active_enforcement(constitutional_supremacy).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(constitutional_supremacy, extractiveness, 0.3).
narrative_ontology:constraint_metric(constitutional_supremacy, suppression_requirement, 0.95).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(constitutional_supremacy, [judicial_authority, individual_liberty]).
constraint_victim(constitutional_supremacy, [legislative_supremacy, executive_discretion]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CITIZEN (Marbury) - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - A citizen with a vested legal right.
   WHEN: immediate - The loss of a commission/job.
   WHERE: trapped - Bound by the "rule of law" which must provide a remedy.
   SCOPE: national - The law of the land.
   
   WHY THIS CLASSIFICATION:
   To the individual seeking a right, the Constitution is an unchangeable 
   Mountain. It is the fundamental law that protects their specific commission 
   against the "arbitrary" will of the Executive.
   
   NARRATIVE EVIDENCE:
   "The very essence of civil liberty certainly consists in the right of 
   every individual to claim the protection of the laws."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_supremacy,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(constitutional_supremacy, individual_liberty),
        constraint_victim(constitutional_supremacy, []),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(constitutional_supremacy, S),
    S > 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE JUDICIARY (Marshall) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - The department that "must of necessity expound 
   and interpret."
   WHEN: historical - Building a "government of laws, and not of men."
   WHERE: arbitrage - Balancing the different departments of government.
   SCOPE: national - The supreme law.
   
   WHY THIS CLASSIFICATION:
   To the Judge, the Constitution is a "Rope"—a functional mechanism 
   for coordinating the republic. It is a tool they must use to "discharge 
   their duties agreeably to the constitution."
   
   NARRATIVE EVIDENCE:
   "It is emphatically the province and duty of the judicial department 
   to say what the law is."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_supremacy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(constitutional_supremacy, judicial_authority),
        constraint_victim(constitutional_supremacy, []),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LEGISLATURE (Congress) - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional - The rule-making body whose acts are invalidated.
   WHEN: immediate - A specific statute is struck down.
   WHERE: constrained - Prevented from overstepping constitutional bounds.
   SCOPE: local - A specific act (e.g., Section 13).
   
   WHY THIS CLASSIFICATION:
   From the perspective of a legislature that believes it has the power to 
   act, judicial review is a "Noose." It is a coercive mechanism that 
   extracts their finality and subjects them to an unelected "analytical" 
   oversight.
   
   NARRATIVE EVIDENCE:
   "To what purpose are powers limited... if these limits may, at any 
   time, be passed by those intended to be restrained?"
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    constitutional_supremacy,
    noose,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(constitutional_supremacy, judicial_authority),
        constraint_victim(constitutional_supremacy, legislative_supremacy),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(constitutional_supremacy, E),
    E > 0.2,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(marbury_tests).

test(hierarchy_of_laws) :-
    % Verify the Constitution (Mountain) trumps Statute (Noose/Rope)
    constraint_indexing:constraint_classification(constitutional_supremacy, mountain, context(agent_power(individual_moderate), _, _, _, _, _)),
    domain_priors:suppression_score(constitutional_supremacy, S),
    S > 0.9.

test(institutional_agency) :-
    % The Judge sees the mechanism as a functional Rope
    constraint_indexing:constraint_classification(constitutional_supremacy, rope, context(agent_power(institutional), _, arbitrage, _, _, _)).

:- end_tests(marbury_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: Marshall frames the Constitution as a "Mountain" 
 * to justify why the Court *cannot* act on an unconstitutional law. 
 * This creates a "Rope" (Judicial Review) that the Court uses to manage 
 * the other branches.
 * 2. PERSPECTIVAL GAP: The gap here is between the "Powerless" citizen 
 * who needs the Constitution as an immutable shield (Mountain) and the 
 * "Powerful" Executive/Legislature who find their agency restricted 
 * (Noose) by the Court's interpretation.
 * 3. EXTRACTIVENESS: Low (0.3). While it restricts power, it does so to 
 * preserve the "security" of the whole, rather than for the direct 
 * enrichment of a specific class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_neutrality,
    "Is the Court truly a neutral observer (Analytical) or a participant in the power struggle (Institutional)?",
    resolution_mechanism("Requires evaluation of future case law to see if review is used for coordination or partisan extraction."),
    impact("If Institutional: The 'Rope' of Judicial Review becomes a 'Noose' for the public."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Legislative Supremacy (The British Model):
 * Viability: Historically viable in many nations.
 * Suppression: Marshall rejects this as making the Constitution a "absurd 
 * attempt... to limit a power, in its own nature illimitable."
 * EVIDENCE: Marshall argues that without judicial review, written 
 * constitutions are "solemn mockery."
 * * CONCLUSION: 
 * The rejection of Legislative Supremacy converts the Constitution from a 
 * "Rope" (a social agreement) into a "Mountain" (a natural law hierarchy).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
