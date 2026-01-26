% ============================================================================
% CONSTRAINT STORY: theory_of_visitors
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Sam Lansky / cafebedouin.org, "The Theory of Visitors"
% ============================================================================

:- module(constraint_theory_of_visitors, []).

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
 * 
 * constraint_id: theory_of_visitors
 * human_readable: The Theory of Visitors (Relationship Transience)
 * domain: social/psychological
 * temporal_scope: Perennial / Biographical
 * spatial_scope: Global / Interpersonal
 * 
 * SUMMARY:
 * This constraint defines all human relationships as inherently transient "visitors" who 
 * arrive for a limited time and inevitably depart. It posits a 
 * fundamental trade-off: visitors take something irreplaceable from the agent 
 * upon departure, but leave "souvenirs" (memories/lessons) that can be kept forever.
 * 
 * KEY AGENTS:
 * - The Host (Individual Powerless): The agent who must navigate the "arrival" and "surrender" to the "departure" of others.
 * - The Social Media Platform / Dating App (Institutional): Facilitates quick connections and easy departures.
 * - The Stoic / The Navigator (Individual Moderate): The agent who uses the theory to welcome and surrender to the flux of life.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(theory_of_visitors, 0, 10).
narrative_ontology:constraint_claim(theory_of_visitors, tangled_rope).

% Base extractiveness: 0.65.
% Visitors "take something from you... that you can’t ever get back," liquidating a portion of the host's being.
domain_priors:base_extractiveness(theory_of_visitors, 0.65).

% Suppression score: 0.5.
% The "Theory of Visitors" suppresses the illusion of permanence and the possibility of "forever" relationships to encourage "surrender".
domain_priors:suppression_score(theory_of_visitors, 0.5).

% Enforcement: Emerges naturally from the human condition of mortality and social flux.
domain_priors:emerges_naturally(theory_of_visitors).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(theory_of_visitors, emotional_resilience).
constraint_victim(theory_of_visitors, relational_permanence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MOURNER / THE BEREFT - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Target of the "part that sucks")
   WHEN: immediate (The moment of "departure" or being "stabbed in the back")
   WHERE: trapped (Bounded by the loss of "something that you can't ever get back")
   
   WHY THIS CLASSIFICATION:
   For the individual experiencing loss, transience is a 'Snare.' The visitor 
   extracts an irreplaceable part of the self and leaves, strangling the 
   host's sense of continuity and meaning. This can lead to profound grief and existential distress.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    theory_of_visitors,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SOCIAL MEDIA PLATFORM / DATING APP - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Facilitates connections and departures)
   WHEN: biographical (Building a business model on human interaction)
   WHERE: arbitrage (Monetizes the transience of relationships)
   
   WHY THIS CLASSIFICATION:
   For a social media platform or dating app, the theory of visitors is a 'Rope'.
   Their business model is built on the transience of relationships. They facilitate
   connections and departures, extracting value from each interaction without
   the burden of fostering permanence.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    theory_of_visitors,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE STOIC / THE NAVIGATOR - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has the agency to "welcome" and "surrender")
   WHEN: biographical (The lifespan of "souvenirs" kept forever)
   WHERE: mobile (Moving from one "visitor" to the next)
   
   WHY THIS CLASSIFICATION:
   For the self-aware host, the theory is a 'Rope'—a functional coordination 
   mechanism for internal peace. It provides a map for "surrender" that 
   prevents total liquidation by emphasizing the "souvenirs" over the 
   losses, allowing them to navigate the flux of life gracefully.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    theory_of_visitors,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(theory_of_visitors_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(theory_of_visitors, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(theory_of_visitors, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(theory_of_visitors, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(theory_of_visitors_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'Social Media Platform / Dating App' to
 *    represent the institutional agent. Their business model thrives on the
 *    transient nature of relationships, making it a powerful 'Rope' for them.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Mourner (Snare): Suffers profound loss from transience.
 *    - Social Media (Rope): Monetizes transient connections.
 *    - Stoic (Rope): Uses transience as a tool for personal growth.
 * 
 * 3. TANGLED ROPE: The theory itself is a 'Tangled Rope'. It explains an
 *    inescapable aspect of the human condition (mortality/transience) that
 *    can be a source of profound suffering ('Snare') but also a powerful
 *    tool for emotional resilience and growth ('Rope').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the true value of "souvenirs" against irrecoverable loss.
 */

omega_variable(
    souvenir_valuation_omega,
    "Is the value of the 'souvenir' truly enough to offset the 'part that sucks' (irrecoverable loss), or is the theory a narrative coping mechanism for a terminal 'Snare'?",
    resolution_mechanism("Longitudinal tracking of long-term well-being and psychological resilience in individuals adopting the theory versus those who reject it."),
    impact("If souvenirs offset loss: Theory is a 'Rope' for growth. If loss is terminal: Theory is a 'Snare' masquerading as a Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Eternal Commitment / "Forever" Narrative
 *    Viability: Traditional social ideal for marriage and loyalty.
 *    Suppression: Rejected by the "Theory of Visitors" as an illusion; "Everything
 *    is temporary" is the counter-narrative.
 *
 * CONCLUSION:
 * The "Theory of Visitors" serves as a 'Rope' for navigating the inevitable
 * transience of human relationships. Its core "wisdom" is the suppression of
 * the alternative "forever" narrative, which, if pursued in the face of
 * reality, becomes a profound 'Snare' of disappointment.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/theory_of_visitors].
 * 2. Multi-perspective: ?- multi_index_report(theory_of_visitors).
 * 3. Run tests: ?- run_tests(theory_of_visitors_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */