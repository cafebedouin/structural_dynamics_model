% ============================================================================
% CONSTRAINT STORY: social_narrative_casting
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Criticism as Other People’s Stories" by cafebedouin.org
% ============================================================================

:- module(constraint_social_narrative_casting, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: social_narrative_casting
 * human_readable: Social Narrative Casting (Criticism-as-Projection)
 * domain: social/psychological
 * temporal_scope: Contemporary / Ongoing
 * spatial_scope: Interpersonal / Cognitive
 * * SUMMARY:
 * This constraint defines criticism as an attempt by a critic (the "Director") to "hire" the 
 * subject into a specific role (extra, villain, victim) within the critic's internal narrative 
 * fiction. It frames these stories as "falsehoods" and "understandable illusions" 
 * that the subject can choose to navigate without internalizing.
 * * KEY AGENTS:
 * - The Critic (Director): Projects their internal story to give meaning to their life and 
 * reinforce their ego.
 * - The Subject (Actor): The person being criticized, who must choose whether to "believe in 
 * the part" or "play the role" with awareness.
 * - The Ego: The internal force that reduces experience into narrative fiction to create a 
 * certain "look" that doesn't reflect reality.
 * * NARRATIVE ARC:
 * Criticism begins as an extractive "role assignment" where the critic attempts to use 
 * the subject to make their internal fiction work. The subject can 
 * transform this constraint from a "Snare" of trapped identity into a "Rope" of 
 * useful lessons by maintaining a conscious choice about playing the part.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(social_narrative_casting, 0, 10).
narrative_ontology:constraint_claim(social_narrative_casting, [psychological_projection]).

% Base extractiveness score (0.3 = Moderate-Low)
% Rationale: While the critic attempts to extract the subject's identity for their "brand," 
% the subject's ultimate agency to refuse belief mitigates total extraction.
domain_priors:base_extractiveness(social_narrative_casting, 0.3).

% Suppression score (0.6 = High-Moderate)
% Rationale: Narrative fiction is "designed to create a certain look that doesn’t reflect 
% reality," effectively suppressing lived experience in favor of an understandable 
% illusion.
domain_priors:suppression_score(social_narrative_casting, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(social_narrative_casting, extractiveness, 0.3).
narrative_ontology:constraint_metric(social_narrative_casting, suppression_requirement, 0.6).

% Constraint classification claim
narrative_ontology:constraint_claim(social_narrative_casting, mountain).

% Enforcement requirements
% The constraint "emerges naturally" from the ego's need to project itself and create 
% meaning through stories.
domain_priors:emerges_naturally(social_narrative_casting).



% BENEFICIARIES & VICTIMS
% Beneficiary: The Critic's Ego (gains meaning and brand reinforcement).
narrative_ontology:constraint_beneficiary(social_narrative_casting, critics_ego).
% Victim: Lived Experience (is reduced to an illusion or plot).
narrative_ontology:constraint_victim(social_narrative_casting, lived_experience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CRITICIZED SUBJECT (Actor) - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Target of "you are X" role assignment)
   WHEN: immediate (A specific moment of confrontation/criticism)
   WHERE: trapped (If they believe the role is their life)
   SCOPE: local (The immediate social interaction)
   
   WHY THIS CLASSIFICATION:
   For a subject who internalizes criticism, it is a "Snare." The critic's narrative 
   constrains their identity to a "personification of some trait [the critic] doesn’t 
   like about themselves," leaving them trapped in someone else's movie.
   
   NARRATIVE EVIDENCE:
   "when someone makes a criticism of you... they are trying to hire you as an extra 
   in their movie... as the villain, the victim, the obstacle".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    social_narrative_casting,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(social_narrative_casting, S),

    S > 0.5.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CONSCIOUS AGENT (Aware Actor) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "pause the production")
   WHEN: biographical (Long-term navigation of social roles)
   WHERE: mobile (Can see roles for what they are)
   SCOPE: local (Managing specific circumstances)
   
   WHY THIS CLASSIFICATION:
   For the aware agent, roles are a "Rope." They may be "obligated to play them by 
   circumstances," but they use the role as a functional coordination tool while 
   remaining detached from the underlying "belief" in the part.
   
   NARRATIVE EVIDENCE:
   "you always have the choice about whether to play the part... Some parts have 
   useful lessons to teach us... play it knowing it’s a role".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    social_narrative_casting,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXISTENTIAL ANALYST (Ego-Breaker) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of "narrative fiction" as a system)
   WHEN: civilizational (Fundamental human ego-mechanism)
   WHERE: analytical (Observer stance)
   SCOPE: global (Applies to all human story-telling)
   
   WHY THIS CLASSIFICATION:
   To the analyst, the ego's tendency to create illusions is a "Mountain"—an unchangeable 
   natural law of human consciousness. "No one escape[s]" the ego's control except through 
   radical breaking of plots.
   
   NARRATIVE EVIDENCE:
   "The stories we tell ourselves are narrative fiction... a reduction of our 
   experience to an easily understandable illusion... It’s our ego taking control".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    social_narrative_casting,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE THERAPIST / COACH - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Guides others in navigating narratives)
   WHEN: biographical (Over the course of a client's therapy)
   WHERE: arbitrage (Can analyze and reframe narratives from a position of professional distance)
   SCOPE: regional (The scope of a client's life and social interactions)
   
   WHY THIS CLASSIFICATION:
   For a therapist or coach, understanding social narrative casting is a
   powerful diagnostic and therapeutic tool (Rope). They teach clients to
   recognize when they are being "cast" in a role, helping them detangle
   their identity from the projections of others. The concept is a mechanism
   for building resilience and social awareness.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    social_narrative_casting,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(social_narrative_casting_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that belief vs. detachment transforms a Snare into a Rope.
 */
test(multi_perspective_belief) :-
    % Victim who believes the role is life
    constraint_indexing:constraint_classification(social_narrative_casting, snare, context(agent_power(powerless), _, _, _)),
    % Agent who knows it is just a role
    constraint_indexing:constraint_classification(social_narrative_casting, rope, context(agent_power(individual_moderate), _, _, _)),
    % Analyst who sees the underlying biological ego-system
    constraint_indexing:constraint_classification(social_narrative_casting, mountain, context(agent_power(analytical), _, _, _)),
    % Therapist who uses the system as a tool
    constraint_indexing:constraint_classification(social_narrative_casting, rope, context(agent_power(institutional), _, _, _)).

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that those who don't choose the part suffer more identity extraction.
 */
test(power_extractiveness_casting) :-
    ContextPowerless = context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextInstitutional = context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(regional)),
    constraint_indexing:extractiveness_for_agent(social_narrative_casting, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(social_narrative_casting, ContextInstitutional, Score2),
    Score1 > Score2.

/**
 * TEST 3: Time-horizon immutability
 * Demonstrates that while a single role can be escaped (Rope), the human tendency 
 * to tell stories is civilizational (Mountain).
 */
test(time_immutability_casting) :-
    % Long horizon = fundamental ego law
    constraint_indexing:effective_immutability(time_horizon(civilizational), exit_options(analytical), mountain).

:- end_tests(social_narrative_casting_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. BASE EXTRACTIVENESS (0.3):
 *    Reasoning: Chose moderate-low because while someone "hiring" you for their villain 
 *    role is extractive, the source emphasizes the subject's "choice" and "lessons".
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Focused on the transition from "Unconscious Actor" (Snare) to "Conscious Actor" 
 *    (Rope) to "Observer" (Mountain) to reflect the source's pedagogical arc. Added 
 *    "Therapist" (Institutional) to show the constraint as a tool.
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    [Individual Powerless] → [Snare]: Believing the roles assigned by others makes the 
 *    criticism an inescapable trap of identity.
 *    [Analytical] → [Mountain]: The ego's storytelling is framed as an inherent, 
 *    systemic filter of human experience.
 *    [Institutional] -> [Rope]: A therapist uses the understanding of this system as a tool to help others.
 * 
 * 4. AMBIGUITIES:
 *    - The text mentions "lived experience" as the goal but doesn't define it outside 
 *    the negative space of "breaking free". Resolved via Omega.
 * 
 * 5. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 * 
 * 6. CONFIDENCE:
 *    High: Classifications of Snare/Rope based on internal vs external belief.
 *    Medium: Extractiveness score (dependent on the severity of the "villain" casting).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Objective Self-Correction
 * Viability: The idea that criticism reflects an external, observable truth rather 
 * than just a critic's projection.
 * Suppression: Actively suppressed by the source's claim that stories are "falsehoods" 
 * and criticism is most likely a projection of "traits [critics] don’t like".
 * * ALTERNATIVE 2: Social Narrative Harmony
 * Viability: Harmonizing internal and external stories to reduce friction.
 * Suppression: Rejected in favor of "breaking free" to get to reality.
 * * CONCLUSION:
 * The source's rejection of alternative 1 (objective criticism) shifts the social 
 * interaction from a potential "Rope" for self-improvement into a "Snare" (projection) 
 * that requires an external "Rope" (conscious awareness) to survive.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/social_narrative_casting].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(social_narrative_casting).
 * 
 * 3. Run tests:
 *    ?- run_tests(social_narrative_casting_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(social_narrative_casting).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(social_narrative_casting, [other_id]).
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
% Structural constraint in social domain — low theater, high substance
domain_priors:theater_ratio(social_narrative_casting, 0.06).
narrative_ontology:constraint_metric(social_narrative_casting, theater_ratio, 0.06).

% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (social_narrative_casting)
% ============================================================================

omega_variable(
    omega_criticism_objectivity,
    "Can criticism ever reflect an objective truth about the subject, or is it always a projection of the critic's internal narrative?",
    "Cross-cultural studies of criticism reception and longitudinal tracking of projected vs. observed behavioral patterns.",
    "If objective: Criticism is a Rope for self-improvement. If always projection: It remains a Snare until consciously reframed.",
    confidence_without_resolution(medium)
).
