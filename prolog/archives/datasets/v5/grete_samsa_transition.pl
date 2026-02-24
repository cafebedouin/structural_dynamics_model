% ============================================================================
% CONSTRAINT STORY: grete_samsa_transition
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Metamorphosis by Franz Kafka (David Wyllie Trans.)
% ============================================================================

:- module(constraint_grete_samsa_transition, []).

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
 * 
 * constraint_id: grete_samsa_transition
 * human_readable: Grete's Burden and Ascendance in The Metamorphosis
 * domain: social/economic/familial
 * temporal_scope: Late 19th/Early 20th Century
 * spatial_scope: The Samsa Family Flat
 * 
 * SUMMARY:
 * Grete Samsa undergoes a radical transition from a "somewhat useless" child 
 * to the family's primary decision-maker. This story arc is defined by her
 * changing relationship to Gregor's transformation.
 * 
 * KEY AGENTS:
 * - Early Grete (Individual Powerless): The initial, compassionate caregiver.
 * - Working Grete (Individual Moderate): The exhausted primary breadwinner.
 * - The Landlord/Society (Institutional): Represents the external social pressure on the family.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(grete_samsa_transition, 0, 10).
narrative_ontology:constraint_claim(grete_samsa_transition, tangled_rope).

% Base extractiveness score (0.6): Moderate.
% Rationale: Grete extracts status and power within the family by being the 
% sole mediator for Gregor, but she also suffers the extraction of her youth, 
% labor, and emotional energy.
domain_priors:base_extractiveness(grete_samsa_transition, 0.6).

% Suppression score (0.5): Moderate.
% Rationale: She initially refuses to let her mother see Gregor, 
% "protecting" her mother while consolidating her own control over the room.
domain_priors:suppression_score(grete_samsa_transition, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(grete_samsa_transition, extractiveness, 0.6).
narrative_ontology:constraint_metric(grete_samsa_transition, suppression_requirement, 0.5).

% Enforcement: Requires active enforcement by Grete (locking the door, cleaning).
domain_priors:requires_active_enforcement(grete_samsa_transition).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(grete_samsa_transition, grete_samsa). % Gains status and power.
constraint_victim(grete_samsa_transition, gregor_samsa). % Is ultimately dehumanized and disposed of.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: EARLY GRETE - Rope
   --------------------------------------------------------------------------
   WHO: powerless - A 17-year-old girl with no job or authority.
   WHEN: immediate - Focused on what Gregor will eat "today."
   WHERE: constrained - She is the only one who dares to enter the room.
   
   WHY THIS CLASSIFICATION:
   In the beginning, Gregor's state is a functional coordination challenge. She
   tests his tastes and adjusts the room. Her caregiving is a 'Rope' that gives
   her a vital new role and purpose in a family where she was previously "useless".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    grete_samsa_transition,
    rope,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: WORKING GRETE (LATE STORY) - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate - Now a salesgirl with an outside life.
   WHEN: biographical - Thinking of her future, marriage, and moving.
   WHERE: mobile - She has a job and a life outside the flat.
   
   WHY THIS CLASSIFICATION:
   The burden of cleaning Gregor's room after a full day's work makes the 
   constraint a 'Snare'. Gregor no longer provides; he only consumes her time,
   energy, and reputation, threatening her "blossoming" future.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    grete_samsa_transition,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LANDLORD / SOCIETY - Snare
   --------------------------------------------------------------------------
   WHO: institutional (Represents the social and economic order)
   WHEN: immediate (The threat of eviction or social shame)
   WHERE: constrained (By property rights and social norms)
   
   WHY THIS CLASSIFICATION:
   For the landlord and by extension, society, Gregor's presence is an intolerable
   'Snare'. He is a source of filth and disorder that threatens property value and
   the acceptable social order, justifying the family's eventual eviction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    grete_samsa_transition,
    snare,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(grete_samsa_transition_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(grete_samsa_transition, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(grete_samsa_transition, Type2, context(agent_power(individual_moderate), _, _, _)),
    % The institutional view is also a Snare, but for different reasons.
    % We are only required to have 2+ distinct types.
    Type1 \= Type2.

:- end_tests(grete_samsa_transition_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Landlord/Society' to represent the
 *    external, institutional pressure on the family. From this view, Gregor is
 *    a 'Snare' because he violates property and social norms.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Early Grete (Rope): Gregor's dependency gives her a purpose and power.
 *    - Late Grete (Snare): His dependency becomes a burden that strangles her future.
 *    - Landlord (Snare): Gregor is a threat to social and economic order.
 * 
 * 3. TANGLED ROPE: The overall constraint story is a perfect example of a 'Tangled
 *    Rope' that changes over time. What starts as a coordination mechanism (Rope)
 *    becomes an extractive burden (Snare) as the agent's own context evolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is Grete's true motivation in the story's climax.
 */

omega_variable(
    grete_intent,
    "Did Grete truly believe Gregor could no longer understand them, or was it a strategic lie to ease the family's conscience and justify his removal?",
    resolution_mechanism("Requires access to Grete's internal monologue, which Kafka denies the reader."),
    impact("If Lie: Grete is a ruthless agent cutting a Snare. If Belief: She is a tragic figure trapped by a Rope that became too heavy."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Professional Care
 *    Viability: The family could have used Gregor's savings to hire specialized help.
 *    Suppression: Suppressed by the shame of their "misfortune" and the desire to keep the situation secret.
 * 
 * CONCLUSION:
 * The family's refusal to consider outside alternatives traps Grete in her role
 * as sole caregiver, ensuring that her relationship with Gregor will curdle from
 * a functional 'Rope' into an unbearable 'Snare'.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/grete_samsa_transition].
 * 2. Multi-perspective: ?- multi_index_report(grete_samsa_transition).
 * 3. Run tests: ?- run_tests(grete_samsa_transition_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */