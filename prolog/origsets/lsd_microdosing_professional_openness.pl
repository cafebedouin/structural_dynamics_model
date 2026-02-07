% ============================================================================
% CONSTRAINT STORY: lsd_microdosing_professional_openness
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "Ergot on Rye" (citing Scott Alexander and psilocybin studies)
% ============================================================================

:- module(constraint_lsd_microdosing_professional_openness, []).

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
 * constraint_id: lsd_microdosing_professional_openness
 * human_readable: The Permanent Openness Shift from Microdosing
 * domain: psychological/professional/biochemical
 * temporal_scope: Contemporary / 2026 Era
 * spatial_scope: Global (Corporate/Creative Labor Markets)
 * 
 * SUMMARY:
 * This constraint analyzes the professional application of LSD microdosing to increase 
 * the personality dimension of "Openness to Experience". In professional 
 * settings, this serves as a tool to become "more interesting" and gain 
 * "dividends" in creative roles. The core tension is whether this is a temporary
 * performance enhancer or a permanent, non-consensual personality restructuring.
 * 
 * KEY AGENTS:
 * - The Professional User (Individual Moderate): Utilizes microdosing for a competitive edge.
 * - The Trapped Subject (Individual Powerless): Experiences a permanent, unwanted personality change.
 * - The Regulator / FDA (Institutional): Faces the challenge of an uncontrollable, permanent drug effect.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(lsd_microdosing_professional_openness, 0, 10).
narrative_ontology:constraint_claim(lsd_microdosing_professional_openness, tangled_rope).

% Base extractiveness score (0.4 = Moderate)
% Rationale: The practice extracts the user's "baseline" personality stability 
% to fuel professional "dividends". If permanent, it extracts future "consent"
% to return to the original self.
domain_priors:base_extractiveness(lsd_microdosing_professional_openness, 0.4).

% Suppression score (0.5 = Moderate)
% Rationale: Professional "sanity" and legal frameworks suppress the "madness"
% of psychedelics, even as microdosing trends attempt to normalize it.
domain_priors:suppression_score(lsd_microdosing_professional_openness, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(lsd_microdosing_professional_openness, extractiveness, 0.4).
narrative_ontology:constraint_metric(lsd_microdosing_professional_openness, suppression_requirement, 0.5).

% Enforcement: Requires active individual maintenance (dosing).
domain_priors:requires_active_enforcement(lsd_microdosing_professional_openness).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(lsd_microdosing_professional_openness, the_user). % Gains a competitive edge
constraint_victim(lsd_microdosing_professional_openness, the_user). % Risks permanent personality change

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COMPETITIVE WORKER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Seeking dividends in society)
   WHEN: biographical (Advancing a career over decades)
   WHERE: mobile (Choosing to microdose for openness)
   
   WHY THIS CLASSIFICATION:
   For the professional, microdosing is a 'Rope'—a functional coordination 
   mechanism to expand their "Openness to Experience" and become a "more 
   interesting" player in a competitive market.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lsd_microdosing_professional_openness,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRAPPED SUBJECT (OMEGA = PERMANENT) - Snare
   --------------------------------------------------------------------------
   WHO: powerless (After the "permanent" shift has occurred)
   WHEN: civilizational (A stable shift that lasts a lifetime)
   WHERE: trapped (Cannot return to previous personality "standard")
   
   WHY THIS CLASSIFICATION:
   If the personality shift is permanent, the initial choice becomes a 'Snare'. 
   The agent is "terrified" to find their personality changed "without 
   their consent," with no "neutral ground" to return to the original self.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lsd_microdosing_professional_openness,
    snare,
    context(
        agent_power(powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE REGULATOR (FDA) - Snare
   --------------------------------------------------------------------------
   WHO: institutional (Tasked with ensuring drug safety and efficacy)
   WHEN: historical (Across drug approval cycles)
   WHERE: constrained (Cannot approve a drug with uncontrollable, permanent effects)
   
   WHY THIS CLASSIFICATION:
   For a regulator, a substance that can cause large, permanent personality changes
   is a 'Snare'. The inability to predict or control the outcome, and the lack of a
   return to baseline, makes it impossible to approve under a standard medical
   framework. It strangles the approval process.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lsd_microdosing_professional_openness,
    snare,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(lsd_microdosing_professional_openness_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, Type2, context(agent_power(powerless), _, _, _)),
    % Institutional view is also a Snare, but for different reasons.
    % We only need to check for 2+ distinct types.
    Type1 \= Type2.

:- end_tests(lsd_microdosing_professional_openness_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Replaced the ambiguous 'Psychiatrist' with the
 *    'Regulator/FDA'. This provides a clear institutional agent whose perspective
 *    is a 'Snare' for entirely different reasons than the individual's: one of
 *    control and predictability vs. personal identity.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - User (Rope): A tool for professional advantage.
 *    - Trapped Subject (Snare): An irreversible, non-consensual identity shift.
 *    - Regulator (Snare): An uncontrollable substance that cannot be approved.
 * 
 * 3. TANGLED ROPE: The overall constraint is a 'Tangled Rope' because the user
 *    is pulling on a 'Rope' for self-improvement that may retroactively become
 *    a 'Snare' if the effects are permanent and undesirable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether the personality changes are permanent.
 */

omega_variable(
    hallucinogenic_personality_permanence,
    "Is the increase in Openness from psychedelics a temporary state (Rope) or a permanent restructuring of the adult ego (Snare/Mountain)?",
    resolution_mechanism("Longitudinal personality tracking of users 5+ years after cessation of use."),
    impact("If permanent: The Rope becomes a Snare. If temporary: It remains a state-management Rope."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Traditional professional development (The "Sober" Path)
 *    Viability: The default state of adult personality constancy.
 *    Suppression: Suppressed by a hyper-competitive society where "trying to be a little
 *    more interesting" is seen as necessary to "pay dividends".
 * 
 * CONCLUSION:
 * The Omega variable's resolution determines the nature of the constraint. If the
 * change is temporary, microdosing is just another 'Rope' in the toolkit of
 * self-improvement. If it's permanent, it's a 'Snare' masquerading as a Rope.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/lsd_microdosing_professional_openness].
 * 2. Multi-perspective: ?- multi_index_report(lsd_microdosing_professional_openness).
 * 3. Run tests: ?- run_tests(lsd_microdosing_professional_openness_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */