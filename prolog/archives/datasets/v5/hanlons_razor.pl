% ============================================================================
% CONSTRAINT STORY: hanlons_razor
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Robert J. Hanlon (1980) / Systems Thinking
% ============================================================================

:- module(constraint_hanlons_razor, []).

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
 * constraint_id: hanlons_razor
 * human_readable: Hanlon's Razor ("Never attribute to malice...")
 * domain: social/cognitive/organizational
 * temporal_scope: Permanent (Modern Era)
 * spatial_scope: Global (Interpersonal and Bureaucratic Systems)
 * 
 * SUMMARY:
 * Hanlon's Razor is an adagial heuristic: "Never attribute to malice that which 
 * is adequately explained by stupidity." It serves as a philosophical 
 * constraint on how we model the intentions of others in complex systems, 
 * suggesting that incompetence is a more frequent cause of negative outcomes 
 * than coordinated malevolence.
 * 
 * KEY AGENTS:
 * - The Optimistic Skeptic (Individual Moderate): Uses the razor to maintain social trust.
 * - The Bureaucracy / Government Agency (Institutional): Manages public relations and accountability.
 * - The Marginalized Victim (Individual Powerless): Subject to "stupid" bureaucratic harm.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(hanlons_razor, 0, 10).
narrative_ontology:constraint_claim(hanlons_razor, tangled_rope).

% Base extractiveness: 0.2 (Low)
% It primarily extracts "accountability." By reframing malice 
% as stupidity, it reduces the social/legal penalty for the actor, 
% transferring the emotional or material cost to the victim.
domain_priors:base_extractiveness(hanlons_razor, 0.2).

% Suppression score: 0.4 (Moderate)
% The razor actively suppresses "Conspiracy Theory" or "Malice 
% Hypotheses." While often healthy, it can make genuine systemic exploitation 
% invisible to the casual observer.
domain_priors:suppression_score(hanlons_razor, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(hanlons_razor, extractiveness, 0.2).
narrative_ontology:constraint_metric(hanlons_razor, suppression_requirement, 0.4).

% Enforcement: Emerges naturally as a cognitive shortcut to reduce social friction.
domain_priors:emerges_naturally(hanlons_razor).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(hanlons_razor, [unintended_transgressors, social_cohesion]).
constraint_victim(hanlons_razor, [accountability_seekers, system_critics]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SOCIAL COORDINATOR - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (A team leader or community member).
   WHEN: biographical (Focus on maintaining long-term relationships).
   WHERE: mobile (Can choose when to apply the heuristic).
   
   WHY THIS CLASSIFICATION:
   For the coordinator, Hanlon's Razor is a 'Rope'. It is a vital tool for 
   preventing "death spirals" of retaliation. By assuming a mistake was 
   stupidity, they preserve the ability to work with others without the 
   friction of perceived enmity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hanlons_razor,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BUREAUCRACY / GOVERNMENT AGENCY - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manages public relations and accountability).
   WHEN: historical (Long-term organizational stability).
   WHERE: arbitrage (Can strategically attribute failures to incompetence).
   
   WHY THIS CLASSIFICATION:
   For a bureaucracy, Hanlon's Razor can be a 'Tangled Rope'. It's a 'Rope'
   because it's a convenient heuristic to manage public relations and internal
   accountability by attributing failures to incompetence rather than malice.
   It's 'Tangled' because this can hide genuine systemic issues and erode trust.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hanlons_razor,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MARGINALIZED VICTIM - Snare
   --------------------------------------------------------------------------
   WHO: powerless (A person subject to "stupid" bureaucratic harm).
   WHEN: immediate (Experiencing the harm now).
   WHERE: constrained (Cannot exit the institution causing the harm).
   
   WHY THIS CLASSIFICATION:
   For a victim of structural harm, the razor is a 'Snare'. When an institution 
   claims a harmful policy was just a "mistake" or "unintended," it uses the 
   razor to strangle the victim's ability to demand reform. The "stupidity" 
   defense becomes a mechanism to maintain a harmful status quo.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hanlons_razor,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(hanlons_razor_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(hanlons_razor, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(hanlons_razor, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hanlons_razor, Type3, context(agent_power(powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(hanlons_razor_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Bureaucracy / Government Agency' as
 *    the institutional agent. This highlights how Hanlon's Razor can be
 *    strategically deployed as a 'Tangled Rope' to manage accountability.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Social Coordinator (Rope): A tool for de-escalation.
 *    - Bureaucracy (Tangled Rope): A tool for managing public perception.
 *    - Marginalized Victim (Snare): Prevents accountability for structural harm.
 * 
 * 3. CORE INSIGHT: Hanlon's Razor is a powerful heuristic, but its application
 *    is highly perspectival. What is a benevolent 'Rope' for maintaining social
 *    cohesion can become a 'Snare' when used by institutions to evade
 *    accountability for systemic issues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the fundamental limit of distinguishing incompetence from malice.
 */

omega_variable(
    intentionality_detection_limit,
    "Is there an inherent limit to distinguishing high-entropy incompetence from low-signal malice in complex systems?",
    resolution_mechanism("Information-theoretic analysis of adversarial intent in noise-heavy systems; neurological studies on intent detection."),
    impact("If Yes: Hanlon's Razor describes a 'Mountain' we can never truly surmount. If No: It is a 'Rope' we can choose to refine."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Occam's Razor
 *    Viability: A more general principle: the simplest explanation is usually the best. Hanlon's is a specialized subset.
 *    Suppression: Not actively suppressed, but Hanlon's provides a more specific lens for human interaction.
 *
 * ALTERNATIVE 2: Grey's Law ("Sufficiently advanced incompetence is indistinguishable from malice")
 *    Viability: Reverses the razor, suggesting impact rather than intent is the key metric.
 *    Suppression: Often suppressed in organizational contexts where intent is prioritized over outcome.
 *
 * CONCLUSION:
 * The existence of Grey's Law highlights that choosing to apply Hanlon's Razor
 * is a 'Rope'—a deliberate choice to focus on intent over impact. When this
 * choice allows systemic harm to persist, it becomes a 'Snare' for victims.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/hanlons_razor].
 * 2. Multi-perspective: ?- multi_index_report(hanlons_razor).
 * 3. Run tests: ?- run_tests(hanlons_razor_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */