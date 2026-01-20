% ============================================================================
% CONSTRAINT STORY: hanlons_razor
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: hanlons_razor
 * human_readable: Hanlon's Razor ("Never attribute to malice...")
 * domain: social/cognitive/organizational
 * temporal_scope: Permanent (Modern Era)
 * spatial_scope: Global (Interpersonal and Bureaucratic Systems)
 * * SUMMARY:
 * Hanlon's Razor is an adagial heuristic: "Never attribute to malice that which 
 * is adequately explained by stupidity." It serves as a philosophical 
 * constraint on how we model the intentions of others in complex systems, 
 * suggesting that incompetence is a more frequent cause of negative outcomes 
 * than coordinated malevolence.
 * * KEY AGENTS:
 * - The Optimistic Skeptic: Uses the razor to maintain social trust and avoid 
 * paranoia.
 * - The Incompetent Actor: The source of the "noise" or "error" that looks 
 * like an attack.
 * - The Machiavellian: An agent who hides malice *behind* the appearance 
 * of stupidity.
 * * NARRATIVE ARC:
 * The razor acts as a "buffer" in social networks. It starts as a Rope (a tool 
 * for de-escalation). However, in high-stakes environments, it can become a 
 * Mountain (an unchangeable cultural bias toward leniency) or a Noose 
 * (when used by institutions to evade accountability for structural harm).
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(hanlon_interval, 0, 10).
narrative_ontology:constraint_claim(hanlons_razor, noose).

% Base extractiveness: 0.2 (Low)
% Rationale: It primarily extracts "accountability." By reframing malice 
% as stupidity, it reduces the social/legal penalty for the actor, 
% transferring the emotional or material cost to the victim.
domain_priors:base_extractiveness(hanlons_razor, 0.2).

% Suppression score: 0.4 (Moderate)
% Rationale: The razor actively suppresses "Conspiracy Theory" or "Malice 
% Hypotheses." While often healthy, it can make genuine systemic exploitation 
% invisible to the casual observer.
domain_priors:suppression_score(hanlons_razor, 0.4).

% Enforcement: Emerges naturally as a cognitive shortcut to reduce social friction.
domain_priors:emerges_naturally(hanlons_razor).

% Metrics for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(hanlons_razor, extractiveness, 0.2).
narrative_ontology:constraint_metric(hanlons_razor, suppression_requirement, 0.4).

% Beneficiaries: The Incompetent (who avoid punishment) and the Cynical (who 
% use "stupidity" as a shield for strategy).
constraint_beneficiary(hanlons_razor, [unintended_transgressors, tactical_actors]).

% Victims: The Accuser or Victim of harm who is told their grievance is 
% "just a misunderstanding."
constraint_victim(hanlons_razor, [accountability_seekers, system_critics]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SOCIAL COORDINATOR - ROPE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A team leader or community member).
   WHEN: biographical (Focus on maintaining long-term relationships).
   WHERE: mobile (Can choose when to apply the heuristic).
   SCOPE: local/regional.
   
   WHY THIS CLASSIFICATION:
   For the coordinator, Hanlon's Razor is a Rope. It is a vital tool for 
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SCIENTIFIC OBSERVER - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (A systems theorist or psychologist).
   WHEN: civilizational (Observing universal human behavior).
   WHERE: trapped (Human cognitive limits are fixed).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   To the observer, Hanlon's Razor describes a Mountain. In any large system, 
   entropy and human error are statistical certainties. Mediocrity and 
   incompetence are "natural laws" of complex organizations; coordinated 
   malice is rare because it is too difficult to sustain.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hanlons_razor,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MARGINALIZED VICTIM - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A person subject to "stupid" bureaucratic harm).
   WHEN: immediate (Experiencing the harm now).
   WHERE: constrained (Cannot exit the institution causing the harm).
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For a victim of structural harm, the razor is a Noose. When an institution 
   claims a harmful policy was just a "mistake" or "unintended," it uses the 
   razor to strangle the victim's ability to demand reform. The "stupidity" 
   defense becomes a mechanism to maintain a harmful status quo.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hanlons_razor,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(hanlons_razor_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(hanlons_razor, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(hanlons_razor, Type2, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(hanlons_razor, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(accountability_extraction) :-
    % Extraction should be non-zero as it removes the "cost" of intent.
    domain_priors:base_extractiveness(hanlons_razor, E),
    E > 0.0.

test(systemic_bias_check) :-
    % Long-term civilizational view treats entropy/stupidity as a constant (Mountain).
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(hanlons_razor_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVAL DYNAMICS: The "Noose" classification is the most critical 
 * insight here—how a "kind" heuristic can be weaponized by institutions 
 * to dodge accountability for systemic biases.
 * 2. EXTRACTIVENESS: Kept low (0.2) because the primary "resource" being 
 * taken is not money, but the social right to hold someone responsible 
 * for intent.
 * 3. SUPPRESSION: Set at 0.4 to represent how the razor delegitimizes 
 * "Conspiracy" thinking, which is often correct in high-stakes corruption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    intentionality_detection_limit,
    "Is there a mathematical limit to distinguishing high-entropy incompetence from low-signal malice?",
    resolution_mechanism("Information-theoretic analysis of adversarial intent in noise-heavy systems"),
    impact("If Yes: Hanlon's Razor is a Mountain (we can never know). If No: It is a Rope (we can choose to detect)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Occam's Razor (The simplest explanation is best)
 * Viability: High. Hanlon's is a specialized subset of Occam's.
 * * ALTERNATIVE 2: Grey's Law (Sufficiently advanced incompetence is 
 * indistinguishable from malice)
 * Viability: High. It reverses the razor, suggesting we should focus 
 * on impact rather than intent.
 * * CONCLUSION:
 * The existence of Grey's Law suggests that Hanlon's Razor is a choice 
 * (Rope), and when we are forced to use it despite visible harm, it 
 * becomes a Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_hanlons_razor].
 * 2. Multi-perspective report: ?- multi_index_report(hanlons_razor).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
