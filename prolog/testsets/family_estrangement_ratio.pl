% ============================================================================
% CONSTRAINT STORY: family_estrangement_ratio
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Family Estrangement" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(family_estrangement_ratio, []).

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
 * * constraint_id: family_estrangement_ratio
 * human_readable: The "Family is Forever" Dogma and the Magic Ratio
 * domain: social/psychological
 * temporal_scope: Contemporary / Ongoing
 * spatial_scope: The Familial Fabric / Social Circles
 * * SUMMARY:
 * Family estrangement is the breakdown of supportive relationships, often governed by a "bad magic ratio" where negative interactions deplete reserves of goodwill. Unlike friendships, family is constrained by the mental model that it is "forever," transforming a simple ending into a stigmatized and unacknowledged "silent epidemic".
 * * KEY AGENTS:
 * - The Estranged Individual: An agent who chooses to cut a thread in the family fabric to stop harm.
 * - The "Jerk"/Harmful Relative: A transgressive agent prioritizing their own beliefs or behavior over the relationship.
 * - The Family Network (e.g., Mother-in-Law): Institutional observers who enforce reconciliation to protect the "forever" precedent.
 * - The Social Circle: The wider environment forced to "choose sides" for weddings, funerals, and holidays.
 * * NARRATIVE ARC:
 * Relationships operate on a "magic ratio" of 5:1 positive to negative interactions. While friendships can end, the familial "forever" model acts as a "fell wind" that pushes individuals into a choice between persistent harm or social liquidation (estrangement). This choice ripples through the network, making attendance at major life events impossible for the "little people" who cannot slide under the conflict.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(family_estrangement_ratio, 0, 10).
narrative_ontology:constraint_claim([family_estrangement_ratio], [social_governance]).

% Base extractiveness score (0.8 = High)
% Rationale: The constraint extracts social standing, attendance at vital rituals (weddings/funerals), 
% and family support, leaving individuals "judged, stigmatized and misunderstood".
domain_priors:base_extractiveness(family_estrangement_ratio, 0.8).

% Suppression score (0.7 = High)
% Rationale: The "family is forever" model suppresses the alternative of a clean relationship 
% closure, forcing the "silent epidemic" of unacknowledged strain.
domain_priors:suppression_score(family_estrangement_ratio, 0.7).

% Enforcement: Does it require active maintenance or emerge naturally?
% Rationale: Emerges naturally from the "interwoven fabric" but is actively enforced 
% by observers who "push for reconciliation" or judge the silent.
domain_priors:requires_active_enforcement(family_estrangement_ratio).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(family_estrangement_ratio, extractiveness, 0.8).
narrative_ontology:constraint_metric(family_estrangement_ratio, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(family_estrangement_ratio, family_network_stability). % Precedent of "forever" protects the group
constraint_victim(family_estrangement_ratio, individual_safety). % Individual is extracted of social access for their safety

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ESTRANGED INDIVIDUAL - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Targeted by harm, subject to the "forever" dogma)
   WHEN: immediate (Short-term social consequences of weddings/holidays)
   WHERE: trapped (No conceptual exit that doesn't involve stigma/loss)
   SCOPE: local (The immediate family thread)
   
   WHY THIS CLASSIFICATION:
   For the individual, the "forever" model is a "Snare." If they stay, the "bad ratio" strangles 
   them; if they leave, the "stigma" and loss of life events tightens around their social existence.
   
   NARRATIVE EVIDENCE:
   "people suffering through it often describe feeling judged, stigmatized and misunderstood... 
   makes it impossible to go to weddings, funerals or holidays".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    family_estrangement_ratio,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(family_estrangement_ratio, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE FAMILY NETWORK OBSERVER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-shaping through social pressure and tradition)
   WHEN: generational (Maintaining the continuity of family "forever")
   WHERE: mobile (Can "push for reconciliation" or "choose sides")
   SCOPE: regional (The extended family circle)
   
   WHY THIS CLASSIFICATION:
   For the network, the "forever" dogma is a "Rope"—a tool for coordination. It binds the 
   interwoven threads together, ensuring that even if one relationship fails, the fabric 
   remains whole and "support" can theoretically be healed.
   
   NARRATIVE EVIDENCE:
   "familial relationship implies a fabric of interwoven relationships... larger family 
   network can help heal some of these divisions".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    family_estrangement_ratio,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXISTENTIAL ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the "magic ratio" and behavior patterns)
   WHEN: historical (Viewing the "silent epidemic" as a systemic phenomenon)
   WHERE: analytical (Observer stance)
   SCOPE: global (Universal social dynamics of scarcity/goodwill)
   
   WHY THIS CLASSIFICATION:
   To the analyst, the "magic ratio" is a "Mountain"—a fixed law of social physics. 
   When the ratio falls below 5:1, the "exhaustion of reserves" is an immutable fact 
   of nature that precedes the social event of estrangement.
   
   NARRATIVE EVIDENCE:
   "The magic relationship ratio is useful for understanding estrangement... 
   these reserves can be exhausted".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    family_estrangement_ratio,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(family_estrangement_ratio_tests).

test(multi_perspective_estrangement) :-
    % Individual sees Snare
    constraint_indexing:constraint_classification(family_estrangement_ratio, snare, context(individual_powerless, immediate, trapped, local)),
    % Network sees Rope
    constraint_indexing:constraint_classification(family_estrangement_ratio, rope, context(institutional, generational, mobile, regional)),
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(family_estrangement_ratio, mountain, context(analytical, historical, analytical, global)).

test(power_extractiveness_estrangement) :-
    % The powerless individual pays the higher price (stigma/liquidation)
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, generational, mobile, regional),
    constraint_indexing:extractiveness_for_agent(family_estrangement_ratio, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(family_estrangement_ratio, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_estrangement) :-
    % Over historical horizons, the silent epidemic appears as an unchangeable Mountain.
    constraint_indexing:effective_immutability(historical, analytical, mountain).

:- end_tests(family_estrangement_ratio_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.8):
 * Reasoning: High. The source emphasizes that estrangement results in the "liquidation" 
 * of social participation (weddings, funerals) and subjects individuals to a 
 * "silent epidemic" of stigma. This fulfills the Mandatrophy Gate.
 * 2. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The constraint is a "Rope" for the Network Observer 
 * (who uses it to maintain familial stability) but a "Snare" for the individual 
 * powerless (who is forced into isolation to maintain safety).
 * 3. PERSPECTIVE SELECTION:
 * Chose the Individual (Snare), the Network Observer (Rope), and the Analyst (Mountain) 
 * to reflect the source's tension between personal cost and systemic rules.
 * 4. AMBIGUITIES:
 * - The text mentions "something else prioritised over the relationship" (e.g. mental 
 * illness, addiction). This creates an Omega about the intent vs. the result of the ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    family_estrangement_ratio_extraction_intent,
    "Is the stigma of estrangement a functional protection of the family unit (Mountain) or an extractive tool to force compliance (Snare)?",
    resolution_mechanism("Comparison of health outcomes for individuals in reconcile-at-all-costs families vs. closure-accepting families"),
    impact("If protection: Institutional Mountain. If extraction: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    reconciliation_efficacy_threshold,
    "Can a 'larger family network' actually heal divisions (Rope), or is the prevalence of the 'silent epidemic' proof that the network is a failed Scaffold?",
    resolution_mechanism("Audit of long-term reconciliation rates mediated by extended family networks"),
    impact("If efficacy high: Network is a Rope. If low: Network is a deceptive Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Clean Relationship Closure
 * Viability: Friendships and marriages end as part of life.
 * Suppression: Actively suppressed by the "family is forever" model.
 * Evidence: "But, we accept this as part of friendships... family relationships can only become estranged?".
 * * ALTERNATIVE 2: Balanced Accountability
 * Viability: Both parties address the "magic ratio" before reserves exhaust.
 * Suppression: Often made impossible by "jerks" who care more about their "beliefs than they do about you".
 * * CONCLUSION:
 * The existence of Alternative 1 shifts the "forever" dogma from a potential Rope into a definitive 
 * Snare for those whose safety depends on ending the contact.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into main system: ?- [family_estrangement_ratio].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
