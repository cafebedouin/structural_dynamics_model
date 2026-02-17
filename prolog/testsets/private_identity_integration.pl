% ============================================================================
% CONSTRAINT STORY: private_identity_integration
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "No. 1 Rule: Keep Your Shit to Yourself" by cafebedouin.org
% ============================================================================

:- module(constraint_private_identity_integration, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: private_identity_integration
 * human_readable: The Closed-Door Identity Protocol
 * domain: social/psychological
 * temporal_scope: Contemporary / Identity Formation
 * spatial_scope: Local (Interpersonal) to Global (Digital Public)
 * * SUMMARY:
 * This constraint mandates that new insights regarding needs, wants, and 
 * identity should be kept private for several years while they are 
 * integrated. It posits that public display of raw emotional 
 * processing or "issues" is a breach of social hygiene that serves 
 * neither the subject nor the audience.
 * * KEY AGENTS:
 * - The Novice (Sharer): One discovering "solo polyamory" or other new 
 * identity labels.
 * - The Experienced Mentor: The observer who advocates for the "closed door" 
 * as a survival and social hygiene strategy.
 * - The Public (Twitter): The digital audience where "sophomoric" 
 * displays generate "discussion".
 * * NARRATIVE ARC:
 * An individual discovers a new self-description and immediately "throws 
 * open the door" through a public email or article. The mentor 
 * identifies this as "inept" and "sophomoric," suggesting that the real 
 * work of identity is a "closed-door" process of integration that takes 
 * years to complete.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(private_identity_integration, 0, 10).

% Updated to valid v3.1 ontology pillar: [rope]
narrative_ontology:constraint_claim(private_identity_integration, rope).
narrative_ontology:human_readable(private_identity_integration, "The Closed-Door Identity Protocol").

% Base extractiveness: Moderate (0.4)
% Rationale: Premature public sharing extracts attention and emotional 
% energy from the public for "sophomoric" processing that "isn't doing 
% anyone any favors".
domain_priors:base_extractiveness(private_identity_integration, 0.4).

% Suppression: Moderate (0.5)
% Rationale: The rule suppresses the modern impulse for radical 
% transparency and "ventilation" in favor of "courtesy" and private 
% integration.
domain_priors:suppression_score(private_identity_integration, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(private_identity_integration, extractiveness, 0.4).
narrative_ontology:constraint_metric(private_identity_integration, suppression_requirement, 0.5).

% Enforcement: Requires active individual enforcement ("Keep that shit to 
% yourself").
domain_priors:requires_active_enforcement(private_identity_integration).

% BENEFICIARIES & VICTIMS
% Beneficiary: The Social Fabric (protected from unnecessary drama).
narrative_ontology:constraint_beneficiary(private_identity_integration, social_hygiene).
% Victim: The Raw Impulse (denied immediate public validation/expression).
narrative_ontology:constraint_victim(private_identity_integration, radical_transparency).

% Metrics required for Section 1 of the Executive Summary
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NOVICE (Haili) - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Consumed by "fantasies that were force-fed")
   WHEN: immediate (The moment of discovery of "solo polyamory")
   WHERE: trapped (Feeling "confined" by relationship limits)
   SCOPE: local (Personal relationship/girlhood)
   
   WHY THIS CLASSIFICATION:
   For the novice, traditional relationship limits and the lack of identity 
   labels are a "Snare" that makes them feel "confined" and "narrow". 
   They attempt to break this Snare through public declaration, which the 
   mentor views as a further entanglement in "sophomoric" error.
   
   NARRATIVE EVIDENCE:
   "My entire girlhood had been consumed by fantasies that were force-fed... 
   I had felt confined".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    private_identity_integration,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE EXPERIENCED MENTOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Providing the "obvious" rules of life experience)
   WHEN: biographical (Integrating insights over "a few years")
   WHERE: mobile (Can "close the door" and choose what to share)
   SCOPE: local (Interpersonal hygiene)
   
   WHY THIS CLASSIFICATION:
   For the mentor, Rule No. 1 is a "Rope"—a functional coordination 
   mechanism for the self. It protects the process of identity 
   formation and ensures that the eventual re-emergence is refined rather 
   than messy.
   
   NARRATIVE EVIDENCE:
   "Integrating insights is hard work, and it takes time... Keep that shit 
   to yourself. Work it out. Flush when you’re done".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    private_identity_integration,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the nature of relationships)
   WHEN: historical (The "normal course" of breakups and life experience)
   WHERE: analytical (Observer of the "limits" that define identity)
   SCOPE: global (The "human race" and its social constraints)
   
   WHY THIS CLASSIFICATION:
   To the observer, the fact that "relationships imply limits" is a "Mountain"—an 
   unchangeable natural law of human sociality. One does not escape 
   confinement; one merely chooses which constraints will define their 
   freedom.
   
   NARRATIVE EVIDENCE:
   "At some point, you choose or time chooses for you... relationships 
   imply limits".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    private_identity_integration,
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

:- begin_tests(private_identity_integration_tests).

test(multi_perspective_hygiene) :-
    % Novice sees Snare (Confinement)
    constraint_indexing:constraint_classification(private_identity_integration, Type1, context(powerless, immediate, trapped, local)),
    % Mentor sees Rope (Survival Protocol)
    constraint_indexing:constraint_classification(private_identity_integration, Type2, context(institutional, biographical, mobile, local)),
    % Observer sees Mountain (Relationship Law)
    constraint_indexing:constraint_classification(private_identity_integration, Type3, context(analytical, historical, analytical, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_hygiene) :-
    % The novice (powerless) experiences the extraction of their raw identity 
    % by social "fantasies," while the mentor (institutional wisdom) 
    % experiences the "dividends" of the rule.
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, mobile, local),
    constraint_indexing:extractiveness_for_agent(private_identity_integration, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(private_identity_integration, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability) :-
    % In the short term, identity is a "Snare" of force-fed fantasies.
    constraint_indexing:effective_immutability(immediate, trapped, mountain),
    % Over biographical time, "Keep it to yourself" is a Rope (changeable).
    constraint_indexing:effective_immutability(biographical, mobile, rope).

:- end_tests(private_identity_integration_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: Chose moderate because premature sharing "isn't doing 
 * anyone any favors," effectively wasting the social bandwidth of the 
 * public for unintegrated personal issues.
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Novice (Snare), the Mentor (Rope), and the Observer (Mountain) 
 * to reflect the source's pedagogical arc: from feeling trapped by 
 * limits to using them as a tool for refined identity.
 * * 3. CLASSIFICATION RATIONALE:
 * Novice -> Snare: They feel "confined" by traditional binaries and 
 * "force-fed" fantasies.
 * Mentor -> Rope: Rule No. 1 is presented as a "good idea" and a survival 
 * mechanism to "work it out" privately.
 * Observer -> Mountain: The blog author posits that "relationships 
 * imply limits" as a fundamental, inescapable truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_integration_threshold,
    "How many 'years' are required for raw insight to transform from a 
     socially 'toxic' display into an integrated 'extraordinary' story?",
    resolution_mechanism("Audit of long-term social standing for sharers vs. private integrators"),
    impact("If years=0: Radical transparency is a Rope. If years>0: It is a Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    relationship_limit_nature,
    "Are relationship 'limits' a biological Mountain (necessity) or a 
     social Snare (constraint) that can be optimized away by solo polyamory?",
    resolution_mechanism("Longitudinal happiness metrics of solo polyamorists vs. traditionalists"),
    impact("If Mountain: Solo polyamory is a deceptive Rope. If Snare: It is a true escape."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Radical Transparency ("Throwing open the door")
 * Viability: Effectively what the NYT author did.
 * Suppression: Rejected by the blog author as "sophomoric" and 
 * lacking "life experience".
 * * ALTERNATIVE 2: Solo Polyamory
 * Viability: A term describing non-escalating commitments.
 * Suppression: Shunted as a potential fantasy or "force-fed" alternative 
 * to marriage/loneliness.
 * * CONCLUSION:
 * The existence of Alternative 1 (Sharing) is identified as a "Snare" 
 * for the subject, making Rule No. 1 a functional "Rope" for survival 
 * in a judgmental social environment.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_private_identity_integration].
 * 2. Multi-perspective: ?- multi_index_report(private_identity_integration).
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
% Coordination mechanism in social domain — moderate institutional framing
domain_priors:theater_ratio(private_identity_integration, 0.15).
narrative_ontology:constraint_metric(private_identity_integration, theater_ratio, 0.15).
