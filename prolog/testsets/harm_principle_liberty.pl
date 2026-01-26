% ============================================================================
% CONSTRAINT STORY: harm_principle_liberty
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: On Liberty by John Stuart Mill (Courtney Edition)
% ============================================================================

:- module(constraint_harm_principle_liberty, []).

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
 * * constraint_id: harm_principle_liberty
 * human_readable: The Liberty-Harm Dialectic
 * domain: political/social
 * temporal_scope: 19th Century (1859 - Present)
 * spatial_scope: Global (Modern Liberal Democracies)
 * * SUMMARY:
 * Mill identifies the "Tyranny of the Majority" and the "Despotism of Custom" 
 * as the primary constraints of the modern age. He proposes the "Harm Principle" 
 * as a meta-constraint: society may only coerce an individual to prevent 
 * harm to others. Any other interference (for the person's own good) is 
 * classified as a "Snare" masquerading as a "Rope."
 * * KEY AGENTS:
 * - The Individual (The Eccentric): The victim of social stigma/custom.
 * - The Majority (Public Opinion): The institutional force enforcing uniformity.
 * - The Statesman: The agent tasked with applying the "Republican Remedy" 
 * of limited interference.
 * * NARRATIVE ARC:
 * The text moves from a historical struggle against political rulers (One/Few) 
 * to a more insidious struggle against "Social Tyranny" (The Many). 
 * Mill argues that unless "Individuality" is preserved as a "Mountain" of 
 * right, the "Rope" of social coordination will inevitably tighten into 
 * a "Snare" of Chinese-style stationariness.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(on_liberty_analysis, 0, 10).
narrative_ontology:constraint_claim(harm_principle_liberty, rope).

% Base extractiveness score: 0.5
% Rationale: Society extracts a "fair share" of burdens and conformity 
% to "not injuring interests," but the individual extracts "protection" 
% and the "security" to pursue their own good.
domain_priors:base_extractiveness(harm_principle_liberty, 0.5).

% Suppression score: 0.8
% Rationale: Mill warns that the "Despotism of Custom" is in unceasing 
% antagonism to progress. Social stigma is "as efficacious as law."
domain_priors:suppression_score(harm_principle_liberty, 0.8).

% Enforcement requirements: Law and "Moral Repression" (Public Opinion).
domain_priors:requires_active_enforcement(harm_principle_liberty).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(harm_principle_liberty, extractiveness, 0.5).
narrative_ontology:constraint_metric(harm_principle_liberty, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(harm_principle_liberty, [collective_stability, future_generations]).
constraint_victim(harm_principle_liberty, [eccentrics, original_thinkers]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NON-CONFORMIST - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The person who does "what nobody does."
   WHEN: immediate - Facing the "vituperative language" of the majority.
   WHERE: trapped - The "social stigma" leaves "fewer means of escape" than law.
   SCOPE: local - Their immediate neighborhood and class.
   
   WHY THIS CLASSIFICATION:
   For the eccentric, social custom is a "Snare." It is a coercive system 
   that "dwarfs" their mental expansion to fit a "commonplace" model, 
   offering no exit but "lunacy" proceedings or poverty.
   
   NARRATIVE EVIDENCE:
   "Enslaving the soul itself... penetrating much more deeply into the 
   details of life."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    harm_principle_liberty,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(harm_principle_liberty, collective_mediocrity),
        constraint_victim(harm_principle_liberty, eccentrics),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(harm_principle_liberty, S),
    S > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PHILOSOPHER (Mill) - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical - Observing the "history of mankind."
   WHEN: generational - Thinking of "the permanent interests of man."
   WHERE: arbitrage - Balancing "individual spontaneity" vs "social control."
   SCOPE: global - "In the modern world."
   
   WHY THIS CLASSIFICATION:
   Mill views the Harm Principle as a "Rope"—a vital coordination 
   mechanism. It is a functional rule that allows for "different 
   experiments of living" to prevent civilizational stagnation.
   
   NARRATIVE EVIDENCE:
   "The only freedom which deserves the name, is that of pursuing our 
   own good in our own way."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    harm_principle_liberty,
    rope,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(arbitrage),
        constraint_beneficiary(harm_principle_liberty, human_progress),
        constraint_victim(harm_principle_liberty, []),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: CUSTOM / TRADITION - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional - The "magical influence of custom."
   WHEN: civilizational - "Thousands of years" in the East.
   WHERE: trapped - "Stationary" nations like China.
   SCOPE: national - The "general average of mankind."
   
   WHY THIS CLASSIFICATION:
   To the average person, the "rules which obtain among themselves" 
   appear as "self-evident" as laws of nature. It is a "Mountain" 
   that defines reality and requires no reasons.
   
   NARRATIVE EVIDENCE:
   "Continually mistaken for the first [nature]... custom is there, 
   in all things, the final appeal."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    harm_principle_liberty,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(trapped),
        constraint_beneficiary(harm_principle_liberty, []),
        constraint_victim(harm_principle_liberty, []),
        spatial_scope(national)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(harm_principle_tests).

test(gap_analysis_eccentric_vs_majority) :-
    % Eccentric sees a Snare; Mill sees a Rope; Majority sees a Mountain.
    constraint_indexing:constraint_classification(harm_principle_liberty, snare, context(agent_power(individual_powerless), _, _, _, _, _)),
    constraint_indexing:constraint_classification(harm_principle_liberty, rope, context(agent_power(analytical), _, _, _, _, _)),
    constraint_indexing:constraint_classification(harm_principle_liberty, mountain, context(agent_power(institutional), _, _, _, _, _)).

test(extraction_stigma) :-
    % Verify high extractiveness when social stigma is applied to self-regarding acts
    domain_priors:base_extractiveness(harm_principle_liberty, 0.5).

test(enforcement_required) :-
    domain_priors:requires_active_enforcement(harm_principle_liberty).

:- end_tests(harm_principle_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVAL GAP: The brilliance of Mill’s analysis is identifying 
 * that what the Majority sees as a "Mountain" (Natural Moral Order) is 
 * actually a "Snare" (Coercive Custom) for the Individual. 
 * 2. THE HARM PRINCIPLE AS ROPE: I classified the "Principle" itself 
 * as a Rope because it is a "subordinate legislation" or "centralization 
 * of information" meant to facilitate coordination without crushing 
 * individuality.
 * 3. EXTRACTIVENESS: Set at 0.5. Mill admits society has a "jurisdiction" 
 * over conduct affecting others. The extraction is the "labour and 
 * sacrifices" for common defense, which is a fair trade in a Rope context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_boundary,
    "Where is the line between 'self-regarding' and 'social' acts in a hyper-connected society?",
    resolution_mechanism("Requires a recursive 'impact audit'—if every act 'affects' someone through sympathy, the Snare expands."),
    impact("If the boundary is porous: The Rope of Liberty becomes a Snare of total surveillance."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Paternalism (M. Comte's "Despotism of Society")
 * Viability: Highly viable (The "Chinese Ideal").
 * Suppression: Mill rejects this as "dwarfing" humanity.
 * * ALTERNATIVE 2: Theological Morality
 * Viability: The historical "Universal Church."
 * Suppression: Mill notes it is "incomplete and one-sided," a "reaction 
 * against Paganism."
 * * CONCLUSION:
 * The existence of these alternatives (which seek to move the constraint 
 * back to Mountain/Snare) is what necessitates Mill's defensive Rope 
 * of "Limited Government."
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
