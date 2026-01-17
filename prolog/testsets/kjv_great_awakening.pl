% ============================================================================
% CONSTRAINT STORY: great_awakening_rekindling
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 1.5 Pro
% Source: The First Great Awakening and the "Old Light" vs "New Light" schism
% ============================================================================

:- module(great_awakening_rekindling, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% The Structural Anchor for system extraction
narrative_ontology:interval(great_awakening_rekindling, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: great_awakening_rekindling
 * human_readable: The Great Awakening Re-Indexing
 * domain: religious/social/psychological
 * temporal_scope: 1730 - 1750 CE
 * spatial_scope: American Colonies (Inter-colonial)
 * * SUMMARY:
 * After a century of stability, the KJV had become a "Mountain"—a dry, 
 * institutional fixture of the "Old Light" establishment. The Great 
 * Awakening shattered this by emphasizing "heart religion." This turned 
 * the text into a "Rope" for the marginalized (slaves, women, uneducated) 
 * by granting them direct interpretive agency, while the elite saw this 
 * decentralized authority as a "Noose" strangling colonial stability.
 * * KEY AGENTS:
 * - The "New Light" Convert: Finding personal liberation in the text.
 * - The "Old Light" Minister: Protecting the "Mountain" of established order.
 * - The Analytical Historian: Observing the birth of American individualism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: Low (0.1)
% The movement was highly decentralized; the "benefit" of the text 
% flowed toward individual empowerment rather than an institution.
domain_priors:base_extractiveness(great_awakening_rekindling, 0.1).

% Suppression: Moderate (0.4)
% Old Light ministers tried to ban itinerant preaching, but the 
% "Exit Option" was psychological and local, making suppression difficult.
domain_priors:suppression_score(great_awakening_rekindling, 0.4).

% Enforcement: Emerges naturally (Spontaneous revivalism)
domain_priors:emerges_naturally(great_awakening_rekindling).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NEW LIGHT CONVERT - ROPE
   --------------------------------------------------------------------------
   WHO: individual_powerless (socially) -> individual_moderate (spiritually)
   WHEN: immediate - The "instant" of conversion.
   WHERE: mobile - They can leave "dead" churches for "live" ones.
   SCOPE: local/regional - The traveling revival circuit.
   
   WHY THIS CLASSIFICATION:
   The KJV is a Rope. It is no longer a dead law (Mountain) but a dynamic 
   lifeline. The convert uses the text to bypass the educated clergy and 
   connect directly to God. It provides agency and coordination with other 
   believers across colonial lines.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    great_awakening_rekindling,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE OLD LIGHT MINISTER - NOOSE
   --------------------------------------------------------------------------
   WHO: institutional - The established religious elite.
   WHEN: biographical - Their career and social standing are at stake.
   WHERE: constrained - They are tied to their specific parish/pulpit.
   SCOPE: local/national - The stability of the colonial hierarchy.
   
   WHY THIS CLASSIFICATION:
   For the established elite, this "New Light" use of the KJV is a Noose. 
   It unmoors the text from scholarly tradition, allowing "unlearned" 
   men to challenge the elite's power. It feels like an asymmetric 
   attack on the social order they are sworn to protect.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    great_awakening_rekindling,
    noose,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: analytical - Looking back at the inevitability of the shift.
   WHEN: historical - 250+ years of perspective.
   WHERE: analytical - Not bound by the religious fervor.
   SCOPE: continental - The shaping of the American character.
   
   WHY THIS CLASSIFICATION:
   From a historical distance, the KJV functions as a Mountain. Regardless 
   of the "Old vs New Light" debate, the KJV was the inescapable 
   linguistic bedrock of the era. It was the "natural law" of communication 
   that made the Awakening possible across disparate colonies.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    great_awakening_rekindling,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(continental)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(awakening_tests).

test(re_indexing_agency) :-
    % Test that mobility + moderate power yields a Rope (re-claiming the text)
    constraint_indexing:constraint_classification(great_awakening_rekindling, rope, context(agent_power(individual_moderate), _, exit_options(mobile), _)).

test(institutional_threat_noose) :-
    % Test that institutional agents seeing power erode view the constraint as a Noose
    constraint_indexing:constraint_classification(great_awakening_rekindling, noose, context(agent_power(institutional), _, _, _)).

test(historical_bedrock_mountain) :-
    % Test that historical/analytical perspectives see the persistent "Mountain"
    constraint_indexing:constraint_classification(great_awakening_rekindling, mountain, context(agent_power(analytical), time_horizon(historical), _, _)).

:- end_tests(awakening_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. SHIFT FROM MOUNTAIN TO ROPE: This is the most important pedagogical 
 * point. A constraint is a "Mountain" when it is ignored because it is 
 * "just there." It becomes a "Rope" when someone picks it up and 
 * uses it to move.
 * * 2. THE NOOSE OF THE ELITE: It is ironic that the institutional power 
 * (ministers) felt "noosed" by their own holy book when the peasants 
 * started reading it "incorrectly."
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE: Rationalism / The Enlightenment
 * Viability: High (Jefferson, Franklin).
 * Suppression: In the religious sphere, rationalism was suppressed as 
 * "Deism" or "Atheism."
 * * CONCLUSION:
 * The Awakening was a "Rope" because it provided a populist alternative 
 * to both the "Mountain" of the State Church and the "Noose" of secular 
 * rationalism for the average person.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
