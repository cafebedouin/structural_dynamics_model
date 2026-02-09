% ============================================================================
% CONSTRAINT STORY: great_awakening_rekindling
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 1.5 Pro
% Source: The First Great Awakening and the "Old Light" vs "New Light" schism
% ============================================================================

:- module(constraint_kjv_great_awakening, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

% The Structural Anchor for system extraction
narrative_ontology:interval(kjv_great_awakening, 0, 10).
narrative_ontology:constraint_claim(kjv_great_awakening, snare).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: kjv_great_awakening
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
 * decentralized authority as a "Snare" strangling colonial stability.
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
domain_priors:base_extractiveness(kjv_great_awakening, 0.1).

% Suppression: Moderate (0.4)
% Old Light ministers tried to ban itinerant preaching, but the 
% "Exit Option" was psychological and local, making suppression difficult.
domain_priors:suppression_score(kjv_great_awakening, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(kjv_great_awakening, extractiveness, 0.1).
narrative_ontology:constraint_metric(kjv_great_awakening, suppression_requirement, 0.4).

% Enforcement: Emerges naturally (Spontaneous revivalism)
domain_priors:emerges_naturally(kjv_great_awakening).

narrative_ontology:constraint_beneficiary(kjv_great_awakening, individual_empowerment).
narrative_ontology:constraint_victim(kjv_great_awakening, institutional_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NEW LIGHT CONVERT - ROPE
   --------------------------------------------------------------------------
   WHO: powerless (socially)
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
    kjv_great_awakening,
    rope,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE OLD LIGHT MINISTER - SNARE
   --------------------------------------------------------------------------
   WHO: institutional - The established religious elite.
   WHEN: biographical - Their career and social standing are at stake.
   WHERE: constrained - They are tied to their specific parish/pulpit.
   SCOPE: local/national - The stability of the colonial hierarchy.
   
   WHY THIS CLASSIFICATION:
   For the established elite, this "New Light" use of the KJV is a Snare. 
   It unmoors the text from scholarly tradition, allowing "unlearned" 
   men to challenge the elite's power. It feels like an asymmetric 
   attack on the social order they are sworn to protect.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_great_awakening,
    snare,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

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
    kjv_great_awakening,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(continental)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(kjv_great_awakening_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates how the Great Awakening is viewed differently across agents.
 */
test(multi_perspective_variance) :-
    % New Light Convert (Rope)
    constraint_indexing:constraint_classification(
        kjv_great_awakening,
        Type1,
        context(agent_power(powerless), time_horizon(immediate), exit_options(mobile), spatial_scope(regional))
    ),
    % Old Light Minister (Snare)
    constraint_indexing:constraint_classification(
        kjv_great_awakening,
        Type2,
        context(agent_power(institutional), time_horizon(biographical), exit_options(constrained), spatial_scope(national))
    ),
    % Analytical Observer (Mountain)
    constraint_indexing:constraint_classification(
        kjv_great_awakening,
        Type3,
        context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(continental))
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3. % Ensure all three are distinct

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that the institutional elite (Old Light) perceive higher extraction
 * of their authority than the empowered converts.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(powerless), time_horizon(immediate), exit_options(mobile), spatial_scope(regional)),
    ContextPowerful = context(agent_power(institutional), time_horizon(biographical), exit_options(constrained), spatial_scope(national)),
    constraint_indexing:extractiveness_for_agent(kjv_great_awakening, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(kjv_great_awakening, ContextPowerful, Score2),
    Score1 < Score2.  % The "powerful" experience more extraction in this case.

/**
 * TEST 3: Time-horizon immutability
 * Demonstrates that while the revival was a dynamic event, the text itself is a historical constant.
 */
test(time_immutability) :-
    % Short horizon (immediate) sees the event as a dynamic Rope
    constraint_indexing:effective_immutability(time_horizon(immediate), exit_options(mobile), rope),
    % Long horizon (historical) sees the text as an immutable Mountain.
    constraint_indexing:effective_immutability(time_horizon(historical), exit_options(analytical), mountain).

:- end_tests(kjv_great_awakening_tests).

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
 * 1. SHIFT FROM MOUNTAIN TO ROPE: This is the most important pedagogical 
 * point. A constraint is a "Mountain" when it is ignored because it is 
 * "just there." It becomes a "Rope" when someone picks it up and 
 * uses it to move.
 * 
 * 2. THE SNARE OF THE ELITE: It is ironic that the institutional power 
 * (ministers) felt "snared" by their own holy book when the peasants 
 * started reading it "incorrectly."
 * 
 * 3. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        sincerity_of_conversion,
 *        "Is the 'New Light' conversion a genuine empowerment (Rope) or a form of social contagion (Snare)?",
 *        resolution_mechanism("Analysis of long-term life outcomes of converts vs. non-converts."),
 *        impact("If contagion: The Rope is a ilusion. If genuine: The Rope is a real tool for social change."),
 *        confidence_without_resolution(medium)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Rationalism / The Enlightenment
 *    Viability: High (e.g., Thomas Jefferson, Benjamin Franklin). A major intellectual movement of the era.
 *    Suppression: In the highly religious context of the American colonies, rationalism was often suppressed or viewed with suspicion as "Deism" or "Atheism," especially outside of elite circles.
 * 
 * CONCLUSION:
 * The Great Awakening provided a "Rope" for populist religious expression, offering an alternative to both the rigid "Mountain" of the established State Church and the intellectually demanding "Snare" of secular rationalism, which was less accessible to the average person.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/kjv_great_awakening].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(kjv_great_awakening).
 * 
 * 3. Run tests:
 *    ?- run_tests(kjv_great_awakening_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(kjv_great_awakening).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(kjv_great_awakening, [other_id]).
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
% Extraction is substantive — the constraint's costs are real, not theatrical
domain_priors:theater_ratio(kjv_great_awakening, 0.21).
narrative_ontology:constraint_metric(kjv_great_awakening, theater_ratio, 0.21).
