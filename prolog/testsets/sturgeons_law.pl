% ============================================================================
% CONSTRAINT STORY: sturgeons_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Theodore Sturgeon (1951/1958) / Science Fiction Criticism
% ============================================================================

:- module(constraint_sturgeons_law, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_metric/3,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: sturgeons_law
 * human_readable: Sturgeon's Law ("90% of everything is crap")
 * domain: artistic/sociological
 * temporal_scope: Permanent (Modern Era)
 * spatial_scope: Global (Creative/Information Systems)
 * * SUMMARY:
 * Formulated by sci-fi writer Theodore Sturgeon, the law states that "ninety percent 
 * of everything is crud." It was originally a defense of science fiction against 
 * critics, arguing that while 90% of sci-fi is bad, 90% of *all* literature, art, 
 * and film is also bad. It represents a fundamental baseline of quality distribution.
 * * KEY AGENTS:
 * - The Sci-Fi Writer (Sturgeon): Seeking to defend the 10% "pearls" from being 
 * dismissed by the presence of the 90% "crud."
 * - The Critic: Uses the 90% to invalidate a whole genre or domain.
 * - The Consumer: Navigating a sea of mediocrity to find value.
 * * NARRATIVE ARC:
 * The law functions as a protective shield for niche domains. By acknowledging 
 * the "Mountain" of mediocrity, the observer is freed to find the "Rope" of 
 * excellence. It becomes a "Snare" when the 90% becomes so overwhelming that 
 * the 10% is never discovered or funded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(sturgeon_interval, 0, 10).
narrative_ontology:constraint_claim(sturgeons_law, mountain).

% Base extractiveness: 0.1 (Low)
% Rationale: The "crud" doesn't necessarily extract value; it mostly 
% consumes attention and "shelf space" without providing a return.
domain_priors:base_extractiveness(sturgeons_law, 0.1).

% Suppression score: 0.4 (Moderate)
% Rationale: The sheer volume of the 90% often suppresses the visibility of 
% the high-quality 10%, requiring active "curation" to overcome.
domain_priors:suppression_score(sturgeons_law, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(sturgeons_law, extractiveness, 0.1).
narrative_ontology:constraint_metric(sturgeons_law, suppression_requirement, 0.4).

% Enforcement: Emerges naturally from the difficulty of achieving excellence.
domain_priors:emerges_naturally(sturgeons_law).

% Metrics for Executive Summary
% Beneficiaries: Curators and critics who gain power by filtering the 90%.
constraint_beneficiary(sturgeons_law, expert_curators).

% Victims: The "Average" creator whose work is lost in the 90% noise.
constraint_victim(sturgeons_law, mid_tier_creatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SCI-FI WRITER - ROPE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A professional navigating a maligned genre).
   WHEN: biographical (Protecting their career's validity).
   WHERE: mobile (Can point to the 10% of other genres for comparison).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   For Sturgeon himself, the law is a Rope. It is a coordination tool that 
   standardizes expectations. By admitting 90% is bad, he moors his defense 
   to a universal truth, allowing him to pull the 10% of "hard" sci-fi up 
   to the level of high literature.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sturgeons_law,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CASUAL CONSUMER - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: powerless (A reader browseing a bookstore).
   WHEN: immediate (A single afternoon of searching).
   WHERE: trapped (Cannot change the quality of what is published).
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   To the consumer, the 90% is a Mountain. It is an immovable fact of the 
   environment. They must climb through stacks of "crud" to find something 
   worth reading. They have no power over the distribution; they can only 
   endure it.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sturgeons_law,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ALGORITHMIC FILTER - SNARE
   --------------------------------------------------------------------------
   
   WHO: institutional (A streaming service or social media platform).
   WHEN: historical (Scaling over decades of content).
   WHERE: arbitrage (Can exploit the 90% for cheap engagement).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   For the platform, Sturgeon's Law is a Snare used on the creators. Because 
   "everything is crud," the platform feels justified in using aggressive 
   algorithms that squeeze the 90% for "engagement" while charging the 10% 
   for "visibility." The law justifies a system where quality is secondary 
   to volume.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sturgeons_law,
    snare,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(sturgeons_law_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(sturgeons_law, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(sturgeons_law, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sturgeons_law, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(crud_distribution_immutability) :-
    % From a civilizational view, the ratio is seen as a physical law (Mountain).
    constraint_indexing:constraint_classification(sturgeons_law, mountain, context(_, civilizational, _, _)).

:- end_tests(sturgeons_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: Chose "Rope" for the writer because the law was 
 * intended as an empowering rhetorical device, not a defeatist one.
 * 2. SNARE: Applied the Snare concept to institutional gatekeepers who 
 * use the "90% is crud" axiom to justify poor treatment of creatives 
 * or low-quality automated systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    quality_threshold_subjectivity,
    "Is 'crap' (crud) an objective statistical failure (Mountain) or a 
    subjective perspectival judgment (Rope)?",
    resolution_mechanism("Cross-cultural longitudinal study of consensus on 'classic' status"),
    impact("If Mountain: Sturgeon's ratio is a biological limit. If Rope: It's an index of shifting taste."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Highly Curated Guild Systems
 * Viability: Historically common (e.g., Medieval guilds).
 * Suppression: High. Modern digital platforms prefer "permissionless entry" 
 * which guarantees Sturgeon's Law scales infinitely.
 * * CONCLUSION:
 * The 90/10 ratio is exacerbated by the lack of gatekeepers. In highly 
 * curated systems, the 90% might be 50%, shifting the law from a Mountain 
 * toward a Rope.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * ?- [constraint_sturgeons_law].
 * ?- run_tests(sturgeons_law_tests).
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
% Structural constraint in artistic domain — low theater, high substance
domain_priors:theater_ratio(sturgeons_law, 0.11).
narrative_ontology:constraint_metric(sturgeons_law, theater_ratio, 0.11).

% --- Analytical perspective classification (missing) ---
% chi = 0.1 * 1.15 (analytical) * 1.2 (global) = 0.138
% Classification: scaffold
constraint_indexing:constraint_classification(sturgeons_law, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
