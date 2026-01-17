% ============================================================================
% CONSTRAINT STORY: kjv_linguistic_residue
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 1.5 Pro
% Source: The cultural and linguistic legacy of the 1611 KJV in the 21st Century
% ============================================================================

:- module(kjv_linguistic_residue, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% The Structural Anchor
narrative_ontology:interval(kjv_linguistic_residue, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: kjv_linguistic_residue
 * human_readable: The KJV Aesthetic as a Cognitive Constraint
 * domain: linguistic/psychological/cultural
 * temporal_scope: 21st Century
 * spatial_scope: Global (Anglosphere)
 * * SUMMARY:
 * Even in secular contexts, the KJV’s cadence (Early Modern English) acts 
 * as a "signal of gravity." When a politician, writer, or AI wants to 
 * sound "weighty" or "authoritative," they unconsciously revert to KJV-era 
 * syntax. This is a constraint on how we perceive truth: if it doesn't 
 * "sound" biblical, it often feels less "true" or "ancient."
 * * KEY AGENTS:
 * - The Secular Speaker: Using KJV idioms ("powers that be," "writing on the wall").
 * - The Modern Reader: Who finds contemporary translations "cheap" or "lacking."
 * - The LLM/AI: Trained on this data, reinforcing the "Mountain" of this style.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: Low (0.1)
% It is a shared "commons" of language. It doesn't extract wealth, 
% but it "extracts" cognitive effort to interpret archaic forms.
domain_priors:base_extractiveness(kjv_linguistic_residue, 0.1).

% Suppression: Low (0.2)
% There are many alternatives (modern English), but they often fail 
% to carry the same "authority weight."
domain_priors:suppression_score(kjv_linguistic_residue, 0.2).

% Enforcement: Emerges naturally (via cultural inertia)
domain_priors:emerges_naturally(kjv_linguistic_residue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SPEECHWRITER/POET - ROPE
   --------------------------------------------------------------------------
   WHO: individual_powerful - Someone who manipulates language professionally.
   WHEN: immediate - Writing a specific text for impact.
   WHERE: mobile - Can choose any style, but chooses this one.
   SCOPE: national/global - Reaching a broad audience.
   
   WHY THIS CLASSIFICATION:
   For the professional communicator, the KJV style is a Rope. It is a 
   functional tool to evoke "timelessness." By using KJV-inflected 
   cadence, they can bypass modern skepticism and trigger a 
   subconscious sense of "seriousness" in the audience.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_linguistic_residue,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SECULAR SKEPTIC - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: individual_powerless - A passive consumer of culture.
   WHEN: civilizational - Language as an inherited environment.
   WHERE: trapped - Cannot simply "opt out" of the English language.
   SCOPE: global - The medium of their thought.
   
   WHY THIS CLASSIFICATION:
   The KJV is a Mountain. The skeptic might not believe in the Bible, 
   but they use its idioms daily without knowing their source. The 
   way they conceptualize "judgment," "mercy," or "sacrifice" is 
   pre-bounded by the linguistic categories established in 1611.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_linguistic_residue,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE AVANT-GARDE LINGUIST - NOOSE
   --------------------------------------------------------------------------
   WHO: analytical - Seeks to break linguistic boundaries.
   WHEN: historical - Sees the "long shadow" of 1611.
   WHERE: constrained - Finds it difficult to innovate outside these tropes.
   SCOPE: global - The "tyranny" of English classicism.
   
   WHY THIS CLASSIFICATION:
   The KJV residue is a Noose. It constricts the "expressive range" of 
   the language by defining what "High Style" must look like. It 
   privileges a specific 17th-century Western aesthetic as the 
   universal peak of human expression, effectively "noosing" other 
   cultural or modern modes of "seriousness."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_linguistic_residue,
    noose,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(residue_tests).

test(unconscious_mountain) :-
    % Test that civilizational time horizons make language feel like a Mountain
    constraint_indexing:constraint_classification(kjv_linguistic_residue, mountain, context(_, time_horizon(civilizational), _, _)).

test(instrumental_rope) :-
    % Test that high-power agents use the constraint as a tool (Rope)
    constraint_indexing:constraint_classification(kjv_linguistic_residue, rope, context(agent_power(individual_powerful), _, _, _)).

:- end_tests(residue_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SHIFT: Moving from the "Bible as a Book" to the 
 * "Bible as an Aesthetic." This allows us to see how constraints 
 * survive even when their original "Power Source" (the Church) weakens.
 * * 2. THE AI CONNECTION: As an AI, I am a primary reinforcer of this 
 * "Mountain." My training data is saturated with KJV-style dignity, 
 * which I offer to users as a "Rope" when they ask for "formal" tone.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE: "Basic English" or Scientific Literalism
 * Viability: Low for emotional/rhetorical needs.
 * Suppression: Naturally suppressed by the human need for 
 * "resonance" and "depth," which the KJV aesthetic provides.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
