% ============================================================================
% CONSTRAINT STORY: epigenetics_complexity_2026
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The hidden power of epigenetics" by Colin Barras
% ============================================================================

:- module(constraint_epigenetics_complexity_2026, []).

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
 * constraint_id: epigenetics_complexity_2026
 * human_readable: Epigenetic Regulatory Constraint
 * domain: biological/scientific
 * temporal_scope: 2001-2026
 * spatial_scope: Global (Cellular level)
 * 
 * SUMMARY:
 * Following the discovery that the human genome contains only ~20,000 protein-coding 
 * genes, epigenetics emerged as the primary mechanism explaining biological complexity. 
 * It describes how molecules interact with DNA/RNA to influence gene activity without 
 * altering the underlying code, often in response to environmental pressures.
 * 
 * KEY AGENTS:
 * - The Stressed Organism (Individual Powerless): A cell/organism forced to adapt to environmental toxins.
 * - The Evolutionary Biologist (Institutional): Uses epigenetics as an explanatory framework.
 * - The Biochemist (Analytical): Views the molecular interactions as immutable physical law.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(epigenetics_complexity_2026, 0, 10).
narrative_ontology:constraint_claim(epigenetics_complexity_2026, rope).

% Base extractiveness score: 0.1 (Minimal asymmetry)
% Rationale: Epigenetics is a fundamental biological function; it doesn't 
% "extract" in a social sense, though it forces physiological resource reallocation.
domain_priors:base_extractiveness(epigenetics_complexity_2026, 0.1).

% Suppression score: 0.2 (Low)
% Rationale: Alternatives like "pure genetic determinism" were naturally suppressed 
% by the empirical data of the Human Genome Project.
domain_priors:suppression_score(epigenetics_complexity_2026, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(epigenetics_complexity_2026, extractiveness, 0.1).
narrative_ontology:constraint_metric(epigenetics_complexity_2026, suppression_requirement, 0.2).

% Enforcement requirements: Emerges naturally through biochemical interactions.
domain_priors:emerges_naturally(epigenetics_complexity_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(epigenetics_complexity_2026, adapting_organisms).
constraint_victim(epigenetics_complexity_2026, none).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EVOLUTIONARY BIOLOGIST - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-shaping/Scientific observer)
   WHEN: historical (Century-scale evolutionary rethinking)
   WHERE: mobile (Can navigate between genetic and epigenetic models)
   
   WHY THIS CLASSIFICATION:
   For the scientist, epigenetics is a "Rope"—a functional coordination 
   mechanism that allows them to explain the complexity of evolution 
   beyond simple protein-coding genes.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    epigenetics_complexity_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STRESSED ORGANISM (YEAST) - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Subject to environmental toxins)
   WHEN: immediate (Short-term survival)
   WHERE: trapped (Cannot leave the toxic environment)
   
   WHY THIS CLASSIFICATION:
   For an organism in a toxic environment, epigenetics acts as a "Snare." 
   The environment forces a "silence or die" choice. The mechanism 
   constrains the cell's behavior to a narrow survival path.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    epigenetics_complexity_2026,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOCHEMICAL ANALYST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of molecular law)
   WHEN: civilizational (Species-level biological constants)
   WHERE: analytical (Universal application)
   
   WHY THIS CLASSIFICATION:
   From a molecular perspective, the interaction of molecules with DNA/RNA 
   is a "Mountain"—a fixed physical reality of how life operates. 
   It is a zero-degree-of-freedom constraint of biological architecture.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    epigenetics_complexity_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(epigenetics_complexity_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(epigenetics_complexity_2026_tests).

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
 * 1. PERSPECTIVE SELECTION: 
 *    Focused on the contrast between the organism as a subject of environmental 
 *    pressure (Snare), the biologist using epigenetics as an explanatory tool (Rope), 
 *    and the biochemist seeing the underlying physical laws (Mountain).
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Subject (Snare): Survival in toxins is a narrow, forced path.
 *    - Biologist (Rope): A functional way to explain why 20,000 genes suffice.
 *    - Biochemist (Mountain): The underlying chemical interactions are immutable laws.
 * 
 * 3. AMBIGUITIES: 
 *    The source mentions "some biologists are convinced" epigenetics can influence 
 *    evolution, implying this isn't a settled Mountain for all scientists yet, but a 
 *    contested Rope, which is well-represented by the multiple perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Key uncertainties about the long-term impact and completeness of the theory.
 */

omega_variable(
    epigenetic_inheritance_breadth,
    "To what extent are epigenetic modifications 'reset' vs 'inherited' in humans across generations?",
    resolution_mechanism("Long-term multi-generational cohort studies tracking environmental stress and epigenetic markers."),
    impact("If fully reset: Epigenetics is a biographical Rope. If inherited: It can become a generational Snare or Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    complexity_source_completeness,
    "Does epigenetics fully explain the gap between gene count and biological complexity, or are other major mechanisms yet to be discovered?",
    resolution_mechanism("Complete biochemical modeling of the cell-state space, and discovery of novel regulatory pathways."),
    impact("If incomplete: Epigenetics remains a partial Rope. If complete: It becomes a foundational Mountain of biology."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Gene-Centric Determinism
 *    Viability: Was the dominant theory prior to the Human Genome Project's completion in 2001.
 *    Suppression: Empirically falsified by the unexpectedly low gene count (~20,000) found in the human genome.
 *    Evidence: "The number was so far below expectations that some thought it must be an error."
 * 
 * CONCLUSION:
 * The collapse of Alternative 1 (gene-centric determinism) forced the scientific community to adopt epigenetics as a new 'Rope' to explain observed biological complexity, shifting the paradigm from a simple blueprint to a dynamic regulatory system.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/epigenetics_complexity_2026].
 * 2. Multi-perspective: ?- multi_index_report(epigenetics_complexity_2026).
 * 3. Run tests: ?- run_tests(epigenetics_complexity_2026_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */