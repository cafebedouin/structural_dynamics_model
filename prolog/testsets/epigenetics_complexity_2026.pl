% ============================================================================
% CONSTRAINT STORY: epigenetics_complexity_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The hidden power of epigenetics" by Colin Barras
% ============================================================================

:- module(epigenetics_complexity_2026, []).

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
 * * constraint_id: epigenetics_complexity_2026
 * human_readable: Epigenetic Regulatory Constraint
 * domain: technological/biological
 * temporal_scope: 2001-2026
 * spatial_scope: Global (Biological)
 * * SUMMARY:
 * Following the discovery that the human genome contains only ~20,000 protein-coding 
 * genes—far fewer than the 40,000 initially expected—epigenetics emerged as the 
 * primary mechanism explaining biological complexity. It describes 
 * molecules interacting with DNA/RNA to influence gene activity without 
 * altering the underlying code.
 * * KEY AGENTS:
 * - The Biological Subject: (e.g., Yeast/Cell) - Experiences epigenetic shifts as survival mandates.
 * - The Geneticist: (Institutional/Analytical) - Uses epigenetics as a tool to "squeeze more complexity" from the genome.
 * - The Environment: (The Constraint Driver) - Toxic chemicals or stressors that trigger epigenetic modifications.
 * * NARRATIVE ARC:
 * The collapse of "gene-centrism" (2001) forced a shift toward understanding 
 * how environmental factors influence gene expression. A 2019 study 
 * showed yeast surviving toxins via epigenetic silencing, which eventually 
 * "hard-coded" into genetic evolution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(epigenetics_complexity_2026, 0, 10).

% Base extractiveness score: 0.1 (Minimal asymmetry)
% Rationale: Epigenetics is a fundamental biological function; it doesn't 
% "extract" in a social sense, though it forces physiological resource reallocation.
domain_priors:base_extractiveness(epigenetics_complexity_2026, 0.1).

% Suppression score: 0.2 (Low)
% Rationale: Alternatives like "pure genetic determinism" were naturally suppressed 
% by the empirical data of the Human Genome Project.
domain_priors:suppression_score(epigenetics_complexity_2026, 0.2).

% Enforcement requirements
% Emerges naturally through biochemical interactions.
domain_priors:emerges_naturally(epigenetics_complexity_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(epigenetics_complexity_2026, biological_complexity).
constraint_victim(epigenetics_complexity_2026, genetic_determinism_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EVOLUTIONARY BIOLOGIST - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-shaping/Scientific observer)
   WHEN: historical (Century-scale evolutionary rethinking)
   WHERE: mobile (Can navigate between genetic and epigenetic models)
   SCOPE: global (Species-wide application)
   
   WHY THIS CLASSIFICATION:
   For the scientist, epigenetics is a "Rope"—a functional coordination 
   mechanism that allows them to explain the complexity of evolution 
   beyond simple protein-coding genes.
   
   NARRATIVE EVIDENCE:
   "It helped turbocharge a movement to rethink the evolutionary process".
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STRESSED ORGANISM (Yeast) - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (Subject to environmental toxins)
   WHEN: immediate (Short-term survival)
   WHERE: trapped (Cannot leave the toxic environment)
   SCOPE: local (The immediate chemical exposure)
   
   WHY THIS CLASSIFICATION:
   For an organism in a toxic environment, epigenetics acts as a "Noose." 
   The environment forces a "silent or die" choice. The mechanism 
   constrains the cell's behavior to a narrow survival path.
   
   NARRATIVE EVIDENCE:
   "The toxin killed the yeast... But yeast cells with the capacity to silence 
   that gene... survived".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    epigenetics_complexity_2026,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BIOCHEMICAL ANALYST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of molecular law)
   WHEN: civilizational (Species-level biological constants)
   WHERE: analytical (Universal application)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From a molecular perspective, the interaction of molecules with DNA/RNA 
   is a "Mountain"—a fixed physical reality of how life operates. 
   It is a zero-degree-of-freedom constraint of biological architecture.
   
   NARRATIVE EVIDENCE:
   "Epigenetics is a catch-all term to describe how a wide variety of 
   molecules interact with DNA or RNA".
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
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(epigenetics_complexity_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, T1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, T2, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2.

test(environmental_enforcement) :-
    domain_priors:emerges_naturally(epigenetics_complexity_2026).

:- end_tests(epigenetics_complexity_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SELECTION: Focused on the contrast between the "Yeast" as 
 * a subject of environmental pressure (Noose) and the "Biologist" using 
 * epigenetics as an explanatory tool (Rope).
 * * 2. CLASSIFICATION RATIONALE:
 * - Subject (Noose): Survival in toxins is a narrow, forced path.
 * - Biologist (Rope): A functional way to explain why 20,000 genes suffice 
 * for human complexity.
 * * 3. AMBIGUITIES: The source mentions "some biologists are convinced" 
 * epigenetics can influence evolution, implying this isn't a settled 
 * Mountain yet but a contested Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    epigenetic_inheritance_breadth,
    "To what extent are epigenetic modifications 'reset' vs 'inherited' in humans?",
    resolution_mechanism("Long-term multi-generational cohort studies on environmental stress"),
    impact("If fully reset: Epigenetics is a biographical Rope. If inherited: It's a generational Noose/Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    complexity_source_completeness,
    "Does epigenetics fully explain the gap between gene count and biological complexity, or are other mechanisms missing?",
    resolution_mechanism("Complete biochemical modeling of the cell-state space"),
    impact("If incomplete: Epigenetics remains a partial Rope. If complete: It becomes a foundational Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Gene-Centric Determinism
 * Viability: High prior to 2001 (predicted 100k+ genes).
 * Suppression: Empirically rejected by the 20,000 gene count.
 * Evidence: "The number was so far below expectations".
 * * CONCLUSION:
 * The collapse of Alternative 1 shifted the scientific paradigm from a simple 
 * Blueprint (Mountain) to a dynamic Regulatory System (Rope).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [epigenetics_complexity_2026].
% Report: ?- multi_index_report(epigenetics_complexity_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
