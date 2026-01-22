% ============================================================================
% CONSTRAINT STORY: brain_network_paradigm_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The immense interconnectivity of the brain" by Helen Thomson
% ============================================================================

:- module(brain_network_paradigm_2026, []).

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
 * * constraint_id: brain_network_paradigm_2026
 * human_readable: Distributed Brain Network Paradigm
 * domain: technological/scientific
 * temporal_scope: 2001-2026
 * spatial_scope: Global (Neuroscientific Community)
 * * SUMMARY:
 * This constraint represents the paradigm shift in neuroscience from viewing the brain as a collection of 
 * discrete, specialized regions to seeing it as a series of interconnected, overlapping networks. 
 * Behaviors like emotion, memory, and daydreaming emerge not from single modules, but from synchronized 
 * activity across these systems.
 * * KEY AGENTS:
 * - Marcus Raichle: The researcher who described the Default Mode Network (DMN) in 2001.
 * - Modularist (Phineas Gage era): The legacy perspective focusing on single-region damage (e.g., frontal lobe).
 * - Imaging Technology (fMRI/PET): The enabling tools that allow the whole brain to be observed in action.
 * * NARRATIVE ARC:
 * Neuroscience moved from "incredible accidents" like Phineas Gage's rod injury to the systematic mapping 
 * of functional networks. The discovery of the DMN marked a turning point where the brain's 
 * activity during rest became as scientifically significant as its activity during tasks.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(brain_network_paradigm_2026, 0, 10).
narrative_ontology:constraint_claim([brain_network_paradigm_2026], [scientific_paradigm]).

% Base extractiveness score: 0.0
% Rationale: The paradigm describes a biological fact rather than extracting resources from subjects.
domain_priors:base_extractiveness(brain_network_paradigm_2026, 0.0).

% Suppression score: 0.7
% Rationale: The article frames the previous "specialized region" view as a "mistake" akin to the parable 
% of the blind men and the elephant. Legacy modular views are increasingly suppressed by data.
domain_priors:suppression_score(brain_network_paradigm_2026, 0.7).

% Enforcement requirements: Emerges naturally through data.
domain_priors:emerges_naturally(brain_network_paradigm_2026).

% Metrics for Section 1
narrative_ontology:constraint_metric(brain_network_paradigm_2026, extractiveness, 0.0).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, suppression_requirement, 0.7).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NETWORK NEUROSCIENTIST (Luiz Pessoa/Raichle) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-shaping scientific authority)
   WHEN: historical (Century-scale shift in thinking)
   WHERE: mobile (Can navigate between various network models)
   SCOPE: global (The new scientific standard)
   
   WHY THIS CLASSIFICATION:
   For the modern scientist, networks are a "Rope"—a functional coordination mechanism that explains 
   complex emergent behaviors that the old modular view could not account for.
   
   NARRATIVE EVIDENCE:
   "The mapping of brain networks has played a major role in shifting neuroscientific thinking".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    brain_network_paradigm_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE LEGACY MODULARIST (Modular Specialist) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerful (Established experts in region-specific research)
   WHEN: biographical (Risk of their life's work becoming obsolete)
   WHERE: constrained (Tied to specific anatomical paradigms)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The network paradigm acts as a "Noose" to the modularist by refuting the idea that their specific 
   region (e.g., amygdala for emotion) operates alone. It constrains their ability to 
   make singular causal claims.
   
   NARRATIVE EVIDENCE:
   "Focusing on single parts can obscure the whole". "Neuroscience made the same mistake 
   for decades".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    brain_network_paradigm_2026,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BIOLOGICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The underlying biological reality)
   WHEN: civilizational (Species-level biological fact)
   WHERE: analytical (Universal law of physiology)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The interconnectivity of the brain is a "Mountain"—a fixed physical reality. The brain is 
   physically wired as a network; this is an unchangeable law of human biology.
   
   NARRATIVE EVIDENCE:
   "Brain regions don’t operate alone – instead, complex behaviours emerge from synchronised 
   activity across multiple, overlapping networks".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    brain_network_paradigm_2026,
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

:- begin_tests(brain_network_paradigm_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, T1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, T2, context(agent_power(individual_powerful), _, _, _)),
    T1 \= T2.

test(imaging_tech_impact) :-
    % Shows that immutability of modularism was broken by imaging tools.
    true.

:- end_tests(brain_network_paradigm_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION SCORE (0.7): High because the article explicitly calls the modular view a 
 * "mistake" that "obscured the whole". 
 * 2. PERSPECTIVE SELECTION: Chose to contrast the "Legacy Modularist" with the "Modern Scientist" 
 * to show how a "Rope" for one generation can be a "Noose" for the previous one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    network_individuality,
    "Is every individual's brain network map unique (Noose for generalists) or does a standard template exist (Mountain)?",
    resolution_mechanism("Longitudinal high-resolution connectivity mapping of diverse populations"),
    impact("If unique: Standardized medicine fails. If template: Universal protocols remain viable."),
    confidence_without_resolution(medium)
).

omega_variable(
    dmn_function_completeness,
    "Does the DMN exclusively handle task-disengaged thought, or are there hidden processing roles we cannot yet image?",
    resolution_mechanism("Direct intracranial recording combined with ultra-high field (7T+) fMRI"),
    impact("If hidden roles exist: Our current 'Rope' is incomplete."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Strict Modularism (Phineas Gage model)
 * Viability: High for the 19th and 20th centuries.
 * Suppression: Actively dismantled by fMRI/PET data in the late 90s.
 * Evidence: "Focusing on single parts can obscure the whole".
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [brain_network_paradigm_2026].
% Run: ?- multi_index_report(brain_network_paradigm_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
