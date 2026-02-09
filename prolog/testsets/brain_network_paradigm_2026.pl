% ============================================================================
% CONSTRAINT STORY: brain_network_paradigm_2026
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The immense interconnectivity of the brain" by Helen Thomson
% ============================================================================

:- module(constraint_brain_network_paradigm_2026, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: brain_network_paradigm_2026
 * human_readable: Distributed Brain Network Paradigm
 * domain: technological/scientific
 * temporal_scope: 2001-2026
 * spatial_scope: Global (Neuroscientific Community)
 * 
 * SUMMARY:
 * This constraint represents the paradigm shift in neuroscience from viewing the brain as a collection of 
 * discrete, specialized regions to seeing it as a series of interconnected, overlapping networks. 
 * Behaviors like emotion, memory, and daydreaming emerge not from single modules, but from synchronized 
 * activity across these systems.
 * 
 * KEY AGENTS:
 * - Patient with a Brain Injury (Individual Powerless): Subject to the biological reality of their brain's network.
 * - Network Neuroscientist (Institutional): Leads the paradigm shift with new research.
 * - Legacy Modularist (Individual Moderate): Their specialized knowledge is challenged by the new paradigm.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(brain_network_paradigm_2026, 0, 10).
narrative_ontology:constraint_claim(brain_network_paradigm_2026, mountain).

% Base extractiveness: 0.0.
% The paradigm describes a biological fact rather than extracting resources from subjects.
domain_priors:base_extractiveness(brain_network_paradigm_2026, 0.0).

% Suppression score: 0.7.
% The article frames the previous "specialized region" view as a "mistake" akin to the parable 
% of the blind men and the elephant. Legacy modular views are increasingly suppressed by data.
domain_priors:suppression_score(brain_network_paradigm_2026, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(brain_network_paradigm_2026, extractiveness, 0.0).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, suppression_requirement, 0.7).

% Enforcement: Emerges naturally through data and scientific evidence.
domain_priors:emerges_naturally(brain_network_paradigm_2026).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, neuroscience).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, modular_brain_theories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: PATIENT WITH A BRAIN INJURY - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Subject to the biological reality of their brain's network)
   WHEN: biographical (The long-term journey of recovery and adaptation)
   WHERE: trapped (Bound by the physical limits of their own neurology)
   
   WHY THIS CLASSIFICATION:
   For a patient with a brain injury, the brain's network paradigm is a 'Mountain'.
   The interconnectedness of their brain determines the extent of their recovery
   and the limits of their abilities. It is an unchangeable biological reality
   that dictates the course of their life post-injury.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    brain_network_paradigm_2026,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NETWORK NEUROSCIENTIST - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-shaping scientific authority)
   WHEN: historical (Century-scale shift in thinking)
   WHERE: mobile (Can navigate between various network models)
   
   WHY THIS CLASSIFICATION:
   For the modern scientist, networks are a 'Rope'—a functional coordination mechanism
   that explains complex emergent behaviors that the old modular view could not
   account for. It's a powerful tool for understanding the brain's intricate workings.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: LEGACY MODULARIST - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate (Established experts in region-specific research)
   WHEN: biographical (Risk of their life's work becoming obsolete)
   WHERE: constrained (Tied to specific anatomical paradigms)
   
   WHY THIS CLASSIFICATION:
   The network paradigm acts as a 'Snare' to the modularist by refuting the idea
   that their specific region (e.g., amygdala for emotion) operates alone.
   It constrains their ability to make singular causal claims and challenges
   their established expertise, potentially strangling their career.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    brain_network_paradigm_2026,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(brain_network_paradigm_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(brain_network_paradigm_2026_tests).

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
 * 1. INDIVIDUAL POWERLESS PERSPECTIVE: Added 'Patient with a Brain Injury' as
 *    the individual powerless agent. For them, the brain's network is a 'Mountain'
 *    of unchangeable biological reality.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Patient (Mountain): An immutable biological reality.
 *    - Neuroscientist (Rope): A tool for understanding complex behaviors.
 *    - Modularist (Snare): A threat to established expertise and theories.
 * 
 * 3. CORE INSIGHT: The brain network paradigm shift highlights how scientific
 *    progress can transform our understanding of a 'Mountain' (the brain's
 *    physical structure). What becomes a powerful 'Rope' for new research
 *    simultaneously acts as a 'Snare' for outdated paradigms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the individuality of brain networks.
 */

omega_variable(
    network_individuality,
    "Is every individual's brain network map unique (a 'Snare' for generalized treatments) or does a standard template exist (a 'Rope' for universal protocols)?",
    resolution_mechanism("Longitudinal high-resolution connectivity mapping of diverse populations; personalized medicine trials based on individual connectomes."),
    impact("If unique: Standardized medicine may fail. If template: Universal protocols remain viable."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Strict Modularism (The "Phineas Gage" model)
 *    Viability: Historically dominant in the 19th and 20th centuries, attributing functions to specific brain regions.
 *    Suppression: Actively dismantled by modern fMRI/PET data, which revealed the interconnected nature of brain function, showing that "focusing on single parts can obscure the whole."
 *
 * CONCLUSION:
 * The network paradigm is a 'Rope' that has largely suppressed the older,
 * more limited 'Rope' of strict modularism. This new understanding presents a
 * more accurate 'Mountain' of the brain's complexity, even as it creates a
 * 'Snare' for those invested in the previous model.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/brain_network_paradigm_2026].
 * 2. Multi-perspective: ?- multi_index_report(brain_network_paradigm_2026).
 * 3. Run tests: ?- run_tests(brain_network_paradigm_2026_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */