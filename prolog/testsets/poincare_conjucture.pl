% ============================================================================
% CONSTRAINT STORY: poincare_conjecture
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Poincaré Conjecture / Perelman's Proof (Ricci Flow with Surgery)
% ============================================================================

:- module(constraint_poincare_conjecture, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: poincare_conjecture
 * human_readable: The Poincaré Conjecture
 * domain: mathematical/topological
 * temporal_scope: 1904 - Present (Civilizational)
 * spatial_scope: Global/Abstract (3-Manifolds)
 * * SUMMARY:
 * The Poincaré Conjecture characterizes the 3-sphere (the hypersphere that 
 * bounds the unit ball in 4D space). It asserts that every simply connected, 
 * closed 3-manifold is homeomorphic to the 3-sphere. Proven by Grigori 
 * Perelman in 2003, it serves as the foundational classification of "simple" 
 * 3D space.
 * * KEY AGENTS:
 * - The 3-Manifold: The powerless subject whose identity is fixed by its 
 * topological properties (simply connected, closed).
 * - Grigori Perelman: The analytical analyst who used Ricci Flow to navigate 
 * the geometry toward the topological truth.
 * - The Mathematical Establishment (Clay Institute): The institutional agent 
 * offering prizes and validation, which Perelman famously rejected.
 * * NARRATIVE ARC:
 * For a century, the conjecture was a "Mountain" of unproven but suspected 
 * natural law. Perelman converted it into a "Rope" by providing a constructive 
 * path via Ricci Flow with surgery. However, the social "Snare" of the 
 * Millennium Prize and Fields Medal caused Perelman to exit the system 
 * entirely, viewing the professional validation mechanism as extractive.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(poincare_era, 1904, 2026).
narrative_ontology:constraint_claim(poincare_conjecture, mountain).

% Base extractiveness: 0.2
% Rationale: Mathematical truth itself is non-extractive, but the social 
% apparatus of "Prizes" (Millennium Prize) extracts prestige and personal 
% peace from the mathematician.
domain_priors:base_extractiveness(poincare_conjecture, 0.2).

% Suppression score: 0.1
% Rationale: It defines what is possible in 3D topology, rendering "fake" 
% 3-spheres impossible, but it does not hide other mathematical theories.
domain_priors:suppression_score(poincare_conjecture, 0.1).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(poincare_conjecture, extractiveness, 0.2).
narrative_ontology:constraint_metric(poincare_conjecture, suppression_requirement, 0.1).

% Enforcement: Emerges from the fundamental axioms of topology.
domain_priors:emerges_naturally(poincare_conjecture).

% Metrics
% Beneficiaries & Victims
constraint_beneficiary(poincare_conjecture, topological_physics).
constraint_beneficiary(poincare_conjecture, geometrical_analysts).
constraint_victim(poincare_conjecture, perelman_social_peace). % Social pressure of the prize.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE 3-MANIFOLD (THE SUBJECT) - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The manifold cannot change its topology.
   WHEN: immediate - True throughout its existence.
   WHERE: trapped - Bound by its own connectivity and closure.
   SCOPE: local - Immediate neighborhood of its points.
   
   WHY THIS CLASSIFICATION:
   If a 3D space is simply connected and closed, it *is* a 3-sphere. This is 
   not a choice; it is an unyielding Mountain of topological fate.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    poincare_conjecture,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE RESEARCH MATHEMATICIAN - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to use the theorem to classify new systems.
   WHEN: civilizational - Long-term development of geometry.
   WHERE: mobile - Can apply the theorem to various physical models.
   SCOPE: global - Universal application.
   
   WHY THIS CLASSIFICATION:
   For the working mathematician, the proven conjecture is a "Rope"—a tool 
   of coordination. It allows them to "anchor" their work in a known reality, 
   ensuring that their classification of 3D spaces is robust.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    poincare_conjecture,
    rope,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PERELMAN (THE MILLENNIUM PRIZE) - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Perelman, despite his genius, felt subject 
        to a corrupt social order of prizes.
   WHEN: biographical - The decision to reject the prize defined his life.
   WHERE: constrained - Surrounded by media and social pressure.
   SCOPE: local - His personal interaction with the mathematical community.
   
   WHY THIS CLASSIFICATION:
   The prize was a "Snare." To accept it was to validate a social system of 
   prestige he found "completely irrelevant." The prize attempted to extract 
   his compliance with professional norms in exchange for money.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    poincare_conjecture,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(poincare_conjecture, E),
    E > 0.15. % The extraction is social, not mathematical.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(poincare_conjecture_tests).

test(topology_fate_variance) :-
    % Manifold -> Mountain
    constraint_indexing:constraint_classification(poincare_conjecture, Type1, context(powerless, immediate, trapped, local)),
    % Community -> Rope
    constraint_indexing:constraint_classification(poincare_conjecture, Type2, context(institutional, civilizational, mobile, global)),
    Type1 = mountain,
    Type2 = rope.

test(perelman_exit_logic) :-
    % Perelman's rejection of the prize represents a constrained individual seeing a Snare.
    constraint_indexing:constraint_classification(poincare_conjecture, snare, context(powerless, biographical, constrained, local)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(poincare_conjecture).

:- end_tests(poincare_conjecture_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. PERSPECTIVE SPLIT: 
 * I chose to split the "Mathematical Truth" (Mountain) from the 
 * "Social Validation" (Snare). This is essential to explain why 
 * the discoverer of a "gift to humanity" (Rope) would feel strangled 
 * by it.
 * * 2. EXTRACTIVENESS: 
 * Set at 0.2 because math is pure, but the *discovery process* in 
 * a professional setting is extractive of the discoverer's peace.
 * * 3. AGENT POWER: 
 * Labeled Perelman as `powerless` in the social context, 
 * reflecting his inability to stop the world from awarding him 
 * things he didn't want.
 */

% OMEGA IDENTIFICATION
omega_variable(
    professional_recognition_toxicity,
    "Does the 'Snare' of professional recognition prevent future geniuses from sharing proofs?",
    resolution_mechanism("Monitor the frequency of anonymous vs credited publications of 'unsolvable' problems."),
    impact("If Yes: The social order is a Snare. If No: It is a functional Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Thurston's Geometrization Conjecture
 * Viability: The broader framework that Poincare fits into.
 * Suppression: None; Perelman's proof of Geometrization included Poincare.
 * * ALTERNATIVE 2: Anonymous Publication (arXiv)
 * Viability: How Perelman actually released the work.
 * Suppression: The community attempted to force him back into the 
 * "peer review/prize" Rope, which he experienced as a Snare.
 * * CONCLUSION:
 * Perelman's use of arXiv (Alternative 2) was a "mobile" exit option that 
 * temporarily turned the Snare back into a Rope, but the community's 
 * reaction tried to tighten it again.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [poincare_conjecture].
% Analyze: ?- constraint_indexing:multi_index_report(poincare_conjecture).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(poincare_conjucture, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(poincare_conjucture, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(poincare_conjucture, snare, agent_power(powerless)).

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
% Formal truth — substantive with near-zero performative component
domain_priors:theater_ratio(poincare_conjecture, 0.03).
narrative_ontology:constraint_metric(poincare_conjecture, theater_ratio, 0.03).
