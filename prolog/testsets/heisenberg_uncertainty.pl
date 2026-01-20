% ============================================================================
% CONSTRAINT STORY: heisenberg_uncertainty
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Quantum Mechanics / Werner Heisenberg (1927)
% ============================================================================

:- module(constraint_heisenberg_uncertainty, []).

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
 * * constraint_id: heisenberg_uncertainty
 * human_readable: Heisenberg Uncertainty Principle
 * domain: technological
 * temporal_scope: Permanent (Universal Laws of Physics)
 * spatial_scope: Global (Quantum Scale)
 * * SUMMARY:
 * The Heisenberg Uncertainty Principle states that it is fundamentally impossible 
 * to simultaneously know the exact position and momentum of a particle. 
 * This is not a limit of measurement technology, but a structural property 
 * of quantum systems where the more precisely one property is measured, 
 * the less precisely the other can be known.
 * * KEY AGENTS:
 * - The Quantum Physicist: Analytical observer mapping the fundamental limits of the universe.
 * - The Security Architect: Institutional agent using the principle to ensure data integrity.
 * - The Nanotechnology Engineer: Individual agent struggling against the "noise" and limits of microscopic precision.
 * * NARRATIVE ARC:
 * The principle acts as a "Mountain" of physical reality. While it provides 
 * a "Rope" for coordination in quantum cryptography (by guaranteeing that 
 * eavesdropping can be detected), it functions as a "Noose" for those 
 * attempting classical-style absolute precision at the subatomic level.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(heisenberg_interval, 0, 10).
narrative_ontology:constraint_claim(heisenberg_uncertainty, mountain).

% Base extractiveness score
% Rationale: It extracts "absolute precision" from the observer. 
% It imposes a fundamental "uncertainty tax" on information.
domain_priors:base_extractiveness(heisenberg_uncertainty, 0.2).

% Suppression score
% Rationale: It suppresses the "Classical Realist" alternative where particles 
% have definite trajectories.
domain_priors:suppression_score(heisenberg_uncertainty, 0.4).

% Enforcement requirements
domain_priors:emerges_naturally(heisenberg_uncertainty).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(heisenberg_uncertainty, extractiveness, 0.2).
narrative_ontology:constraint_metric(heisenberg_uncertainty, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(heisenberg_uncertainty, quantum_cryptographers).
constraint_victim(heisenberg_uncertainty, classical_determinists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE THEORETICAL PHYSICIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal informational laws.
   WHEN: civilizational - Viewing the universe as a permanent substrate.
   WHERE: trapped - Logic and physics cannot bypass the commutation relations.
   SCOPE: global - Universal applicability.
   
   WHY THIS CLASSIFICATION:
   To the physicist, the principle is a Mountain. It is an unchangeable 
   consequence of the wave-like nature of matter. No amount of 
   "better measuring" changes this peak in the landscape of reality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    heisenberg_uncertainty,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE QUANTUM KEY DISTRIBUTOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define and utilize formal rules for trust.
   WHEN: biographical - Achieving secure communication over a project lifetime.
   WHERE: arbitrage - Can use uncertainty to detect perturbations by an eavesdropper.
   SCOPE: national - Infrastructure-level security.
   
   WHY THIS CLASSIFICATION:
   For the security architect, the principle is a Rope. It is a 
   coordination mechanism that provides a physical guarantee of privacy. 
   Uncertainty is used as a tether to ensure that "unobserved" data remains 
   truly unobserved.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    heisenberg_uncertainty,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CLASSICAL HARDWARE DESIGNER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the "noise" of the quantum floor.
   WHEN: immediate - Today's struggle to eliminate measurement variance.
   WHERE: constrained - Cannot leave the quantum domain while working at the nano-scale.
   SCOPE: local - Immediate laboratory environment.
   
   WHY THIS CLASSIFICATION:
   For the engineer trying to build a perfectly deterministic nano-switch, 
   the principle is a Noose. The harder they try to fix both 
   position and momentum, the tighter the "fuzziness" strangles their 
   precision, eventually leading to terminal heat or tunneling errors.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    heisenberg_uncertainty,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(heisenberg_uncertainty_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(heisenberg_uncertainty, mountain, context(analytical, civilizational, trapped, global)),
    % Institutional sees Rope
    constraint_indexing:constraint_classification(heisenberg_uncertainty, rope, context(institutional, biographical, arbitrage, national)),
    % Powerless sees Noose
    constraint_indexing:constraint_classification(heisenberg_uncertainty, noose, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_precision) :-
    % Powerless engineers feel the total extraction of precision (Noose).
    % Institutional cryptographers derive utility from the limit (Rope).
    domain_priors:base_extractiveness(heisenberg_uncertainty, Score),
    Score > 0.1.

test(time_immutability_physics) :-
    % Long-term civilizational logic = Mountain.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(heisenberg_uncertainty_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.2):
 * Reasoning: Low extraction because it is a neutral physical law. 
 * It extracts "certainty" but does not discriminate based on class, 
 * though the impact is asymmetric based on agent power.
 * * 2. PERSPECTIVE SELECTION:
 * The Analyst (Physicist) sees the law as a fixed Mountain.
 * The Architect (Cryptographer) sees the law as a Rope for security.
 * The Worker (Engineer) sees the law as a Noose strangling their precision.
 * * 3. AMBIGUITIES:
 * - The transition from quantum uncertainty to classical determinism (decoherence) 
 * creates a boundary where the Noose might be loosened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_vs_ontic_nature,
    "Is the uncertainty fundamentally real (Mountain) or just a limit 
    of our current informational framework (Rope)?",
    resolution_mechanism("Development of a grand unified theory or resolution 
    of the measurement problem"),
    impact("If Ontic: Absolute Mountain. If Epistemic: A Rope we can eventually untie."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Classical Mechanics
 * Viability: High at macro-scales, where uncertainty is negligible.
 * Suppression: Extreme at the quantum level; the laws of physics 
 * actively punish classical assumptions through contradictory data.
 * * CONCLUSION:
 * The existence of classical mechanics as a suppressed alternative at the 
 * quantum scale confirms that for the nanotechnologist, the principle 
 * acts as a Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [constraint_heisenberg_uncertainty].
 * 2. Multi-perspective: ?- multi_index_report(heisenberg_uncertainty).
 * 3. Run tests: ?- run_tests(heisenberg_uncertainty_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
