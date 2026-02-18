% ============================================================================
% CONSTRAINT STORY: russells_paradox_self_reference
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_russells_paradox_self_reference, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: russells_paradox_self_reference
 * human_readable: Russell's Paradox (Naive Set Theory Collapse)
 * domain: mathematical/logical
 * * SUMMARY:
 * Russell's Paradox arises from considering the set of all sets that do not contain themselves.
 * The paradox is that such a set must contain itself if and only if it does not, a logical contradiction.
 * This discovery invalidated the intuitive foundations of naive set theory (specifically Frege's Basic Law V) and forced the development of more rigorous axiomatic systems like ZFC and Type Theory to avoid such self-references.
 * * KEY AGENTS:
 * - The Naive Set Theorist (e.g., Gottlob Frege): Subject (Powerless/Moderate) whose foundational work is invalidated.
 * - The Axiomatic Architect (e.g., Zermelo, Russell): Beneficiary (Institutional) who uses the paradox to coordinate the development of new, consistent foundations.
 * - The Analytical Logician: Auditor (Analytical) observing the structural properties of formal systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: 0.7. The paradox is highly extractive. It "takes" the consistency of any naive system, forcing a massive reinvestment in complex, less intuitive axioms to regain logical "solvency." The cost of avoiding the paradox is the loss of simplicity.
domain_priors:base_extractiveness(russells_paradox_self_reference, 0.7).
% Rationale: 0.4. It suppresses "unrestricted comprehension," the idea that any property can define a set. This intuitive approach is rendered unusable in formal logic.
domain_priors:suppression_score(russells_paradox_self_reference, 0.4).
% Rationale: 0.0. The paradox is a pure, functional, logical constraint with no performative or theatrical component.
domain_priors:theater_ratio(russells_paradox_self_reference, 0.0).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(russells_paradox_self_reference, extractiveness, 0.7).
narrative_ontology:constraint_metric(russells_paradox_self_reference, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(russells_paradox_self_reference, theater_ratio, 0.0).

% Constraint self-claim (what does the constraint claim to be?)
% The paradox doesn't "claim" anything, but its discovery reveals a fundamental, unchangeable feature of logic.
narrative_ontology:constraint_claim(russells_paradox_self_reference, snare).
narrative_ontology:human_readable(russells_paradox_self_reference, "Russell's Paradox (Naive Set Theory Collapse)").
narrative_ontology:topic_domain(russells_paradox_self_reference, "mathematical/logical").

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(russells_paradox_self_reference, axiomatic_set_theorists).
narrative_ontology:constraint_victim(russells_paradox_self_reference, naive_set_theory_proponents).
narrative_ontology:constraint_victim(russells_paradox_self_reference, gottlob_frege).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The naive set theorist (like Frege) whose system is destroyed by the paradox.
% χ = 0.7 (ε) * 1.5 (powerless) * 1.2 (global) = 1.26. This is a definitive Snare.
constraint_indexing:constraint_classification(russells_paradox_self_reference, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The axiomatic architect who uses the paradox to justify and coordinate the adoption of new, more robust foundations for mathematics.
% χ = 0.7 (ε) * -0.2 (institutional) * 1.2 (global) = -0.168. This is a pure coordination tool with negative perceived extraction.
constraint_indexing:constraint_classification(russells_paradox_self_reference, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The default analytical context. The analysis reveals a high-extraction constraint that traps naive systems, classifying it as a Snare.
% χ = 0.7 (ε) * 1.15 (analytical) * 1.2 (global) = 0.966.
constraint_indexing:constraint_classification(russells_paradox_self_reference, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russells_paradox_self_reference_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the powerless victim and the institutional beneficiary.
    constraint_indexing:constraint_classification(russells_paradox_self_reference, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russells_paradox_self_reference, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify the base extractiveness is high enough to trigger Snare/Tangled Rope classifications.
    narrative_ontology:constraint_metric(russells_paradox_self_reference, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(russells_paradox_self_reference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original file incorrectly classified the paradox as a Mountain from the powerless perspective.
 * A Mountain must have base extraction <= 0.15. With an extraction of 0.7, the paradox is fundamentally extractive—it destroys the value (consistency) of naive systems.
 * Therefore, for any agent trapped by it (like Frege or a naive set), it functions as a Snare.
 * The perspectival gap is stark: for those whose work is invalidated, it's a destructive trap (Snare). For those building the next generation of logic, it's an essential coordination point that justifies their work and rallies the community (Rope).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint demonstrates how a single phenomenon can be both a Snare and a Rope.
 * Mandatrophy would occur if the system only saw the Snare aspect (the destruction of Frege's system) and missed the massive coordination benefit (the creation of ZFC).
 * By indexing the classification, the system correctly identifies that for the beneficiaries (the community of mathematicians), the paradox functions as a Rope, coordinating the move to safer foundations. This prevents the system from mislabeling a foundational discovery as purely destructive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_russells_paradox_self_reference,
    "Is there a unique 'True' universe of sets (a Mountain), or is mathematics a pluralism of consistent but arbitrary axiomatic systems (a collection of Scaffolds)?",
    "Further investigation into whether large cardinal axioms eventually settle undecidable statements like the Continuum Hypothesis.",
    "If a true Mountain exists, logic is discovered. If only Scaffolds exist, logic is invented and coordinated.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(russells_paradox_self_reference, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for a timeless logical paradox is static. The paradox's properties do not change.
% This is required because base_extractiveness > 0.46.

% Theater ratio over time (is always zero):
narrative_ontology:measurement(russells_paradox_self_reference_tr_t0, russells_paradox_self_reference, theater_ratio, 0, 0.0).
narrative_ontology:measurement(russells_paradox_self_reference_tr_t5, russells_paradox_self_reference, theater_ratio, 5, 0.0).
narrative_ontology:measurement(russells_paradox_self_reference_tr_t10, russells_paradox_self_reference, theater_ratio, 10, 0.0).

% Extraction over time (is always constant):
narrative_ontology:measurement(russells_paradox_self_reference_ex_t0, russells_paradox_self_reference, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(russells_paradox_self_reference_ex_t5, russells_paradox_self_reference, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(russells_paradox_self_reference_ex_t10, russells_paradox_self_reference, base_extractiveness, 10, 0.7).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The paradox functions as a coordination tool by enforcing a standard of consistency.
narrative_ontology:coordination_type(russells_paradox_self_reference, enforcement_mechanism).

% Network relationships: Russell's Paradox is part of the "foundational crisis" in early 20th-century mathematics, which is structurally linked to Gödel's later work.
narrative_ontology:affects_constraint(russells_paradox_self_reference, goedels_incompleteness).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */