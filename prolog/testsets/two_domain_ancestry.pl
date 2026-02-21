% ============================================================================
% CONSTRAINT STORY: two_domain_ancestry
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-01
% ============================================================================

:- module(constraint_two_domain_ancestry, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: two_domain_ancestry
 *   human_readable: The Two-Domain Tree of Life
 *   domain: technological (genomics), scientific
 *
 * SUMMARY:
 *   Based on comprehensive genomic analysis of Asgard archaea, this constraint
 *   represents the discovery that eukaryotes (all complex life) did not
 *   emerge as a third, distinct domain of life, but rather evolved from
 *   within the archaeal domain. This establishes the "two-domain tree"
 *   (Bacteria and Archaea) as the fundamental model of life's history,
 *   invalidating the long-standing "three-domain" model. The constraint is
 *   the structure of reality itself, as revealed by new technology.
 *
 * KEY AGENTS (by structural relationship):
 *   - Evolutionary Biologists: (organized/mobile) — All researchers in the field are subject to this finding; it forms a new foundation for their work. They are neither beneficiaries nor victims, but observers of a natural law.
 *   - Proponents of the Three-Domain Model: (institutional/constrained) — Their theoretical framework is rendered obsolete by the evidence. They are "constrained" by the new data, which represents a natural limit.
 *   - Analytical Observer: (analytical/analytical) — Sees the constraint as a fundamental, unchangeable fact of natural history, a classic Mountain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(two_domain_ancestry, 0.05).
domain_priors:suppression_score(two_domain_ancestry, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(two_domain_ancestry, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(two_domain_ancestry, extractiveness, 0.05).
narrative_ontology:constraint_metric(two_domain_ancestry, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(two_domain_ancestry, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(two_domain_ancestry, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(two_domain_ancestry, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(two_domain_ancestry, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This constraint represents a fact about natural evolutionary history.
% It is not human-designed or enforced. Required for the mountain metric gate.
domain_priors:emerges_naturally(two_domain_ancestry).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain constraint representing a natural law, this constraint does not
% have beneficiaries or victims in the standard sense. All agents are subject
% to it equally. The classification is invariant across all perspectives.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% UNIFORM-TYPE CONSTRAINT (MOUNTAIN-ONLY):
% A natural law constraint classifies as Mountain from all perspectives.
% The index (P,T,E,S) does not alter the classification because the base
% extractiveness (ε) and suppression are below the required thresholds.
% We include multiple perspectives to demonstrate this invariance.

% PERSPECTIVE 1: THE FIELD RESEARCHER
% A working biologist whose research must now conform to this new model.
constraint_indexing:constraint_classification(two_domain_ancestry, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: THE INSTITUTION
% A university or funding body that must update its curriculum and research
% priorities.
constraint_indexing:constraint_classification(two_domain_ancestry, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context, which sees the constraint as a fundamental
% property of the system being studied.
constraint_indexing:constraint_classification(two_domain_ancestry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(two_domain_ancestry_tests).

test(classification_invariance, [forall(member(Power, [organized, institutional, analytical]))]) :-
    % Verify the constraint is a Mountain from all key perspectives.
    constraint_indexing:constraint_classification(two_domain_ancestry, mountain, context(agent_power(Power), _, _, _)).

test(natural_law_profile_adherence) :-
    % Verify the metrics meet the stringent requirements for a Mountain/Natural Law.
    domain_priors:base_extractiveness(two_domain_ancestry, E),
    E =< 0.25,
    domain_priors:suppression_score(two_domain_ancestry, S),
    S =< 0.05,
    narrative_ontology:constraint_metric(two_domain_ancestry, accessibility_collapse, AC),
    AC >= 0.85,
    narrative_ontology:constraint_metric(two_domain_ancestry, resistance, R),
    R =< 0.15,
    domain_priors:emerges_naturally(two_domain_ancestry).

:- end_tests(two_domain_ancestry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as a Mountain because it represents a
 *   fundamental, verifiable fact about the natural world, revealed by new
 *   genomic sequencing technology. Its base extractiveness (ε=0.05) and
 *   suppression (0.02) are minimal, far below the Mountain thresholds. It does
 *   not extract value; it describes reality. It does not suppress alternatives
 *   through coercion but invalidates them through evidence.
 *
 *   The Natural Law Profile metrics are key:
 *   - `emerges_naturally` is true, as this is a finding about evolutionary history.
 *   - `accessibility_collapse` (0.95) is high because the comprehensive genomic
 *     data makes alternative models of eukaryotic origin scientifically inaccessible.
 *   - `resistance` (0.10) is low. While individual scientists may cling to the
 *     old model, the field as a whole faces no coherent way to resist the
 *     evidence; doing so would be unscientific.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a Mountain-type constraint representing a
 *   natural law, its classification is invariant across all possible indices.
 *   An evolutionary biologist, a university, and an analytical observer all
 *   perceive the same fundamental limit on valid theories of life's origin.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality logic (beneficiary/victim) does not apply to Mountain
 *   constraints. The concept of "benefiting" from or being a "victim" of a
 *   natural law is incoherent. All agents are equally subject to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a limit imposed by nature, not
 *   by human institutions. It avoids mislabeling a scientific discovery as a
 *   coordinating `Rope` (which would imply it's a convention) or a `Snare`
 *   (which would imply it's an artificial, extractive barrier). The ε-invariance
 *   principle is crucial here; the old "three-domain model" was a different
 *   constraint (a `Piton`), supported by a different set of observations (a
 *   different ε). This story correctly isolates the new finding as its own
 *   constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_two_domain_ancestry,
    'Is the Asgard archaea lineage the final, definitive origin point for eukaryotes, or simply the closest relative found to date?',
    'Further deep-environmental sequencing and discovery of new, more divergent archaeal lineages. Resolution requires more empirical data.',
    'If a closer, undiscovered relative exists, this Mountain remains but its specific details would be refined. If a completely different origin is found (highly unlikely), this constraint would be invalidated and replaced.',
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_two_domain_ancestry, empirical,
    'Whether an even closer archaeal relative to eukaryotes exists, pending further deep-sea genomic sampling.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(two_domain_ancestry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for this constraint, as its base_extractiveness
% (0.05) is below the threshold (ε > 0.46) for mandatory drift tracking. As a
% Mountain, its properties are considered stable over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint.

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from the colloquial concept
% of the "Tree of Life model". Decomposed because ε differs across observables
% (the available genomic data over time), per the ε-invariance principle.
% This Mountain (the new model) makes the old model obsolete.
%
% Related stories:
%   - three_domain_hypothesis (ε≈0.15, now a Piton) - The previous scientific consensus.
%
% The discovery of this `two_domain_ancestry` (a Mountain) is the causal factor
% that turns the `three_domain_hypothesis` (a former Rope) into a `Piton`.
narrative_ontology:affects_constraint(two_domain_ancestry, three_domain_hypothesis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No directionality overrides are needed. This is a Mountain constraint,
% for which the beneficiary/victim framework and directionality scaling
% do not apply. The classification is invariant.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */