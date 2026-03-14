% ============================================================================
% CONSTRAINT STORY: sequential_compactness_equivalence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sequential_compactness_equivalence, []).

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
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sequential_compactness_equivalence
 *   human_readable: Sequential Compactness Equivalence in Topological Spaces
 *   domain: mathematics/topology
 *
 * SUMMARY:
 *   Sequential compactness equivalence is a mathematical theorem stating that
 *   in metrizable topological spaces, a subset is compact if and only if it
 *   is sequentially compact. This constraint represents a paradigm example of
 *   a Mountain-type constraint: it emerges necessarily from the logical
 *   structure of metric topology, exhibits zero degrees of freedom across all
 *   observables, and cannot be negotiated, redesigned, or escaped by any
 *   agent. Unlike institutional constraints (which depend on enforcement) or
 *   coordination mechanisms (which depend on beneficiary/victim
 *   relationships), sequential compactness equivalence depends only on the
 *   internal consistency of the mathematical system. The constraint has no
 *   beneficiaries or victims — it is a structural fact that applies uniformly
 *   to all agents engaging with metrizable spaces.
 *
 * KEY AGENTS:
 *   - The Topology Student: Agent compelled to accept equivalence as a given; cannot exit or renegotiate
 *   - The Mathematician: Discovers the equivalence as a logical consequence; recognizes it as theorem rather than choice
 *   - The Mathematics Institution: Teaches and uses the equivalence as immutable natural law; no enforcement needed
 *   - The Logical System: The metric topology axioms themselves generate the constraint through internal necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sequential_compactness_equivalence, 0.12).
domain_priors:suppression_score(sequential_compactness_equivalence, 0.03).
domain_priors:theater_ratio(sequential_compactness_equivalence, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sequential_compactness_equivalence, extractiveness, 0.12).
narrative_ontology:constraint_metric(sequential_compactness_equivalence, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(sequential_compactness_equivalence, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sequential_compactness_equivalence, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(sequential_compactness_equivalence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sequential_compactness_equivalence, mountain).
narrative_ontology:human_readable(sequential_compactness_equivalence, "Sequential Compactness Equivalence in Topological Spaces").
narrative_ontology:topic_domain(sequential_compactness_equivalence, "mathematics/topology").

domain_priors:emerges_naturally(sequential_compactness_equivalence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For any student or practitioner working in topological spaces, sequential compactness equivalence appears as an immutable structural fact. In metrizable spaces, the definitions coincide by logical necessity. No exit, no alternative framework, no room for negotiation or redesign.
constraint_indexing:constraint_classification(sequential_compactness_equivalence, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From the analytical/mathematical perspective examining the full logical structure, sequential compactness equivalence in metrizable spaces is a theorem-level constraint: it follows necessarily from the metric topology axioms. The equivalence is not enforced by any agent or institution. It emerges from the internal logical structure of the space itself.
constraint_indexing:constraint_classification(sequential_compactness_equivalence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Mathematics departments and research institutions treat sequential compactness equivalence as immutable natural law. The constraint cannot be changed, negotiated, or worked around — it is simply discovered and taught. No suppression mechanism is needed because the constraint has zero degrees of freedom.
constraint_indexing:constraint_classification(sequential_compactness_equivalence, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sequential_compactness_equivalence_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sequential_compactness_equivalence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sequential_compactness_equivalence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sequential_compactness_equivalence, ExtMetricName, E),
    domain_priors:suppression_score(sequential_compactness_equivalence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sequential_compactness_equivalence),
    narrative_ontology:constraint_metric(sequential_compactness_equivalence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sequential_compactness_equivalence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sequential_compactness_equivalence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Near-zero. The constraint does not extract from any agent. It does not benefit one group at the expense of another. It simply is — a structural feature of metrizable spaces that applies equally to all who work with them. The small non-zero value reflects minimal measurement ambiguity around edge cases and alternative formalizations, but the core constraint is non-extractive. Suppression (0.03): Near-zero. No suppression mechanism is needed because no one would want to violate the equivalence. It is not that agents are prevented from denying it — rather, denial is unintelligible within the mathematical framework. Resistance (0.08): Very low. The constraint faces virtually no resistance because all agents recognize it as necessary. Theater ratio (0.15): Very low. The mathematical proof is direct; the equivalence is established through logical derivation, not through institutional ritual or performative acceptance. The small theater value reflects only the unavoidable pedagogical staging required to teach the result to students unfamiliar with topology.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this constraint — all three perspectives (student, analyst, institution) agree that sequential compactness equivalence is a necessary mathematical truth. The absence of a perspectival gap is itself diagnostic: it confirms that the constraint is not an artifact of power asymmetry, institutional design, or strategic positioning. Every observer, regardless of power level or exit options, perceives the constraint as immutable and equivalent. This uniform classification is the defining signature of a genuine Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to this constraint because there is no extraction flow. The constraint has no beneficiaries and no victims — it is a shared structural necessity that affects all agents identically. The d value would be meaningless here; the χ formula does not apply. The constraint's classification as Mountain depends entirely on its internal logical necessity (ε ≤ 0.25, suppression ≤ 0.05), not on beneficiary/victim relationships or power dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_vs_theorem,
    'Is sequential compactness equivalence a definition (stipulative, changeable) or a theorem (discovered, invariant)?',
    'Historical analysis of mathematical development: did mathematicians choose these definitions for convenience, or did they discover that metrizability forces equivalence?',
    'If stipulative: constraint is institutional convention (Rope or Tangled Rope). If discovered: constraint is mathematical necessity (Mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_vs_theorem, conceptual, 'Whether sequential compactness equivalence is a definition or a discovered theorem').

omega_variable(
    non_metrizable_extension,
    'In non-metrizable spaces where sequential compactness and compactness diverge, does this constitute a single constraint with context-dependent truth value, or two separate constraints?',
    'Check whether ε changes when measured in metrizable vs non-metrizable contexts. If yes: two constraints (decompose per ε-invariance principle). If no: single constraint with stable ε.',
    'If two constraints: write separate stories for metrizable and non-metrizable cases. If one: this story is complete as-is.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_metrizable_extension, conceptual, 'Whether the equivalence breaks in non-metrizable spaces indicates constraint decomposition').

omega_variable(
    accessibility_of_proof,
    'Does the mathematical proof of equivalence require specialized knowledge (graduate topology) that could make the constraint appear mysterious or contingent to non-experts?',
    'Empirical: survey mathematics students at different levels; assess what percentage perceive the equivalence as ''just how it works'' vs ''logically necessary''.',
    'If high opacity: the constraint''s accessibility_collapse may be overestimated; the practical constraint experienced by undergraduates might be Piton (theater) rather than Mountain (necessity). If low opacity: accessibility_collapse confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_of_proof, empirical, 'Whether proof accessibility affects perceived necessity of the equivalence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sequential_compactness_equivalence, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seq_comp_tr_t0, sequential_compactness_equivalence, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seq_comp_tr_t1000, sequential_compactness_equivalence, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(seq_comp_tr_t2000, sequential_compactness_equivalence, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(seq_comp_be_t0, sequential_compactness_equivalence, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(seq_comp_be_t1000, sequential_compactness_equivalence, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(seq_comp_be_t2000, sequential_compactness_equivalence, base_extractiveness, 2000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sequential_compactness_equivalence, information_standard).
narrative_ontology:affects_constraint(sequential_compactness_equivalence, compactness_in_function_spaces).
narrative_ontology:affects_constraint(sequential_compactness_equivalence, completeness_in_metric_spaces).
narrative_ontology:affects_constraint(sequential_compactness_equivalence, bolzano_weierstrass_theorem).

% DUAL FORMULATION NOTE:
% Sequential compactness equivalence is upstream of several applied constraints in functional analysis and topology. Claims about compactness of function spaces, completeness properties, and the Bolzano-Weierstrass theorem all depend on the logical structure that produces this equivalence in metrizable contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
