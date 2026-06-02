% ============================================================================
% CONSTRAINT STORY: heine_borel_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_heine_borel_theorem, []).

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
 *   constraint_id: heine_borel_theorem
 *   human_readable: Heine-Borel Theorem
 *   domain: mathematical/topology
 *
 * SUMMARY:
 *   The Heine-Borel theorem is a foundational result in real analysis,
 *   stating that for Euclidean space R^n, a set is compact if and only if it
 *   is closed and bounded. This is a mathematical truth whose necessity
 *   follows from the definitions of compactness and the topological structure
 *   of Euclidean space. The constraint exhibits zero degrees of freedom
 *   across all observer perspectives: no mathematician contests it, no axiom
 *   system avoiding it remains standard, and no alternative characterization
 *   of compactness in R^n replaces it. The theorem is not a power
 *   arrangement, institutional enforcement, or contingent discovery — it is a
 *   logical consequence that emerges necessarily from the axioms of
 *   mathematics. The modest theater ratio (0.15) reflects that mathematical
 *   communication about the theorem contains some pedagogical scaffolding and
 *   proof presentation, but the core logical content is non-negotiable. The
 *   low extractiveness (0.08) indicates minimal coercion or asymmetric
 *   benefit — the theorem constrains everyone equally who works in Euclidean
 *   topology.
 *
 * KEY AGENTS:
 *   - Working Mathematicians: All users of topology in R^n must accept the theorem as binding; none can dispute or circumvent it
 *   - Mathematical Community: Institutional consensus reinforces the theorem through pedagogy, research norms, and textbook canonicity; but this reinforcement reflects logical necessity, not power
 *   - Axiom System (ZFC): The formal substrate from which the theorem derives as logical consequence; ZFC itself is the ultimate constraint
 *   - Analytical Observer: Sees the theorem as a natural law of topology — not a socially constructed constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(heine_borel_theorem, 0.08).
domain_priors:suppression_score(heine_borel_theorem, 0.02).
domain_priors:theater_ratio(heine_borel_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heine_borel_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(heine_borel_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(heine_borel_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(heine_borel_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(heine_borel_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(heine_borel_theorem, mountain).
narrative_ontology:human_readable(heine_borel_theorem, "Heine-Borel Theorem").
narrative_ontology:topic_domain(heine_borel_theorem, "mathematical/topology").

domain_priors:emerges_naturally(heine_borel_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING MATHEMATICIAN (MOUNTAIN) — Must navigate the theorem as an immutable structural property of topology. Cannot construct counterexample or avoid its constraints. The equivalence between closed-and-bounded and compact in Euclidean spaces is a fixed fact, not negotiable. Zero degrees of freedom.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL COMMUNITY (MOUNTAIN) — All rigorous mathematics accepts the Heine-Borel characterization as foundational. No organized body of mathematicians disputes it or seeks alternatives. The constraint is invariant across pedagogical contexts, proof methods, and research programs. Institutional consensus reflects logical necessity, not power arrangement.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: AXIOM SYSTEM / FORMAL THEORY (MOUNTAIN) — Within ZFC set theory, the Heine-Borel theorem is a logical consequence of the axioms. No institutional decision can override it. The constraint emerges necessarily from the axiom structure. Accessibility collapse is extreme: the theorem cannot be made more accessible without weakening the axioms themselves.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a pure mathematics standpoint, the Heine-Borel theorem represents an irreducible structure of Euclidean topology. No external power relationship, no extraction, no coercion. The theorem is what it is: a logical consequence of the definition of compactness and the structure of R^n.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heine_borel_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(heine_borel_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(heine_borel_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(heine_borel_theorem, ExtMetricName, E),
    domain_priors:suppression_score(heine_borel_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(heine_borel_theorem),
    narrative_ontology:constraint_metric(heine_borel_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(heine_borel_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(heine_borel_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The theorem imposes constraints on mathematical reasoning in Euclidean topology, but these constraints are symmetric — they apply equally to all mathematicians and all proofs. No agent extracts asymmetric benefit. The slight non-zero value (not exactly 0.0) accounts for the logical cost of learning and applying the theorem correctly; this cost is real but universally distributed. Suppression (0.02): Minimal. Mathematicians cannot circumvent the theorem, but this is not coercion — it is the structure of logical necessity. No alternative pathways exist that evade the constraint through power dynamics. Resistance (0.08): Very low. The theorem is not actively resisted because it is universally recognized as logically sound. Accessibility collapse (0.92): Very high. The theorem cannot be made accessible without undermining the logical foundations it rests on. There is no 'simpler version' that preserves the full content — understanding it requires mastery of topology. Emerges naturally (true): The theorem is a logical consequence of foundational axioms, not an enforced social rule.
 *
 * PERSPECTIVAL GAP:
 *   This is a uniform-type constraint (Mountain-only). All six DR perspectives would yield Mountain classification, so only four are presented. The perspectival gap is flat — there is no disagreement on the theorem's validity or necessity. Working mathematicians, the organized mathematical community, the formal axiom system, and the analytical observer all classify the Heine-Borel theorem identically. This uniformity is the signature of a natural law: it is invariant across all observation frames.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this Mountain constraint. The theorem imposes no asymmetric extraction. All agents (mathematicians, communities, axiom systems) experience it identically as a fixed logical structure. The canonical d value for mountains is near 0.0 (universal beneficiary status), but for pure mathematical truths, the concept of 'beneficiary' does not apply — there is no extraction flow. The theorem is a shared constraint, not an institutional power arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   UNIVERSAL MATHEMATICS: The Heine-Borel theorem resolves the mandatrophy by exemplifying a constraint that is purely logical and has zero institutional content. It cannot be mislabeled as extraction (no victims) or coordination (no heterogeneous interests to coordinate). It is simply a true mathematical statement that all perspectives confirm. The theorem is the gold-standard example of a Mountain: invariant across all observable frames, resistant to all empirical alternatives, and emerging necessarily from the axioms of mathematics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalization_outside_euclidean,
    'Does the Heine-Borel characterization extend to non-Euclidean spaces, and if so, under what conditions does it fail?',
    'Examination of the theorem''s proof structure to identify which assumptions are specific to Euclidean topology vs. general topological properties. Analysis of Heine-Borel variants in metric spaces, Banach spaces, and general topological spaces.',
    'If the theorem is fundamentally tied to Euclidean geometry: confirms mountain status in R^n specifically. If it generalizes or fails in structured ways: may indicate the ''mountain'' applies only to a specific domain, with other domains exhibiting different constraint structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalization_outside_euclidean, empirical, 'Generalization of Heine-Borel outside Euclidean space').

omega_variable(
    constructive_computability,
    'In constructive mathematics (without the law of excluded middle), does the Heine-Borel theorem retain its classical form and validity?',
    'Comparison of classical proof with constructive proof of compactness in R^n. Analysis of whether the closed-and-bounded characterization requires classical logic or holds intuitionistically.',
    'If true constructively: confirms universality of the constraint. If false or requires weakening: indicates the theorem''s necessity depends on classical logical axioms, not on topology alone — a subtle dependency that would refine the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructive_computability, conceptual, 'Validity of Heine-Borel in constructive mathematics').

omega_variable(
    finite_precision_approximation,
    'When applied to finite-precision numerical computation, does the Heine-Borel theorem constrain algorithm design, or is it a pure theoretical truth with no computational consequence?',
    'Analysis of how finite-precision arithmetic approximates compactness and closed-and-bounded sets. Examination of numerical algorithms that depend on compactness assumptions and their failure modes under rounding.',
    'If computationally consequential: reveals a hidden extraction mechanism (numerical algorithms must account for the constraint). If purely theoretical: confirms the constraint is a mathematical truth with no extractive institutional dimension in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finite_precision_approximation, empirical, 'Computational consequences of Heine-Borel in finite precision').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(heine_borel_theorem, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hb_tr_t0, heine_borel_theorem, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hb_tr_t100, heine_borel_theorem, theater_ratio, 100, 0.15).
narrative_ontology:measurement(hb_tr_t200, heine_borel_theorem, theater_ratio, 200, 0.18).

% Extraction over time
narrative_ontology:measurement(hb_be_t0, heine_borel_theorem, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(hb_be_t100, heine_borel_theorem, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(hb_be_t200, heine_borel_theorem, base_extractiveness, 200, 0.09).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(heine_borel_theorem, information_standard).
narrative_ontology:affects_constraint(heine_borel_theorem, metric_space_compactness).
narrative_ontology:affects_constraint(heine_borel_theorem, sequential_compactness_equivalence).
narrative_ontology:affects_constraint(heine_borel_theorem, bolzano_weierstrass_theorem).

% DUAL FORMULATION NOTE:
% Heine-Borel is a foundational constraint upstream of many other compactness-related theorems in real analysis. It does not decompose into multiple structural constraints with different ε values — the theorem is logically monolithic. Network links indicate downstream theorems that depend on or generalize Heine-Borel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
