% ============================================================================
% CONSTRAINT STORY: finite_simple_groups_classification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finite_simple_groups_classification, []).

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
 *   constraint_id: finite_simple_groups_classification
 *   human_readable: The Classification of Finite Simple Groups (CFSG)
 *   domain: mathematical/abstract_algebra
 *
 * SUMMARY:
 *   The Classification of Finite Simple Groups (CFSG) is a landmark theorem
 *   in abstract algebra completed over the 20th century, culminating in full
 *   proof by approximately 2004 (with the completion of the second generation
 *   proof). The theorem states that every finite simple group belongs to one
 *   of 18 infinite families (cyclic groups of prime order, alternating
 *   groups, groups of Lie type, and a few special families), is one of 26
 *   sporadic groups, or is an alternating group. The CFSG is one of the most
 *   significant theorems in pure mathematics, often cited as the longest
 *   proof ever assembled (distributed across more than 10,000 pages and
 *   hundreds of papers). This constraint exemplifies a mountain
 *   classification: the structure of finite simple groups is logically
 *   invariant, independent of institutional arrangements, funding, or
 *   observer perspective. No mathematician can 'opt out' of the constraint —
 *   it is not a convention but an immutable structural fact about finite
 *   groups.
 *
 * KEY AGENTS:
 *   - Pure Mathematics Community: Analytical community (analytical/analytical) — recognizes CFSG as a completed logical structure; the classification is irreducible and eternal
 *   - Group Theory Researchers: Moderate researchers (moderate/analytical) — constrained by CFSG but benefit from its completeness; no extraction occurs because no agent benefits asymmetrically
 *   - Mathematical Logic: Foundational discipline (analytical/analytical) — verifies the logical consistency and completeness of the proof; no beneficiary or victim
 *   - Applied Fields (Cryptography, Coding Theory, Physics): Moderate users (moderate/analytical) — may use CFSG results but are not extractively constrained; the theorem is a resource, not a trap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finite_simple_groups_classification, 0.12).
domain_priors:suppression_score(finite_simple_groups_classification, 0.03).
domain_priors:theater_ratio(finite_simple_groups_classification, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finite_simple_groups_classification, extractiveness, 0.12).
narrative_ontology:constraint_metric(finite_simple_groups_classification, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(finite_simple_groups_classification, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(finite_simple_groups_classification, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(finite_simple_groups_classification, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finite_simple_groups_classification, mountain).
narrative_ontology:human_readable(finite_simple_groups_classification, "The Classification of Finite Simple Groups (CFSG)").
narrative_ontology:topic_domain(finite_simple_groups_classification, "mathematical/abstract_algebra").

domain_priors:emerges_naturally(finite_simple_groups_classification).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: APPLIED MATHEMATICIAN (MOUNTAIN) — The CFSG is an immutable structural fact about abstract group theory. Whether this theorem is used in applications or ignored, the theorem holds independently. No exit option exists; the classification is not contingent on institutional arrangements or funding priorities. The applied mathematician cannot 'opt out' of the theorem's existence — it constrains what is logically possible in finite group theory.
constraint_indexing:constraint_classification(finite_simple_groups_classification, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: GROUP THEORIST (MOUNTAIN) — The CFSG is an immutable logical structure. A group theorist researching finite groups must accept the classification as a constraint on what finite simple groups can exist. They may choose to work in other areas of algebra, but they cannot 'disagree' with the CFSG — it is not subject to scientific contestation or institutional power. The constraint is irreducible.
constraint_indexing:constraint_classification(finite_simple_groups_classification, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: PURE MATHEMATICIAN / ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of mathematical logic and structure, the CFSG is a completed classification of a well-defined mathematical object. Every finite simple group either falls into one of the 18 families, is one of 26 sporadic groups, or is an alternating group. This is not a probabilistic claim, not a conjecture, not a matter of interpretation. It is a theorem with a complete proof. The constraint is eternal, universal, and carries zero degrees of freedom for all indices.
constraint_indexing:constraint_classification(finite_simple_groups_classification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finite_simple_groups_classification_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(finite_simple_groups_classification, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finite_simple_groups_classification, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(finite_simple_groups_classification, ExtMetricName, E),
    domain_priors:suppression_score(finite_simple_groups_classification, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(finite_simple_groups_classification),
    narrative_ontology:constraint_metric(finite_simple_groups_classification, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(finite_simple_groups_classification, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(finite_simple_groups_classification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Extremely low. The CFSG is a pure mathematical fact with no asymmetric extraction. No agent benefits at the expense of another. The classification is a public mathematical good — all mathematicians have equal access to the theorem and its consequences. The minimal value (0.12 rather than 0.00) reflects minor institutional costs in learning and verifying the proof, but these are not extraction costs; they are standard knowledge-acquisition costs in mathematics. Suppression (0.03): Negligible. There are no coercive barriers to studying or using the CFSG. Alternative approaches to group theory (representation theory, homological algebra) are available and freely studied. Theater ratio (0.15): Very low. The CFSG proof is highly technical and substantive; performative content is minimal. The proof verification process is mathematically rigorous, not theatrical. The slight non-zero value reflects standard mathematical exposition overhead — the proof must be written down, taught in seminars, and communicated — but this overhead is necessary for knowledge transmission, not performative.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in the CFSG. All perspectives — applied, theoretical, analytical, institutional — classify the constraint as Mountain. This is a rare property, characteristic of true natural laws in mathematics. The invariance across all perspectives is precisely the signature of an irreducible mathematical structure. The CFSG does not appear as Rope or Snare from any legitimate mathematical vantage point because the classification is not a coordination mechanism requiring enforcement, and it is not extractive. The lack of perspectival gap is evidence of the theorem's fundamental status.
 *
 * DIRECTIONALITY LOGIC:
 *   The CFSG exhibits zero directionality variation across all perspectives because no extraction occurs. All agents — applied mathematicians, group theorists, logicians, engineers using group-theoretic methods — experience the same d value: d = 0.5 (symmetric), because no one benefits asymmetrically and no one is a trapped victim. The constraint is a structural fact of mathematics, not a power relationship. The sigmoid f(d) yields f(0.5) = 0.65 baseline, but this is academic knowledge-acquisition effort, not extraction. The scope modifier σ(S) = 1.0 (universal), confirming that the CFSG holds in all mathematical contexts. The effective extractiveness χ = ε × f(d) × σ(S) = 0.12 × 0.65 × 1.0 ≈ 0.08, confirming mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The CFSG requires no mandatrophy resolution because it exhibits zero extraction across all perspectives and all time periods. The constraint is not subject to mandatrophy — the degeneration of coordination function into pure extraction — because it has no coordination function to degenerate. It is purely a mathematical fact. The theorem does not maintain itself through institutional theater or suppression; it maintains itself because it is logically true. Unlike institutional constraints (scaffolds, snares, pitons) that can degrade over time, mathematical theorems do not degrade. Once proven, the CFSG is eternally true, regardless of how many mathematicians study it or how institutional interest waxes and wanes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_completeness,
    'Is the proof of the CFSG truly complete, or does it rely on unresolved sub-theorems or computational verification that could fail under scrutiny?',
    'Review of the complete proof (20,000+ pages across hundreds of papers) and identification of any gaps or circular dependencies. Verification of computer-assisted proofs in the classification (e.g., proof of uniqueness for certain sporadic groups).',
    'If the proof is incomplete: CFSG drops from Mountain to Tangled Rope or Piton (institutional maintenance of an incomplete claim). If the proof is complete and verified: CFSG remains Mountain, confirming irreducible mathematical truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_completeness, empirical, 'Whether the CFSG proof is logically and computationally complete').

omega_variable(
    quasisimple_group_boundary,
    'Does the classification extend cleanly to quasisimple groups (covering groups of simple groups), or does the boundary between simple and quasisimple introduce classificatory ambiguity?',
    'Examination of the relationship between simple and quasisimple group classifications; determination of whether all quasisimple extensions of finite simple groups are enumerated or whether new quasisimple groups could exist outside the classification.',
    'If the boundary is clean: CFSG scope is well-defined (Mountain). If the boundary is ambiguous: CFSG may describe a proper subset of a larger natural class (Mountain becomes Rope with definitional tension).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quasisimple_group_boundary, conceptual, 'Whether the simple/quasisimple boundary is natural or conventional').

omega_variable(
    infinitary_extension,
    'Does the CFSG structure extend to infinite simple groups, or is the finiteness restriction fundamental to the classification?',
    'Investigation of whether infinite simple groups exhibit analogous family structures (Lie groups, pro-finite groups) and whether their classification would follow from or relate to the CFSG.',
    'If extension is possible and analogous: CFSG is an instance of a deeper principle (reduces to Mountain). If extension fails: finiteness is a genuine restriction (CFSG remains Mountain but narrowly scoped).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infinitary_extension, conceptual, 'Whether the classification principle extends to infinite groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finite_simple_groups_classification, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfsg_tr_t0, finite_simple_groups_classification, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cfsg_tr_t50, finite_simple_groups_classification, theater_ratio, 50, 0.14).
narrative_ontology:measurement(cfsg_tr_t100, finite_simple_groups_classification, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(cfsg_be_t0, finite_simple_groups_classification, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cfsg_be_t50, finite_simple_groups_classification, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(cfsg_be_t100, finite_simple_groups_classification, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finite_simple_groups_classification, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
