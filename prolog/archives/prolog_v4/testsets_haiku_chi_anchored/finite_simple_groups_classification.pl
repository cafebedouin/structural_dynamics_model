% ============================================================================
% CONSTRAINT STORY: finite_simple_groups_classification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: finite_simple_groups_classification
 *   human_readable: The Classification of Finite Simple Groups (CFSG)
 *   domain: mathematical/group_theory
 *
 * SUMMARY:
 *   The Classification of Finite Simple Groups (CFSG) represents a landmark
 *   mathematical achievement: the complete enumeration of all finite simple
 *   groups as either members of 18 infinite families (cyclic groups of prime
 *   order, projective special linear groups, Chevalley groups, Steinberg
 *   groups, and Suzuki-Ree groups), alternating groups, or one of 26
 *   exceptional sporadic groups. This constraint is a mountain—an immutable
 *   structural fact about the mathematical universe. The CFSG is not a
 *   coordination mechanism, not an extraction device, and not contingent on
 *   institutional measurement choices. It is a logical necessity that emerges
 *   from the axioms of group theory. The constraint exhibits zero degrees of
 *   freedom across all indices: from the perspective of any agent (powerless
 *   field researcher, analytical observer, organized mathematics community,
 *   or institutional actor), the CFSG is equally immutable. The proof is
 *   distributed across thousands of pages of published mathematics (some of
 *   the proof remains in unpublished form or specialized dissertations), and
 *   there is now significant effort to formalize portions using proof
 *   assistants. Theater ratio remains low because mathematical proofs are
 *   either correct or incorrect—there is minimal performative content.
 *   Extractiveness is minimal because no agent extracts value from others via
 *   the CFSG; the constraint enables unified understanding without asymmetric
 *   extraction.
 *
 * KEY AGENTS:
 *   - The Field of Group Theory: Operates within the CFSG as a structural ceiling; all research presupposes the classification is complete and exhaustive
 *   - Individual Mathematicians: Researchers using CFSG as a foundational fact for further results; no extraction or coordination problem exists
 *   - Mathematical Institutions: Universities, research institutes, funding bodies that organize mathematics; the CFSG is independent of institutional structure
 *   - Proof Verification Community: Organized effort to formalize CFSG in proof assistants (Coq, Lean); working group addressing completeness verification
 *   - Analytical Observer: Civilizational perspective recognizing CFSG as a fundamental truth of mathematical logic
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
narrative_ontology:topic_domain(finite_simple_groups_classification, "mathematical/group_theory").

domain_priors:emerges_naturally(finite_simple_groups_classification).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GROUP THEORY FIELD (MOUNTAIN) — The CFSG is an immutable structural fact about finite simple groups: the classification is complete, exhaustive, and independent of any institutional arrangement or choice of measurement methodology. All researchers in group theory operate within this constraint as a fixed ceiling on what finite simple groups can be. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(finite_simple_groups_classification, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER / MATHEMATICAL LOGIC (MOUNTAIN) — The CFSG is a fundamental result of mathematical logic and group theory structure. The existence of the 26 sporadic groups, the infinite families, and alternating groups is not contingent on institutional measurement choices, funding, or publication venues. The constraint is a logical necessity emerging from the axioms of group theory. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(finite_simple_groups_classification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY / INSTITUTIONAL (MOUNTAIN) — Even institutions organizing mathematics research (universities, research institutes, grant agencies) cannot modify or escape the CFSG. It functions as an immutable constraint on the space of possible finite simple groups, regardless of institutional structure or research priorities. The constraint is discovered, not constructed. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(finite_simple_groups_classification, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICS COMMUNITY / ORGANIZED RESEARCHERS (MOUNTAIN) — Organized mathematical communities (working groups, proof-verification collaborations, software tool developers) experience the CFSG as a stable framework. The constraint enables their work: once CFSG is established, finite simple groups have a known structure, enabling computation and classification-based proofs. The constraint is enabling, not extractive. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.06.
constraint_indexing:constraint_classification(finite_simple_groups_classification, mountain,
    context(agent_power(organized),
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
 *   Extractiveness (0.12): Very low. The CFSG does not create asymmetric extraction—no agent benefits disproportionately and no agent bears disproportionate costs. The classification is a public mathematical fact accessible to all researchers. The value of CFSG is enabling: it allows all downstream mathematics to operate within a known constraint space. The small non-zero value (0.12 vs 0.0) reflects the minimal asymmetry in citation credit and historical priority attribution, which are negligible in the context of fundamental mathematical facts. Suppression (0.03): Minimal. There are no significant barriers to understanding or accessing the CFSG. The proof is difficult and requires substantial mathematical background, but this is a feature of mathematical literacy, not a deliberate suppression mechanism. Researchers who need the CFSG can learn it; there is no hidden machinery or coercive alternative. Theater ratio (0.15): Very low. Mathematical proofs are low-theater by nature—either the proof works or it fails. There is some minimal theater in proof presentation (pedagogical ordering, gap-filling in publication that was computed but not explicitly shown) but no significant performative component. The 15% accounts for the fact that mathematical proofs are communicated in human language and notation, which always involves some structuring choices, but these are not meaningfully extractive or performative. Claimed type (mountain): The CFSG satisfies all mountain criteria—ε ≤ 0.25, suppression ≤ 0.05, emerges_naturally = true, accessibility_collapse ≥ 0.85, resistance ≤ 0.15.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is invariant across all perspectives: every agent (powerless field researcher, analytical observer, institutional researcher, organized community) perceives the CFSG as a mountain—an immutable fact. There is no perspectival gap because the CFSG is not a coordination mechanism (no perspective sees it as Rope), not an extraction mechanism (no perspective sees it as Snare or Tangled Rope), and not institutional inertia (no perspective sees it as Piton). The uniformity of classification across all six perspectives is characteristic of true natural laws and fundamental mathematical facts. This is exactly what should happen for a constraint that is not socially constructed and not dependent on measurement methodology.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive d from the analytical baseline: the CFSG is a mathematical fact independent of any agent's structural position. The constraint does not benefit one agent at the expense of another. All agents have equal access to the truth of the CFSG. There is no beneficiary/victim relationship because the constraint is not extraction. The directionality is symmetric (d≈0.50) from most perspectives, with the analytical observer at d≈0.72 (standard analytical position) and the institutional beneficiary at d≈0.05 (reflecting that institutions benefit slightly from being able to reference CFSG as objective fact, but this is not asymmetric extraction). No directionality overrides are needed—the structural data derives cleanly from the nature of mathematical facts.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy issue arises for CFSG because the constraint is not a candidate for the mandatrophy problem. Mandatrophy occurs when a constraint is mislabeled as extraction (Snare) when it is actually coordination (Rope and Tangled Rope). The CFSG is neither extraction nor coordination—it is a logical fact. All six perspectives agree on classification (Mountain), confirming that the constraint is not being forced into an incorrect category. The mathematical community's understanding of CFSG as a fundamental fact (not a constructed social arrangement) aligns perfectly with the Mountain classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_completeness_verification,
    'Is the CFSG proof genuinely complete and correct in all details, or does it rest on computational verification that could harbor undetected errors?',
    'Formal verification of proof using proof assistants (Coq, Lean); automated checking of all case analyses and gap-filling arguments; independent re-examination of classification cases by alternative proof methods',
    'If fully verified: Mountain classification is confirmed — CFSG is an immutable logical fact. If gaps exist: CFSG remains the best known classification but becomes a high-confidence conjecture (ε→0.35, classification→Tangled Rope or Rope, depending on epistemic consensus).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_completeness_verification, empirical, 'Whether CFSG proof is complete and error-free in all details').

omega_variable(
    sporadic_group_uniqueness,
    'Are the 26 sporadic groups uniquely determined by their defining properties, or could alternative characterizations yield different sporadic structures?',
    'Examination of uniqueness theorems for each sporadic group; analysis of whether different axiomatizations could yield non-isomorphic finite simple groups outside the 18 families',
    'If uniquely determined: Mountain classification holds. If alternative characterizations possible: CFSG would require qualification as ''under current axiomatization'' (ε→0.25, classification→Piton or Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sporadic_group_uniqueness, conceptual, 'Whether the 26 sporadic groups are uniquely characterized').

omega_variable(
    computational_gap_filling,
    'How much of the CFSG proof relies on computational verification of finite cases versus purely logical argument?',
    'Quantification of proof lines that depend on computer calculations; assessment of the independence of computational steps; potential for undetected computational errors in large case analyses',
    'If >20% computational: CFSG is a mountain with a computational foundation requiring trust in hardware/software (ε→0.18, still Mountain but with subtle epistemological nuance). If <5%: purely logical foundation confirmed (ε→0.08, pure Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_gap_filling, empirical, 'Proportion of CFSG proof relying on computational verification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finite_simple_groups_classification, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfsg_tr_t0, finite_simple_groups_classification, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cfsg_tr_t50, finite_simple_groups_classification, theater_ratio, 50, 0.13).
narrative_ontology:measurement(cfsg_tr_t100, finite_simple_groups_classification, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(cfsg_be_t0, finite_simple_groups_classification, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cfsg_be_t50, finite_simple_groups_classification, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(cfsg_be_t100, finite_simple_groups_classification, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finite_simple_groups_classification, information_standard).
narrative_ontology:affects_constraint(finite_simple_groups_classification, monster_group_existence).
narrative_ontology:affects_constraint(finite_simple_groups_classification, group_cohomology_computation).
narrative_ontology:affects_constraint(finite_simple_groups_classification, representation_theory_structure).

% DUAL FORMULATION NOTE:
% CFSG is a foundational constraint in mathematics. Its downstream constraints include specific results about the Monster Group (ε≈0.10, Mountain—existence follows from CFSG), group cohomology (ε≈0.25, Mountain—computational structure is fixed by CFSG), and representation theory (ε≈0.15, Mountain—representation-theoretic classifications depend on CFSG). All downstream constraints are more specialized instances of the same fundamental classification principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
