% ============================================================================
% CONSTRAINT STORY: feline_human_cancer_homology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feline_human_cancer_homology, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feline_human_cancer_homology
 *   human_readable: Homology of Feline and Human Cancers
 *   domain: biological/medical_science
 *
 * SUMMARY:
 *   A landmark 2026 study from Cornell University establishes deep molecular
 *   and genetic parallels between several common feline cancers and their
 *   human counterparts. This discovery provides a powerful new natural model
 *   for studying cancer biology and testing novel therapies. The constraint
 *   is the body of scientific knowledge itself—a set of discovered facts
 *   about biological reality.
 *
 * KEY AGENTS:
 *   - Comparative Oncology Researchers (analytical): Primary beneficiaries who gain a powerful new research paradigm.
 *   - Pharmaceutical Companies (institutional/arbitrage): Beneficiaries who can accelerate drug development and reduce costs by using more accurate animal models.
 *   - Pet Owners (powerless/trapped): Indirect, long-term beneficiaries of new treatments, but immediately confronted by the 'mountain' of biological fact.
 *   - Feline-Specific Research Foundations (organized/constrained): Experience the knowledge as both a coordinating tool and a potential source of competition for funding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feline_human_cancer_homology, 0.04).
domain_priors:suppression_score(feline_human_cancer_homology, 0.02).
domain_priors:theater_ratio(feline_human_cancer_homology, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feline_human_cancer_homology, extractiveness, 0.04).
narrative_ontology:constraint_metric(feline_human_cancer_homology, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(feline_human_cancer_homology, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feline_human_cancer_homology, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(feline_human_cancer_homology, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feline_human_cancer_homology, mountain).
narrative_ontology:human_readable(feline_human_cancer_homology, "Homology of Feline and Human Cancers").
narrative_ontology:topic_domain(feline_human_cancer_homology, "biological/medical_science").

domain_priors:emerges_naturally(feline_human_cancer_homology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feline_human_cancer_homology, comparative_oncology_researchers).
narrative_ontology:constraint_beneficiary(feline_human_cancer_homology, pharmaceutical_developers).
narrative_ontology:constraint_beneficiary(feline_human_cancer_homology, veterinary_medicine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL (MOUNTAIN) — From a scientific standpoint, the homology is a discovered, unchangeable fact of biology. It represents a new mountain on the landscape of knowledge, fundamentally altering the understanding of cancer across species. Its low ε and high accessibility_collapse are definitional.
constraint_indexing:constraint_classification(feline_human_cancer_homology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PET OWNER (MOUNTAIN) — For an individual whose pet is diagnosed with one of these cancers, the biological reality is an immediate, immovable object. They are trapped by the physical facts of the disease. The knowledge that it mirrors human cancer offers little immediate solace and simply describes the mountain they must face.
constraint_indexing:constraint_classification(feline_human_cancer_homology, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: R&D CONSORTIUM (ROPE) — This institution views the discovery as a powerful coordination mechanism. It enables the use of feline subjects as highly accurate models for human therapies, coordinating research efforts and capital allocation globally. The knowledge itself is a non-extractive Rope that facilitates massive efficiency gains. As a beneficiary with arbitrage exit, χ is negative.
constraint_indexing:constraint_classification(feline_human_cancer_homology, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SPECIALIZED FOUNDATION (TANGLED ROPE) — This group sees a dual-edged sword. The homology validates their work but also invites larger, human-focused funds to co-opt their research area, potentially redirecting resources away from uniquely feline cancer problems. They are constrained by this new competitive landscape. NOTE: This classification is a perspectival warning; the base ε=0.04 is too low for a true Tangled Rope, indicating that the extractive dynamic is a property of a separate, downstream constraint concerning resource allocation, not this foundational knowledge itself.
constraint_indexing:constraint_classification(feline_human_cancer_homology, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feline_human_cancer_homology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feline_human_cancer_homology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feline_human_cancer_homology, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(feline_human_cancer_homology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feline_human_cancer_homology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(feline_human_cancer_homology, ExtMetricName, E),
    domain_priors:suppression_score(feline_human_cancer_homology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(feline_human_cancer_homology),
    narrative_ontology:constraint_metric(feline_human_cancer_homology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(feline_human_cancer_homology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(feline_human_cancer_homology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (ε=0.04) and Suppression (0.02) are extremely low, as this constraint represents a body of scientific knowledge. It doesn't inherently extract or coerce. The NL profile metrics are high (accessibility_collapse=0.95) and low (resistance=0.10) because once scientifically established, this biological fact becomes difficult to contest and is accepted as a baseline reality, fulfilling the criteria for a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   The gap arises from the difference between knowledge and its application. For the pure scientist, it's a Mountain—a fixed fact. For an R&D consortium, that same fact becomes a Rope—a tool for coordinating complex research and investment. For a smaller, specialized foundation, the application of the knowledge creates resource competition, making it feel like a Tangled Rope. The core constraint remains a Mountain; the other perspectives reveal how it is operationalized by different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are those who can leverage this new information standard to coordinate activity, principally researchers and developers. They have low directionality (d). There are no direct victims of the knowledge itself, so no group has a structurally high d value relative to this constraint. The potential for victimhood arises in downstream constraints related to funding and policy, as hinted at by the Feline-Specific Research Foundation's perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This story demonstrates the critical principle of ε-invariance and constraint decomposition. The core discovery of homology is a Mountain. Attempting to bundle the downstream effects (e.g., shifts in research funding, ethical debates on animal models) into this single story would incorrectly inflate its extractiveness. The framework correctly identifies the core fact as a Mountain and uses the network graph to link it to subsequent, structurally distinct constraints (like resource allocation) that are Tangled Ropes or Snares. This prevents the mischaracterization of a natural law by conflating it with the social systems that react to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feline_human_cancer_homology, 2026, 2056).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feline_human_cancer_homology, information_standard).
narrative_ontology:affects_constraint(feline_human_cancer_homology, feline_oncology_funding_prioritization).
narrative_ontology:affects_constraint(feline_human_cancer_homology, animal_models_in_drug_development).

% DUAL FORMULATION NOTE:
% This constraint represents the pure scientific discovery (a Mountain). Its practical and ethical implications are modeled in separate, downstream constraints which this one enables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
