% ============================================================================
% CONSTRAINT STORY: genetic_predisposition_mania
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_genetic_predisposition_mania, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: genetic_predisposition_mania
 *   human_readable: "Genetic Blueprint for Manic Episodes in Bipolar Disorder"
 *   domain: technological
 *
 * SUMMARY:
 *   A network of genes has been identified whose expression patterns strongly
 *   predict the onset of manic episodes in individuals with bipolar disorder.
 *   This constraint is not a single gene but a polygenic, systemic property of
 *   an individual's biology. It represents a fundamental, non-negotiable
 *   biological limit on neurological state regulation for affected individuals.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individuals with bipolar disorder: Primary target (powerless/trapped) — they are subject to this biological limit.
 *   - Clinical and Research Institutions: Primary beneficiary (institutional/arbitrage) — they benefit from the *knowledge* of this constraint to coordinate research, develop therapies, and manage patient populations.
 *   - Bioethicists / Systems Biologists: Analytical observer — sees the full structure from biological mechanism to societal implication.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_predisposition_mania, 0.05).
domain_priors:suppression_score(genetic_predisposition_mania, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(genetic_predisposition_mania, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_predisposition_mania, extractiveness, 0.05).
narrative_ontology:constraint_metric(genetic_predisposition_mania, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(genetic_predisposition_mania, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. These values ensure the constraint passes the
% mountain metric gate.
narrative_ontology:constraint_metric(genetic_predisposition_mania, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(genetic_predisposition_mania, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(genetic_predisposition_mania, mountain).
narrative_ontology:human_readable(genetic_predisposition_mania, "Genetic Blueprint for Manic Episodes in Bipolar Disorder").
narrative_ontology:topic_domain(genetic_predisposition_mania, "technological").

% --- Emergence flag (required for mountain constraints) ---
% This constraint is a biological phenomenon that emerges from genetics
% without human design or enforcement. This flag is required for the
% mountain classification metric gate to fire.
domain_priors:emerges_naturally(genetic_predisposition_mania).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Omitted as this is a Mountain constraint. The constraint is a natural law
% of biology; it does not have inherent beneficiaries or victims in the same
% way a man-made policy does. The *knowledge about* the constraint has
% beneficiaries (researchers, pharma), but that is a separate, downstream
% constraint (see Network Data section).

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
% This constraint is a physical/biological reality. Its classification is
% invariant across all perspectives. The following perspectives demonstrate
% this invariance.

% PERSPECTIVE 1: THE INDIVIDUAL WITH THE PREDISPOSITION
% From their perspective, the genetic network is an unchangeable,
% non-negotiable fact of their existence.
constraint_indexing:constraint_classification(genetic_predisposition_mania, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CLINICAL/RESEARCH INSTITUTION
% Even for the institution that benefits from understanding this constraint,
% the constraint *itself* is a fixed parameter of nature they must work with.
constraint_indexing:constraint_classification(genetic_predisposition_mania, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The systems biologist or bioethicist sees the genetic network as a
% fundamental component of a complex biological system.
constraint_indexing:constraint_classification(genetic_predisposition_mania, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_predisposition_mania_tests).

test(invariance) :-
    % For a Mountain, we verify invariance, not a perspectival gap.
    constraint_indexing:constraint_classification(genetic_predisposition_mania, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition_mania, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition_mania, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == mountain,
    TypeTarget == TypeBeneficiary,
    TypeBeneficiary == TypeAnalytical.

test(natural_law_profile_adherence) :-
    domain_priors:base_extractiveness(genetic_predisposition_mania, E), E =< 0.25,
    domain_priors:suppression_score(genetic_predisposition_mania, S), S =< 0.05,
    narrative_ontology:constraint_metric(genetic_predisposition_mania, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(genetic_predisposition_mania, resistance, R), R =< 0.15,
    domain_priors:emerges_naturally(genetic_predisposition_mania).

:- end_tests(genetic_predisposition_mania_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it represents a
 *   biological reality that is, for the individual, unchangeable, non-negotiable,
 *   and emerges naturally from their genetic makeup. The base extractiveness (ε)
 *   is near zero (0.05) because the gene network itself does not "extract" value;
 *   it is a state of being. The suppression score is also near zero (0.02)
 *   as there are no alternatives to one's own genome to suppress. The NL
 *   profile metrics (`accessibility_collapse`=0.95, `resistance`=0.05) and the
 *   `emerges_naturally` flag confirm its status as a natural law of a
 *   person's biology.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in the *classification type*; all agents
 *   perceive the biological reality as a fixed Mountain. The significant gap
 *   exists in the *implications* of this knowledge. For the individual, it's a
 *   limitation. For a research institution, it's an opportunity for coordination
 *   (a Rope). This distinction is handled by decomposing the problem into two
 *   constraints, per the ε-invariance principle (see Network Data section).
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint, directionality is not a primary driver of
 *   classification. The concept of "beneficiary" and "victim" applies more
 *   to the socio-technical systems built *around* this biological fact, rather
 *   than the fact itself. Thus, `constraint_beneficiary` and `constraint_victim`
 *   are not declared for this story.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification is crucial for preventing Mandatrophy. Mislabeling this
 *   biological reality as a Rope would imply it is merely a coordination
 *   problem that can be "solved." Mislabeling it as a Snare would incorrectly
 *   attribute intentional extraction to a natural process. By classifying the
 *   biological layer as a Mountain, we isolate the fixed reality. Any
 *   extraction or coordination must then be located in a separate, downstream,
 *   man-made constraint (e.g., a diagnostic or insurance framework), which can
 *   be analyzed on its own terms as a potential Tangled Rope or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_genetic_predisposition_mania,
    'Is the identified gene network truly causal for mania, or is it a very strong correlation with an unknown upstream biological driver?',
    'Longitudinal studies with genetic knockdown/editing experiments (e.g., in organoid models) that demonstrate a direct causal link between the network expression and manic-like cellular phenotypes.',
    'If causal, the Mountain classification is robust. If merely correlational, this constraint is a Piton (a diagnostic signal with no causal power), and the true Mountain lies elsewhere.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(genetic_predisposition_mania, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for this constraint as base_extractiveness (0.05) is below
% the 0.46 threshold. A biological constant does not exhibit lifecycle drift
% in the same manner as a social institution.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type and Boltzmann floor are not applicable to a Mountain.

% --- Network Decomposition (Constraint Families) ---
% This story models the biological reality (ε≈0). A separate constraint would
% model the socio-technical framework built upon this knowledge, which would have
% a much higher ε. This decomposition is mandated by the ε-invariance principle.

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from the colloquial concept of "mania genetics".
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - mania_prs_framework (ε≈0.45, Tangled Rope) - The diagnostic, clinical, and insurance framework built on this knowledge.

narrative_ontology:affects_constraint(genetic_predisposition_mania, mania_prs_framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The constraint is a Mountain, and its classification
% is not sensitive to the directionality `d`. The structural invariance is the
% dominant feature.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */