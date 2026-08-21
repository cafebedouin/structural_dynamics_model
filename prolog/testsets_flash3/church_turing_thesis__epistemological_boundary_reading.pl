% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__epistemological_boundary_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis: Epistemological Boundary Reading
 *   domain: philosophy_of_mathematics/philosophy_of_computation
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis as an epistemological
 *   boundary: what we can formally prove computable is exactly what Turing
 *   machines can compute. This reading defines the scope of 'knowable
 *   computation' within formal systems, rather than making a claim about
 *   physical reality or a mere mathematical definition. It implicitly
 *   excludes non-constructive mathematical claims about computability and
 *   theoretical hypercomputation from the domain of formally verifiable
 *   computation. The claimed type is 'rope' because it provides a crucial
 *   coordination function for formal reasoning, but the metrics reflect a
 *   low-to-moderate extractiveness due to the exclusion of alternative
 *   computational paradigms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.35).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.6).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis: Epistemological Boundary Reading").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/philosophy_of_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '963efc86-37ba-40f0-a28a-792d3e29bc6e').
narrative_ontology:cs_kernel_codification('963efc86-37ba-40f0-a28a-792d3e29bc6e', formalized).
narrative_ontology:cs_authority_grounding('963efc86-37ba-40f0-a28a-792d3e29bc6e', expertise).
narrative_ontology:cs_interpretation_layer_present('963efc86-37ba-40f0-a28a-792d3e29bc6e').
narrative_ontology:cs_reading_relation('963efc86-37ba-40f0-a28a-792d3e29bc6e', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('963efc86-37ba-40f0-a28a-792d3e29bc6e', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('963efc86-37ba-40f0-a28a-792d3e29bc6e', foundational, computability_is_formal_provability).
narrative_ontology:cs_axiom_status(computability_is_formal_provability, holdable).
narrative_ontology:cs_axiom_grounding('963efc86-37ba-40f0-a28a-792d3e29bc6e', computability_is_formal_provability, deontological).
narrative_ontology:cs_axiom('963efc86-37ba-40f0-a28a-792d3e29bc6e', foundational, turing_machines_define_formal_limits).
narrative_ontology:cs_axiom_status(turing_machines_define_formal_limits, holdable).
narrative_ontology:cs_axiom_grounding('963efc86-37ba-40f0-a28a-792d3e29bc6e', turing_machines_define_formal_limits, conventional).
narrative_ontology:cs_reference_frame('963efc86-37ba-40f0-a28a-792d3e29bc6e', formal_proof_theoretic_framework).
narrative_ontology:cs_drift_state('963efc86-37ba-40f0-a28a-792d3e29bc6e', contemporary_hypercomputation_debates, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('963efc86-37ba-40f0-a28a-792d3e29bc6e', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computer_scientists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, formal boundary for what counts as a 'computable' function, aligning with their emphasis on explicit constructions and proofs. The thesis provides a stable foundation for their work, defining the scope of what is formally knowable in computation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians, beneficiary,
    institutional, generational, identity_locked, global).

% Rely on the thesis as the foundational definition of computability, enabling the design and analysis of algorithms and computational models. It provides a stable theoretical framework for the entire discipline, defining the limits of what can be achieved by mechanical procedures.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computer_scientists, beneficiary,
    institutional, generational, identity_locked, global).

% Bear the cost of having their non-constructive approaches to computability (e.g., those relying on infinite choice sequences or uncomputable oracles without explicit reduction to Turing machines) excluded from the mainstream definition of 'formally knowable computation'. Their work is often marginalized or re-framed as outside the scope of the thesis.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_theorists, payer,
    moderate, biographical, constrained, global).

% Propose models of computation (e.g., based on exotic physics or infinitary operations) that exceed Turing computability. This reading of the thesis directly excludes their claims from the domain of 'formally knowable computation', forcing them to argue for a redefinition of the boundary or to operate outside the established framework.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_theorists, payer,
    moderate, biographical, constrained, global).

% Analyze the philosophical implications and interpretations of the Church-Turing Thesis, including its status as a definition, an empirical claim, or an epistemological boundary. They observe the debates and the structural effects of each reading without being directly subject to its enforcement.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_mathematics, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universally accepted, formal boundary for what constitutes 'computable' in a proof-theoretic sense, allowing mathematicians and computer scientists to coordinate on a shared understanding of the limits of formal provability in computation.
% TRANSFER_FUNCTION: Transfers epistemic authority and definitional clarity to Turing-computable functions, while excluding non-constructive or hypercomputational claims from the domain of 'formally knowable computation'.
% ABSENT_VOICES: Theorists working on non-Turing models of computation or non-constructive proofs of computability are often implicitly excluded from the core discourse on 'computability' as defined by this reading, their work relegated to 'beyond the boundary' rather than challenging it directly.
% DISAPPEARANCE_RATIONALE: If this epistemological boundary vanished, the field of computability theory would lose its primary formal anchor. What counts as a 'computable' function would become highly ambiguous, leading to fragmentation in proof methods and a lack of consensus on the scope of formal computation. The entire edifice of theoretical computer science would need to be re-evaluated.
% FOUNDING_PROBLEM: To provide a rigorous, formal definition of 'effective calculability' or 'computability' that could be universally accepted and used as a basis for mathematical proofs and the design of computing machines.
% FOUNDING_PROBLEM_CORROBORATION: The problem of formally defining computability remains live for constructive mathematicians and computer scientists, who continue to rely on the thesis for foundational clarity. Philosophers of mathematics corroborate the ongoing need for such a boundary, even as they debate its nature.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__epistemological_boundary_reading_tests).
:- end_tests(church_turing_thesis__epistemological_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.35) because while it defines a boundary, it doesn't actively 'take' resources, but rather 'excludes' certain types of claims from a domain. Suppression is moderate (0.6) as it requires active enforcement within formal systems to maintain the boundary of what is considered a valid proof of computability. Accessibility collapse is high (0.7) because once this epistemological boundary is accepted, alternatives for 'formally knowable computation' largely collapse. Resistance is low (0.2) because within the core communities (constructive mathematics, computer science), the thesis is widely accepted as a foundational principle. Theater ratio is low (0.1) as its function is primarily definitional and methodological, with little performative overhead.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, the thesis is a foundational rope, providing clarity and a shared framework. From the perspective of victims, it acts as a subtle snare, limiting the scope of legitimate inquiry or forcing their work into a 'beyond Turing' category that is not considered 'formally knowable'. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Constructive mathematicians and computer scientists are beneficiaries (d near 0.0) as the thesis provides a stable, universally accepted framework for their work. Non-constructive computability theorists and hypercomputation theorists are payers/victims (d near 1.0) as their work is either excluded or marginalized by this epistemological boundary. Philosophers of mathematics are observers, analyzing its implications without being directly subject to its enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_of_hypercomputation,
    'Are hypercomputational models genuinely ''computation'' in a sense relevant to the Church-Turing Thesis, or do they operate in a fundamentally different domain?',
    'Development of a widely accepted formal framework that rigorously connects or distinguishes hypercomputational models from Turing-machine models, or empirical evidence of physical hypercomputation.',
    'If hypercomputation is accepted as a valid form of ''computation'', the epistemological boundary defined by this reading would be challenged, potentially increasing its extractiveness and suppression as it would actively exclude a recognized form of computation. If it''s deemed fundamentally different, the boundary remains intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_of_hypercomputation, conceptual, 'Ambiguity regarding the scope and definition of ''computation'' itself, particularly at the boundaries of Turing equivalence.').

omega_variable(
    constructive_vs_non_constructive_proofs,
    'To what extent do non-constructive proofs of computability genuinely challenge the ''formally knowable'' aspect of this reading, or are they simply different methodological approaches?',
    'A philosophical consensus or formal meta-mathematical result clarifying the relationship between constructive and non-constructive proofs in the context of computability theory.',
    'If non-constructive proofs are seen as equally ''formally knowable'' for computability, this reading''s suppressive aspect would decrease, as it would no longer exclude a significant class of mathematical claims. If the distinction is maintained as fundamental, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructive_vs_non_constructive_proofs, conceptual, 'Ambiguity in the interpretation of ''formally knowable'' in the context of different mathematical proof traditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.2).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.4).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Church-Turing Thesis kernel. This 'epistemological_boundary_reading' defines the limits of formally knowable computation, distinct from its status as a mathematical definition or a physical claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
