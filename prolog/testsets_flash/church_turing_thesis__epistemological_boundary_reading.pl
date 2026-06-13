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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis: Epistemological Boundary Reading
 *   domain: philosophy_of_mathematics/computation
 *
 * SUMMARY:
 *   This constraint represents the Church-Turing Thesis as an epistemological
 *   boundary, defining what counts as 'formally knowable computation.' It
 *   asserts that any function for which we can *prove* computability is
 *   exactly Turing-computable, irrespective of whether non-Turing-computable
 *   functions might exist physically or mathematically. This reading
 *   emphasizes the limits of formal proof and constructive methods in
 *   computability theory.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.3).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.6).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis: Epistemological Boundary Reading").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, 'dc25e784-828e-49ee-9770-7df79cbb7575').
narrative_ontology:cs_kernel_codification('dc25e784-828e-49ee-9770-7df79cbb7575', formalized).
narrative_ontology:cs_authority_grounding('dc25e784-828e-49ee-9770-7df79cbb7575', expertise).
narrative_ontology:cs_interpretation_layer_present('dc25e784-828e-49ee-9770-7df79cbb7575').
narrative_ontology:cs_reading_relation('dc25e784-828e-49ee-9770-7df79cbb7575', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc25e784-828e-49ee-9770-7df79cbb7575', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('dc25e784-828e-49ee-9770-7df79cbb7575', foundational, computability_is_provability).
narrative_ontology:cs_axiom_status(computability_is_provability, holdable).
narrative_ontology:cs_axiom_grounding('dc25e784-828e-49ee-9770-7df79cbb7575', computability_is_provability, deontological).
narrative_ontology:cs_axiom('dc25e784-828e-49ee-9770-7df79cbb7575', secondary, turing_machine_is_universal_formal_model).
narrative_ontology:cs_axiom_status(turing_machine_is_universal_formal_model, holdable).
narrative_ontology:cs_axiom_grounding('dc25e784-828e-49ee-9770-7df79cbb7575', turing_machine_is_universal_formal_model, conventional).
narrative_ontology:cs_reference_frame('dc25e784-828e-49ee-9770-7df79cbb7575', formal_provability_paradigm).
narrative_ontology:cs_drift_state('dc25e784-828e-49ee-9770-7df79cbb7575', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dc25e784-828e-49ee-9770-7df79cbb7575', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computer_scientists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, philosophers_of_hypercomputation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, provable boundary for what counts as computable, aligning with their emphasis on explicit constructions and proofs. The thesis provides a stable foundation for their work.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians, beneficiary,
    institutional, generational, identity_locked, global).

% Rely on the thesis to define the limits of what algorithms can achieve, guiding research and development in computability theory and algorithm design. It provides a shared understanding of 'computable'.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computer_scientists, beneficiary,
    institutional, generational, identity_locked, global).

% Are constrained by the thesis's definition of 'knowable computation,' which can exclude or marginalize theoretical work on computability that relies on non-constructive methods or hypothetical 'hypercomputers' not reducible to Turing machines. Their work is often framed as outside the 'standard' definition.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_theorists, payer,
    moderate, biographical, constrained, global).

% Challenge the epistemological boundary, arguing that the thesis limits inquiry into physically or mathematically possible forms of computation beyond Turing machines. They bear the cost of having their research often dismissed as 'not real computation' within mainstream discourse.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_hypercomputation, payer,
    moderate, biographical, constrained, global).

% Are the primary custodians and interpreters of the Church-Turing Thesis, defining its scope and implications for formal systems. They enforce the methodological boundary by setting standards for what constitutes a 'computability proof'.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, foundational_logicians, agenda_setter,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, rigorous standard for what constitutes 'computable' within formal systems, enabling mathematicians and computer scientists to communicate and build upon a common foundation for computability theory.
% TRANSFER_FUNCTION: Transfers epistemic authority and methodological legitimacy to Turing-computable models, while implicitly excluding or de-prioritizing non-Turing-computable or non-constructive approaches to computability.
% ABSENT_VOICES: The 'hypercomputation' community and some philosophers of mathematics, who argue for broader definitions of computability, are often marginalized in mainstream discussions, their perspectives deemed outside the established framework.
% DISAPPEARANCE_RATIONALE: If the thesis as an epistemological boundary vanished, the foundations of computability theory would become highly ambiguous. What counts as a 'computable function' or a 'proof of computability' would be open to constant redefinition, leading to fragmentation in research and a lack of shared understanding across disciplines.
% FOUNDING_PROBLEM: Before the Church-Turing Thesis, there was no universally accepted, rigorous definition of 'effective computability,' leading to ambiguity and difficulty in formalizing what could be computed by an algorithm.
% FOUNDING_PROBLEM_CORROBORATION: The problem of rigorously defining computability remains live, as new computational paradigms emerge. The consensus among mathematicians and computer scientists (outside the direct beneficiaries) is that the thesis continues to provide a necessary, stable definition, even if its philosophical implications are debated.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).

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
 *   The constraint is classified as a Rope because it provides a crucial coordination function for formal computability theory, establishing a shared methodological standard. However, it has a low-to-moderate extractiveness (0.3) and moderate suppression (0.6) because it implicitly excludes or marginalizes alternative approaches to computability that do not fit the Turing model or constructive proof methods. The 'requires_active_enforcement' is true because the boundary is maintained through peer review, curriculum design, and the framing of what constitutes 'valid' research in the field. Theater ratio is low (0.1) as its function is genuinely foundational.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, the thesis is a robust, indispensable tool for formalizing computation. From the perspective of payers, it is a restrictive methodological gate that limits the scope of inquiry into broader notions of computability. The engine will compute different classifications for these seats based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Constructive mathematicians and computer scientists are beneficiaries, as the thesis provides a stable, shared foundation for their work (low directionality). Non-constructive computability theorists and philosophers of hypercomputation are payers, as their work is often excluded or de-prioritized by this boundary (high directionality). Foundational logicians act as agenda-setters, defining and enforcing the methodological limits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemological_vs_definitional,
    'Is the Church-Turing Thesis primarily an epistemological boundary for formal proof, or a conventional mathematical definition of ''effective computability''?',
    'Analysis of how the thesis is invoked in foundational debates: if its force derives from what can be proven, it''s epistemological; if from its utility as a stipulative definition, it''s definitional.',
    'If purely definitional (mathematical_definition_reading), its extractiveness would be lower, as it would be a choice of language rather than an exclusion of methods. If epistemological, the exclusion of non-constructive methods is a core function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemological_vs_definitional, conceptual, 'Ambiguity between epistemological boundary and conventional definition.').

omega_variable(
    epistemological_vs_physical,
    'Does the epistemological boundary reading implicitly or explicitly foreclose the possibility of physical hypercomputation, or does it remain agnostic on physical limits?',
    'Careful textual analysis of foundational arguments by proponents of this reading: do they claim physical impossibility, or merely formal unprovability?',
    'If it implicitly forecloses physical hypercomputation, its suppression of ''philosophers_of_hypercomputation'' is more direct and its extractiveness higher. If agnostic, the suppression is purely methodological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemological_vs_physical, conceptual, 'Relationship between epistemological and physical claims.').


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
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.2).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
