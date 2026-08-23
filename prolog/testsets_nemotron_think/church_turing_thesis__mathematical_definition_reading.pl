% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__mathematical_definition_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis as Mathematical Definition of Effective Computability
 *   domain: philosophy_of_mathematics / philosophy_of_computation / foundations_of_computer_science
 *
 * SUMMARY:
 *   This constraint story captures the mathematical_definition_reading of the
 *   Church-Turing thesis: the thesis is a stipulative definition fixing the
 *   meaning of 'effective computability' to coincide with Turing-machine
 *   computability (equivalently, lambda-definability, general recursiveness,
 *   etc.). It is a coordination convention adopted by the mathematical
 *   community because the major formal models turned out to be extensionally
 *   equivalent. The constraint has negligible extraction, no suppression, and
 *   no victims — it is a pure Rope. The definition is not enforced; it is
 *   adopted because it enables clear communication and theorem-proving. The
 *   kernel_id is 'church_turing_thesis'; this reading instantiates the
 *   'mathematical_definition_reading'. Sibling readings are
 *   'physical_claim_reading' (empirical claim about physics) and
 *   'epistemological_boundary_reading' (boundary of formally knowable
 *   computation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.03).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Mathematical Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics / philosophy_of_computation / foundations_of_computer_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '9a0bd976-00fc-454c-bae6-e9b40e8bfa57').
narrative_ontology:cs_kernel_codification('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', formalized).
narrative_ontology:cs_authority_grounding('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', expertise).
narrative_ontology:cs_reading_relation('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', foundational, church_turing_thesis_is_stipulative_definition).
narrative_ontology:cs_axiom_status(church_turing_thesis_is_stipulative_definition, holdable).
narrative_ontology:cs_axiom_grounding('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', church_turing_thesis_is_stipulative_definition, conventional).
narrative_ontology:cs_axiom('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', secondary, effective_computability_has_no_pretheoretic_meaning).
narrative_ontology:cs_axiom_status(effective_computability_has_no_pretheoretic_meaning, holdable).
narrative_ontology:cs_axiom_grounding('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', effective_computability_has_no_pretheoretic_meaning, conventional).
narrative_ontology:cs_reference_frame('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', stipulative_definition_framework).
narrative_ontology:cs_drift_state('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9a0bd976-00fc-454c-bae6-e9b40e8bfa57', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_community).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_scientists).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, church_turing_thesis_as_definition).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, equivalence_of_formal_models_of_computation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the definition as a shared conceptual foundation that enables theorem-proving, model comparison, and cross-disciplinary communication without ambiguity about what counts as an algorithm. The definition costs nothing to adopt and provides a stable reference point.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_community, beneficiary,
    organized, generational, arbitrage, universal).

% Relies on the thesis as the definitional bedrock of the discipline: complexity classes, computability theory, and programming language semantics all take Turing-computability as the meaning of 'effective procedure'. No enforcement is needed; the definition is adopted because it works.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_scientists, beneficiary,
    organized, generational, arbitrage, universal).

% Analyzes the thesis from outside the mathematical practice, debating whether it is a definition, an empirical claim, or an epistemological boundary. Their work does not change the mathematical convention but maps the conceptual landscape around it.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    analytical, civilizational, analytical, universal).

% Investigates whether physical systems can compute beyond the Turing limit (hypercomputation). The definition reading treats this as a separate empirical question orthogonal to the stipulated meaning of 'effective computability', so their research program proceeds in parallel without engaging the definition.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, physical_computation_researchers, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, universally agreed-upon meaning for 'effectively computable function' so that mathematicians and computer scientists can state theorems, define complexity classes, and compare models of computation without talking past each other.
% TRANSFER_FUNCTION: Transfers nothing material; it aligns terminology and conceptual boundaries across the mathematical and computational sciences. The 'gain' is shared conceptual clarity, not a resource flow.
% ABSENT_VOICES: Hypercomputation proponents and physicists who believe the thesis makes an empirical claim about nature are not part of the mathematical community that adopts the definition. They would object to the claim that the thesis is merely a definition, but their objection concerns a different reading of the same words.
% DISAPPEARANCE_RATIONALE: If the definitional convention vanished, the mathematical literature would lose its shared referent for 'effective computability'. Theorems would need explicit model specifications, complexity classes would fragment by machine model, and cross-disciplinary communication would degrade. The mathematical community would quickly reconverge on a replacement convention (likely the same one).
% FOUNDING_PROBLEM: In the 1930s, multiple formal models of computation (lambda calculus, recursive functions, Turing machines, Post systems) were proposed independently. Mathematicians needed to know whether they captured the same intuitive notion of 'effective procedure' or whether some were strictly more powerful. The thesis resolved this by stipulating that 'effective computability' means Turing-computability, making the equivalence of models a theorem rather than an open question.
% FOUNDING_PROBLEM_CORROBORATION: The mathematical community's continued use of the thesis as a definition is attested by every computability theory textbook and the universal practice of stating results relative to Turing machines without qualification. The equivalence of models is a proven theorem (Kleene, Turing, Church), corroborated by the entire subsequent development of recursion theory and complexity theory. No external party disputes that the models are equivalent; the dispute is only over whether the thesis is a definition or something more.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.03, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__mathematical_definition_reading_tests).
:- end_tests(church_turing_thesis__mathematical_definition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.03) because the definition transfers no resources and imposes no costs — anyone can use alternative terminology or study hypercomputation; they simply won't be using the standard meaning of 'effective computability'. Suppression is minimal (0.02) because no one is prevented from proposing other definitions or investigating physical hypercomputation; the convention simply dominates mathematical discourse by consensus. Theater ratio is negligible (0.01) because the definition performs its coordination function genuinely, without performative overhead. Accessibility collapse is very high (0.92) because once the equivalence of models is understood, alternative definitions of 'effective computability' that don't match the Turing limit are simply different concepts, not viable competitors for the same term. Resistance is low (0.08) because the mathematical community converged on this definition rapidly (1930s–1950s) and has maintained it without serious challenge from within mathematics.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute near-identical types across all seats because the constraint is a genuine coordination mechanism with no asymmetric extraction. The definitional reading experiences the constraint as a Mountain-like stability (high accessibility_collapse, low resistance) but it is authored as a Rope because it is a human convention, not a natural law. The physical_claim_reading and epistemological_boundary_reading are separate constraints with different metric profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   The mathematical_community and computer_scientists are beneficiaries (d ≈ 0.05): they gain conceptual clarity and a shared foundation at zero cost. Philosophers_of_computation are observers (d = 0.5): they analyze but neither gain nor lose from the convention. Physical_computation_researchers are excluded (d ≈ 0.5 but structurally outside): they pursue a different question (physical computability) that the definition reading treats as orthogonal. No stakeholder is a payer; there is no extraction to direct.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (equivalence of formal models) remains live in the sense that the definition continues to serve its coordination function. The definition has not outlived its purpose; it is actively used as the foundation of computability theory and complexity theory. No mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_vs_empirical_claim_ambiguity,
    'Does the mathematical community''s use of the Church-Turing thesis as a definition genuinely settle the matter, or does it implicitly rely on an unexamined empirical assumption that human computation matches Turing computation?',
    'Historical analysis of the 1930s reception: did Church, Turing, Kleene, and Gödel regard the thesis as a definition, a hypothesis, or a discovered equivalence? Gödel''s later writings suggest he viewed it as an empirical claim vindicated by the equivalence of models.',
    'If the founders treated it as an empirical hypothesis, the definition reading is a later rationalization; the constraint would have a non-zero extraction from the physical_claim_reading''s perspective. If it was always a definition, the extraction remains near-zero and the Rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_vs_empirical_claim_ambiguity, conceptual, 'Whether the definition reading accurately captures the thesis''s original epistemic status or retrospectively reinterprets it.').

omega_variable(
    kernel_reading_relations,
    'What is the structural relationship between the mathematical_definition_reading and its sibling readings of the church_turing_thesis kernel?',
    'Analyze whether any single framework can hold both this reading and a sibling reading simultaneously. The definition reading and physical claim reading address different domains (convention vs. physics) and can coexist. The definition reading and epistemological boundary reading can also coexist — the boundary follows from the definition plus the fact that our formal systems are Turing-complete.',
    'If relations are correctly identified as ''coexists_with'', the kernel contains three live readings with distinct constraint types (Rope, Tangled Rope/Snare, Mountain). If any relation is ''forecloses'', the kernel structure is more polarized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations among the three readings of the Church-Turing thesis kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.01).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1950, 0.01).
narrative_ontology:measurement(chur_tr_t1970, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(chur_tr_t1990, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1990, 0.01).
narrative_ontology:measurement(chur_tr_t2010, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2010, 0.01).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.03).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(chur_be_t1970, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1970, 0.02).
narrative_ontology:measurement(chur_be_t1990, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1990, 0.03).
narrative_ontology:measurement(chur_be_t2010, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2010, 0.03).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2024, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1936, 0.02).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1950, 0.01).
narrative_ontology:measurement(chur_su_t1970, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1970, 0.01).
narrative_ontology:measurement(chur_su_t1990, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1990, 0.02).
narrative_ontology:measurement(chur_su_t2010, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2010, 0.02).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__mathematical_definition_reading, 0.02).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial 'Church-Turing thesis' into three structurally distinct constraints with different ε values and types. The mathematical_definition_reading (this story) is a Rope with ε≈0.03. The physical_claim_reading is an empirical claim about physics with higher ε (contested, extractive for hypercomputation research). The epistemological_boundary_reading marks the limit of formal provability, functioning as a Mountain from the perspective of mathematical logic but with epistemic tension from Gödelian incompleteness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
