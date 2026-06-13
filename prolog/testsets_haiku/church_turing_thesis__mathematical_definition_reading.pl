% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis as Mathematical Definition of Effective Computability
 *   domain: philosophy_of_mathematics/foundations_of_computer_science
 *
 * SUMMARY:
 *   The Church-Turing thesis asserts that the mathematical models of
 *   effective computability proposed by Church (lambda calculus), Turing
 *   (machines), Gödel and Herbrand (recursive functions), and Post (canonical
 *   systems) are equivalent — they compute exactly the same set of functions.
 *   This story interprets the thesis as a *mathematical definition*
 *   stipulating what we shall *mean* by 'effective computability,' true by
 *   convention rather than empirically testable. Under this reading, the
 *   thesis is not an assertion about nature (whether physical processes can
 *   exceed Turing bounds) nor a boundary claim (whether formal proof and
 *   computation have the same scope), but simply a linguistic convention: we
 *   use 'effectively computable' to mean 'computable by a Turing machine (or
 *   any of the equivalent models).' This is a low-extractiveness coordination
 *   constraint — it serves mathematical clarity without creating victims or
 *   suppressing genuine alternatives (alternatives would be semantic choices
 *   about terminology, not mathematical facts being hidden). The constraint
 *   type is Rope: genuine coordination of mathematical terminology across
 *   independent formalisms, with minimal asymmetry or coercion.
 *
 * KEY AGENTS:
 *   - Mathematical logic community: uses the definition in proofs and textbooks; benefits from unified terminology
 *   - Computer science foundations: grounds complexity theory and decidability results on the definition
 *   - Proof theory: depends on the definition to separate algorithmically decidable from undecidable problems
 *   - Quantum computing researchers: excluded from consensus, argue the definition should extend to quantum models
 *   - Hypercomputation researchers: excluded from consensus, develop models beyond the definition's scope
 *   - Empirical physicists: analytical observers, measure what physical systems can compute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.08).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Mathematical Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/foundations_of_computer_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, 'a1bbbab8-f153-4929-8878-269f6aa8c4f3').
narrative_ontology:cs_kernel_codification('a1bbbab8-f153-4929-8878-269f6aa8c4f3', formalized).
narrative_ontology:cs_authority_grounding('a1bbbab8-f153-4929-8878-269f6aa8c4f3', expertise).
narrative_ontology:cs_interpretation_layer_present('a1bbbab8-f153-4929-8878-269f6aa8c4f3').
narrative_ontology:cs_reading_relation('a1bbbab8-f153-4929-8878-269f6aa8c4f3', church_turing_thesis__physical_claim_reading, forecloses).
narrative_ontology:cs_reading_relation('a1bbbab8-f153-4929-8878-269f6aa8c4f3', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('a1bbbab8-f153-4929-8878-269f6aa8c4f3', foundational, effective_computability_is_definitional).
narrative_ontology:cs_axiom_status(effective_computability_is_definitional, holdable).
narrative_ontology:cs_axiom_grounding('a1bbbab8-f153-4929-8878-269f6aa8c4f3', effective_computability_is_definitional, conventional).
narrative_ontology:cs_axiom('a1bbbab8-f153-4929-8878-269f6aa8c4f3', secondary, equivalence_of_models_is_mathematical_fact).
narrative_ontology:cs_axiom_status(equivalence_of_models_is_mathematical_fact, holdable).
narrative_ontology:cs_axiom_grounding('a1bbbab8-f153-4929-8878-269f6aa8c4f3', equivalence_of_models_is_mathematical_fact, empirically_contingent).
narrative_ontology:cs_reference_frame('a1bbbab8-f153-4929-8878-269f6aa8c4f3', church_turing_definition_as_stipulation).
narrative_ontology:cs_drift_state('a1bbbab8-f153-4929-8878-269f6aa8c4f3', contemporary_quantum_hypercomputation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a1bbbab8-f153-4929-8878-269f6aa8c4f3', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_logic_community).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_science_foundations).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, proof_theory).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, formalist_ontology_of_computation).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, church_thesis_as_definitional_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the Church-Turing thesis as a settled definition of computability in proofs, publications, and pedagogy. The definition allows uniform treatment of diverse computational models (lambda calculus, Turing machines, Post systems, recursive functions) as equivalent formulations of the same concept. Benefits from the stability this provides for discourse and proof construction.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_logic_community, beneficiary,
    institutional, generational, mobile, global).

% Builds theoretical results on the assumption that 'computable' means 'Turing-computable by definition.' This unified terminology allows results to port across different computational models without qualification. The definition is a coordination device that prevents fragmentation of foundational theory.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_science_foundations, beneficiary,
    institutional, generational, mobile, global).

% Depends on the thesis to separate decidable from undecidable problems. The definition anchors what 'algorithmic solvability' means in formal systems, enabling precise theorems about the limits of proof and computation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, proof_theory, beneficiary,
    moderate, biographical, constrained, global).

% Would argue that the definition's scope should extend to quantum models of computation, or that quantum advantage demands a reconceptualization of what 'effective computability' covers. They are outside the foundational consensus that treats the thesis as definitional rather than empirical.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, quantum_computing_researchers, excluded,
    organized, biographical, mobile, global).

% Investigate mathematical models that purport to compute beyond Turing limits (oracle machines, infinite-time Turing machines, blum-shub-smale machines). From this reading of the definition, their results are not about 'computability' but about formal properties of non-computable models — their objection would be that the definition is too narrow.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, hypercomputation_researchers, excluded,
    moderate, biographical, mobile, global).

% Measure what physical systems can compute and whether physical limits constrain computation. They take no position on whether the thesis is a definition or an empirical claim; they report what happens in nature.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, empirical_physicists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified definition of 'effective computability' across multiple independent mathematical formalisms (lambda calculus, Turing machines, recursive functions, Post systems, Markov algorithms). Solves the coordination problem of whether these models compute the same class of functions, and if so, what that class is called.
% TRANSFER_FUNCTION: No net transfer — the constraint is constitutive. What flows is clarification: the thesis transfers the burden of proof from 'show that your model is computable' to 'show that your model is not equivalent to Turing machines.' This shifts where the definitional authority sits (with Turing machines as the canonical model) rather than moving resources.
% ABSENT_VOICES: Researchers in hypercomputation and quantum computing whose models produce results outside classical Turing bounds are structurally excluded from the definitional consensus. They dispute whether the thesis captures the right domain and argue for extended or alternative definitions of computability. Their objection — that the definition is too narrow for physical or mathematical phenomena — is not heard in foundational texts that treat the thesis as settled.
% DISAPPEARANCE_RATIONALE: If the Church-Turing thesis as a definition disappeared, mathematical logic would retain the fact that lambda calculus, Turing machines, and recursive functions are equivalent in their expressive power. Logicians would introduce that equivalence by pointing to the models themselves rather than invoking a named thesis. The coordination problem is solved by the models' mathematical relationship, not by the social consensus around the thesis name. The thesis vanishing would not change what mathematicians can prove about computability — it would only force them to refer to the equivalence more explicitly each time they need it.
% FOUNDING_PROBLEM: In the 1930s, 'effective computability' had no precise mathematical definition, despite being central to Gödel's incompleteness results and Church's work on decidability. Different researchers proposed different formalizations (Church's lambda calculus, Turing's machines, Gödel-Herbrand recursive functions, Post's canonical systems). The founding problem was: are these formalizations equivalent, and if so, which one defines 'effective computability'?
% FOUNDING_PROBLEM_CORROBORATION: Mathematical logic and recursion theory textbooks (Rogers, Sipser, Soare, Enderton) from outside the original thesis-authoring community confirm that the equivalence of these models remains mathematically important and the thesis remains the standard way to name the unified concept. The original founders (Church, Turing, Gödel, Post) all attested to needing a precise definition; contemporary foundations scholars attest it is still useful.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_unchanged).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).

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
 *   Extractiveness is very low (0.08 by 2026) because the constraint is constitutive — it does not extract resources or asymmetrically benefit one party over another. All participants (logic, CS, proof theory) gain equal benefit from the unified definition. Suppression is minimal (0.02) because the definition is not maintained by coercion or the exclusion of competitors — it is maintained by consensus and pedagogy. Theater ratio is negligible (0.05) because the constraint's function (alignment on terminology) is its actual operation — there is no significant gap between what the constraint claims to do and what it does. Accessibility_collapse is low (0.15) because alternatives remain formally available: researchers could use different definitions, call lambda-computable functions by a different name, treat Turing machines and recursive functions as separate concepts. The alternatives are not chosen because the consensus definition is more useful, not because they are impossible. Resistance is low (0.12) because the definition faces no real opposition — even those who dispute the reading (quantum researchers, hypercomputation researchers) do not argue that the definition itself is wrong, only that it may be too narrow or miss important phenomena. The measurements show a slight rise over the 90-year interval as the definition became more embedded in curricula and textbooks (theater and suppression rising from near-zero to minimal levels), but the constraint never becomes substantially extractive because its fundamentally coordinative nature is stable.
 *
 * PERSPECTIVAL GAP:
 *   From the mathematical logic community's perspective, the constraint is pure coordination — a settled naming convention that lets them work efficiently. From the quantum computing researcher's perspective, the constraint is exclusionary — their models and results are named 'non-computable,' which marginalizes them from the central discourse. From the empirical physicist's perspective (analytical observer), the constraint is a piece of mathematical culture with no bearing on what physical systems can do. The engine computes each seat's perception of the constraint from the stakeholder data, power, and exit options — the mathematical logic community's high institutional power and global scope, combined with beneficiary status and mobile exit, produces one classification; the quantum researcher's organized power, biographical horizon, and excluded status produces another. The divergence is the signal the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   All stakeholders are near the symmetric end of directionality (d ≈ 0.5) because all benefit equally from the coordination without being targeted by extraction. The mathematical logic community, computer science foundations, and proof theory are listed as beneficiaries because they depend on and use the definition — they collect clarity and utility without paying a cost. There are no victims because a mathematical definition cannot be violated, and the constraint produces no concentrated gains that are subtracted from anyone's allocation. Quantum computing and hypercomputation researchers are excluded (not victimized) — they are outside the consensus, not harmed by it. The definition simply does not describe their models, which is a feature of the definition, not an injustice done to them. This is why there is no victim set: a definitional constraint has no victims, only exclusions and inclusions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not suffer mandatrophy because its founding problem (establishing a unified definition of computability) remains live and the constraint directly solves it. The thesis is not an atrophied function maintained theatrically — it is actively used in every introduction to the theory of computation, every recursion theory course, every complexity theory paper that needs to define 'decidable' or 'computable.' There is no gap between the constraint's founding purpose and its current operation. If anything, the constraint has strengthened over time as foundational computer science became more formalized and the need for a unified definition more pressing. The measurement series shows stable operation, not drift toward theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_vs_empirical_reading_boundary,
    'Is the Church-Turing thesis genuinely a mathematical definition (true by convention, making the reading''s core premise sound), or does it make a hidden empirical claim about what is physically computable (supporting the physical_claim_reading)?',
    'Examine the foundational texts (Church 1936, Turing 1937) and contemporary foundational logic textbooks for whether they treat the thesis as a stipulation of terminology or as an assertion about nature. If contemporary physics discovers a physical process that computes beyond Turing limits, the empirical reading would be falsified while the definition-reading would remain unchanged (the definition would simply not describe physical computation).',
    'If the thesis is genuinely definitional, the mathematical_definition_reading is sound and the constraint is low-extractiveness coordination. If empirical claims are hidden in the definition, the physical_claim_reading forecloses this reading — they cannot coexist in the same framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_vs_empirical_reading_boundary, conceptual, 'Whether the thesis is definitional or makes a hidden physical/empirical commitment').

omega_variable(
    hypercomputation_as_definitional_extension,
    'Do hypercomputation models (oracle machines, infinite-time Turing machines, blum-shub-smale machines) represent genuine extensions of the concept of ''effective computability'' or merely formal models that compute non-computable functions?',
    'The definition-reading entails that these models compute functions that are not computable by the definition; researchers in hypercomputation argue the definition should be extended to cover these models. This is a meta-mathematical question: should we expand the *definition* of effective computability to include these models, or keep the definition fixed and treat hypercomputation as a separate formal concept?',
    'If hypercomputation models represent a legitimate extension of computability, then the definition-reading of the thesis is incomplete or parochial, and the epistemological_boundary_reading might better capture the foundational role of the thesis. If the definition is rightly restricted to classical Turing computability, then hypercomputation models are interesting but definitionally outside the scope of ''effective computability.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypercomputation_as_definitional_extension, conceptual, 'Whether the definition of effective computability should be extended to cover hypercomputational models').

omega_variable(
    quantum_advantage_and_definitional_scope,
    'Do quantum computers compute ''effectively'' in the sense captured by the Church-Turing thesis, or does quantum advantage demonstrate that the definition needs revision?',
    'Quantum computers can factor and solve certain problems faster than known classical algorithms, but all quantum-computable functions are Turing-computable (they compute the same set of yes/no answers, just faster). This is consistent with the definition-reading: quantum machines are *more efficient* but not more *expressive*. The question is whether ''effective computability'' should include efficiency classes, which would require redefining the concept.',
    'If ''effective'' remains means ''computable in principle (regardless of time/space cost),'' the definition-reading holds unchanged. If ''effective'' is reinterpreted to include efficiency, the definition itself requires revision and the thesis becomes a historical artifact rather than the binding definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_advantage_and_definitional_scope, conceptual, 'Whether quantum computational advantage requires redefining effective computability').

omega_variable(
    internalized_suppression_of_alternative_definitions,
    'To what extent does the thesis''s status as a ''definition'' rest on active philosophical enforcement (textbooks, curricula, peer review) versus being a self-evident formalization that needs no defense?',
    'Survey foundational mathematics and computer science curricula to measure how the thesis is presented (as a definitional stipulation vs. as an empirical-seeming claim). Examine refusal patterns in peer review: are alternative definitions of computability rejected as incoherent, or as non-standard?',
    'If the definition is self-evident, suppression measures are minimal and the constraint is genuinely low-extractiveness coordination. If the definition requires active pedagogical and editorial enforcement to exclude alternatives, then suppression is higher and the reading is less a pure definition and more a defended claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_alternative_definitions, empirical, 'Whether the thesis''s status as definition requires active suppression of alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.0).
narrative_ontology:measurement_basis(chur_tr_t1936, projected).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement_basis(chur_tr_t1950, observed).
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement_basis(chur_tr_t1980, observed).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement_basis(chur_tr_t2000, observed).
narrative_ontology:measurement(chur_tr_t2015, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2015, 0.05).
narrative_ontology:measurement_basis(chur_tr_t2015, observed).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2026, 0.05).
narrative_ontology:measurement_basis(chur_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.0).
narrative_ontology:measurement_basis(chur_be_t1936, projected).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1950, 0.04).
narrative_ontology:measurement_basis(chur_be_t1950, observed).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1980, 0.06).
narrative_ontology:measurement_basis(chur_be_t1980, observed).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2000, 0.07).
narrative_ontology:measurement_basis(chur_be_t2000, observed).
narrative_ontology:measurement(chur_be_t2015, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2015, 0.08).
narrative_ontology:measurement_basis(chur_be_t2015, observed).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2026, 0.08).
narrative_ontology:measurement_basis(chur_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1936, 0.0).
narrative_ontology:measurement_basis(chur_su_t1936, projected).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1950, 0.01).
narrative_ontology:measurement_basis(chur_su_t1950, observed).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1980, 0.015).
narrative_ontology:measurement_basis(chur_su_t1980, observed).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2000, 0.018).
narrative_ontology:measurement_basis(chur_su_t2000, observed).
narrative_ontology:measurement(chur_su_t2015, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2015, 0.02).
narrative_ontology:measurement_basis(chur_su_t2015, observed).
narrative_ontology:measurement(chur_su_t2026, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2026, 0.02).
narrative_ontology:measurement_basis(chur_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__mathematical_definition_reading, 0.04).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis is a contested kernel admitting multiple readings, each instantiating a structurally distinct constraint. This story (mathematical_definition_reading) treats the thesis as a mathematical definition — coordination with negligible extraction. The physical_claim_reading interprets the same kernel as an empirical claim about nature — higher extractiveness and contested legitimacy. The epistemological_boundary_reading interprets the kernel as a boundary-marking claim about formal provability — intermediate extractiveness and different victim structure. Each reading is a separate constraint story with its own ε, beneficiary structure, and type. They are linked via network.affects_constraints to enable the corpus to track how the same contested kernel produces different constraint instantiations depending on how the parties interpret it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
