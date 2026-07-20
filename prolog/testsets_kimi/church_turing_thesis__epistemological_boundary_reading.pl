% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Church-Turing Thesis â Epistemological Boundary Reading
 *   domain: philosophy_of_mathematics/philosophy_of_computation
 *
 * SUMMARY:
 *   This constraint story instantiates the epistemological_boundary_reading
 *   of the church_turing_thesis kernel. The thesis is read here not as a mere
 *   mathematical definition nor as an empirical physical claim, but as a
 *   boundary on formally knowable computation: functions we can prove
 *   computable are exactly the Turing-computable functions, and this boundary
 *   holds independently of what is physically possible. The kernel conflates
 *   three structurally distinct claims; this reading isolates the
 *   methodological exclusion that bars non-constructive computability claims
 *   from epistemic legitimacy in mainstream computability theory. Sibling
 *   readings: mathematical_definition_reading (stipulative convention) and
 *   physical_claim_reading (empirical assertion about the universe).
 *
 * KEY AGENTS:
 *   - Classical computability theorists (agenda_setter, institutional): administer the boundary through peer review and curriculum
 *   - Constructivist mathematicians (beneficiary, organized): gain vindication for constructive proof norms
 *   - Non-constructivist mathematicians (payer, organized): bear exclusion of their existence-proof methods
 *   - Hypercomputation researchers (excluded, moderate): claims ruled out of order by the epistemological boundary
 *   - Philosophers of computation (observer, analytical): evaluate the boundary's epistemic status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.35).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.45).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis â Epistemological Boundary Reading").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/philosophy_of_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '5bd15b2d-0115-47ea-ba6e-78d97a6c8d91').
narrative_ontology:cs_kernel_codification('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', formalized).
narrative_ontology:cs_authority_grounding('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', expertise).
narrative_ontology:cs_interpretation_layer_present('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91').
narrative_ontology:cs_reading_relation('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', foundational, provable_computability_requires_constructive_demonstration).
narrative_ontology:cs_axiom_status(provable_computability_requires_constructive_demonstration, holdable).
narrative_ontology:cs_axiom_grounding('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', provable_computability_requires_constructive_demonstration, conventional).
narrative_ontology:cs_axiom('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', foundational, epistemic_boundary_independent_of_physical_possibility).
narrative_ontology:cs_axiom_status(epistemic_boundary_independent_of_physical_possibility, holdable).
narrative_ontology:cs_axiom_grounding('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', epistemic_boundary_independent_of_physical_possibility, conventional).
narrative_ontology:cs_reference_frame('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', formal_provable_computability_boundary).
narrative_ontology:cs_drift_state('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', contemporary_hypercomputation_discourse, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('5bd15b2d-0115-47ea-ba6e-78d97a6c8d91', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, classical_computability_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructivist_mathematicians).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructivist_mathematicians).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, constructivist_demonstration_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the boundary of formal computability through journal editorship, refereeing standards, and graduate curricula. They enforce the requirement that computability claims must be grounded in Turing-machine-level construction or equivalent formal demonstration, maintaining the field's methodological coherence.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, classical_computability_theorists, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the thesis because it vindicates the constructivist commitment that mathematical existence requires explicit construction. Their methodological preference for provable algorithms is reinforced by the field's epistemological boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructivist_mathematicians, beneficiary,
    organized, generational, constrained, global).

% Bear the cost of the boundary: their non-constructive existence proofs for computable functions are ruled epistemologically invalid within mainstream computability theory, marginalizing their methodological approach and restricting their access to legitimacy in the field.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructivist_mathematicians, payer,
    organized, generational, constrained, global).

% Explore physical and theoretical models of computation beyond Turing limits. Their epistemological claims are structurally excluded from mainstream computability venues because the boundary declares formal knowability independent of physical possibility; they are heard as curiosities rather than legitimate challengers.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers, excluded,
    moderate, biographical, constrained, global).

% Analyze the epistemological status of the boundary, distinguishing between the mathematical theorem, the methodological convention, and the physical claim. They evaluate whether the exclusion of non-constructive methods is a necessary feature of formal knowability or a contingent disciplinary choice.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_computation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a precise, intersubjectively verifiable boundary for what counts as formally provable computability, coordinating mathematical discourse by settling proof standards and preventing methodological anarchy in computability theory.
% TRANSFER_FUNCTION: Moves epistemic authority and publication legitimacy from non-constructive mathematical claims to constructive, Turing-machine-demonstrable proofs, transferring disciplinary standing from non-constructivist to constructivist approaches.
% ABSENT_VOICES: Hypercomputation researchers and non-constructivist mathematicians are formally present in the academic conversation but their epistemological claims are excluded from legitimacy; they would object to the boundary's independence from physical possibility if the forums treated their objections as orderable business.
% DISAPPEARANCE_RATIONALE: If the epistemological boundary vanished, constructivists would lose a key vindication while non-constructivists would gain legitimacy for existence-only computability proofs; the field would face methodological pluralism. Some would view this as liberation, others as a catastrophic loss of clarity and rigor.
% FOUNDING_PROBLEM: Before the thesis, 'computability' lacked a precise extension: there was no agreed standard for when a function could be said to be effectively calculable, leading to conceptual confusion and disputes over what counted as a solution to the Entscheidungsproblem and related questions.
% FOUNDING_PROBLEM_CORROBORATION: Historians and philosophers of mathematics (e.g., Soare, Sieg, Copeland) attest the pre-1936 foundational confusion from outside the benefiting parties; the simultaneous, independent formulations by Church, Turing, and Post corroborate that the problem was genuine and widely recognized.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.35, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.35) because the boundary genuinely coordinates by providing a crisp proof standard, but it asymmetrically extracts by excluding non-constructive methods that are valid in broader mathematical practice. Suppression (0.45) reflects active enforcement through peer review, curriculum gatekeeping, and citation norms. Theater (0.25) is relatively low because the boundary is functional, though some maintenance is ritual. Accessibility collapse is high (0.70) because once the proof framework is accepted, non-constructive alternatives appear as non-mathematical rather than alternative mathematics. Resistance (0.40) comes from non-constructivist traditions and hypercomputation researchers. The temporal series shows gradual institutionalization of the boundary from 0.20 to 0.35 over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (classical computability theorists), the boundary is a necessary methodological achievement that prevents confusion and secures the field; from the payer seat (non-constructivist mathematicians), it is an arbitrary restriction that illegitimately excludes well-formed mathematical techniques from epistemic legitimacy. The engine will compute different per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical computability theorists are structural beneficiaries and agenda-setters (low d); their professional identity and institutional control are reinforced by the boundary. Constructivist mathematicians are beneficiaries (low d) because the boundary vindicates their methodological commitments. Non-constructivist mathematicians are targets (high d) because their proof methods are excluded from legitimacy. Hypercomputation researchers sit near full target (very high d) because their epistemological claims are structurally ruled out of order. Philosophers of computation occupy the analytical seat (d analytically determined).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve the genuine coordination problem of defining computability precisely (founding problem: pre-1936 conceptual confusion over effective calculability). That problem remains live, so mandatrophy is not declared. The slight rise in theater ratio reflects institutional sedimentation rather than functional atrophy; the boundary still coordinates the field, even if it also extracts through methodological exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_implicity_in_epistemic_boundary,
    'Does the epistemological boundary tacitly import physical assumptions about finite human cognitive capacities, despite claiming independence from physical possibility?',
    'Philosophical analysis of whether the proof-theoretic framework''s notion of ''formal provability'' implicitly assumes finitary, physically realizable symbol manipulation.',
    'If physical assumptions are tacitly imported, the boundary collapses into the physical claim reading; if genuinely independent, the epistemological boundary stands as a distinct methodological constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_implicity_in_epistemic_boundary, conceptual, 'Whether the epistemic boundary is genuinely independent of physical possibility.').

omega_variable(
    constructive_exclusion_necessity,
    'Is the exclusion of non-constructive computability proofs a necessary feature of formal knowability, or a contingent methodological preference of the classical computability tradition?',
    'Comparative study of logical frameworks that admit non-constructive computability proofs and whether they yield a coherent, stable epistemology of computation.',
    'If contingent, the extraction is higher than necessary and the constraint functions partly as disciplinary gatekeeping; if necessary, the extraction is the price of epistemic clarity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructive_exclusion_necessity, conceptual, 'Whether non-constructive exclusion is necessary or contingent.').

omega_variable(
    kernel_reading_ambiguity,
    'Which reading of the Church-Turing thesis kernel â epistemological boundary, mathematical definition, or physical claim â best captures the structural role of the constraint in contemporary discourse?',
    'Corpus-level analysis of how practitioners deploy the thesis: whether they treat it as a stipulative definition, a provable boundary of knowability, or an empirical claim about physical reality.',
    'Determines which constraint story''s classification and metrics apply; shifts the beneficiary/victim structure and the naturalness claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Structural ambiguity between sibling readings of the Church-Turing kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 88).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctthesis_ebr_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ctthesis_ebr_tr_t15, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(ctthesis_ebr_tr_t30, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(ctthesis_ebr_tr_t45, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(ctthesis_ebr_tr_t60, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(ctthesis_ebr_tr_t75, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 75, 0.23).
narrative_ontology:measurement(ctthesis_ebr_tr_t88, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 88, 0.25).

% Extraction over time
narrative_ontology:measurement(ctthesis_ebr_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ctthesis_ebr_be_t15, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(ctthesis_ebr_be_t30, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(ctthesis_ebr_be_t45, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 45, 0.3).
narrative_ontology:measurement(ctthesis_ebr_be_t60, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement(ctthesis_ebr_be_t75, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 75, 0.34).
narrative_ontology:measurement(ctthesis_ebr_be_t88, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 88, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(church_turing_thesis__epistemological_boundary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, identity_coordination).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, physical_claim_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the church_turing_thesis kernel. The kernel decomposes into three structurally distinct claims: the epistemological boundary (this file), the mathematical definition, and the physical claim. Each has a different epsilon, beneficiary structure, and classification. They share a natural-language label but not a structural identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
