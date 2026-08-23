% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Church-Turing Thesis as Epistemological Boundary of Provable Computability
 *   domain: philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   The epistemological boundary reading of the Church-Turing thesis holds
 *   that the thesis marks the limit of what can be *proven* computable within
 *   formal systems: a function is computable iff there exists a constructive
 *   proof formalizable in a Turing-equivalent system. This reading is
 *   distinct from the mathematical definition reading (which treats the
 *   thesis as a stipulative convention) and the physical claim reading (which
 *   treats it as an empirical hypothesis about the universe). The constraint
 *   operates as a methodological norm in computability theory — it
 *   coordinates practice by defining valid proof methods, but asymmetrically
 *   excludes non-constructive approaches, making it a tangled rope. The
 *   measurement series shows rising extractiveness and suppression from 1936
 *   to 2024 as the field consolidated around proof-theoretic standards and
 *   marginalized classical methods.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.35).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.45).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary of Provable Computability").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/philosophy_of_computation/foundations_of_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e').
narrative_ontology:cs_kernel_codification('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', distributed).
narrative_ontology:cs_authority_grounding('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', expertise).
narrative_ontology:cs_interpretation_layer_present('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e').
narrative_ontology:cs_reading_relation('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', foundational, computability_requires_constructive_proof).
narrative_ontology:cs_axiom_status(computability_requires_constructive_proof, holdable).
narrative_ontology:cs_axiom_grounding('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', computability_requires_constructive_proof, conventional).
narrative_ontology:cs_axiom('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', foundational, turing_equivalence_exhausts_formal_proof).
narrative_ontology:cs_axiom_status(turing_equivalence_exhausts_formal_proof, holdable).
narrative_ontology:cs_axiom_grounding('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', turing_equivalence_exhausts_formal_proof, empirically_contingent).
narrative_ontology:cs_reference_frame('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', formal_proof_theoretic_framework).
narrative_ontology:cs_drift_state('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', contemporary_computability_theory, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8ae2c634-f7b5-4ae2-a6cb-1448f0aaa93e', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, proof_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, classical_recursion_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematicians).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, turing_equivalence_of_formal_systems).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, formal_proof_captures_effective_computation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their methodological commitment to constructive proof aligns with the thesis as an epistemological boundary. The constraint validates their approach and marginalizes non-constructive existence proofs in computability. They can move between subfields but their professional identity is tied to constructive methods.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians, beneficiary,
    organized, biographical, mobile, global).

% Set the standards for what counts as a valid computability proof in journals, conferences, and curricula. The epistemological reading gives their proof-theoretic methods privileged status. They administer the constraint through peer review and editorial control. Exit is easy — they dominate the field.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proof_theorists, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, proof_theorists, agenda_setter).

% Use classical (non-constructive) methods in recursion theory — e.g., existence proofs via law of excluded middle, non-constructive definability. The epistemological boundary reading treats their methods as not establishing 'genuine' computability. They bear the cost of marginalization: their results are seen as 'less constructive' or merely 'classical'. Exit means adopting constructive methods, which requires retraining and abandoning established research programs.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, classical_recursion_theorists, payer,
    organized, biographical, constrained, global).

% Mathematicians in adjacent fields (set theory, model theory, reverse mathematics) who use non-constructive methods to make computability claims. The constraint excludes their style of result from 'computability proper'. They pay in reduced recognition and citation within core computability theory. Exit is constrained by the depth of non-constructive methods in their home fields.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematicians, payer,
    moderate, biographical, constrained, global).

% Investigate whether physical systems can compute beyond Turing machines (hypercomputation, analog computation, quantum gravity models). This reading explicitly brackets physical possibility ('regardless of physical possibility'), rendering their research questions invisible to the constraint's framework. They cannot exit the exclusion without changing the reading's premises.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, physical_computation_researchers, excluded,
    organized, biographical, trapped, global).

% Analyze the thesis from outside the mathematical practice — asking what the thesis *is* (definition, empirical claim, epistemological boundary). They see the full structure of the kernel contest. Their exit is analytical: they can adopt any reading as an object of study.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, proof_theorists).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mathematical practice by establishing a shared standard for what counts as a computability proof: a proof must be formalizable in a Turing-equivalent system. This prevents proliferation of incommensurable computability notions and enables cumulative progress in recursion theory.
% TRANSFER_FUNCTION: Moves epistemic authority and publication access from non-constructive methods to constructive/formalizable methods. Classical existence proofs are transferred to the status of 'non-constructive' or 'merely classical' — they do not establish computability proper.
% ABSENT_VOICES: Hypercomputation theorists and physicists proposing super-Turing physical models are structurally excluded by the 'regardless of physical possibility' clause. They would object that the boundary is arbitrarily drawn to protect mathematical practice from empirical challenge. Their absence is built into the reading's premise.
% DISAPPEARANCE_RATIONALE: If the epistemological boundary vanished, computability theory would lose its shared standard of proof. Non-constructive existence proofs would gain equal standing with constructive ones. The field would fragment into competing notions of computability, and the Turing-equivalence consensus that enables cumulative results (e.g., relative computability, degree theory) would dissolve.
% FOUNDING_PROBLEM: Early computability theory (1930s) faced a proliferation of formalisms (lambda calculus, recursive functions, Turing machines, Post systems) with no agreed criterion for which captured 'effective computability'. The thesis provided a unifying boundary: all adequate formalisms are Turing-equivalent, and any proof of computability must be formalizable in one of them.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical records (Gödel's 1946 remark that Turing's analysis gave 'a precise and unquestionably adequate definition' of mechanical procedure) and by contemporary proof theorists who maintain that the thesis's role as a proof standard remains essential. No corroboration from outside the beneficiary set is needed because the problem is internal to mathematical practice — the coordination function is acknowledged by all parties in the field, including classical recursion theorists who accept Turing equivalence while contesting the epistemological reading's exclusion of non-constructive methods.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low-moderate (0.35) because the constraint's primary function is coordination (shared proof standard) but it extracts epistemic authority from non-constructive methods. Suppression (0.45) reflects active enforcement through peer review, journal standards, and curriculum design — non-constructive computability claims are rejected or reclassified. Theater is low (0.15) because the coordination function is genuine and the enforcement is not primarily performative. Accessibility collapse (0.60) is moderate: alternative computability notions exist (e.g., in reverse mathematics, hypercomputation theory) but are confined to subfields. Resistance (0.50) is significant: classical recursion theorists and hypercomputation researchers actively contest the boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the proof theorist's seat, the constraint is a rope — genuine coordination enabling cumulative science. From the classical recursion theorist's seat, it is a snare — their established methods are excluded by a methodological norm they did not choose and cannot easily exit. The engine computes this divergence from the structural data: same power level (organized), but different exit_options (arbitrage vs constrained) and roles (beneficiary/agenda_setter vs payer).
 *
 * DIRECTIONALITY LOGIC:
 *   Proof theorists and constructive mathematicians are beneficiaries (d near 0.15) — they set the agenda and their methods are privileged. Classical recursion theorists and non-constructive mathematicians are payers (d near 0.80) — their methods are excluded from 'computability proper' and they bear the cost of marginalization. Physical computation researchers are excluded (d not computed as they are not coordinated by the constraint) — their questions are bracketed out by the 'regardless of physical possibility' clause. Philosophers of computation are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unifying computability formalisms) remains live — new formalisms (quantum, biological, analog) still need evaluation against the Turing standard. The constraint has not atrophied; its coordination function is active. However, the epistemological reading's exclusion of non-constructive methods has intensified over time (rising extractiveness/suppression), suggesting the coordination function has acquired an extractive layer. This is precisely the tangled rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the epistemological boundary reading a distinct constraint from the mathematical definition reading, or do they collapse into a single methodological practice?',
    'Test whether the two readings diverge on concrete proof-theoretic questions: e.g., does a classical existence proof of a computable function (without constructive witness) count as establishing computability? The definition reading says yes (it''s a convention about ''effective computability''); the epistemological reading says no (no constructive proof = not proven computable). If mathematical practice treats them differently, they are distinct constraints.',
    'If they collapse, this story duplicates the mathematical_definition_reading story. If distinct, the ε-invariance principle requires separate stories — which this is. The measured ε=0.35 reflects the epistemological reading''s specific exclusion of non-constructive proofs, not the definition reading''s near-zero extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the epistemological boundary and mathematical definition readings are structurally distinct constraints.').

omega_variable(
    physical_possibility_bracketing,
    'Does the ''regardless of physical possibility'' clause function as a legitimate methodological bracket, or as a suppression mechanism that insulates the constraint from empirical challenge?',
    'Track whether hypercomputation proposals are engaged on their mathematical merits or dismissed a priori by appeal to the thesis. If the latter, the bracket operates as suppression. If the former, it is a genuine methodological separation.',
    'If suppression, the constraint''s effective extraction from physical_computation_researchers is higher than measured (they are not just excluded but actively blocked). This would increase suppression toward snare territory. If legitimate bracket, the low extraction from this group is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(physical_possibility_bracketing, empirical, 'Whether the physical-possibility bracket is methodological hygiene or epistemic suppression.').

omega_variable(
    constructive_vs_classical_boundary_stability,
    'Is the boundary between constructive and classical computability proofs stable, or does it shift with advances in proof theory (e.g., proof mining, realizability, reverse mathematics)?',
    'Monitor whether results previously classified as ''non-constructive'' are reclassified as constructive via proof-theoretic analysis (e.g., Kohlenbach''s proof mining extracting bounds from classical proofs). If the boundary erodes, the constraint''s victim set shrinks and extraction decreases.',
    'If the boundary erodes, extractiveness trends toward 0 and the constraint becomes a rope. If it hardens, extractiveness rises and the constraint trends toward snare. Current measurement (0.35 stable since ~2010) suggests a plateau.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructive_vs_classical_boundary_stability, empirical, 'Whether the constructive/classical boundary in computability theory is fixed or porous to proof-theoretic advances.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctt_epist_bound_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(ctt_epist_bound_tr_t1950, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(ctt_epist_bound_tr_t1970, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(ctt_epist_bound_tr_t1990, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(ctt_epist_bound_tr_t2010, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(ctt_epist_bound_tr_t2024, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(ctt_epist_bound_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.2).
narrative_ontology:measurement(ctt_epist_bound_be_t1950, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(ctt_epist_bound_be_t1970, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(ctt_epist_bound_be_t1990, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(ctt_epist_bound_be_t2010, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(ctt_epist_bound_be_t2024, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ctt_epist_bound_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.3).
narrative_ontology:measurement(ctt_epist_bound_su_t1950, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(ctt_epist_bound_su_t1970, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(ctt_epist_bound_su_t1990, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1990, 0.43).
narrative_ontology:measurement(ctt_epist_bound_su_t2010, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(ctt_epist_bound_su_t2024, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__epistemological_boundary_reading, 0.02).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, hypercomputation_proposals).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, reverse_mathematics_computability).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the 'Church-Turing thesis' kernel. The epistemological_boundary_reading treats the thesis as a methodological norm for computability proofs (tangled rope, ε=0.35). The mathematical_definition_reading treats it as a stipulative definition (rope, ε≈0.05). The physical_claim_reading treats it as an empirical hypothesis (mountain or snare depending on physics, ε contested). They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__epistemological_boundary_reading, organized, 0.15).
constraint_indexing:directionality_override(church_turing_thesis__epistemological_boundary_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
