% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__physical_claim_reading, []).

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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis: Physical Computability Claim
 *   domain: philosophy_of_computation/foundations_of_physics
 *
 * SUMMARY:
 *   This constraint represents the 'physical claim' reading of the
 *   Church-Turing Thesis: the assertion that no physical process can compute
 *   functions beyond Turing-machine computability. This reading is distinct
 *   from the mathematical definition or epistemological boundary
 *   interpretations. It functions as a Tangled Rope because it coordinates
 *   research within established limits (benefiting mainstream CS and physics)
 *   while actively suppressing or marginalizing research into
 *   hypercomputation or physical systems that might exceed these limits
 *   (victims). Its persistence relies on active enforcement through peer
 *   review, funding decisions, and the framing of 'possible' physics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.55).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.65).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis: Physical Computability Claim").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_computation/foundations_of_physics").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '22d9d3a9-9da3-421a-9273-1c2ae469e108').
narrative_ontology:cs_kernel_codification('22d9d3a9-9da3-421a-9273-1c2ae469e108', formalized).
narrative_ontology:cs_authority_grounding('22d9d3a9-9da3-421a-9273-1c2ae469e108', expertise).
narrative_ontology:cs_interpretation_layer_present('22d9d3a9-9da3-421a-9273-1c2ae469e108').
narrative_ontology:cs_reading_relation('22d9d3a9-9da3-421a-9273-1c2ae469e108', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('22d9d3a9-9da3-421a-9273-1c2ae469e108', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('22d9d3a9-9da3-421a-9273-1c2ae469e108', foundational, physical_processes_are_turing_computable).
narrative_ontology:cs_axiom_status(physical_processes_are_turing_computable, holdable).
narrative_ontology:cs_axiom_grounding('22d9d3a9-9da3-421a-9273-1c2ae469e108', physical_processes_are_turing_computable, empirically_contingent).
narrative_ontology:cs_reference_frame('22d9d3a9-9da3-421a-9273-1c2ae469e108', turing_machine_as_universal_physical_model).
narrative_ontology:cs_drift_state('22d9d3a9-9da3-421a-9273-1c2ae469e108', contemporary_quantum_computing_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('22d9d3a9-9da3-421a-9273-1c2ae469e108', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_computer_science).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, theoretical_physics).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the stability and predictive power of the Church-Turing Thesis as a physical claim, which grounds the limits of what can be built and studied. It sets research agendas and allocates funding based on this understanding. Exit would mean a paradigm shift.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_computer_science, agenda_setter,
    institutional, generational, constrained, global).

% Relies on the physical Church-Turing Thesis to define the computational capabilities of physical systems, influencing theories of cosmology, quantum gravity, and information theory. A challenge to the thesis would require fundamental revisions to many physical models.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, theoretical_physics, beneficiary,
    institutional, generational, constrained, universal).

% Their research aims to identify or construct physical systems that can compute beyond Turing limits. They face significant skepticism, funding challenges, and professional marginalization due to the prevailing acceptance of the physical Church-Turing Thesis. Their identity is tied to challenging this boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, identity_locked, global).

% Claim to have built quantum computers that perform tasks intractable for classical Turing machines. While not necessarily 'hypercomputational' in the theoretical sense, their work pushes against the practical and theoretical limits implied by the physical Church-Turing Thesis, attracting scrutiny and requiring careful framing to avoid direct contradiction.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, payer,
    organized, immediate, constrained, global).

% Analyze the conceptual foundations and implications of the Church-Turing Thesis, including its various interpretations and empirical status. They observe the debates and contribute to the meta-discussion without directly benefiting or paying from its enforcement.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, empirically grounded understanding of the limits of physical computation, allowing computer scientists and physicists to coordinate research efforts and build technologies within a common theoretical framework.
% TRANSFER_FUNCTION: Transfers legitimacy and resources to research programs operating within Turing-computable limits, while implicitly or explicitly withholding them from research into hypercomputation or physical processes claimed to exceed these limits.
% ABSENT_VOICES: Future researchers who might discover or engineer physical processes capable of hypercomputation are currently excluded from the mainstream discourse and resource allocation, as their potential findings are deemed impossible by the prevailing physical interpretation of the thesis.
% DISAPPEARANCE_RATIONALE: If the physical Church-Turing Thesis were definitively disproven overnight, it would trigger a profound paradigm shift across computer science, physics, and philosophy. New computational models would emerge, funding for hypercomputation research would surge, and our understanding of the universe's fundamental computational capabilities would be rewritten.
% FOUNDING_PROBLEM: To establish a rigorous, universal definition of 'computable function' that aligns with both mathematical intuition and the capabilities of physical machines, providing a foundational limit for the nascent fields of computer science and theoretical physics.
% FOUNDING_PROBLEM_CORROBORATION: The problem of defining the limits of computation remains live, as attested by ongoing debates in theoretical computer science and physics. Mainstream researchers corroborate its continued relevance, while hypercomputation researchers contest the 'solution' offered by the physical claim, arguing it prematurely closes off empirical inquiry.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because while it provides a stable framework, it imposes costs on alternative research paths. Suppression is high (0.65) due to the active gatekeeping against hypercomputation research, which is often dismissed as 'unphysical' or 'impossible' rather than empirically investigated. Accessibility collapse is high (0.7) because the thesis, if true, fundamentally limits what can be built or discovered. Resistance is moderate (0.4) from hypercomputation researchers, but they are a minority. Theater ratio is low (0.1) as the claim is genuinely believed to be true by its beneficiaries, not merely performed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mainstream computer science and theoretical physics, the physical Church-Turing Thesis is a foundational principle that enables progress. From the perspective of hypercomputation researchers, it is a restrictive dogma that stifles innovation and empirical inquiry into the true limits of computation. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream computer science and theoretical physics are beneficiaries (low d) as the thesis provides a stable, productive framework for their work. Hypercomputation researchers and quantum supremacy claimants are targets (high d) as their work is either directly suppressed or viewed with skepticism due to the thesis. Philosophers of computation are observers (analytical d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_falsifiability,
    'Is the physical Church-Turing Thesis truly empirically falsifiable, or does it function as a regulative principle that shapes what counts as ''physical computation''?',
    'Discovery of a physical process that demonstrably computes a non-Turing function, or a philosophical argument demonstrating its unfalsifiability within current scientific paradigms.',
    'If unfalsifiable, its classification shifts closer to a Mountain (conceptual limit) or a Snare (enforced dogma) rather than an empirical Tangled Rope. If falsifiable and falsified, it becomes a Scaffold that has served its purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_falsifiability, conceptual, 'Ambiguity regarding the empirical status and falsifiability of the physical Church-Turing Thesis.').

omega_variable(
    hypercomputation_evidence_threshold,
    'What level of empirical evidence would be required for the scientific community to accept a physical hypercomputer, and is this threshold currently achievable?',
    'A clear, reproducible experimental demonstration of a physical system solving a known non-Turing computable problem, accepted by a broad consensus of physicists and computer scientists.',
    'If the threshold is impossibly high, the constraint''s suppression is effectively absolute, pushing it towards a Snare. If achievable, the constraint is a genuine empirical claim, and its classification depends on the outcome of the empirical test.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hypercomputation_evidence_threshold, empirical, 'The practical and epistemic barriers to demonstrating hypercomputation.').

omega_variable(
    quantum_supremacy_implications,
    'Do current or future quantum supremacy claims genuinely challenge the physical Church-Turing Thesis, or do they merely demonstrate practical computational advantage within Turing limits?',
    'Formal proof that a quantum algorithm performs a function provably non-Turing computable, or a consensus among theoretical computer scientists that quantum supremacy remains within the Turing paradigm.',
    'If quantum supremacy implies non-Turing computability, the constraint''s extractiveness and suppression would be severely challenged, potentially leading to a reclassification as a Scaffold or even a Piton. If not, the constraint remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_supremacy_implications, empirical, 'The relationship between quantum supremacy and the physical Church-Turing Thesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__physical_claim_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__physical_claim_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(chur_tr_t1980, church_turing_thesis__physical_claim_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__physical_claim_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__physical_claim_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.3).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__physical_claim_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(chur_be_t1980, church_turing_thesis__physical_claim_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__physical_claim_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__physical_claim_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.4).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__physical_claim_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(chur_su_t1980, church_turing_thesis__physical_claim_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__physical_claim_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__physical_claim_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Church-Turing Thesis kernel. This 'physical claim' reading asserts an empirical limit on the universe's computational power, influencing how the mathematical and epistemological aspects are understood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
