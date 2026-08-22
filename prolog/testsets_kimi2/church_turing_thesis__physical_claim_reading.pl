% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Physical Church-Turing Thesis (Empirical Claim Reading)
 *   domain: philosophical/foundational_computer_science
 *
 * SUMMARY:
 *   The physical Church-Turing thesis reading treats the conjecture as an
 *   empirical claim about the universe: no physical process can compute
 *   functions beyond Turing-machine computability. This reading functions as
 *   a constraint on scientific inquiry, coordinating mainstream physics and
 *   computer science around a shared computability boundary while
 *   simultaneously suppressing hypercomputation research programs and
 *   non-standard computational models. As one reading of a contested kernel,
 *   it must be disambiguated from the mathematical definition reading (which
 *   treats CT as a stipulative definition with negligible extraction) and the
 *   epistemological boundary reading (which concerns knowability rather than
 *   physical possibility).
 *
 * KEY AGENTS:
 *   - Mainstream computability establishment (agenda_setter/beneficiary, institutional/constrained): enforces the physical thesis as a boundary condition through peer review, funding, and curricula; benefits from paradigm stability.
 *   - Hypercomputation researchers (payer, moderate/constrained): pursue non-Turing computation; bear professional marginalization and funding exclusion.
 *   - Philosophy of computation scholars (observer, analytical/analytical): analyze the kernel's multiple readings and their structural consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.58).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.62).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Physical Church-Turing Thesis (Empirical Claim Reading)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophical/foundational_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, 'e57476c2-2882-4b83-b715-5e339cb7672e').
narrative_ontology:cs_kernel_codification('e57476c2-2882-4b83-b715-5e339cb7672e', formalized).
narrative_ontology:cs_authority_grounding('e57476c2-2882-4b83-b715-5e339cb7672e', expertise).
narrative_ontology:cs_interpretation_layer_present('e57476c2-2882-4b83-b715-5e339cb7672e').
narrative_ontology:cs_reading_relation('e57476c2-2882-4b83-b715-5e339cb7672e', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('e57476c2-2882-4b83-b715-5e339cb7672e', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('e57476c2-2882-4b83-b715-5e339cb7672e', foundational, physical_processes_computationally_bounded).
narrative_ontology:cs_axiom_status(physical_processes_computationally_bounded, holdable).
narrative_ontology:cs_axiom_grounding('e57476c2-2882-4b83-b715-5e339cb7672e', physical_processes_computationally_bounded, empirically_contingent).
narrative_ontology:cs_reference_frame('e57476c2-2882-4b83-b715-5e339cb7672e', classical_effective_computability_framework).
narrative_ontology:cs_drift_state('e57476c2-2882-4b83-b715-5e339cb7672e', post_quantum_computing_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e57476c2-2882-4b83-b715-5e339cb7672e', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, mainstream_computability_establishment).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers peer review, curriculum standards, and funding priorities that treat the physical Church-Turing thesis as a boundary condition for legitimate research. Benefits from paradigm stability, consolidated funding flows, and epistemic authority over what counts as physically possible computation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mainstream_computability_establishment, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, mainstream_computability_establishment, beneficiary).

% Develop and experimentally investigate non-Turing computational models. Bear the costs of systematic publication rejection, funding denial, and professional marginalization because their research program is treated as physically impossible under the dominant empirical thesis.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Analyze the epistemic status of the Church-Turing kernel and its multiple readings. They map how the physical claim reading borrows authority from the mathematical definition reading and trace the structural consequences for scientific inquiry.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophy_of_computation_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, mainstream_computability_establishment).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified empirical boundary between physically possible computation and unphysical speculation, allowing physicists and computer scientists to share a common framework for what 'computation' means in nature.
% TRANSFER_FUNCTION: Moves research funding, journal space, hiring lines, and epistemic credibility from non-standard and hypercomputation research programs to mainstream computability and quantum computing programs that accept Turing bounds.
% ABSENT_VOICES: Hypercomputation researchers are formally present in academia but functionally excluded from top-tier venues and funding bodies; they would object to the physical thesis's treatment as settled empirical fact but are not seated in the committees that enforce it.
% DISAPPEARANCE_RATIONALE: If the physical Church-Turing thesis vanished as a binding constraint, funding allocations would shift toward non-Turing physical models, curricula would revise the interface between physics and computability, and the epistemic stigma on hypercomputation would dissolve.
% FOUNDING_PROBLEM: The early twentieth century lacked a rigorous boundary between 'effectively calculable' and unrigorous speculation; the thesis was built to anchor computability in mathematical and physical rigor.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream computability historians and foundational computer scientists attest the problem is still live; hypercomputation researchers and some quantum foundations scholars attest the founding problem is solved and the arrangement now blocks legitimate inquiry; independent historians of science provide corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because the constraint diverts significant intellectual and financial resources from alternative research programs but does not extract material wealth. Suppression is moderate-high (0.62) because the thesis persists through active gatekeeping in journals and funding bodies, not merely through evident consensus. Theater ratio is low (0.25) because the mainstream community sincerely believes the boundary is physically real, though a fraction of defense is performative boundary-policing. Accessibility collapse is high (0.70) because once the physical thesis is accepted, hypercomputation appears conceptually confused rather than empirically open. Resistance is moderate (0.40) because a small, marginalized community of hypercomputation researchers continues to challenge the boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the mainstream establishment seat, the constraint is a rope or mountainâa necessary empirical boundary preventing unphysical speculation. From the hypercomputation researcher seat, it is a snare or tangled ropeâan enforced extraction of their research viability based on an unproven empirical hypothesis. The engine computes this divergence from the structural data; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainstream computability establishment sits near the beneficiary end: they administer the constraint and collect paradigm stability and resource concentration. Hypercomputation researchers sit near the target end: they bear the costs of exclusion and funding diversion. The directionality is sharpened by the establishment's constrained exit (identity fusion with the paradigm) versus the researchers' constrained but non-trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve the problem of unrigorous calculability claims in the early twentieth century. That founding problem is now contested: mainstream scholars argue physical boundaries remain necessary, while critics argue the empirical claim has hardened into an obstruction. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags potential mandatrophy: the arrangement persists beyond its original justification and would rearrange the world if removed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_truth_of_physical_ct,
    'Is the physical Church-Turing thesis true as a matter of physical law, or is it an empirically falsifiable hypothesis that may already be challenged by quantum or other non-classical processes?',
    'Experimental demonstration of physical hypercomputation or a rigorous physical proof of its impossibility.',
    'If true, the constraint''s suppression may be reclassified as necessary boundary-keeping; if false, it is a false summit or snare blocking legitimate research.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_truth_of_physical_ct, empirical, 'Empirical truth status of the physical computability bound').

omega_variable(
    kernel_reading_contamination,
    'Does the conflation of the physical claim reading with the mathematical definition reading inflate the authority of the physical claim beyond its empirical warrant?',
    'Discourse analysis of how the label ''Church-Turing thesis'' is deployed in physics and computer science arguments.',
    'If the mathematical reading''s authority is borrowed to suppress empirical challenges, effective extraction is higher than the physical claim''s empirical status warrants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contamination, conceptual, 'Authority contamination across kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 85).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(chur_tr_t17, church_turing_thesis__physical_claim_reading, theater_ratio, 17, 0.1).
narrative_ontology:measurement(chur_tr_t34, church_turing_thesis__physical_claim_reading, theater_ratio, 34, 0.15).
narrative_ontology:measurement(chur_tr_t51, church_turing_thesis__physical_claim_reading, theater_ratio, 51, 0.2).
narrative_ontology:measurement(chur_tr_t68, church_turing_thesis__physical_claim_reading, theater_ratio, 68, 0.23).
narrative_ontology:measurement(chur_tr_t85, church_turing_thesis__physical_claim_reading, theater_ratio, 85, 0.25).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(chur_be_t17, church_turing_thesis__physical_claim_reading, base_extractiveness, 17, 0.25).
narrative_ontology:measurement(chur_be_t34, church_turing_thesis__physical_claim_reading, base_extractiveness, 34, 0.4).
narrative_ontology:measurement(chur_be_t51, church_turing_thesis__physical_claim_reading, base_extractiveness, 51, 0.5).
narrative_ontology:measurement(chur_be_t68, church_turing_thesis__physical_claim_reading, base_extractiveness, 68, 0.55).
narrative_ontology:measurement(chur_be_t85, church_turing_thesis__physical_claim_reading, base_extractiveness, 85, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(chur_su_t17, church_turing_thesis__physical_claim_reading, suppression_requirement, 17, 0.28).
narrative_ontology:measurement(chur_su_t34, church_turing_thesis__physical_claim_reading, suppression_requirement, 34, 0.48).
narrative_ontology:measurement(chur_su_t51, church_turing_thesis__physical_claim_reading, suppression_requirement, 51, 0.58).
narrative_ontology:measurement(chur_su_t68, church_turing_thesis__physical_claim_reading, suppression_requirement, 68, 0.61).
narrative_ontology:measurement(chur_su_t85, church_turing_thesis__physical_claim_reading, suppression_requirement, 85, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Church-Turing thesis' conflates three structurally distinct constraints. This story isolates the empirical physical claim (moderate extraction, active suppression). The mathematical definition reading has near-zero extraction and no victims. The epistemological boundary reading has different scope and victim profile. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
