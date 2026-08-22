% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Church-Turing Thesis as Epistemological Boundary of Formal Knowability
 *   domain: philosophy of mathematics / foundations of computer science
 *
 * SUMMARY:
 *   This constraint instantiates the epistemological_boundary_reading of the
 *   church_turing_thesis kernel. It treats the thesis not as a mere
 *   definitional stipulation nor as an empirical physical law, but as the
 *   boundary of what can be formally proven computable: any function for
 *   which we can construct a proof of computability is Turing-computable, and
 *   this holds independently of what physical systems might achieve. The
 *   constraint methodologically excludes non-constructive computability
 *   claims and super-Turing research programs from mainstream computability
 *   theory, operating as an enforced gate on valid proof. The constraint
 *   family includes the mathematical_definition_reading (conventional
 *   stipulation) and physical_claim_reading (empirical claim about physical
 *   processes), decomposed per the Îµ-invariance principle because each
 *   reading produces a distinct beneficiary-victim structure and Îµ value.
 *
 * KEY AGENTS:
 *   - computability_gatekeepers: Agenda-setting editors and committees who enforce proof standards (institutional/arbitrage)
 *   - classical_computability_field: Beneficiary community whose paradigm is stabilized by the boundary (organized/constrained)
 *   - hypercomputation_researchers: Payers whose super-Turing research is methodologically excluded (moderate/constrained)
 *   - non_constructive_computability_claimants: Payers whose non-constructive proofs are ruled invalid for computability (moderate/mobile)
 *   - foundations_observers: Analytical observers studying the thesis meta-theoretically (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.28).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.42).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary of Formal Knowability").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy of mathematics / foundations of computer science").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '2f59d383-2dc8-42be-8e65-eb9f32955780').
narrative_ontology:cs_kernel_codification('2f59d383-2dc8-42be-8e65-eb9f32955780', formalized).
narrative_ontology:cs_authority_grounding('2f59d383-2dc8-42be-8e65-eb9f32955780', expertise).
narrative_ontology:cs_interpretation_layer_present('2f59d383-2dc8-42be-8e65-eb9f32955780').
narrative_ontology:cs_reading_relation('2f59d383-2dc8-42be-8e65-eb9f32955780', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f59d383-2dc8-42be-8e65-eb9f32955780', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('2f59d383-2dc8-42be-8e65-eb9f32955780', foundational, formal_knowability_turing_bounded).
narrative_ontology:cs_axiom_status(formal_knowability_turing_bounded, holdable).
narrative_ontology:cs_axiom_grounding('2f59d383-2dc8-42be-8e65-eb9f32955780', formal_knowability_turing_bounded, conventional).
narrative_ontology:cs_axiom('2f59d383-2dc8-42be-8e65-eb9f32955780', secondary, non_constructive_computability_excluded).
narrative_ontology:cs_axiom_status(non_constructive_computability_excluded, holdable).
narrative_ontology:cs_axiom_grounding('2f59d383-2dc8-42be-8e65-eb9f32955780', non_constructive_computability_excluded, conventional).
narrative_ontology:cs_reference_frame('2f59d383-2dc8-42be-8e65-eb9f32955780', classical_formal_provability).
narrative_ontology:cs_drift_state('2f59d383-2dc8-42be-8e65-eb9f32955780', contemporary_hypercomputation_challenges, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2f59d383-2dc8-42be-8e65-eb9f32955780', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, classical_computability_field).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, constructive_computability_canon).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Editors of flagship logic and computability journals, conference program committees, and senior researchers who enforce the boundary of what counts as a valid computability proof. They reject submissions relying on non-constructive or super-Turing claims to establish computability, and they shape graduate curricula to reflect the Turing-computable paradigm.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computability_gatekeepers, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% The broad community of researchers working within Turing-computable frameworks. They benefit from stable disciplinary boundaries, shared formal standards, established textbooks, and funding streams that flow to problems framed as Turing-computable. Their professional identity is constituted by the boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, classical_computability_field, beneficiary,
    organized, generational, constrained, universal).

% Researchers developing super-Turing models such as oracle machines, infinite-time Turing machines, and physical hypercomputation. Their work is methodologically excluded from mainstream computability venues because it exceeds the epistemological boundary, forcing them into fringe journals, interdisciplinary spaces, or non-academic research paths.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, universal).

% Mathematicians who make existence claims about computable functions via non-constructive means. Their proofs are ruled invalid for establishing computability within the dominant framework, though they may continue to publish in other areas of mathematics where non-constructive methods remain fully accepted.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants, payer,
    moderate, biographical, mobile, universal).

% Philosophers and logicians who study the Church-Turing thesis meta-theoretically. They analyze the boundary, its historical evolution, and its methodological function without being directly coordinated or extracted by it, though their own discourse depends on the controversy remaining live.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, foundations_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, shared boundary for what counts as formally provable computability, enabling the field of computability theory to operate with agreed-upon standards of evidence, proof, and disciplinary identity.
% TRANSFER_FUNCTION: Moves academic legitimacy, publication access, research funding, and curriculum space from non-constructive and super-Turing approaches to constructive Turing-level computability research.
% ABSENT_VOICES: Hypercomputation researchers and non-constructive mathematicians are structurally underrepresented in mainstream computability venues; their objections to the boundary are largely heard only in specialized forums, philosophy journals, or interdisciplinary conferences outside the field's core.
% DISAPPEARANCE_RATIONALE: If the epistemological boundary vanished overnight, mainstream computability journals would need to broaden their scopes to admit non-constructive and super-Turing results, graduate curricula would reorganize around competing foundations, and the classical field's methodological monopoly would dissolve into a plurality of computability concepts.
% FOUNDING_PROBLEM: The early-twentieth-century crisis of defining effective calculability with sufficient precision to distinguish genuine algorithms from vague intuitive procedures, and to settle the Entscheidungsproblem.
% FOUNDING_PROBLEM_CORROBORATION: Historians of logic and philosophical observers outside the computability mainstream (e.g., Soare, Hodges) document the original need to formalize effective calculability. Hypercomputation researchers and critics from constructive mathematics attest from outside the beneficiary set that the founding problem has been solved and the boundary now functions as methodological exclusion rather than necessary hygiene.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.28, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.28) is low-to-moderate: the boundary is internally stable and coherent within proof-theoretic frameworks, but it systematically redirects legitimacy from excluded research programs. Suppression (0.42) reflects steady academic gatekeeping through peer review, hiring, and curriculum design rather than dramatic coercion. Theater ratio (0.35) captures the increasing performative maintenance of the boundary as foundational dogma even as alternative models gain technical sophistication. Accessibility collapse (0.75) is high because, once the framework is accepted, alternatives become nearly unthinkable within the discipline. Resistance (0.40) comes from marginalized researchers and philosophers who contest the boundary. The metrics and claim are authored independently: the claim is tangled_rope because the constraint genuinely coordinates the field (enabling shared standards) while asymmetrically extracting from excluded approaches.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (computability gatekeepers) experiences the constraint as necessary methodological hygiene that prevents the field from dissolving into vague intuition. The payer seats (hypercomputation researchers, non-constructive claimants) experience it as an artificial ceiling that excludes valid mathematics. The beneficiary seat (classical field) experiences it as productive coordination that stabilizes their professional identity. The observer seat sees the structural asymmetry: the coordination function is real, but its enforcement produces a captured methodological monoculture.
 *
 * DIRECTIONALITY LOGIC:
 *   Gatekeepers have low directionality (near the beneficiary end) because they control the constraint and their authority is validated by it. The classical field has moderately low directionality: they benefit from stable boundaries but are also identity-locked into the paradigm. Hypercomputation researchers have high directionality (near the target end) because the constraint directly extracts their publication access and epistemic legitimacy. Non-constructive claimants have intermediate directionality: their mobility into other mathematical domains moderates their exposure relative to the trapped hypercomputation researchers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdefining effective calculabilityâwas genuine and is arguably solved. The constraint's persistence as a hard boundary beyond that solution risks mandatrophy. However, because the boundary is contested (founding_problem_status: contested) and the coordination function remains live (the field still needs shared standards to avoid fragmentation), the constraint has not fully atrophied into a piton. It is a tangled rope: the coordination is genuine, the extraction is genuine, and the distinction is preserved by the fact that alternatives are structurally excluded rather than merely disadvantaged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (epistemological_boundary_reading) of the contested kernel church_turing_thesis; how does its classification change under sibling readings?',
    'Comparative analysis of the three compiled constraint stories for this kernel, examining divergence in beneficiary-victim structures and Îµ values.',
    'If the mathematical_definition_reading dominates, this constraint dissolves into a convention with negligible extraction; if the physical_claim_reading dominates, the victim set and scope shift entirely to physical processes and engineers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel reading identity and sibling divergence').

omega_variable(
    boundary_naturality_or_convention,
    'Is the epistemological boundary of formal knowability a discovered logical limit or a convention maintained by disciplinary practice?',
    'Historical analysis of alternative formalizations of computability and their acceptance or rejection by the proof-theoretic community.',
    'If conventional, the constraint''s extraction is higher and its classification as tangled_rope strengthens; if a discovered limit, it trends toward mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_naturality_or_convention, conceptual, 'Whether the boundary is natural or constructed').

omega_variable(
    suppression_mechanism_in_academic_gatekeeping,
    'Is the exclusion of non-constructive computability claims enforced by structural gatekeeping (peer review, funding panels, hiring committees) or by internalized methodological consensus?',
    'Survey of journal acceptance rates for hypercomputation papers and interview data on referee attitudes toward non-constructive computability proofs.',
    'If structural gatekeeping dominates, suppression is higher and the constraint is more snare-like; if internalized consensus dominates, it operates as diffuse coordination with lower effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_in_academic_gatekeeping, empirical, 'Structural vs internalized enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(chur_tr_t15, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(chur_tr_t45, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(chur_tr_t75, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 75, 0.33).
narrative_ontology:measurement(chur_tr_t90, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 90, 0.35).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(chur_be_t15, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(chur_be_t45, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 45, 0.35).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement(chur_be_t75, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 75, 0.3).
narrative_ontology:measurement(chur_be_t90, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 90, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(chur_su_t15, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(chur_su_t45, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(chur_su_t75, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 75, 0.45).
narrative_ontology:measurement(chur_su_t90, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 90, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, identity_coordination).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, physical_claim_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the church_turing_thesis kernel, decomposed per the Îµ-invariance principle because each reading produces a distinct beneficiary-victim structure, Îµ value, and coordination type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
