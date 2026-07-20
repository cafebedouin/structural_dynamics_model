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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   domain: philosophy of mathematics / philosophy of computation / foundations of computer science
 *
 * SUMMARY:
 *   This constraint instantiates the physical_claim_reading of the
 *   church_turing_thesis kernel. Where the mathematical_definition_reading
 *   treats the thesis as a definitional stipulation and the
 *   epistemological_boundary_reading treats it as a limit on knowability,
 *   this reading treats it as an empirical claim about physical reality: no
 *   physical process can compute functions beyond Turing-machine
 *   computability. As an empirical generalization rather than a theorem, it
 *   is contestableâyet it operates in practice as an enforced boundary that
 *   marginalizes hypercomputation research. The claim/metric independence is
 *   deliberate: the constraint is claimed as tangled_rope because it
 *   simultaneously coordinates the field and asymmetrically extracts from
 *   dissenting researchers, while the metrics reflect moderate but rising
 *   extraction as challenges accumulate.
 *
 * KEY AGENTS:
 *   - computable_physics_gatekeepers (institutional/agenda_setter): enforce the boundary through peer review, funding, and curriculum
 *   - computable_physics_establishment (powerful/beneficiary): receives validation and resource security from the stable paradigm
 *   - hypercomputation_researchers (moderate/payer): bear career and legitimacy costs for working outside the CT boundary
 *   - philosophy_computation_analysts (analytical/observer): document the conceptual and historical structure without bearing direct costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.45).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.55).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Physical Church-Turing Thesis (Empirical Claim Reading)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy of mathematics / philosophy of computation / foundations of computer science").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, 'fa7c5375-2eba-4ebf-8910-47b936197a39').
narrative_ontology:cs_kernel_codification('fa7c5375-2eba-4ebf-8910-47b936197a39', formalized).
narrative_ontology:cs_authority_grounding('fa7c5375-2eba-4ebf-8910-47b936197a39', expertise).
narrative_ontology:cs_interpretation_layer_present('fa7c5375-2eba-4ebf-8910-47b936197a39').
narrative_ontology:cs_reading_relation('fa7c5375-2eba-4ebf-8910-47b936197a39', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa7c5375-2eba-4ebf-8910-47b936197a39', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('fa7c5375-2eba-4ebf-8910-47b936197a39', foundational, physical_computation_is_turing_limited).
narrative_ontology:cs_axiom_status(physical_computation_is_turing_limited, holdable).
narrative_ontology:cs_axiom_grounding('fa7c5375-2eba-4ebf-8910-47b936197a39', physical_computation_is_turing_limited, empirically_contingent).
narrative_ontology:cs_axiom('fa7c5375-2eba-4ebf-8910-47b936197a39', secondary, non_turing_computation_bears_extraordinary_burden).
narrative_ontology:cs_axiom_status(non_turing_computation_bears_extraordinary_burden, holdable).
narrative_ontology:cs_axiom_grounding('fa7c5375-2eba-4ebf-8910-47b936197a39', non_turing_computation_bears_extraordinary_burden, conventional).
narrative_ontology:cs_reference_frame('fa7c5375-2eba-4ebf-8910-47b936197a39', physical_turing_boundary).
narrative_ontology:cs_drift_state('fa7c5375-2eba-4ebf-8910-47b936197a39', contemporary_hypercomputation_challenges, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fa7c5375-2eba-4ebf-8910-47b936197a39', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, computable_physics_establishment).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, turing_computability_universality).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, effective_computability_physicalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the boundary between legitimate and speculative physical computation through journal editorship, peer review, funding panels, and curriculum design. They enforce the physical Church-Turing thesis as a hard constraint on what counts as computable-in-principle, rejecting or heavily scrutinizing proposals for hypercomputation as physically misguided.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, computable_physics_gatekeepers, agenda_setter,
    institutional, generational, analytical, global).

% Researchers and institutions whose work assumes Turing computability as the upper bound of physical information processing. They benefit from a stable disciplinary boundary that secures funding, validation, and epistemic authority without needing to evaluate non-standard computational models or revise foundational textbooks.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, computable_physics_establishment, beneficiary,
    powerful, generational, mobile, global).

% Explore physical models that might exceed Turing computabilityârelativistic hypercomputation, non-linear quantum mechanical models, and analog neural networks. Their papers face elevated scrutiny, their funding streams are narrower, and their career prospects in mainstream departments are precarious because their research program is treated as violating an established physical law rather than as a live empirical hypothesis.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Analyze the conceptual status of the Church-Turing thesis kernel, documenting whether it functions as definition, empirical claim, or epistemic boundary. They bear none of the constraint's direct costs but trace its logical structure and historical enforcement.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophy_computation_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, computable_physics_establishment).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the physics and computer science communities on a shared boundary for what counts as computable-in-principle by physical systems, preventing proliferation of non-comparable computational models and establishing a common vocabulary for computational complexity in physical law.
% TRANSFER_FUNCTION: Moves legitimacy, funding priority, publication access, and epistemic authority from physical models that might exceed Turing computability to the established Turing-computable paradigm; concentrates the power to define computational possibility in the mainstream foundations community.
% ABSENT_VOICES: Researchers proposing relativistic hypercomputation, non-linear quantum analog computation, and other non-Turing physical models are structurally excluded from editorial boards and major funding bodies; their objections appear in niche venues but rarely alter mainstream physical review standards.
% DISAPPEARANCE_RATIONALE: If the empirical claim were no longer enforced as a boundary, peer review and funding would shift toward evaluating non-Turing physical models on empirical merits; the demarcation between legitimate physics of computation and speculative hypercomputation would collapse and reorganize around testability rather than a priori bounds.
% FOUNDING_PROBLEM: To determine whether the abstract, mathematically defined limits of effective computability correspond to the actual upper bounds of information processing permitted by the laws of physics.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream computability theorists and physicists such as Deutsch and Wolfram attest the problem is solved. Hypercomputation researchers including Copeland, Hogarth, and Siegelmann, along with some foundational physicists, attest that the empirical question remains open because physical laws are incomplete. Corroboration from outside the benefiting establishment is provided by the hypercomputation research community itself, which bears the costs of the constraint.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).
:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint extracts career opportunity and legitimacy from hypercomputation researchers without extracting material wealth; suppression (0.55) reflects active enforcement through peer review and funding gatekeeping rather than physical coercion. Theater_ratio (0.35) captures the increasing performative dimension of CT defense as hypercomputation challenges mountâmore energy is spent reaffirming the boundary than testing it. Accessibility_collapse (0.60) indicates that while alternative models exist, they are largely inaccessible to mainstream career pathways. Resistance (0.40) is moderate because the hypercomputation community persists but remains institutionally weak. Measurements share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, the physical CT thesis is a necessary boundary condition that prevents physics from dissolving into unconstrained speculation. From the payer seat, the same structure operates as a suppressive barrier that excludes viable research programs from funding and publication. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The computable_physics_establishment is the declared beneficiary (low directionality, subsidy-like effective extraction). Hypercomputation_researchers are the declared victim (high directionality, amplified effective extraction). Gatekeepers sit near the beneficiary end due to their institutional power and analytical exit options, even though they do not directly capture the extracted rents. Philosophy_computation_analysts sit at the analytical pole with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents the mandatrophy error of treating it as a mountain (it is an empirical generalization, not a proven natural law) and also prevents mislabeling it as a pure snare (it does solve a genuine coordination problem by giving physics and computer science a shared computability boundary). The coordination component is real; the asymmetric extraction is equally real. Both must be present for the classification to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_ct_empirical_status,
    'Is the physical Church-Turing thesis actually true as a law of nature, or merely a provisional generalization from current physics?',
    'Construction of a physical hypercomputer, or a deductive proof from complete physical laws that hypercomputation is impossible.',
    'If proven true as a natural law, the constraint would recompute toward Mountain for all seats; if proven false or underdetermined, the current Tangled Rope classification is confirmed and the extraction is revealed as paradigmatic rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_ct_empirical_status, empirical, 'Whether the physical CT thesis is a genuine natural law or a contestable empirical claim').

omega_variable(
    suppression_vs_coordination_boundary,
    'Does the physical CT thesis function primarily as a necessary coordinative boundary for physics, or as a suppressive barrier to alternative research programs?',
    'Comparative analysis of acceptance rates and funding allocation for hypercomputation proposals versus mainstream computability research, controlling for methodological quality and empirical content.',
    'If the coordination component is separable from the enforcement, the constraint is better modeled as a snare with a coordination cover story; if inseparable, the tangled_rope classification is structurally robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_vs_coordination_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(chur_tr_t10, church_turing_thesis__physical_claim_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(chur_tr_t20, church_turing_thesis__physical_claim_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__physical_claim_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(chur_tr_t40, church_turing_thesis__physical_claim_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chur_be_t10, church_turing_thesis__physical_claim_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(chur_be_t20, church_turing_thesis__physical_claim_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__physical_claim_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(chur_be_t40, church_turing_thesis__physical_claim_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(chur_su_t10, church_turing_thesis__physical_claim_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(chur_su_t20, church_turing_thesis__physical_claim_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__physical_claim_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(chur_su_t40, church_turing_thesis__physical_claim_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis kernel decomposes into three structurally distinct constraints. The mathematical_definition_reading has negligible extraction (definitional). The epistemological_boundary_reading extracts epistemic authority. This physical_claim_reading extracts research opportunity from hypercomputation researchers while coordinating the computable-physics boundary. Each has a distinct epsilon, stakeholder set, and classification; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
