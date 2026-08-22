% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: Narrow Linking Permissive Reading of GPL Derivative Work Trigger
 *   domain: legal/technological
 *
 * SUMMARY:
 *   This constraint story captures the narrow linking permissive reading of
 *   the GPL's derivative work trigger — the interpretation that dynamic
 *   linking (and even static linking in some formulations) constitutes mere
 *   aggregation, not derivation, such that only actual modifications to
 *   GPL-covered code trigger the copyleft source-disclosure obligation. This
 *   reading is the de facto standard in Linux kernel development (via the
 *   'syscall exception' and module interface practice), Android's userspace,
 *   and most corporate GPL compliance programs. It functions as a
 *   coordination mechanism enabling a vast software ecosystem where GPL
 *   libraries serve as infrastructure for proprietary applications. However,
 *   it structurally transfers the GPL's intended propagation of user freedoms
 *   from end users to proprietary vendors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.15).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.12).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Narrow Linking Permissive Reading of GPL Derivative Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "legal/technological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '55409223-bb8c-45c4-8796-c13e2d3b63ff').
narrative_ontology:cs_kernel_codification('55409223-bb8c-45c4-8796-c13e2d3b63ff', formalized).
narrative_ontology:cs_authority_grounding('55409223-bb8c-45c4-8796-c13e2d3b63ff', lineage).
narrative_ontology:cs_interpretation_layer_present('55409223-bb8c-45c4-8796-c13e2d3b63ff').
narrative_ontology:cs_reading_relation('55409223-bb8c-45c4-8796-c13e2d3b63ff', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('55409223-bb8c-45c4-8796-c13e2d3b63ff', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('55409223-bb8c-45c4-8796-c13e2d3b63ff', foundational, linking_is_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('55409223-bb8c-45c4-8796-c13e2d3b63ff', linking_is_not_derivation, conventional).
narrative_ontology:cs_axiom('55409223-bb8c-45c4-8796-c13e2d3b63ff', foundational, derivative_work_requires_creative_modification).
narrative_ontology:cs_axiom_status(derivative_work_requires_creative_modification, holdable).
narrative_ontology:cs_axiom_grounding('55409223-bb8c-45c4-8796-c13e2d3b63ff', derivative_work_requires_creative_modification, conventional).
narrative_ontology:cs_reference_frame('55409223-bb8c-45c4-8796-c13e2d3b63ff', gplv2_literal_text_reading).
narrative_ontology:cs_drift_state('55409223-bb8c-45c4-8796-c13e2d3b63ff', contemporary_ecosystem_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55409223-bb8c-45c4-8796-c13e2d3b63ff', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, application_developers_using_gpl_libraries).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_proprietary_modules).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_ecosystem_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, application_developers_using_gpl_libraries).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, linking_is_mere_aggregation).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, derivative_work_requires_creative_modification).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, module_boundary_is_api_not_linking_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ship closed-source modules that dynamically link to GPL libraries without triggering copyleft obligations. They invest in proprietary differentiation layers while relying on GPL code for core functionality. Their exit is trivial — they could rewrite or replace the GPL dependency but choose not to because the reading permits the combination.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_module_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Build applications incorporating GPL libraries via dynamic linking, treating the library as a black-box component. They gain development velocity and ecosystem access without source-disclosure obligations for their application code. They bear indirect cost if the reading is overturned — relicensing or rewriting would be required.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, application_developers_using_gpl_libraries, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, application_developers_using_gpl_libraries, payer).

% Use software that combines GPL libraries with proprietary modules under this reading. They lose the GPL's guarantee of source availability, modification rights, and repair freedom for the proprietary portions. Exit means abandoning the software or switching to fully-free alternatives, which may not exist for their use case.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_proprietary_modules, payer,
    powerless, biographical, constrained, global).

% Contribute to GPL codebases expecting copyleft propagation. This reading allows their contributions to be incorporated into mixed proprietary/GPL systems without the proprietary parts reciprocating. Their exit is constrained by sunk investment in the ecosystem and ideological commitment to copyleft principles.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_ecosystem_participants, payer,
    organized, generational, constrained, global).

% Maintain the GPL license text and advocate for broad copyleft interpretation. They are structurally excluded from adjudicating this reading — courts and corporate counsel determine its acceptance. They would object that the reading defeats the license's propagation purpose, but their objection carries no binding authority.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_and_copyleft_advocates, excluded,
    institutional, generational, analytical, global).

% Adjudicate whether dynamic linking creates a derivative work under copyright law and the GPL. Their rulings instantiate or reject this reading. They bear no direct cost or benefit from the outcome but hold the structural power to settle the constraint's enforcement.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, court_and_legal_interpretive_authority, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables modular software composition across license boundaries: developers can combine GPL libraries with independently licensed components without negotiating license compatibility for each combination, reducing transaction costs in the software supply chain.
% TRANSFER_FUNCTION: Transfers source-availability rights and modification freedoms from end users and the GPL ecosystem to proprietary module vendors and application developers, who retain control over their proprietary layers while benefiting from GPL code.
% ABSENT_VOICES: End users of proprietary modules and downstream recipients in the software supply chain would object to the loss of source-availability guarantees but are not represented in licensing negotiations or court proceedings that shape this reading.
% DISAPPEARANCE_RATIONALE: If this reading were rejected overnight and the broad copyleft reading prevailed, proprietary modules linking to GPL libraries would need to be relicensed under GPL, open-sourced, or replaced — restructuring the business models of major vendors and the architecture of countless software systems.
% FOUNDING_PROBLEM: Early GPL enforcement uncertainty around dynamic linking created legal risk for developers who wanted to use GPL libraries as components in larger systems. The narrow reading emerged to clarify that mere linking — without modification of the GPL code — does not trigger copyleft, enabling a stable ecosystem of GPL libraries used by proprietary applications.
% FOUNDING_PROBLEM_CORROBORATION: FSF and copyleft advocates attest the founding problem was never just legal clarity but ensuring user freedom propagates through all combination mechanisms; they argue the narrow reading solves a problem the GPL was not designed to solve. Corporate legal departments and major OS vendors (Linux kernel, Android) corroborate the narrow reading as the practical basis for their GPL library usage, but they are beneficiaries of the reading.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).
:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because the constraint primarily enables coordination — it permits combinations that would otherwise be legally uncertain. The extraction that exists is the transfer of source-availability rights from users to vendors, which is real but limited in scope compared to the coordination value created. Suppression is low (0.12) because no active enforcement prevents the alternative reading; the constraint persists through legal precedent, industry practice, and the absence of decisive court rulings against it. Theater ratio is very low (0.08) because the constraint's operation is genuinely functional — it solves a real modularity problem — though the FSF's continued advocacy for the broad reading creates performative tension. Accessibility collapse is moderate (0.25) because alternative readings (broad copyleft, interface boundary) remain live and legally plausible. Resistance is moderate (0.45) because the FSF and copyleft advocates actively contest this reading, but they lack enforcement power.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary vendor seat, this reading is a rope — genuine coordination enabling efficient software composition. From the end-user seat, it is a snare — the GPL's protection is structurally circumvented. From the GPL contributor seat, it is a tangled rope — their contributions enable a coordination function that extracts from the very freedom they intended to propagate. The engine computes this divergence from the stakeholder power/exit declarations; the claimed_type (rope) reflects the dominant coordination frame but does not adjudicate the seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors and application developers are structural beneficiaries (d near 0.0-0.2) — they gain license certainty and avoid copyleft obligations. End users and GPL ecosystem participants are structural payers (d near 0.7-0.9) — they lose freedoms the GPL was designed to guarantee. FSF advocates are excluded (analytical seat) — they contest the reading but cannot enforce their interpretation. Courts are agenda-setters — they could settle the constraint but have largely declined to rule definitively, leaving the reading as de facto law.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal clarity for library usage) remains live but contested — the narrow reading solved the clarity problem but at the cost of the propagation problem. The constraint has not resolved its mandatrophy because the GPL's stated purpose (user freedom propagation) is frustrated by the reading's operation, yet the reading persists because it enables the ecosystem that sustains the GPL codebase itself. This is not a piton (no theatrical maintenance) but a stable tangled_rope-at-some-seats / rope-at-others equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'This constraint is one reading (narrow_linking_permissive_reading) of the contested kernel gpl_derivative_work_trigger. How does the committer structure — the kernel''s three readings and their structural relationships — affect the classification of this specific reading?',
    'Structural analysis of the kernel''s readings as separate constraint stories linked via network.affects_constraints. The narrow reading''s classification depends on whether its coordination function is genuine (rope) or whether the propagation-frustration constitutes extraction (tangled_rope/snare at payer seats).',
    'If the kernel structure shows this reading forecloses the broad reading''s propagation function without substituting an alternative freedom-preserving mechanism, the extraction at payer seats is structural, not incidental. This would shift the reading''s effective type toward tangled_rope at payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Commitment-system framing of the GPL derivative work trigger as a kernel with three readings; this reading''s structural relationship to its siblings.').

omega_variable(
    linking_boundary_doctrinal_stability,
    'Is the ''linking is not derivation'' boundary doctrinally stable under copyright law, or does it depend on jurisdictional variance and unsettled case law?',
    'Track court rulings on dynamic linking and derivative works across major jurisdictions (US, EU, Japan). A definitive ruling against the narrow reading would collapse this constraint and force reclassification of the GPL library ecosystem.',
    'If the boundary is legally unstable, the constraint''s low suppression score is misleading — the constraint persists only because courts have not ruled, not because the legal theory is sound. A ruling against it would instantly increase suppression to near-1 as vendors scramble for compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linking_boundary_doctrinal_stability, empirical, 'Legal stability of the linking/aggregation distinction under copyright law.').

omega_variable(
    propagation_frustration_as_extraction,
    'Does the frustration of the GPL''s propagation goal constitute extraction from end users and contributors, or is it merely the absence of a positive right that the GPL never successfully secured in this context?',
    'Comparative analysis of GPL library ecosystems under narrow vs. broad readings: measure source-availability outcomes, user modification rates, and vendor contribution behavior. If the narrow reading demonstrably reduces user freedom relative to a counterfactual broad-reading world, the frustration is extractive.',
    'If propagation frustration is extractive, this reading is tangled_rope (coordination + asymmetric extraction) rather than rope. The engine computes this via effective extraction at payer seats; the omega documents the structural ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(propagation_frustration_as_extraction, conceptual, 'Whether the GPL''s unmet propagation goal in the narrow reading counts as extraction from its intended beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 1991, 0.02).
narrative_ontology:measurement(gpl__tr_t2000, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(gpl__tr_t2007, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2007, 0.06).
narrative_ontology:measurement(gpl__tr_t2015, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2015, 0.07).
narrative_ontology:measurement(gpl__tr_t2020, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(gpl__tr_t2026, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 1991, 0.05).
narrative_ontology:measurement(gpl__be_t2000, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(gpl__be_t2007, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2007, 0.12).
narrative_ontology:measurement(gpl__be_t2015, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2015, 0.14).
narrative_ontology:measurement(gpl__be_t2020, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(gpl__be_t2026, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2026, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 1991, 0.05).
narrative_ontology:measurement(gpl__su_t2000, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(gpl__su_t2007, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2007, 0.1).
narrative_ontology:measurement(gpl__su_t2015, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2015, 0.11).
narrative_ontology:measurement(gpl__su_t2020, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2020, 0.12).
narrative_ontology:measurement(gpl__su_t2026, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, linux_kernel_syscall_exception).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, android_gpl_userspace_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the gpl_derivative_work_trigger kernel. The broad_copyleft_reading asserts linking creates derivative works; the interface_boundary_reading asserts clean API boundaries permit aggregation. This reading asserts linking per se is aggregation; only modification triggers obligations. The three readings have different ε values (this reading: ε≈0.15; broad reading: ε≈0.65 for proprietary vendors; interface reading: ε≈0.30) because they structure the beneficiary/victim relationship differently. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, institutional, 0.15).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, powerful, 0.1).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, moderate, 0.2).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, powerless, 0.85).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
