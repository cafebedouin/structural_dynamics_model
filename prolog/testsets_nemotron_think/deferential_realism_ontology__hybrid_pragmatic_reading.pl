% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Constraint Typology (Hybrid Pragmatic Reading)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism constraint typology itself functions as a
 *   constraint on how constraints are classified. This story captures the
 *   hybrid_pragmatic_reading: the typology has a fixed core (mountains as
 *   physical/logical invariants, ropes as genuine coordination solutions)
 *   grounded in observational epsilon, but a contested periphery
 *   (tangled_ropes, snares) where epsilon measurement becomes constructed —
 *   dependent on normative judgments about who counts as a legitimate
 *   beneficiary. The core classifications are stable across interpretive
 *   communities; the peripheral ones are openly contested. Medium suppression
 *   operates through academic gatekeeping, funding requirements, and the
 *   framework's own schema validation that makes alternative typologies
 *   structurally illegible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.5).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Constraint Typology (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '92d80a57-d4f5-49ba-a5c9-acdc22fd27b0').
narrative_ontology:cs_kernel_codification('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', formalized).
narrative_ontology:cs_authority_grounding('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', practice).
narrative_ontology:cs_interpretation_layer_present('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0').
narrative_ontology:cs_reading_relation('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', deferential_realism_ontology__rhetorical_scaffold_reading, influences).
narrative_ontology:cs_axiom('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', foundational, hybrid_epsilon_measurement_valid).
narrative_ontology:cs_axiom_status(hybrid_epsilon_measurement_valid, holdable).
narrative_ontology:cs_axiom_grounding('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', hybrid_epsilon_measurement_valid, empirically_contingent).
narrative_ontology:cs_axiom('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', foundational, core_periphery_distinction_structurally_real).
narrative_ontology:cs_axiom_status(core_periphery_distinction_structurally_real, holdable).
narrative_ontology:cs_axiom_grounding('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', core_periphery_distinction_structurally_real, deontological).
narrative_ontology:cs_reference_frame('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', original_typology_formulation).
narrative_ontology:cs_drift_state('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', contemporary_multi_community_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('92d80a57-d4f5-49ba-a5c9-acdc22fd27b0', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, dominant_interpretive_community).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_gatekeepers).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, marginalized_interpretive_communities).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, misclassified_constraint_subjects).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, dissenting_analysts).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_typology_has_fixed_core).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, peripheral_classification_is_normatively_contested).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, hybrid_epsilon_measurement_is_valid).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the formal schema, validation tools, and canonical examples of the constraint typology. Control the authoritative repository and decide which extensions or modifications are accepted. Their authority derives from being the recognized stewards of the framework's technical infrastructure.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers, agenda_setter,
    institutional, generational, arbitrage, global).

% The primary community of analysts who apply the typology in published work, policy briefs, and institutional reviews. Their interpretive norms shape how peripheral classifications (tangled_rope vs snare) are resolved in practice. They benefit from the framework's legitimacy and their centrality in its application.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, dominant_interpretive_community, beneficiary,
    organized, biographical, mobile, global).

% Journal editors, funding agency reviewers, and academic department chairs who treat the typology as a required analytical lens. They benefit from a stable evaluative standard but also shape which readings count as competent. Their exit is constrained by career dependence on the framework's institutional recognition.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_gatekeepers, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_gatekeepers, agenda_setter).

% Analysts from non-Western epistemological traditions, critical theory backgrounds, or activist-scholar positions whose readings of constraints are systematically disadvantaged in peripheral classification. They bear the cost of having their constraint analyses dismissed or requiring translation into the dominant vocabulary.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, marginalized_interpretive_communities, payer,
    moderate, biographical, constrained, regional).

% People and groups whose lived constraints are classified differently than they would self-classify — e.g., a community experiencing a snare that the framework's dominant reading calls a tangled_rope. They bear the material cost of misclassification (policy neglect, misdirected intervention) with no voice in the classification process.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, misclassified_constraint_subjects, payer,
    powerless, immediate, trapped, local).

% Scholars who reject the hybrid pragmatic reading's core/periphery distinction or its hybrid epsilon methodology but publish in venues where the framework is normative. They would object to the framework's epistemic authority but are structurally excluded from the maintenance and gatekeeping processes.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, dissenting_analysts, excluded,
    moderate, biographical, constrained, global).

% Philosophers of science, metrologists, and comparative epistemologists who study the typology as an object of analysis rather than a tool. They see the full structural field including the contested periphery and the power dynamics of classification, but do not participate in the framework's operational enforcement.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared analytical vocabulary and classification structure that allows constraint analysts across domains to communicate, compare, and build cumulative knowledge about constraint types without reinventing categories for each case.
% TRANSFER_FUNCTION: Moves classificatory authority and epistemic legitimacy from marginalized readings and constraint subjects to the dominant interpretive community and institutional gatekeepers, via the contested periphery where normative judgments about 'legitimate beneficiaries' determine whether a constraint is tangled_rope (coordination + extraction) or snare (pure extraction).
% ABSENT_VOICES: Practitioners from non-Western epistemological traditions (indigenous constraint ontologies, Global South institutional analysts), activists directly experiencing constraints the framework classifies differently, independent scholars without institutional affiliation who cannot access the gatekeeping venues where classification norms are set.
% DISAPPEARANCE_RATIONALE: If the typology vanished overnight, the field of constraint analysis would lose its shared vocabulary and comparative structure. New classification schemes would emerge but with different core/periphery boundaries, different epsilon measurement conventions, and different power distributions among interpretive communities. The specific hybrid pragmatic reading's synthesis of observational and constructed epsilon would be a particular casualty.
% FOUNDING_PROBLEM: The need for a systematic way to distinguish natural constraints (mountains, ropes) from constructed extractive ones (snares, tangled_ropes) in policy and institutional analysis, without either treating all constraints as natural facts or all as pure power plays.
% FOUNDING_PROBLEM_CORROBORATION: Independent scholars in philosophy of science (e.g., Cartwright on nomological machines), institutional economics (e.g., Ostrom on design principles), and critical policy analysis attest the core distinction problem is real and persistent. The framework's own maintainers attest both core and periphery problems remain live; no external corroboration exists for the claim that the periphery problem is 'solved' by the hybrid pragmatic reading.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the hybrid epsilon: low for core (observational, near-zero extraction) but substantial for periphery (constructed, where classification decisions move resources and legitimacy). Suppression (0.5) is medium — not violent coercion but structural: the schema, the validator, the canonical examples, and the gatekeeping venues make it costly to sustain alternative readings. Theater (0.3) captures performative application of the framework where analysts go through classification motions without engaging the normative periphery. Accessibility collapse (0.4) is moderate: alternative typologies exist but the framework's network effects and institutional embedding raise switching costs. Resistance (0.45) comes from the contested periphery — marginalized communities and dissenting analysts actively challenge peripheral classifications.
 *
 * PERSPECTIVAL GAP:
 *   From the maintainer/gatekeeper seat, the framework is a rope (genuine coordination, minimal extraction). From the marginalized community seat, the periphery operates as a snare (normative capture masquerading as classification). From the misclassified subject seat, it's a snare with identity-locked exit (they cannot exit the classification imposed on them). The engine computes this divergence from the structural data; the hybrid_pragmatic_reading explicitly acknowledges the gap rather than resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework maintainers and institutional gatekeepers are structural beneficiaries (d near 0.0): they control the schema, the validator, the venues, and collect epistemic rents. The dominant interpretive community benefits (d ~0.2) from normative authority in peripheral classification. Marginalized interpretive communities and misclassified constraint subjects are targets (d ~0.8-0.9): they bear the cost of translation, misclassification, and exclusion. Dissenting analysts are excluded (not in the directionality computation as participants). External observers sit at analytical (d=0.5 by convention).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing natural from constructed constraints) remains live. The core typology continues to serve it. The periphery has accumulated mandatrophy: the hybrid epsilon methodology was meant to handle contested cases but has become a mechanism for the dominant community to settle contests in its favor. The framework persists not because the periphery works but because the core is too useful to abandon and the institutional cost of replacement is prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_periphery_boundary_stability,
    'Is the core/periphery boundary itself stable, or does the periphery expand to absorb formerly core classifications as the framework encounters new domains?',
    'Longitudinal analysis of classification disputes: track whether constraints originally classified as mountain/rope get reclassified to tangled_rope/snare as interpretive communities contest them.',
    'If the boundary drifts, the ''fixed core'' claim is falsified and the hybrid reading collapses into either the immutable_diagnostic_reading (if core holds) or rhetorical_scaffold_reading (if all becomes peripheral).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_periphery_boundary_stability, empirical, 'Whether the core/periphery distinction is structurally stable or a moving target.').

omega_variable(
    normative_capture_measurement,
    'How can we measure when peripheral classification serves power rather than analytical accuracy, given that the hybrid reading treats normative judgment as constitutive of peripheral epsilon?',
    'Develop a counterfactual test: compare peripheral classifications of the same constraint by analysts from different interpretive communities with no stake in the outcome; measure variance attributable to community affiliation vs. constraint features.',
    'If community affiliation predicts classification better than constraint features, the hybrid reading''s epsilon is capture, not measurement. This would support the rhetorical_scaffold_reading''s critique.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_capture_measurement, conceptual, 'Whether hybrid epsilon in the periphery measures constraint structure or analyst position.').

omega_variable(
    hybrid_epsilon_coherence,
    'Can observational epsilon (core) and constructed epsilon (periphery) be combined into a single metric without category error?',
    'Formal measurement theory analysis: test whether the two epsilon scales share a common unit, zero point, and transformation rules. If not, the hybrid metric is a category error.',
    'If incoherent, the hybrid reading''s claimed_type (tangled_rope) rests on a metric that doesn''t exist. The reading would need to split into two constraints (core rope, periphery snare) per epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_epsilon_coherence, conceptual, 'Whether hybrid epsilon is a coherent metric or a category error.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the hybrid_pragmatic_reading''s commitment to a stable core logically foreclose the rhetorical_scaffold_reading''s claim that all classification is normative declaration?',
    'Logical analysis: if the hybrid reading asserts ''mountains exist as observational invariants'', does that directly contradict ''all constraint types are normative declarations'' such that no single framework could hold both?',
    'If forecloses, the readings cannot coexist in one analytical framework — the kernel would be genuinely fractured. If coexists_with, the kernel hosts a stable pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading''s core commitment logically excludes the rhetorical scaffold reading.').

omega_variable(
    suppression_mechanism_in_academic_gatekeeping,
    'Is the framework''s suppression structural (journal requirements, funding mandates) or internalized (analysts self-censor because they''ve absorbed the framework''s categories as ''how analysis works'')?',
    'Post-exit study: track analysts who leave the framework''s institutional orbit — do they continue to use its categories, invert them, or develop alternatives? Persistence of categories after exit suggests internalization.',
    'If internalized, effective suppression is higher than structural measures suggest. The constraint carries its own suppression mechanism into the analyst''s cognitive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_academic_gatekeeping, empirical, 'Whether academic suppression is external or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dro_hpr_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(dro_hpr_tr_t0, observed).
narrative_ontology:measurement(dro_hpr_tr_t5, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(dro_hpr_tr_t5, observed).
narrative_ontology:measurement(dro_hpr_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(dro_hpr_tr_t10, observed).
narrative_ontology:measurement(dro_hpr_tr_t15, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(dro_hpr_tr_t15, observed).
narrative_ontology:measurement(dro_hpr_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(dro_hpr_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(dro_hpr_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(dro_hpr_be_t0, observed).
narrative_ontology:measurement(dro_hpr_be_t5, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(dro_hpr_be_t5, observed).
narrative_ontology:measurement(dro_hpr_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(dro_hpr_be_t10, observed).
narrative_ontology:measurement(dro_hpr_be_t15, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(dro_hpr_be_t15, observed).
narrative_ontology:measurement(dro_hpr_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(dro_hpr_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(dro_hpr_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(dro_hpr_su_t0, observed).
narrative_ontology:measurement(dro_hpr_su_t5, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement_basis(dro_hpr_su_t5, observed).
narrative_ontology:measurement(dro_hpr_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(dro_hpr_su_t10, observed).
narrative_ontology:measurement(dro_hpr_su_t15, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(dro_hpr_su_t15, observed).
narrative_ontology:measurement(dro_hpr_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(dro_hpr_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__hybrid_pragmatic_reading, 0.03).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_classification_schema).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_analysis_framework).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, policy_constraint_mapping).

% DUAL FORMULATION NOTE:
% This reading decomposes the deferential_realism_ontology kernel alongside immutable_diagnostic_reading and rhetorical_scaffold_reading. The hybrid_pragmatic_reading claims the core is observational (low epsilon) while the periphery is constructed (higher epsilon); the immutable reading claims all epsilon is observational; the rhetorical reading claims all epsilon is normative declaration. The three readings form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, institutional, 0.15).
constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
