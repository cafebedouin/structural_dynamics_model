% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Integration for Competence Retention
 *   domain: safety/organizational
 *
 * SUMMARY:
 *   High-reliability organizations maintain competence for catastrophe
 *   prevention through a hybrid system: regular simulator exercises provide
 *   safe, repeatable training, but simulators are validated and updated
 *   through investigation of real-world near-miss incidents and minor
 *   failures. This reading asserts that near-misses provide sufficient
 *   real-world feedback to keep training current without requiring full
 *   catastrophes. Operators and safety investigators bear the workload of
 *   reporting and analyzing incidents; the training program and
 *   organizational learning system benefit from the captured data. The
 *   constraint is CLAIMED as tangled rope because it genuinely coordinates
 *   (hybrid training feedback loop) while asymmetrically extracting (workload
 *   and classification risk fall on operators and investigators). The claim
 *   and metrics are independent: metrics describe a moderately extractive,
 *   mildly enforced arrangement that begins as genuine coordination but
 *   drifts slightly toward theater (near-miss investigation becomes
 *   compliance ritual disconnected from simulator updating).
 *
 * KEY AGENTS:
 *   - safety_training_program_administrator: Institutional agenda-setter; controls what counts as valid feedback, decides simulator updates, benefits from near-miss volume. Power: institutional. Exit: constrained (role is the position).
 *   - front_line_operators: Powerless payers with identity-locked exit (professional identity, regulatory compliance); bear reporting burden and classification risk but benefit from better training feedback. Extraction: moderate but internalized through professional commitment.
 *   - safety_investigators: Moderate-power payers; workload grows with incident volume; face conflicting loyalties in classification (operator-protection vs. organization-protection). Constrained exit (professional identity, organizational dependency).
 *   - competing_training_modalities: Excluded powerful voices (catastrophe-purists, simulation-sufficiency advocates, apprenticeship traditions); locked out of near-miss classification process despite relevant expertise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.38).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.22).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Integration for Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety/organizational").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, '47a717a0-7c84-41c8-a00e-f2fe60723950').
narrative_ontology:cs_kernel_codification('47a717a0-7c84-41c8-a00e-f2fe60723950', distributed).
narrative_ontology:cs_authority_grounding('47a717a0-7c84-41c8-a00e-f2fe60723950', lineage).
narrative_ontology:cs_interpretation_layer_present('47a717a0-7c84-41c8-a00e-f2fe60723950').
narrative_ontology:cs_reading_relation('47a717a0-7c84-41c8-a00e-f2fe60723950', competence_retention_exercise__simulation_as_sufficient, influences).
narrative_ontology:cs_reading_relation('47a717a0-7c84-41c8-a00e-f2fe60723950', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_axiom('47a717a0-7c84-41c8-a00e-f2fe60723950', foundational, feedback_sufficiency_without_catastrophe).
narrative_ontology:cs_axiom_status(feedback_sufficiency_without_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('47a717a0-7c84-41c8-a00e-f2fe60723950', feedback_sufficiency_without_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('47a717a0-7c84-41c8-a00e-f2fe60723950', foundational, hybrid_validation_necessity).
narrative_ontology:cs_axiom_status(hybrid_validation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('47a717a0-7c84-41c8-a00e-f2fe60723950', hybrid_validation_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('47a717a0-7c84-41c8-a00e-f2fe60723950', training_feedback_loop_maturity).
narrative_ontology:cs_drift_state('47a717a0-7c84-41c8-a00e-f2fe60723950', contemporary_compliance_institutionalization, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('47a717a0-7c84-41c8-a00e-f2fe60723950', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_training_program).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, organizational_learning_system).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, front_line_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, safety_investigators).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).
:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint does provide genuine coordination benefit (near-miss feedback genuinely improves simulator validity) but also concentrates data-capture power in the training administrator's hands and loads investigation work on operators and investigators who have no proportional gain. Suppression is low (0.22) because participation is motivated by professional identity and regulatory requirement, not by active coercion — operators WANT to maintain competence and comply with rules; the suppression that exists is the identity-lock mechanism and the implicit career risk in incident classification. Theater ratio rises gradually (0.08 → 0.18) over the interval because early in the constraint's operation the near-miss investigation genuinely feeds simulator design; over time, incident classification becomes institutionalized and decouples slightly from actual simulator updating — near-miss reports are archived as compliance artifacts rather than active learning inputs. The trajectory levels off at 0.18 because the constraint stabilizes: organizations learn to maintain the fiction of integration without genuine feedback coupling. Accessibility collapse is moderate-high (0.65) because once the constraint is understood, alternatives (pure simulation, catastrophe-based learning) appear inadequate — the near-miss hybrid becomes the only acceptable middle path. Resistance is moderate (0.42) because some operators and investigators push back on the classification process (questioning its fairness, arguing incidents are misclassified for organizational protection), but they lack institutional power to disrupt the system; their resistance is absorbed as procedural improvement rather than structural change.
 *
 * PERSPECTIVAL GAP:
 *   The training administrator and front-line operators experience this constraint divergently. From the administrator's seat, near-miss investigation is a genuine coordinating mechanism that generates irreplaceable training data — high-fidelity feedback that simulation design cannot produce alone. From the operator's seat, near-miss investigation is a compliance burden that carries classification risk (whether the incident will be read as operator error or system failure) and offers no direct benefit — the operator's competence improves only if the feedback actually reaches simulator design, which is uncertain. The engine should compute a beneficiary-side type (the administrator's reading is closer to rope, genuine coordination) and a payer-side type (the operator's reading is closer to tangled_rope or mild snare, extraction with a coordination cover story). This divergence is exactly what the structural data encodes: beneficiaries/victims + constrained exit → different directionalities per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Front-line operators (powerless, identity_locked exit, victims list): high directionality (d ≈ 0.75–0.85) because they are trapped by professional identity, bear the investigation workload, and face classification risk without proportional benefit. The constraint extracts their time and carries implicit threat (misclassification). Safety investigators (moderate power, constrained exit, victims list): moderate-high directionality (d ≈ 0.65–0.75) because they expand their workload significantly with incident investigation obligations, but they retain some professional agency in classification decisions (moderate power) and may genuinely believe near-miss investigation improves safety (internalized benefit). Training administrator (institutional power, constrained exit by role, beneficiaries list): low directionality (d ≈ 0.15–0.25) because the administrator collects the data, controls the classification, shapes simulator updates, and captures the budget/legitimacy benefit of a mature learning system. The administrator has constrained exit not because of external barriers but because the role IS the constraint's administration — exit would mean abdicating the power. Competing voices (excluded, trapped): high directionality (d ≈ 0.90–1.0) structurally, though they are not formally seated; their exclusion from classification decisions is the suppression mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The near-miss reading avoids the mandatrophy trap (false mandate) by grounding the founding problem in a real trilemma: competence must be maintained, simulation alone is insufficient for validation, and catastrophe-based learning is indefensible. The near-miss integration constraint genuinely solves this trilemma. However, there is a secondary mandatrophy risk: if near-miss investigation devolves into compliance ritual (incidents are reported, classified, and filed but do not actually update simulators), the founding problem PERSISTS but the constraint becomes theater. The measurement series tracking theater_ratio rising to 0.18 captures this drift: the constraint's mandate (keep training valid through field feedback) is outliving its function (near-miss data → simulator updating) as the system institutionalizes. At theater_ratio > 0.5, a piton classification would be warranted; at 0.18, the constraint is still functionally coupled to its original problem, but the trajectory suggests degradation risk. Mandatrophy resolution: the constraint is NOT resolved because the problem it was built to solve (competence maintenance through feedback) is still live and the constraint still produces some feedback coupling. If simulator updating stopped entirely while incident reporting continued, mandatrophy would fire and the classification would shift toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_feedback_coupling_decay,
    'Do near-miss investigations actually translate into simulator updates, or does the incident-reporting process become decoupled from training feedback over time?',
    'Audit the simulator training scenarios against the incident database: measure the proportion of investigated near-misses that appear as updated scenarios within 2 years of investigation. If coupling falls below ~70%, the constraint is devolving into theater.',
    'If coupling decays substantially, the constraint loses its coordination function and becomes pure extraction (operators report incidents, investigators classify them, trainers archive them, but simulator design does not actually respond). Classification would shift toward piton or snare. If coupling holds, the constraint remains tangled_rope with genuine coordination component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(near_miss_feedback_coupling_decay, empirical, 'Whether near-miss investigation genuinely feeds simulator updating or becomes compliance theater.').

omega_variable(
    classification_bias_toward_system,
    'Are near-miss incidents systematically classified as system-based (protecting operators) or operator-error-based (protecting the organization), and does this classification bias limit the actual learning value?',
    'Examine incident classification patterns longitudinally: track the ratio of system-based to operator-error classifications and correlate with organizational changes. If bias exists, determine whether it is structural (investigator pressure) or reflective of true system causation.',
    'A systematic bias toward one classification type suggests the constraint''s extraction function (operator time and classification work) exceeds its coordination function (genuine learning). High operator-error classification suggests extraction; high system-based classification suggests organizational protection. True balance would reflect genuine mixed causation. Sustained bias that masks learning opportunities would shift the classification from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_bias_toward_system, empirical, 'Whether incident classification serves learning or protects organizational or operator interests at learning''s expense.').

omega_variable(
    identity_lock_internalization_suppression,
    'Is the measured suppression (0.22) structural — external barriers to refusing incident reporting — or internalized through professional identity commitment?',
    'Post-exit analysis: if operators who leave the organization continue to believe near-miss reporting is essential to safety, suppression is substantially internalized. If operators express relief at no longer participating, suppression was structural. Industry surveys of retired operators provide direct evidence.',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than the scalar suggests — operators carry the constraint''s commitments with them after exit, limiting alternatives and mobility. If structural, the constraint depends on continuous role-based exclusion. Internalization favors snare classification; structural suppression supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_suppression, empirical, 'Whether identity-locked suppression is structural or internalized through professional commitment.').

omega_variable(
    kernel_reading_alternative_sufficiency,
    'Does the near-miss reading''s claim of sufficiency (catastrophes are not required) hold up under empirical challenge from the catastrophe-as-necessary reading?',
    'Comparative analysis across high-reliability organizations: do those using ONLY near-miss integration (without waiting for catastrophes) maintain competence as effectively as those that have experienced and learned from real catastrophes? Do organizations that experienced a real catastrophe subsequently improve competence maintenance beyond those using hybrid near-miss systems?',
    'If near-miss integration alone sustains competence equally well, the near-miss reading forecloses the catastrophe-as-necessary reading — the axiom ''feedback_sufficiency_without_catastrophe'' holds and the sibling reading becomes unnecessary. If catastrophe-exposed organizations demonstrably maintain competence better, the near-miss reading is merely one tool, not sufficient, and coexists with the catastrophe-as-necessary reading rather than foreclosing it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_sufficiency, empirical, 'Whether near-miss feedback alone is sufficient to maintain competence or whether catastrophe-level events provide irreducible learning that near-miss systems cannot capture.').

omega_variable(
    simulation_fidelity_validation_gap,
    'Can simulator training be validated as competence-maintaining without field-incident feedback? Can pure simulation (the sibling reading) achieve the validation standard the near-miss reading claims to provide?',
    'Controlled experiment or natural experiment: compare competence retention in organizations using (A) near-miss-integrated simulation, (B) pure high-fidelity simulation with external validation, and (C) catastrophe-based learning alone. Measure competence decay, incident severity, and recovery time.',
    'If pure simulation with external validation equals or exceeds near-miss-integrated performance, the simulation_as_sufficient reading forecloses the near-miss reading''s necessity claim. If near-miss integration provides irreducible value, the near-miss reading coexists with (but is superior to) pure simulation. If catastrophe-based learning is superior to both, the kernel remains genuinely contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_validation_gap, empirical, 'Whether near-miss integration is necessary for simulator validation, or whether other validation methods can achieve equivalent competence maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 5, 0.11).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 10, 0.14).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 15, 0.16).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.17).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 25, 0.18).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 30, 0.18).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 15, 0.21).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 25, 0.22).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, simulator_training_system).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, incident_investigation_protocol).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, organizational_memory_codification).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'competence_retention_exercise'. Sibling constraints in the family: (1) competence_retention_exercise__simulation_as_sufficient (pure high-fidelity simulation is sufficient), (2) competence_retention_exercise__catastrophe_as_necessary (only catastrophes provide irreducible learning). All three readings share the core problem — how to maintain catastrophe-avoidance competence — but diverge on sufficiency of feedback sources. The near-miss reading claims a hybrid system (simulation + field validation) is both necessary and sufficient. The three constraints should be read together: their epsilon values will differ substantially (simulation_as_sufficient may show lower extraction if it claims no workload burden; catastrophe_as_necessary may show higher resistance if it confronts the human cost argument; near-miss_as_bridge sits in the middle). Each constraint's classification will be computed independently by the engine; the family structure allows comparative analysis across readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__near_miss_as_bridge, powerless, 0.78).
constraint_indexing:directionality_override(competence_retention_exercise__near_miss_as_bridge, moderate, 0.68).
constraint_indexing:directionality_override(competence_retention_exercise__near_miss_as_bridge, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
