% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Based Competence Maintenance with Judgment Decay
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint is the hybrid_decay_reading of the contested kernel
 *   exercise_as_competence_maintenance. The standing arrangement is the
 *   institutionalized practice of using simulation exercises as the primary
 *   mechanism for maintaining emergency-response competence. Under this
 *   reading, the kernel is composite: simulation genuinely exercises
 *   procedural competence (muscle memory, script adherence, equipment
 *   handling) but fails to exercise judgment-under-stakes (improvisation,
 *   anomaly recognition, affect regulation in real consequence). The
 *   arrangement therefore presents a coordination function that is real but
 *   bounded, while asymmetrically extracting by substituting a cheaper,
 *   safer, measurable activity for the full competence requirement. Victims
 *   include frontline operators who carry the decayed capacity and the
 *   crisis-exposed public who bear the harm when judgment fails. Sibling
 *   readings are simulation_sufficiency_reading (simulation fully exercises
 *   the kernel) and lived_catastrophe_necessity_reading (only real
 *   catastrophe exercises the kernel).
 *
 * KEY AGENTS:
 *   - simulation_training_providers (agenda_setter / organized / mobile) â design and sell the curricula that become the standard
 *   - organizational_compliance_officers (beneficiary / moderate / constrained) â satisfy requirements at reduced cost and liability
 *   - frontline_operators (payer / moderate / constrained) â gain procedural fluency but lose untested judgment capacities
 *   - crisis_exposed_public (payer / powerless / trapped) â rely on the system and bear failure costs
 *   - safety_human_factors_researchers (observer / analytical / analytical) â document the decomposition of competence and the decay of judgment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.6).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Based Competence Maintenance with Judgment Decay").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '5381a306-94cb-4931-89fa-a6cc701b5417').
narrative_ontology:cs_kernel_codification('5381a306-94cb-4931-89fa-a6cc701b5417', formalized).
narrative_ontology:cs_authority_grounding('5381a306-94cb-4931-89fa-a6cc701b5417', expertise).
narrative_ontology:cs_interpretation_layer_present('5381a306-94cb-4931-89fa-a6cc701b5417').
narrative_ontology:cs_reading_relation('5381a306-94cb-4931-89fa-a6cc701b5417', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('5381a306-94cb-4931-89fa-a6cc701b5417', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('5381a306-94cb-4931-89fa-a6cc701b5417', foundational, procedural_judgment_complementarity).
narrative_ontology:cs_axiom_status(procedural_judgment_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('5381a306-94cb-4931-89fa-a6cc701b5417', procedural_judgment_complementarity, empirically_contingent).
narrative_ontology:cs_axiom('5381a306-94cb-4931-89fa-a6cc701b5417', foundational, simulation_partial_legitimacy).
narrative_ontology:cs_axiom_status(simulation_partial_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5381a306-94cb-4931-89fa-a6cc701b5417', simulation_partial_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('5381a306-94cb-4931-89fa-a6cc701b5417', differentiated_exercise_framework).
narrative_ontology:cs_drift_state('5381a306-94cb-4931-89fa-a6cc701b5417', contemporary_safety_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5381a306-94cb-4931-89fa-a6cc701b5417', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_compliance_officers).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_training_providers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_exposed_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_competence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and deliver simulation curricula, certify completion hours, and advocate for simulation-based training standards. Revenue and institutional role depend on simulation being accepted as a legitimate exercise of operational competence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_training_providers, agenda_setter,
    organized, biographical, mobile, national).

% Procure simulation hours to satisfy regulatory and internal competence requirements at lower cost and lower liability than live high-stakes exercises. Report upward that competence maintenance targets are met without needing to verify judgment-under-stakes.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_compliance_officers, beneficiary,
    moderate, biographical, constrained, national).

% Repeat simulated scenarios until procedural responses are automatic. Rarely encounter unscripted high-stakes decisions in training. Certification and employment depend on simulation completion. Carry the risk of being unprepared when real incidents deviate from script.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators, beneficiary).

% Rely on emergency response systems to function under real catastrophe. Not consulted on training design or exercise selection. Bear the harm when responders, despite logged simulation hours, fail to improvise under novel, high-stakes conditions.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_exposed_public, payer,
    powerless, immediate, trapped, local).

% Publish evidence distinguishing procedural skill from judgment-under-stakes, document overconfidence transfer from low-consequence simulation, and argue for mixed training regimes. Findings are cited in standards debates but often overridden by budget and compliance logics.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, safety_human_factors_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns emergency response procedures across personnel and organizations through repeated rehearsal; maintains standardized action sequences, equipment familiarity, and inter-team coordination protocols.
% TRANSFER_FUNCTION: Moves organizational compliance credit and training budget allocation from judgment-exercising activities to controllable, measurable simulation hours; moves the deferred cost of judgment failure to future crisis victims and to frontline operators who face unscripted scenarios unprepared.
% ABSENT_VOICES: Victims of past and future crises where judgment collapsed are not in the training design room; experienced responders who learned judgment through real catastrophic events are structurally sidelined because their expertise is narrative and non-scalable; insurers and liability attorneys who would demand evidence of judgment competence are absent because current metrics measure procedure, not outcome.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, organizations would lose their primary compliance pathway and would need to redesign training around genuine high-stakes exercise or accept unverified competence; simulation providers would lose revenue streams; frontline operators would face different certification requirements; the safety field would reorganize around competence verification rather than hour-counting.
% FOUNDING_PROBLEM: Live high-stakes exercises are dangerous, expensive, and ethically difficult to arrange; organizations needed a repeatable, scalable, safe method to keep responders prepared without exposing them to catastrophic risk during training.
% FOUNDING_PROBLEM_CORROBORATION: Safety researchers and veteran incident commanders attest that the founding problem is real but has been overextended to justify abandoning all high-stakes judgment training; institutional risk managers and training providers attest it remains live. Independent empirical studies from cognitive psychology and disaster sociology corroborate that judgment-under-stakes is not exercised in low-consequence simulation, supporting a shifted-function reading.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58) reflects the gap between claimed full-competence maintenance and actual partial maintenance: the arrangement extracts by substituting simulation for high-stakes judgment exercise. Suppression (0.60) is substantial because institutional budgets, accreditation standards, and liability frameworks actively channel training investment away from live high-stakes drills and real-field experience; alternatives are not illegal but are structurally starved. Theater ratio (0.40) captures the growing performative dimension: competence is increasingly measured by hours logged and checklists passed rather than demonstrated judgment, though the procedural training remains functionally real. Accessibility_collapse (0.45) is moderate because genuine alternatives (live exercises, apprenticeship in real incidents) exist but are costly, dangerous, and institutionally disfavored. Resistance (0.35) is moderate: safety researchers and some veteran operators dissent, but they are outweighted by compliance and budget logics. The claim/metric gap is deliberate: the constraint coordinates procedures effectively, but the metrics describe an arrangement that extracts through bounded substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as a genuine coordination achievement: repeatable, safe, scalable training that keeps procedures aligned. The payer seats experience it as a slowly tightening substitution: frontline operators feel the mismatch between training reality and incident demand; the crisis-exposed public is unaware until failure occurs. The engine will compute divergent per-seat classifications because the directionality derivation assigns low d to the compliance and provider seats and high d to the operator and public seats, amplified by their power and exit asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (organizational_compliance_officers, simulation_training_providers) collect cost avoidance and revenue respectively; their exit options (constrained for compliance officers in institutional roles, mobile for providers who can shift markets) place them toward the beneficiary end of directionality. Victims (frontline_operators, crisis_exposed_public) bear the competence gap; operators are employment-constrained and the public is structurally trapped, placing both toward the full-target end. The observer seat sits near symmetric. The structural derivation therefore produces strong seat divergence without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement prevents mislabeling in both directions. Against a pure rope reading, the victim set and the rising theater/extraction series show that substitution has become parasitic: the coordination function is real but incomplete, and the incompleteness is systematically covered rather than remedied. Against a pure snare reading, the procedural competence gain is real and measurable; the constraint is not cover for extraction but a hybrid where extraction rides on genuine coordination. The temporal drift (rising extraction and suppression from T0 to T24) suggests the founding coordination has been progressively colonized by budget and liability logics, but it has not fully atrophied into piton because the procedural function still works.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the hybrid_decay_reading of kernel exercise_as_competence_maintenance. If the simulation_sufficiency_reading were empirically validated, would the constraint reclassify as rope or scaffold?',
    'Empirical validation of judgment transfer from high-fidelity simulation to real-stakes performance across diverse emergency domains.',
    'If simulation sufficiency is proven, effective extraction drops sharply and the constraint might reclassify as rope; if the hybrid reading holds, extraction remains structurally embedded and the tangled-rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural position of this reading within the contested kernel').

omega_variable(
    judgment_simulability_threshold,
    'Is judgment-under-stakes inherently non-simulable, or does a fidelity threshold exist where simulation genuinely exercises it?',
    'Comparative performance studies of expert responders in high-fidelity simulation versus live unscripted incidents under measured cognitive load and consequence.',
    'If a simulability threshold exists, the constraint''s extraction is contingent on current technology and could fall with fidelity advances; if judgment is inherently non-simulable, the extraction is permanent and structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_simulability_threshold, empirical, 'Whether judgment-under-stakes is simulable at any fidelity').

omega_variable(
    procedural_judgment_separability,
    'Are procedural competence and judgment-under-stakes separable components, or does the decomposition mask a unitary skill that decays holistically?',
    'Longitudinal tracking of responder performance in real incidents against backgrounds of simulation-only versus mixed training regimes.',
    'If competence is unitary, the constraint''s partial-extraction framing is false and the arrangement may be closer to benign rope or total snare; if separable, the tangled-rope classification is structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_judgment_separability, conceptual, 'Validity of the competence decomposition').

omega_variable(
    institutional_cost_pressure,
    'To what extent does cost and liability minimization drive simulation dominance over independent pedagogical evidence?',
    'Budgetary analysis and regulatory-text comparison across jurisdictions with differing live-exercise requirements and liability regimes.',
    'If cost-driven, the beneficiary structure is clearly extraction-oriented; if pedagogy-driven, the constraint may be a well-intentioned rope or scaffold misaligned with evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_cost_pressure, empirical, 'Economic motive behind simulation dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exercise_hybrid_decay_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t4, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t8, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t12, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t16, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t24, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(exercise_hybrid_decay_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(exercise_hybrid_decay_be_t4, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(exercise_hybrid_decay_be_t8, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(exercise_hybrid_decay_be_t12, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(exercise_hybrid_decay_be_t16, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(exercise_hybrid_decay_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(exercise_hybrid_decay_be_t24, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(exercise_hybrid_decay_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(exercise_hybrid_decay_su_t4, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(exercise_hybrid_decay_su_t8, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(exercise_hybrid_decay_su_t12, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(exercise_hybrid_decay_su_t16, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(exercise_hybrid_decay_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(exercise_hybrid_decay_su_t24, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the exercise_as_competence_maintenance family. The kernel decomposes into three structurally distinct claims because the referent 'exercise maintains competence' conflates unitary and composite competence ontologies, and conflates rehearsal with genuine exercise. Each reading carries a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
