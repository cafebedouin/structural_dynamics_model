% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: State Medical Mandate â Bodily Autonomy Reading
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story captures the bodily_autonomy_primary reading of the
 *   contested kernel legitimate_health_intervention. The standing arrangement
 *   is state-mandated medical intervention enforced through employment
 *   conditionalities and access restrictions. From this reading, informed
 *   consent is an absolute precondition of legitimacy; state coercion
 *   constitutes extraction of bodily integrity regardless of demonstrated
 *   public health benefit. The metrics are authored to reflect the structural
 *   extraction and coercion visible from this seat, while the coordination
 *   function is acknowledged as the arrangement's stated purpose without
 *   being endorsed as legitimating.
 *
 * KEY AGENTS:
 *   - state_public_health_authority (institutional/agenda_setter/beneficiary) â administers and enforces the mandate, collects compliance and institutional legitimacy
 *   - mandate_coerced_individuals (powerless/payer) â bear extraction via compelled medical intrusion and loss of autonomy
 *   - vulnerable_populations (moderate/beneficiary) â receive risk reduction externality from others' coerced compliance
 *   - healthcare_system (institutional/beneficiary) â receives capacity relief and institutional resources
 *   - civil_liberties_organizations (organized/observer) â analyze and contest the constraint's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.78).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "State Medical Mandate â Bodily Autonomy Reading").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '05c9c93c-9787-4a4f-88cf-434cdfb210fd').
narrative_ontology:cs_kernel_codification('05c9c93c-9787-4a4f-88cf-434cdfb210fd', formalized).
narrative_ontology:cs_authority_grounding('05c9c93c-9787-4a4f-88cf-434cdfb210fd', lineage).
narrative_ontology:cs_interpretation_layer_present('05c9c93c-9787-4a4f-88cf-434cdfb210fd').
narrative_ontology:cs_reading_relation('05c9c93c-9787-4a4f-88cf-434cdfb210fd', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('05c9c93c-9787-4a4f-88cf-434cdfb210fd', legitimate_health_intervention__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('05c9c93c-9787-4a4f-88cf-434cdfb210fd', foundational, informed_consent_absolute_prerequisite).
narrative_ontology:cs_axiom_status(informed_consent_absolute_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('05c9c93c-9787-4a4f-88cf-434cdfb210fd', informed_consent_absolute_prerequisite, deontological).
narrative_ontology:cs_axiom('05c9c93c-9787-4a4f-88cf-434cdfb210fd', foundational, bodily_integrity_trumps_collective_benefit).
narrative_ontology:cs_axiom_status(bodily_integrity_trumps_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('05c9c93c-9787-4a4f-88cf-434cdfb210fd', bodily_integrity_trumps_collective_benefit, deontological).
narrative_ontology:cs_reference_frame('05c9c93c-9787-4a4f-88cf-434cdfb210fd', informed_consent_absolute_framework).
narrative_ontology:cs_drift_state('05c9c93c-9787-4a4f-88cf-434cdfb210fd', contemporary_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('05c9c93c-9787-4a4f-88cf-434cdfb210fd', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authority).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, healthcare_system).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandatory medical intervention policy and enforces it through employment conditionalities, access restrictions, and documentation requirements. Collects compliance metrics, institutional legitimacy, and expanded regulatory precedent. Cannot easily reverse course without admitting error or inviting legal challenge.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authority, beneficiary).

% Must undergo specified medical intervention or surrender bodily autonomy to retain employment, access public accommodations, or maintain legal status. Face loss of income, social participation, and mobility if they refuse. No affordable lawful alternative pathway exists within the jurisdiction.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, immediate, trapped, national).

% Receive reduced pathogen exposure and healthcare system strain as a downstream effect of population-level compliance. Do not control the mandate and cannot opt out of the conferred protection. Their benefit depends on the coerced participation of others.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, vulnerable_populations, beneficiary,
    moderate, biographical, constrained, national).

% Experiences reduced acute caseload and resource depletion when population compliance is high. Supports mandate policy as a capacity management instrument. Receives institutional resources, emergency funding, and political relevance from the crisis-response framework.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, healthcare_system, beneficiary,
    institutional, biographical, constrained, national).

% Litigate and publicly contest the constitutional and ethical basis of medical mandates. Neither collect benefits nor bear the direct costs of the constraint, but analyze its legitimacy and represent dissenting perspectives in courts and discourse.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing uncontrolled infectious disease transmission by aligning individual preventive health decisions with population-level risk reduction through centralized vaccination, testing, or treatment policy.
% TRANSFER_FUNCTION: Moves bodily compliance and medical documentation status from individuals to the state; moves health risk reduction and healthcare capacity relief to vulnerable populations and healthcare institutions.
% ABSENT_VOICES: Individuals with documented medical contraindications who are nonetheless swept into enforcement machinery; religious and philosophical objectors excluded from policy deliberation; public health strategists advocating non-coercive alternatives (education, targeted protection, voluntary incentives) who lack institutional seating.
% DISAPPEARANCE_RATIONALE: If the mandate and its enforcement infrastructure disappeared overnight, coerced individuals would regain unrestricted employment and access, the healthcare system would face renewed capacity planning challenges, and vulnerable populations would lose the conferred externality of population compliance. Public health governance would revert to voluntary or incentive-based instruments, rearranging the legal and economic situation of millions.
% FOUNDING_PROBLEM: Uncontrolled epidemic or pandemic infectious disease overwhelming healthcare capacity and producing excess mortality, particularly among vulnerable groups, in the absence of sufficient voluntary uptake of preventive measures.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists and healthcare administrators attest the threat remains live or was live at founding. Civil liberties organizations and dissenting clinicians attest the threat was manageable through less coercive means; no neutral party unambiguously corroborates the necessity of the coercive arrangement over alternatives.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint compels physical intrusion into bodily integrity as a condition of economic and social participation; this is a severe extraction from the autonomy reading's perspective. Suppression is higher (0.85) because persistence depends on actively penalizing non-compliance through employment and access bars, not on voluntary adherence. Theater_ratio is moderate-high (0.48) because while the biological intervention may be effective, a growing share of enforcement activity centers on documentation verification, access theater, and symbolic compliance rather than actual risk reduction. Accessibility_collapse is high (0.82) because lawful alternatives (unrestricted employment, unmasked access, unvaccinated travel) collapse to near zero once the mandate is in force. Resistance is substantial (0.72) because the constraint faces organized litigation, political protest, and non-compliance. The claimed type is tangled_rope: a genuine disease-prevention coordination function is present, but it is inseparable from asymmetric extraction of bodily autonomy from a trapped population.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter/beneficiary seats (state authority, healthcare system) experience the constraint as a necessary public health coordination mechanism; their computed type may approach rope or tangled_rope with low effective extraction. The payer seat (coerced individuals) experiences the same structure as severe extraction with negligible coordination benefit; their computed type may approach snare. The engine computes this divergence from the same structural data â the divergence itself is the signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state authority, vulnerable populations, healthcare system) receive low directionality: the constraint subsidizes their goals (compliance, risk reduction, capacity). The payer (coerced individuals) receives high directionality: the constraint extracts bodily autonomy and economic access. The state authority's dual role as agenda_setter and beneficiary places its d near the full-beneficiary end, though its constrained exit prevents the arbitrage-grade mobility that would push it to the extreme subsidy end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â epidemic capacity crisis â is contested but not dead. Without the R5 genealogy fields, this constraint could be misclassified as a snare (ignoring the real coordination function) or a rope (ignoring the coercion). The R5 status of contested, combined with high extraction and enforcement, produces the tangled_rope classification: the coordination is live but the extraction is asymmetric and enforced. This prevents both the naive public-benefit defense (rope) and the naive tyranny narrative (snare) by encoding both structural facts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_severity_epsilon_sensitivity,
    'Does the extractiveness of the constraint scale primarily with the severity of enforcement mechanisms (scope of employment penalties, access restrictions) or with the intrinsic invasiveness of the medical intervention itself?',
    'Cross-jurisdictional comparison of mandates with identical biological interventions but divergent enforcement intensity; if epsilon tracks enforcement, the extraction is in the coercion layer, not the medical layer.',
    'If extraction tracks enforcement, the constraint is tangled_rope/snare; if it tracks intrinsic invasiveness, it may read as a different constraint type or require decomposition by intervention type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_severity_epsilon_sensitivity, empirical, 'Whether epsilon is driven by enforcement severity or medical invasiveness').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression in this constraint primarily structural (legal employment bars, access denial) or internalized (stigma, self-censorship, medical distrust that persists after formal rules ease)?',
    'Post-mandate trajectory observation: if compliance pressure and social exclusion persist after formal enforcement is withdrawn, suppression was partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s extraction continues even during formal relaxation phases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in medical mandates').

omega_variable(
    bodily_autonomy_kernel_contest,
    'Does the absolute primacy of informed consent in this reading logically foreclose the public_health_primary and proportionality readings, or do these frameworks merely coexist as incommensurable live positions?',
    'Historical stress-testing: whether autonomy-absolutist frameworks survive empirical demonstrations of catastrophic mortality that proportionality or public-health readings would have prevented; whether adherents revise or reaffirm the absolute premise.',
    'If empirical outcomes can override the axiom, the reading''s grounding is empirically_contingent and engine-computed foreclosure may apply; if not, the axiom is deontological and the readings structurally coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_kernel_contest, conceptual, 'Whether the kernel''s sibling readings are foreclosed or coexistent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_tr_t4, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 4, 0.2).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_tr_t8, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 8, 0.32).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.42).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_tr_t18, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 18, 0.46).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_be_t4, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_be_t8, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_be_t18, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_su_t4, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_su_t8, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_su_t18, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 18, 0.84).
narrative_ontology:measurement(legitimate_health_intervention_bodily_autonomy_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimate_health_intervention kernel. The three readings share the same institutional referent â state medical intervention mandates â but instantiate mutually incompatible normative frameworks. They form a constraint family linked by shared historical and legal material but divergent epsilon, beneficiary structure, and stakeholder classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
