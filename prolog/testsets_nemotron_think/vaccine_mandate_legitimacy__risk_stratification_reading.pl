% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Risk-Stratified Vaccine Mandate Legitimacy Threshold
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the risk_stratification_reading of the
 *   vaccine_mandate_legitimacy kernel. The reading holds that mandate
 *   legitimacy is contingent on a demonstrable actuarial risk threshold:
 *   blanket mandates fail proportionality because they impose on low-risk
 *   populations without sufficient justification, but targeted mandates
 *   covering high-risk settings (healthcare, congregate care) and populations
 *   are permissible when evidence shows the threshold is met. The constraint
 *   is the proportionality test itself — the legal/constitutional rule that
 *   conditions state coercion on a calibrated risk assessment. It is a
 *   tangled_rope: it performs genuine coordination (protecting the vulnerable
 *   through targeted immunization) while extracting compliance from
 *   designated groups, and it requires active enforcement (mandate orders,
 *   exemption adjudication, penalty mechanisms). The victim set
 *   (targeted_mandate_subjects) varies with the threshold definition — a
 *   structural delta noted in the kernel context.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.45).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.55).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Risk-Stratified Vaccine Mandate Legitimacy Threshold").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, 'a6a9eb23-bf00-46b9-82b7-0885ef0c0548').
narrative_ontology:cs_kernel_codification('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', formalized).
narrative_ontology:cs_authority_grounding('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', lineage).
narrative_ontology:cs_interpretation_layer_present('a6a9eb23-bf00-46b9-82b7-0885ef0c0548').
narrative_ontology:cs_reading_relation('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_axiom('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', foundational, proportionality_requirement_for_state_coercion).
narrative_ontology:cs_axiom_status(proportionality_requirement_for_state_coercion, holdable).
narrative_ontology:cs_axiom_grounding('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', proportionality_requirement_for_state_coercion, deontological).
narrative_ontology:cs_axiom('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', foundational, actuarial_risk_threshold_legitimacy).
narrative_ontology:cs_axiom_status(actuarial_risk_threshold_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', actuarial_risk_threshold_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', proportionality_constitutionalism).
narrative_ontology:cs_drift_state('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', post_covid_emergency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6a9eb23-bf00-46b9-82b7-0885ef0c0548', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_infrastructure).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, targeted_mandate_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, legislatures).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the actuarial risk threshold, designate high-risk settings, and issue targeted mandate orders. They bear the burden of demonstrating that the threshold is met and that less restrictive alternatives are insufficient. Their legitimacy depends on maintaining public trust in the proportionality calculus.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Enact the statutory framework authorizing risk-stratified mandates, setting the legal threshold and procedural safeguards. They benefit politically from appearing to balance rights and safety, but face electoral pressure from both mandate opponents and vulnerable-population advocates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, legislatures, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, legislatures, beneficiary).

% Adjudicate challenges to specific mandates against the proportionality standard. They set the effective threshold through case law — what counts as sufficient actuarial evidence, what less-restrictive alternatives must be tried first. Their rulings define the constraint's operational boundary.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Immunocompromised individuals, elderly in congregate care, and others who cannot be protected by their own vaccination. They gain protection when mandates reduce community transmission in their environments. They have no exit from their vulnerability and no leverage over threshold-setting.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Hospital systems, long-term care facilities, and disease surveillance networks. They benefit from reduced outbreak burden and staffing stability when mandates cover their workforce and residents. They advocate for threshold triggers but cannot unilaterally impose mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_infrastructure, beneficiary,
    organized, generational, constrained, national).

% Healthcare workers, long-term care staff, and other groups designated by the risk threshold. They bear the compliance burden (vaccination, testing, masking, or job loss) when the threshold is triggered. Their exit options are limited: change profession, seek exemption, or comply. The threshold determines whether they are in the payer seat at all.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, targeted_mandate_subjects, payer,
    moderate, biographical, constrained, national).

% Advocates and individuals who hold that any state-compelled medical intervention violates a non-derogable right. They are structurally excluded from the threshold-setting process because the risk-stratification framework presupposes that some coercion can be legitimate — their categorical rejection has no purchase within the framework.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_absolutists, excluded,
    organized, biographical, trapped, national).

% Organized opposition to vaccine mandates on safety, efficacy, or conspiracy grounds. They challenge the actuarial evidence itself rather than the proportionality structure. The framework treats their objections as empirical disputes to be resolved by evidence, not as threshold-setting inputs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, anti_vaccine_movements, excluded,
    organized, biographical, constrained, global).

% Analyze the threshold's coherence, its application across jurisdictions, and its drift over time. They do not set policy but shape the intellectual framework courts and legislatures draw on. Their exit is analytical — they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, bioethics_scholars, observer,
    analytical, generational, analytical, global).

% Produce the actuarial risk estimates that trigger or withdraw mandates. Their models determine whether the threshold is met. They are observers of the legal framework but architects of its factual predicate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, epidemiologists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents disease outbreaks in high-risk settings through targeted immunization requirements triggered by demonstrable actuarial risk exceeding a proportionality threshold, avoiding the overbreadth of blanket mandates while preserving protection for those who cannot protect themselves.
% TRANSFER_FUNCTION: Moves the compliance burden (vaccination, testing, masking, or employment consequences) from vulnerable populations who bear disproportionate harm onto targeted mandate subjects (healthcare workers, congregate care staff/residents) when and only when actuarial evidence shows the risk threshold is met.
% ABSENT_VOICES: Absolute bodily autonomy advocates who reject any state coercion as categorically impermissible are structurally excluded — the framework's premise is that coercion CAN be legitimate if proportional. Anti-vaccine movements challenging the empirical predicate are treated as evidentiary disputes, not threshold-setting participants. Both would object to the framework's existence but have no seat in its calibration.
% DISAPPEARANCE_RATIONALE: Without the proportionality threshold as the operative legal test, constitutional doctrine would collapse to one of two poles: either blanket mandate authority (public health primacy) or categorical prohibition (bodily autonomy primacy). The risk-stratification framework is the specific doctrinal mechanism that mediates between them; its disappearance forces a regime change.
% FOUNDING_PROBLEM: How to reconcile state public health authority with individual bodily integrity in a pluralistic constitutional order — specifically, how to permit coercive health measures when genuinely necessary while preventing their deployment as routine governance tools.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts in multiple jurisdictions (German Bundesverfassungsgericht 2022 Measles Protection Act ruling, Canadian Supreme Court 2021 Trinity Western, ECHR 2021 Vavřička) have articulated proportionality frameworks for vaccine mandates as a live doctrinal question. Legal scholars outside the public health establishment (e.g., Gostin & Wiley critiquing Jacobson's breadth, Epstein on police power limits) corroborate that the founding problem remains contested — the proportionality test is not settled law but a contested middle ground.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).
:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint extracts compliance only from groups where actuarial risk justifies it, not from the general population. Suppression (0.55) reflects that enforcement is targeted and subject to judicial review, not blanket. Theater ratio (0.25) is low-moderate: the threshold test is functional, but some jurisdictions perform proportionality analysis ritualistically while applying mandates broadly. Accessibility collapse (0.50) is moderate — targeted subjects have constrained exit (change jobs, seek exemptions) but not total closure. Resistance (0.60) is significant from both excluded seats (autonomy absolutists, anti-vaccine movements) and from payer-seat actors challenging threshold calibration.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seats, the constraint is a calibrated coordination mechanism that prevents the greater harm of blanket mandates. From the payer seat, it is an extraction mechanism whose trigger threshold is set by others and whose actuarial basis they cannot independently verify. From the excluded seats, it is a legitimating veneer for state coercion. The engine computes these divergent classifications from the structural data — the claimed_type (tangled_rope) captures the coordinator's view, but payer seats may compute as snare if the threshold is routinely met.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, legislatures, and courts are agenda_setters (d near 0.0 — they control the threshold). Vulnerable populations and public health infrastructure are beneficiaries (d low — they gain protection). Targeted_mandate_subjects are payers (d high — they bear compliance costs when threshold triggers). Bodily_autonomy_absolutists and anti_vaccine_movements are excluded (d undefined — they are outside the framework's coordination logic). Observers (bioethicists, epidemiologists) sit at d=0.5 analytically. The threshold mechanism means payer status is conditional — the same individual may be a payer in a pandemic wave and not between waves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling public health authority with bodily integrity) remains contested — the proportionality framework has not resolved it but institutionalized the contest. Mandatrophy is not resolved: the framework persists because neither extreme reading has displaced it, not because the founding problem is solved. The constraint's extraction has drifted upward over the interval (0.25→0.45) as threshold triggers expanded from narrow (specific outbreaks) to broader (endemic disease management), suggesting mandatrophy drift toward snare if the trend continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the risk_stratification_reading a stable constitutional doctrine or an unstable compromise that will collapse into one of the extreme readings?',
    'Longitudinal analysis of constitutional court decisions: if proportionality analysis becomes increasingly deferential (collapsing to public_health_primacy) or increasingly strict (collapsing to bodily_autonomy_primacy), the middle reading is transient.',
    'If the reading is transient, its claimed_type (tangled_rope) describes a temporary doctrinal state, not a stable constraint. The constraint story would need a sunset_clause or reclassification as scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the risk-stratification middle ground is a stable equilibrium or a transitional doctrine.').

omega_variable(
    threshold_operationalization,
    'What actuarial risk threshold (R0, IFR, hospitalization rate, etc.) and what evidentiary standard actually trigger the mandate? The framework is indeterminate without this specification.',
    'Comparative analysis of statutory thresholds and judicial applications across jurisdictions; empirical study of how often thresholds are met vs. claimed.',
    'A low threshold with weak evidence collapses the reading into public_health_primacy (de facto blanket mandates). A high threshold with strong evidence collapses it toward bodily_autonomy_primacy (mandates never triggered). The constraint''s actual extractiveness depends entirely on this calibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_operationalization, empirical, 'The indeterminacy of the actuarial threshold that makes the constraint''s operational extraction unknowable without specification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, employment consequences) or internalized (professional ethics, social pressure on healthcare workers to comply)?',
    'Post-mandate surveys of targeted subjects: if compliance persists after legal mandate lifts, internalized suppression is significant.',
    'If internalized, effective suppression is higher than legal measures suggest — the constraint operates through professional identity as well as law. This would increase payer-seat extraction beyond the authored 0.45.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in targeted mandate enforcement.').

omega_variable(
    victim_set_variability,
    'How does the victim set (targeted_mandate_subjects) change when the threshold definition shifts between epidemiological indicators (R0 vs. IFR vs. hospital capacity)?',
    'Scenario modeling: apply different threshold definitions to the same epidemiological data and map the resulting mandated populations.',
    'If victim set composition shifts dramatically (e.g., from healthcare workers to all essential workers), the constraint''s extraction redistributes across populations — a structural instability in the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_variability, empirical, 'Sensitivity of the victim set to threshold definition — the declared structural delta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vml_rsr_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vml_rsr_tr_t6, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(vml_rsr_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(vml_rsr_tr_t18, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(vml_rsr_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(vml_rsr_tr_t30, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(vml_rsr_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(vml_rsr_be_t6, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(vml_rsr_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(vml_rsr_be_t18, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement(vml_rsr_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(vml_rsr_be_t30, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vml_rsr_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vml_rsr_su_t6, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(vml_rsr_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(vml_rsr_su_t18, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(vml_rsr_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(vml_rsr_su_t30, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__risk_stratification_reading, 0.1).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the vaccine_mandate_legitimacy constraint family. Each reading instantiates a different constraint from the same kernel with different ε values, victim sets, and coordination/extraction balances. The risk_stratification_reading has moderate ε (0.45) because it limits mandates to threshold-meeting cases; bodily_autonomy_primacy_reading would have ε≈0 (no mandates = no extraction) but high suppression for those who want mandates; public_health_primacy_reading would have higher ε (broader mandates) but lower suppression per mandate (less judicial scrutiny).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__risk_stratification_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
