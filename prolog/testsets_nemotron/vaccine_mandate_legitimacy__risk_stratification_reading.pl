% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Risk-Stratified Vaccine Mandate Legitimacy
 *   domain: public_health/constitutional/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the risk_stratification_reading of the
 *   contested kernel 'vaccine_mandate_legitimacy.' It holds that state
 *   coercion to vaccinate is legitimate only when an actuarial risk threshold
 *   is met — the infection fatality rate, transmission risk, or healthcare
 *   system strain must exceed a defined level for the specific subpopulation
 *   targeted. Blanket mandates fail proportionality; targeted mandates (by
 *   age, occupation, comorbidity, local epidemiology) are permissible. The
 *   constraint coordinates protection for the vulnerable while limiting
 *   coercion to where the numbers justify it. But the threshold is a moving
 *   target: variant evolution, waning immunity, and modeling disputes mean
 *   the mandate boundary shifts, creating a theater of 'following the
 *   science' while the threshold itself is a policy choice. Extraction falls
 *   on those just above the line (low-risk adults in surge periods) and those
 *   wrongly denied exemptions; beneficiaries are the clinically vulnerable
 *   and the institutional apparatus that administers the system. The
 *   constraint requires active enforcement (mandate thresholds don't
 *   self-execute) and has identifiable victims — a tangled rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.35).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Risk-Stratified Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '420d65b8-1903-4331-a311-7569e0d133e5').
narrative_ontology:cs_kernel_codification('420d65b8-1903-4331-a311-7569e0d133e5', distributed).
narrative_ontology:cs_authority_grounding('420d65b8-1903-4331-a311-7569e0d133e5', practice).
narrative_ontology:cs_interpretation_layer_present('420d65b8-1903-4331-a311-7569e0d133e5').
narrative_ontology:cs_reading_relation('420d65b8-1903-4331-a311-7569e0d133e5', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('420d65b8-1903-4331-a311-7569e0d133e5', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('420d65b8-1903-4331-a311-7569e0d133e5', foundational, proportionality_requires_actuarial_justification).
narrative_ontology:cs_axiom_status(proportionality_requires_actuarial_justification, holdable).
narrative_ontology:cs_axiom_grounding('420d65b8-1903-4331-a311-7569e0d133e5', proportionality_requires_actuarial_justification, conventional).
narrative_ontology:cs_axiom('420d65b8-1903-4331-a311-7569e0d133e5', foundational, least_restrictive_means_applies_to_public_health_coercion).
narrative_ontology:cs_axiom_status(least_restrictive_means_applies_to_public_health_coercion, holdable).
narrative_ontology:cs_axiom_grounding('420d65b8-1903-4331-a311-7569e0d133e5', least_restrictive_means_applies_to_public_health_coercion, conventional).
narrative_ontology:cs_reference_frame('420d65b8-1903-4331-a311-7569e0d133e5', proportionality_constrained_mandate_authority).
narrative_ontology:cs_drift_state('420d65b8-1903-4331-a311-7569e0d133e5', post_emergency_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('420d65b8-1903-4331-a311-7569e0d133e5', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_exposure_occupational_groups).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_infrastructure).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_unvaccinated_adults).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_hesitant_with_medical_exemptions_denied).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, employers_enforcing_mandates_under_legal_threat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, employers_enforcing_mandates_under_legal_threat).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, high_exposure_occupational_groups).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, proportionality_principle_in_public_health_law).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, least_restrictive_means_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, actuarial_justification_for_state_coercion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and adjusts the actuarial risk threshold that triggers mandate authority; issues guidance, allocates vaccines, and bears political accountability for outcomes. Can revise thresholds as evidence evolves but faces pressure to maintain or expand mandates once issued.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, state_public_health_agency, agenda_setter,
    institutional, generational, analytical, national).

% Depend on high community vaccination rates for indirect protection; cannot safely vaccinate or mount adequate immune response. Gain direct survival benefit from targeted mandates covering their contacts and caregivers. Have no exit from vulnerability; advocacy is their only leverage.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_populations, beneficiary,
    organized, biographical, constrained, national).

% Healthcare workers, first responders, congregate care staff — mandated first under risk-stratified rules. Gain workplace safety and reduced transmission to vulnerable patients; bear compliance costs, career risk from refusal, and moral injury from coercing colleagues. Exit means leaving profession.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_exposure_occupational_groups, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, high_exposure_occupational_groups, payer).

% Adults below the actuarial risk threshold who remain unvaccinated by choice. Subject to targeted mandates only when local epidemiology crosses threshold (outbreaks, variant surges). Bear restriction of movement, employment conditions, and social access. Can often avoid mandates by relocating, changing jobs, or waiting for threshold to drop.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_unvaccinated_adults, payer,
    moderate, biographical, mobile, national).

% Individuals with genuine medical contraindications or sincere religious objections whose exemption requests are denied under narrow statutory criteria. Bear full mandate force without the risk profile that justifies it. No effective exit — cannot vaccinate, cannot get exemption, cannot easily escape jurisdiction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_hesitant_with_medical_exemptions_denied, payer,
    powerless, biographical, trapped, national).

% Private and public employers legally required to enforce vaccination as condition of employment. Gain workforce stability and reduced liability; bear enforcement costs, litigation risk, workforce attrition, and morale damage. Caught between state mandate and employee resistance; cannot exit the role without ceasing operations.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, employers_enforcing_mandates_under_legal_threat, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, employers_enforcing_mandates_under_legal_threat, beneficiary).

% Adjudicate proportionality challenges to mandate thresholds; define the legal standard for 'actuarial justification' and 'least restrictive means.' Their rulings set the boundary conditions for the constraint's operation. Do not enforce or bear costs directly.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Produces the risk estimates and transmission models that thresholds reference. Their methodological choices (IFR estimates, contact matrices, variant assumptions) directly determine who falls above/below the mandate line. Contested terrain — models are policy inputs, not neutral facts.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, epidemiological_modeling_community, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of achieving sufficient population immunity to protect those who cannot protect themselves, while minimizing the coercive footprint by restricting mandates to subpopulations where the actuarial benefit exceeds the liberty cost.
% TRANSFER_FUNCTION: Moves liberty interests (bodily autonomy, freedom of movement, employment access) from low-risk unvaccinated individuals and marginal exemption-denied persons to immunocompromised populations and high-exposure workers in the form of reduced infection risk; moves enforcement burden and litigation risk to employers and state agencies.
% ABSENT_VOICES: Children and adolescents in jurisdictions where mandates apply only to adults — their transmission role is epidemiologically significant but their liberty interests are unrepresented in the threshold calculus. Future cohorts who inherit the precedent of actuarial justification for state bodily intrusion. Global South populations excluded from the modeling data that threshold-setting relies on.
% DISAPPEARANCE_RATIONALE: If the risk-stratified mandate framework vanished, blanket mandates would either expand (public health primacy reading) or collapse entirely (bodily autonomy reading). Immunocompromised populations would lose their primary structural protection. Employers would face a binary: require all vaccines or none. The actuarial threshold itself is a coordination device — its disappearance forces a regime choice.
% FOUNDING_PROBLEM: The 2020-2021 pandemic revealed that blanket vaccine mandates triggered massive resistance, legal defeat, and political polarization while failing to protect the most vulnerable — because mandates applied uniformly regardless of individualized risk-benefit calculus, violating proportionality principles embedded in constitutional law and public health ethics.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and bioethics commissions (Nuffield Council, WHO SAGE) attest the founding problem is live: proportionality requires risk-stratified coercion. Constitutional courts in multiple jurisdictions (German BVerfG, US Supreme Court NFIB/OSHA, Canadian Charter challenges) have ruled blanket mandates disproportionate while upholding targeted ones — corroboration from outside the beneficiary set. Vaccine-hesitant advocacy groups and libertarian legal orgs attest the problem is manufactured: the 'blanket mandate failure' was a feature of political choice, not structural necessity.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects that the constraint transfers liberty from a defined but shifting victim set to a defined beneficiary set — not total extraction, not zero. Suppression (0.35) is moderate: mandates are enforced but exits exist (relocation, job change, waiting out surges) and resistance is legally recognized (exemption processes, judicial review). Theater ratio (0.28) captures the gap between 'actuarial threshold' as a technical criterion and its reality as a negotiated political boundary — modeling choices, lagging data, and variant uncertainty make the threshold performative in part. Accessibility collapse (0.55): alternatives (NPIs, targeted protection, voluntary vaccination) persist but are treated as failed once the threshold triggers. Resistance (0.62) is high: legal challenges, political mobilization, and compliance evasion are sustained. The cyclical measurement pattern (peaks at T=12 corresponding to Delta/Omicron waves) reflects the constraint's epidemic-driven dynamics.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a precision instrument: coercion applied only where the math demands it. From the trapped payer seat (exemption-denied), it is a blunt instrument: the math was wrong for them, but the machinery doesn't reverse. From the mobile payer seat, it is a conditional tax: pay when the threshold triggers, exit when it drops. The engine will compute these divergences from the structural data — the claimed type (tangled_rope) asserts both coordination and extraction are real and neither reduces to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health agency sits at d≈0.15 (beneficiary end) — it administers the constraint and gains legitimacy/power from it, though it bears political risk. Immunocompromised populations and high-exposure workers are beneficiaries (d≈0.1-0.2) — they gain protection without bearing enforcement cost. Low-risk unvaccinated adults are symmetric-to-target (d≈0.55-0.7 depending on surge phase) — they bear the mandate when triggered but have mobile exit. Exemption-denied individuals are full targets (d≈0.9) — trapped, no exit, bear full coercion without the risk profile. Employers are payers with secondary benefit (d≈0.5) — caught in the middle. Courts and modelers are analytical observers (d≈0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proportionality failure of blanket mandates) is contested but not dead — new variants, new vaccines, new population immunity profiles keep the actuarial calculus live. Mandatrophy would occur if the threshold becomes a ratchet (only rises, never falls) or if the exemption-denied population grows without review. Current theater ratio (0.28) suggests the coordination function still carries weight; if theater crosses 0.5 while extractiveness holds, the constraint drifts toward piton — maintained for appearance after the epidemiological justification fades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_definition_ambiguity,
    'What constitutes the actuarial risk threshold — a fixed IFR number, a healthcare capacity metric, a transmission rate, or a composite index? Who decides, and how often is it revised?',
    'Statutory definition with mandatory periodic review by an independent body; judicial enforcement of review deadlines; transparency requirements for model inputs.',
    'If the threshold is a single fixed metric, the constraint is more rule-like (rope-ward). If it is a discretionary composite, the constraint is more discretionary (snare-ward). The victim set size varies directly with threshold definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_definition_ambiguity, conceptual, 'Whether the actuarial threshold is a determinate rule or a discretionary standard.').

omega_variable(
    coordination_collapse_risk,
    'Does the risk-stratified reading actually coordinate between the extreme readings, or does it collapse into one of them under pressure (e.g., during a severe wave, does ''targeted'' become ''universal'' in practice)?',
    'Longitudinal study of mandate scope during epidemic waves: track whether targeted mandates expand to universal coverage and whether they retract post-wave.',
    'If the constraint collapses to public_health_primacy under stress, its claimed proportionality is performative — the coordination function is a fair-weather device. If it holds the line, the threshold is a genuine structural brake.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_collapse_risk, empirical, 'Whether the proportionality constraint holds under epidemic pressure or ratchets toward universal mandates.').

omega_variable(
    exemption_denial_mechanism,
    'Are medical/religious exemptions denied because criteria are genuinely narrow, or because administrators apply them restrictively to protect the mandate''s epidemiological target?',
    'Administrative data on exemption grant rates vs. applications; qualitative study of decision-maker reasoning; comparison across jurisdictions with identical criteria.',
    'If denials are administrative restriction, the victim set (exemption-denied) is larger than the statutory design — the constraint extracts from people it claims to protect. This would increase effective extraction for the powerless seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_denial_mechanism, empirical, 'Whether exemption denials reflect statutory design or administrative drift.').

omega_variable(
    modeling_capture_risk,
    'Do the epidemiological models that set thresholds reflect independent science, or are they shaped by the policy demand for mandate justification?',
    'Pre-registration of modeling protocols; blind forecasting tournaments; structural separation of modeling bodies from mandate-issuing agencies.',
    'If models are captured, the actuarial threshold is endogenous to the mandate apparatus — the coordination story is circular. This would reclassify the constraint toward snare (coordination as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modeling_capture_risk, conceptual, 'Whether the evidentiary basis for the threshold is independent of the mandate it justifies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vml_rsr_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vml_rsr_tr_t0, observed).
narrative_ontology:measurement(vml_rsr_tr_t6, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(vml_rsr_tr_t6, observed).
narrative_ontology:measurement(vml_rsr_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement_basis(vml_rsr_tr_t12, observed).
narrative_ontology:measurement(vml_rsr_tr_t18, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 18, 0.29).
narrative_ontology:measurement_basis(vml_rsr_tr_t18, observed).
narrative_ontology:measurement(vml_rsr_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(vml_rsr_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(vml_rsr_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(vml_rsr_be_t0, observed).
narrative_ontology:measurement(vml_rsr_be_t6, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement_basis(vml_rsr_be_t6, observed).
narrative_ontology:measurement(vml_rsr_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(vml_rsr_be_t12, observed).
narrative_ontology:measurement(vml_rsr_be_t18, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 18, 0.41).
narrative_ontology:measurement_basis(vml_rsr_be_t18, observed).
narrative_ontology:measurement(vml_rsr_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement_basis(vml_rsr_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(vml_rsr_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(vml_rsr_su_t0, observed).
narrative_ontology:measurement(vml_rsr_su_t6, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(vml_rsr_su_t6, observed).
narrative_ontology:measurement(vml_rsr_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(vml_rsr_su_t12, observed).
narrative_ontology:measurement(vml_rsr_su_t18, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 18, 0.37).
narrative_ontology:measurement_basis(vml_rsr_su_t18, observed).
narrative_ontology:measurement(vml_rsr_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement_basis(vml_rsr_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__risk_stratification_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint family (vaccine_mandate_legitimacy) decomposes the single colloquial label 'vaccine mandate legitimacy' into three structurally distinct readings with different ε, different victim sets, and different coordination/extraction profiles. The risk_stratification_reading claims a proportionality brake that the other two reject (primacy readings) or absolutize (autonomy reading). ε differs: public_health_primacy has higher ε (broader victim set), bodily_autonomy has ε≈0 for the mandate itself (coercion is the constraint) but high ε for the prohibition on mandates. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__risk_stratification_reading, organized, 0.35).
constraint_indexing:directionality_override(vaccine_mandate_legitimacy__risk_stratification_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
