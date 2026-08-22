% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality-Conditioned Vaccine Mandate
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint embodies a proportionality reading of vaccine mandate
 *   legitimacy: mandates are permissible only when disease severity,
 *   transmission risk, and vaccine safety jointly satisfy strict
 *   proportionality thresholds, with robust exemption pathways. It is one
 *   reading of the contested kernel 'vaccine_mandate_balance', distinct from
 *   a categorical public-health-primacy reading (which treats collective
 *   protection as superseding individual consent whenever voluntary
 *   compliance fails) and a categorical bodily-autonomy reading (which treats
 *   state-compelled medical intervention as inviolably impermissible). The
 *   proportionality reading makes mandate legitimacy context-dependent and
 *   pathogen-specific: a smallpox-like pathogen (high severity, high
 *   transmission, safe vaccine) easily clears the threshold; seasonal
 *   influenza does not. Both victim sets (those burdened by mandates and
 *   those endangered by their absence) are conditional on epidemiological
 *   parameters, not fixed categories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.45).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.35).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality-Conditioned Vaccine Mandate").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '5d9119b4-b745-490e-a043-97ac55db5778').
narrative_ontology:cs_kernel_codification('5d9119b4-b745-490e-a043-97ac55db5778', formalized).
narrative_ontology:cs_authority_grounding('5d9119b4-b745-490e-a043-97ac55db5778', lineage).
narrative_ontology:cs_interpretation_layer_present('5d9119b4-b745-490e-a043-97ac55db5778').
narrative_ontology:cs_reading_relation('5d9119b4-b745-490e-a043-97ac55db5778', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_reading_relation('5d9119b4-b745-490e-a043-97ac55db5778', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('5d9119b4-b745-490e-a043-97ac55db5778', foundational, mandate_legitimacy_requires_proportionality_thresholds).
narrative_ontology:cs_axiom_status(mandate_legitimacy_requires_proportionality_thresholds, holdable).
narrative_ontology:cs_axiom_grounding('5d9119b4-b745-490e-a043-97ac55db5778', mandate_legitimacy_requires_proportionality_thresholds, conventional).
narrative_ontology:cs_axiom('5d9119b4-b745-490e-a043-97ac55db5778', foundational, exemptions_must_be_robust_and_accessible).
narrative_ontology:cs_axiom_status(exemptions_must_be_robust_and_accessible, holdable).
narrative_ontology:cs_axiom_grounding('5d9119b4-b745-490e-a043-97ac55db5778', exemptions_must_be_robust_and_accessible, deontological).
narrative_ontology:cs_reference_frame('5d9119b4-b745-490e-a043-97ac55db5778', constitutional_proportionality_balancing).
narrative_ontology:cs_drift_state('5d9119b4-b745-490e-a043-97ac55db5778', post_covid_mandate_litigation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5d9119b4-b745-490e-a043-97ac55db5778', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, healthcare_infrastructure).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_advocates).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, religious_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, low_risk_demographics).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, proportionality_principle_in_public_health).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, least_restrictive_means_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, contextual_constitutional_balancing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce mandate policies calibrated to disease severity, transmission dynamics, and vaccine safety data. Must justify thresholds through transparent epidemiological modeling and revise mandates as conditions change. Bear political accountability for both action and inaction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, biographical, mobile, national).

% Cannot be vaccinated or mount adequate immune response; depend on community-level protection to participate in society. Face lethal risk from uncontrolled transmission. No exit from vulnerability; their survival depends on others' compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Avoids collapse from surge capacity demands when mandates reduce severe disease burden. Staff exhaustion and resource diversion prevented. Institutional survival tied to mandate effectiveness but cannot individually opt out of the system.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, healthcare_infrastructure, beneficiary,
    organized, biographical, constrained, regional).

% Face employment, education, and movement restrictions for non-compliance. Bear perceived or actual vaccine risks. Can sometimes access exemptions but process is burdensome and uncertain. Exit requires significant life restructuring or relocation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_individuals, payer,
    moderate, biographical, constrained, national).

% Organize legal challenges and political opposition framing mandates as categorical rights violations. Identity fused to principle of inviolable consent; concession on any mandate feels like principle surrender. Cannot exit advocacy without identity dissolution.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_advocates, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_advocates, excluded).

% Claim exemption under sincere religious belief; face scrutiny of belief sincerity and often denial when mandates deemed proportionate. Exit from belief community is existential; compliance experienced as spiritual violation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, religious_objectors, payer,
    moderate, biographical, identity_locked, national).

% Young, healthy populations with minimal personal risk from disease. Bear mandate costs (side effects, autonomy loss) primarily for others' benefit. Can often avoid mandates through geographic mobility or low-enforcement jurisdictions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, low_risk_demographics, payer,
    moderate, immediate, mobile, national).

% Adjudicate proportionality challenges: whether mandate thresholds are evidence-based, exemptions are genuinely accessible, and less restrictive alternatives were exhausted. Their rulings set the operational boundary of the constraint.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns individual vaccination decisions with collective epidemic control when voluntary uptake is insufficient to protect vulnerable populations and prevent healthcare collapse; provides a structured, evidence-gated mechanism for temporary liberty restriction.
% TRANSFER_FUNCTION: Moves compliance burden (vaccination, exemption navigation, restriction acceptance) from vaccine-hesitant, autonomy-prioritizing, religious, and low-risk individuals to the collective benefit of immunocompromised persons, healthcare system stability, and public health authority legitimacy.
% ABSENT_VOICES: Future generations who inherit precedent on state bodily intrusion authority; global populations in jurisdictions without vaccine access who bear pandemic externalities without mandate protections; children unable to consent whose parents' exemption claims determine their exposure.
% DISAPPEARANCE_RATIONALE: Without proportionality-gated mandates, either uncontrolled transmission harms vulnerable populations and collapses healthcare (if mandates vanish entirely) or categorical mandates persist without evidence thresholds (if public_health_primary reading dominates). The proportionality structure itself — evidence thresholds, exemption robustness, sunset review — is what makes the arrangement calibratable rather than binary.
% FOUNDING_PROBLEM: Historical tension between state police power to protect public health (Jacobson v. Massachusetts, 1905) and evolving constitutional recognition of bodily integrity, informed consent, and religious liberty; exacerbated by COVID-19 mandate variation revealing no shared framework for when mandates are legitimate.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside beneficiary institutions (e.g., Gostin & Wiley on public health law evolution; Griffin on constitutional avoidance in pandemic response; libertarian and civil liberties organizations' amicus briefs across multiple mandate cases) attest the founding problem persists: no stable doctrinal equilibrium exists between collective protection and individual refusal rights.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.45) reflects that mandates extract significant bodily autonomy and compliance costs from refusers, but only when epidemiological conditions justify it — extraction varies with pathogen. Suppression (0.35) is moderate: enforcement exists (employment/school exclusion, fines) but exemptions provide pressure relief; suppression spikes during acute phases (2021) and recedes as mandates narrow. Theater ratio (0.22) is low-moderate: the proportionality framework is genuinely operationalized in many jurisdictions (evidence thresholds, review cadences, exemption processes) though performative compliance theater exists. Accessibility collapse (0.38) reflects that alternatives (masking, testing, remote work) persist but degrade under high transmission. Resistance (0.42) is substantial: legal challenges, political mobilization, and non-compliance are persistent but not universal.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat, the constraint is coordination: a calibrated tool that activates only when needed. From identity-locked payer seats (bodily autonomy advocates, religious objectors), it is extraction: a mechanism that can always find 'proportionality' when the state wants to mandate. From trapped beneficiaries (immunocompromised), it is essential protection that may be withdrawn prematurely. The engine computes per-seat type from these structural asymmetries; the claimed_type (tangled_rope) captures the hybrid coordination/extraction nature at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters with institutional power and mobile exit (can resign, move jurisdictions) — directionality near beneficiary end. Immunocompromised populations are trapped beneficiaries (powerless, no exit from vulnerability) — directionality strongly beneficiary. Healthcare infrastructure is organized beneficiary with constrained exit. Vaccine-hesitant individuals are payers with constrained exit (life restructuring required). Bodily autonomy advocates are identity-locked payers (concession = identity dissolution). Religious objectors are identity-locked payers. Low-risk demographics are payers with mobile exit. Constitutional courts are analytical observers. The proportionality structure creates conditional directionality: as disease parameters shift, who is beneficiary vs. payer shifts — this is the reading's defining feature.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading avoids mandatrophy by making mandate legitimacy explicitly conditional and reversible: when the founding problem (acute threat to vulnerable populations and healthcare capacity) recedes, the mandate must sunset or narrow. This contrasts with public_health_primary (which risks becoming snare by treating mandates as default) and bodily_autonomy_primary (which risks becoming mountain by treating any mandate as categorically illegitimate). The proportionality structure's sunset logic is its mandatrophy defense — but only if exemption robustness and evidence thresholds are genuinely enforced, not performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_operationalization,
    'Can strict proportionality thresholds be operationalized in a way that prevents authorities from always finding proportionality when they want to mandate?',
    'Comparative analysis of jurisdictions with codified proportionality frameworks (e.g., German Infection Protection Act §28a, Canadian Oakes test applications) vs. those without: do codified thresholds constrain executive discretion measurably?',
    'If thresholds are manipulable, the reading collapses toward public_health_primary in practice (extraction disguised as coordination); if thresholds genuinely bind, the reading maintains its tangled_rope character with real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_operationalization, empirical, 'Whether proportionality thresholds function as genuine constraints on authority or as rhetorical cover.').

omega_variable(
    exemption_robustness_vs_coordination,
    'How robust must exemptions be before they undermine the coordination function (herd immunity) the mandate exists to achieve?',
    'Epidemiological modeling of exemption rates vs. effective reproduction number across pathogen profiles; legal analysis of exemption denial rates and judicial review standards.',
    'If robust exemptions necessarily break coordination, the reading contains an internal contradiction (coordination function requires exemption weakness); if exemptions can be robust without breaking coordination, the tangled_rope structure is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exemption_robustness_vs_coordination, conceptual, 'Structural tension between exemption robustness and coordination efficacy.').

omega_variable(
    kernel_reading_boundary_proportionality_vs_public_health,
    'Does the proportionality reading foreclose the public_health_primary reading, or do they coexist as live positions in constitutional discourse?',
    'Doctrinal analysis: can a single constitutional framework simultaneously hold that (a) mandates require proportionality AND (b) collective protection supersedes individual consent when voluntary compliance fails? Or does accepting (a) logically entail rejecting (b)?',
    'If forecloses: the two readings cannot coexist in one legal system; the kernel has a binary structure. If coexists_with: both readings remain live across different courts/jurisdictions; the kernel is genuinely pluralistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_proportionality_vs_public_health, conceptual, 'Structural relationship between proportionality and public-health-primacy readings of the mandate kernel.').

omega_variable(
    kernel_reading_boundary_proportionality_vs_autonomy,
    'Does the proportionality reading foreclose the bodily_autonomy_primary reading, or do they coexist?',
    'Doctrinal analysis: does accepting that mandates can be proportionate in some circumstances logically entail rejecting the claim that bodily integrity is categorically inviolable? Or can one hold both that autonomy is prima facie inviolable AND that it yields to strict proportionality?',
    'If forecloses: proportionality reading and autonomy reading are mutually exclusive within a framework. If coexists_with: autonomy reading persists as a limiting principle that shapes proportionality analysis without being displaced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_proportionality_vs_autonomy, conceptual, 'Structural relationship between proportionality and bodily-autonomy readings.').

omega_variable(
    pathogen_specific_epsilon_variation,
    'How much does the constraint''s base extractiveness actually vary across pathogens (smallpox vs. COVID-19 vs. seasonal flu vs. HPV)?',
    'Comparative mandate policy analysis across pathogens with different severity/transmission/safety profiles; measurement of compliance costs, enforcement intensity, and exemption accessibility per pathogen.',
    'If ε variation is large (e.g., 0.7 for smallpox, 0.15 for flu), the reading''s context-dependence is structurally real. If ε variation is small, the proportionality framework may be nominal — the same extraction occurs regardless of pathogen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_specific_epsilon_variation, empirical, 'Whether the reading''s claimed pathogen-dependent ε variation is empirically realized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(vacc_tr_t2021, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2021, 0.42).
narrative_ontology:measurement(vacc_tr_t2022, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2022, 0.28).
narrative_ontology:measurement(vacc_tr_t2023, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2023, 0.18).
narrative_ontology:measurement(vacc_tr_t2024, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement(vacc_tr_t2025, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement(vacc_tr_t2026, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement(vacc_tr_t2027, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2027, 0.19).
narrative_ontology:measurement(vacc_tr_t2028, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2028, 0.21).
narrative_ontology:measurement(vacc_tr_t2029, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2029, 0.23).
narrative_ontology:measurement(vacc_tr_t2030, vaccine_mandate_balance__proportionality_reading, theater_ratio, 2030, 0.22).

% Extraction over time
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2022, 0.48).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2023, 0.38).
narrative_ontology:measurement(vacc_be_t2024, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement(vacc_be_t2025, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement(vacc_be_t2026, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2026, 0.43).
narrative_ontology:measurement(vacc_be_t2027, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2027, 0.41).
narrative_ontology:measurement(vacc_be_t2028, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2028, 0.4).
narrative_ontology:measurement(vacc_be_t2029, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2029, 0.42).
narrative_ontology:measurement(vacc_be_t2030, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 2030, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2022, 0.38).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2023, 0.25).
narrative_ontology:measurement(vacc_su_t2024, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2024, 0.3).
narrative_ontology:measurement(vacc_su_t2025, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2025, 0.35).
narrative_ontology:measurement(vacc_su_t2026, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2026, 0.32).
narrative_ontology:measurement(vacc_su_t2027, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2027, 0.3).
narrative_ontology:measurement(vacc_su_t2028, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2028, 0.31).
narrative_ontology:measurement(vacc_su_t2029, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2029, 0.33).
narrative_ontology:measurement(vacc_su_t2030, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 2030, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary_reading).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary_reading).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, school_vaccine_requirements).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, healthcare_worker_mandates).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, travel_vaccination_requirements).

% DUAL FORMULATION NOTE:
% This proportionality_reading decomposes the 'vaccine_mandate_balance' kernel alongside public_health_primary_reading and bodily_autonomy_primary_reading. The proportionality reading has ε that varies by pathogen (0.15–0.7), conditional victim sets, and requires active enforcement of evidence thresholds and exemptions. The public_health_primary reading has higher baseline ε (categorical mandate authority) and treats individual refusal as coordination failure. The bodily_autonomy_primary reading has near-zero ε for mandates (categorical prohibition) but high ε for disease spread on refusers. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, organized, 0.85).
constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, moderate, 0.75).
constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
