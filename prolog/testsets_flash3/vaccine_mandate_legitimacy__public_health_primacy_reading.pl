% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy (Public Health Primacy Reading)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'public health primacy' reading of vaccine
 *   mandate legitimacy, where the state's duty to prevent collective harm
 *   justifies mandate authority, and unvaccinated status is framed as a
 *   negative externality. This reading prioritizes collective well-being over
 *   individual autonomy when faced with public health crises. It is one of
 *   several competing interpretations of the underlying
 *   'vaccine_mandate_legitimacy' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.65).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.75).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'd7a571ba-8089-40b7-b730-ec9f01dab065').
narrative_ontology:cs_kernel_codification('d7a571ba-8089-40b7-b730-ec9f01dab065', formalized).
narrative_ontology:cs_authority_grounding('d7a571ba-8089-40b7-b730-ec9f01dab065', lineage).
narrative_ontology:cs_interpretation_layer_present('d7a571ba-8089-40b7-b730-ec9f01dab065').
narrative_ontology:cs_reading_relation('d7a571ba-8089-40b7-b730-ec9f01dab065', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d7a571ba-8089-40b7-b730-ec9f01dab065', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('d7a571ba-8089-40b7-b730-ec9f01dab065', foundational, collective_harm_prevention_duty).
narrative_ontology:cs_axiom_status(collective_harm_prevention_duty, holdable).
narrative_ontology:cs_axiom_grounding('d7a571ba-8089-40b7-b730-ec9f01dab065', collective_harm_prevention_duty, deontological).
narrative_ontology:cs_axiom('d7a571ba-8089-40b7-b730-ec9f01dab065', foundational, unvaccinated_status_is_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_is_externality, holdable).
narrative_ontology:cs_axiom_grounding('d7a571ba-8089-40b7-b730-ec9f01dab065', unvaccinated_status_is_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('d7a571ba-8089-40b7-b730-ec9f01dab065', public_health_emergency_doctrine).
narrative_ontology:cs_drift_state('d7a571ba-8089-40b7-b730-ec9f01dab065', post_acute_pandemic_phase, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d7a571ba-8089-40b7-b730-ec9f01dab065', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_public).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting population health, they issue and enforce vaccine mandates, viewing unvaccinated status as a collective externality. They benefit from enhanced authority and reduced disease burden.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced disease transmission and a sense of collective safety. They support mandates as a necessary measure for public good, bearing minimal direct costs from the mandate itself.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of mandates, including job loss, travel restrictions, and social exclusion. Their refusal is often tied to deeply held beliefs or distrust, making exit (vaccination) a high-cost identity-locked option.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals, payer,
    powerless, immediate, identity_locked, local).

% Argue against mandates on grounds of individual liberty and bodily autonomy. While they participate in legal challenges, their arguments are often sidelined by the public health primacy framing.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Benefit from reduced strain on resources due to lower hospitalization rates for vaccine-preventable diseases. They implement mandate policies and manage compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_systems, beneficiary,
    institutional, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity and reduce the spread of infectious diseases, ensuring public health and safety by treating unvaccinated status as a negative externality.
% TRANSFER_FUNCTION: Transfers the burden of disease prevention from the collective (vaccinated public, healthcare systems) to unvaccinated individuals through coercive measures, in exchange for collective health benefits.
% ABSENT_VOICES: Individuals and groups prioritizing absolute bodily autonomy or specific risk-stratification approaches are marginalized in this framing, which prioritizes collective good over individual choice when a public health threat is perceived.
% DISAPPEARANCE_RATIONALE: If the public health primacy justification for mandates vanished, the state's authority to impose such measures would collapse, leading to a significant shift in public health policy, individual behavior regarding vaccination, and potentially increased disease burden, requiring a complete reorganization of public health strategies.
% FOUNDING_PROBLEM: The problem of managing infectious disease outbreaks and preventing widespread illness and death, particularly when individual choices create collective risks.
% FOUNDING_PROBLEM_CORROBORATION: The World Health Organization and national public health bodies consistently corroborate the ongoing threat of infectious diseases and the need for collective action. Independent epidemiological studies support the efficacy of vaccination in reducing transmission and severe outcomes.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant costs imposed on unvaccinated individuals, while suppression (0.75) is high due to the active enforcement mechanisms (e.g., job loss, travel bans) required to maintain compliance. The theater ratio is low (0.10) because the public health function is genuinely active and not merely performative in this reading. Accessibility collapse is moderate (0.60) as alternatives to vaccination are severely restricted, but not entirely eliminated (e.g., some exemptions, remote work options). Resistance is high (0.70) due to strong opposition from individuals and advocacy groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this is a necessary and legitimate coordination mechanism. From the perspective of unvaccinated individuals, it is a coercive snare. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the vaccinated public are beneficiaries, gaining collective safety and reduced disease burden. Unvaccinated individuals are the primary victims, bearing the direct costs of compliance or exclusion. Healthcare systems also benefit from reduced strain. Civil liberties advocates are excluded, as their arguments are not central to this reading's justification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_quantification_ambiguity,
    'How precisely can the ''externality'' of unvaccinated status be quantified, and does it consistently exceed the costs of mandates?',
    'Longitudinal epidemiological studies comparing disease burden and healthcare costs in populations with and without mandates, controlling for other variables. Economic modeling of externality costs vs. individual mandate costs.',
    'If the externality is consistently low or mandate costs are disproportionately high, the justification for high extractiveness and suppression weakens, potentially reclassifying the constraint towards a Snare or Tangled Rope with less coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_quantification_ambiguity, empirical, 'Uncertainty in the precise quantification of the negative externality of unvaccinated status.').

omega_variable(
    proportionality_threshold_ambiguity,
    'At what threshold of collective risk does the public health duty override individual bodily autonomy, and is this threshold universally agreed upon?',
    'Philosophical and legal consensus-building on bioethical principles, or judicial rulings establishing clear proportionality tests for public health interventions.',
    'If the threshold is high or contested, the ''public health primacy'' reading''s legitimacy is weakened, potentially shifting the classification towards a more extractive type from the perspective of those whose autonomy is overridden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in the proportionality threshold for public health interventions overriding individual rights.').

omega_variable(
    mandate_effectiveness_drift,
    'Does the effectiveness of mandates in achieving public health goals drift over time, especially with evolving pathogen characteristics or vaccine efficacy?',
    'Ongoing real-world effectiveness studies of mandates against new variants and changing epidemiological landscapes. Public health policy reviews that adapt mandates based on current data.',
    'If mandates become less effective, the justification for their extractiveness and suppression erodes, potentially leading to a reclassification towards a Piton or a less legitimate Tangled Rope, as the coordination function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_effectiveness_drift, empirical, 'Temporal drift in the effectiveness of vaccine mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_legitimacy' kernel. Its structural properties differ significantly from sibling readings like 'bodily_autonomy_primacy_reading' and 'risk_stratification_reading', which emphasize individual rights or actuarial risk thresholds, respectively. Each reading constitutes a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
