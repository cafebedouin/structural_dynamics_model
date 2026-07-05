% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate as Collective-Protection Obligation (Public-Health-Primary Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the
 *   public_health_mandate_authority kernel: the mandate is framed as a
 *   collective obligation owed to those who cannot protect themselves
 *   biologically (the immunocompromised) or whose care depends on unsurged
 *   healthcare capacity. Under this reading, the immunocompromised enter the
 *   victim set only when the mandate fails or is weakened; the
 *   mandate-resistant population is framed not as rights-holders being
 *   violated but as externality-generators whose refusal transfers risk onto
 *   the vulnerable commons. This is a structurally distinct constraint from
 *   the bodily_autonomy_primary reading (which places the resistant
 *   individual in the victim set and treats the mandate itself as the
 *   violation) and from the proportionality_reading (which makes legitimacy a
 *   sliding-scale function of threat severity, alternatives, coercion
 *   magnitude, and duration rather than a categorical obligation). Each
 *   reading is authored as its own file with its own ε; this file does not
 *   average or hedge across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.58).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.62).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate as Collective-Protection Obligation (Public-Health-Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '8e1545ca-f8c8-4137-8bdf-e03d7a90910d').
narrative_ontology:cs_kernel_codification('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', distributed).
narrative_ontology:cs_authority_grounding('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', distributed).
narrative_ontology:cs_reading_relation('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', foundational, collective_protection_duty_overrides_individual_medical_refusal).
narrative_ontology:cs_axiom_status(collective_protection_duty_overrides_individual_medical_refusal, holdable).
narrative_ontology:cs_axiom_grounding('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', collective_protection_duty_overrides_individual_medical_refusal, deontological).
narrative_ontology:cs_axiom('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', foundational, unmitigated_transmission_risk_constitutes_actionable_externality).
narrative_ontology:cs_axiom_status(unmitigated_transmission_risk_constitutes_actionable_externality, holdable).
narrative_ontology:cs_axiom_grounding('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', unmitigated_transmission_risk_constitutes_actionable_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', collective_protection_duty_framework).
narrative_ontology:cs_drift_state('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', post_peak_transmission_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8e1545ca-f8c8-4137-8bdf-e03d7a90910d', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_system_capacity).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, compliant_population).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, immunocompromised_patients_when_mandate_fails).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, employers_and_institutions).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, collective_action_duty_to_the_vulnerable_commons).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, externality_based_limits_on_bodily_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces the mandate (vaccination, masking, or similar) tied to employment eligibility, school attendance, or service access. Frames the mandate as a duty owed to those who cannot protect themselves through immune response alone. Sets thresholds for compliance, exemption criteria, and enforcement mechanisms (termination, exclusion, fines).
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot mount full immune response themselves and depend structurally on the vaccination/precaution status of everyone around them (herd protection). When the mandate holds, they benefit from reduced circulating disease. When the mandate is weakened, exempted, or under-enforced, they absorb direct physical risk with no exit — they cannot opt into a safer commons alone.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, immunocompromised_patients, payer).

% Hospital and ICU capacity is a shared, finite resource; mandates that reduce transmission reduce surge load. Named for completeness as the collective good the mandate is built to preserve, not as an acting party.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_system_capacity, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(public_health_mandate_authority__public_health_primary, healthcare_system_capacity).

% Object to the mandated intervention on medical, religious, or personal grounds and face termination, exclusion from public accommodations, or loss of licensure as the enforcement mechanism. Under this reading they are framed as imposing an externality (transmission risk) on the vulnerable commons, not as bearing a rights violation — their exit is constrained to unemployment, relocation, or litigation.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_workers, payer,
    powerless, biographical, constrained, regional).

% Complies with the mandate at moderate personal cost and receives continued access to employment, services, and reduced disease burden in return. Bears little of the coercive weight because compliance itself satisfies the constraint.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, compliant_population, beneficiary,
    moderate, biographical, mobile, national).

% Implement and enforce the mandate as a condition of employment or service, often under legal requirement or liability pressure. Absorb administrative cost and workforce attrition when resistant employees exit rather than comply.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, employers_and_institutions, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, employers_and_institutions, payer).

% Adjudicate exemption claims, review mandate proportionality, and produce the record from which competing readings of the kernel (bodily autonomy vs. public health vs. proportionality) are argued.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, bioethics_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunity or precaution behavior so that people who cannot protect themselves through their own immune response (or through personal precaution alone) are shielded by the aggregate behavior of others, and so that finite healthcare capacity is not overwhelmed.
% TRANSFER_FUNCTION: Moves bodily and economic autonomy from mandate-resistant individuals (who must comply, exit their livelihood, or seek exemption) to the vulnerable commons (immunocompromised patients and the healthcare system), converting individual risk-bearing into collectively distributed protection.
% ABSENT_VOICES: Mandate-resistant individuals with genuine medical contraindications not covered by exemption categories are rarely heard in the framing that treats resistance as free-riding; their specific case is absorbed into the general 'resistant' category rather than examined on its own terms.
% DISAPPEARANCE_RATIONALE: Under this reading, if the mandate disappeared overnight, immunocompromised patients and healthcare capacity would face materially higher risk (the world rearranges for them) — but mandate-resistant workers would regain employment and bodily autonomy immediately (the world also rearranges for them, in the opposite direction). Whether the net verdict is 'world rearranges' or 'world unchanged' depends on which population's rearrangement is weighted, which is exactly the contest between this reading and its siblings.
% FOUNDING_PROBLEM: Communicable disease transmission in a population with an immunocompromised subgroup and finite healthcare capacity, where individual risk-taking imposes costs on others who cannot mitigate that risk themselves.
% FOUNDING_PROBLEM_CORROBORATION: Immunocompromised patient advocacy groups and hospital capacity data (outside the mandate-issuing authority) corroborate that the underlying epidemiological problem remains live during active transmission periods. Mandate-resistant workers and civil liberties organizations, from outside the beneficiary set, dispute that the mandate as currently enforced is still proportionate to the residual risk, particularly post-peak-transmission; this dispute is the live edge between this reading and the proportionality reading.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, contested).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply during active-transmission enforcement (0.35 to 0.62 across the first 18 months) as mandates tighten and coercive enforcement (job loss, service exclusion) intensifies, then eases slightly as transmission recedes and exemption processes mature — but does not return to baseline, reflecting persistent enforcement infrastructure. Suppression follows a similar but front-loaded curve: it climbs faster than extraction during the acute enforcement build-out (reflecting hardening compliance infrastructure) and plateaus rather than falls, since exemption and appeals machinery, once built, tends to persist. Theater ratio stays low throughout — enforcement in this reading is functionally tied to actual transmission-reduction goals, not performative; it rises only modestly as some late-stage compliance theater (badge checks, expired mandates left nominally in force) accumulates.
 *
 * PERSPECTIVAL GAP:
 *   From the public-health-authority and immunocompromised-beneficiary seats, the mandate reads as a functioning tangled rope: real coordination (herd protection) bundled with necessary enforcement against those who would free-ride. From the mandate-resistant-worker seat, the identical structure reads as coercive extraction with no meaningful exit — the same enforcement machinery, differently experienced. The engine computing divergent per-seat types from this one structural dataset is the intended output, not an inconsistency to fix.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised patients derive d near the beneficiary end when the mandate holds (protected by others' compliance) but shift sharply toward the target end in the counterfactual where mandates fail (trapped, no independent exit from the shared disease environment) — this dual position is why they appear as both beneficiary and conditional victim. Mandate-resistant workers sit near the full-target end: the mandate transfers cost onto them (employment, autonomy) to fund the collective good, and their exit options are constrained rather than mobile because employment and service access are broadly conditioned on compliance. Compliant population and employers occupy near-symmetric to moderately-beneficiary positions: they bear compliance cost but retain full access.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communicable disease threatening a vulnerable subpopulation and finite hospital capacity) is genuinely live during active transmission surges, which supports treating enforcement during those periods as coordination rather than pure extraction. But the founding-problem-corroboration split — advocacy groups for the vulnerable commons attest the problem persists, while civil liberties observers attest post-peak enforcement has outlived its proportionate justification — is exactly the seam between this reading and the proportionality reading. This story does not resolve that seam; it names it as an omega and leaves the founding-problem status as contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_public_health_primary,
    'Is the public-health-primary framing (mandate as obligation to the vulnerable commons, resistant individuals as externality-generators) the correct lens for evaluating this mandate, or does the bodily-autonomy-primary or proportionality framing better capture the structural reality?',
    'No empirical resolution exists — this is a genealogically contested kernel with three live readings authored as separate constraint files (public_health_primary, bodily_autonomy_primary, proportionality_reading), linked via network.affects_constraints. Which reading a given court, legislature, or ethics body adopts determines who is placed in the victim set.',
    'Under this reading, immunocompromised patients are victims only in the mandate-failure counterfactual and mandate-resistant workers are excluded from victimhood (framed as externality-generators). Under the bodily_autonomy_primary reading, the victim set inverts: mandate-resistant individuals become the primary victims and the mandate itself is the extractive structure. Under the proportionality_reading, victimhood is a sliding-scale function of threat severity and duration rather than fixed to either group.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_public_health_primary, conceptual, 'This story is one reading of the public_health_mandate_authority kernel; the reading selection itself is unresolved and framing-dependent.').

omega_variable(
    externality_framing_of_free_riders,
    'Is framing mandate-resistant individuals as free-riders imposing an externality on the vulnerable commons a defensible characterization, or does it presuppose the conclusion (that bodily autonomy is subordinate to collective risk-reduction) that the kernel contest is actually about?',
    'Compare against cases where externality framing is applied outside public health (e.g., risk-imposing behavior generally) to test whether the framing is applied consistently or only invoked to justify mandate enforcement.',
    'If the externality framing is question-begging, the extractiveness measured against mandate-resistant workers under-counts the rights cost this reading imposes on them; if the framing is sound, the measured extraction correctly reflects a legitimate cost of preventing harm to others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_framing_of_free_riders, conceptual, 'Whether treating mandate resistance as pure externality-generation is itself a contested normative move, not a neutral description.').

omega_variable(
    mandate_failure_counterfactual_measurement,
    'How should the ''immunocompromised as victims when mandate fails'' clause be operationalized — does partial compliance (e.g., 70% uptake) count as mandate failure, and at what uptake threshold does the vulnerable commons become measurably unprotected?',
    'Epidemiological threshold data (herd immunity thresholds vary by pathogen and setting) could establish a compliance level below which the protective function demonstrably fails, converting the counterfactual into an observed state.',
    'Without a clear threshold, the victim-set switch for immunocompromised patients is a binary authored assumption rather than a measured transition, which weakens the precision of the extractiveness trajectory during partial-compliance periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_failure_counterfactual_measurement, empirical, 'The mandate-failure condition that shifts immunocompromised patients into the victim set is not sharply defined by an uptake threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.12).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.15).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__public_health_primary, theater_ratio, 18, 0.19).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.2).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__public_health_primary, theater_ratio, 30, 0.21).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__public_health_primary, theater_ratio, 36, 0.22).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__public_health_primary, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__public_health_primary, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__public_health_primary, base_extractiveness, 36, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__public_health_primary, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__public_health_primary, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__public_health_primary, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the public_health_mandate_authority kernel, each authored as a separate constraint file with its own ε, beneficiary/victim structure, and classification. public_health_primary (this file) fixes the victim set as mandate-resistant individuals plus immunocompromised-under-failure; bodily_autonomy_primary inverts the victim set to center the mandate-resistant as rights-violation victims; proportionality_reading makes the victim set and ε a function of a sliding-scale proportionality test rather than fixing it categorically. All three link to each other via affects_constraints; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
