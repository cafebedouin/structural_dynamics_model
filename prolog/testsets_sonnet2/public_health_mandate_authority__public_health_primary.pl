% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Public Health Mandate as Commons-Protection Obligation (Public-Health-Primary Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the public
 *   health mandate authority kernel: the mandate is an obligation owed to the
 *   vulnerable commons — the immunocompromised and healthcare infrastructure
 *   — discharged through collective action that non-compliant individuals are
 *   structurally treated as failing to honor. Under this reading, when
 *   population coverage is adequate the mandate functions as genuine
 *   coordination protecting those who cannot protect themselves; when it
 *   fails or is under-enforced, the immunocompromised enter the victim set
 *   directly (exposed to transmission they cannot mitigate). The
 *   mandate-resistant are excluded from the victim set by this reading's own
 *   terms — their costs (termination, exclusion) are framed as internalizing
 *   an externality they were imposing, not as harms warranting independent
 *   moral weight. This is a distinct constraint from the
 *   bodily_autonomy_primary and proportionality_reading siblings, which
 *   assign different victim sets and different ε values from the same
 *   underlying mandate practice — see kernel_context.
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
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate as Commons-Protection Obligation (Public-Health-Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, 'd0a394b8-dbbd-41b0-abaf-7e56f402e7e2').
narrative_ontology:cs_kernel_codification('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', distributed).
narrative_ontology:cs_authority_grounding('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', distributed).
narrative_ontology:cs_reading_relation('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', foundational, collective_welfare_can_override_individual_bodily_sovereignty).
narrative_ontology:cs_axiom_status(collective_welfare_can_override_individual_bodily_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', collective_welfare_can_override_individual_bodily_sovereignty, instrumental).
narrative_ontology:cs_axiom('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', foundational, noncompliance_constitutes_externality_not_rights_claim).
narrative_ontology:cs_axiom_status(noncompliance_constitutes_externality_not_rights_claim, holdable).
narrative_ontology:cs_axiom_grounding('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', noncompliance_constitutes_externality_not_rights_claim, conventional).
narrative_ontology:cs_reference_frame('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', commons_protection_obligation_framework).
narrative_ontology:cs_drift_state('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', post_acute_outbreak_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0a394b8-dbbd-41b0-abaf-7e56f402e7e2', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_workforce).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, hospital_systems).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_employees).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, immunocompromised_patients_when_mandate_fails).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, healthcare_workforce).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, employers_and_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces vaccination or masking mandates for employment, schooling, or service access, justified as the minimum coordination needed to keep transmission below the threshold that overwhelms hospital capacity and endangers those who cannot be vaccinated. Administers exemption processes and penalty structures, and answers to elected officials and courts for the mandate's continuation.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Cannot reliably vaccinate or mount immune response themselves; their protection depends entirely on transmission suppression among the surrounding population achieved through others' compliance. Have no exit from dependency on collective compliance — cannot personally close the gap the mandate exists to close.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_patients, beneficiary,
    powerless, biographical, trapped, local).

% Depend on mandate-driven population immunity to keep admissions within surge capacity; staffing shortages during outbreak peaks directly threaten their ability to treat unrelated emergencies. Benefit structurally from high compliance but bear organizational strain when mandates are weakly enforced or contested in court.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, hospital_systems, beneficiary,
    organized, biographical, constrained, regional).

% Protected from occupational exposure by mandates covering colleagues and patients, but also frequently the population subject to workplace vaccination requirements themselves, facing termination for non-compliance. Their exit option is leaving the healthcare field entirely, which most cannot afford.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_workforce, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, healthcare_workforce, payer).

% Face termination, exclusion from schooling, or loss of service access for declining a mandated intervention, regardless of personal medical history, prior infection, or religious objection. Under this reading their refusal is treated as a negative externality imposed on the vulnerable commons, not as a rights claim warranting independent weight — their cost is the direct transfer this constraint imposes to secure population-level compliance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_employees, payer,
    powerless, biographical, constrained, national).

% Implement and enforce the mandate as a condition of employment or service, absorbing administrative cost, exemption litigation, and workforce attrition from termination of non-compliant staff, while also facing liability exposure if they fail to enforce and an outbreak results.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, employers_and_institutions, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, employers_and_institutions, payer).

% Argue that treating non-compliance purely as externality-imposition erases the independent weight of bodily autonomy claims; under the public-health-primary reading their framework is not admitted as a competing value, only as a cost to be overridden. They contest the reading from outside its own terms.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicate the scope of mandate authority, weighing public-health-primary claims against bodily-autonomy and proportionality claims, and can narrow, expand, or strike the mandate through litigation and statute.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Suppresses transmission below the threshold that would overwhelm hospital surge capacity and expose people who cannot be vaccinated or who have compromised immune systems to disproportionate risk they cannot mitigate individually.
% TRANSFER_FUNCTION: Moves the cost of population-level risk reduction — bodily submission to intervention, employment risk, exemption-litigation burden — from the immunocompromised and healthcare infrastructure onto individuals who would otherwise decline the intervention, treating their non-compliance as an externality rather than an autonomy claim.
% ABSENT_VOICES: Mandate-resistant individuals with religious, medical-history, or autonomy-based objections are heard only as the source of the externality being corrected, not as holders of a competing claim with independent standing; civil liberties advocates and the proportionality tradition are structurally excluded from this reading's own framework, though they appear as external critics.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished overnight, compliance would fall toward baseline voluntary uptake, transmission among the unprotected would rise, and immunocompromised individuals and hospital systems would bear the resulting exposure and surge risk with no institutional mechanism currently substituting for the coordination the mandate provides under this reading.
% FOUNDING_PROBLEM: Individually rational non-compliance (perceived low personal risk, distrust, cost of compliance) aggregates into population-level transmission that harms people who cannot protect themselves and strains finite healthcare capacity — a classic collective-action failure that voluntary measures alone did not resolve during acute outbreak periods.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and hospital systems attest the problem remains live wherever vaccination or transmission-control coverage is incomplete and immunocompromised populations remain exposed. Independent epidemiological modeling from outside the mandate-administering agencies (academic transmission-dynamics research) corroborates that coverage gaps causally elevate risk to the unprotected commons; civil liberties advocates and some public-health ethicists outside the enforcing institutions dispute whether the mandate remains the least-restrictive means once alternative mitigations (testing, ventilation, voluntary uptake plateaus) are available, making the status contested rather than settled even among non-beneficiary observers.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58) reflects the real coercive cost imposed on mandate-resistant employees (job loss, exclusion) as the price of closing the collective-action gap; it is not zero because the reading concedes genuine coercion even while framing it as legitimate cost-internalization. Suppression (0.62) captures that exit from the mandate is genuinely constrained by employment and service dependency, not merely inconvenient. The oscillating measurement series (extraction and suppression rising through outbreak peaks at t=12-18, then relaxing as case counts fall and voluntary uptake plateaus) reflects that mandate enforcement in this domain is crisis-cyclical rather than monotonic — enforcement intensity tracks epidemiological severity, which is a feature of the coordination problem, not drift alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised patients and hospital systems are the structural beneficiaries under this reading — the mandate's entire justification is protecting them, and they have essentially no independent capacity to substitute for population compliance (their exit options are trapped/constrained). Mandate-resistant employees are the structural targets: the reading assigns them the cost of the externality-correction with no independent autonomy claim recognized in-frame. Public health authorities and employers occupy dual agenda-setter/payer positions — they administer the mandate but also absorb enforcement and litigation cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (collective-action failure exposing the unprotected commons) is genuinely contested rather than resolved or clearly dead: in high-transmission periods it is plainly live (corroborated by epidemiological modeling independent of enforcing agencies); in low-transmission, high-voluntary-uptake periods, continued mandate enforcement risks becoming disproportionate to residual risk, which is exactly the dispute the proportionality_reading sibling exists to adjudicate. This reading does not resolve that tension — it asserts the commons-protection obligation as primary and treats proportionality concerns as external.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_framing_versus_autonomy_claim,
    'Is non-compliance genuinely best modeled as an externality imposed on the vulnerable commons, or does treating it that way pre-empt an autonomy claim that has independent moral standing regardless of aggregate effect?',
    'This is the precise fault line between this reading and the bodily_autonomy_primary sibling; it is not resolvable by data internal to either reading — it is a conceptual/normative disagreement about whether collective welfare consequences can override bodily sovereignty, tracked here as the committer-axis question rather than folded into this constraint''s own classification.',
    'If the autonomy claim is granted independent standing, mandate_resistant_employees would need to be re-evaluated as bearing a harm not fully offset by the coordination benefit, which would push this reading''s own extraction and victim-set accounting toward the bodily_autonomy_primary reading''s structure — but that would be a different constraint, not a revision of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_framing_versus_autonomy_claim, conceptual, 'Committer-axis disagreement located at the externality-vs-autonomy framing boundary between sibling readings.').

omega_variable(
    proportionality_threshold_location,
    'At what point does declining case severity or rising voluntary uptake make continued mandate enforcement disproportionate to residual risk, under this reading''s own commons-protection logic?',
    'Epidemiological threshold analysis correlating hospital capacity utilization and immunocompromised-population exposure risk against mandate relaxation, cross-checked against the proportionality_reading sibling''s own criteria.',
    'If the threshold is regularly crossed without corresponding mandate relaxation, this reading''s claim to be strictly protective (rather than partially inertial) weakens, and the oscillating measurement pattern would show sustained suppression past the point this reading''s own logic would justify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_location, empirical, 'Whether enforcement intensity tracks the residual risk this reading claims to be protecting against.').

omega_variable(
    immunocompromised_representation_gap,
    'Do immunocompromised patients themselves have an organized voice in setting mandate policy, or are they represented only through public health authorities'' framing of their interests?',
    'Survey of policy-formation processes for direct immunocompromised-patient advocacy input versus proxy representation by public health agencies and hospital systems.',
    'If representation is entirely proxied, the ''beneficiary'' framing may overstate how much this reading actually centers immunocompromised interests versus institutional (hospital system, agency) interests that align with but are not identical to patient interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_representation_gap, empirical, 'Whether the named beneficiary group has direct voice or only proxy representation in the reading''s own institutional apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.14).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.2).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__public_health_primary, theater_ratio, 18, 0.25).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.24).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__public_health_primary, theater_ratio, 30, 0.2).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__public_health_primary, theater_ratio, 36, 0.22).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__public_health_primary, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__public_health_primary, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__public_health_primary, base_extractiveness, 36, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__public_health_primary, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__public_health_primary, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__public_health_primary, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the natural-language concept 'public health mandate authority': this story (public_health_primary), public_health_mandate_authority__bodily_autonomy_primary, and public_health_mandate_authority__proportionality_reading. Each reading assigns a different ε, a different victim set, and potentially a different classification to structurally the same standing mandate practice, per the ε-invariance principle — they are linked as siblings, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
