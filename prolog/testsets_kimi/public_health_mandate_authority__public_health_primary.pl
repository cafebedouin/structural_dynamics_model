% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate Authority â Public Health Primary Reading
 *   domain: public_health_law_constitutional_rights_bioethics
 *
 * SUMMARY:
 *   This constraint is the public_health_primary reading of the
 *   public_health_mandate_authority kernel. It instantiates the claim that
 *   collective obligation to protect vulnerable commons (immunocompromised
 *   populations, healthcare infrastructure) justifies coercive public health
 *   mandates, including employment and service exclusion for non-compliant
 *   individuals. In this reading, the unvaccinated are framed as free-riders
 *   imposing an externality rather than rights-bearing victims, while the
 *   mandate-resistant bear the direct coercive cost of employment and access
 *   loss. The immunocompromised are positioned as beneficiaries of collective
 *   action, though they enter the victim set when mandates fail to achieve
 *   coverage. The constraint operates as a tangled rope: it coordinates
 *   genuine epidemiological protection for a vulnerable population while
 *   asymmetrically extracting from a resistant minority via state coercion.
 *
 * KEY AGENTS:
 *   - Public health authorities (agenda_setter): Set mandate scope, penalties, and enforcement machinery.
 *   - Immunocompromised population (beneficiary): Receives reduced exposure risk when mandates succeed; victimized by outbreak when they fail.
 *   - Healthcare system (beneficiary): Avoids surge capacity crises via suppressed transmission.
 *   - Mandate-resistant individuals (payer): Bear employment loss and service exclusion as the direct cost of coercion.
 *   - Unvaccinated adults (excluded): Framed as free-riders; their autonomy claims are delegitimized in this reading.
 *   - Judicial review benches (observer): Evaluate constitutional challenges but typically defer to public health expertise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.72).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.78).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority â Public Health Primary Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law_constitutional_rights_bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '88670764-15b7-4115-a020-c3e07b089e77').
narrative_ontology:cs_kernel_codification('88670764-15b7-4115-a020-c3e07b089e77', formalized).
narrative_ontology:cs_authority_grounding('88670764-15b7-4115-a020-c3e07b089e77', expertise).
narrative_ontology:cs_interpretation_layer_present('88670764-15b7-4115-a020-c3e07b089e77').
narrative_ontology:cs_reading_relation('88670764-15b7-4115-a020-c3e07b089e77', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('88670764-15b7-4115-a020-c3e07b089e77', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('88670764-15b7-4115-a020-c3e07b089e77', foundational, collective_protection_over_bodily_autonomy).
narrative_ontology:cs_axiom_status(collective_protection_over_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('88670764-15b7-4115-a020-c3e07b089e77', collective_protection_over_bodily_autonomy, deontological).
narrative_ontology:cs_reference_frame('88670764-15b7-4115-a020-c3e07b089e77', collective_immunity_commons).
narrative_ontology:cs_drift_state('88670764-15b7-4115-a020-c3e07b089e77', post_emergency_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('88670764-15b7-4115-a020-c3e07b089e77', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_system).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, communal_immunity_obligation).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, state_police_power_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce vaccination mandates under state police power or public health emergency statutes, set penalty structures for non-compliance including employment exclusion, and justify the measures via epidemiological models of community transmission and healthcare capacity.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Rely on population-level vaccination coverage to reduce environmental pathogen exposure because their own immune responses cannot be bolstered by vaccines; they experience reduced community risk when mandates are effective and heightened exposure when compliance is patchy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, constrained, national).

% Operates intensive care and inpatient capacity that faces surge risk during uncontrolled outbreaks; benefits from reduced patient volumes and stabilized staffing when community transmission is suppressed by broad vaccination.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_system, beneficiary,
    institutional, generational, constrained, national).

% Face termination of employment, exclusion from public accommodations, and loss of professional licensure for declining vaccination; they bear the direct coercive cost of the mandate and experience it as state-compelled medical compliance or exile from economic life.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, biographical, constrained, national).

% Are categorically treated as free-riders imposing externality on the vulnerable commons in this reading's framing; their claims to bodily autonomy or risk-based personal choice are excluded from the victim narrative and delegitimized in policy discourse.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, unvaccinated_adults, excluded,
    organized, biographical, constrained, national).

% Review constitutional challenges to mandate programs, evaluating claims under police power doctrine and proportionality, but typically defer to public health expertise during declared emergencies.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, judicial_review_benches, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protecting immunocompromised populations and preventing healthcare system collapse by ensuring sufficient vaccination coverage to reduce transmission of communicable disease where voluntary uptake is insufficient to achieve community protection.
% TRANSFER_FUNCTION: Moves compliance burden and employment or service-access risk from the state to individuals who resist vaccination, transferring epidemiological risk away from the vulnerable onto those who bear the coercion cost.
% ABSENT_VOICES: Mandate-resistant individuals facing employment termination are underrepresented in policy design; unvaccinated populations are framed as free-riders rather than rights-bearers; immunocompromised individuals are invoked symbolically but rarely seated at the decision table.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished overnight, unvaccinated individuals would return to workplaces and public venues, transmission dynamics would shift, immunocompromised populations would face heightened exposure risk, and healthcare systems would anticipate surge capacity demands â the collective immunity framework would reorganize around individual risk management.
% FOUNDING_PROBLEM: Preventing infectious disease outbreaks that overwhelm healthcare capacity and kill vulnerable populations who cannot mount protective immune responses, where voluntary uptake is insufficient to achieve community protection.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists and immunologists outside the enforcing authority attest to the vulnerability of the immunocompromised and historical ICU surge data; civil liberties advocates and some health economists contest that the founding problem justifies the coercion level, arguing alternative containment strategies were under-explored. The corroboration is split by institutional seat.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the mandate extracts employment and civic access from a defined resistant population without monetary compensation. Suppression (0.78) is higher still because the constraint persists only through active enforcement â employer mandates, credential revocation, and venue exclusion â not through spontaneous compliance. Theater ratio (0.40) reflects moderate performative maintenance: masking, distancing signage, and compliance theater persist as political signaling even after epidemiological rationale shifts. Accessibility collapse (0.65) captures the delegitimization of individual risk-management alternatives in policy discourse. Resistance (0.70) measures active litigation, protest, and political mobilization against the mandate. The temporal series trace enforcement hardening from initial emergency declaration through peak contestation to a plateau of institutionalized coercion.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as legitimate, necessary coordination to protect the vulnerable commons. The payer seat experiences the same structure as state-compelled medical conformity with punitive economic sanctions. The excluded seat experiences it as delegitimization of their autonomy claims. These divergences are structurally determined by the agent's relationship to the coercion mechanism and their medical vulnerability, not by disagreement about facts alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The immunocompromised population and healthcare system sit near the beneficiary end of directionality: the constraint is built to reduce their risk and is experienced as protective subsidy. Mandate-resistant individuals sit near the full-target end: they are the seat from which extraction is collected via employment and service loss. Unvaccinated adults are excluded from the victim framing entirely in this reading, experiencing the constraint as narrative erasure rather than direct cost. Public health authorities administer the constraint but do not personally bear its costs or receive its epidemiological benefits; their directionality is mid-range, structurally aligned with enforcement capacity rather than extraction capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â protecting the vulnerable during an outbreak that overwhelms healthcare â is contested but not clearly dead. However, the constraint's persistence has partially decoupled from real-time epidemiology: mandates remain in force or in institutional memory after acute emergency declarations end, suggesting some mandatrophy. The theater ratio of 0.40 signals that a substantial share of current enforcement activity is performative maintenance of authority rather than responsive to live transmission dynamics. A full piton verdict would require higher theater and lower beneficiary capture; the current profile fits tangled rope because the coordination function (healthcare surge avoidance) remains partially live while extraction from the resistant is structurally embedded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'How would the beneficiary and victim sets restructure if the bodily_autonomy_primary or proportionality_reading were adopted instead of this public_health_primary reading?',
    'Comparative legal analysis of jurisdictions adopting each reading: measure shifts in employment-exclusion rates, litigation targets, and which population groups are granted standing as rights-bearers versus free-riders.',
    'If bodily_autonomy_primary were adopted, mandate-resistant individuals would exit the victim set and the constraint would likely reclassify toward rope or dissolve entirely; if proportionality_reading were adopted, the victim set would expand or contract based on real-time threat severity, making the constraint temporally unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Sibling reading adoption would reallocate victimhood and beneficiary status across the kernel.').

omega_variable(
    immunocompromised_beneficiary_or_victim,
    'When mandates fail to achieve coverage, do the resulting harms to the immunocompromised place them in the victim set of the constraint itself, or merely mark the constraint''s failure mode?',
    'Track whether policy discourse treats immunocompromised harms under failed mandates as a reason to intensify the constraint (victimhood externalized to non-compliance) or as evidence that the constraint is ineffective (victimhood internal to the mandate structure).',
    'If classified as victims of the constraint, directionality for this seat shifts toward the target end and the constraint''s extraction profile becomes more symmetric; if classified as external failure, the constraint retains its asymmetric tangled-rope shape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_beneficiary_or_victim, conceptual, 'Ambiguity in whether immunocompromised harms under failed mandates constitute constraint victimhood.').

omega_variable(
    mandate_efficacy_vs_extraction,
    'Does the mandate produce sufficient epidemiological benefit to the immunocompromised to justify the employment and service extraction from the resistant, or is the coordination function separable from the coercive mechanism?',
    'Natural experiment from jurisdictions that shifted from mandate to education and access campaigns: compare immunocompromised infection rates and healthcare surge metrics before and after the policy shift.',
    'If outcomes hold without coercion, the coordination function is separable and the constraint leans toward snare; if outcomes collapse, the coercion is structurally necessary and the tangled-rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_efficacy_vs_extraction, empirical, 'Whether epidemiological protection requires the coercive extraction mechanism or is achievable by less extractive means.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.28).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.35).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__public_health_primary, theater_ratio, 18, 0.45).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.48).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__public_health_primary, theater_ratio, 30, 0.46).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__public_health_primary, theater_ratio, 36, 0.44).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__public_health_primary, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__public_health_primary, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__public_health_primary, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__public_health_primary, suppression_requirement, 18, 0.85).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__public_health_primary, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__public_health_primary, suppression_requirement, 36, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__public_health_primary, 0.1).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the public_health_mandate_authority kernel family. The public_health_primary reading shares the same regulatory domain as its siblings but instantiates a distinct structural claim with a different epsilon, beneficiary/victim structure, and directionality distribution. Epsilon varies across the family because the same legal instrument is read as either coordination-dominated (this reading, under contestation) or extraction-dominated (bodily_autonomy_primary frames it as pure snare), while proportionality_reading produces a temporally variable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
