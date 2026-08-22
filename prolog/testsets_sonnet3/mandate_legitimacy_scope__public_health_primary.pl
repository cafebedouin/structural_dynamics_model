% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Public-Health-Primary Reading: State Vaccination Mandate as Duty to the Vulnerable
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the
 *   mandate_legitimacy_scope kernel: state authority to compel vaccination is
 *   legitimate whenever necessary to protect vulnerable populations (the
 *   immunocompromised, infants too young to vaccinate, elderly in congregate
 *   care) from serious harm. Under this reading, the immunocompromised are
 *   recast structurally as a victim class WHEN mandates are absent or weak —
 *   their exposure risk is what justifies compulsion — while vaccine-refusing
 *   individuals bear an enforceable duty to protect third parties, which is
 *   precisely why they appear here as payers rather than as autonomous
 *   risk-bearers. This is a distinct constraint from the
 *   bodily_autonomy_primary reading (which places the individual's bodily
 *   integrity as the controlling premise and would make the refuser a victim,
 *   not a duty-bearer) and from the proportionality_reading (which conditions
 *   legitimacy on a case-by-case severity/efficacy/alternatives calculus
 *   rather than treating vulnerable-population protection as sufficient on
 *   its own). Each reading is its own constraint with its own ε and its own
 *   beneficiary/victim structure; they are linked, not merged.
 *
 * KEY AGENTS:
 *   - immunocompromised_and_medically_fragile: primary beneficiary and rationale-bearer (powerless/trapped) — the population whose vulnerability grounds the mandate's legitimacy claim
 *   - public_health_authorities: agenda-setter (institutional/analytical) — sets and enforces the mandate under the vulnerable-protection rationale
 *   - vaccine_refusing_individuals: primary target (powerless/constrained) — bears the enforceable duty this reading imposes
 *   - civil_liberties_advocates: excluded voice (organized/mobile) — objections largely answered-by-assumption rather than engaged within this reading's own logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.68).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.55).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Public-Health-Primary Reading: State Vaccination Mandate as Duty to the Vulnerable").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'b52158b2-d97f-4a56-bb72-6096a5fbf629').
narrative_ontology:cs_kernel_codification('b52158b2-d97f-4a56-bb72-6096a5fbf629', distributed).
narrative_ontology:cs_authority_grounding('b52158b2-d97f-4a56-bb72-6096a5fbf629', expertise).
narrative_ontology:cs_interpretation_layer_present('b52158b2-d97f-4a56-bb72-6096a5fbf629').
narrative_ontology:cs_reading_relation('b52158b2-d97f-4a56-bb72-6096a5fbf629', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b52158b2-d97f-4a56-bb72-6096a5fbf629', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('b52158b2-d97f-4a56-bb72-6096a5fbf629', foundational, third_party_serious_harm_overrides_individual_medical_refusal).
narrative_ontology:cs_axiom_status(third_party_serious_harm_overrides_individual_medical_refusal, holdable).
narrative_ontology:cs_axiom_grounding('b52158b2-d97f-4a56-bb72-6096a5fbf629', third_party_serious_harm_overrides_individual_medical_refusal, deontological).
narrative_ontology:cs_axiom('b52158b2-d97f-4a56-bb72-6096a5fbf629', secondary, vulnerable_population_exposure_alone_licenses_compulsion).
narrative_ontology:cs_axiom_status(vulnerable_population_exposure_alone_licenses_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('b52158b2-d97f-4a56-bb72-6096a5fbf629', vulnerable_population_exposure_alone_licenses_compulsion, instrumental).
narrative_ontology:cs_reference_frame('b52158b2-d97f-4a56-bb72-6096a5fbf629', police_power_communicable_disease_doctrine).
narrative_ontology:cs_drift_state('b52158b2-d97f-4a56-bb72-6096a5fbf629', contemporary_post_pandemic_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b52158b2-d97f-4a56-bb72-6096a5fbf629', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_and_medically_fragile).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, elderly_care_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, infants_too_young_to_vaccinate).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_authorities).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_refusing_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, religious_exemption_seekers).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, workers_in_mandate_conditioned_employment).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, herd_immunity_threshold_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, state_police_power_over_contagious_disease).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot be vaccinated themselves or mount adequate immune response, and depend entirely on high vaccination rates in the surrounding population (herd immunity) to avoid exposure. In this reading, their vulnerability is the primary justification for compulsion; without a mandate they enter the story as a victim class of unvaccinated-adjacent contagion risk. They have no exit from the general population and no independent means of protecting themselves.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_and_medically_fragile, beneficiary,
    powerless, biographical, trapped, national).

% Set vaccination requirements for school enrollment, employment in health-adjacent sectors, or general public activity, and enforce them through exclusion, fines, or conditioned access. Frame their authority as protecting those who cannot protect themselves, drawing on established police-power doctrine over communicable disease. Do not personally bear the cost of compliance or refusal.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Object to vaccination on medical, religious, or philosophical grounds and are subjected to exclusion from schools, workplaces, or public accommodations, or to direct fines, under this reading's framing of their choice as risking serious harm to vulnerable others. Their exit options are narrow: relocate to a jurisdiction without the mandate, accept exclusion from major institutions, or comply against their preference. In this reading, they are recast from autonomous decision-makers into bearers of an enforceable duty to protect third parties.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_refusing_individuals, payer,
    powerless, biographical, constrained, national).

% Hold sincere religious objections to vaccination and seek exemption; under this reading, exemptions are narrowed or denied precisely because the vulnerable-protection rationale treats individual belief as subordinate to collective serious-harm prevention. They experience the mandate as a forced choice between conscience and institutional participation.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, religious_exemption_seekers, payer,
    powerless, biographical, constrained, national).

% Employed in healthcare, education, or other settings where vaccination is made a condition of continued employment. Face termination or unpaid leave if they decline, regardless of their personal risk assessment, because their proximity to vulnerable populations (patients, students) is what triggers the requirement under this reading.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, workers_in_mandate_conditioned_employment, payer,
    moderate, biographical, constrained, national).

% Residents of long-term care facilities and other congregate settings where an unvaccinated staff member or visitor can introduce lethal outbreaks. Under this reading their serious-harm exposure is the central justification for extending mandates into staff and visitor populations; they have essentially no independent capacity to reduce their own exposure.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, elderly_care_populations, beneficiary,
    powerless, biographical, trapped, national).

% Argue that compelled medical intervention without robust individualized proportionality review sets a dangerous precedent regardless of the vulnerable-protection rationale. Their bodily-autonomy-centered objections are treated in this reading as outweighed by third-party harm prevention, and their proportionality concerns (about narrower alternatives) are largely absent from this reading's justificatory frame.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunity so that people who cannot be vaccinated or cannot mount protective immune responses are shielded from contagious disease by the vaccination status of those around them — a genuine externality problem, since an individual's vaccination choice affects others' exposure risk.
% TRANSFER_FUNCTION: Moves the burden of achieving herd immunity from the vulnerable population (who would otherwise bear elevated disease risk) onto vaccine-refusing individuals, who are made to bear exclusion, employment loss, or compliance costs as the price of participating in shared institutions.
% ABSENT_VOICES: Civil liberties advocates and bodily-autonomy theorists are structurally present in the broader debate but largely absent from THIS reading's own justificatory logic — the public-health-primary frame treats their objections as already answered by the seriousness of third-party harm, rather than engaging them on their own terms.
% DISAPPEARANCE_RATIONALE: If this reading's authority to compel vaccination disappeared, vaccination rates in some populations would fall below herd-immunity thresholds, immunocompromised and congregate-care populations would face materially higher exposure to preventable serious illness, and the institutional levers (school enrollment conditions, employment conditions) that currently enforce coverage would no longer be available to public health authorities.
% FOUNDING_PROBLEM: Contagious diseases with serious morbidity/mortality for a subset of the population (infants, immunocompromised, elderly) can only be controlled at the population level; individual risk-benefit calculations by vaccine-eligible people do not internalize the risk imposed on those who cannot be vaccinated or cannot respond to vaccination.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and long-term-care ombudspersons outside the mandate-enforcing agencies corroborate that outbreak risk to congregate vulnerable populations tracks measurably with local vaccination coverage. Civil liberties organizations and some constitutional scholars, also outside the benefiting public-health institutions, dispute that the founding problem justifies compulsion at the scope currently exercised, arguing the same protective goal is achievable through less restrictive means — that dispute is the proportionality_reading's subject, not resolved here.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68) because, under this reading's own lights, the standing arrangement under contest is the CURRENT scope and enforcement intensity of mandates as applied to vaccine-refusing individuals — and the reading holds that inadequate mandate enforcement (or narrow exemptions) directly translates into serious, measurable harm to a defenseless population. The referent is the mandate arrangement as currently practiced, not the reading's own endorsed ideal (per the ε-referent rule for kernel readings) — so ε reflects the genuine costs this reading imposes on refusers/exemption-seekers through exclusion and conditioned employment, which the reading treats as justified but which are nonetheless real transfers. Suppression is moderate (0.55) and rising over the interval, reflecting increasing institutional reliance on exclusion mechanisms (school, employment) rather than persuasion as coverage plateaus. Theater ratio stays low (0.15) because the enforcement mechanism (exclusion/employment conditions) is functionally tied to the stated goal, not performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised, elderly congregate-care residents, and public health authorities are declared beneficiaries: the first two collect protection without administering the mandate, the third administers it and derives institutional legitimacy from doing so. Vaccine-refusing individuals, religious exemption seekers, and mandate-conditioned workers are declared victims/payers: this reading structurally assigns them a duty-to-protect-others that they did not choose, and channels the costs of achieving herd immunity onto them via exclusion or employment loss. Their exit options are genuinely constrained (relocation, job loss, or compliance) rather than trapped, which keeps their derived directionality high but not at the absolute ceiling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (serious harm to those who cannot protect themselves via vaccination) remains at least partially live per epidemiological corroboration outside the enforcing agencies, which prevents this reading from being classified as pure zombie mandatrophy. But the founding_problem_status is marked contested rather than live because independent constitutional scholarship disputes whether the SCOPE of current mandates (rather than the underlying protective goal) still matches the magnitude of the problem — this is exactly the gap the sibling proportionality_reading exists to interrogate, and the tangled_rope classification here reflects that a genuine coordination function (herd immunity) persists alongside asymmetric extraction (duty imposed on refusers) requiring active enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_of_vulnerability_rationale,
    'Is protecting vulnerable populations from serious harm SUFFICIENT on its own to legitimate compulsion, or must it be weighed against disease severity, vaccine safety/efficacy, and less restrictive alternatives (the proportionality_reading''s test)?',
    'Comparative constitutional and bioethics analysis of jurisdictions that adopt a pure vulnerable-protection standard versus a proportionality standard, tracking divergence in mandate scope and judicial outcomes.',
    'If sufficiency is rejected in favor of proportionality, this reading''s high ε is partly an artifact of skipping a severity/alternatives filter that would narrow the mandate''s legitimate scope and reduce the burden placed on refusers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_vulnerability_rationale, conceptual, 'Whether vulnerable-population protection alone (this reading) or a multi-factor proportionality test (sibling reading) should govern mandate legitimacy.').

omega_variable(
    duty_to_protect_vs_bodily_integrity,
    'Does the state''s authority to protect third parties from serious harm override an individual''s bodily integrity interest in refusing an unwanted medical intervention, or does bodily integrity function as a near-absolute side-constraint regardless of third-party harm (the bodily_autonomy_primary reading)?',
    'No empirical resolution is available — this is a foundational disagreement about the relative weight of two normative goods (bodily integrity vs. third-party protection) that different legal traditions and philosophical frameworks resolve differently.',
    'Adopting bodily_autonomy_primary would remove vaccine-refusing individuals from the victim/payer set entirely and instead treat compulsion itself as the extractive act, inverting this story''s beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duty_to_protect_vs_bodily_integrity, preference, 'Foundational normative disagreement located at the axiom level between this reading and the bodily_autonomy_primary sibling.').

omega_variable(
    herd_immunity_measurement_uncertainty,
    'How precisely can the marginal contribution of a single refuser''s vaccination status to aggregate herd-immunity risk be measured, versus other factors (waning immunity, variant emergence, non-vaccine mitigation)?',
    'Epidemiological modeling comparing outbreak incidence in populations with varying coverage levels, holding other factors constant where possible.',
    'If the marginal individual contribution is small relative to other drivers, the duty-to-protect framing imposed on individual refusers is less well-grounded, which would lower the justified extraction this reading assigns to mandate enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_measurement_uncertainty, empirical, 'Empirical uncertainty in attributing population-level outbreak risk to individual vaccination refusal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__public_health_primary, theater_ratio, 4, 0.1).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__public_health_primary, theater_ratio, 8, 0.12).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__public_health_primary, theater_ratio, 12, 0.13).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__public_health_primary, theater_ratio, 16, 0.14).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.15).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__public_health_primary, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__public_health_primary, 0.1).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'legitimate scope of state vaccination mandate authority' (the mandate_legitimacy_scope kernel), per the ε-invariance principle. public_health_primary (this story) authors high ε for the standing mandate arrangement because, by its own lights, mandate absence or narrowing directly harms an identifiable vulnerable population. bodily_autonomy_primary authors the mandate itself as the extractive act (inverting beneficiary/victim sets). proportionality_reading conditions ε on a multi-factor test rather than treating either bodily integrity or vulnerable-protection as categorically controlling. All three share the kernel but are structurally distinct constraints with different ε, different stakeholders, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
