% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public-Health-Primary Reading of Vaccine Mandate Authority
 *   domain: public_health/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the vaccine
 *   mandate balance kernel: once voluntary compliance falls short of the
 *   epidemiological threshold required for herd immunity, this reading holds
 *   that collective protection of vulnerable populations categorically
 *   supersedes individual consent claims, including sincere religious
 *   objection. The reading names immunocompromised people and unvaccinable
 *   infants as the victim class WHEN mandates are absent, and treats
 *   unvaccinated-coerced individuals as payers bearing a justified cost
 *   rather than as victims of an illegitimate imposition — this is the
 *   reading's own premise, not a neutral description. Sibling readings
 *   (bodily_autonomy_primary, proportionality_reading) draw the victim line
 *   differently and are NOT part of this file; they are separate constraints
 *   linked by kernel_context and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter (institutional/analytical) — designs and enforces mandate mechanisms
 *   - immunocompromised_populations: primary beneficiary (powerless/trapped) — depends entirely on population immunity
 *   - unvaccinated_objectors: payer (moderate/constrained) — bears exclusion and enforcement costs, not classed as victim under this reading
 *   - religious_exemption_seekers: payer (moderate/constrained) — exemption claims narrowed once threshold crossed
 *   - courts_and_legislatures: observer (institutional/analytical) — adjudicate the reading's survival under judicial review
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.62).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.71).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public-Health-Primary Reading of Vaccine Mandate Authority").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '6787e85a-7639-46d4-a80a-878a5ca77855').
narrative_ontology:cs_kernel_codification('6787e85a-7639-46d4-a80a-878a5ca77855', distributed).
narrative_ontology:cs_authority_grounding('6787e85a-7639-46d4-a80a-878a5ca77855', distributed).
narrative_ontology:cs_reading_relation('6787e85a-7639-46d4-a80a-878a5ca77855', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('6787e85a-7639-46d4-a80a-878a5ca77855', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('6787e85a-7639-46d4-a80a-878a5ca77855', foundational, collective_protection_supersedes_individual_consent_at_threshold).
narrative_ontology:cs_axiom_status(collective_protection_supersedes_individual_consent_at_threshold, holdable).
narrative_ontology:cs_axiom_grounding('6787e85a-7639-46d4-a80a-878a5ca77855', collective_protection_supersedes_individual_consent_at_threshold, instrumental).
narrative_ontology:cs_axiom('6787e85a-7639-46d4-a80a-878a5ca77855', foundational, vulnerable_third_party_exposure_risk_outweighs_bodily_autonomy_claim).
narrative_ontology:cs_axiom_status(vulnerable_third_party_exposure_risk_outweighs_bodily_autonomy_claim, holdable).
narrative_ontology:cs_axiom_grounding('6787e85a-7639-46d4-a80a-878a5ca77855', vulnerable_third_party_exposure_risk_outweighs_bodily_autonomy_claim, deontological).
narrative_ontology:cs_reference_frame('6787e85a-7639-46d4-a80a-878a5ca77855', police_power_communicable_disease_precedent).
narrative_ontology:cs_drift_state('6787e85a-7639-46d4-a80a-878a5ca77855', contemporary_post_pandemic_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6787e85a-7639-46d4-a80a-878a5ca77855', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, infants_too_young_to_vaccinate).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_agencies).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, religious_exemption_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vaccine_manufacturers).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, herd_immunity_threshold_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, state_police_power_over_communicable_disease).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccination thresholds, determines when voluntary uptake has failed to reach herd immunity, and designs mandate mechanisms (school exclusion, employment conditions, travel restriction) plus enforcement machinery (fines, exclusion orders, in some jurisdictions civil commitment for isolation). Justifies compulsion as the only remaining lever once persuasion campaigns plateau below threshold.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Cannot be vaccinated themselves or mount adequate immune response, and depend entirely on population-level immunity to avoid lethal exposure. Have no exit from community contact (school, hospital, workplace) and no individual action available; their survival odds are a direct function of the vaccination rate around them, which only mandate enforcement reliably secures once voluntary compliance stalls.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Below the age threshold for vaccination against diseases like pertussis and measles, protected only by the vaccination status of surrounding caregivers, siblings, and community members. Have no voice or agency of their own in this arrangement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, infants_too_young_to_vaccinate, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_balance__public_health_primary, infants_too_young_to_vaccinate).

% Decline vaccination on grounds ranging from medical caution to distrust of institutions to bodily autonomy conviction, and are subjected to school exclusion, employment conditions, fines, or travel restriction until compliant or exempted. In this reading, their objection is not treated as a cognizable harm once population risk crosses the lethal-exposure threshold — their exit options (moving to a jurisdiction without the mandate, homeschooling, remote work) are real but costly, and are the mechanism by which the state converts voluntary preference into required conduct.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_objectors, payer,
    moderate, biographical, constrained, regional).

% Seek exemption from mandates on sincerely held religious grounds; under this reading exemption pathways are narrowed or eliminated once voluntary compliance falls below herd-immunity threshold, because the reading holds that no individual claim — religious or otherwise — outweighs the lethal exposure risk to vulnerable third parties. They bear exclusion from school, work, or public accommodation as the cost of maintaining their claim.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, religious_exemption_seekers, payer,
    moderate, biographical, constrained, regional).

% Benefit from mandate-driven demand floors and often from liability shields accompanying compulsory programs, though they do not administer or enforce the mandate themselves. Their gain is incidental to, not the purpose of, the arrangement, and this reading does not treat them as its object.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Adjudicate challenges to mandate authority, weighing state police power against individual liberty claims, and can narrow, uphold, or strike enforcement mechanisms. Their rulings determine whether this reading's premise — that collective protection categorically supersedes consent once thresholds are crossed — survives judicial review.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunity so that diseases cannot find enough susceptible hosts to sustain transmission chains, protecting those who cannot be vaccinated or cannot mount immune response themselves.
% TRANSFER_FUNCTION: Moves bodily decision-making authority from the individual to the state once voluntary uptake fails to reach the epidemiological threshold; moves the residual transmission risk that would otherwise fall on immunocompromised and unvaccinable populations onto compliance-refusing individuals, who bear exclusion, fines, or forced vaccination instead.
% ABSENT_VOICES: Unvaccinated objectors and religious exemption seekers are present as payers but their consent claims are, under this reading's own premise, not treated as cognizable objections once the threshold is crossed — they are heard in court and in public comment but this reading holds their claims are structurally outweighed, not merely outvoted.
% DISAPPEARANCE_RATIONALE: If mandate authority disappeared overnight, vaccination rates in hesitant subpopulations would likely fall further below herd-immunity thresholds, exposing immunocompromised people and unvaccinable infants to materially higher risk of lethal outbreak exposure; public health agencies would lose their primary lever beyond persuasion, and disease reintroduction events (as documented in historical mandate-relaxation episodes) would become more frequent.
% FOUNDING_PROBLEM: Persuasion-only public health campaigns plateaued below the vaccination coverage needed to prevent sustained transmission of highly contagious diseases (measles, pertussis) in dense populations, leaving immunocompromised people and infants exposed to lethal risk they could not personally mitigate.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists outside government public health agencies (academic disease modelers) corroborate that voluntary-only regimes have historically fallen below herd-immunity thresholds in several documented outbreaks; independent immunologists corroborate that immunocompromised populations lack an individual substitute for population immunity. Unvaccinated-objector advocacy groups dispute the necessity framing but do not dispute that voluntary uptake has, in specific historical episodes, fallen short of threshold.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62 by interval end because this reading's own operation extracts consent-authority from objecting individuals through active enforcement (exclusion, fines, forced compliance in extreme cases) — that IS the mechanism the reading endorses, so its extractiveness measure is high even though the reading regards the extraction as justified. Suppression (0.71) is high and rising because maintaining mandate compliance against a resistant minority requires escalating enforcement infrastructure (school exclusion registries, workplace attestation systems, exemption-narrowing litigation) as voluntary compliance erodes. Theater ratio stays low-moderate (0.22) because the enforcement machinery is doing real epidemiological work, not merely performing compliance theater — though a growing share of it, per the temporal trend, shifts toward exemption litigation rather than direct disease prevention.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies are structural agenda-setters, not extraction beneficiaries in the rent-seeking sense — they administer but do not personally collect. Immunocompromised populations and unvaccinable infants are the clearest beneficiaries: trapped, powerless, and wholly dependent on population-level immunity secured by the mandate. Unvaccinated objectors and religious exemption seekers sit at the target end of directionality under this reading specifically because the reading's founding premise is that their consent claim is subordinated once the threshold is crossed — this is a reading-internal fact, not an external judgment this file imports.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (voluntary compliance failing to reach herd-immunity threshold) is authored as live, not dead — outbreak data in under-vaccinated communities continues to corroborate it from outside public health agencies themselves (independent epidemiological modeling). This blocks a mandatrophy mislabel in the direction of 'the mandate has outlived its function' as a default reading; but because this is one contested reading, the corpus also carries the proportionality reading, which holds the mandate is legitimate only within narrower bounds — the mandatrophy question in THIS reading is whether enforcement has expanded beyond what threshold-maintenance requires, tracked here by the rising theater_ratio and suppression_requirement series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_determination_authority,
    'Who determines when voluntary compliance has ''failed'' to reach herd immunity, and is that determination itself contestable on scientific or political grounds?',
    'Independent epidemiological audit of the threshold-setting methodology used by public health agencies, compared across jurisdictions and against retrospective outbreak data.',
    'If threshold determinations are politically inflated or scientifically contested, the reading''s core premise (that consent is subordinated only once a genuine threshold failure occurs) is undermined and the extraction measured here would be less justified than the reading claims; if thresholds are robustly established, the reading''s premise is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_determination_authority, empirical, 'Whether the herd-immunity threshold trigger is a stable scientific fact or a contestable political judgment.').

omega_variable(
    consent_subordination_boundary,
    'Does this reading''s premise that consent is subordinated to necessity generalize to ALL vaccine-preventable diseases, or only to a subset meeting some severity/transmissibility floor?',
    'Route this question to the proportionality_reading constraint file, which treats severity/transmission/safety thresholds as a gating condition rather than as satisfied wholesale once any threshold is crossed.',
    'If the public-health-primary reading''s premise is read as generalizing across all diseases without severity gating, it forecloses much of the proportionality_reading''s distinguishing claim; if read narrowly, the two readings can coexist by applying to different disease classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_subordination_boundary, conceptual, 'How broadly this reading''s necessity-supersedes-consent premise is meant to apply relative to the proportionality reading''s disease-class gating.').

omega_variable(
    religious_exemption_narrowing_omega,
    'Is narrowing or eliminating religious exemption pathways, specifically, required by the necessity premise, or is it an enforcement-design choice made by agenda-setters that goes beyond what threshold-maintenance strictly requires?',
    'Compare vaccination coverage outcomes in jurisdictions with narrow vs. broad religious exemption pathways, controlling for baseline hesitancy rates, to see whether exemption breadth is actually the marginal factor in threshold failure.',
    'If exemption breadth is not the marginal factor, the enforcement extraction measured against religious_exemption_seekers exceeds what this reading''s own premise justifies, which would push the computed type toward snare at that seat even under this reading''s own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_exemption_narrowing_omega, empirical, 'Whether religious-exemption narrowing is necessity-justified or discretionary enforcement excess.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__public_health_primary, theater_ratio, 4, 0.12).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__public_health_primary, theater_ratio, 8, 0.15).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__public_health_primary, theater_ratio, 12, 0.17).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__public_health_primary, theater_ratio, 16, 0.19).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__public_health_primary, theater_ratio, 20, 0.21).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__public_health_primary, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__public_health_primary, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__public_health_primary, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__public_health_primary, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__public_health_primary, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__public_health_primary, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__public_health_primary, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__public_health_primary, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__public_health_primary, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__public_health_primary, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__public_health_primary, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__public_health_primary, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__public_health_primary, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This file is one of three sibling constraints decomposing the natural-language 'vaccine mandate' debate per the ε-invariance principle: public_health_primary (this file, ε=0.62, victims=unvaccinated objectors/religious exemption seekers), bodily_autonomy_primary (expected higher ε, victims=all mandate-subject individuals, no threshold-based subordination of consent), and proportionality_reading (expected intermediate ε, victims limited to cases failing strict proportionality tests, robust exemptions preserved). Each carries its own stable ε and classification; they are linked here rather than merged because measuring the same mandate arrangement by different consent-weighting premises produces materially different ε values — a decomposition case, not a single constraint with a hidden parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
