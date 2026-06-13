% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: State Vaccine Mandate Authority (Public Health Primacy Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint captures ONE READING of the vaccine-mandate legitimacy
 *   kernel: the public-health-primacy reading, which frames unvaccinated
 *   status as an externality and the state duty to prevent collective harm as
 *   justifying mandate authority. This reading coexists with two contending
 *   readings within the same kernel: the bodily-autonomy-primacy reading
 *   (medical self-determination is absolute; state coercion is categorically
 *   impermissible) and the risk-stratification reading (mandate legitimacy is
 *   contingent on actuarial risk thresholds; blanket mandates fail
 *   proportionality). The ε of THIS reading is structurally distinct from the
 *   siblings: it asserts high beneficiary capture (public health bureaucracy
 *   gains authority and expanded scope) and names the refuser population as
 *   victims bearing suppression costs. The sibling readings distribute
 *   victims differently (bodily-autonomy reading has no victims, only
 *   authority claimants; risk-stratification reading splits victims by
 *   actuarial class). These are not measurement-perspective variants of one
 *   constraint; they are three structurally distinct constraints whose kernel
 *   is the contested legitimacy claim itself.
 *
 * KEY AGENTS:
 *   - public_health_bureaucracy: institutional agenda-setter; gains expanded authority and legitimacy for coercive intervention
 *   - vaccine_mandate_refusers: moderate-power payer; identity-locked (medical autonomy fused to identity); bear employment/education/licensure sanctions
 *   - medical_exemption_deniers: powerless trapped payer; medical contraindication makes vaccination medically impossible; face mandate's full force without exit
 *   - vaccinated_populations: organized beneficiary; receive herd-immunity protection and reduced disease risk
 *   - vaccine_efficacy_dissenters: excluded moderate-power seat; their empirical and ethical objections are structurally barred from mandate legitimacy evaluation
 *   - constitutional_courts: analytical observer; adjudicate whether public-health-primacy reading is constitutionally valid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.71).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "State Vaccine Mandate Authority (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'b9832e35-9fcd-4515-8732-1cbc85f6b0c3').
narrative_ontology:cs_kernel_codification('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', formalized).
narrative_ontology:cs_authority_grounding('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', lineage).
narrative_ontology:cs_interpretation_layer_present('b9832e35-9fcd-4515-8732-1cbc85f6b0c3').
narrative_ontology:cs_reading_relation('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', foundational, collective_harm_justifies_individual_override).
narrative_ontology:cs_axiom_status(collective_harm_justifies_individual_override, holdable).
narrative_ontology:cs_axiom_grounding('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', collective_harm_justifies_individual_override, deontological).
narrative_ontology:cs_axiom('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', foundational, externality_framing_establishes_victimhood).
narrative_ontology:cs_axiom_status(externality_framing_establishes_victimhood, holdable).
narrative_ontology:cs_axiom_grounding('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', externality_framing_establishes_victimhood, empirically_contingent).
narrative_ontology:cs_reference_frame('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', state_duty_to_prevent_collective_disease_harm).
narrative_ontology:cs_drift_state('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', contemporary_vaccine_durability_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b9832e35-9fcd-4515-8732-1cbc85f6b0c3', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_exemption_deniers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) and rising through the interval (0.48→0.68) because: (1) the mandate transfers control over individual medical decisions to an institutional actor without individual consent; (2) the refuser population bears escalating sanctions (employment, education access, travel) for non-compliance with a choice the reading treats as settled ('externality'); (3) the beneficiary (public health bureaucracy) captures expanded authority and legitimacy to intervene in future health domains. The measurement series shows extractiveness stabilizing by t=30, suggesting the mandate reaches a enforcement plateau. Suppression is high (0.71) because active enforcement machinery is required to sustain compliance: employment termination, licensure revocation, school exclusion, and travel bans are all coercive acts. Theater is moderate (0.42) and rising (0.18→0.42): the reading's rhetoric emphasizes collective-harm prevention and scientific consensus, but as the interval progresses, enforcement activity increasingly defends the mandate itself rather than disease prevention (t=24 onward shows enforcement continuing despite plateaued vaccination rates, suggesting performative maintenance). Accessibility collapse is moderate (0.62): alternatives exist (risk stratification, voluntary incentives, treatment protocols) but the reading forecloses them as incompatible with the public-health duty framing.
 *
 * PERSPECTIVAL GAP:
 *   The institutional seat (public health bureaucracy) experiences this as legitimate authority to prevent collective harm; the refuser seat experiences it as coercive override of medical autonomy; the medical-exemption-denial seat experiences it as categorically impossible (medical reality trumps legal mandate). The disparity arises from fundamentally different premises about what harm counts and whose interests are the reference point. The bureaucracy operates from aggregate-population framing (externality prevention justifies individual override); refusers operate from individual-agency framing (consent is inviolable; harms must be measured at the individual level). These premises are not reconcilable by additional data — they are foundational commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vaccinated_populations, public_health_bureaucracy) have low directionality: they collect protection/authority without bearing extraction costs. Victims (vaccine_mandate_refusers, medical_exemption_deniers) have high directionality: they lose medical autonomy, face coercive sanctions, and have constrained exit options. The refuser seat is identity-locked (medical autonomy fused to self-concept) rather than merely constrained, raising the suppression cost. The medical-exemption-denial seat is trapped (exit impossible: cannot vaccinate due to contraindication, cannot refuse without sanctions), yielding directionality near 0.95. Excluded seats (vaccine_efficacy_dissenters, bodily_autonomy_advocates) are not stakeholders in the mandate structure but are administratively suppressed by the reading's framing (their objection is treated as obstruction, not legitimate uncertainty).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading is NOT mandatrophic at the present interval. It is a live tangled rope: genuine coordination function (herd immunity) paired with asymmetric extraction (refuser population loses medical autonomy). The founding problem (disease externality) remains live, though contested. Theater is moderate and rising but has not reached the 0.6+ threshold for piton diagnosis. The crucial question is whether theater will continue rising after vaccination plateaus (t=30 onward): if enforcement activity persists despite stable or declining vaccination gaps, and if the reading's public rationale (disease prevention) decouples from actual enforcement targets (political dissent, alternative framings), then mandatrophy becomes evident. Current data show enforcement rising through t=18 (climbing suppression_requirement) but plateauing thereafter, consistent with a legitimate coordination reaching steady state. The interval would need to extend to t=60+ to observe whether theater rises further as enforcement becomes purely defensive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vaccine_externality_empirical_grounding,
    'Is unvaccinated status a genuine negative externality (scientifically measurable disease transmission from unvaccinated to vaccinated) or an institutional framing of transmission risk?',
    'Randomized controlled trials or natural experiments measuring actual transmission rates from unvaccinated to vaccinated in high-vaccination-prevalence settings; comparison of disease incidence in vaccinated populations with and without mandate-driven vaccination gains.',
    'If externality is empirically robust, the public-health-primacy reading''s core premise is sound and mandates are justified on collective-action grounds. If externality is small, atypical, or dependent on vaccine durability assumptions now questioned, the reading''s empirical ground erodes and risk-stratification becomes more defensible. If transmission from vaccinated to vaccinated is substantial (breakthrough infection), the externality framing fails and bodily-autonomy reading gains force (individual vaccination does not prevent individual''s transmission of harm, so collective mandate is irrational).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccine_externality_empirical_grounding, empirical, 'Whether unvaccinated status is a genuine measurable externality or an institutional construct.').

omega_variable(
    suppression_internalization_trajectory,
    'Is the refuser population''s suppression internalized (they have accepted the mandate''s framing and carry it after enforcement mechanisms are removed) or structurally dependent (removal of employment/education/travel barriers causes suppression to collapse)?',
    'Post-mandate-removal measurement: if legal mandates are lifted, observe vaccination refusal rates, attitude surveys, and social resistance over 12+ months. Internalized suppression persists; structural suppression collapses.',
    'If internalized, the mandate has achieved a durable shift in the population''s own conception of medical autonomy and collective duty — the constraint achieves self-perpetuation even without active enforcement. If structural, the constraint''s persistence depends entirely on ongoing coercive machinery; removal of enforcement causes immediate reversion. This informs whether the constraint should be classified as partially internalized snare (internalization present) or pure snare (structural suppression only).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether suppression is internalized by refusers or dependent on external enforcement.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Is the public-health-primacy reading logically foreclosed by the bodily-autonomy-primacy reading, or do both readings coexist as live positions held by different parties?',
    'Examine whether a single institutional actor (court, regulatory body, philosophical framework) can coherently hold both readings simultaneously, or whether the readings are held by mutually-excluding factions. If both readings can coexist in a unified framework, they coexist_with; if adopting one logically requires rejecting the other''s core premise, they foreclose.',
    'If forecloses, the two readings are in genuine contradiction and courts/legislatures face a deterministic choice; if coexists_with, both readings remain live and the constraint is the site of an ongoing political contest, not a settled empirical question. This affects whether the mandate''s legitimacy is adjudicated as a once-and-for-all constitutional question or as an open political negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the public-health and bodily-autonomy readings are logically incompatible or coexistent.').

omega_variable(
    medical_exemption_administrative_reality,
    'To what extent does the mandate''s administrative implementation actually recognize medical contraindications as legitimate, versus treating them as rare exceptions to be minimized or as objections to be overcome?',
    'Audit of exemption-approval rates: are documented medical contraindications routinely accepted, or are approvals rare and challenged? Survey of physicians: how often are they pressured to withhold medical exemptions to maintain vaccination rates? Case documentation: are individuals with genuine contraindications actually exempted or coerced into unsafe vaccination?',
    'If exemptions are genuinely honored, the mandate has a moral and medical escape valve that protects the powerless-trapped population from categorical harm. If exemptions are administratively rare or pressured, the mandate functionally becomes absolute, and the medical-exemption-denial seat becomes a substantial victim population. This affects whether the constraint''s suppression is justified as proportionate harm-prevention or should be reclassified as categorical coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_exemption_administrative_reality, empirical, 'Whether medical exemptions are administratively honored or administratively suppressed.').

omega_variable(
    public_health_bureaucracy_regulatory_capture,
    'To what extent is the public health bureaucracy''s framing of the mandate captured by political pressures (electoral cycling, pharma lobbying, liability protection) versus grounded in disinterested epidemiological assessment?',
    'Track institutional positions pre-, during, and post-pandemic across multiple independent public health jurisdictions; compare advice given by unaffiliated epidemiologists versus advice given by mandating authorities; examine funding flows and liability structures shaping bureaucratic positions.',
    'If the bureaucracy is substantially captured, the mandate''s beneficiary (public health bureaucracy) is itself a victim of institutional incentive misalignment, and the constraint''s extractiveness is higher than it appears (the bureaucracy extracts authority without necessarily achieving health outcomes). If the bureaucracy is relatively independent, its beneficiary position is justified by its functional role in coordinating a genuine collective good. This affects whether the constraint is tangled rope (genuine coordination with asymmetric capture) or snare (coordination frame masking institutional extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_health_bureaucracy_regulatory_capture, conceptual, 'Whether the public health bureaucracy''s mandate position is independence-grounded or politically captured.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 36, 0.42).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 36, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_license_restriction_authority).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_emergency_governance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vaccine_mandate_legitimacy kernel. The public-health-primacy reading treats unvaccinated status as an externality and grounds the mandate in state duty to prevent collective harm. Sibling readings instantiate distinct constraints: bodily_autonomy_primacy_reading frames medical self-determination as inviolable (no externality framing); risk_stratification_reading frames mandate legitimacy as conditional on actuarial risk thresholds. All three readings coexist as live positions in ongoing political and constitutional dispute. The network edges link all three as a constraint family; the ε value of EACH reading is independent of the siblings (ε-invariance: one reading, one ε).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
