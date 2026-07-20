% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Harm Reduction Authority: Decriminalization with Public Health Service Mandate
 *   domain: public_health_policy/criminal_justice
 *
 * SUMMARY:
 *   This constraint instantiates the harm_reduction_reading of the
 *   substance_control_authority kernel. It describes state authority that
 *   decriminalizes personal drug use and substitutes public health
 *   interventions for criminal sanctions. Users exit the criminal victim set
 *   but remain in a partial victim set due to persistent health harms from
 *   unregulated supply. Third parties bear residual crime and disease
 *   externalities because decriminalization without legalization sustains
 *   illicit markets. The constraint is claimed as tangled_rope: it solves a
 *   genuine coordination problem (collective management of overdose and
 *   disease outbreaks) while asymmetrically extracting from users (continued
 *   health risks, constrained liberty) and third parties (localized
 *   externalities), and requires active enforcement to maintain the boundary
 *   between tolerated use and prohibited commerce.
 *
 * KEY AGENTS:
 *   - state_public_health_authority (agenda_setter/institutional/analytical): administers the decriminalization framework and captures institutional expansion
 *   - drug_users (beneficiary/powerless/constrained): exit criminalization but remain exposed to unregulated supply harms
 *   - service_providers (beneficiary/organized/mobile): receive state funding and legitimacy under the constraint
 *   - third_party_residents (payer/moderate/constrained): bear residual externalities from persistent illicit markets
 *   - illicit_suppliers (payer/moderate/trapped): bear displaced enforcement pressure under continued supply prohibition
 *   - prohibition_advocates and legalization_advocates (excluded/organized/constrained): structurally absent from the policy conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.48).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.52).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Harm Reduction Authority: Decriminalization with Public Health Service Mandate").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, 'da34ffa4-184b-4c3b-9e45-8f1ad1a11b05').
narrative_ontology:cs_kernel_codification('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', formalized).
narrative_ontology:cs_authority_grounding('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', expertise).
narrative_ontology:cs_interpretation_layer_present('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05').
narrative_ontology:cs_reading_relation('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', foundational, state_may_accept_drug_use_to_minimize_aggregate_harm).
narrative_ontology:cs_axiom_status(state_may_accept_drug_use_to_minimize_aggregate_harm, holdable).
narrative_ontology:cs_axiom_grounding('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', state_may_accept_drug_use_to_minimize_aggregate_harm, empirically_contingent).
narrative_ontology:cs_axiom('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', foundational, criminalization_of_use_counterproductive_to_health_outcomes).
narrative_ontology:cs_axiom_status(criminalization_of_use_counterproductive_to_health_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', criminalization_of_use_counterproductive_to_health_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', public_health_authority_framework).
narrative_ontology:cs_drift_state('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', contemporary_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da34ffa4-184b-4c3b-9e45-8f1ad1a11b05', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, drug_users).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, state_public_health_authority).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, service_providers).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, third_party_residents).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, illicit_suppliers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework that decriminalizes personal drug use while channeling users into public health services. Funds needle exchanges, safe consumption sites, and treatment referrals. Maintains active enforcement against illicit supply chains and regulates the boundary between tolerated personal use and prohibited commerce. Expands institutional scope and budget through the public health mandate.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, state_public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Are no longer subject to criminal penalties for personal possession and use. Gain access to state-funded harm reduction services including sterile supplies, overdose reversal medications, and treatment pathways. Continue to experience health harms from unregulated drug supply and remain dependent on a service infrastructure they did not design. Cannot legally purchase drugs, so must still interact with illicit markets.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, drug_users, beneficiary,
    powerless, immediate, constrained, local).

% Operate needle exchanges, safe consumption sites, and mobile outreach units under state contract and regulatory oversight. Receive funding, legal protection, and professional legitimacy from the constraint. Their work reduces overdose deaths and disease transmission but depends on the continued criminalization of supply and the political sustainability of the harm reduction model.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, service_providers, beneficiary,
    organized, biographical, mobile, regional).

% Live in neighborhoods where illicit drug markets persist because decriminalization stops short of legalization. Bear residual risks of property crime, discarded paraphernalia, and communicable disease exposure. Fund the service infrastructure through taxation. Did not choose the policy but absorb its localized externalities.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, third_party_residents, payer,
    moderate, biographical, constrained, local).

% Continue to operate in criminalized supply chains since production, trafficking, and sale remain prohibited. Bear intensified enforcement attention that has been displaced from users to suppliers. Face incarceration, asset seizure, and violence risks that decriminalization for users does not reduce.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, illicit_suppliers, payer,
    moderate, immediate, trapped, local).

% Maintain that drug use is inherently harmful and morally intolerable, requiring criminal sanction to protect users and communities. Are excluded from policy-setting tables where harm reduction is treated as settled public health consensus. Would reassert criminal penalties for possession if admitted.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% Argue that decriminalization without regulated legal supply perpetuates dangerous illicit markets and denies users quality-controlled products. Are excluded from the policy conversation when the state frames the choice as binary between prohibition and harm reduction. Would push for full commercial legalization and regulated access.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective management of drug-related health externalities by replacing criminal sanctions with public health engagement, reducing overdose mortality and infectious disease transmission through centralized service provision and population-level behavior change.
% TRANSFER_FUNCTION: Moves state authority from criminal justice frameworks to public health frameworks; moves public expenditure from enforcement and incarceration to clinical and community services; moves drug users from carceral settings to health service settings; maintains the transfer of supply-side enforcement pressure onto illicit traffickers.
% ABSENT_VOICES: Prohibition advocates who view drug use as requiring criminal sanction are excluded from policy tables in harm reduction jurisdictions. Legalization advocates who view decriminalization as an incoherent half-measure are likewise excluded. Both groups would contest the constraint's framing but are structurally marginalized in the public health consensus.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight, personal possession would revert to criminal status or advance to unregulated legalization, public health services would lose state mandate and funding, enforcement resources would shift back to user-level arrests, and overdose and disease trajectories would change significantly. Institutional arrangements depend on it.
% FOUNDING_PROBLEM: Criminalization of drug use produced mass incarceration, uncontrolled disease transmission, high overdose mortality, and organized crime proliferation without substantially reducing drug prevalence.
% FOUNDING_PROBLEM_CORROBORATION: International public health organizations (WHO, UNODC) and epidemiological researchers outside the immediate service-provider beneficiary set attest to the health harms of criminalization. Conversely, law enforcement associations and conservative political parties contest the founding problem, asserting that decriminalization increases disorder and that criminalization successfully suppresses use.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the constraint genuinely reduces criminalization harms but preserves significant health and liberty costs for users by stopping short of legalization. Suppression (0.52) reflects the active enforcement required to police the decriminalization/legalization boundary and suppress illicit supply. Theater ratio (0.30) captures the performative dimension of maintaining a public health facade while preserving the structural conditions (illicit markets) that generate the harms services treat. Accessibility collapse (0.45) is moderate: full legalization and full prohibition remain conceptually available but politically suppressed. Resistance (0.58) is moderate-to-high: prohibition advocates, some community groups, and legalization advocates all actively contest the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the state_public_health_authority seat, the constraint is legitimate expertise-based coordination saving lives and reducing disease. From the drug_user seat, it is partial liberation coupled with continued exposure to dangerous supply and service dependency. From the third_party_resident seat, it is an imposed externality that relocated rather than resolved neighborhood disorder. From the illicit_supplier seat, it is intensified enforcement risk. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_public_health_authority sits near the beneficiary end (gains authority, budget, and mission expansion). Drug_users have mixed directionality: they benefit from exiting criminalization but are targeted by continued health harms from unregulated supply; the structural data codes them in both beneficiary and victim arrays, producing a mid-range effective extraction that is health-harm-weighted. Third_party_residents and illicit_suppliers are structural targets (high d) bearing externalities and enforcement respectively. Service_providers are near-beneficiaries (funding flows to them). Excluded advocates have analytical or constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure coordination (rope would ignore the continued victimhood of users and third parties) or pure extraction (snare would deny the genuine mortality and disease reductions the framework produces). The temporal measurements show slowly rising extractiveness as the institutional apparatus matures, suggesting the coordination function may be accumulating bureaucratic overhead without a corresponding sunset â but the absence of a sunset clause and the genuine persistence of the founding health problems prevent scaffold classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    illicit_market_persistence,
    'Does decriminalization without legalization structurally sustain the illicit supply chain that generates third-party crime and disease risks, making the harm reduction framework self-undermining?',
    'Jurisdictional comparison between harm reduction with decriminalization-only versus jurisdictions that have introduced regulated supply models; differential outcomes in overdose rates, infectious disease incidence, and property crime.',
    'If yes, the framework''s third-party victim set is endogenous to the constraint itself, which would raise the effective extractiveness and push the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(illicit_market_persistence, empirical, 'Whether the constraint''s partial liberalization sustains the harms it claims to manage.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (users compelled into services by threat of criminalization for non-engagement) or internalized (users voluntarily seek services due to health concerns)?',
    'Post-policy transition suppression trajectory: compare service engagement rates and user self-reporting before and after full decriminalization in jurisdictions that have implemented it.',
    'If structural, effective extraction is higher than the raw measure suggests because users carry the coercive pressure with them; if internalized, the constraint operates closer to genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in public health engagement.').

omega_variable(
    kernel_reading_stability,
    'Can the harm reduction reading stabilize without drifting toward either prohibition (under political pressure) or legalization (as evidence accumulates)?',
    'Longitudinal policy trajectory in jurisdictions adopting harm reduction over multi-decade intervals.',
    'If the reading is structurally unstable, it is better understood as a transitional scaffold rather than a steady-state tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether the harm reduction reading is a durable equilibrium or a waystation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sca_hr_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sca_hr_tr_t10, substance_control_authority__harm_reduction_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(sca_hr_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(sca_hr_tr_t30, substance_control_authority__harm_reduction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(sca_hr_tr_t40, substance_control_authority__harm_reduction_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(sca_hr_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sca_hr_be_t10, substance_control_authority__harm_reduction_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(sca_hr_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(sca_hr_be_t30, substance_control_authority__harm_reduction_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(sca_hr_be_t40, substance_control_authority__harm_reduction_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sca_hr_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sca_hr_su_t10, substance_control_authority__harm_reduction_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(sca_hr_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(sca_hr_su_t30, substance_control_authority__harm_reduction_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(sca_hr_su_t40, substance_control_authority__harm_reduction_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_authority kernel, decomposed per the Îµ-invariance principle because the prohibition, harm reduction, and legalization readings have structurally distinct beneficiary/victim sets, enforcement mechanisms, and Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
