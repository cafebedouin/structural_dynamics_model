% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Harm Reduction Authority: Decriminalization with Public Health Intervention
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story models the harm reduction reading of the
 *   substance_control_authority kernel: the state accepts that drug use
 *   occurs and deploys public health interventions (OAT, SSP, SCS, naloxone)
 *   to minimize health harms while maintaining decriminalization (not
 *   legalization) of possession. The constraint coordinates life-saving
 *   services for a stigmatized population but extracts residual costs onto
 *   third parties (disease risk, neighborhood disorder) and maintains state
 *   authority over the drug supply chain through continued supply-side
 *   prohibition. It is neither pure coordination (third parties pay
 *   uncompensated) nor pure extraction (PWUD genuinely benefit, mortality
 *   drops). The claimed type is tangled_rope; metrics describe a constraint
 *   that has stabilized at moderate extractiveness with persistent theater
 *   from prohibitionist rhetoric.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: Primary beneficiary (exits criminal system) and partial payer (health harms) — powerless/identity_locked
 *   - third_parties_general_public: Victim (bears disease/crime spillovers) — moderate/constrained
 *   - communities_affected_by_drug_markets: Victim (concentrated spillovers) — powerless/trapped
 *   - public_health_agencies: Agenda setter (administers constraint) — institutional/arbitrage
 *   - harm_reduction_ngos: Beneficiary (funding/mandate) — organized/mobile
 *   - law_enforcement: Agenda setter/payer (role contraction) — institutional/constrained
 *   - healthcare_providers: Beneficiary/payer (workload shift) — organized/mobile
 *   - international_drug_control_regime: Excluded (treaty tension) — institutional/trapped
 *   - public_health_researchers: Observer (analytical seat) — analytical/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.62).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.48).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Harm Reduction Authority: Decriminalization with Public Health Intervention").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '968a4617-caed-4993-901e-024ff51f82f3').
narrative_ontology:cs_kernel_codification('968a4617-caed-4993-901e-024ff51f82f3', fixed_text).
narrative_ontology:cs_authority_grounding('968a4617-caed-4993-901e-024ff51f82f3', lineage).
narrative_ontology:cs_interpretation_layer_present('968a4617-caed-4993-901e-024ff51f82f3').
narrative_ontology:cs_reading_relation('968a4617-caed-4993-901e-024ff51f82f3', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('968a4617-caed-4993-901e-024ff51f82f3', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('968a4617-caed-4993-901e-024ff51f82f3', foundational, pragmatic_harm_minimization_over_moral_purity).
narrative_ontology:cs_axiom_status(pragmatic_harm_minimization_over_moral_purity, holdable).
narrative_ontology:cs_axiom_grounding('968a4617-caed-4993-901e-024ff51f82f3', pragmatic_harm_minimization_over_moral_purity, instrumental).
narrative_ontology:cs_axiom('968a4617-caed-4993-901e-024ff51f82f3', secondary, decriminalization_without_commercialization).
narrative_ontology:cs_axiom_status(decriminalization_without_commercialization, holdable).
narrative_ontology:cs_axiom_grounding('968a4617-caed-4993-901e-024ff51f82f3', decriminalization_without_commercialization, conventional).
narrative_ontology:cs_reference_frame('968a4617-caed-4993-901e-024ff51f82f3', public_health_pragmatism_framework).
narrative_ontology:cs_drift_state('968a4617-caed-4993-901e-024ff51f82f3', contemporary_overdose_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('968a4617-caed-4993-901e-024ff51f82f3', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, harm_reduction_ngos).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, third_parties_general_public).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, communities_affected_by_drug_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, healthcare_providers).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, healthcare_providers).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, harm_reduction_evidence_base).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, public_health_pragmatism).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, decriminalization_reduces_overdose_mortality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% No longer face criminal penalties for possession/use; access syringe programs, OAT, supervised consumption. Still bear health harms (overdose risk, infectious disease, stigma). Identity fused with drug use makes exit from 'patient' role difficult; relapse cycles bind them to the service system. Benefit from decriminalization but pay with ongoing health vulnerability and institutional dependence.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs, payer).

% Bear disease transmission risks (HIV, HCV) and crime externalities (property theft, public disorder) from drug markets that persist under decriminalization. No direct say in policy design; geographic exit possible but costly. The constraint's coordination function (public health services) reduces but does not eliminate these spillovers; the residual is borne without compensation.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, third_parties_general_public, payer,
    moderate, biographical, constrained, local).

% Concentrated in specific neighborhoods where open drug scenes persist despite decriminalization. Bear disproportionate crime, discarded syringes, public injection, and social disorder. Lack political power to demand service relocation or market suppression. Trapped by housing costs and historical disinvestment; the constraint's services often locate in their neighborhoods without their consent.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, communities_affected_by_drug_markets, payer,
    powerless, generational, trapped, local).

% Design and fund harm reduction infrastructure (OAT, SSP, SCS, naloxone distribution). Gain legitimacy, funding streams, and expanded mandate. Justify authority through evidence-based pragmatism. Could pivot to other public health priorities but institutional identity is now fused with harm reduction; exit would mean dismantling built infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive state contracts and philanthropic funding to deliver frontline services. Professionalized workforce with transferable skills; can exit to other health sectors. Advocate for policy expansion; their survival depends on the constraint's persistence but they are not captive to it.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, harm_reduction_ngos, beneficiary,
    organized, biographical, mobile, national).

% Reduced arrest burden for possession but retain supply-side enforcement. Organizationally resistant to role contraction; overtime budgets and asset forfeiture revenue shrink. Institutional culture resists 'soft' approaches; individual officers constrained by union contracts and promotion pathways. Bear friction of coordinating with health agencies they historically opposed.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement, payer).

% Gain reimbursed OAT prescribing, new specialty certifications, and patient panels. Absorb complex comorbidity workload without proportional staffing increases. Emergency departments see reduced acute overdose presentations but increased chronic management burden. Professional exit to other specialties is feasible but costly.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, healthcare_providers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, healthcare_providers, payer).

% UN treaty system (1961, 1971, 1988) mandates criminalization; harm reduction policies exist in treaty tension. INCB issues critical reports but lacks enforcement teeth. States implementing harm reduction face diplomatic pressure but no material sanctions. The regime is trapped by its own treaty architecture — cannot adapt without member state consensus, which is blocked by prohibitionist powers.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, international_drug_control_regime, excluded,
    institutional, generational, trapped, global).

% Produce evidence on intervention effectiveness, cost-effectiveness, and implementation barriers. No material stake in policy outcomes; career incentives align with publication volume and citation impact. See full structure: coordination benefits, residual extraction on third parties, and the political equilibrium that sustains decriminalization without legalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a public health infrastructure (OAT, syringe service programs, supervised consumption, naloxone distribution, low-threshold primary care) that reduces population-level overdose mortality, infectious disease transmission, and acute health crises — solving the coordination problem of connecting a stigmatized, hard-to-reach population with life-saving services without requiring abstinence as a precondition.
% TRANSFER_FUNCTION: Moves state enforcement resources from criminal processing of possession to health service provision; moves health risks (overdose, infection) from unmanaged street settings into clinical supervision; moves crime and disorder externalities onto third parties in neighborhoods where services concentrate; moves legitimacy and funding to public health agencies and NGOs.
% ABSENT_VOICES: Residents of neighborhoods hosting concentrated harm reduction services (often low-income, racialized communities) who bear disproportionate spillovers but are rarely consulted in service siting decisions. Families of PWUD who navigate the system without formal representation. International treaty bodies (INCB) whose objections are noted but structurally excluded from domestic policy implementation.
% DISAPPEARANCE_RATIONALE: If harm reduction infrastructure vanished overnight: overdose deaths would surge (evidence from service closures during COVID); HIV/HCV outbreaks would re-emerge among PWUD (Scott County IN 2015 precedent); emergency departments would absorb unmanaged acute presentations; street disorder would increase as supervised consumption sites close; public health agencies would lose a primary operational mandate; law enforcement would revert to possession arrests at scale.
% FOUNDING_PROBLEM: The AIDS crisis among people who inject drugs (1980s) demonstrated that criminalization blocks access to sterile syringes and drives epidemic transmission; the overdose crisis (2010s-present) showed that prohibition-era supply control cannot prevent fentanyl contamination. Harm reduction was built to solve: how to reduce mortality and morbidity when the drug supply is unregulated and the user population is criminalized.
% FOUNDING_PROBLEM_CORROBORATION: WHO, UNAIDS, and CDC attest the founding problem (HIV/overdose among PWUD) remains live globally. Prohibitionist governments (e.g., Russia, Singapore, US federal level pre-2023) contest that the problem is solvable through supply reduction alone. Independent epidemiological modeling (e.g., Lancet commissions) corroborates that without harm reduction, mortality and transmission would be substantially higher — corroboration comes from outside the beneficiary set (public health agencies and NGOs).
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects that the state captures the legitimacy gains of 'pragmatic' policy while offloading spillover costs onto third parties and maintaining supply-side prohibition rents. Suppression (0.48) is lower than prohibition but nonzero: possession decriminalization reduces but doesn't eliminate police contact, and service access is gated by clinical criteria. Theater (0.38) captures persistent 'tough on drugs' political signaling that coexists with service expansion. Accessibility collapse (0.52) — alternatives (legalization, abolition) exist but are politically suppressed. Resistance (0.54) — from prohibitionist politicians, neighborhood groups, and treaty bodies. The measurement grid shows extractiveness rising as fentanyl increased harm severity (justifying more services), theater rising as prohibitionist rhetoric adapted, suppression requirement stabilizing after decriminalization implementation.
 *
 * PERSPECTIVAL GAP:
 *   From the PWUD seat: the constraint is a rope (genuine coordination, net benefit, voluntary engagement). From the third-party/community seat: it is a snare (extraction of spillovers without consent or compensation). From the public health agency seat: it is a scaffold (transitional toward full legalization/regulation, but no sunset declared). From the law enforcement seat: it is a piton (degraded prohibition, maintained theatrically). The engine computes these divergences from the structural data; the authored claim (tangled_rope) reflects the system-level hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   PWUD are beneficiaries (d ~ 0.2) — decriminalization and services subsidize them — but identity_locked exit keeps them from full beneficiary status (health harms persist). Third parties and affected communities are payers (d ~ 0.8) — constrained/trapped exit, bear uncompensated spillovers. Public health agencies are agenda setters with arbitrage exit (d ~ 0.15) — they capture mandate/funding. Law enforcement is agenda setter/payer (d ~ 0.6) — institutional role contraction but structural power remains. NGOs and providers are mobile beneficiaries (d ~ 0.25). International regime is excluded/trapped (d ~ 0.9) — treaty architecture prevents adaptation. Researchers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (AIDS/overdose mortality under prohibition) remains live — fentanyl supply makes it more acute. The constraint has not outlived its function; rather, its function has expanded as the drug supply became more lethal. No mandatrophy resolution: the arrangement persists because the problem persists, not because of inertia. However, the decriminalization-without-legalization equilibrium may represent a piton-like stabilization if the state refuses to regulate supply (which would solve the contamination problem) while claiming harm reduction solves the demand-side problem. The mandatrophy risk is not that the constraint is obsolete, but that it becomes a permanent substitute for supply-side regulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is the harm_reduction_reading a distinct constraint from the prohibition_reading and legalization_reading, or a transitional position on a single policy continuum?',
    'Test whether the three readings produce mutually exclusive operational logics: prohibition requires supply eradication; legalization requires market regulation; harm reduction requires service provision without supply control. If all three can be simultaneously implemented in different jurisdictions for the same substances, they are distinct constraints.',
    'If distinct, each reading gets its own ε and classification. If a continuum, they are measurement variants of one constraint (ε-invariance violation). The ε-invariance principle requires decomposition — which this story follows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints or observable variants.').

omega_variable(
    suppression_mechanism_ambiguity_pwud,
    'Is the residual suppression experienced by people who use drugs structural (clinical gatekeeping, zoning restrictions on services, police harassment despite decriminalization) or internalized (stigma, self-exclusion from services, identity fusion with ''patient'' role)?',
    'Post-policy-change suppression trajectory: if suppression metrics persist after decriminalization and service scale-up in jurisdictions that implemented both, the residual is partially internalized. Compare PWUD service engagement in decriminalized vs. prohibitionist settings controlling for service availability.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — PWUD carry the suppression with them after formal barriers fall. This would increase effective extraction for the PWUD seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity_pwud, empirical, 'Structural vs. internalized suppression for the primary beneficiary population.').

omega_variable(
    third_party_risk_attribution,
    'Are the disease transmission and crime risks borne by third parties causally attributable to the harm reduction constraint (service concentration effects) or to the persistent supply-side prohibition that the constraint maintains?',
    'Natural experiment: compare spillover metrics in jurisdictions with harm reduction + decriminalization vs. harm reduction + continued criminalization vs. legalization. If spillovers track supply prohibition intensity rather than service presence, the extraction is misattributed to the harm reduction constraint.',
    'If spillovers are prohibition effects, the harm reduction constraint''s extraction is lower than measured; the true extractive structure is the prohibition_reading. This would reclassify this reading toward rope and the prohibition reading toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_risk_attribution, empirical, 'Causal attribution of third-party harms: harm reduction services vs. persistent supply prohibition.').

omega_variable(
    stability_of_decriminalization_without_legalization,
    'Is the decriminalization-without-legalization equilibrium a stable steady state (scaffold with no sunset) or an unstable transitional form that must resolve toward either prohibition or legalization?',
    'Historical duration analysis: no jurisdiction has maintained decriminalization-without-legalization for >40 years without either recriminalizing (e.g., Oregon 2024) or moving toward regulated markets (e.g., Switzerland heroin-assisted treatment expansion, Canada safe supply pilots). Track policy trajectory in Portugal, Czechia, Netherlands, Oregon.',
    'If unstable, the constraint is a scaffold misclassified as tangled_rope — its coordination function is genuinely transitional. If stable, tangled_rope is correct: a persistent hybrid that solves the political problem of ''doing something'' without threatening prohibitionist supply control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_of_decriminalization_without_legalization, conceptual, 'Whether the hybrid policy equilibrium is a stable attractor or a transient state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scr_hr_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(scr_hr_tr_t8, substance_control_authority__harm_reduction_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(scr_hr_tr_t16, substance_control_authority__harm_reduction_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(scr_hr_tr_t24, substance_control_authority__harm_reduction_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(scr_hr_tr_t32, substance_control_authority__harm_reduction_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(scr_hr_tr_t40, substance_control_authority__harm_reduction_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(scr_hr_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(scr_hr_be_t8, substance_control_authority__harm_reduction_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(scr_hr_be_t16, substance_control_authority__harm_reduction_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(scr_hr_be_t24, substance_control_authority__harm_reduction_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(scr_hr_be_t32, substance_control_authority__harm_reduction_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(scr_hr_be_t40, substance_control_authority__harm_reduction_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(scr_hr_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(scr_hr_su_t8, substance_control_authority__harm_reduction_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(scr_hr_su_t16, substance_control_authority__harm_reduction_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(scr_hr_su_t24, substance_control_authority__harm_reduction_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(scr_hr_su_t32, substance_control_authority__harm_reduction_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(scr_hr_su_t40, substance_control_authority__harm_reduction_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This harm_reduction_reading decomposes the substance_control_authority kernel. The prohibition_reading (criminalization) has higher extractiveness on PWUD but lower on third parties; the legalization_reading (regulated markets) has lower extractiveness on both but requires state capacity for market regulation. All three share the referent (state authority over psychoactive substances) but instantiate different ε and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, institutional, 0.15).
constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
