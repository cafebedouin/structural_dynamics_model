% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Regime for Substance Use
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the harm_reduction_reading of the
 *   substance_control_kernel. The kernel is the contested question of how the
 *   state should relate to substance use. The harm reduction reading claims
 *   substance use is a health condition requiring pragmatic intervention to
 *   reduce harm, independent of cessation. This reading emerged from the AIDS
 *   crisis as a pragmatic compromise: users exit the criminal victim set for
 *   possession but remain subject to paternalistic health intervention;
 *   supply chains remain criminalized; the state shifts from punisher to
 *   service provider. The constraint is a tangled rope — it coordinates
 *   genuine life-saving services (needle exchange, OAT, overdose prevention)
 *   while extracting compliance through clinical gatekeeping, maintaining
 *   supply-side criminalization that fuels violence and incarceration for
 *   low-level participants, and legitimizing a therapeutic state that claims
 *   benevolence while exercising control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.35).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.45).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Regime for Substance Use").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, 'b1000adf-9244-4dbd-98dd-6a513b9a7493').
narrative_ontology:cs_kernel_codification('b1000adf-9244-4dbd-98dd-6a513b9a7493', implicit).
narrative_ontology:cs_authority_grounding('b1000adf-9244-4dbd-98dd-6a513b9a7493', practice).
narrative_ontology:cs_interpretation_layer_present('b1000adf-9244-4dbd-98dd-6a513b9a7493').
narrative_ontology:cs_reading_relation('b1000adf-9244-4dbd-98dd-6a513b9a7493', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1000adf-9244-4dbd-98dd-6a513b9a7493', substance_control_kernel__legalization_reading, influences).
narrative_ontology:cs_axiom('b1000adf-9244-4dbd-98dd-6a513b9a7493', foundational, substance_use_is_health_condition_not_crime).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition_not_crime, holdable).
narrative_ontology:cs_axiom_grounding('b1000adf-9244-4dbd-98dd-6a513b9a7493', substance_use_is_health_condition_not_crime, empirically_contingent).
narrative_ontology:cs_axiom('b1000adf-9244-4dbd-98dd-6a513b9a7493', foundational, pragmatic_intervention_requires_no_cessation_precondition).
narrative_ontology:cs_axiom_status(pragmatic_intervention_requires_no_cessation_precondition, holdable).
narrative_ontology:cs_axiom_grounding('b1000adf-9244-4dbd-98dd-6a513b9a7493', pragmatic_intervention_requires_no_cessation_precondition, instrumental).
narrative_ontology:cs_reference_frame('b1000adf-9244-4dbd-98dd-6a513b9a7493', prohibition_as_default_state).
narrative_ontology:cs_drift_state('b1000adf-9244-4dbd-98dd-6a513b9a7493', contemporary_harm_reduction_consensus, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b1000adf-9244-4dbd-98dd-6a513b9a7493', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, treatment_infrastructure_operators).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, users_accessing_services).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs_under_paternalistic_supervision).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, low_level_supply_chain_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, users_accessing_services).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, substance_use_is_health_condition).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, pragmatic_intervention_reduces_harm).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, cessation_not_prerequisite_for_care).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and fund harm reduction programs (needle exchange, OAT, overdose prevention sites). They set clinical guidelines and eligibility criteria. Their budgets expand under this reading. They face political risk if programs are seen as enabling use, but have institutional inertia and professional legitimacy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate syringe services, OAT clinics, supervised consumption sites. Receive public funding and professional recognition. Bear operational costs, regulatory compliance burdens, and community opposition. Their existence depends on the policy frame; they cannot easily exit without losing professional identity and funding streams.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers, payer).

% Run residential and outpatient treatment programs. Benefit from referral pipelines created by harm reduction engagement. Have diversified revenue (insurance, grants, private pay). Can pivot to other behavioral health services if policy shifts. Not structurally dependent on this specific constraint.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, treatment_infrastructure_operators, beneficiary,
    powerful, biographical, mobile, national).

% Access sterile equipment, overdose reversal, medication-assisted treatment without requiring abstinence. Gain survival benefits and health access. But remain subject to clinical gatekeeping, program rules, surveillance, and the stigma of being 'in the system.' Exit means losing the only reliable safety net; identity is fused with patient/client status.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, users_accessing_services, beneficiary,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, users_accessing_services, payer).

% No longer face possession charges for personal use, but are channeled into mandatory assessment, treatment coercion (drug courts, probation conditions), and surveillance. The 'health' frame legitimizes compulsion that the criminal frame made visible. They bear the costs of a system that claims to help them but controls them. Exit is geographically and economically blocked.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs_under_paternalistic_supervision, payer,
    powerless, biographical, trapped, local).

% Supply remains criminalized; street-level sellers, couriers, and small-scale producers face unchanged enforcement. Harm reduction does not extend to them. They absorb the violence and incarceration risk of the prohibited supply chain that the health system depends on for its patient population. No exit without leaving the trade entirely — which the same economic conditions that drew them in make nearly impossible.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, low_level_supply_chain_participants, payer,
    powerless, biographical, trapped, regional).

% Retain supply-side enforcement mandate while ceding possession enforcement to health sector. Budgets shift toward interdiction and trafficking investigations. They resist full decriminalization but accept harm reduction as force multiplier for 'upstream' targets. Institutional identity remains 'war on drugs' — pivot is rhetorical and budgetary, not cultural.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, payer).

% Oppose harm reduction as normalization of use. Mobilize electoral and legislative pressure to restrict funding, zone out services, recriminalize possession. They are excluded from the policy consensus that instantiated this reading but retain veto points. Their power is defensive and reactive.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibitionist_political_actors, excluded,
    powerful, biographical, arbitrage, national).

% Argue that harm reduction is a half-measure that preserves the criminal supply chain and state paternalism. Push for regulated markets, full decriminalization, and removal of clinical gatekeeping. They are excluded from the current policy settlement but shape the Overton window. Their constituency overlaps with users but their structural position is distinct.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, mobile, national).

% Produce the evidence base for harm reduction's mortality and morbidity reductions. Their metrics (overdose deaths, HIV/HCV incidence, treatment retention) become the primary observables of the constraint's success. They do not set policy but define the terms on which it is judged.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, epidemiologists_and_health_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public health response to substance-related mortality and disease transmission by channeling people who use drugs into service engagement without requiring abstinence as a precondition. Solves the collective action problem of reaching a hidden, stigmatized population with life-saving interventions.
% TRANSFER_FUNCTION: Moves public funding and clinical authority from criminal justice institutions to health institutions. Transfers the burden of engagement from police to clinicians. People who use drugs receive services but surrender autonomy to clinical gatekeeping. Low-level supply chain participants bear unchanged criminalization costs that subsidize the health system's patient pipeline.
% ABSENT_VOICES: People who use drugs who reject the patient identity and want autonomy over their consumption (not services). Low-level supply chain participants who are erased by the health frame. Abolitionist organizers who see harm reduction as legitimizing the carceral state's pivot from prison to clinic. They are absent because the policy consensus defines 'stakeholders' as service providers and funders, not the subjected population.
% DISAPPEARANCE_RATIONALE: If harm reduction infrastructure vanished overnight, overdose deaths would spike within weeks, blood-borne disease transmission would rebound, and the health system would lose its primary engagement pathway with the most marginalized users. The criminal system would reabsorb the population — but with no service capacity, mortality would exceed pre-harm-reduction baselines. The supply chain would remain criminalized. The world rearranges violently.
% FOUNDING_PROBLEM: The AIDS crisis among people who inject drugs in the 1980s-90s, combined with the failure of criminalization to reduce use or overdose deaths, created a legitimacy crisis for prohibition. Harm reduction emerged as the pragmatic compromise: keep supply illegal, but stop killing users through preventable disease and overdose.
% FOUNDING_PROBLEM_CORROBORATION: Harm reduction pioneers (e.g., Dutch junkiebond, UK Mersey model, Australian NSP architects) attest the founding problem was AIDS and overdose mortality — and that it remains live because the structural drivers (prohibition, stigma, poverty) persist. Prohibitionist critics attest the founding problem was 'drug use itself' and that harm reduction abandoned the solution (enforcement). Epidemiological data from WHO, EMCDDA, and CDC corroborate that the mortality and disease drivers the founders identified are still operating, though the specific substances and populations have shifted.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).
:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate — the constraint transfers resources to health services and away from criminal justice, but the paternalistic frame extracts autonomy from users and leaves supply-chain violence intact. Suppression (0.45) is significant but lower than prohibition — possession enforcement recedes, but clinical coercion (drug courts, mandated treatment, probation conditions) and supply enforcement persist. Theater (0.28) is rising — early harm reduction was raw pragmatism (underground needle exchange); institutionalization brought professionalization, funding requirements, and 'evidence-based' gatekeeping that serve organizational survival as much as user needs. Accessibility collapse (0.42) is moderate — alternatives exist (unregulated use, black market, abstinence-only) but are dangerous or inaccessible. Resistance (0.58) is high — from prohibitionists (moral objection), legalization advocates (half-measure critique), and users themselves (refusal of patient identity).
 *
 * PERSPECTIVAL GAP:
 *   From the public health seat, this is a rope — genuine coordination solving a collective action problem (reaching hidden populations with life-saving interventions). From the supervised user seat, it is a snare — the 'health' frame masks continued control, and exit from the patient role means losing the only safety net. From the supply-chain participant seat, it is a mountain — nothing changed; prohibition's violence persists. The engine computes these per-seat types from the structural data; the claimed tangled_rope captures the hybrid coordination/extraction structure at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and harm reduction providers are structural beneficiaries — they gain budgets, authority, and professional legitimacy (d near 0.1). Treatment operators benefit but have exit (d ~0.3). Users accessing services are dual-positioned: they gain survival resources but lose autonomy to clinical gatekeeping; identity-locked exit makes their effective d ~0.6 despite beneficiary role. People under paternalistic supervision and low-level supply participants are full targets — trapped exit, no benefit capture, d ~0.9. Law enforcement is an agenda setter that pays transition costs but retains core mandate (d ~0.4). Excluded voices (prohibitionists, legalization advocates) have arbitrage/mobile exit — they operate outside the constraint's direct operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (AIDS/overdose crisis) is contested as live or solved. The constraint persists because it solves a real coordination problem (mortality reduction) while creating a self-sustaining institutional complex (funding streams, professional careers, evidence industries). Mandatrophy is not resolved — the arrangement has not outlived its function, but its function has expanded beyond the founding problem into a permanent therapeutic governance regime. The theater rise tracks this expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_vs_autonomy_boundary,
    'Where does the harm reduction reading''s clinical gatekeeping end and autonomous use begin? Is the ''health condition'' frame inherently paternalistic, or can it be instantiated without coercion?',
    'Compare jurisdictions with decriminalization + voluntary services (Portugal, Czechia) vs. those with mandated treatment pathways (US drug courts, compulsory treatment in Asia). Track autonomy metrics: voluntary vs. coerced treatment entry, service discontinuation rates, user-reported agency.',
    'If the frame is inherently paternalistic, the constraint''s extractiveness is structurally higher than measured — the health claim is cover for control. If voluntary instantiation is stable, the constraint can evolve toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy_boundary, conceptual, 'Whether the health frame necessitates paternalistic control or can support autonomous use.').

omega_variable(
    supply_chain_criminalization_externality,
    'Does the harm reduction reading''s maintenance of supply-side criminalization constitute an extraction mechanism that subsidizes the health system''s patient pipeline?',
    'Counterfactual modeling: if supply were regulated, would overdose mortality and disease transmission fall further? Compare harm reduction outcomes in jurisdictions with de facto supply tolerance (e.g., Dutch coffee shops, US state cannabis markets) vs. strict supply enforcement.',
    'If supply criminalization feeds the health system''s patient volume (via violence, adulteration, unstable dosing), the constraint''s extraction is structurally dependent on the prohibition it claims to mitigate — a tangled rope where coordination requires the extraction it disavows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_criminalization_externality, empirical, 'Whether the constraint''s coordination function parasitically depends on the supply-side criminalization it preserves.').

omega_variable(
    kernel_reading_relations,
    'What are the structural relationships between the harm_reduction_reading and its sibling readings (prohibition_reading, legalization_reading)?',
    'Analyze whether any single policy framework could instantiate multiple readings simultaneously, or whether adoption of one logically commits a jurisdiction to rejecting the others. Track policy transitions: jurisdictions that moved from prohibition to harm reduction to legalization (or back).',
    'If harm_reduction forecloses prohibition, the kernel has a directional drift. If all three coexist, the kernel is a stable contested space. If harm_reduction influences legalization by normalizing state regulation, the drift is toward state-managed markets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations between the three readings of the substance_control_kernel.').

omega_variable(
    internalized_suppression_among_users,
    'Is the suppression experienced by people_who_use_drugs_under_paternalistic_supervision structural (clinical coercion, legal mandate) or internalized (acceptance of patient identity, belief that they need supervision)?',
    'Longitudinal qualitative studies of users who exit mandated treatment: do they continue voluntary engagement? Measure suppression persistence after legal mandate removal (e.g., post-probation). Compare self-reported autonomy in voluntary vs. coerced treatment cohorts.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural measure — the target carries the constraint with them. This would increase effective extraction for the identity-locked seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_among_users, empirical, 'Structural vs. internalized suppression mechanism for the supervised user seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1985, substance_control_kernel__harm_reduction_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(subs_tr_t1995, substance_control_kernel__harm_reduction_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(subs_tr_t2005, substance_control_kernel__harm_reduction_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(subs_tr_t2015, substance_control_kernel__harm_reduction_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(subs_tr_t2020, substance_control_kernel__harm_reduction_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(subs_tr_t2025, substance_control_kernel__harm_reduction_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t1985, substance_control_kernel__harm_reduction_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(subs_be_t1995, substance_control_kernel__harm_reduction_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(subs_be_t2005, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(subs_be_t2015, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2015, 0.32).
narrative_ontology:measurement(subs_be_t2020, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2020, 0.34).
narrative_ontology:measurement(subs_be_t2025, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1985, substance_control_kernel__harm_reduction_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(subs_su_t1995, substance_control_kernel__harm_reduction_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(subs_su_t2005, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(subs_su_t2015, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(subs_su_t2020, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(subs_su_t2025, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_kernel decomposes into three readings with distinct ε values and beneficiary/victim structures. Prohibition_reading: high extractiveness (~0.75), users and low-level supply as victims, moral order as vindicated proposition. Legalization_reading: low extractiveness (~0.15), regulated market operators as beneficiaries, third-party harm prevention as coordination. Harm_reduction_reading (this story): moderate extractiveness (~0.35), health agencies and users as dual-positioned beneficiaries/payers, supply chain as persistent victims. The three form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, powerless, 0.85).
constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, organized, 0.25).
constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
