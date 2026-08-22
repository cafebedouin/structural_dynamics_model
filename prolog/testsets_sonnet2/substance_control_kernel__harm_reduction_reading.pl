% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Harm Reduction Reading of Substance Control (Health-Framed Supervision)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the harm reduction reading of the substance
 *   control kernel: substance use is treated as a chronic health condition
 *   requiring pragmatic, non-judgmental intervention to reduce mortality and
 *   morbidity, independent of whether use ceases. Under this reading, the
 *   user exits the criminal victim set that characterizes the prohibition
 *   reading — arrest and incarceration recede as the primary user-facing
 *   mechanism — but the user does not exit state involvement altogether. They
 *   become subject to a health-supervisory apparatus: registration, case
 *   management, monitored consumption, and data collection on overdose and
 *   disease transmission, administered by public health institutions rather
 *   than police. Critically, the supply chain remains fully criminalized
 *   under this reading, so users continue to depend on an unregulated,
 *   contaminated, unpredictable drug supply — the harm reduction apparatus
 *   manages the consequences of that criminalized supply without removing it.
 *   This is a distinct constraint from both the prohibition_reading (which
 *   criminalizes and punishes the user directly) and the legalization_reading
 *   (which would remove the paternalistic supervisory function entirely and
 *   regulate supply commercially). Each reading has its own epsilon and
 *   stakeholder structure; they are linked only via network reference, not
 *   merged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.42).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.38).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Reading of Substance Control (Health-Framed Supervision)").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, 'e2850415-e3d5-472b-a35f-2dfd32150767').
narrative_ontology:cs_kernel_codification('e2850415-e3d5-472b-a35f-2dfd32150767', distributed).
narrative_ontology:cs_authority_grounding('e2850415-e3d5-472b-a35f-2dfd32150767', expertise).
narrative_ontology:cs_interpretation_layer_present('e2850415-e3d5-472b-a35f-2dfd32150767').
narrative_ontology:cs_reading_relation('e2850415-e3d5-472b-a35f-2dfd32150767', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2850415-e3d5-472b-a35f-2dfd32150767', substance_control_kernel__legalization_reading, influences).
narrative_ontology:cs_axiom('e2850415-e3d5-472b-a35f-2dfd32150767', foundational, use_cessation_not_required_for_care).
narrative_ontology:cs_axiom_status(use_cessation_not_required_for_care, holdable).
narrative_ontology:cs_axiom_grounding('e2850415-e3d5-472b-a35f-2dfd32150767', use_cessation_not_required_for_care, instrumental).
narrative_ontology:cs_axiom('e2850415-e3d5-472b-a35f-2dfd32150767', foundational, addiction_is_chronic_health_condition).
narrative_ontology:cs_axiom_status(addiction_is_chronic_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('e2850415-e3d5-472b-a35f-2dfd32150767', addiction_is_chronic_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('e2850415-e3d5-472b-a35f-2dfd32150767', secondary, supply_side_criminalization_remains_necessary).
narrative_ontology:cs_axiom_status(supply_side_criminalization_remains_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e2850415-e3d5-472b-a35f-2dfd32150767', supply_side_criminalization_remains_necessary, conventional).
narrative_ontology:cs_reference_frame('e2850415-e3d5-472b-a35f-2dfd32150767', medicalized_supervisory_intervention).
narrative_ontology:cs_drift_state('e2850415-e3d5-472b-a35f-2dfd32150767', post_overdose_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2850415-e3d5-472b-a35f-2dfd32150767', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, hospital_systems).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, unregulated_supply_dependent_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, family_and_community_members).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, addiction_is_a_chronic_health_condition).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, abstinence_is_not_a_precondition_for_care).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exit the criminal victim set of prohibition (no longer primary arrest targets) but become subjects of a health-surveillance apparatus: mandated registration at needle exchanges or supervised consumption sites, case-managed by health workers, tracked for overdose and disease data. They remain dependent on a criminalized, unregulated supply chain for the substance itself, so the harm-reduction services mitigate but do not remove the core danger. Exit from the arrangement means either abstaining (not required, but the only true exit) or returning to fully clandestine use with none of the mitigations.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, payer,
    powerless, biographical, trapped, local).

% Rely on a supply chain that remains fully criminalized under this reading — only the user-facing enforcement recedes. They face contamination, unpredictable potency, and overdose risk that harm reduction services (naloxone distribution, drug checking) mitigate but cannot eliminate because the reading does not touch supply legality. They are the population whose overdose and infection statistics become the reading's primary success metric, without having consented to that role.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, unregulated_supply_dependent_users, payer,
    powerless, immediate, trapped, local).

% Design and administer the harm reduction framework: needle exchanges, supervised consumption sites, naloxone programs, case management protocols. They set the terms of what counts as acceptable intervention and collect the overdose/disease-transmission data that justifies continued funding and authority. Their institutional survival is increasingly tied to administering this framework rather than to any specific health outcome.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Nonprofit and contracted organizations that run the exchanges and consumption sites, receiving grant funding tied to the harm-reduction framework's continuation. They benefit from stable program funding and expanded mandate but depend on the health-condition framing remaining politically viable; a shift toward either prohibition or full legalization would restructure or eliminate their funding model.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers, agenda_setter).

% Absorb reduced emergency costs from overdose deaths and infectious disease outbreaks (HIV, hepatitis C) that the harm reduction infrastructure prevents or catches earlier, relative to a pure-prohibition baseline. They benefit financially and operationally without administering the constraint themselves.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, hospital_systems, beneficiary,
    institutional, generational, analytical, regional).

% Retain full enforcement authority over the supply chain (dealers, traffickers, manufacturers) even as user-facing enforcement recedes. This preserves a substantial enforcement mandate and budget while shifting public narrative credit for reduced user-side harm to the health agencies. They still make arrests, just further up the supply chain, and their institutional footprint is not meaningfully reduced by this reading.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, agenda_setter).

% Low-level dealers and couriers, often themselves substance users, remain fully within the criminal enforcement apparatus that this reading leaves untouched. They have no voice in a framework that reclassifies the user as a patient while leaving the supplier as a criminal target — a distinction that is often factually blurry at the street level.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, supply_chain_participants, excluded,
    powerless, immediate, trapped, local).

% Benefit from reduced overdose deaths and disease transmission among relatives and neighbors who use drugs, and from reduced visible public disorder associated with unmanaged use, without bearing direct costs of the intervention.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, family_and_community_members, beneficiary,
    moderate, biographical, mobile, local).

% Object that harm reduction normalizes and enables continued use rather than pursuing cessation and moral accountability. Their objection is heard in political debate but structurally excluded from the design of the harm-reduction apparatus itself, which is built and run by public health rather than criminal-justice moral framing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibitionist_advocacy_groups, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public-health response to substance use that reduces overdose deaths, disease transmission, and acute health-system costs by meeting users where they are, without requiring abstinence as a precondition for receiving care or mitigation services.
% TRANSFER_FUNCTION: Moves enforcement resources and narrative legitimacy from a punitive to a service-provision model; moves health and mortality risk data collection onto the user population; moves funding from carceral budgets toward health agencies and contracted service providers, while leaving supply-side criminal enforcement largely intact.
% ABSENT_VOICES: Supply chain participants (low-level dealers, often themselves users) have no seat in a framework that reclassifies the user as patient but leaves the supplier as criminal target. Prohibitionist advocacy groups are heard in political debate but excluded from designing the apparatus. Legalization advocates argue the paternalistic health-supervision layer is itself a residual control mechanism that should be dismantled, not administered.
% DISAPPEARANCE_RATIONALE: If this reading's apparatus (needle exchanges, supervised consumption sites, harm-reduction case management) disappeared overnight, overdose deaths and disease transmission rates would rise sharply in short order, hospital emergency burden would increase, and the political vacuum would likely be filled by a reversion to prohibition-style enforcement absent an active legalization alternative. Real institutional and clinical infrastructure currently depends on this arrangement's continuation.
% FOUNDING_PROBLEM: Rising overdose deaths and HIV/hepatitis C transmission among people who use drugs, driven by unsafe use practices under a purely punitive regime that deterred contact with health services and left supply quality and use conditions entirely unregulated.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological data (overdose mortality statistics, disease surveillance) collected by public health bodies outside the harm-reduction provider organizations themselves corroborates that the mortality and transmission problem remains live; peer-reviewed evaluations of supervised consumption sites in multiple jurisdictions, conducted by academic researchers not funded by the service providers, support continued need. Legalization advocates, an outside constituency, corroborate that the underlying health risks are real but argue the paternalistic supervisory layer this reading retains is not itself required to address them.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) reflects genuine reduction relative to a punitive baseline but retained paternalistic control: users are relieved of criminal liability but placed under continuous health surveillance and case management they did not choose, and the underlying supply-side danger is unaddressed by this reading. Suppression (0.38) is meaningfully lower than a prohibition reading's would be, but is non-trivial because harm reduction sites often require registration, behavioral compliance, and data-sharing as conditions of service — declining services can still trigger loss of access to other supports. Theater ratio (0.28) reflects an honest but partial function: services demonstrably reduce overdose deaths and disease transmission (this is not primarily theatrical), but a rising share of program activity over the interval is data collection and reporting infrastructure that serves institutional survival and funding renewal rather than the users themselves. Accessibility collapse (0.45) is moderate: this reading forecloses on cessation-as-precondition models of care, but users can still access harm reduction services or return to unmanaged use — the pathways are not fully collapsed. Resistance (0.55) reflects real friction from prohibitionist advocacy on one side and legalization advocacy on the other, both objecting to the health-supervisory middle position from opposite directions.
 *
 * PERSPECTIVAL GAP:
 *   From the public health agency and provider seats, this arrangement is a coordination success: mortality and transmission rates improve, funding is justified, institutional mandate is coherent. From the payer seats (people who use drugs), the same arrangement is experienced as a substitution of one form of state control (punitive) for another (supervisory-paternalistic), with the core danger (contaminated, unregulated supply) left fully in place. The engine's per-seat computation should reflect this: the agenda-setter and beneficiary seats likely compute closer to rope, while the payer seats compute closer to tangled_rope or even snare-adjacent given continued supply-side danger they did not consent to and cannot exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and harm reduction service providers sit at the beneficiary/agenda-setter end: they design and administer the apparatus and their institutional survival depends on its continuation. Hospital systems and community members are diffuse beneficiaries who gain from reduced acute costs and visible disorder without bearing the administrative burden. Law enforcement retains a beneficiary position via continued supply-side enforcement mandate even as its user-facing role recedes — this is a structural continuity the reading does not disturb. People who use drugs and unregulated-supply-dependent users are the payers: they bear residual overdose and infection risk from a criminalized supply chain the reading does not touch, plus the new costs of health surveillance and behavioral conditions attached to service access. Supply chain participants are excluded entirely — the reading's reclassification of the user as patient does not extend to them, and they remain fully within the punitive apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (overdose mortality and disease transmission under punitive-only conditions) remains live and independently corroborated by epidemiological surveillance outside the service-provider organizations, so this is not a case of mandate outliving function. However, the tangled_rope structure is appropriate rather than a pure rope because genuine coordination benefit (reduced mortality, reduced transmission, reduced acute health costs) coexists with asymmetric extraction: users bear ongoing supply-side danger and new supervisory costs while institutional actors (public health agencies, providers, law enforcement) gain funding, mandate, and narrative legitimacy from administering a partial fix rather than pursuing the more complete remedy (supply regulation) that the legalization reading would provide. The active enforcement requirement is satisfied by continued supply-chain criminalization, which this reading depends on to keep the user-as-patient framing coherent — if supply were also decriminalized, this reading would collapse into something closer to the legalization reading's territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_criminalization_coherence,
    'Can a health-condition framing of use coherently coexist with continued full criminalization of supply, or does retaining supply-side punishment contaminate the health framing by keeping users dependent on a dangerous, unregulated product?',
    'Compare overdose and contamination-related harm rates in jurisdictions that pair harm reduction services with continued supply criminalization versus jurisdictions that pair harm reduction with regulated/decriminalized supply (e.g., Portugal-style decriminalization, Swiss heroin-assisted treatment, or full legalization pilots).',
    'If harm rates remain substantially elevated under continued supply criminalization relative to supply-regulated comparators, it would support the legalization reading''s claim that this reading''s paternalistic supervisory layer is a partial and insufficient fix rather than a stable equilibrium — potentially reclassifying this constraint''s extraction as higher than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_criminalization_coherence, empirical, 'Whether the health framing is undercut by retained supply-side criminalization.').

omega_variable(
    paternalism_vs_care_boundary,
    'Where is the line between the supervisory conditions this reading imposes (registration, case management, monitored consumption) as genuine care infrastructure versus as a residual control mechanism replacing criminal-justice control with health-bureaucratic control?',
    'User-reported experience surveys and qualitative research on whether service conditions are experienced as supportive or as surveillance/compliance burdens, compared across program designs with varying levels of mandatory participation.',
    'If users overwhelmingly experience the supervisory conditions as burdensome control rather than supportive care, the extractiveness score should rise and the constraint moves closer to snare/tangled_rope on the payer seat; if experienced as genuinely supportive, closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_care_boundary, conceptual, 'Whether supervisory conditions are care or control from the user''s own vantage.').

omega_variable(
    kernel_framing_choice,
    'Is the health-condition framing of substance use itself contestable as the correct kernel-level premise, independent of which reading is adopted — i.e., is the disagreement really about facts (is addiction a chronic disease) or about values (what obligations does the state have toward people who use drugs)?',
    'This is inherently a conceptual/preference-laden question not fully resolvable by data alone; track whether cross-disciplinary consensus (addiction medicine, epidemiology, ethics, law) converges or remains split along disciplinary and political lines over time.',
    'If a strong empirical/professional consensus solidifies around the chronic-disease model, this reading''s foundational premise strengthens relative to the prohibition_reading''s moral-transgression premise (which becomes harder to hold as empirically credible); this does not resolve the harm_reduction vs legalization axis, which is more a values dispute about paternalism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the health-condition kernel premise is empirically or normatively contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__harm_reduction_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__harm_reduction_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__harm_reduction_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__harm_reduction_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(subs_tr_t24, substance_control_kernel__harm_reduction_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__harm_reduction_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__harm_reduction_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__harm_reduction_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__harm_reduction_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(subs_be_t24, substance_control_kernel__harm_reduction_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__harm_reduction_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__harm_reduction_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__harm_reduction_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__harm_reduction_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(subs_su_t24, substance_control_kernel__harm_reduction_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the substance_control_kernel. prohibition_reading treats substance use as moral transgression requiring punishment (high extraction, users as primary criminal victims, full supply and demand criminalization). legalization_reading treats substance use as an individual liberty issue with state intervention limited to externality capture (lowest extraction, regulated commercial supply, no paternalistic supervision). This harm_reduction_reading occupies a structural middle position: it relieves users of criminal liability and offers genuine mortality/morbidity-reducing services, but retains supply-side criminalization and imposes new paternalistic health-supervisory conditions on users, producing a distinct epsilon (0.42) between the other two readings' epsilon values. Each file authors its own beneficiary/victim structure and stakeholder seats; they are not merged and do not share a single epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
