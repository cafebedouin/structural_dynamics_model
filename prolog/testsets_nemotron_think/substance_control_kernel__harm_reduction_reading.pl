% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Harm Reduction Framework for Substance Use
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm reduction reading reframes substance use from a criminal justice
 *   problem to a public health condition, establishing state-funded services
 *   (needle exchange, opioid agonist therapy, supervised consumption, drug
 *   checking) that reduce mortality and disease transmission without
 *   requiring abstinence. Users exit the criminal victim set for possession
 *   but remain subject to paternalistic health interventions — mandatory
 *   treatment referrals, supervised consumption rules, medication compliance
 *   conditions. The supply chain remains fully criminalized, maintaining a
 *   parallel enforcement track. The state shifts from punisher to service
 *   provider, extracting legitimacy and budget authority from the health
 *   frame while retaining coercive leverage over both users (through service
 *   conditionalities) and suppliers (through unchanged trafficking laws).
 *   This is a tangled rope: genuine coordination on overdose and disease
 *   reduction coexists with asymmetric extraction via paternalistic service
 *   conditions and continued supply-chain criminalization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.42).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.55).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Framework for Substance Use").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '8f092aed-bf66-4337-b9cb-9601170291a6').
narrative_ontology:cs_kernel_codification('8f092aed-bf66-4337-b9cb-9601170291a6', formalized).
narrative_ontology:cs_authority_grounding('8f092aed-bf66-4337-b9cb-9601170291a6', extraction).
narrative_ontology:cs_interpretation_layer_present('8f092aed-bf66-4337-b9cb-9601170291a6').
narrative_ontology:cs_reading_relation('8f092aed-bf66-4337-b9cb-9601170291a6', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f092aed-bf66-4337-b9cb-9601170291a6', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('8f092aed-bf66-4337-b9cb-9601170291a6', foundational, health_condition_framing_primacy).
narrative_ontology:cs_axiom_status(health_condition_framing_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8f092aed-bf66-4337-b9cb-9601170291a6', health_condition_framing_primacy, deontological).
narrative_ontology:cs_axiom('8f092aed-bf66-4337-b9cb-9601170291a6', foundational, state_paternalistic_intervention_justified_for_harm_reduction).
narrative_ontology:cs_axiom_status(state_paternalistic_intervention_justified_for_harm_reduction, holdable).
narrative_ontology:cs_axiom_grounding('8f092aed-bf66-4337-b9cb-9601170291a6', state_paternalistic_intervention_justified_for_harm_reduction, instrumental).
narrative_ontology:cs_reference_frame('8f092aed-bf66-4337-b9cb-9601170291a6', public_health_emergency_response).
narrative_ontology:cs_drift_state('8f092aed-bf66-4337-b9cb-9601170291a6', contemporary_fentanyl_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8f092aed-bf66-4337-b9cb-9601170291a6', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, general_public).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_apparatus).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, drug_suppliers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, law_enforcement).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, harm_reduction_reduces_overdose_mortality).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, needle_exchange_reduces_hiv_transmission).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, pragmatic_health_intervention_superior_to_punitive_approach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% No longer criminally punished for possession/use, but subject to mandatory treatment referrals, supervised consumption site rules, medication-assisted treatment requirements, and paternalistic service conditions. Benefit from sterile equipment, overdose prevention, and low-threshold care but cannot access services without accepting state-defined treatment frameworks. Exit from the intervention framework is structurally difficult due to stigma, dependency on state services, and identity fusion with 'patient' role.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, beneficiary).

% Designs and funds harm reduction services (needle exchange, OAT, supervised consumption, drug checking). Gains institutional legitimacy, expanded mandate, and budget authority by managing the 'health condition' frame. Enforces service standards and treatment pathways. Retains coercive leverage through mandatory reporting, treatment compliance requirements, and control over service access criteria. Can pivot between harm reduction and abstinence-oriented models as political winds shift.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% No longer targets low-level possession, reducing workload and political liability. Retains supply-chain enforcement mandate (trafficking, manufacturing) with enhanced resources redirected from possession policing. Benefits from reduced community hostility and clearer mission focus. Bears cost of adapting to new protocols and loss of asset forfeiture revenue from possession arrests. Exit from the reconfigured role is constrained by institutional inertia and union contracts.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, law_enforcement, payer).

% Remain fully criminalized at all supply-chain levels (import, wholesale, retail). Face unchanged or intensified enforcement. No access to harm reduction services or legal protections. The harm reduction framework explicitly excludes them from its coordination function while maintaining their extraction as enforcement targets. Exit from illicit trade is structurally trapped by capital requirements, violence risk, and criminal record barriers.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, drug_suppliers, payer,
    moderate, biographical, trapped, national).

% Experiences reduced visible drug use, discarded syringes, overdose deaths in public spaces, and communicable disease spillover. Pays through taxes for harm reduction infrastructure. Can exit the framework's effects by relocating, but the national scope makes this costly. Benefits are diffuse and indirect; costs are concentrated in taxation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Advocate for recovery-oriented systems requiring cessation. Structurally excluded from harm reduction policy design and funding decisions. Their preferred framework (treatment-as-abstinence) competes for the same public resources. Would object to state-sanctioned substance use facilitation but lack institutional access to block harm reduction expansion.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, abstinence_only_advocates, excluded,
    moderate, generational, constrained, national).

% Generate evidence on overdose mortality, disease transmission, service uptake, and cost-effectiveness. Their findings shape the harm reduction framework's legitimacy but they do not administer services or bear enforcement costs. Exit is analytical — they can change research focus without personal consequence.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces overdose mortality, HIV/HCV transmission, and public disorder from substance use by providing sterile equipment, supervised consumption, opioid agonist therapy, and low-threshold medical care — solving the collective action problem of disease spread and fatal overdose without requiring abstinence.
% TRANSFER_FUNCTION: Moves public funding from criminal justice (arrest, incarceration, court processing) to health services (OAT, needle exchange, supervised consumption sites, drug checking). People who use drugs receive services but cede autonomy to state-defined treatment pathways. Drug suppliers remain extraction targets for supply-chain enforcement. Law enforcement resources shift from possession to trafficking interdiction.
% ABSENT_VOICES: Abstinence-only advocates (excluded from policy design), people who use drugs who reject paternalistic service conditions (structurally silenced by service access requirements), drug suppliers (criminalized by design), and communities impacted by persistent supply-chain violence (their safety is not a framework observable).
% DISAPPEARANCE_RATIONALE: If harm reduction vanished overnight, overdose deaths would spike within weeks (loss of naloxone distribution, supervised consumption, OAT access), HIV/HCV outbreaks would re-emerge among PWID (loss of sterile equipment), public disorder would increase (visible use, discarded syringes), and the state would revert to possession-based enforcement — reorganizing criminal justice, health, and street-level dynamics.
% FOUNDING_PROBLEM: The failure of criminal prohibition to reduce substance-related harm: escalating overdose mortality (particularly fentanyl), HIV/HCV epidemics among people who inject drugs, mass incarceration for possession, and the inability of abstinence-only treatment to engage the majority of people with substance use disorders.
% FOUNDING_PROBLEM_CORROBORATION: WHO, UNAIDS, and CDC endorse harm reduction as evidence-based; the overdose crisis (100k+ deaths/year in US alone) demonstrates prohibition's failure; academic literature (e.g., Lancet commissions, Cochrane reviews) corroborates mortality and transmission reductions from OAT and needle exchange; law enforcement leadership in multiple jurisdictions (e.g., LEAP, police chiefs associations) publicly attest that possession enforcement fails to reduce supply or harm.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects the moderated but persistent transfer: users gain services but lose autonomy; suppliers remain extraction targets; public funds shift from carceral to health systems but remain under state control. Suppression (0.55) is mid-range — possession enforcement recedes but supply-chain enforcement intensifies, and service conditionalities create soft coercion. Theater ratio (0.28) captures the gap between 'voluntary, low-threshold' rhetoric and the reality of treatment pathways that gate housing, benefits, and legal relief on compliance. Accessibility collapse (0.35) is moderate — alternatives exist (black market, abstinence programs, untreated use) but are severely degraded by the framework's dominance. Resistance (0.48) comes from both prohibitionists (who want harsher enforcement) and PWUD advocates (who want full decriminalization and autonomy).
 *
 * PERSPECTIVAL GAP:
 *   From the public health apparatus seat, the constraint is a rope — genuine coordination solving overdose and disease. From the PWUD seat, it is a tangled rope — coordination with paternalistic extraction. From the supplier seat, it is a snare — pure extraction via continued criminalization. From the law enforcement seat, it is a scaffold — transitional with sunset on possession enforcement but persistent supply mandate. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) represents the dominant structural truth across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The public health apparatus is the structural beneficiary (agenda_setter, d near 0.0) — it gains mandate, funding, and legitimacy. People who use drugs are dually positioned: beneficiaries of services (d ~ 0.3) but payers of paternalistic conditionalities (d ~ 0.7), netting identity_locked exit. Drug suppliers are full targets (payer, trapped, d ~ 0.95) — supply criminalization is unchanged. Law enforcement are partial beneficiaries (reduced possession burden) but constrained by redirected mandate. General public are diffuse beneficiaries (d ~ 0.2). Abstinence advocates are excluded (no structural position). Researchers are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's failure to reduce harm) remains live — overdose mortality continues to rise, novel psychoactive substances emerge, and supply-chain violence persists. The harm reduction framework has not resolved the core problem but has mitigated its deadliest manifestations. Mandatrophy is not resolved: the framework persists because it manages crisis symptoms without addressing the structural drivers (illicit market incentives, poverty, trauma, prohibition-driven potency escalation). The 'temporary' health-frame justification has hardened into a permanent institutional arrangement with its own bureaucratic inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_voluntariness_boundary,
    'At what point do service conditionalities (housing contingent on treatment, mandatory reporting, supervised consumption rules) convert harm reduction from voluntary coordination into coercive extraction?',
    'Longitudinal tracking of service dismissal rates, housing loss after treatment non-compliance, and PWUD self-reported autonomy across jurisdictions with varying conditionality regimes.',
    'If conditionalities are structurally pervasive, the constraint''s extraction is higher than measured and its classification shifts toward snare for the PWUD seat; if genuinely voluntary, the coordination function dominates and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_voluntariness_boundary, empirical, 'Whether the health intervention''s paternalistic elements constitute asymmetric extraction.').

omega_variable(
    supply_criminalization_undermines_harm_reduction,
    'Does the continued criminalization of the supply chain structurally undermine harm reduction''s coordination function by maintaining adulterated supply, fentanyl contamination, and violence that harm reduction services must then mitigate?',
    'Comparative analysis of overdose mortality and drug purity in jurisdictions with harm reduction but persistent supply criminalization vs. those with regulated supply (e.g., heroin-assisted treatment, cannabis regulation).',
    'If supply criminalization generates the very harms harm reduction treats, the coordination function is parasitically dependent on the extraction function — a tangled_rope signature where the ''rope'' creates the need for the ''tangle''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_criminalization_undermines_harm_reduction, conceptual, 'Whether the constraint''s two functions are structurally coupled in a generative loop.').

omega_variable(
    state_service_provider_capture,
    'Does the state''s role as harm reduction service provider create a dependency that prevents transition to less paternalistic models (e.g., user-led collectives, regulated supply, full decriminalization)?',
    'Institutional analysis of funding flows, regulatory capture of service standards, and political resistance to models that reduce state control (e.g., safe supply prescribing, drug user unions as service providers).',
    'If state capture is structural, the framework is a piton-in-formation — coordination function atrophying while institutional maintenance persists. If user-led models gain traction, the constraint may evolve toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_service_provider_capture, preference, 'Whether the state''s service provider role is a transitional scaffold or a permanent institutional capture.').

omega_variable(
    kernel_reading_identity,
    'Is the harm_reduction_reading a coherent structural reading of the substance_control_kernel, or does it conflate two distinct constraints (possession decriminalization + health services) that should be decomposed per ε-invariance?',
    'Test whether ε differs when measuring possession decriminalization alone vs. health service conditionalities alone vs. supply enforcement alone. If ε varies across these observables, decompose into separate constraint stories.',
    'If decomposition is warranted, this single story masks distinct extraction profiles: possession decriminalization (rope), health services with conditionalities (tangled_rope), supply enforcement (snare). The current ε = 0.42 would be a composite artifact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the harm reduction frame obscures multiple constraints with different ε values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_tr_t1980, substance_control_kernel__harm_reduction_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_tr_t1990, substance_control_kernel__harm_reduction_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_tr_t2000, substance_control_kernel__harm_reduction_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_tr_t2010, substance_control_kernel__harm_reduction_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_tr_t2020, substance_control_kernel__harm_reduction_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_tr_t2025, substance_control_kernel__harm_reduction_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_be_t1980, substance_control_kernel__harm_reduction_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_be_t1990, substance_control_kernel__harm_reduction_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_be_t2000, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_be_t2010, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_be_t2020, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_be_t2025, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_su_t1980, substance_control_kernel__harm_reduction_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_su_t1990, substance_control_kernel__harm_reduction_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_su_t2000, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_su_t2010, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_su_t2020, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement(substance_control_kernel__harm_reduction_reading_su_t2025, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, supply_chain_criminalization).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, public_health_funding_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the harm_reduction_reading of the substance_control_kernel. It decomposes the kernel into a specific constraint where the health frame coordinates harm reduction while extracting via paternalistic conditionalities and persistent supply criminalization. The prohibition_reading and legalization_reading are sibling constraints with distinct ε values and stakeholder structures. The supply_chain_criminalization constraint is structurally influenced by this reading (reduced possession enforcement redirects resources to supply interdiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, powerless, 0.65).
constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, institutional, 0.1).
constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, organized, 0.2).
constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
