% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm Reduction Drug Policy Framework
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm_reduction_reading instantiates a constraint where substance use
 *   is framed as a public health issue and state authority derives from a
 *   duty to minimize harm without criminalization. Users are medicalized
 *   rather than criminalized — but treatment mandates, reporting
 *   requirements, and conditionality of social benefits create moderate
 *   extractiveness. A persistent black market (prohibition residue) continues
 *   to generate violence and extraction, particularly in marginalized
 *   communities. The constraint claims to be a rope (coordination of health
 *   services) but operates as a tangled_rope: genuine coordination of harm
 *   reduction services coexists with asymmetric extraction from low-income
 *   users who face treatment mandates while the black market persists.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.42).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.58).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Drug Policy Framework").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'c9a783a1-8f39-4cdc-98a0-db44bfec14ae').
narrative_ontology:cs_kernel_codification('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', formalized).
narrative_ontology:cs_authority_grounding('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', lineage).
narrative_ontology:cs_interpretation_layer_present('c9a783a1-8f39-4cdc-98a0-db44bfec14ae').
narrative_ontology:cs_reading_relation('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', substance_control_legitimacy__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', foundational, state_duty_to_minimize_harm_justifies_mandates).
narrative_ontology:cs_axiom_status(state_duty_to_minimize_harm_justifies_mandates, holdable).
narrative_ontology:cs_axiom_grounding('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', state_duty_to_minimize_harm_justifies_mandates, instrumental).
narrative_ontology:cs_axiom('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', foundational, medicalization_preferable_to_criminalization).
narrative_ontology:cs_axiom_status(medicalization_preferable_to_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', medicalization_preferable_to_criminalization, empirically_contingent).
narrative_ontology:cs_reference_frame('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', public_health_supremacy_framework).
narrative_ontology:cs_drift_state('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', contemporary_overdose_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c9a783a1-8f39-4cdc-98a0-db44bfec14ae', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_ngos).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs_medicalized).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, low_income_people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, marginalized_communities_targeted_by_enforcement).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs_medicalized).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, public_health_supremacy_over_criminal_law).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, harm_reduction_as_state_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and fund harm reduction frameworks; set treatment protocols and mandate compliance conditions. Control licensing of treatment providers and allocation of public health funding. Justify the framework as minimizing population-level harm. Can pivot to other public health priorities if political winds shift.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive state funding and mandated patient flows from treatment mandates. Operate opioid agonist therapy programs, counseling services, and monitoring regimes. Their revenue depends on the constraint's enforcement — if mandates weaken, patient volumes drop. Can exit to private practice or other specialties.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    organized, biographical, mobile, regional).

% Gain legitimacy, funding, and policy access from the harm reduction framing. Run syringe services, overdose prevention sites, and advocacy. Dependent on state grants that require compliance with reporting and service delivery metrics. Some are co-opted into administering mandates they originally opposed. Exit means losing institutional access.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, harm_reduction_ngos, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, harm_reduction_ngos, observer).

% Access medication-assisted treatment, sterile equipment, and overdose prevention without criminal penalties for possession. But face mandatory counseling, observed dosing, urine testing, and risk of benefit sanctions for non-compliance. Their 'choice' is structured by the mandate — voluntary treatment exists but is capacity-constrained. Exit means returning to unregulated use or the black market.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs_medicalized, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs_medicalized, payer).

% Bear the most intrusive treatment mandates (daily observed dosing, frequent testing, housing/benefit conditionality) and highest black market price premiums. Lack resources for private treatment alternatives. Face compounding extraction: time costs of compliance, loss of autonomy, exposure to violence in black markets. Cannot exit — no money for private care, no political voice to change mandates.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, low_income_people_who_use_drugs, payer,
    powerless, immediate, trapped, local).

% Experience disproportionate enforcement of treatment mandates and residual prohibition enforcement (paraphernalia laws, public use citations). Black market violence concentrates in these communities. Public health services are under-resourced relative to need. The 'harm reduction' frame does not reach them equally. Exit means geographic displacement or continued exposure.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, marginalized_communities_targeted_by_enforcement, payer,
    powerless, biographical, trapped, regional).

% Operate outside the legal framework — supply unregulated substances at inflated prices. Their exclusion is structural: the constraint's persistence (both harm reduction and prohibition residues) depends on maintaining a prohibited supply chain. Would lose market position if legalization_reading prevailed. Some diversify into legitimate cannabis markets where available.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_participants, excluded,
    organized, biographical, constrained, national).

% Retain authority over trafficking, manufacturing, and public order offenses. Resist full decriminalization; advocate for treatment mandates with criminal backup. Their budget and mission depend on the prohibition residue. Can pivot to other enforcement priorities but lose drug-war institutional rationale.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, law_enforcement_residual, observer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, law_enforcement_residual, agenda_setter).

% Study outcomes across jurisdictions: Portugal's decriminalization, Switzerland's heroin-assisted treatment, Canada's safe supply, US state-level variation. See the full constraint family — harm_reduction_reading as implemented, not as theorized. No stake in any reading's victory.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level harm reduction: overdose prevention, disease transmission reduction, connection to services for a stigmatized population. Solves the collective action problem of providing health services to people who use drugs when no single provider has incentive or reach.
% TRANSFER_FUNCTION: Moves public funding and mandated compliance from state to treatment providers and public health agencies; moves autonomy and time from low-income users to the mandate apparatus; moves black market revenue to prohibited suppliers. The 'public health' transfer is bidirectional — services flow to users, compliance flows from them.
% ABSENT_VOICES: People who use drugs who reject medicalization entirely (autonomy advocates, legalization_reading adherents) are excluded from policy design. Their objection — that any state mandate over substance use is illegitimate — is structurally excluded because the harm_reduction_reading's authority derives from 'duty to minimize harm,' not autonomy. They are present in advocacy but absent from mandate-setting rooms.
% DISAPPEARANCE_RATIONALE: If the harm reduction framework vanished overnight: treatment mandates would dissolve (some users would lose access to medication-assisted treatment), public health funding would be reallocated, black market would expand into the service vacuum, overdose deaths would likely spike in the short term. The world rearranges — but the rearrangement is contested: prohibition_reading advocates say it proves the constraint was necessary; legalization_reading advocates say it proves the constraint blocked autonomy-based solutions.
% FOUNDING_PROBLEM: Overdose crisis, HIV/HCV transmission among people who inject drugs, mass incarceration for simple possession, and the failure of prohibition to reduce supply or demand. The arrangement was built to replace criminalization with health-centered response while maintaining state authority over substance use.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies attest the problem is live (rising overdose deaths, novel psychoactive substances). User-led organizations and some jurisdictions attest the founding problem is substantially solved where harm reduction is fully implemented (Portugal, Switzerland) — the constraint persists as mandate expansion. Independent epidemiological analyses support both readings: overdose mortality rises where mandates are punitive, falls where services are voluntary and comprehensive.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).
:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects treatment mandate compliance costs, reporting burdens, and black market price premiums borne disproportionately by low-income users. Suppression (0.58) is moderate — the constraint does not criminalize possession but enforces treatment adherence through legal sanctions and benefit conditionality. Theater ratio (0.28) is rising: the 'public health' framing increasingly covers enforcement of treatment compliance rather than voluntary services. Accessibility collapse (0.45) is partial — alternatives exist (voluntary treatment, decriminalization models) but are structurally marginalized. Resistance (0.35) comes from user-led movements and some jurisdictions refusing mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the public health agency seat, this is genuine coordination: services are delivered, overdose deaths decline, the constraint works. From the low-income user seat, the same structure is extraction: mandates remove autonomy, black market violence persists, the 'health' frame obscures coercion. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) captures the structural truth that both seats are simultaneously valid.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and treatment providers are beneficiaries (d ~0.15): they receive funding, authority, and patient flows. Harm reduction NGOs are beneficiaries (d ~0.25): they gain legitimacy and resources but face co-optation. Medicalized users are in a dual position — they benefit from services (d ~0.3) but bear mandates (d ~0.7). Low-income users and marginalized communities are primary targets (d ~0.85): they face the most intrusive mandates and black market violence. Black market participants are excluded (d ~0.95): the constraint's persistence depends on their exclusion from legitimate markets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (overdose crisis, HIV transmission, mass incarceration for possession) is contested — some jurisdictions show it is substantially solved, others show it persists. The constraint has not developed a sunset clause despite harm reduction advocates' original intent. Mandatrophy is unresolved: the treatment mandate apparatus continues to expand (rising theater_ratio, rising extractiveness) even where the founding problem has shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the substance_control_legitimacy kernel, or does it collapse into the prohibition_reading in practice?',
    'Trace enforcement outcomes: if treatment mandates functionally criminalize non-compliance, the reading converges with prohibition. Measure arrest rates for treatment non-adherence vs. possession.',
    'If convergence is structural, the harm_reduction_reading is not a stable separate constraint — its ε and extraction profile would be reattributed to the prohibition_reading. The kernel would have only two live readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether the harm_reduction_reading maintains structural distinctness from prohibition_reading under actual enforcement.').

omega_variable(
    black_market_extraction_boundary,
    'How much of the constraint''s measured extraction derives from the persistent black market (prohibition residue) vs. the harm reduction apparatus itself (treatment mandates, reporting requirements)?',
    'Decompose extraction: compare jurisdictions with identical harm reduction frameworks but different black market penetration (e.g., Portugal vs. Canada vs. Switzerland). Attribute extraction variance to enforcement of treatment mandates vs. black market violence.',
    'If black market extraction dominates, the constraint''s tangled_rope character is largely inherited from prohibition_reading; if treatment mandate extraction dominates, the harm reduction apparatus itself is extractive. Changes the structural diagnosis of who benefits and who pays.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_market_extraction_boundary, conceptual, 'Attribution of extraction between prohibition residue and harm reduction apparatus.').

omega_variable(
    treatment_mandate_coercion_mechanism,
    'Is the suppression from treatment mandates structural (legal sanctions, benefit conditionality) or internalized (therapeutic compliance as identity)?',
    'Post-exit trajectory study: track suppression levels for people who complete mandated treatment vs. those who exit the system. If suppression persists after legal mandate ends, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the medicalized subject carries the constraint forward voluntarily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_mandate_coercion_mechanism, empirical, 'Structural vs. internalized suppression in mandated treatment.').

omega_variable(
    cs_framing_alternative,
    'Does the harm_reduction_reading''s kernel framing center on ''state duty to minimize harm'' (public health authority) or ''bodily autonomy constrained by harm to others'' (liberal autonomy)?',
    'Analyze legislative preambles, judicial opinions, and policy documents for the stated authority ground. If authority derives from ''protecting vulnerable populations from themselves'', it is public_health_paternalism framing; if from ''preventing third-party harms'', it is liberal_autonomy framing.',
    'Public_health_paternalism framing influences toward prohibition_reading (expands state duty); liberal_autonomy framing influences toward legalization_reading (limits state duty). The reading''s structural neighbors shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Ambiguity in the kernel''s authority grounding for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t6, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(subs_tr_t18, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(subs_be_t6, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(subs_be_t18, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 18, 0.39).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(subs_su_t6, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 6, 0.49).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(subs_su_t18, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel decomposes into three readings with distinct ε values: prohibition_reading (high extractiveness, high suppression, claimed snare), harm_reduction_reading (moderate extractiveness, moderate suppression, claimed tangled_rope), legalization_reading (low extractiveness, low suppression, claimed rope). They share the referent (state authority over substance use) but instantiate different constraints with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, institutional, 0.15).
constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
