% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Model: Substance Use as Health Condition
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm reduction reading treats substance use as a health condition
 *   requiring pragmatic medical intervention (methadone, buprenorphine,
 *   naloxone, sterile equipment) to reduce overdose and disease transmission,
 *   independent of whether the user achieves abstinence. Users shift from
 *   criminal defendants to medical patients; enforcement moves from
 *   possession/use to supply-side trafficking. The state becomes a service
 *   provider rather than a punisher. This is ONE reading of a contested
 *   kernel: the substance control kernel is framed by three incompatible
 *   readings (prohibition, harm reduction, legalization), each grounding
 *   legitimacy in a different premise. The harm reduction reading coexists
 *   with legalization (both non-criminalize users but differ on supply
 *   legality and state paternalism) and coexists with prohibition (both
 *   remain active readings in different jurisdictions). This constraint story
 *   instantiates ONLY the harm reduction reading; the sibling readings are
 *   separate constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: Primary payer (medicalization replaces criminalization but adds paternalistic gatekeeping); secondary beneficiary (access to treatment replaces incarceration risk)
 *   - public_health_institutions: Agenda-setter (control medical framing, define treatment standards, measure success via harm reduction metrics)
 *   - harm_reduction_providers: Beneficiary (institutional funding and legitimacy tied to the medical model; gatekeeping power over treatment)
 *   - law_enforcement_agencies: Payer (lose authority over users, caseload reduction) and secondary agenda-setter (remain responsible for supply-side enforcement)
 *   - drug_supply_communities: Victim (supply remains criminalized while demand is medicalized; disproportionate enforcement burden)
 *   - addiction_medicine_physicians: Beneficiary (gain professional authority, funding, and gatekeeping control)
 *   - criminal_justice_reformers: Beneficiary (decriminalization aligns with broader reform goals but supply criminalization remains)
 *   - analytical_observer: Sees the bifurcated structure (medical demand, criminal supply)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.62).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.48).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Model: Substance Use as Health Condition").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '24be4e2a-8eec-4421-9036-f25adf55ff6b').
narrative_ontology:cs_kernel_codification('24be4e2a-8eec-4421-9036-f25adf55ff6b', distributed).
narrative_ontology:cs_authority_grounding('24be4e2a-8eec-4421-9036-f25adf55ff6b', extraction).
narrative_ontology:cs_interpretation_layer_present('24be4e2a-8eec-4421-9036-f25adf55ff6b').
narrative_ontology:cs_reading_relation('24be4e2a-8eec-4421-9036-f25adf55ff6b', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('24be4e2a-8eec-4421-9036-f25adf55ff6b', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('24be4e2a-8eec-4421-9036-f25adf55ff6b', foundational, substance_use_is_chronic_health_condition).
narrative_ontology:cs_axiom_status(substance_use_is_chronic_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('24be4e2a-8eec-4421-9036-f25adf55ff6b', substance_use_is_chronic_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('24be4e2a-8eec-4421-9036-f25adf55ff6b', foundational, harm_reduction_independent_of_abstinence).
narrative_ontology:cs_axiom_status(harm_reduction_independent_of_abstinence, holdable).
narrative_ontology:cs_axiom_grounding('24be4e2a-8eec-4421-9036-f25adf55ff6b', harm_reduction_independent_of_abstinence, deontological).
narrative_ontology:cs_reference_frame('24be4e2a-8eec-4421-9036-f25adf55ff6b', substance_use_as_treatable_medical_condition).
narrative_ontology:cs_drift_state('24be4e2a-8eec-4421-9036-f25adf55ff6b', contemporary_opioid_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('24be4e2a-8eec-4421-9036-f25adf55ff6b', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_institutions).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_providers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, drug_supply_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-substantial (0.62 endpoint) because the constraint imposes paternalistic medical gatekeeping and identity-locking on users even as it removes criminal penalties. The temporal series shows rising extractiveness in the early interval (0–15 years) as treatment infrastructure builds and gatekeeping is refined, then plateaus as the system matures. Suppression is moderate (0.48 endpoint) and declining over time — the shift from criminal to medical means coercive force (police, incarceration) declines, but internalized medical compliance ('you must take medication to be helped') replaces it. Theater is modest and stable (~0.31) because the harm reduction function is real (overdose prevention, disease control measurably occur), though some enforcement effort defends the supply-side criminalization asymmetry rather than health outcomes. Accessibility_collapse is substantial (0.67) because users face structural difficulty exiting the medicalized role: they are identity-locked ('person in recovery', 'chronic patient') by institutional definition, and the alternative path (return to criminal prosecution) was worse. Resistance is high (0.71) because users, supply communities, libertarian critics, and legalization advocates all mount real opposition to the constraint — it is not naturalized, and every beneficiary (health institutions, providers) must actively defend it.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_institutions seat experiences this as genuine coordination (solving a real public health problem, building medical infrastructure). The people_who_use_drugs seat experiences it as paternalistic extraction (medical authority replaces criminal authority, but gatekeeping persists). The drug_supply_communities seat experiences pure victimhood (criminalization intensified while demand-side moves to medical). The law_enforcement_agencies seat experiences it as loss of jurisdiction and resource, though they retain supply-side authority. These perspectives compute to different directionalities and different effective extraction — this divergence is exactly what the framework exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   people_who_use_drugs sit in a high-extraction seat (d near 1.0): they bear the compliance burden, identity-locking, gatekeeping costs. They are also described as beneficiaries (treatment access, reduced overdose risk, no criminal prosecution) — this is genuine dual-position asymmetry: they benefit from treatment and fear criminal prosecution, but they pay the medicalization price. The engine derives d from beneficiary/victim declarations plus exit_options (identity_locked is a high-d signal). public_health_institutions benefit from authority and funding (low d, near beneficiary end). harm_reduction_providers benefit from legitimacy and resource flow (low d). law_enforcement_agencies pay in authority loss but retain supply-side power (moderate d, near 0.5). drug_supply_communities are pure targets (d near 1.0: no benefit, only supply-side criminalization burden). The directionality override is not needed — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Harm reduction is not mandatrophic in the classical sense (founding problem still live, constraint still performs function). However, the constraint's bifurcation (medical demand, criminal supply) creates asymmetric function decay: the medical side solves overdose and disease transmission, but the criminal supply side persists structurally, making the constraint incomplete at addressing root causes. The founding problem (overdose, disease, mass incarceration, treatment barriers) is partly solved (users in treatment access help, incarceration drops) but incompletely (supply-side remains criminalized, transit costs rise, low-level dealers face disproportionate enforcement, geographic inequality between harm-reduction and prohibition jurisdictions grows). The constraint's function is live but geographically and structurally bounded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_demand_asymmetry_sustainability,
    'Can a bifurcated system (medicalized demand, criminalized supply) persist indefinitely, or does supply-side criminalization pressure demand-side medicalization toward prohibition regression?',
    'Longitudinal policy tracking: do harm reduction jurisdictions shift back toward prohibition when supply-side enforcement costs rise, or do they intensify supply-side measures to defend the medical demand boundary? Jurisdictional comparison: which diverges first — demand-side medicalization or supply-side enforcement stringency?',
    'If supply criminalization destabilizes the system, the constraint regresses toward prohibition (becomes snare rather than tangled rope). If supply measures intensify without demand regression, extraction on supply communities rises and the constraint approaches snare classification. If both remain stable, the bifurcation persists as structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_demand_asymmetry_sustainability, empirical, 'Whether the medical/criminal bifurcation is a stable equilibrium or structurally unstable.').

omega_variable(
    paternalism_internalization_trajectory,
    'Is the measured suppression (0.48, declining) genuinely declining, or is coercive paternalism being internalized (users accept medical gatekeeping as legitimate)?',
    'Post-treatment suppression trajectory: follow cohorts of users after exiting medical treatment — do they accept medication/monitoring as legitimately necessary, or do they experience it as imposed? Qualitative research with people exiting the system.',
    'If internalized, effective suppression is higher than measured (the constraint carries suppression with users even after institutional exit). If rejected, rising resistance should push back on medical gatekeeping, forcing increased suppression_requirement to maintain it. This affects whether the constraint is sustainable or faces growing resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_internalization_trajectory, empirical, 'Whether suppression is structurally declining or being internalied as perceived legitimacy.').

omega_variable(
    medical_model_naturalness,
    'Is the medical model of addiction a genuine natural fact (brain chemistry, neural plasticity, chronic disease process) or a constructed framing that benefits medical institutions?',
    'Neuroscience consensus and the existence of competing biological models (social determination, choice architecture, reinforcement learning without pathology). Does the ''disease'' model survive empirical challenge, or is it defended for institutional reasons?',
    'If genuine natural model, the constraint is coordinating around a real fact. If constructed and beneficiary-defended, the constraint is a false summit — medical framing riding on real public health problems (overdose, disease) but claiming authority via medicalization that is not warranted. This feeds FSM evaluation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medical_model_naturalness, conceptual, 'Whether the medical model of addiction is natural fact or constructed institutional framing.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity-locking in harm reduction (people defined as ''in recovery'', ''chronic patients'') structurally necessary for maintaining compliance, or is it a therapeutic boundary that could be loosened?',
    'Jurisdictions experimenting with ''recovery-oriented systems of care'' that deemphasize illness identity and foregrounding of abstinence and self-efficacy: do outcomes differ (relapse rates, treatment engagement, post-treatment functioning) from identity-locked models?',
    'If loosening identity-lock improves outcomes, the constraint''s extraction on users can be reduced without sacrificing function. If identity-lock is necessary for compliance, removing it raises extraction on people_who_use_drugs but is aligned with autonomy values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-locking is functionally necessary or therapeutically optional.').

omega_variable(
    sibling_reading_empirical_test,
    'Which reading (prohibition, harm reduction, legalization) produces the lowest overdose mortality and disease transmission in real-world implementation?',
    'Cross-jurisdictional comparative outcomes data: Portugal (decriminalization + treatment), Netherlands (harm reduction + quasi-legal supply tolerance), Switzerland (heroin-assisted treatment), Sweden (strict prohibition + abstinence focus), Canada (decriminalization experiment), Uruguay/Canada (legalization experiment).',
    'Empirical superiority of one reading''s outcomes does not prove legitimacy (the constraint could be effective at harm reduction but still extractive), but it would falsify claims that prohibition is necessary to prevent overdose or that legalization alone solves problems without medical intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_test, empirical, 'Real-world comparative effectiveness of the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subs_tr_t3, substance_control_kernel__harm_reduction_reading, theater_ratio, 3, 0.27).
narrative_ontology:measurement(subs_tr_t6, substance_control_kernel__harm_reduction_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__harm_reduction_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(subs_tr_t25, substance_control_kernel__harm_reduction_reading, theater_ratio, 25, 0.31).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subs_be_t3, substance_control_kernel__harm_reduction_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(subs_be_t6, substance_control_kernel__harm_reduction_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__harm_reduction_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(subs_be_t25, substance_control_kernel__harm_reduction_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(subs_su_t3, substance_control_kernel__harm_reduction_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(subs_su_t6, substance_control_kernel__harm_reduction_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__harm_reduction_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(subs_su_t25, substance_control_kernel__harm_reduction_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, public_health_institutional_authority).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, supply_chain_criminalization).

% DUAL FORMULATION NOTE:
% The harm_reduction_reading is one of three structurally incompatible readings of the substance_control_kernel. The sibling readings (prohibition_reading, legalization_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and enforcement mechanisms. The kernel contest itself is the subject of the 'substance_control_kernel__kernel_comparison' meta-constraint story, which analyzes how the three readings compete for institutional adoption. Each reading story is complete in itself; the network links document structural influence and shared root problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
