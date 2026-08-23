% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Preparedness Regime (Hybrid Reading)
 *   domain: institutional/safety-regulation
 *
 * SUMMARY:
 *   A large-scale institutional preparedness regime — mandatory life-safety
 *   inspection plus recurring evacuation exercises — operates across
 *   commercial and residential buildings. Under this reading the regime is
 *   internally stratified: the engineering-inspection stratum still performs
 *   its function (licensed professionals verify structural, alarm, and egress
 *   systems against physical standards, and findings correlate with real
 *   failure prevention), while the exercise stratum has largely detached from
 *   its function (sessions are scheduled, announced, and scored as attendance
 *   events; their scenarios rarely match real emergencies, and measured
 *   performance is scheduling compliance rather than response capability).
 *   The regime as a whole therefore carries a genuine coordination core
 *   wrapped in a growing performative shell, sustained by statute, insurance
 *   pricing, and liability doctrine. Extraction is localized: it concentrates
 *   where compliance is performed without capability being built. KEY AGENTS
 *   (by structural relationship): - institutional_administrators:
 *   agenda-setting beneficiary (organized/constrained) — schedules and
 *   attests compliance, collects liability defensibility -
 *   regulatory_agencies: agenda-setter (institutional/constrained) — writes
 *   mandates, audits, gains a legible surface, bears enforcement cost -
 *   liability_insurers: beneficiary (institutional/arbitrage) — prices off
 *   certificates, runs nothing - fire_protection_engineers: beneficiary
 *   (moderate/mobile) — exercises live judgment in the competent stratum -
 *   building_occupants: payer with incidental benefit (powerless/constrained)
 *   — real protection from inspection, time cost and residual risk from
 *   hollow exercises - hourly_facility_staff: payer (powerless/trapped) —
 *   performs the exercises, gains least, refuses at career cost -
 *   mobility_impaired_occupants: excluded (powerless/trapped) — their
 *   scenarios are the ones the exercises never test -
 *   disaster_research_community: observer (analytical/analytical) — sees the
 *   stratified structure whole
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.48).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Preparedness Regime (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/safety-regulation").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '61f23c2a-1c65-400d-bdcb-708c9256b64d').
narrative_ontology:cs_kernel_codification('61f23c2a-1c65-400d-bdcb-708c9256b64d', formalized).
narrative_ontology:cs_authority_grounding('61f23c2a-1c65-400d-bdcb-708c9256b64d', expertise).
narrative_ontology:cs_interpretation_layer_present('61f23c2a-1c65-400d-bdcb-708c9256b64d').
narrative_ontology:cs_reading_relation('61f23c2a-1c65-400d-bdcb-708c9256b64d', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('61f23c2a-1c65-400d-bdcb-708c9256b64d', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_axiom('61f23c2a-1c65-400d-bdcb-708c9256b64d', foundational, preparedness_is_heterogeneous).
narrative_ontology:cs_axiom_status(preparedness_is_heterogeneous, holdable).
narrative_ontology:cs_axiom_grounding('61f23c2a-1c65-400d-bdcb-708c9256b64d', preparedness_is_heterogeneous, empirically_contingent).
narrative_ontology:cs_axiom('61f23c2a-1c65-400d-bdcb-708c9256b64d', secondary, component_level_audit_required).
narrative_ontology:cs_axiom_status(component_level_audit_required, holdable).
narrative_ontology:cs_axiom_grounding('61f23c2a-1c65-400d-bdcb-708c9256b64d', component_level_audit_required, instrumental).
narrative_ontology:cs_reference_frame('61f23c2a-1c65-400d-bdcb-708c9256b64d', competent_core_performative_shell).
narrative_ontology:cs_drift_state('61f23c2a-1c65-400d-bdcb-708c9256b64d', contemporary_compliance_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('61f23c2a-1c65-400d-bdcb-708c9256b64d', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, liability_insurers).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, fire_protection_engineers).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, building_occupants).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, hourly_facility_staff).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, building_occupants).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, compliance_documentation_doctrine).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, routine_drill_efficacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schedule and announce evacuation exercises, select their scenarios, and sign the compliance attestations that insurers and regulators accept. Exercise time falls mostly on staff below them, while the resulting documented record defends them in litigation and passes audits. Declining to run the exercises would expose them to citation and uninsured liability, so the calendar continues regardless of what the exercises demonstrate.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, institutional_administrators, agenda_setter,
    organized, biographical, constrained, national).

% Write and amend the mandates, audit facilities against them, and accept or reject the documentation produced. The compliance record gives them a legible surface they can inspect at scale; enforcing it consumes budget and political capital, and they collect no fee stream from it. Their leverage is periodic: it peaks after publicized disasters and erodes between them.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Price premiums and set coverage terms keyed to inspection certificates and exercise logs. They run neither the inspections nor the exercises; the certificate system lets them underwrite thousands of buildings they never visit. When loss data disappoints they can reprice or withdraw from a market entirely.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, liability_insurers, beneficiary,
    institutional, biographical, arbitrage, continental).

% Licensed professionals who physically examine structures, alarms, sprinklers, and egress paths and sign findings that carry legal weight. Their judgment is genuinely exercised and their findings feed real remediation; their livelihood and professional standing depend on the inspection side of the regime remaining substantive.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, fire_protection_engineers, beneficiary,
    moderate, biographical, mobile, national).

% Live and work in regulated buildings. They receive real protection from inspected structural and suppression systems, and they spend scheduled hours in announced exercises that route them down familiar stairs to familiar muster points. They cannot individually verify the systems they rely on and cannot readily relocate away from buildings whose exercise programs consume their time.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, building_occupants, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, building_occupants, beneficiary).

% Run the exercises and complete the checklists on instruction, often during peak workload or on their own time, under discipline for non-participation. They gain the least from the exercise program and have the least say in its design; refusing means finding another job.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, hourly_facility_staff, payer,
    powerless, immediate, trapped, local).

% Use wheelchairs or have limited mobility in buildings whose evacuation planning centers on stairwell descent. Announced exercises routinely exclude them or relegate them to designated refuge areas that are themselves rarely tested. They would redesign the exercises around their scenarios but are not in the rooms where scenarios are chosen.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, mobility_impaired_occupants, excluded,
    powerless, biographical, trapped, local).

% Study post-incident behavior, exercise-transfer validity, and inspection effectiveness across jurisdictions. They publish on which preparedness activities predict survival and which predict paperwork, and they see the whole stratified picture that any single participant seat sees only locally.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, disaster_research_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared, verifiable record that buildings and response plans meet minimum safety standards: licensed inspection generates trustworthy structural and systems information that individual occupants cannot produce themselves, and scheduled exercises give dispersed occupants common knowledge of alarms, routes, and muster points.
% TRANSFER_FUNCTION: Moves staff time and attention into compliance performance; moves inspection fees from facility operators to licensed firms; moves premium discounts and litigation defensibility to operators and insurers; leaves the residual risk of hollow response capability sitting with occupants.
% ABSENT_VOICES: Mobility-impaired occupants, night-shift staff who rarely encounter daytime announced exercises, and residents of buildings where drills are pre-announced would all object that the tested scenario is not the real one. They are outside the rooms where exercise scenarios are designed, and the compliance record is silent about their exclusion.
% DISAPPEARANCE_RATIONALE: Insurance pricing, liability allocation, and occupancy certification all key off the inspection-and-exercise record. Overnight removal would force reorganization of who verifies safety and how; unpriced risk would migrate back onto occupants until substitute verification arrangements emerged, and the litigation posture of every facility operator would reset.
% FOUNDING_PROBLEM: Serial building disasters killed occupants because no one had verified egress, alarms, and structural systems, and because untrained occupants froze or stampeded. The regime was built to make safety externally verifiable and response rehearsed.
% FOUNDING_PROBLEM_CORROBORATION: Fire-protection regulators' post-incident investigation series and national institute forensic reports corroborate the inspection-side problem as still live, from outside the benefiting parties. Behavioral studies of real evacuations and firefighter after-action reports attest that announced exercises measure scheduling compliance rather than response capability. No benefiting party attests that the exercise-side founding problem remains solved in its original form.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is tangled_rope because the regime jointly satisfies the three structural marks: a genuine coordination function (externally verifiable safety information no occupant can produce alone), asymmetric extraction (compliance labor and residual risk flow to those who neither design nor profit from the exercises), and active enforcement (statute, audit, and insurability). The metrics describe the stratification: extractiveness 0.48 reflects extraction concentrated in the exercise stratum while the inspection stratum stays near coordination cost; theater_ratio 0.52 places the performative share just past half, matching the reading's partition; suppression 0.55 is moderate — enforcement is real but soft (citation and insurability, not force), and suppression is authored as a raw structural property, unscaled by power or scope. Accessibility_collapse 0.45: alternatives (validated scenario training, unannounced exercises, third-party outcome audits) remain imaginable and partly piloted, but the compliance templates crowd them out. Resistance 0.45: checkbox-minimalism, gamed attendance, and episodic post-disaster reform pressure. The temporal series share one grid (five-year points over a 1980-2025 window). Theater and extractiveness ratchet upward across the window; suppression_requirement sawtooths — spiking after the major fire disasters near the window's start, mid-window, and late-window, decaying between — and the cycle is itself partly the mechanism: each reform wave widens the documented-compliance surface, whose substance then decays, lifting the theater floor while enforcement intensity resets. End-state scalars were read at t=45, on the decay limb following the latest ratchet.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator seat the regime is a functioning obligation met on schedule; from the engineer seat it is a working verification craft; from the insurer seat it is an actuarial input; from the staff and occupant seats it is time taken and risk retained. Same nominal regime, four different experienced objects — the engine computes this divergence from power, exit, and role asymmetries rather than from the authored claim. Coalition potential among the powerless seats (tenant associations, union grievances over uncompensated exercise time) is real but historically episodic: it spikes after disasters and dissipates between them, which is why the powerless seats persist at high directional load rather than organizing it away.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrators derive near-beneficiary directionality: they collect the attestation's defensive value and externalize exercise time downward. Insurers sit nearest the beneficiary pole: certificate-fed underwriting with arbitrage-grade exit. Engineers are genuine beneficiaries of the competent stratum with mobile exit. Agencies are listed beneficiaries (the legible compliance surface) but their net position is only mildly favorable once enforcement cost is counted, so their computed directionality sits well above the other beneficiaries'. Occupants mix real inspection benefit against exercise time cost and unhedged residual risk, landing them mid-high; hourly staff, who perform without designing and cannot refuse, sit nearest the target pole. Mobility-impaired occupants are excluded rather than coordinated — the exercise program's blind spot is precisely their scenario. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the regime as a single rope would hide the localized extraction; reading it as a piton would erase the live inspection function. The stratified reading keeps both facts load-bearing: the founding problem of the inspection stratum (unverifiable structural safety) is live, while the founding problem of the exercise stratum (untrained panic) has been transformed — announced compliance sessions no longer rehearse the surprise conditions they were built for. Mandatrophy is therefore resolved per-stratum, not globally: the obsolescence flag attaches to the exercise subsystem, and the structurally indicated remedy is component-level replacement of ritual exercises with validated, unannounced scenario training — not abolition of the regime, whose inspection core still earns its keep.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_stratification_boundary,
    'Where exactly does the competent/ritual boundary fall, and is this hybrid partition the right framing of the preparedness kernel, or do the universal siblings (all-competent, all-husk) better fit the evidence?',
    'Component-level transfer audits: compare post-incident behavior and system-failure rates for inspected versus uninspected systems, and for drilled versus undrilled populations; adopt whichever framing the component-level evidence supports.',
    'If inspection is also captured, the constraint collapses toward the husk reading (inertial performance throughout); if exercises demonstrably build transferable readiness, it collapses toward the competence reading (pure coordination). Either shift changes epsilon materially and reclassifies the affected strata.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_stratification_boundary, conceptual, 'Boundary location and framing choice for the preparedness_persistence kernel; this story is the hybrid reading of that kernel.').

omega_variable(
    drill_transfer_validity,
    'What fraction of scheduled exercise activity transfers to behavior under real, unannounced emergencies?',
    'Post-incident forensics comparing casualty curves and egress times across buildings with differing exercise regimes; validated scenario assessments replacing attendance counts as the performance measure.',
    'Low transfer confirms the ritual stratum and sustains the high theater_ratio; high transfer lowers epsilon toward coordination cost and pulls the whole regime toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_transfer_validity, empirical, 'Transfer validity of scheduled exercises to real emergency behavior.').

omega_variable(
    inspection_capture_drift,
    'Is the inspection stratum eroding toward self-certification and rubber-stamp third-party review?',
    'Re-inspection studies correlating inspector identity, fee structure, and violation discovery rates; longitudinal comparison of findings for first-time versus repeat clients.',
    'Capture shrinks the competent stratum, raising both epsilon and theater_ratio and dating the husk transition earlier than this reading projects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_capture_drift, empirical, 'Whether the competent stratum is itself drifting toward performance.').

omega_variable(
    false_confidence_externality,
    'Does ritualized exercising actively increase harm — complacency, over-reliance on announced-warning assumptions — beyond the time it consumes?',
    'Behavioral comparison of occupants from high-exercise versus low-exercise buildings in actual incidents, controlling for building type and occupancy.',
    'A positive externality raises effective harm above the time-cost measure and strengthens the case for replacing rather than continuing the exercises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_confidence_externality, empirical, 'Possible negative externality of hollow exercise programs on real-world response.').

omega_variable(
    reform_ratchet_asymmetry,
    'Do post-disaster reform waves permanently expand the compliance surface faster than their substantive content decays — that is, is the theater floor monotonically rising?',
    'Cross-jurisdiction panel data on documentation requirements added per reform wave versus requirements later retired.',
    'A ratcheting floor predicts continued theater_ratio growth absent structural reform and dates a future piton transition for the exercise stratum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_ratchet_asymmetry, conceptual, 'Whether the disaster-reform cycle ratchets performance upward irreversibly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_hybrid_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(prep_hybrid_tr_t5, preparedness_persistence__hybrid_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(prep_hybrid_tr_t10, preparedness_persistence__hybrid_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(prep_hybrid_tr_t15, preparedness_persistence__hybrid_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(prep_hybrid_tr_t20, preparedness_persistence__hybrid_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(prep_hybrid_tr_t25, preparedness_persistence__hybrid_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(prep_hybrid_tr_t30, preparedness_persistence__hybrid_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement(prep_hybrid_tr_t35, preparedness_persistence__hybrid_reading, theater_ratio, 35, 0.49).
narrative_ontology:measurement(prep_hybrid_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.51).
narrative_ontology:measurement(prep_hybrid_tr_t45, preparedness_persistence__hybrid_reading, theater_ratio, 45, 0.52).

% Extraction over time
narrative_ontology:measurement(prep_hybrid_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(prep_hybrid_be_t5, preparedness_persistence__hybrid_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(prep_hybrid_be_t10, preparedness_persistence__hybrid_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(prep_hybrid_be_t15, preparedness_persistence__hybrid_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(prep_hybrid_be_t20, preparedness_persistence__hybrid_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(prep_hybrid_be_t25, preparedness_persistence__hybrid_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement(prep_hybrid_be_t30, preparedness_persistence__hybrid_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(prep_hybrid_be_t35, preparedness_persistence__hybrid_reading, base_extractiveness, 35, 0.49).
narrative_ontology:measurement(prep_hybrid_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(prep_hybrid_be_t45, preparedness_persistence__hybrid_reading, base_extractiveness, 45, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(prep_hybrid_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(prep_hybrid_su_t5, preparedness_persistence__hybrid_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(prep_hybrid_su_t10, preparedness_persistence__hybrid_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(prep_hybrid_su_t15, preparedness_persistence__hybrid_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(prep_hybrid_su_t20, preparedness_persistence__hybrid_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(prep_hybrid_su_t25, preparedness_persistence__hybrid_reading, suppression_requirement, 25, 0.63).
narrative_ontology:measurement(prep_hybrid_su_t30, preparedness_persistence__hybrid_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement(prep_hybrid_su_t35, preparedness_persistence__hybrid_reading, suppression_requirement, 35, 0.55).
narrative_ontology:measurement(prep_hybrid_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(prep_hybrid_su_t45, preparedness_persistence__hybrid_reading, suppression_requirement, 45, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'institutional preparedness' decomposes into three readings of the kernel preparedness_persistence, each with its own epsilon and beneficiary structure. competence_reading (upstream, highest empirical confidence in the inspection literature) is cited by administrators as evidence the regime works; husk_reading is cited by critics as evidence it has hollowed; this hybrid_reading holds the middle with component-level evidence and inherits citations from both sides. The stories are linked pairwise via affects_constraints; each carries its own stable epsilon over the same standing arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
