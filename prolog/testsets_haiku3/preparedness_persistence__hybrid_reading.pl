% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness: Competent Engineering / Ritualized Evacuation
 *   domain: institutional/disaster-preparedness/commitment-systems
 *
 * SUMMARY:
 *   This constraint models a hybrid-reading of preparedness persistence where
 *   disaster-preparedness infrastructure is stratified into two divergent
 *   tracks: engineering inspection (competence-maintaining, expensive,
 *   continuous) and evacuation drills (ritualized, annual, theater-heavy).
 *   The hybrid reading asserts that this stratification is neither pure
 *   natural law (the competence_reading) nor pure atrophy (the husk_reading),
 *   but a sustainable administrative compromise where some subsystems remain
 *   functionally competent while others have slid into performative
 *   maintenance. The constraint's persistence depends on institutional
 *   enforcement that keeps the two tracks separate — administrators benefit
 *   from this separation because it allows them to maintain compliance status
 *   while avoiding the full cost of continuous operational readiness.
 *   Responders and occupants bear the cost of the divergence: they know the
 *   dual-track system leaves gaps in actual evacuation capability, yet audit
 *   processes treat the separation as legitimate.
 *
 * KEY AGENTS:
 *   - Institutional administrators: set the dual-track policy, benefit from compliance status without full-cost readiness
 *   - Engineering inspection teams: maintain live technical knowledge, remain genuinely competent
 *   - Frontline responders: conduct drills, maintain readiness, observe failure modes the system does not act on
 *   - Evacuation-dependent populations: evacuate under annual protocols that do not track their actual capability
 *   - Compliance auditors: certify the two tracks separately, benefit from codified pass/fail criteria
 *   - Building occupants at risk: excluded from preparedness planning, actual evacuation capability degrading between drills
 *   - Disaster researchers: observe that annual drills do not predict real-evacuation behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.58).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.45).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness: Competent Engineering / Ritualized Evacuation").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/disaster-preparedness/commitment-systems").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '15d31b65-0c4a-4381-bb54-004d6f9d4bbc').
narrative_ontology:cs_kernel_codification('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', formalized).
narrative_ontology:cs_authority_grounding('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', lineage).
narrative_ontology:cs_interpretation_layer_present('15d31b65-0c4a-4381-bb54-004d6f9d4bbc').
narrative_ontology:cs_reading_relation('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', foundational, preparedness_stratification_is_empirical).
narrative_ontology:cs_axiom_status(preparedness_stratification_is_empirical, holdable).
narrative_ontology:cs_axiom_grounding('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', preparedness_stratification_is_empirical, empirically_contingent).
narrative_ontology:cs_axiom('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', secondary, behavioral_readiness_degradation_is_detectable).
narrative_ontology:cs_axiom_status(behavioral_readiness_degradation_is_detectable, holdable).
narrative_ontology:cs_axiom_grounding('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', behavioral_readiness_degradation_is_detectable, empirically_contingent).
narrative_ontology:cs_reference_frame('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', dual_track_live_preparedness).
narrative_ontology:cs_drift_state('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', contemporary_institutional_audit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15d31b65-0c4a-4381-bb54-004d6f9d4bbc', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, compliance_auditors).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_emergency_responders).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, evacuation_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, engineering_inspection_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and enforce the dual-track preparedness mandate: technical inspection standards (engineering-grounded, expensive, continuous) and evacuation drills (annually scheduled, performative, cheaper). They benefit from the constraint by achieving compliance status without the full cost of maintaining operational evacuation capability year-round. They set the audit criteria that distinguish 'sufficient inspection' from 'redundant drill'.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, institutional_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Conduct structural and safety inspections on buildings and systems. They benefit from consistent funding and authority to certify safety; their expertise remains current through active technical work. They experience the constraint as enabling their function, not constraining it — they are the competent track.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspection_teams, beneficiary,
    powerful, biographical, mobile, national).

% Conduct and enforce evacuation drills, maintain readiness, and respond to actual emergencies. They bear the cost of the stratification: they know from annual drills what people will actually do (panic, confusion, poor route knowledge), yet the constraint treats the drill as sufficient certification of preparedness. Their local knowledge of failure modes is not fed back to improve the standing plan.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_emergency_responders, payer,
    moderate, biographical, constrained, national).

% Live or work in buildings and neighborhoods subject to evacuation protocols. They participate in annual drills that are often treated as theater (compliance exercise, not operational rehearsal). Their actual evacuation knowledge degrades between drills; they have no input into whether drills match real hazard scenarios or reflect actual building use patterns.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, evacuation_dependent_populations, payer,
    powerless, biographical, trapped, local).

% Verify that institutions conduct required inspections and drills and meet documentation standards. They benefit from the constraint's codification: clear, auditable categories (inspection yes/no, drill conducted yes/no) that are easy to verify without needing to assess operational readiness directly. They set the pass/fail criteria that keep the two tracks separate.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, compliance_auditors, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, compliance_auditors, agenda_setter).

% Are the actual human substrate affected by evacuation success or failure but are not represented in preparedness planning or audit processes. They do not attend planning meetings, do not voice concerns about drill design, and do not validate whether their own mobility, language, or disability needs are accommodated in the evacuation plan.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, building_occupants_at_risk, excluded,
    powerless, immediate, trapped, local).

% Study how people actually behave under evacuation stress (through simulation, historical disaster analysis). They observe that annual drills do not reliably predict real evacuation outcomes; actual crises show systematic failures in route knowledge, decision-making, and coordination that match-day drill participation does not prevent. Their research documents the gap between the two preparedness tracks.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, disaster_simulation_researchers, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates dual preparedness mechanisms: (1) continuous technical inspection to prevent structural hazards, and (2) periodic behavioral rehearsal (drills) to maintain population evacuation knowledge. The separation solves a genuine coordination problem: continuous operational evacuation readiness is expensive; technical expertise is scarce; different knowledge-maintenance mechanisms suit each domain. The dual track allows technical specialists to maintain safety infrastructure while distributing behavioral rehearsal responsibility to building managers and local responders.
% TRANSFER_FUNCTION: Moves compliance-status authority and institutional legitimacy from continuous operational readiness demonstration to categorical certification (engineering inspected, drill conducted). Administrators and auditors collect compliance status and authority to certify 'preparedness maintained'; responders and occupants transfer the capability to demonstrate real evacuation readiness on demand — the behavioral knowledge-maintenance burden is outsourced to annual theater rather than continuous practice.
% ABSENT_VOICES: Occupants at immediate evacuation risk, people with disabilities whose evacuation needs are not drill-reflected, vulnerable populations (children, elderly, medically dependent) whose specific needs require modified plans. Disaster-simulation researchers and responders themselves are only partially voiced (their expertise is heard in technical inspection; their observations about drill-to-real-evacuation divergence is not integrated into policy). These actors would testify that annual drills do not maintain their evacuation capability and that stratified preparedness leaves behavioral risks unaddressed.
% DISAPPEARANCE_RATIONALE: If this stratified constraint dissolved and preparedness re-integrated, institutions would face a choice: either invest in continuous behavioral-readiness verification (expensive, labor-intensive, politically unpopular), or abandon the pretense that behavioral preparedness is maintained (accept documented gaps in occupant knowledge, lower disaster-readiness expectations). The current arrangement persists because it allows administrators to claim preparedness is 'maintained' while avoiding the cost of continuous behavioral verification.
% FOUNDING_PROBLEM: Large-scale disasters reveal two classes of failure: (1) structural hazards (buildings collapse, exits are blocked, design flaws trap people), and (2) behavioral failures (occupants panic, lack route knowledge, miscommunicate). These require different expertise and different intervention — structural engineers address design; behavioral specialists address knowledge and decision-making. Early 20th-century disaster response recognized the need for both.
% FOUNDING_PROBLEM_CORROBORATION: Structural engineers and building safety inspectors attest that continuous technical inspection prevents the structural-hazard class of failures effectively. Disaster-simulation researchers, historical disaster analysis, and responder testimony attest that the behavioral-failure class persists and that annual drills do NOT reliably prevent it — real evacuations still show decision paralysis, route confusion, and vulnerability patterns that match-day drill participation does not predict. The founding problem (both failure classes are real) is live; the institutional split response to it is contested.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The constraint is claimed as piton because extractiveness is moderate (0.58 at endpoint) and theater is high (0.62 average), yet the arrangement persists without concentrated beneficiary actively maintaining it — administrators benefit because it solves their compliance problem, not because they continuously invest to keep drills operational. Engineering inspection remains genuinely competent (it is the Mountain-like component), while evacuation drills are increasingly performative (the Piton component). The stratification itself is the administrative genius: keep technical standards strict enough that structural failures are rare, but allow behavioral-readiness standards to slide into annual theater. Suppression is moderate (0.45 average) because the enforcement burden is light — the two tracks enforce themselves (engineers care about their competence; drills happen because regulations require them) without heavy top-down coercion. Resistance is high (0.71) because responders and occupants continuously observe the gap between the performance (drill) and the outcome (actual evacuation capability). The measurement series shows gradual extraction creep (extractiveness rising from 0.48 to 0.61 then dropping to 0.58 at t=40, suggesting a mid-interval spike in enforcement/administrative attention, then slight relaxation). Theater rises from 0.55 to 0.65 (more performative over time) then slightly recovers to 0.62 — suggesting awareness of excessive ritualization. Suppression requirement stays low and flat, indicating the separation between the tracks is self-maintaining: no party is forced hard; they simply operate in separate institutional silos.
 *
 * PERSPECTIVAL GAP:
 *   The institutional administrator seat perceives this as a reasonable compromise: technical risks are mitigated by continuous engineering inspection; behavioral risks are addressed by annual population rehearsal; together they provide coverage without requiring continuous expensive operational evacuation readiness. The responder seat perceives the same structure as abdication: the annual drill does not update their operational readiness, does not address the specific occupant base they serve, and does not reflect the failure modes they observe in real emergencies. The occupant seat perceives it as abstract: drills happen, compliance exists, but their own evacuation knowledge degrades annually and they have no say in whether the plan reflects their actual capabilities. From the auditor seat, the constraint is successful: two clear audit lines, simple pass/fail criteria, low verification cost. From the researcher seat, the constraint is partially falsifying: it produces documented divergence between drill outcomes and real-evacuation behavior. The engine computes these divergences from the power/exit/beneficiary/victim structure; the hybrid reading asserts that no single perspective captures the whole — the constraint is genuinely mixed (competence + ritual) because it services multiple incompatible institutional logics simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional administrators and compliance auditors are structural beneficiaries (d near 0.2–0.3): they collect compliance status and organizational stability without bearing the cost of continuous evacuation readiness. Engineering teams are near-beneficiaries (d near 0.3–0.4): they benefit from continuous work and authority to certify safety, but their competence is genuine — the constraint does not extract from them, it enables their function. Responders are mid-target (d near 0.6): they bear the cost of maintaining dual protocols and conducting theater drills, but they also benefit from the engineering competence (they are more confident that buildings are structurally sound) and from the institutional mandate to exist (their jobs depend on the preparedness apparatus). Evacuation-dependent populations are full targets (d near 0.85–0.95): they are trapped in the geography, excluded from planning, and their actual evacuation capability is treated as a second-order problem (the constraint assumes occupants will be ready because a drill happened annually; it does not assume administrators will maintain responder or occupant competence continuously). The directionality spread is broad, reflecting the hybrid nature: this is not a constraint that extracts uniformly, but one that creates asymmetric benefits and costs that administrative separation makes invisible.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids simple mandatrophy because the founding problem (two independent failure modes) is genuinely live and the mandate to maintain both is live. However, the hybrid reading identifies incipient mandatrophy in the behavioral-readiness track: the founding problem (people do not know how to evacuate) persists, but the institutional commitment to solving it (continuous behavioral readiness) has atrophied and been replaced by annual theater. The engineering track remains vital — inspections are continuous, their results drive real changes in buildings. The behavioral track has become a memorial exercise that performs the appearance of maintenance without updating occupant knowledge. The constraint's persistence is propped by the asymmetry: because the engineering track works, administrators can claim 'preparedness is maintained' and justify scaling back the behavioral track. The husk_reading (preparedness is all theater) and the competence_reading (preparedness is fully live) are both structurally false for this constraint; the hybrid reading captures the true asymmetry: some parts live, some parts ossified, same institutional shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_readiness_measurement_gap,
    'Are annual evacuation drills a reliable measure of actual occupant evacuation capability, or does occupant knowledge and muscle memory degrade substantially between drills in ways the drill performance does not capture?',
    'Longitudinal study of occupant evacuation knowledge at multiple time points between drills (t=0 months post-drill, t=3, t=6, t=9, t=12 months pre-drill); comparison of drill performance vs. real-emergency outcomes (historical disaster data); simulation studies testing occupant behavior under actual hazard vs. drill conditions.',
    'If behavioral readiness degrades substantially between drills and annual performance does not predict real-evacuation outcomes, the behavioral-readiness component is indefensible as a preparedness mechanism — it is pure theater masking a real gap. If drills maintain capability reliably, the hybrid reading should be revised to competence_reading. The gap directly determines whether the behavioral track is piton or rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_readiness_measurement_gap, empirical, 'Whether drills maintain real evacuation readiness or have become decoupled from actual capability.').

omega_variable(
    stratification_as_administrative_necessity_vs_choice,
    'Is the separation between technical and behavioral preparedness tracks structurally necessary (two genuinely independent modes of failure requiring different expertise), or is it an administrative choice that obscures and enables the slide of the behavioral track into theater?',
    'Comparative analysis of disaster-response systems that do NOT separate the tracks (jurisdictions that mandate continuous behavioral-readiness verification, not annual drills); examination of whether technical inspection and behavioral rehearsal could be integrated into a single continuous cycle; case studies of institutions that have re-integrated the tracks after prior separation.',
    'If the separation is necessary, the hybrid reading is justified as an honest reflection of different intervention modes. If it is administrative choice, the separation serves to hide the behavioral-track degradation and justify reduced investment — the constraint would be better classified as a snare (cover story for extraction of institutional resources from preparedness). This determination affects whether mandatrophy has begun (the behavioral mandate is being abandoned while administrators claim preparedness is still live).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_as_administrative_necessity_vs_choice, conceptual, 'Whether the two-track structure is inherent to preparedness or an administrative convenience that obscures extraction.').

omega_variable(
    extraction_to_administrators_vs_shared_benefit,
    'Do administrators and auditors benefit from the constraint''s persistence in extractive terms (they would lose status/authority/resources if preparedness were integrated and continuously verified), or do they benefit from a coordination function that distributes real preparedness work efficiently?',
    'Budget tracking of preparedness resources allocated to engineering vs. behavioral components over time; administrative testimony about why the two-track model persists and what would change if integrated; analysis of whether administrators actively defend the separation against reform proposals.',
    'If administrators benefit extractively (they collect compliance status and authority while reducing their cost of maintaining preparedness), the constraint is snare-adjacent and mandatrophy is active — they benefit from the behavioral track remaining theater. If the two-track model is genuinely cost-efficient and reflects real preparation choices, the extraction is moderate and justifiable. The answer determines whether this is piton (inert performance) or tangled_rope (real coordination, some extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_to_administrators_vs_shared_benefit, empirical, 'Whether the constraint''s persistence serves administrative extraction or genuine preparedness coordination.').

omega_variable(
    reading_equivalence_hypothesis,
    'Are the competence_reading, husk_reading, and hybrid_reading fundamentally different claims about the same constraint, or are they reading-dependent artifacts produced by different measurement frames (what counts as ''competent'', what counts as ''real preparedness'')?',
    'Clarify what observable fact would satisfy each reading. Competence: both tracks remain operationally vital (real drills, real inspections). Husk: both tracks are primarily performative (theater drills, perfunctory inspections). Hybrid: technical track live, behavioral track theater. These are empirically distinct (the measurements should show different patterns for each). If measurements cannot distinguish them, the readings are artifacts of framing, not facts about the constraint.',
    'If the readings are genuinely empirically distinct (measurable differences in drill outcomes, inspection effectiveness, occupant knowledge retention), then the hybrid reading is a factual finding that some systems are competent and some are ritual. If the readings are all plausible framings of the same data, the choice among them is interpretive and the committer frame is active. The answer affects whether the constraint has a discoverable type or whether type depends on reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_equivalence_hypothesis, conceptual, 'Whether the three kernel readings represent empirically distinct states or interpretive framings of equivalent data.').

omega_variable(
    occupant_identity_lock_mechanism,
    'For evacuation-dependent populations, is exit from the constraint identity-locked (they cannot leave the building/neighborhood because their life/work is there), structurally constrained (they could leave but at high cost), or trapped (they cannot leave under any circumstance)?',
    'Qualitative research with occupants about exit possibilities (could you move, what would it cost, what keeps you); economic analysis of moving costs relative to income; mapping of alternative housing/work options; ethnographic documentation of relocation patterns after major disasters.',
    'If identity-locked (home/life fused with the location), the suppression is internalized — even after the constraint''s enforcement is removed, occupants carry the immobilization with them. If constrained/trapped, the suppression is structural and external. The classification affects whether the constraint remains extractive after exit, and whether occupants are truly excluded or complicit in the constraint''s persistence through their own unwillingness to leave.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupant_identity_lock_mechanism, empirical, 'Mechanism and permanence of occupant immobility in the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__hybrid_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__hybrid_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__hybrid_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__hybrid_reading, theater_ratio, 25, 0.65).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t35, preparedness_persistence__hybrid_reading, theater_ratio, 35, 0.64).
narrative_ontology:measurement_basis(prep_tr_t35, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__hybrid_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__hybrid_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__hybrid_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__hybrid_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t35, preparedness_persistence__hybrid_reading, base_extractiveness, 35, 0.61).
narrative_ontology:measurement_basis(prep_be_t35, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_persistence__hybrid_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__hybrid_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_persistence__hybrid_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_persistence__hybrid_reading, suppression_requirement, 25, 0.46).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t35, preparedness_persistence__hybrid_reading, suppression_requirement, 35, 0.47).
narrative_ontology:measurement_basis(prep_su_t35, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the 'preparedness_persistence' kernel. The competence_reading instantiates preparedness as fully operationally vital across both tracks. The husk_reading instantiates preparedness as largely memorial performance. The hybrid_reading (this constraint) identifies empirical stratification: engineering inspection remains live and competent; evacuation drills have largely become ritualized. The three readings are linked by shared kernel identity but diverge on the factual question of which preparedness mechanisms remain operationally vital. All three are empirically falsifiable — they make measurable claims about drill outcomes, inspector effectiveness, and occupant knowledge retention — though the readings emphasize different measurement protocols.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
