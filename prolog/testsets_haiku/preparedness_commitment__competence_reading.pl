% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint story models preparedness as LIVE EXERCISED
 *   COMPETENCE—the reading in which drills genuinely test decision-making,
 *   generational knowledge transfer actually happens through supervised
 *   practice, and organizational capacity is maintained adaptively across
 *   personnel turnover. This is ONE reading of the contested 'preparedness
 *   commitment' kernel. Alternative readings include the 'husk_reading'
 *   (drills perform compliance but competence is hollow) and the
 *   'hybrid_reading' (memorial elements stabilize commitment while competence
 *   elements maintain function). The three readings have different ε values
 *   because they measure fundamentally different constraint structures: this
 *   reading's high accessibility_collapse (0.92) and low resistance (0.08)
 *   reflect that once the competence mechanism is understood, alternatives
 *   collapse (competence cannot be delegated or postponed) and active
 *   resistance is minimal (no party structurally opposes real competence,
 *   though some may cut corners). The claim/metric gap is intentional: the
 *   constraint is CLAIMED as rope (genuine coordination with participation
 *   benefits) while the authored metrics place it at the lower-extractiveness
 *   end of rope and the measurement series shows extraction rising very
 *   slightly over a generational cycle (as knowledge-transmission costs
 *   accumulate and the organization faces mounting pressure to reduce drill
 *   time).
 *
 * KEY AGENTS:
 *   - Operational Organization: Maintains drills and training infrastructure; bears the cost of exercise disruption
 *   - Trained Personnel: Core to the constraint—embodied knowledge and identity-locked participation
 *   - Protected Population: Benefits only when crisis arrives; cannot perceive the constraint until competence is tested
 *   - Senior Cohort: Pays through knowledge-transmission work; embodies the tacit judgment that must be transmitted
 *   - Junior Cohort: Pays through learning pressure; must develop competence before they can lead
 *   - Oversight Body: Monitors whether drills test competence or perform theater; can reorient the commitment
 *   - Neighboring Agencies: Excluded from or included in the preparedness commitment; their competence depends on integration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.28).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "institutional/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '1d28cf9b-4e41-4f41-a438-fd8387f34528').
narrative_ontology:cs_kernel_codification('1d28cf9b-4e41-4f41-a438-fd8387f34528', formalized).
narrative_ontology:cs_authority_grounding('1d28cf9b-4e41-4f41-a438-fd8387f34528', practice).
narrative_ontology:cs_interpretation_layer_present('1d28cf9b-4e41-4f41-a438-fd8387f34528').
narrative_ontology:cs_reading_relation('1d28cf9b-4e41-4f41-a438-fd8387f34528', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('1d28cf9b-4e41-4f41-a438-fd8387f34528', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('1d28cf9b-4e41-4f41-a438-fd8387f34528', foundational, drills_test_real_decision_making).
narrative_ontology:cs_axiom_status(drills_test_real_decision_making, holdable).
narrative_ontology:cs_axiom_grounding('1d28cf9b-4e41-4f41-a438-fd8387f34528', drills_test_real_decision_making, empirically_contingent).
narrative_ontology:cs_axiom('1d28cf9b-4e41-4f41-a438-fd8387f34528', foundational, competence_embodied_in_personnel).
narrative_ontology:cs_axiom_status(competence_embodied_in_personnel, holdable).
narrative_ontology:cs_axiom_grounding('1d28cf9b-4e41-4f41-a438-fd8387f34528', competence_embodied_in_personnel, empirically_contingent).
narrative_ontology:cs_reference_frame('1d28cf9b-4e41-4f41-a438-fd8387f34528', practice_based_knowledge_transmission).
narrative_ontology:cs_drift_state('1d28cf9b-4e41-4f41-a438-fd8387f34528', contemporary_budget_constrained, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1d28cf9b-4e41-4f41-a438-fd8387f34528', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, operational_organization).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, protected_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, trained_personnel).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, trained_personnel).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, senior_experienced_cohort).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, junior_incoming_cohort).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, organizational_memory_is_embodied_practice).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, competence_requires_continuous_exercise).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, generational_knowledge_transfer_through_participation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the routine structure of drills, training cycles, and real-world deployment. Designs and executes exercises that test decision-making under pressure. Carries the institutional memory through assigned personnel, documented procedures, and embedded practices. Bears the ongoing cost of maintaining competence: staff time, equipment maintenance, disruption to normal operations during exercises. Benefits from operational credibility and organizational resilience.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, operational_organization, agenda_setter,
    organized, generational, constrained, regional).

% Participate in drills and training as core professional identity. Internalize procedures, decision patterns, and judgment heuristics through repeated exercise. Develop tacit competence that cannot be transmitted through documents alone. Pay through time and attention cost (opportunity cost of deployment-preparation vs. other institutional work). Exit would constitute professional dereliction and identity dissolution—competence is inseparable from how they understand themselves.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, trained_personnel, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, trained_personnel, payer).

% Depend on the organization's maintained competence during actual disasters. Cannot opt out or exit; their survival may depend on whether the organization's drills translated into real decision-making capacity or remained theater. They receive the constraint's benefit only when the crisis arrives—until then, the competence is invisible to them.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, protected_population, beneficiary,
    powerless, immediate, trapped, regional).

% Bear the burden of knowledge transfer: mentoring, demonstrating, correcting, explaining decision logic. This transmission work compresses their own operational availability. They are the embodied library—without their active teaching during exercises, the competence decays. Retirement or turnover without effective transmission creates a drop in organizational capacity.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, senior_experienced_cohort, payer,
    moderate, biographical, constrained, regional).

% Must absorb the competence through participation in drills and real operations before they are ready to lead. Cannot obtain this knowledge from documents or classrooms alone—it requires supervised decision-making under time pressure. They are identity-locked through professional requirement and cultural expectation: competence is what makes them part of the organization.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, junior_incoming_cohort, payer,
    powerless, biographical, identity_locked, regional).

% Monitors whether drills are testing real competence or performing compliance. Audits after-action reviews and deployment outcomes. Can recommend retraining, procedure revision, or equipment changes. Takes no direct action but influences the organization's drill design and learning agenda.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, oversight_body, observer,
    institutional, generational, analytical, regional).

% May be coordinated INTO the organization's drills or may be excluded from them. If excluded, they cannot develop compatible competence or test inter-agency coordination. Their operational capacity depends partly on whether they are treated as part of the preparedness commitment or isolated from it.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, neighboring_agencies, excluded,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, operational_organization).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains organizational capacity to respond effectively to crises by embedding decision-making competence in personnel through supervised practice. Solves the problem of generational knowledge transfer: how does expertise survive retirements and transitions? Answer: through continuous exercise that recreates the decision environment and allows judgment to be transmitted through performance, not documents.
% TRANSFER_FUNCTION: Moves personnel time and attention from other organizational work into drills, training cycles, and knowledge-transmission activity. The exercise environment moves decision responsibility from senior mentors (who would normally handle it alone) to junior personnel (who must learn to handle it). This creates a cost in terms of operational efficiency during normal periods but generates a benefit in capacity when crises arrive.
% ABSENT_VOICES: Personnel in agencies excluded from the preparedness commitment—they would object that their isolation prevents them from developing compatible competence or understanding the organization's decision logic. Retirees and departed cohorts who could validate whether their successors actually acquired competence or merely memorized the form. The protected population does not object because they cannot perceive the constraint until it fails; if competence is only theater, their objections come too late.
% DISAPPEARANCE_RATIONALE: If the drills and training cycles vanished overnight, competence would begin to decay immediately. Judgment heuristics maintained through practice would become stale. Incoming personnel would lack supervised experience in the decision environments that actually matter. The next major crisis would likely reveal gaps—decisions made incorrectly, procedures forgotten, inter-agency coordination broken. The protected population would suffer higher casualty or damage rates. Over a generational cycle, the organization would essentially reset to the competence level of an untrained new hire.
% FOUNDING_PROBLEM: Organizations responsible for disaster response (emergency management, military command, medical emergency services) face a generational knowledge transfer problem: expertise accumulated by experienced personnel must transfer to their successors, but crisis decision-making cannot be learned from textbooks alone. The founding problem is how to maintain operational competence across generational turnover.
% FOUNDING_PROBLEM_CORROBORATION: After-action reports from major disaster responses consistently identify competence loss and coordination failures traceable to inadequate training or knowledge gaps among deployed personnel. Academic literature on organizational learning and high-reliability organizations documents that competence decays if drills and practice are suspended (Weick & Sutcliffe, Reason). Personnel from neighboring agencies that WERE integrated into the preparedness commitment report higher confidence in coordination during actual events compared to agencies that were excluded (inter-agency debriefs). This corroboration comes from outside the organization's own benefit structure—it comes from entities whose interests would be served equally if the organization had not maintained competence.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28 at interval end) because the constraint benefits all involved seats: the organization maintains its core capacity, personnel internalize professional identity and judgment, and the protected population gains safety. The measurement series shows a slight rise from 0.18 to 0.28 over the 40-year interval, reflecting the mounting opportunity cost of maintaining drills as budget pressures increase and competing operational demands grow. Suppression is VERY LOW (0.12) because the constraint works by voluntary participation and alignment, not coercion—personnel are identity-locked by professional culture, not forced by external threats. Theater is LOW (0.15) and stable because in this reading, drills genuinely test decision-making and failure in a drill is treated as a learning event, not a cover-up. The slight upward drift in both extractiveness and theater reflects budget constraints nudging the organization toward shorter, more-scripted drills and less real-time decision pressure—a slow drift toward the husk_reading, but not yet there. Accessibility_collapse is HIGH (0.92) because once you understand that organizational competence cannot exist without continuous supervised practice, the alternative of 'skip drills, read the manual when crisis comes' collapses as an option. Resistance is VERY LOW (0.08) because no internal seat opposes genuine competence—the constraint's persistence depends on alignment, not on coercion or suppression of dissent.
 *
 * PERSPECTIVAL GAP:
 *   The senior cohort and the junior cohort should compute different directionalities from the same constraint, even though both are identity-locked and trained. The senior cohort pays a high direct cost (knowledge-transmission work, opportunity cost of mentoring) and receives the benefit indirectly (organizational resilience, professional reputation). The junior cohort pays through learning pressure and time cost but receives direct identity benefit (becoming competent is how they become part of the organization). The measurement series tracks the aggregate organization, but a per-cohort measurement would show different extraction profiles—seniors bearing more of the transmission burden, juniors absorbing the learning cost earlier in their trajectory. The oversight body sits outside this cost structure and experiences the constraint as pure coordination (no cost to them, clear benefit from maintained capacity). The engine computes these per-seat differences from the stakeholder situation descriptions; the commentary surfaces the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The organization and protected population are structural beneficiaries (d near 0.0-0.2): they collect the core benefit of maintained competence without bearing the primary cost of knowledge transfer. Trained personnel—especially the senior cohort—are partial targets and partial beneficiaries (d near 0.4-0.6): they pay through time and attention cost but benefit through professional identity and organizational resilience. The junior cohort is shifted toward the target end (d near 0.5-0.7) because they absorb the learning pressure and cannot opt out without identity dissolution. Neighboring agencies are either excluded (d undefined) or included (d shifts them toward beneficiary if they gain compatible competence). No directionality override is needed—the structural derivation captures the asymmetry from beneficiary/victim declarations and exit_options.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is explicitly ANTI-mandatrophy: the founding problem (generational knowledge transfer in high-reliability organizations) remains live, the constraint solves it actively, and the organization maintains it because it is essential to its core function. There is no zombie state here—if drills stopped, competence would decay measurably within a single deployment cycle. The slight upward drift in extractiveness reflects NOT mandatrophy but rather budget pressure introducing small compromises (shorter drills, more scripting, less real-time pressure). The constraint would cross into mandatrophy territory only if the organization began maintaining drills DESPITE competence decay—if after-action reviews showed failures but the drills continued unchanged. That shift would be the husk_reading. The competence_reading explicitly assumes competence is maintained and drills remain genuinely corrective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_transmission_mechanism,
    'Is the embodied tacit competence in senior personnel actually transmitting to juniors through drill participation, or are juniors learning to follow procedures without developing the judgment that matters in novel situations?',
    'Compare performance data from junior personnel in actual crisis deployments before and after they have participated in a full drill cycle. If drill participation predicts better decision-making under pressure, transmission is happening; if drill participation predicts procedure compliance without adaptive judgment, transmission is failing.',
    'If transmission is failing, the constraint is drifting toward the husk_reading despite apparent adherence to drill routines. The organization would need to restructure drills to create higher-stakes decision pressure and real-time error correction, not compliance checking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_transmission_mechanism, empirical, 'Whether drills actually transmit judgment or only procedure.').

omega_variable(
    generational_cohort_alignment,
    'As generations replace one another, do incoming cohorts share the same implicit understanding of WHY drills matter and HOW they connect to real operational competence, or do new cohorts inherit only the FORM of the drills without the epistemological commitment?',
    'Interview and cognitive task data from personnel at multiple career stages about their understanding of drill purpose and their confidence in their own decision-making capability. Structural interviews asking senior, mid-career, and junior personnel to explain drill design rationale and failure modes.',
    'If generational alignment is breaking, the constraint is drifting toward husk_reading through a failure of cultural transmission, not formal procedure. The organization would need to invest in explicit teaching about drill purpose and decision rationale, not just procedure rehearsal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_cohort_alignment, empirical, 'Whether the competence commitment survives generational turnover.').

omega_variable(
    crisis_pressure_versus_drill_pressure,
    'Are drills creating sufficient decision pressure to maintain judgment competence, or is real crisis pressure so much higher that personnel trained only in lower-pressure drills collapse under actual deployment conditions?',
    'Comparison of performance on decision tasks under simulated-crisis pressure (high time pressure, incomplete information, consequence awareness) versus routine drill pressure. Stress physiology and cognitive function data during high-pressure simulation drills versus routine drills.',
    'If drill pressure is insufficient, the constraint is failing at its core function: competence is not being maintained. The organization would need to escalate the realism and pressure in drills to authentic crisis levels, risking equipment damage and personnel stress but gaining genuine competence validation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crisis_pressure_versus_drill_pressure, empirical, 'Whether drill conditions adequately test crisis-response competence.').

omega_variable(
    readings_as_competing_framings,
    'Are the competence_reading and husk_reading fundamentally incompatible readings of the same kernel, or are they describing different PHASES of a single constraint''s evolution (i.e., does every preparedness commitment start as competence_reading and gradually decay into husk_reading under budget pressure)?',
    'Longitudinal case study of a single organization''s drills over multiple generational cycles. Track whether competence metrics (decision-making quality, after-action learning, personnel confidence) decline over time while drill infrastructure remains formally constant. Track whether theater_ratio rises while base_extractiveness remains low (the drift signature).',
    'If the readings describe phases of decay, the boundary between them is not a logical foreclosure but a temporal threshold. Mandatrophy would be the husk_reading entering a terminal phase where theater persists despite zero competence. Policy interventions would aim to detect and arrest the decay before the threshold, not to prevent transition between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readings_as_competing_framings, conceptual, 'Whether readings are incompatible structures or phases of constraint evolution.').

omega_variable(
    neighboring_agency_integration,
    'Does integrating neighboring agencies into the preparedness commitment improve inter-agency competence and coordination, or does it create an implicit hierarchy where one organization''s drills dominate and others become followers rather than full participants?',
    'Compare inter-agency coordination performance in disaster response between agencies that have jointly trained and exercised versus agencies that have trained separately. Measure whether excluded agencies report confidence in their ability to coordinate with the lead organization.',
    'If integration improves coordination, the constraint''s scope should be expanded to include neighboring agencies explicitly. If integration creates hierarchy, the constraint splits into multiple readings per organization (some agencies experience rope, others experience snare). The excluded status of neighboring agencies is a stability risk: their autonomy is constrained but their competence is not guaranteed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neighboring_agency_integration, empirical, 'Whether preparedness commitment can be shared across organizational boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__competence_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__competence_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__competence_reading, theater_ratio, 25, 0.14).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__competence_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__competence_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__competence_reading, base_extractiveness, 25, 0.27).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__competence_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__competence_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__competence_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__competence_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__competence_reading, suppression_requirement, 25, 0.115).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__competence_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(prep_su_t30, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel has three structurally distinct readings. The competence_reading treats preparedness as live exercised knowledge—drills genuinely test decision-making and competence is maintained adaptively. The husk_reading treats preparedness as memorial performance—drills persist in form while competence decays. The hybrid_reading treats preparedness as layered—memorial elements stabilize the institutional commitment while competence elements maintain operational function. All three readings share the same foundational kernel (routine exercises meant to maintain capacity) but instantiate different structural relationships between form and function, ritual and competence, stability and adaptation. The three readings have different ε values: competence_reading has low extractiveness (0.28) because the constraint solves the actual founding problem; husk_reading has high extractiveness (approaching 0.6+) because the constraint persists as theater while competence fails; hybrid_reading has moderate extractiveness (0.35-0.45) because memorial and competence functions coexist but create friction. Each reading is a separate constraint story with its own beneficiary/victim structure, measurements, and omegas. The network links all three stories together so that cross-reading analysis can track institutional drift and reading transitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__competence_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
