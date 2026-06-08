% ============================================================================
% CONSTRAINT STORY: patient_demand_escalation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_patient_demand_escalation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: patient_demand_escalation
 *   human_readable: Patient Demand Escalation in Primary Care
 *   domain: health_workforce_economics/organizational_behavior/gender_labor
 *
 * SUMMARY:
 *   Patient demand escalation in primary care represents a structural shift
 *   in healthcare delivery expectations over the past two decades. Patients
 *   increasingly expect immediate access (same-day appointments, rapid
 *   message response), multi-problem visits (addressing 3+ concerns per
 *   encounter), and 24/7 availability (after-hours calls, weekend messages)
 *   without corresponding increases in physician time allocation or
 *   compensation. This constraint exhibits genuine coordination function —
 *   patients with complex chronic conditions benefit from expanded access,
 *   and multi-problem visits are clinically efficient — but also substantial
 *   asymmetric extraction concentrated on primary care physicians and
 *   clinical staff. The 41.1% of exiting physicians citing 'unrealistic
 *   patient demands' as a reason for leaving primary care signals that the
 *   constraint's extraction has reached identity-lock-breaking severity for a
 *   significant fraction of the workforce. The constraint is enforced through
 *   professional duty norms, patient satisfaction metrics tied to
 *   reimbursement, malpractice liability concerns, and the
 *   healthcare-as-service-industry framing that positions patients as
 *   customers whose expectations must be met. Theater ratio (0.48) reflects
 *   the performative dimension: physicians spend substantial time on message
 *   response and documentation that does not improve clinical outcomes but
 *   satisfies administrative and liability requirements. The constraint's
 *   extractiveness has increased steadily from 0.32 (2000) to 0.58 (2020) as
 *   electronic health records enabled asynchronous messaging, patient portals
 *   expanded access expectations, and value-based care models tied
 *   reimbursement to satisfaction scores.
 *
 * KEY AGENTS:
 *   - Primary Care Physicians: Primary victim (powerless/identity_locked) — bear time and emotional extraction without compensation adjustment; 41.1% exit citing unrealistic demands
 *   - Clinical Support Staff: Secondary victim (powerless/constrained) — bear message triage and patient frustration management without decision authority
 *   - Physician Families: Tertiary victim (moderate/constrained) — bear extraction through absent evenings, interrupted weekends, and burnout affecting family stability
 *   - Patients with Complex Needs: Primary beneficiary (moderate/mobile) — benefit from expanded access and multi-problem visit efficiency
 *   - Health System Administrators: Institutional beneficiary (institutional/arbitrage) — benefit from patient satisfaction metrics and competitive positioning
 *   - Insurance Payers: Institutional beneficiary (institutional/arbitrage) — benefit from primary care gatekeeping reducing specialist and emergency utilization
 *   - Primary Care Advocacy Coalition: Organized agents (organized/constrained) — building alternative care models with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(patient_demand_escalation, 0.58).
domain_priors:suppression_score(patient_demand_escalation, 0.62).
domain_priors:theater_ratio(patient_demand_escalation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(patient_demand_escalation, extractiveness, 0.58).
narrative_ontology:constraint_metric(patient_demand_escalation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(patient_demand_escalation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(patient_demand_escalation, tangled_rope).
narrative_ontology:human_readable(patient_demand_escalation, "Patient Demand Escalation in Primary Care").
narrative_ontology:topic_domain(patient_demand_escalation, "health_workforce_economics/organizational_behavior/gender_labor").

domain_priors:requires_active_enforcement(patient_demand_escalation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(patient_demand_escalation, patients_with_complex_needs).
narrative_ontology:constraint_beneficiary(patient_demand_escalation, health_system_administrators).
narrative_ontology:constraint_beneficiary(patient_demand_escalation, insurance_payers).
narrative_ontology:constraint_victim(patient_demand_escalation, primary_care_physicians).
narrative_ontology:constraint_victim(patient_demand_escalation, clinical_support_staff).
narrative_ontology:constraint_victim(patient_demand_escalation, physician_families).
narrative_ontology:constraint_vindicates(patient_demand_escalation, patient_centered_care_doctrine).
narrative_ontology:constraint_vindicates(patient_demand_escalation, healthcare_as_service_industry_framing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIMARY CARE PHYSICIAN (SNARE) — Identity-locked by professional commitment and caregiving identity. Structurally mobile (could change specialties, leave medicine, relocate) but identity is constituted through the patient relationship and professional duty. Exit would require abandoning the physician identity constructed over decades of training and practice. Experiences maximum extraction: rising message volume, multi-problem visits, after-hours contact expectations with no corresponding time allocation or compensation increase. The 41.1% exit reason 'unrealistic patient demands' reflects identity lock breaking under sustained extraction — physicians leave only when the identity frame itself fractures.
constraint_indexing:constraint_classification(patient_demand_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: CLINICAL SUPPORT STAFF (SNARE) — Constrained by regional labor market, specialized training investment, and economic dependency. Face high barriers to exit (retraining costs, wage loss, geographic immobility) but not identity-locked — their professional identity is less fused with patient service than physicians'. Bear extraction through message triage burden, patient frustration management, and schedule compression without decision-making authority or compensation adjustment. Lower power than physicians but similar extraction experience.
constraint_indexing:constraint_classification(patient_demand_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PATIENT WITH COMPLEX NEEDS (TANGLED ROPE) — Mobile within the healthcare system (can switch providers, seek specialists, use urgent care) and benefits from expanded access expectations (can address multiple problems per visit, message between visits, expect after-hours response). But also constrained by the system's capacity limits: longer wait times, shorter visit durations, physician burnout reducing care quality. Genuine coordination function (multi-problem visits are clinically efficient for complex patients) coexists with extraction from physicians. Mixed experience: beneficiary of access expansion, victim of system strain.
constraint_indexing:constraint_classification(patient_demand_escalation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: HEALTH SYSTEM ADMINISTRATOR (ROPE) — Benefits from patient satisfaction metrics, reduced emergency department utilization, and competitive positioning through expanded access promises. Experiences the constraint as coordination: meeting patient expectations improves market share and payer relationships. Has arbitrage-level exit (can shift between health systems, consulting, policy roles) and institutional power to set access policies. Net beneficiary — extraction runs away from this agent toward physicians.
constraint_indexing:constraint_classification(patient_demand_escalation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE PAYER (ROPE) — Benefits from primary care gatekeeping that reduces specialist referrals and emergency utilization. Expanded primary care access (multi-problem visits, message-based triage, after-hours availability) substitutes for higher-cost care settings. Experiences the constraint as efficient coordination: primary care physicians absorb demand that would otherwise flow to expensive alternatives. Has arbitrage exit and institutional power. Net beneficiary.
constraint_indexing:constraint_classification(patient_demand_escalation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PHYSICIAN FAMILY MEMBER (TANGLED ROPE) — Constrained by economic dependency on physician income and geographic ties to physician's practice location. Benefits from physician's professional status and income but bears extraction through absent evenings (message response), interrupted weekends (patient calls), and biographical-scale burnout affecting family stability. Genuine coordination function (family economic security) coexists with extraction (time and emotional availability). Mixed experience: beneficiary of physician career, victim of demand escalation.
constraint_indexing:constraint_classification(patient_demand_escalation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: PRIMARY CARE ADVOCACY COALITION (SCAFFOLD) — Organized agents (medical associations, physician unions, burnout researchers) see the demand escalation as a temporary coordination failure with a sunset: team-based care models, AI-assisted triage, asynchronous care platforms, and scope-of-practice expansion for nurse practitioners and physician assistants are building alternative pathways that redistribute demand. Constrained by institutional inertia and payer resistance but has organizational power and sees an exit path. Estimated sunset: 10-15 years for care team models to mature and for compensation structures to adjust.
constraint_indexing:constraint_classification(patient_demand_escalation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination (patients with complex needs benefit from expanded access; multi-problem visits are clinically efficient; message-based triage reduces unnecessary office visits) and asymmetric extraction (physicians bear time and emotional costs without compensation adjustment; identity-locked professionals cannot exit; 41.1% cite unrealistic demands as exit reason). The coordination function is real but the extraction is substantial and concentrated on a specific victim group. Requires active enforcement through professional norms, patient satisfaction metrics, and malpractice liability. Tangled Rope classification reflects the structural coexistence of coordination and extraction.
constraint_indexing:constraint_classification(patient_demand_escalation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(patient_demand_escalation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(patient_demand_escalation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(patient_demand_escalation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(patient_demand_escalation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(patient_demand_escalation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Primary care physicians bear rising message volume (average 50+ patient messages per day in high-volume practices), multi-problem visits that compress clinical time, and after-hours contact expectations without corresponding time allocation or compensation increase. The 41.1% exit rate citing unrealistic demands indicates extraction has reached workforce-destabilizing levels. However, extraction is not maximal (not 0.8+) because some physicians successfully set boundaries, some practices implement team-based triage, and some patients genuinely benefit from expanded access. The value reflects the biographical-scale career impact: physicians who stay adapt their practice or accept burnout; physicians who leave cite demand escalation as a primary driver. Suppression (0.62): Moderate-high. Significant barriers to exit include professional duty norms (Hippocratic commitment, patient abandonment concerns), identity lock (physician identity constituted through patient service), malpractice liability (failure to respond to messages creates legal risk), employment contracts (productivity expectations, patient satisfaction metrics), and economic dependency (medical school debt, income requirements). Suppression has increased over the interval as electronic health records made message volume trackable and patient satisfaction scores became tied to reimbursement. But suppression is not total — physicians can and do exit primary care, shift to concierge models, or reduce clinical hours. Theater ratio (0.48): Moderate. Substantial performative activity includes message documentation for liability protection (not clinical value), satisfaction survey optimization (scripted communication), and administrative response to patient complaints. But theater is not dominant — much of the expanded access does serve genuine clinical coordination (multi-problem visits for complex patients, message-based triage reducing unnecessary office visits). Theater has increased as administrative requirements have grown but remains below the threshold where performance dominates function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — rising patient expectations for expanded access — appears as pure extraction (Snare) from the powerless physician and staff perspectives, mixed coordination-extraction (Tangled Rope) from the moderate patient and family perspectives, pure coordination (Rope) from the institutional administrator and payer perspectives, and temporary problem with sunset (Scaffold) from the organized advocacy perspective. The physician sees identity-locked extraction: professional duty norms prevent exit while demands escalate without compensation. The patient sees genuine coordination: multi-problem visits are efficient and message-based triage is convenient. The administrator sees market advantage: patient satisfaction drives competitive positioning. The payer sees cost control: primary care gatekeeping reduces expensive specialist and emergency utilization. The advocacy coalition sees a solvable problem: team-based care models and AI-assisted triage can redistribute demand. The analytical observer sees the structural coexistence: genuine coordination function (patients benefit) and asymmetric extraction (physicians bear costs) enforced through professional norms and satisfaction metrics. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The 41.1% exit rate citing unrealistic demands is the empirical signal that extraction has exceeded sustainable levels for a significant workforce fraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Primary care physicians are the primary victims — they bear the time and emotional costs of demand escalation without compensation adjustment. Their identity lock (professional duty, caregiving identity) combined with powerless structural position produces high directionality toward full target (d approaching 0.9-1.0). Clinical support staff are secondary victims with constrained exit options, producing high but slightly lower directionality (d around 0.8). Physician families are tertiary victims with moderate power and constrained exit, producing moderate-high directionality (d around 0.6-0.7). Patients with complex needs are primary beneficiaries — they gain from expanded access and multi-problem visit efficiency — with mobile exit options, producing low directionality toward beneficiary (d around 0.2-0.3). Health system administrators and insurance payers are institutional beneficiaries with arbitrage exit, producing very low or negative directionality (d around 0.0-0.1). The primary care advocacy coalition has organized power and constrained exit, with mixed beneficiary/victim status (building solutions but bearing organizational costs), producing moderate directionality (d around 0.4-0.5). The analytical observer sees both coordination and extraction, with analytical exit options producing low directionality (d around 0.3).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Tangled Rope classification captures the structural coexistence of genuine coordination and asymmetric extraction. The coordination function is real: patients with complex chronic conditions benefit from expanded access; multi-problem visits are clinically efficient for addressing comorbidities; message-based triage reduces unnecessary office visits and emergency department utilization. But the extraction is also real: physicians bear time costs (50+ messages per day, after-hours contact, multi-problem visit compression) without compensation adjustment; professional duty norms and identity lock prevent exit; 41.1% of exiting physicians cite unrealistic demands. The constraint is not pure coordination (Rope) because identifiable victims exist and extraction is substantial. It is not pure extraction (Snare) because the coordination function is genuine and some patients genuinely benefit. It is Tangled Rope because both functions coexist and the constraint requires active enforcement (professional norms, satisfaction metrics, malpractice liability) to persist. The mandate (expanded patient access) has not outlived its function — the coordination need is real — but the implementation extracts asymmetrically from physicians. The scaffold perspective (advocacy coalition) represents a real structural feature: team-based care models and AI-assisted triage are building alternative pathways with a 10-15 year sunset horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_legitimacy_threshold,
    'What threshold distinguishes legitimate patient need for expanded access from extractive demand escalation enabled by professional duty norms?',
    'Clinical outcome analysis: correlation between multi-problem visit frequency and health outcomes; comparison of patient satisfaction vs physician burnout trajectories; identification of demand patterns that improve vs degrade care quality',
    'If threshold is low (most demand is legitimate need): constraint is primarily coordination (Rope from more perspectives). If threshold is high (most demand is extractive): constraint is primarily extraction (Snare from more perspectives). Current 41.1% exit rate citing unrealistic demands suggests threshold is being exceeded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_legitimacy_threshold, empirical, 'Threshold distinguishing legitimate access need from extractive demand').

omega_variable(
    team_based_care_sufficiency,
    'Do team-based care models (nurse practitioners, physician assistants, care coordinators) genuinely redistribute demand or merely add coordination overhead while preserving physician extraction?',
    'Longitudinal comparison of physician message volume, after-hours contact, and burnout rates in team-based vs traditional practices; analysis of which demand types are successfully delegated vs which remain physician-dependent',
    'If effective: scaffold perspective confirmed — team models provide a real sunset. If ineffective: demand escalation is structurally tied to physician role regardless of team composition, and the scaffold perspective is aspirational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(team_based_care_sufficiency, empirical, 'Whether team-based care models effectively redistribute demand').

omega_variable(
    identity_lock_mechanism,
    'Is physician identity lock primarily professional (duty to patients, Hippocratic commitment) or economic (sunk training costs, debt burden, income dependency)?',
    'Exit pattern analysis: comparison of exit rates and exit reasons across debt levels, career stages, and specialty types; qualitative analysis of exit narratives distinguishing identity-based vs economic barriers',
    'If primarily professional: identity lock is cognitive and could shift with reframing (e.g., ''sustainable practice is better patient care''). If primarily economic: identity lock is material constraint misattributed to identity, and interventions must address economic barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether physician identity lock is professional duty or economic dependency').

omega_variable(
    gender_differential_extraction,
    'Does demand escalation extract differentially from female vs male physicians through gendered expectations of emotional labor and availability?',
    'Gender-stratified analysis of message volume, after-hours contact frequency, multi-problem visit rates, and burnout scores; comparison of patient expectations and satisfaction ratings by physician gender',
    'If differential extraction exists: the constraint has a gender dimension not captured in base extractiveness, and female physicians experience higher effective extraction. If no differential: the constraint extracts uniformly across gender.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_differential_extraction, empirical, 'Whether demand escalation extracts differentially by physician gender').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.62) structural (malpractice liability, employment contracts, professional licensing requirements) or internalized (physicians believe they deserve the demands, have been isolated from alternative practice models, or have fused their identity with unlimited availability)?',
    'Post-exit suppression trajectory: if physicians who leave primary care report continued guilt, anxiety, or identity crisis, reclassify suppression as partially internalized. If they report relief and structural barriers as the primary constraint, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — physicians carry the suppression with them after exit. If structural, interventions can target external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(patient_demand_escalation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pde_theater_2000, patient_demand_escalation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(pde_theater_2005, patient_demand_escalation, theater_ratio, 5, 0.32).
narrative_ontology:measurement(pde_theater_2010, patient_demand_escalation, theater_ratio, 10, 0.38).
narrative_ontology:measurement(pde_theater_2015, patient_demand_escalation, theater_ratio, 15, 0.43).
narrative_ontology:measurement(pde_theater_2020, patient_demand_escalation, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(pde_extract_2000, patient_demand_escalation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pde_extract_2005, patient_demand_escalation, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(pde_extract_2010, patient_demand_escalation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(pde_extract_2015, patient_demand_escalation, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(pde_extract_2020, patient_demand_escalation, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pde_suppress_2000, patient_demand_escalation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(pde_suppress_2005, patient_demand_escalation, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(pde_suppress_2010, patient_demand_escalation, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(pde_suppress_2015, patient_demand_escalation, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(pde_suppress_2020, patient_demand_escalation, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(patient_demand_escalation, resource_allocation).
narrative_ontology:affects_constraint(patient_demand_escalation, primary_care_physician_burnout).
narrative_ontology:affects_constraint(patient_demand_escalation, electronic_health_record_burden).
narrative_ontology:affects_constraint(patient_demand_escalation, fee_for_service_misalignment).

% DUAL FORMULATION NOTE:
% Patient demand escalation is structurally distinct from but causally linked to physician burnout (the psychological outcome), EHR burden (the documentation mechanism), and fee-for-service misalignment (the compensation structure). Each has its own extractiveness value. This constraint focuses on the demand-side structural shift; the linked constraints address the supply-side responses and systemic misalignments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
