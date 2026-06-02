% ============================================================================
% CONSTRAINT STORY: physician_call_reluctance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_physician_call_reluctance, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: physician_call_reluctance
 *   human_readable: Physician After-Hours Call Reluctance
 *   domain: healthcare_systems/institutional_communication/organizational_behavior
 *
 * SUMMARY:
 *   Physician after-hours call reluctance is presented within healthcare
 *   institutions as an immutable feature of medical practice — a natural
 *   consequence of physician cognitive load limits, burnout risk, and the
 *   need for sustainable work-life boundaries. Nurses absorb the gatekeeping
 *   burden, deploying standardized scripts to assess patient needs and
 *   determine whether physician contact is warranted. Patients experience
 *   delays in physician contact for urgent needs, accepting this as a natural
 *   constraint of hospital care. The constraint appears as a mountain
 *   (unchangeable natural law) from the perspectives of nurses and patients,
 *   who have no power to alter physician availability and no exit from the
 *   system. Physicians and hospital administration experience the constraint
 *   as coordination (rope) — a rational division of labor that protects
 *   physician capacity and reduces costs. The analytical observer recognizes
 *   both the genuine coordination function (triage is necessary) and the
 *   asymmetric extraction (nurses bear decision-making risk without
 *   prescribing authority; patients bear delay-related harm risk). The
 *   constraint is a candidate false summit: the specific institutional
 *   arrangement that concentrates gatekeeping burden on nurses while
 *   preserving physician autonomy is not a law of nature but a policy choice
 *   that benefits identifiable agents.
 *
 * KEY AGENTS:
 *   - Night-Shift Nurse: Primary victim (powerless/trapped) — bears gatekeeping burden and decision-making risk without prescribing authority; cannot change physician behavior or exit the role
 *   - Patient with Urgent Needs: Secondary victim (moderate/constrained) — experiences delays in physician contact; high switching costs and information asymmetry create perceived immutability
 *   - Physician: Primary beneficiary (institutional/arbitrage) — protected from after-hours interruptions; reduced cognitive load and burnout risk; frames call reluctance as professional boundary-setting
 *   - Hospital Administration: Secondary beneficiary (institutional/arbitrage) — reduced physician overtime costs; improved physician retention through burnout protection; sees nurse gatekeeping as cost-efficient coordination
 *   - Healthcare Systems Analyst: Analytical observer (analytical/analytical) — recognizes both coordination function and asymmetric extraction; identifies mountain framing as potential false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(physician_call_reluctance, 0.08).
domain_priors:suppression_score(physician_call_reluctance, 0.03).
domain_priors:theater_ratio(physician_call_reluctance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(physician_call_reluctance, extractiveness, 0.08).
narrative_ontology:constraint_metric(physician_call_reluctance, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(physician_call_reluctance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(physician_call_reluctance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(physician_call_reluctance, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(physician_call_reluctance, mountain).
narrative_ontology:human_readable(physician_call_reluctance, "Physician After-Hours Call Reluctance").
narrative_ontology:topic_domain(physician_call_reluctance, "healthcare_systems/institutional_communication/organizational_behavior").

domain_priors:emerges_naturally(physician_call_reluctance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(physician_call_reluctance, physicians).
narrative_ontology:constraint_beneficiary(physician_call_reluctance, hospital_administration).
narrative_ontology:constraint_victim(physician_call_reluctance, nurses).
narrative_ontology:constraint_victim(physician_call_reluctance, patients_with_urgent_needs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NIGHT-SHIFT NURSE (MOUNTAIN) — Experiences physician unavailability as an immutable feature of the healthcare system. Cannot change physician behavior, cannot prescribe, cannot exit the gatekeeping role. The constraint appears as natural law: 'doctors don't take calls at night' is treated as a fact of nature rather than a policy choice.
constraint_indexing:constraint_classification(physician_call_reluctance, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PATIENT WITH URGENT NEEDS (MOUNTAIN) — Experiences delayed physician contact as an unchangeable reality of hospital care. High switching costs (cannot change hospitals mid-treatment) and information asymmetry (cannot assess whether delay is necessary) create perceived immutability. The patient accepts 'the doctor will call back when available' as a natural constraint of medical practice.
constraint_indexing:constraint_classification(physician_call_reluctance, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PHYSICIAN (ROPE) — Experiences call protocols as legitimate coordination: triage systems filter unnecessary interruptions, allowing focus on critical cases. The physician benefits from reduced cognitive load and burnout protection. Call reluctance is framed as professional boundary-setting necessary for sustainable practice. Low experienced extraction — the constraint serves their interests.
constraint_indexing:constraint_classification(physician_call_reluctance, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HOSPITAL ADMINISTRATION (ROPE) — Sees nurse gatekeeping as cost-efficient coordination: nurses handle routine queries, physicians handle complex decisions. The division of labor appears as rational resource allocation. Administration benefits from reduced physician overtime costs and improved physician retention (lower burnout). Experiences the constraint as a coordination mechanism that solves the legitimate problem of physician capacity limits.
constraint_indexing:constraint_classification(physician_call_reluctance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both genuine coordination (triage is necessary; physicians cannot take every call) and asymmetric extraction (nurses bear decision-making burden without authority; patients experience delays that sometimes cause harm). The constraint coordinates scarce physician attention but does so through a mechanism that systematically transfers risk and cognitive load to less-powerful agents. The analyst sees the mountain framing (physician unavailability as natural law) as a false summit — the specific distribution of gatekeeping burden is a policy choice, not an inherent feature of medical practice.
constraint_indexing:constraint_classification(physician_call_reluctance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(physician_call_reluctance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(physician_call_reluctance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(physician_call_reluctance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(physician_call_reluctance, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(physician_call_reluctance, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(physician_call_reluctance, ExtMetricName, E),
    domain_priors:suppression_score(physician_call_reluctance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(physician_call_reluctance),
    narrative_ontology:constraint_metric(physician_call_reluctance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(physician_call_reluctance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(physician_call_reluctance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint has a genuine coordination function (triage of physician attention is necessary given capacity limits), and the extraction component (nurses bearing risk without authority, patients experiencing delays) is relatively small in magnitude. Most nurse gatekeeping decisions are routine and do not require physician intervention; most patient delays do not cause harm. The low extractiveness reflects that the constraint is closer to legitimate coordination than to pure extraction, though the asymmetry is real. Suppression (0.03): Very low. Nurses have limited formal power to change physician call protocols, but the barriers are primarily institutional inertia and professional hierarchy rather than active coercion. Patients have high switching costs but are not legally or physically trapped. The low suppression reflects that alternatives exist (hospitalist models, nurse practitioner authority, telemedicine) and are being adopted in some systems. Resistance (0.08): Very low. The constraint is not actively resisted by most participants — nurses accept gatekeeping as part of their role, patients accept delays as normal, physicians frame boundaries as professional necessity. The low resistance supports the mountain classification from nurse and patient perspectives but also flags the constraint as a false summit candidate: lack of resistance may reflect naturalization rather than genuine immutability. Accessibility collapse (0.92): Very high. From the nurse and patient perspectives, physician unavailability appears as an unchangeable feature of the system. Nurses cannot imagine a system where they have prescribing authority for routine urgent scenarios; patients cannot imagine a system where physicians are immediately available. The high accessibility collapse is the primary driver of the mountain classification from these perspectives. Theater ratio (0.15): Low. Nurse script deployment is largely functional — scripts guide genuine triage decisions rather than performative compliance. Some theater exists (documentation of 'attempted physician contact' when the nurse knows the physician will not respond), but it is a minor component. The low theater ratio supports the coordination function and distinguishes this constraint from degraded institutional rituals (pitons).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a classic false summit pattern: what appears as natural law (mountain) from the perspectives of powerless agents (nurses, patients) is revealed as a contingent institutional arrangement when viewed from the analytical perspective. Nurses and patients experience physician unavailability as unchangeable because they have no power to alter physician behavior and no exit from the system. The high accessibility collapse (0.92) reflects genuine perceptual closure: from within the nurse or patient role, alternative arrangements are literally unimaginable. Physicians and administration experience the constraint as coordination (rope) because it serves their interests: reduced interruptions, lower costs, sustainable workload. The analytical observer recognizes both the genuine coordination function (triage is necessary) and the asymmetric extraction (burden transfer without authority). The perspectival gap is not a disagreement about facts but a difference in structural position: the same institutional arrangement appears as immutable natural law, rational coordination, or asymmetric extraction depending on where you stand in the power hierarchy. The false summit detector will flag this constraint because it has the mountain metric profile (low extractiveness, low suppression, high accessibility collapse, low resistance, emerges naturally) but also has identifiable beneficiaries (physicians, administration) — the signature of a naturalized contingent arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Physicians are primary beneficiaries: the constraint protects their time, reduces cognitive load, and prevents burnout. They experience low or negative effective extraction (the constraint serves their interests). Hospital administration is a secondary beneficiary: reduced overtime costs and improved physician retention. Nurses are primary victims: they bear gatekeeping burden and decision-making risk without prescribing authority. However, their victimization is moderate rather than severe — most gatekeeping decisions are routine, and the cognitive burden is manageable. Patients with urgent needs are secondary victims: they experience delays in physician contact, but most delays do not cause harm. The directionality derivation chain computes d from these structural relationships: beneficiaries with arbitrage exit options (physicians, administration) get low d → low/negative chi; victims with trapped or constrained exit options (nurses, patients) get higher d → higher chi. The perspectival gap emerges from these different structural positions: beneficiaries see coordination (rope), victims see immutability (mountain), and the analytical observer sees both (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the mountain classification from nurse and patient perspectives is a genuine perceptual phenomenon (high accessibility collapse, low resistance) while simultaneously being a false summit (the constraint benefits identifiable agents and could be reorganized). The mandatrophy is not 'is this a mountain or not?' but 'is the mountain perception accurate or naturalized?' The analytical observer's tangled rope classification is not a contradiction of the nurse's mountain classification — it is a recognition that the nurse's perception is structurally determined by their powerless position. The constraint IS a mountain from the nurse's perspective (unchangeable within their power horizon) AND a false summit from the analytical perspective (changeable at the institutional level). The omega variables document the empirical uncertainties that would resolve whether the mountain framing is justified: if physician unavailability is a genuine cognitive capacity limit, the mountain classification is correct; if it is a constructed professional boundary, the mountain classification naturalizes extraction. The false summit mechanism does not require high coupling or high extractiveness — it requires only that a constraint with mountain metrics has identifiable beneficiaries, which flags the possibility that the 'natural law' framing serves those beneficiaries' interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_capacity_vs_professional_boundary,
    'Is physician after-hours unavailability a natural capacity limit (cognitive load, burnout risk, patient safety) or a constructed professional boundary that could be reorganized without harm?',
    'Comparative analysis of healthcare systems with different call protocols (hospitalist models, nurse practitioner prescribing authority, telemedicine triage). Measurement of patient outcomes, nurse burnout, and physician satisfaction across models.',
    'If natural capacity limit: mountain classification is correct from all perspectives — the constraint is an immutable feature of human cognitive limits. If constructed boundary: the mountain classification from nurse and patient perspectives is a false summit — the constraint naturalizes a contingent institutional arrangement that benefits physicians and administration at nurses'' and patients'' expense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_capacity_vs_professional_boundary, empirical, 'Whether physician unavailability reflects natural limits or constructed boundaries').

omega_variable(
    nurse_gatekeeping_harm_threshold,
    'At what frequency of delayed physician contact does nurse gatekeeping without prescribing authority cause measurable patient harm?',
    'Retrospective analysis of adverse events correlated with time-to-physician-contact; comparison of outcomes in systems with vs without nurse prescribing authority for common urgent scenarios.',
    'If harm threshold is low (delays >30 minutes cause measurable harm): extractiveness is higher than measured, and the constraint is a snare from patient perspective. If harm threshold is high (delays <4 hours rarely cause harm): extractiveness is lower, and the constraint is closer to genuine coordination (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nurse_gatekeeping_harm_threshold, empirical, 'Harm threshold for delayed physician contact in urgent scenarios').

omega_variable(
    script_deployment_as_extraction_proxy,
    'Does high nurse script deployment frequency indicate efficient triage (coordination) or responsibility without authority (extraction)?',
    'Qualitative analysis of nurse decision-making autonomy: do scripts empower nurses to handle routine cases independently, or do they formalize the transfer of cognitive burden without corresponding authority? Correlation of script frequency with nurse burnout and job satisfaction.',
    'If scripts empower: they are a coordination tool, and extractiveness is low. If scripts formalize burden transfer: they are an extraction mechanism, and the constraint is a tangled rope or snare from nurse perspective rather than a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_deployment_as_extraction_proxy, conceptual, 'Whether nurse scripts represent coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(physician_call_reluctance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phys_call_tr_t0, physician_call_reluctance, theater_ratio, 0, 0.12).
narrative_ontology:measurement(phys_call_tr_t5, physician_call_reluctance, theater_ratio, 5, 0.14).
narrative_ontology:measurement(phys_call_tr_t10, physician_call_reluctance, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(phys_call_be_t0, physician_call_reluctance, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(phys_call_be_t5, physician_call_reluctance, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(phys_call_be_t10, physician_call_reluctance, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(physician_call_reluctance, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint could be decomposed into multiple stories if different observables yield different epsilon values: (1) time-to-physician-contact for truly urgent cases (higher extractiveness if delays cause harm), (2) nurse cognitive burden from gatekeeping (moderate extractiveness from responsibility without authority), (3) physician burnout protection (low extractiveness, genuine coordination). The current story uses time-to-physician-contact as the primary observable and models the constraint as a single integrated phenomenon. If empirical analysis reveals that these observables yield structurally different epsilon values, decomposition would be warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
