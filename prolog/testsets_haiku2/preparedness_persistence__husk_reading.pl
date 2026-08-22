% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Preparedness Husk — Ritual Compliance Over Operational Capacity
 *   domain: institutional/social/disaster_preparedness
 *
 * SUMMARY:
 *   This is the husk_reading of the preparedness_persistence kernel: drills
 *   and inspections are memorial performance, form persists while operational
 *   competence atrophies. The constraint examined here is the standing
 *   arrangement — the performed preparedness mandate and its institutional
 *   enforcement — assessed under this specific reading's lights. The reading
 *   frames preparedness as a narrative that decouples from practice: agencies
 *   comply with mandates by staging drills and documenting inspections, but
 *   the actual training depth, equipment maintenance, and scenario adaptation
 *   that would produce real response capability have been crowded out by
 *   compliance logistics. The beneficiary is institutional legitimacy itself
 *   (the narrative that governance is competent) and the regulatory
 *   bureaucracy that maintains that narrative by enforcing the mandate. The
 *   victims are flood-vulnerable populations and responders who experience
 *   the gap between performed competence and actual capacity acutely. This
 *   reading competes with competence_reading (drills ARE live exercised
 *   knowledge) and hybrid_reading (some components remain competent, others
 *   ritualized). The husk_reading asserts that the primary function — actual
 *   operational readiness — has atrophied; what persists is the institutional
 *   form.
 *
 * KEY AGENTS:
 *   - regulatory_bureaucracy: Administers the mandate; benefits from compliance theater (institutional legitimacy); faces no operational consequences when response fails
 *   - local_emergency_agencies: Conduct mandated drills; pay the cost in resource diversion; constrained exit (sanctions for non-compliance)
 *   - flood_vulnerable_populations: Depend on actual response capacity; trapped in geography; exposed by substitution of performed for genuine preparedness
 *   - frontline_emergency_responders: Identity-locked to emergency work; experience gap between drill stage and real chaos acutely; cannot exit
 *   - political_leadership: Gains legitimacy from appearance of preparedness; low accountability for actual response failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.62).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.71).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Preparedness Husk — Ritual Compliance Over Operational Capacity").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "institutional/social/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_persistence__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, 'ec67a8f5-a9c4-40a8-a5ce-6de207969fea').
narrative_ontology:cs_kernel_codification('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', formalized).
narrative_ontology:cs_authority_grounding('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', extraction).
narrative_ontology:cs_interpretation_layer_present('ec67a8f5-a9c4-40a8-a5ce-6de207969fea').
narrative_ontology:cs_reading_relation('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', preparedness_persistence__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', foundational, drills_are_performed_compliance).
narrative_ontology:cs_axiom_status(drills_are_performed_compliance, holdable).
narrative_ontology:cs_axiom_grounding('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', drills_are_performed_compliance, empirically_contingent).
narrative_ontology:cs_axiom('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', foundational, institutional_legitimacy_decoupled_from_outcome).
narrative_ontology:cs_axiom_status(institutional_legitimacy_decoupled_from_outcome, holdable).
narrative_ontology:cs_axiom_grounding('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', institutional_legitimacy_decoupled_from_outcome, instrumental).
narrative_ontology:cs_reference_frame('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', adaptive_coordination_framework).
narrative_ontology:cs_drift_state('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', contemporary_atrophied_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec67a8f5-a9c4-40a8-a5ce-6de207969fea', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, regulatory_bureaucracy).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, institutional_legitimacy).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, flood_vulnerable_populations).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, frontline_emergency_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, local_emergency_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces preparedness mandates: coordinates drill schedules, approves inspection protocols, reviews compliance documentation. Maintains the constraint because it demonstrates governance capacity and fulfills statutory obligations. Does not itself suffer when actual flood response fails — the bureaucracy's success is measured by whether mandates exist and are performed, not by whether the performed activity is functionally adequate.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, regulatory_bureaucracy, agenda_setter,
    institutional, generational, analytical, national).

% Non-actor entity: the narrative that institutions are prepared and vigilant. The constraint vindicates this narrative by producing performed evidence of preparedness (drills, inspections, checklists) regardless of whether actual capacity exists. Institutional legitimacy is not a person or org; it is the abstraction that collecting evidence serves.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, institutional_legitimacy, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_persistence__husk_reading, institutional_legitimacy).

% Conduct drills and maintain inspection documentation to satisfy the mandate, diverting resources from actual equipment maintenance, training depth, and scenario adaptation. Staff turnover means institutional knowledge bleeds away between drills; the drills themselves test rote procedures rather than adaptive response. They administer the constraint locally (agenda_setter) but also bear its costs (payer) when budgets are exhausted on drill logistics rather than supplies. Cannot exit: failure to perform drills triggers audit and sanction.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, local_emergency_agencies, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, local_emergency_agencies, agenda_setter).

% Live in flood-risk areas and depend on community preparedness for survival. When an actual flood occurs, the response quality reflects the agencies' genuine training depth and equipment readiness — both of which atrophy under the husk constraint because resources go to compliance theater rather than substance. They cannot exit the geography. The constraint extracts from them by substituting performed preparedness for actual preparedness, leaving them exposed.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, flood_vulnerable_populations, payer,
    powerless, biographical, trapped, local).

% Carry the operational burden when floods occur. They are identity-locked to emergency work (career, professional identity, institutional loyalty); they cannot exit even when they recognize that drills are rote performance that does not prepare them for real chaos. The constraint extracts from them by performing competence ritually while actual competence (multi-scenario training, equipment fluency, cross-agency coordination) is deferred or foregone. They experience the gap between drill-stage performance and field-stage chaos acutely.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, frontline_emergency_responders, payer,
    moderate, biographical, identity_locked, local).

% Voices that would argue for performance measurement grounded in actual response outcomes, not compliance counts. They are structurally excluded from the mandate-setting process: compliance metrics (drills per year, inspections per cycle) are easier to audit and defend than outcome metrics (response times, casualty reduction). Academic research on disaster response and international disaster-management bodies offer alternative frameworks but are not empowered to set mandate terms.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, competing_institutional_narratives, excluded,
    institutional, generational, constrained, national).

% Gains legitimacy from the appearance of preparedness: can point to drill schedules, inspection reports, and compliance certifications as evidence of governance competence. When actual floods occur, political accountability is diffused (blame the agencies, blame climate change, blame the community for not heeding warnings). The constraint allows political leaders to claim preparedness without funding the actual capacity that makes preparedness real.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, political_leadership, beneficiary,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, regulatory_bureaucracy).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates community safety through systematic periodic testing and inspection of flood-response infrastructure and personnel. Creates a shared framework (schedule, standards, documentation) that allows distributed agencies to act in alignment.
% TRANSFER_FUNCTION: Transfers resources (staff time, equipment budget, interagency attention) from actual competence-building (depth training, equipment maintenance, scenario adaptation) to compliance performance (drill staging, inspection documentation, certification logistics). The extracted resources flow to administrative satisfaction rather than operational capability.
% ABSENT_VOICES: Flood-affected communities at the end of a failed response; emergency responders who recognize the gap between drill fidelity and real chaos; researchers in disaster management who document low correlation between drill compliance and response efficacy; international disaster-management bodies with outcome-focused frameworks.
% DISAPPEARANCE_RATIONALE: If drills and mandates vanished overnight, agencies would reallocate resources from compliance staging to operational depth; actual training scenarios would replace checkbox drills; equipment would be maintained rather than staged for inspection; responder turnover would trigger systematic knowledge transfer rather than rote repetition. The institutional framing of preparedness would collapse — political leaders would lose their compliance-based legitimacy claim, but response capacity might actually improve.
% FOUNDING_PROBLEM: Early disasters revealed catastrophic coordination failures: agencies acted in isolation, used incompatible equipment, had no shared procedures, and lacked basic interagency communication. Drills and inspections were built to solve this: create a common framework and prove periodically that it works.
% FOUNDING_PROBLEM_CORROBORATION: The regulatory bureaucracy attests the problem is live: drills prove coordination exists. Disaster researchers and flood-affected communities attest the founding coordination problem is substantially solved (modern communication, legal mandates, standing agencies) but the husk constraint persists as bureaucratic theater; responders testify that real coordination failures in actual floods trace to resource depletion and turnover, not coordination ignorance — the discovered failure mode is different from the founding problem.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio (0.78) is high because the constraint's entire persistence mechanism is institutional performance: drills are scheduled, inspections are conducted, documentation is certified — none of these activities meaningfully test or build the adaptive capacity that real floods demand. Extractiveness (0.62) is moderate-high because the constraint siphons resources from actual competence-building to compliance logistics. Suppression (0.71) is substantial because agencies cannot exit (mandated, audited, sanctioned); responders cannot exit (professional identity, institutional loyalty); vulnerable populations cannot exit (geography, no alternative protection). The husk reading is a Piton because: (1) beneficiaries (bureaucracy, institutional legitimacy) do not actively profit from the constraint — they inherit it and maintain it theatrically; (2) victims (responders, vulnerable people) are diffusely exposed rather than concentrated, so their combined resistance cannot dislodge the arrangement; (3) the administrative apparatus could change the mandate but the cost of fixing it (admitting preparedness is ritualized, restructuring around actual outcomes) exceeds any single administrator's pain, so it persists through inertia. Accessibility collapse (0.45 at start) is moderate because individual responders and communities CAN theoretically advocate for outcome-focused metrics, but the structural collapse (0.82-0.85) is high because the institutional framework (statutory mandates, compliance metrics, audit procedures) makes the husk narrative self-reinforcing — agencies prove compliance by showing drills, not by showing real response capacity. Resistance (0.52-0.59 at organizational and class levels) is modest and declining because the constraint has become internalized as normal institutional life; agencies design operations around mandate satisfaction, responders train to pass drills, communities expect drills as the marker of preparedness.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory bureaucracy and political leadership see a Rope or even a Mountain (preparedness is a natural fact of governance; drills prove it exists). The local agencies and responders see a Snare or Tangled Rope (they are coerced into compliance that doesn't produce the coordination benefit it nominally provides). The vulnerable populations see pure extraction (they depend on actual capacity but receive only the appearance of it). The engine computes per-seat types from this structural divergence: from the agenda-setter seat (bureaucracy) it may register as coordination met by enforcement; from the victim seat (responders, vulnerable people) it computes as pure extraction or attenuated benefit. The husk_reading does NOT try to average these — it stakes the structural claim that beneficiaries exist (bureaucracy collects legitimacy) but they are not concentrated enough to maintain the constraint themselves (no profit motive), and victims exist (responders, vulnerable people) but are too diffuse or identity-locked to dislodge it. This is the diagnostic signature of a Piton: what persists is mostly performance, and no actor has sufficient incentive to either maintain it actively or fix it decisively.
 *
 * DIRECTIONALITY LOGIC:
 *   The regulatory bureaucracy sits near the beneficiary end (d ~0.1-0.25): it sets the mandate, collects institutional legitimacy as evidence of governance, faces no operational sanction for response failure. Local agencies sit near the target end (d ~0.7-0.85): they pay resource costs, face audit sanctions, cannot exit. Vulnerable populations sit at full target (d ~0.95): they depend on the response, gain nothing from performed preparedness, bear the loss when actual capacity fails. Responders sit near-target but identity-locked (d ~0.75-0.85): they bear costs, cannot exit due to professional identity-fusion, experience the gap acutely. Political leadership sits near-beneficiary (d ~0.2-0.35): gains legitimacy, low accountability. The directionality structure is asymmetric: beneficiary seat (bureaucracy) has institutional power and analytical exit (can shift policy discourse); target seats (agencies, responders, vulnerable people) have moderate to powerless power and constrained/identity_locked exit. This asymmetry drives the extraction: the beneficiary can maintain the mandate because the costs are diffuse and the victims are disconnected.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: early disasters showed coordination failures that drills could address. The coordination function (shared framework, periodic practice) was real and newly valuable. Over time, the coordination problem has mostly been solved through legal infrastructure, standing agencies, and communication technology. What persists is the institutional form — drills continue to be mandated and performed, agencies continue to allocate resources to compliance, but the marginal value of the next drill to actual emergency response is now near zero or negative (drills consume resources that could go to depth training). The mandate has outlived its function. This is the classic mandatrophy signature: the institutional arrangement persists by inertia (it is harder to change the mandate than to continue it), and what sustains it is not the coordination benefit but the institutional legitimacy (agencies can point to drills as evidence of preparedness, political leaders can claim governance competence, the bureaucracy can claim it is managing risk). The husk_reading slots this as Piton: a former coordination mechanism that has atrophied into theater. The hybrid_reading would argue that SOME components of preparedness remain competent (e.g., structural engineering inspection) while others are ritualized (evacuation drills) — a more mixed picture. The competence_reading would argue that drills ARE live exercised knowledge and that responder turnover is addressed through them — a complete rejection of the husk_reading's core premise. The three readings occupy different seats in the same institutional debate and produce different constraint classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_competence_boundary,
    'At what point does a drill shift from being live exercised knowledge (building adaptive capacity) to being performed compliance (demonstrating existing procedures)?',
    'Observational study: compare agencies with high drill compliance and low response-outcome scores to agencies with lower compliance and higher outcomes; measure training transfer via responder interviews and scenario tests post-drill; track equipment functional status vs. inspection status.',
    'If performance and competence are empirically separable, the husk_reading is vindicated: drills can be performed well while competence atrophies. If they are entangled (good performance indicates latent competence), the competence_reading gains ground. If they are stratified (some drills build competence, others are pure theater), the hybrid_reading wins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_competence_boundary, empirical, 'Whether preparedness drills are mechanisms for building adaptive capacity or for performing existing procedures.').

omega_variable(
    mandate_origin_vs_current_function,
    'Is the founding coordination problem (early disasters revealed agency isolation and incompatible procedures) still live, or has it been substantially solved by legal infrastructure and standing agencies?',
    'Literature review of post-mandate disaster outcomes; interviews with responders and agencies on what coordination challenges remain unsolved; comparison of coordination failure rates before and after mandate implementation.',
    'If the founding problem is dead and the constraint persists anyway, that is mandatrophy in its clearest form — the constraint''s function has been accomplished and hollowed out. If the founding problem is still live, the constraint may be a genuine rope. If the founding problem is contested, the three readings remain live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_origin_vs_current_function, empirical, 'Whether the institutional mandate addresses a live coordination problem or has become theater.').

omega_variable(
    responder_identity_lock_depth,
    'How deeply are frontline responders identity-locked to emergency work and preparedness participation? Would they exit (career change, reduced compliance) if they could, or do they actively endorse preparedness work?',
    'Anonymous survey of responders on perceived gap between drill competence and real-flood chaos; longitudinal tracking of responder retention rates; interviews on exit barriers (professional identity, economic dependence, social status, loyalty).',
    'If identity-lock is strong and responders perceive a competence gap, suppression is internalized and the constraint''s effective extraction is higher than the structural measure suggests. If responders are intrinsically motivated and believe drills ARE adequate, the constraint''s extraction is lower and may be coordination, not capture. The suppression measurement itself would be reinterpreted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responder_identity_lock_depth, empirical, 'Whether responder participation in preparedness is intrinsically motivated or identity-locked and coercive.').

omega_variable(
    legitimacy_asymmetry_and_capture,
    'Who benefits from the institutional legitimacy the drills and inspections provide, and do they have the power to maintain the mandate even if outcomes decline?',
    'Analysis of mandate persistence through policy cycle: does compliance slip when outcomes are poor (adaptive governance) or does compliance remain high (institutional theater)? Do agencies with failed response outcomes face mandate changes, or do they face audit pressure to report better compliance?',
    'If institutional legitimacy is the primary beneficiary and bureaucracies maintain the mandate regardless of outcomes, the constraint is captured by the legitimacy-seeking motive and is pure extraction. If outcomes drive mandate revision, the constraint is responsive feedback and may be Rope. If some jurisdictions decouple and others maintain, the constraint is contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_asymmetry_and_capture, conceptual, 'Whether institutional legitimacy is the true beneficiary of the preparedness mandate, and whether this creates lock-in against outcome-focused revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__husk_reading, theater_ratio, 5, 0.69).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__husk_reading, theater_ratio, 10, 0.72).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__husk_reading, theater_ratio, 15, 0.75).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__husk_reading, theater_ratio, 20, 0.77).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__husk_reading, theater_ratio, 25, 0.78).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__husk_reading, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__husk_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__husk_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__husk_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__husk_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__husk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(prep_su_t5, preparedness_persistence__husk_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__husk_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(prep_su_t15, preparedness_persistence__husk_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__husk_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(prep_su_t25, preparedness_persistence__husk_reading, suppression_requirement, 25, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(prep_grid_01, preparedness_persistence__husk_reading, accessibility_collapse(class), 0, 0.28).
narrative_ontology:measurement(prep_grid_02, preparedness_persistence__husk_reading, accessibility_collapse(class), 25, 0.31).
narrative_ontology:measurement(prep_grid_03, preparedness_persistence__husk_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(prep_grid_04, preparedness_persistence__husk_reading, accessibility_collapse(individual), 25, 0.42).
narrative_ontology:measurement(prep_grid_05, preparedness_persistence__husk_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(prep_grid_06, preparedness_persistence__husk_reading, accessibility_collapse(organizational), 25, 0.72).
narrative_ontology:measurement(prep_grid_07, preparedness_persistence__husk_reading, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement(prep_grid_08, preparedness_persistence__husk_reading, accessibility_collapse(structural), 25, 0.85).
narrative_ontology:measurement(prep_grid_09, preparedness_persistence__husk_reading, resistance(class), 0, 0.62).
narrative_ontology:measurement(prep_grid_10, preparedness_persistence__husk_reading, resistance(class), 25, 0.59).
narrative_ontology:measurement(prep_grid_11, preparedness_persistence__husk_reading, resistance(individual), 0, 0.38).
narrative_ontology:measurement(prep_grid_12, preparedness_persistence__husk_reading, resistance(individual), 25, 0.35).
narrative_ontology:measurement(prep_grid_13, preparedness_persistence__husk_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(prep_grid_14, preparedness_persistence__husk_reading, resistance(organizational), 25, 0.54).
narrative_ontology:measurement(prep_grid_15, preparedness_persistence__husk_reading, resistance(structural), 0, 0.28).
narrative_ontology:measurement(prep_grid_16, preparedness_persistence__husk_reading, resistance(structural), 25, 0.25).
narrative_ontology:measurement(prep_grid_17, preparedness_persistence__husk_reading, stakes_inflation(class), 0, 0.65).
narrative_ontology:measurement(prep_grid_18, preparedness_persistence__husk_reading, stakes_inflation(class), 25, 0.71).
narrative_ontology:measurement(prep_grid_19, preparedness_persistence__husk_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(prep_grid_20, preparedness_persistence__husk_reading, stakes_inflation(individual), 25, 0.48).
narrative_ontology:measurement(prep_grid_21, preparedness_persistence__husk_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(prep_grid_22, preparedness_persistence__husk_reading, stakes_inflation(organizational), 25, 0.64).
narrative_ontology:measurement(prep_grid_23, preparedness_persistence__husk_reading, stakes_inflation(structural), 0, 0.48).
narrative_ontology:measurement(prep_grid_24, preparedness_persistence__husk_reading, stakes_inflation(structural), 25, 0.52).
narrative_ontology:measurement(prep_grid_25, preparedness_persistence__husk_reading, suppression(class), 0, 0.45).
narrative_ontology:measurement(prep_grid_26, preparedness_persistence__husk_reading, suppression(class), 25, 0.48).
narrative_ontology:measurement(prep_grid_27, preparedness_persistence__husk_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(prep_grid_28, preparedness_persistence__husk_reading, suppression(individual), 25, 0.58).
narrative_ontology:measurement(prep_grid_29, preparedness_persistence__husk_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(prep_grid_30, preparedness_persistence__husk_reading, suppression(organizational), 25, 0.75).
narrative_ontology:measurement(prep_grid_31, preparedness_persistence__husk_reading, suppression(structural), 0, 0.88).
narrative_ontology:measurement(prep_grid_32, preparedness_persistence__husk_reading, suppression(structural), 25, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three constraints, each a different reading of what preparedness means and whether the mandate produces it. The husk_reading (this constraint) asserts drills are memorial performance; the competence_reading asserts they are live practice; the hybrid_reading asserts both are partially true across different components. The readings occupy different seats in institutional discourse and produce different ε values and type classifications. They are linked by network.affects_constraints because they share the same contested kernel and each reading's adoption influences the legitimacy conditions of the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__husk_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
