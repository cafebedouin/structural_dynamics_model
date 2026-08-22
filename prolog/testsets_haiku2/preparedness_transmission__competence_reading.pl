% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission via Competence Validation (Drills and Inspections as Live Knowledge)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint is ONE READING of a contested kernel about preparedness
 *   transmission. The COMPETENCE READING instantiates the case where drills
 *   and inspections successfully transmit and re-validate live operational
 *   knowledge — responders recognize novel hazard signatures, improve under
 *   scenario variation, and the system sustains adaptive capacity across
 *   generational transitions. This reading competes with the HUSK READING
 *   (memorial ritual persisting while operational knowledge hollows out) and
 *   the HYBRID READING (physical infrastructure competence remains high but
 *   civilian coordination knowledge decays). The three readings share the
 *   same kernel (what preparedness transmission is and how it works) but
 *   differ in their empirical claims about whether and how knowledge is
 *   actually being renewed. This story authors the competence reading only,
 *   as a clean ε-invariant constraint with its own beneficiary structure,
 *   directionality, and metrics.
 *
 * KEY AGENTS:
 *   - Emergency Management Authority — agenda-setter; designs, schedules, and evaluates drills; maintains visible competence
 *   - Trained Responders — dual beneficiary/payer; build skills through drill participation; gain confidence; competence certification is career-dependent
 *   - Population Protected by Response Capacity — powerless beneficiaries; depend on responder competence during actual events; cannot exit
 *   - Fiscal Oversight Bodies — payers; allocate recurring drill budgets; can reallocate if not convinced of capability gains
 *   - Inspecting Auditors — beneficiaries; evaluate performance and detect novel failure signatures; distinguish live competence from script-following
 *   - Scenario Design Specialists — beneficiaries; create novel scenarios that force improvisational thinking; validate that knowledge is live
 *   - Institutional Memory Keepers — observers; carry forward lessons from past disasters; prevent ritual hollowing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.28).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.15).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission via Competence Validation (Drills and Inspections as Live Knowledge)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, 'e5caaff0-4b87-4fa0-aab6-c0cb534abd18').
narrative_ontology:cs_kernel_codification('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', distributed).
narrative_ontology:cs_authority_grounding('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', expertise).
narrative_ontology:cs_interpretation_layer_present('e5caaff0-4b87-4fa0-aab6-c0cb534abd18').
narrative_ontology:cs_reading_relation('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', foundational, drill_scenario_novelty_sustains_adaptive_capacity).
narrative_ontology:cs_axiom_status(drill_scenario_novelty_sustains_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', drill_scenario_novelty_sustains_adaptive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', foundational, inspectors_detect_live_vs_scripted_knowledge).
narrative_ontology:cs_axiom_status(inspectors_detect_live_vs_scripted_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', inspectors_detect_live_vs_scripted_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', generational_knowledge_transmission_via_live_practice).
narrative_ontology:cs_drift_state('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', contemporary_budget_pressure_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e5caaff0-4b87-4fa0-aab6-c0cb534abd18', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, population_protected_by_response_capacity).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, administrative_authority_maintaining_readiness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, trained_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, inspecting_auditors).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, scenario_design_specialists).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, trained_responders).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, fiscal_oversight_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, schedules, and evaluates drills and inspections. Sets the protocols for scenario design, performance metrics, and remediation requirements. Bears responsibility for public safety outcomes and faces legislative/constituent pressure when preparedness fails. Must continuously justify the resource allocation to drills and maintain visible institutional competence.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_management_authority, agenda_setter,
    institutional, generational, constrained, national).

% Participate in drills and inspections as part of operational duty. Build and maintain skills through repeated scenario rehearsal. Gain confidence in their own decision-making under pressure and discover gaps in capability before real events. The repeated exercises are their primary mechanism for staying current with evolving hazards and organizational procedures. Exit is difficult — career depends on competence certification.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, trained_responders, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, trained_responders, payer).

% Depends entirely on the competence of the response system during actual disasters. The drills that validate responder capability are not directly visible to them but their outcomes (faster response, fewer coordination failures, better triage) affect survival outcomes. They cannot exit the relationship; they are the intended beneficiary of the constraint's operation.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, population_protected_by_response_capacity, beneficiary,
    powerless, immediate, trapped, regional).

% Allocate recurring budget for drill design, participation, and evaluation. These costs are substantial and compete with other budget priorities. They can reallocate funds if convinced that drills are not producing real capability improvement. Their scrutiny creates pressure to demonstrate tangible competence gains, not mere compliance with drill schedules.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, fiscal_oversight_bodies, payer,
    organized, biographical, mobile, national).

% Conduct performance evaluations during and after drills, looking for novel failure signatures, adaptive reasoning under uncertainty, and situational improvisation. Their expertise grows as they see more scenario variations and learn to distinguish dead performance from live competence. They occupy the observation and evaluation seat that validates whether knowledge is actually being transmitted and renewed, not merely performed.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inspecting_auditors, beneficiary,
    moderate, biographical, constrained, national).

% Design realistic, challenging scenarios that force improvisational thinking rather than script-following. Success is measured by whether responders encounter genuinely novel decision points in drills. They must stay current with emerging hazard signatures, changing organizational structures, and past failure modes to create scenarios that test live knowledge rather than rehearsal.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, scenario_design_specialists, beneficiary,
    moderate, biographical, constrained, regional).

% Long-serving personnel, historians, or institutional archives that carry forward lessons from past disasters and near-misses. They provide the baseline against which each drill can be evaluated for novelty and improvement. Their role is to prevent the constraint from degrading into mere theater — they serve as the epistemological check against hollowing out.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, institutional_memory_keepers, observer,
    moderate, generational, constrained, regional).

% Could mandate different preparation standards, fund or defund drills, or alter the institutional mandate for emergency management. They are kept outside the drill-evaluation process itself; their role comes only after a major failure produces political pressure. If they were inside the drill design, the scenario construction might be biased toward politically popular outcomes rather than realistic capability validation.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, legislative_bodies, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, emergency_management_authority).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the temporal knowledge problem: each generation of responders must re-validate their capability to recognize and respond to novel hazards rather than relying on inherited doctrine that may no longer fit current conditions. Drills force re-learning, not rote compliance, and inspections measure whether knowledge is actually live and adaptive, not just performed.
% TRANSFER_FUNCTION: Moves the responsibility for competence maintenance from a few senior figures (who might die or leave) to the whole response system, distributed across training cycles, drill participation, and inspector evaluation. The transfer is from institutional seniority to organizational capability; the mechanism is repeated, scenario-varied exercise that forces the system to stay current.
% ABSENT_VOICES: Politicians who might demand drills tailored to politically visible scenarios rather than realistic ones; disaster survivors whose experiences could inform more adaptive scenario design but are rarely integrated into the drill system; communities at highest risk from specific hazard types who might demand scenario investment tailored to their actual exposure rather than generic 'all-hazards' approaches.
% DISAPPEARANCE_RATIONALE: If drills and inspections ceased, the response system would initially appear unchanged — the same personnel, procedures, and institutional structures would persist. But within one generation (20–30 years), the competence would hollow out: responders would rely on inherited procedures that no longer match current hazard profiles; novel failure signatures would go unrecognized during actual events; improvisation capability would decay as untested decision-makers default to scripted responses. A real disaster 15 years after drill cessation would reveal the degradation. The world rearranges because the constraint prevents this decay — its disappearance allows it to happen.
% FOUNDING_PROBLEM: Disaster response capability must be renewed with every generation of responders because hazard environments shift (climate, urbanization, infrastructure change), organizational structures evolve, and the tacit knowledge of senior personnel is lost when they retire or die. Without forced re-validation, each organization drifts toward documentary knowledge (written procedures) disconnected from actual capability, and learns this gap only during a real disaster.
% FOUNDING_PROBLEM_CORROBORATION: Post-disaster reviews consistently identify responder capability gaps and coordination failures that drills-with-novel-scenarios could have surfaced earlier. Institutional memory keepers and auditors attest that generation-to-generation competence loss is observable when drills become routine and scenario variation declines. Independent disaster research documents capability loss in organizations that reduce drill frequency or move to pure documentation-based training.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under the COMPETENCE READING, extractiveness is LOW (0.28) because the constraint operates as genuine coordination: drills solve a real collective-action problem (maintaining knowledge across generations) that participants willingly undertake as part of professional duty. Suppression is very low (0.15) because responders participate because they recognize the capability gain, not because they are coerced. Theater ratio is LOW (0.12) because scenario variation forces real problem-solving rather than rehearsal — inspectors see genuine improvisational reasoning under pressure, not role-playing compliance. The measurement series shows extractiveness rising slightly and stabilizing around 0.28 as fiscal pressure increases (budget scrutiny forces justification of drill costs) and some routinization creeps in, but stays well within rope territory because the core competence-validation function persists. Theater remains stable at 0.12, indicating that the constraint has not degraded into performative ritual. Suppression stays low because participation is sustained by professional recognition of capability value, not external coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   The Emergency Management Authority is the beneficiary-with-power (sets the agenda, controls evaluation standards, faces accountability for response outcomes). Trained responders sit near symmetric with a tilt toward benefit: they gain career advancement and genuine capability from drill participation, but also bear the cost of time and psychological strain; exit is constrained by career dependence. Fiscal oversight bodies are payers (allocate budget) but mobile (can reallocate). The population is a powerless beneficiary (gains protection but cannot exit). Inspecting auditors and scenario designers are beneficiaries (their professional expertise is valued and their work shapes organizational capability). Under this reading, no agent bears high directed extraction costs because the constraint solves a problem all parties recognize — the directionality derives from the beneficiary/victim declarations showing coordination alignment, not asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE: the need to renew preparedness knowledge across generational transitions remains real and pressing. The disappearance verdict is WORLD_REARRANGES: if drills ceased, the response system would degrade slowly (over 15–30 years) as knowledge decayed and novel hazards went unrecognized. The constraint carries no evidence of mandatrophy under the COMPETENCE READING — the founding problem has not outlived its function. However, under the HUSK READING, the same constraint would show signs of mandatrophy: the founding problem would be declared DEAD (knowledge is already hollowed out; drills are memorial ritual) while the world_rearranges verdict persists (the organizational structure and political commitment to drills have inertia). The mandatrophy analysis here is that the readings diverge precisely on this question: whether the constraint is solving a live problem or maintaining a zombie function. The narrative evidence (budget pressure, routine drift, declining scenario novelty) could support either reading; that ambiguity is captured in the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ritual_boundary,
    'Are drills and inspections actually producing live, adaptive competence that differs from rote compliance, or has the organizational knowledge hollowed out behind continued performance of the same activities?',
    'Compare pre- vs. post-drill performance metrics on novel hazard scenarios: if responders show improvisational reasoning and novel failure-mode recognition, competence is live; if they default to scripted procedures even under scenario variation, knowledge is hollowed out.',
    'If hollowed out, the constraint reclassifies from ROPE (genuine coordination) to SNARE (ritual maintenance); extracted value shifts from coordination cost to organizational legitimacy theater; extractiveness rises; theater ratio rises sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ritual_boundary, empirical, 'Whether the competence reading''s core claim (live knowledge renewal) is supported by responder performance under novel scenarios.').

omega_variable(
    stratified_competence_hypothesis,
    'Is competence uniformly sustained across all response domains, or has it decayed unevenly (e.g., high in engineering/infrastructure, low in civilian coordination)?',
    'Audit performance by functional domain: if certain roles (hazmat, structural engineering) show sustained adaptive capacity while others (emergency operations center coordination, public information) show script-dependent reasoning, competence is stratified.',
    'If stratified, the constraint operates as a HYBRID: real rope for infrastructure domains, snare-like ritual for coordination domains. Mixed classification would indicate that the constraint''s effectiveness depends on domain-specific knowledge preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratified_competence_hypothesis, empirical, 'Whether preparedness competence is uniformly sustained or unevenly distributed across response functions.').

omega_variable(
    scenario_novelty_degradation,
    'Are drill scenarios genuinely novel and forcing improvisational reasoning, or have they drifted toward routinized templates that responders can execute without real cognitive effort?',
    'Track scenario design evolution over time: measure the cognitive novelty (number of decision points that require active reasoning vs. procedural execution) and compare performance variance (high variance indicates improvisation; low variance indicates script-following).',
    'If scenarios have degraded into templates, the constraint is drifting toward HUSK READING territory: theater ratio rises sharply, extractiveness plateaus, the constraint persists as organizational memory ritual rather than live capability validation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scenario_novelty_degradation, empirical, 'Whether scenario design maintains genuine novelty and forces improvisational thinking, or has drifted toward routinized templates.').

omega_variable(
    reading_foreclosure_via_empirical_accumulation,
    'As data accumulates about whether drills are producing genuine competence improvement, will the empirical evidence foreclose one or more of the three readings, or will the readings remain coexistent because they rest on irreducible value disagreements about what constitutes ''real preparedness''?',
    'Observe whether post-disaster after-action reviews cite drill-identified failures as evidence for (competence reading) or failures missed by drills as evidence against (husk/hybrid readings). If evidence accumulates unambiguously, one reading forecloses the others; if post-disaster findings remain contested despite evidence, readings remain coexistent.',
    'If a reading is empirically foreclosed, the constraint family reduces in size and the certification status of non-foreclosed readings changes. If readings remain coexistent despite evidence, they rest on conceptual disagreements that empirical data cannot resolve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_empirical_accumulation, conceptual, 'Whether the three readings of preparedness transmission can be empirically distinguished or remain coexistent despite evidence accumulation.').

omega_variable(
    institutional_memory_keeper_role_robustness,
    'How dependent is the COMPETENCE READING on the presence of institutional memory keepers (long-serving personnel, historians, archives) who can recognize decay when it happens? Does the constraint self-stabilize, or does it require external epistemological guardrails?',
    'Observe organizations that lose institutional memory keepers (due to retirement, budget cuts, archival loss) and compare the trajectory of scenario design novelty and post-drill assessments. High dependence would suggest the constraint is fragile without these roles.',
    'If the competence reading depends critically on institutional memory keepers, the constraint is more vulnerable to organizational churn; loss of these roles would enable drift toward HUSK READING. High dependence would suggest that the constraint is not self-sustaining coordination but requires maintained epistemological infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_memory_keeper_role_robustness, empirical, 'Whether the competence-reading''s sustainability depends on institutional roles that carry forward past lessons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prep_tr_t5, preparedness_transmission__competence_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__competence_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(prep_tr_t15, preparedness_transmission__competence_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(prep_tr_t25, preparedness_transmission__competence_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__competence_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__competence_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_be_t5, preparedness_transmission__competence_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__competence_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(prep_be_t15, preparedness_transmission__competence_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(prep_be_t25, preparedness_transmission__competence_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__competence_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__competence_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(prep_su_t5, preparedness_transmission__competence_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__competence_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(prep_su_t15, preparedness_transmission__competence_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__competence_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(prep_su_t25, preparedness_transmission__competence_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__competence_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__competence_reading, suppression_requirement, 40, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% Preparedness transmission is a contested kernel instantiated by three structurally distinct readings: COMPETENCE (live knowledge renewal via scenario novelty), HUSK (organizational ritual with hollowed knowledge), and HYBRID (stratified competence by domain). All three share the kernel commitment that preparedness knowledge must be renewed across generations but diverge on empirical claims about whether the re-validation mechanism works. This story authors COMPETENCE only, as a clean ε-invariant constraint. The HUSK and HYBRID readings are separate constraint stories in the family. Sibling linkage enables the engine to track which reading's predictions are falsified or supported as evidence accumulates (post-disaster reviews, scenario design audits, organizational performance metrics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
