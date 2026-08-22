% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This is the HUSK READING of the contested preparedness_retention kernel.
 *   It asserts that disaster preparedness in modern governance has become
 *   largely memorial performance—drills and inspections are rituals that
 *   satisfy institutional accountability requirements and feel like retention
 *   but lack genuine live competence. Resources flow toward visible
 *   compliance (checklist completion, documented exercises, audit
 *   satisfaction) and away from tacit skill-building (high-stress scenario
 *   exposure, deliberate practice, adaptive learning). The constraint's
 *   beneficiary is institutional legitimacy narrative; its victim is actual
 *   response capacity during real D5 events. The husk reading competes with
 *   the competence reading (which treats drills as genuine
 *   competence-building) and the hybrid reading (which treats technical
 *   agencies as competent while broader governance becomes ceremonial). This
 *   story instantiates only the husk reading; the sibling readings are
 *   separate constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Institutional legitimacy apparatus: the audit, compliance, and accountability structures that benefit from the appearance of preparedness
 *   - Compliance administrators: emergency management offices and audit teams that design drills and verify documentation
 *   - Responder personnel: firefighters, EMTs, civil defense coordinators who participate in drills and respond to actual events
 *   - Technical specialists: disaster response experts excluded from the design of preparedness regimes
 *   - Community stakeholders: residents whose actual safety depends on responder competence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.71).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.62).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'ac4cfc92-7475-483b-b28c-84a9569404e5').
narrative_ontology:cs_kernel_codification('ac4cfc92-7475-483b-b28c-84a9569404e5', implicit).
narrative_ontology:cs_authority_grounding('ac4cfc92-7475-483b-b28c-84a9569404e5', extraction).
narrative_ontology:cs_interpretation_layer_present('ac4cfc92-7475-483b-b28c-84a9569404e5').
narrative_ontology:cs_reading_relation('ac4cfc92-7475-483b-b28c-84a9569404e5', preparedness_retention__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('ac4cfc92-7475-483b-b28c-84a9569404e5', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('ac4cfc92-7475-483b-b28c-84a9569404e5', foundational, drills_lack_live_competence_transfer).
narrative_ontology:cs_axiom_status(drills_lack_live_competence_transfer, holdable).
narrative_ontology:cs_axiom_grounding('ac4cfc92-7475-483b-b28c-84a9569404e5', drills_lack_live_competence_transfer, empirically_contingent).
narrative_ontology:cs_axiom('ac4cfc92-7475-483b-b28c-84a9569404e5', secondary, compliance_theater_erodes_response_capacity).
narrative_ontology:cs_axiom_status(compliance_theater_erodes_response_capacity, holdable).
narrative_ontology:cs_axiom_grounding('ac4cfc92-7475-483b-b28c-84a9569404e5', compliance_theater_erodes_response_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('ac4cfc92-7475-483b-b28c-84a9569404e5', genuinely_prepared_society).
narrative_ontology:cs_drift_state('ac4cfc92-7475-483b-b28c-84a9569404e5', contemporary_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac4cfc92-7475-483b-b28c-84a9569404e5', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, institutional_legitimacy_apparatus).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, compliance_administrators).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, actual_response_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, responder_personnel).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, community_stakeholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The set of governance structures, audit requirements, and accountability frameworks that derive legitimacy from the APPEARANCE of preparedness. Drills satisfy inspection schedules, compliance checklists are marked complete, officials can attest 'preparedness protocols were exercised.' The institution's standing depends on visible compliance, not on whether responders can actually execute under pressure.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, institutional_legitimacy_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Emergency management offices, audit teams, and compliance coordinators who design and run drills, maintain checklists, and verify that protocols exist and were 'tested.' They are assessed on whether procedures were followed and documented, not on whether a real event would succeed. Resources they control (training budgets, drill scheduling, equipment maintenance) flow to documented compliance over tacit skill building.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, compliance_administrators, agenda_setter,
    organized, biographical, mobile, national).

% Firefighters, emergency medical technicians, civil defense coordinators, and front-line disaster responders. They participate in mandated drills (which consume hours) that are designed to be completable in simulation, not to build genuine expertise under stress. They understand the gap between drill conditions and real conditions but have limited power to redirect resources to meaningful training.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, responder_personnel, payer,
    moderate, biographical, constrained, local).

% Residents and vulnerable populations who depend on actual emergency response competence. They have no seat at the compliance table. Their safety depends on responder skill and institutional capacity, which the husk constraint systematically degrades in favor of documentary performance.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, community_stakeholders, payer,
    powerless, immediate, trapped, local).

% Legislative bodies, audit offices, and higher-level authorities that review preparedness status. They see compliance reports and drill certifications; they have limited visibility into whether the reported preparedness translates to actual capacity. They can mandate more drills but face institutional inertia against redirecting resources from visible compliance to invisible competence.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, oversight_bodies, observer,
    institutional, generational, analytical, national).

% Experts in disaster response, organizational learning, and high-reliability operations who understand the distinction between theater and competence. They are largely excluded from the design of preparedness regimes; their input would highlight the gap and threaten the legitimacy narrative. Where they do participate (specialized technical agencies), they operate in a different stream from the general compliance apparatus.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, technical_specialists, excluded,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, institutional_legitimacy_apparatus).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates accountability structures: creates a shared standard (drills, inspections, checklists) that allows multiple agencies and levels of government to attest that 'preparedness happened.' Solves a genuine collective-action problem: without documented protocols and exercises, no entity can prove it is prepared, and blame is diffuse when disaster strikes.
% TRANSFER_FUNCTION: Transfers resources (time, budget, organizational attention) from competence-building (deliberate practice, scenario exposure, skill refinement) to compliance-demonstration (drill execution, checklist completion, documentation). The transfer flows away from actual response capacity and toward institutional legitimacy narrative.
% ABSENT_VOICES: Technical specialists in disaster preparedness and organizational learning are systematically excluded from the design of preparedness regimes; their input would call attention to the theater-competence gap and jeopardize the compliance narrative. Responders themselves are present but constrained—they participate in drills as directed, not as designers. Community stakeholders have no voice in preparedness governance at all.
% DISAPPEARANCE_RATIONALE: If the husk constraint (the mandate to demonstrate preparedness through compliance ritual) disappeared, resource allocation would shift: drills would become optional and designed for learning rather than certification; training budgets would flow to high-stress scenario exposure instead of low-fidelity exercises; responders would spend more time on tacit skill development and less on checklist completion. The institutional accountability structure would be forced to defend itself on different grounds—actual capacity measures rather than documentary compliance.
% FOUNDING_PROBLEM: After catastrophic disasters where official preparedness appeared adequate on paper but response failed operationally, inquiry bodies asked: how can we ensure this does not happen again? The answer was to mandate visible, auditable preparedness: documented protocols, scheduled drills, inspection regimes. The founding problem was institutional accountability and public confidence.
% FOUNDING_PROBLEM_CORROBORATION: Institutional and government bodies attest the founding problem is still live and that drills and inspections solve it. Technical researchers in disaster preparedness and organizational learning (from outside the compliance apparatus) attest that the founding problem was real but that the chosen solution—compliance theater—does not address it and may make actual response capacity worse by consuming the time and budget that genuine skill-building requires. Post-disaster inquiries consistently find that disasters strike agencies that had current certifications and completed recent drills, suggesting the founding accountability problem has been solved (no more surprise unpreparedness) but the real problem (response competence) remains unaddressed.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The theater_ratio measurement series (0.58→0.78 over the interval) is the signature of a Piton constraint: a high and rising proportion of the constraint's activity is performative rather than functionally essential. A drill designed to be completable by the book and passed by observers is theater; a high-stress scenario that exposes gaps and teaches adaptation is functional. As compliance requirements become more extensive and documentation more rigorous, the functional proportion shrinks. Extractiveness rises (0.52→0.71) because resources consumed in compliance drills are unavailable for competence-building; this extraction is from actual response capacity (a non-agent but the outcome-measure). Suppression requirement is moderate and stable (0.45→0.62) because the constraint's persistence depends less on external coercion than on institutional inertia and internalized belief that drills satisfy preparedness. Resistance is moderate (0.55) because responders and some technical voices push back, but the leverage of compliance-administrators and institutional-legitimacy-apparatus is stronger. The measurement series share one time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   Institutional legitimacy apparatus and compliance administrators perceive the constraint as genuine coordination (solving accountability and public confidence problems) and genuine competence-building (drills develop readiness). From their position, the constraint is a Rope or Tangled Rope. Responders and technical specialists perceive it as theater that consumes resources without building real competence. From their position, it is a Piton or Snare. The engine computes this divergence from the structural data: beneficiary seats (institutional legitimacy, compliance administrators) derive d closer to 0.0; payer seats (responders, community) derive d closer to 1.0. The claimed type (Piton) reflects the husk reading's assertion that the constraint's primary function (accountability) has been preserved but the secondary function (competence-building) has atrophied—inertial institutional persistence in the absence of concentrated benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional legitimacy apparatus benefits from the constraint—it derives standing from the appearance of preparedness. Compliance administrators benefit from the resources and authority directed to them; they face assessment on whether procedures were followed, not on whether disaster response succeeds. From their seats, d is near the beneficiary end. Responders and community stakeholders bear costs without collecting: they invest time in low-fidelity drills rather than high-fidelity training, and they face the consequences when actual capacity fails. From their seats, d is near the target end. The constraint's persistence depends on suppression (audit requirements, mandatory drills, compliance cycles), but this suppression is lighter than it would be in a pure snare because significant internalizing has occurred—compliance administrators and even some responders have come to believe that drills are competence-building, which reduces the need for external enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: after disasters, inquiry commissions found that agencies had adequate written protocols but failed operationally, creating accountability crises. The chosen solution was to mandate visible preparedness: drills, inspections, documentation. This solved the accountability problem partially (officials can attest procedures were tested) but did not solve the competence problem (responders still fail under real stress). Over time, compliance satisfaction became decoupled from competence measurement; the audit process itself became the proof of preparedness. The function of the constraint has shifted: it no longer primarily builds competence (if it ever did) and now primarily generates institutional legitimacy narrative. The mandate has outlived its functional purpose—this is classic mandatrophy. The constraint persists not because it solves the founding problem well, but because it solves a different problem: it gives officials and auditors a way to demonstrate they 'did something' about preparedness without requiring difficult measurement of actual capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice,
    'Is preparedness actually memorial performance (husk reading) or is it live exercised competence (competence reading) or stratified between institutional actors (hybrid reading)?',
    'Post-disaster empirical audit: measure responder performance against pre-event certification status. If certified agencies fail at tasks they drilled, the husk reading is supported. If certified agencies perform well, the competence reading is supported. If performance is stratified (technical agencies perform well, generalist responders perform poorly), the hybrid reading is supported.',
    'The engine computes this reading as Piton (atrophied function, maintained theatrically). If the competence reading were correct, the classification would be Rope (genuine coordination). If the hybrid reading were correct, the classification would be mixed by institutional seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice, empirical, 'Which of the three sibling readings of the preparedness_retention kernel best describes reality.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (external pressure to run drills, audits, compliance cycles) or internalized (responders and administrators have come to believe drills ARE competence-building and do not seek alternatives)?',
    'Survey responders and administrators on: (1) whether they believe current drills build real competence, (2) what changes they would make if external compliance requirements were removed, (3) whether they have attempted to design high-fidelity training and faced resource constraints. High agreement on (1) without corresponding high performance in (2) suggests internalization.',
    'If suppression is largely internalized, the constraint''s persistence no longer requires active external enforcement—it persists through cognitive capture. Post-exit suppression trajectory (responders who leave the system) would show whether suppression is retained. This would support a Snare classification over Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in the preparedness apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__husk_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__husk_reading, theater_ratio, 10, 0.67).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__husk_reading, theater_ratio, 15, 0.71).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__husk_reading, theater_ratio, 20, 0.75).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__husk_reading, theater_ratio, 25, 0.77).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__husk_reading, theater_ratio, 30, 0.78).
narrative_ontology:measurement(prep_tr_t35, preparedness_retention__husk_reading, theater_ratio, 35, 0.78).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__husk_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__husk_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__husk_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__husk_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__husk_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__husk_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(prep_be_t35, preparedness_retention__husk_reading, base_extractiveness, 35, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__husk_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__husk_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__husk_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__husk_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__husk_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__husk_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(prep_su_t35, preparedness_retention__husk_reading, suppression_requirement, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three structurally distinct constraints because they make contradictory claims about whether certified preparedness translates to actual response capacity. The husk reading (this story) treats drills as memorial performance with low real competence benefit. The competence reading treats drills as genuine competence-building. The hybrid reading treats competence as stratified by institutional type. Each reading has its own ε (competence reading lower, husk reading higher), its own beneficiary/victim structure, and its own classification. The three stories are linked via network.affects_constraints because the underlying disagreement is about the same referent (what preparedness actually is)—if one reading is correct, it constrains which of the others can be true.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__husk_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
