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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Preparedness Drill and Inspection Husk: Form Without Competence
 *   domain: institutional/disaster_preparedness/commitment_systems
 *
 * SUMMARY:
 *   A civil protection administration maintains a ceremonial apparatus of
 *   drills and inspections that reports high readiness while actual
 *   operational competence atrophies. The drills are performed on outdated
 *   flood-risk models and procedures that have not been validated against
 *   real conditions. Equipment is maintained nominally (passing inspection)
 *   but not actively tested. New responders are trained on the procedures as
 *   doctrine and internalize false confidence. Populations at flood risk are
 *   reassured by the visible apparatus but are not protected by actual
 *   competence. This reading instantiates preparedness as a Piton: atrophied
 *   function mistaken for a Mountain (natural disaster preparedness) or a
 *   Rope (genuine coordination). The constraint persists not because
 *   participants benefit from real preparedness but because they benefit from
 *   the appearance of preparedness: administrators get legitimacy, responders
 *   get validated self-image, committees get budget justification, and the
 *   institutional narrative stays intact. When a flood comes, the deficit
 *   between ceremonial readiness and actual competence is revealed, but the
 *   apparatus survives because it serves a function independent of flood
 *   response — it serves institutional legitimacy.
 *
 * KEY AGENTS:
 *   - civil_protection_administration: Agenda-setter (institutional, constrained exit) — designs, schedules, and certifies the apparatus; no operational pressure to update
 *   - agency_responders: Mixed beneficiary/payer (moderate power, constrained exit) — benefit from validated identity and budget security; pay by learning atrophied procedures
 *   - junior_responders: Victim/payer (powerless, identity-locked) — trained on outdated procedures; false competence becomes internalized identity
 *   - populations_at_flood_risk: Victims (powerless, trapped) — misled by visible apparatus; bear actual flood risk
 *   - institutional_legitimacy_narrative: Non-agent beneficiary (analytical) — vindicated by the existence of the apparatus independent of actual competence
 *   - peer_jurisdictions: Observers (institutional, analytical) — trapped in the same ceremonial equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.58).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.42).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.76).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Preparedness Drill and Inspection Husk: Form Without Competence").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "institutional/disaster_preparedness/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, 'fb54dd7c-e232-4bd0-845d-79e412145b32').
narrative_ontology:cs_kernel_codification('fb54dd7c-e232-4bd0-845d-79e412145b32', implicit).
narrative_ontology:cs_authority_grounding('fb54dd7c-e232-4bd0-845d-79e412145b32', extraction).
narrative_ontology:cs_interpretation_layer_present('fb54dd7c-e232-4bd0-845d-79e412145b32').
narrative_ontology:cs_reading_relation('fb54dd7c-e232-4bd0-845d-79e412145b32', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb54dd7c-e232-4bd0-845d-79e412145b32', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('fb54dd7c-e232-4bd0-845d-79e412145b32', foundational, form_persists_without_function).
narrative_ontology:cs_axiom_status(form_persists_without_function, holdable).
narrative_ontology:cs_axiom_grounding('fb54dd7c-e232-4bd0-845d-79e412145b32', form_persists_without_function, empirically_contingent).
narrative_ontology:cs_axiom('fb54dd7c-e232-4bd0-845d-79e412145b32', foundational, legitimacy_via_ceremony_not_competence).
narrative_ontology:cs_axiom_status(legitimacy_via_ceremony_not_competence, holdable).
narrative_ontology:cs_axiom_grounding('fb54dd7c-e232-4bd0-845d-79e412145b32', legitimacy_via_ceremony_not_competence, instrumental).
narrative_ontology:cs_reference_frame('fb54dd7c-e232-4bd0-845d-79e412145b32', preparedness_via_continuous_practice).
narrative_ontology:cs_drift_state('fb54dd7c-e232-4bd0-845d-79e412145b32', contemporary_accelerated_climate_risk, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb54dd7c-e232-4bd0-845d-79e412145b32', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, civil_protection_administration).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, institutional_legitimacy_narrative).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, populations_at_flood_risk).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, junior_responders_trained_on_atrophied_procedures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, agency_responders).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, budget_allocation_committees).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, agency_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates and schedules drills, conducts inspections, certifies readiness. Reports compliance upward to elected officials. Maintains the ceremonial apparatus of preparedness: the calendar of exercises, the checklist regime, the annual inspection schedules. Faces no operational pressure to update the drills when real flood conditions change, because the drills serve legitimacy (are we prepared?) not competence (can we actually handle a flood). When a flood comes, the administration points to the drill record as evidence of due diligence.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, civil_protection_administration, agenda_setter,
    institutional, generational, constrained, national).

% Responders participate in scheduled drills and pass them: they practice on outdated maps, use equipment that has not actually been maintained between drills, execute procedures that were written for a different flood risk profile. The drill success validates their department's budget request and their own standing (they performed well). During actual floods, the procedures often fail because they were never tested against real conditions, only against the drill scenario. The responder is trapped: they cannot opt out of drills (it is a job requirement), and they internalize a sense of competence from drill performance that later proves false under real conditions.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, agency_responders, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, agency_responders, payer).

% New recruits learn the drill procedures as doctrine: this is how we do evacuations, this is the communication protocol, this is the shelter sequencing. They have no access to real-world flood data to test these procedures against. Their professional identity becomes bound to the drill: executing it well means they are competent. When a real flood comes and the procedures fail, they bear the operational and moral cost — they blame themselves for poor execution when the procedures themselves are the problem. The identity lock is complete: 'I am a trained responder in the system' means accepting the drill procedures as legitimate, even after they fail.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, junior_responders_trained_on_atrophied_procedures, payer,
    powerless, biographical, identity_locked, national).

% They are told the administration runs drills and passes inspections, therefore they are protected. The visibility of the drill machinery (the sirens go off, traffic stops, shelter signs are tested) creates a false sense of security. When an actual flood arrives and the response fails — evacuation routes are wrong, shelters are unprepared, communication breaks down — the population discovers the drills were theater. They are trapped both before (cannot evacuate a flood by individual choice alone) and after (cannot sue or demand change while the disaster is happening; the time for institutional learning is in the gap between floods).
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, populations_at_flood_risk, payer,
    powerless, biographical, trapped, national).

% The doctrine that 'a state that prepares is a capable state' is vindicated by the existence of the apparatus. The drill calendar, inspection checklist, and certified readiness are proof of the doctrine's truth. The narrative persists independent of whether actual competence exists — the apparatus maintains itself as evidence of the narrative.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, institutional_legitimacy_narrative, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_persistence__husk_reading, institutional_legitimacy_narrative).

% They allocate funds to disaster preparedness based on the completion of drills and inspections. The visible apparatus — audited schedules, certified staff, passed exercises — justifies budget allocations without requiring independent assessment of actual competence. The constraint allows them to claim credit for preparedness while avoiding the cost of building real competence: testing procedures against actual climate and terrain data, maintaining equipment in constant readiness, training for conditions that have changed since the last flood.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, budget_allocation_committees, beneficiary,
    institutional, generational, constrained, national).

% Alternative approaches — continuous simulation against current climate models, real-time equipment maintenance, scenario planning with actual flood-risk data — are structurally excluded from the institutional apparatus. Proposing them means challenging the legitimacy of the drill framework, which triggers resistance from the administration and budget committees. The exclusion is not violent; it is structural: the alternative framework would require admitting the current drills are inadequate, which the institution cannot afford.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, rival_preparedness_frameworks, excluded,
    moderate, generational, trapped, national).

% Other jurisdictions run similar drill and inspection regimes. The constraint is self-reinforcing across jurisdictions: each jurisdiction can point to the others' drills as evidence that the framework is standard and therefore adequate. No jurisdiction has leverage to exit unilaterally; all are trapped in the ceremonial equilibrium. Real innovations (climate-responsive planning, equipment maintenance networks, cross-jurisdiction coordination) fail to scale because they require coordinated exit from the drill framework.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, peer_jurisdictions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, civil_protection_administration).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically solved: maintaining institutional memory of flood response procedures across electoral cycles and administrative turnovers. Drills trained new cohorts, inspections kept equipment provisioned, the apparatus ensured continuity of response capability even as staff changed.
% TRANSFER_FUNCTION: Transfers resources (budget, time, attention) from populations and junior responders to the administrative apparatus that manages the drills and from actual competence-building to legitimacy-building. The flood-risk population pays by bearing undetected incompetence; junior responders pay by learning outdated procedures and internalizing false confidence.
% ABSENT_VOICES: Climate scientists and hydrologists who have updated flood-risk models are not consulted in drill design. Equipment engineers who could assess maintenance needs are not in the chain. Populations in flood zones who have experienced failed evacuations are rarely included in procedure revision. The excluded are structurally prevented from speaking: the drill framework treats preparedness as a solved problem, so contrary evidence is filtered out as noise.
% DISAPPEARANCE_RATIONALE: If the drill framework vanished, the administration would face immediate pressure to build actual competence: updating procedures against current data, testing equipment under realistic conditions, training responders on new flood scenarios. The vacuum would be filled by alternative frameworks (continuous simulation, climate-responsive planning, maintenance networks). Populations would stop relying on the visible apparatus and would begin demanding transparency about actual readiness. The institutional narrative would shift from 'we prepare ceremonially' to 'we must build real competence or admit defeat.'
% FOUNDING_PROBLEM: Early twentieth-century floods caught administrations unprepared; no institutional memory, no trained responders, no equipment caches. The drill framework was built to solve this: regular practice, staff training, equipment inspection, institutional continuity across changes in personnel.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists attest that modern flood risks are different from historical floods in magnitude and timing; historical procedures are no longer adequate. Responders who have experienced real floods attest that drills do not prepare them for actual conditions. Peer jurisdictions that have updated their frameworks (climate-responsive scenario planning, maintenance-first approaches) report higher readiness than drill-based frameworks achieve. No voice from outside the administration confirms that the founding problem (loss of institutional memory) remains live — the claim that 'we need drills to remember' is now self-serving mythology.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Theater ratio is very high (0.76) because the apparatus is mostly performative: drills are scheduled and executed, inspections are completed and recorded, reports are filed — but these activities do not substantially improve actual flood response capacity. As climate models change and flood risks shift, the procedures become increasingly disconnected from reality, yet the drilling apparatus persists unchanged. The theater rises as the gap widens between what the drills claim to test and what actual conditions would demand. Base extractiveness (0.58) is moderate-high because the apparatus extracts resources (time, budget, personnel attention) from actual competence-building and transfers them to legitimacy-building. Suppression is moderate (0.42) because the constraint is not maintained by overt coercion — it is maintained by the lack of salient alternatives: everyone is trapped in the ceremonial equilibrium (peer jurisdictions run the same drills, so exiting looks like admitting failure). The temporal series shows theater and extractiveness both rising and then plateauing: theater rises as the deficit between ceremony and reality grows, then stabilizes at a high level once the apparatus has fully shifted to legitimacy function; extractiveness rises similarly, then plateaus because the institutional benefit (legitimacy, budget, identity) reaches equilibrium. Measurements are one shared grid across all metrics at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The administration and responders see genuine preparedness (the drills are real, the equipment is inspected, the procedures are documented) and compute the constraint as Rope (honest coordination). The populations at flood risk, if they could perceive the actual procedure-reality gap, would see it as Snare (coercive theater that leaves them at risk). Junior responders compute it as Rope until they face a real flood and discover the procedures fail; then they recompute it as Piton (the form persists but the function has atrophied). The committer frame captures this divergence: this reading (husk_reading) instantiates the 'form persists without function' perspective, while the competence_reading instantiates the 'procedures are live knowledge' perspective. The engine computes per-seat classifications from the structural data; the authored claim (Piton) reflects this reading's assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   The civil protection administration has low directionality (near beneficiary end, ~0.2) — they set the apparatus and collect legitimacy from it; they have institutional power and can choose to update or maintain drills. Junior responders have high directionality (near target end, ~0.85) — they are powerless, identity-locked, and forced to internalize the procedures as competence even as they atrophy. Populations at flood risk have very high directionality (~0.95) — they are powerless, trapped, and bear the actual risk while told they are protected. Peer jurisdictions appear as observers (analytical exit) rather than co-victims because they are institutional actors with the power to update (they do not, by choice, because institutional and budgetary pressures keep them in equilibrium). The directionality divergence is the core structural fact: those with power to change the apparatus benefit from its persistence as ceremony; those without power bear the cost of its actual incompetence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy: the founding mandate (maintain institutional memory of procedures across personnel changes) was real when procedures were stable across time (early 20th century, before climate change accelerated flood risk profiles). That mandate is now dead — procedures must be continuously updated, not preserved, because the flood risk landscape is changing faster than the update cycle of the drill apparatus. The constraint persists not because the mandate is live but because the apparatus now serves a different function (institutional legitimacy) that nobody wants to admit. The Piton classification captures this: the real work (actual competence) has atrophied, but the ceremonial work persists because it serves unspoken stakeholder interests (administrators get legitimacy, committees get cover, responders get validated identity). The mandatrophy is resolved (acknowledged in the founding_problem_status: dead and in the commentary) — the constraint is now officially serving a function different from its founding purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theater_vs_competence_boundary,
    'At what point does a drill become pure theater rather than live practice? Where is the boundary between ceremonial rehearsal and genuine skill maintenance?',
    'Trace a real flood event: measure the correlation between drill performance (did the administration pass the last inspection?) and actual response performance (did the procedures work?). A near-zero correlation indicates theater; a high correlation indicates genuine practice.',
    'A demonstrated low correlation reclassifies the constraint from Rope (coordination function) to Piton (atrophied form). High correlation would support the competence_reading instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_vs_competence_boundary, empirical, 'Whether drills predict actual response success or only administrative compliance.').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For junior responders, is the internalized false competence structural (economic: ''I cannot admit the procedures are wrong without destroying my career'') or internalized (cognitive: ''I genuinely believe the procedures are correct because I was trained on them, and retraining would mean admitting I was wrong'')?',
    'Post-flood interview data: responders who experience procedure failure — do they blame themselves (internalized), blame the training (structural), or blame the apparatus (system-level)? Track whether responders who leave the profession shed the false confidence (internalized) or retain it (both mechanisms).',
    'If primarily internalized, the suppression persists even after exit from the apparatus, and the victim cost is higher. If primarily structural, the victim cost is bounded by exit. This modulates the effective extraction the junior-responder seat computes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Whether junior-responder suppression is structural or internalized.').

omega_variable(
    alternative_frameworks_exclusion_mechanism,
    'Are rival preparedness frameworks (climate-responsive scenario planning, continuous equipment maintenance, cross-jurisdiction coordination) excluded by institutional inability to switch, or by structural design of the drill apparatus itself?',
    'Attempt to adopt an alternative framework in one jurisdiction: measure the institutional resistance (budget, regulatory, legitimacy) that blocks the switch. Compare jurisdictions that have attempted switches with those that have not.',
    'If exclusion is institutional (path dependence, vested interests), the constraint remains Piton but with a potential transition path. If exclusion is structural (the drill apparatus actively suppresses alternatives), the constraint is closer to Snare (coercive foreclosure of better options).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_frameworks_exclusion_mechanism, empirical, 'Whether the drill apparatus excludes alternatives by institutional inertia or structural coercion.').

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is one reading of a contested kernel: preparedness_persistence. The husk_reading asserts ''form persists while function atrophies.'' The competence_reading asserts ''procedures are live knowledge.'' Which reading is correct, or are both tenable depending on which data you attend to?',
    'The readings differ on what constitutes evidence of readiness (ceremony + documentation vs. performance under real conditions) and what preparedness requires (procedures vs. competence). This is a conceptual boundary, not an empirical gap: the readings co-instantiate the kernel; resolving which is ''correct'' requires choosing a framework for what preparedness means.',
    'If the husk_reading is adopted, the constraint is Piton and mandatrophy is resolved. If the competence_reading is adopted, the constraint is Rope and the founding problem is live. The hybrid_reading splits the difference (some components competent, some ritualized). The engine computes per-seat classifications; the committer frame explains which reading animates this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Committer ambiguity: which reading of the preparedness_persistence kernel is structurally true, or do both coexist as frames for the same empirical facts?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__husk_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__husk_reading, theater_ratio, 10, 0.66).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__husk_reading, theater_ratio, 15, 0.7).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__husk_reading, theater_ratio, 20, 0.73).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__husk_reading, theater_ratio, 25, 0.75).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__husk_reading, theater_ratio, 30, 0.76).
narrative_ontology:measurement(prep_tr_t35, preparedness_persistence__husk_reading, theater_ratio, 35, 0.76).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__husk_reading, theater_ratio, 40, 0.76).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__husk_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__husk_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__husk_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__husk_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__husk_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__husk_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(prep_be_t35, preparedness_persistence__husk_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__husk_reading, base_extractiveness, 40, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel is contested across three readings: husk_reading (this story, Piton), competence_reading (Rope), hybrid_reading (mixed). Each reading instantiates a different constraint with a different epsilon, beneficiary structure, and type. The readings share a referent (the drill and inspection apparatus) but diverge on what the apparatus accomplishes (theater vs. practice; atrophied vs. live). This story is the husk_reading (form without function). The competence_reading asserts the same apparatus is genuine coordination. The hybrid_reading asserts the apparatus is partially atrophied in some components and live in others. The three stories are linked via network.affects_constraints because each reading's viability influences the others — evidence that the husk_reading is structurally true undermines the competence_reading, and evidence of hybrid operation (some competent, some ritualized) moves the classification toward the hybrid_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
