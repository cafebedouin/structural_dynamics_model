% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission via Live Exercise (Competence Reading)
 *   domain: institutional/civil_defense
 *
 * SUMMARY:
 *   This constraint instantiates the competence_reading of the
 *   preparedness_transmission kernel. In this reading, drills and inspections
 *   function as live re-validation of operational knowledge — each generation
 *   participates in scenario-varied exercises that test adaptive capacity,
 *   surface novel failure modes, and transmit tacit knowledge of
 *   improvisation and coordination under pressure. The constraint persists
 *   because disaster response systems cannot wait for real catastrophes to
 *   validate competence; the founding coordination problem (generational
 *   knowledge loss without live exercise) remains live. The key structural
 *   claim of this reading: adaptive capacity, novel-signature recognition,
 *   and effective improvisation across organizational boundaries are
 *   maintained through practice, not decayed into hollow ritual.
 *
 * KEY AGENTS:
 *   - disaster_response_personnel: Organized actors (emergency managers, first responders) who maintain readiness through drill cycles; they benefit from validated competence and suffer modest extraction (time cost); their exit is constrained by institutional obligation and professional identity.
 *   - drill_inspectors_and_evaluators: Institutional agenda-setters who design scenarios, observe performance, and surface novel failure signatures; they guide knowledge transmission across generations.
 *   - inter_organizational_coordinators: Powerful actors (agencies that must work together in real events) who use drills to practice coordination protocols and build institutional muscle memory for inter-agency negotiation.
 *   - junior_and_new_personnel: Moderate-power beneficiaries whose competence accumulation depends on substantive live exercise; without it, they inherit only abstract procedures.
 *   - political_oversight_bodies: Analytical observers who assess whether competence is maintained or degraded; their attention is reactive to real disasters.
 *   - infrastructure_and_technology_sector: Excluded actors whose knowledge of critical-system failure modes and cascading failures is absent from most coordination drills.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.28).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.15).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission via Live Exercise (Competence Reading)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "institutional/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '48a00228-bb8f-4cb2-bfad-eb2018cb6891').
narrative_ontology:cs_kernel_codification('48a00228-bb8f-4cb2-bfad-eb2018cb6891', distributed).
narrative_ontology:cs_authority_grounding('48a00228-bb8f-4cb2-bfad-eb2018cb6891', practice).
narrative_ontology:cs_interpretation_layer_present('48a00228-bb8f-4cb2-bfad-eb2018cb6891').
narrative_ontology:cs_reading_relation('48a00228-bb8f-4cb2-bfad-eb2018cb6891', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('48a00228-bb8f-4cb2-bfad-eb2018cb6891', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('48a00228-bb8f-4cb2-bfad-eb2018cb6891', foundational, adaptive_competence_maintained_through_practice).
narrative_ontology:cs_axiom_status(adaptive_competence_maintained_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('48a00228-bb8f-4cb2-bfad-eb2018cb6891', adaptive_competence_maintained_through_practice, empirically_contingent).
narrative_ontology:cs_axiom('48a00228-bb8f-4cb2-bfad-eb2018cb6891', foundational, novel_failure_signatures_recognized_by_scenario_variation).
narrative_ontology:cs_axiom_status(novel_failure_signatures_recognized_by_scenario_variation, holdable).
narrative_ontology:cs_axiom_grounding('48a00228-bb8f-4cb2-bfad-eb2018cb6891', novel_failure_signatures_recognized_by_scenario_variation, empirically_contingent).
narrative_ontology:cs_reference_frame('48a00228-bb8f-4cb2-bfad-eb2018cb6891', live_exercise_knowledge_transmission).
narrative_ontology:cs_drift_state('48a00228-bb8f-4cb2-bfad-eb2018cb6891', contemporary_inspection_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('48a00228-bb8f-4cb2-bfad-eb2018cb6891', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, disaster_response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, general_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, inter_organizational_coordinators).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, junior_and_new_personnel).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, disaster_response_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Emergency managers, first responders, civil defense coordinators who maintain operational readiness through repeated drill participation. They benefit from live validation of their own competence and discovery of novel failure modes before real events. They also bear the cost of drill time, scenario preparation, and the cognitive load of maintaining readiness across generational cohorts. Their exit from the system is constrained by institutional obligation and professional identity.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, disaster_response_personnel, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, disaster_response_personnel, payer).

% Set drill parameters, observe performance, document novel failure signatures, and provide feedback that loops back into training. They maintain the constraint's operation by designing scenarios that vary enough to prevent rote performance while remaining grounded in plausible disaster profiles. Their role requires staying current with changing threat landscapes and infrastructure evolution.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, drill_inspectors_and_evaluators, agenda_setter,
    institutional, generational, mobile, national).

% Agencies that must work together in real disasters (fire, health, police, transportation, utilities) use drills to practice coordination protocols and surface friction points in communication, resource sharing, and command authority. The drill constraint lets them build institutional muscle memory for inter-agency handoff and negotiation without waiting for actual catastrophe.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inter_organizational_coordinators, beneficiary,
    powerful, generational, constrained, national).

% Legislature, executive leadership, and public auditing bodies watch whether preparedness systems maintain competence. They assess whether drills are generating real learning or performing empty compliance. Their attention is periodic and reactive to real disasters rather than continuous.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, political_oversight_bodies, observer,
    institutional, biographical, analytical, national).

% New hires and younger cohorts learn operational knowledge through drill participation and feedback cycles. Without live exercise, they would inherit only abstract procedures and doctrine. Their competence accumulation depends on drills being substantive knowledge-testing rather than theatrical compliance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, junior_and_new_personnel, beneficiary,
    moderate, biographical, constrained, national).

% Private utilities, telecommunications, and digital infrastructure operators are sometimes included in drills but often sidelined in coordination scenarios. They hold critical knowledge about failure modes and recovery cascades but participate only when explicitly invited. Their absence from competence-building drills means disaster response protocols may not account for infrastructure interdependencies.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, infrastructure_and_technology_sector, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational readiness across generational cohorts and inter-organizational boundaries by treating drills and inspections as live knowledge-exercise: each drill tests whether personnel can recognize novel failure signatures, adapt established procedures to scenario variation, and coordinate across agency boundaries under realistic time pressure.
% TRANSFER_FUNCTION: Moves organizational time and cognitive effort (from incident response capacity, planning cycles, and individual training budgets) into the live exercise system. The transfer is reciprocal: personnel invest time in drills and receive validated competence feedback and novel threat recognition in return.
% ABSENT_VOICES: Infrastructure and private-sector operators whose systems are critical to disaster response are often excluded from coordination drills. They would voice that public-sector exercises test procedures that assume infrastructure availability but do not validate cascading failure modes or recovery sequencing with operators present. Academic disaster researchers and post-incident review teams would attest that procedural knowledge alone, without live exercise cycles, degrades rapidly across generational turnovers.
% DISAPPEARANCE_RATIONALE: If live exercise drills and inspections stopped, response capability would degrade within 5-7 years as generational knowledge transfer broke down. New cohorts would inherit only doctrine, not the adaptive, improvisation-laden competence that live scenario variation builds. The first major disaster after drill cessation would likely reveal coordination failures at inter-agency boundaries and novel failure-mode blindness. World does not merely reorganize — it becomes more fragile.
% FOUNDING_PROBLEM: Civil defense and disaster response systems cannot rely on real disasters to validate competence; waiting for catastrophe to test procedures is catastrophically inefficient. Competence must be maintained and transmitted across generational turnover through live, repeated, scenario-varied exercise that surface failures before they cost lives.
% FOUNDING_PROBLEM_CORROBORATION: Post-disaster after-action reports consistently document that agencies with maintained drill cycles adapted faster and identified novel failure modes more quickly than those with lapsed or perfunctory exercise programs. Independent disaster researchers (Quarantelli, Tierney, Comfort) attest to the generational degradation of tacit knowledge without institutional practice cycles. Emergency management professional associations affirm that competence requires live exercise. This corroboration comes from outside the benefiting parties' internal assessments.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).

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
 *   Extractiveness is low (0.28 at interval end) because the constraint's primary function is coordination — maintaining readiness across organizational boundaries and generational turnover — and the extraction (time, effort, opportunity cost) is distributed and reciprocal (personnel invest time, receive competence validation in return). Suppression is very low (0.15) because participation is sustained by institutional obligation and professional identity, not coercion; exit is constrained but not forcibly prevented. Theater ratio is also low (0.22) because, in the competence_reading, drills are substantively testing knowledge and surfacing novel failure modes rather than performing compliance. The measurement series is flat at the end (plateau at 0.28 extractiveness) indicating the constraint has stabilized at its equilibrium operational cost — the extraction is the necessary overhead of maintaining readiness, not rent-seeking drift. This reading asserts that extractiveness is stable and low because the coordination function remains genuine and the cost is proportional to its delivery.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (inspectors), this is a coordination mechanism they maintain and which validates their professional expertise. From the payer seat (personnel bearing time cost), it is genuine coordination they depend on. From the excluded seat (infrastructure operators), it is a sidelined capability with unvalidated knowledge about cascading failures. The engine computes these different directionalities from the structural data; the perspectival gap is that the constraint's competence function is robust from inside the public-sector boundaries but may hide infrastructure-coupling blindness.
 *
 * DIRECTIONALITY LOGIC:
 *   Disaster response personnel (organized power, constrained exit) are near-symmetric: they bear the cost of drill time and effort but benefit from validated competence and novel-threat discovery. Inspectors (institutional, mobile exit) sit closer to beneficiary end — they run the system and have better exit options. Inter-organizational coordinators (powerful, constrained exit) are also near-symmetric: they need the coordination but also invest substantially. Junior personnel (moderate power) sit slightly toward target (constrained, dependent on training). Infrastructure operators (excluded) are not seats in this constraint — they are absent entirely from the coordination function this reading claims. The low extractiveness and symmetric directionality for most seats is consistent with a rope classification: the constraint coordinates a genuine problem with modest overhead and no concentrated beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (generational knowledge loss without live exercise) is live — the constraint is not a zombie. The claim (rope) and the measured metrics (low extractiveness, low theater, low suppression) are aligned, and independent corroboration from post-disaster studies supports that live exercise maintains competence across generational turnover better than lapsed or perfunctory systems. Mandatrophy is not detected on this reading because the coordination function persists and the structural data do not show extraction drift or theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_validity,
    'Can inspectors reliably distinguish between authentic adaptive competence (novel failure signature recognition, effective improvisation under scenario variation) and polished performance that masks hollow operational knowledge?',
    'Post-disaster field validation: comparing actual response quality to pre-disaster drill evaluations. High correlation between drill-assessed competence and real-event adaptive performance would validate the measurement; low correlation would indicate the drills are measuring compliance theater rather than competence.',
    'If measurement is invalid (inspectors cannot distinguish husk from competence), the constraint degrades to a scaffold or piton — the real coordination function persists only until generational turnover erodes tacit knowledge undetected. If valid, the constraint remains rope (genuine coordination function with modest extraction cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_measurement_validity, empirical, 'Whether live exercise validation reliably measures adaptive competence.').

omega_variable(
    sibling_reading_distinction,
    'This is the competence_reading: drills transmit and validate operational knowledge through live exercise. The husk_reading claims drills persist as memorial ritual with hollowed-out knowledge. What observable distinguishes them?',
    'Post-disaster performance trajectories and generational transition outcomes. Competence-reading systems show intact knowledge transmission at cohort handoff and rapid novel failure-mode adaptation in real events. Husk-reading systems show sudden capability gaps at generational turnover and slow recognition of unfamiliar failure modes. The two readings instantiate different structural predictions about knowledge persistence.',
    'If a system transitions from competence-reading to husk-reading, the constraint''s functional basis erodes while its performative infrastructure persists — the outcome is reclassification to piton. This omega documents that the two readings are empirically distinguishable, not merely interpretive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_distinction, empirical, 'Structural distinction between competence_reading and husk_reading via knowledge transmission outcomes.').

omega_variable(
    infrastructure_exclusion_cascade,
    'Does excluding private-sector infrastructure operators from coordination drills create a hidden competence gap that only surfaces during disasters affecting critical infrastructure systems?',
    'Audit of drills that include private-sector participation vs. those that exclude them; post-disaster analysis of coordination speed and effectiveness when infrastructure dependencies are live variables vs. assumed stable.',
    'If significant gaps exist, the competence-reading''s adaptive capacity claim is partly false — the system can adapt to scenario variation within the public sector but not across infrastructure boundaries. This would argue for hybrid_reading classification or constraint decomposition (separate stories for public-sector and cross-sector preparedness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_exclusion_cascade, empirical, 'Whether infrastructure operator exclusion creates undetected competence gaps.').

omega_variable(
    generational_tacit_knowledge_decay,
    'In this reading, live exercise transmits tacit knowledge (improvisation under pressure, failure signature recognition) across generations. But how much of the transmission loss over time is inherent to the knowledge domain vs. recoverable through drill design changes?',
    'Longitudinal cohort studies tracking knowledge retention 10+ years post-training, comparing individuals with sustained drill participation vs. those with lapsed cycles. High retention with maintained drills + sharp decay with lapses supports the reading; uniform decay regardless would indicate tacit knowledge has inherent time-horizon limits the constraint cannot overcome.',
    'If decay is inherent to the domain, the constraint''s coordination function has a built-in shelf life — it can validate competence for a 5-7 year window but not across longer generational cycles. This would constrain the reading''s claim of robust inter-generational transmission.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_tacit_knowledge_decay, empirical, 'Whether live exercise can sustain tacit knowledge transmission across generational gaps.').

omega_variable(
    reading_kernel_committer_declaration,
    'This story instantiates the competence_reading of the preparedness_transmission kernel. Sibling readings husk_reading and hybrid_reading represent different structural claims about whether knowledge persists, degrades, or stratifies across sectors. How should we distinguish which reading is structurally true of a given system at a given time?',
    'This is a conceptual omega: the readings are not mere interpretations of one fixed constraint but different structural predictions. Competence_reading predicts adaptive-capacity trajectories; husk_reading predicts rapid degradation at turnover; hybrid_reading predicts sector-specific stratification. Systems can transition between readings as structural conditions change. The resolution is not to declare one reading true globally but to track which reading''s predictions match the observed system behavior over time.',
    'This omega routes the committer-frame decomposition (kernel_id preparedness_transmission has three structurally distinct readings) through the omega apparatus rather than leaving it implicit in the narrative. It documents that a system''s reading membership is empirically testable and can shift — a system in competence_reading that shows husk_reading signatures (sudden capability gaps, failure to recognize novel modes) has undergone a structural transition, not a mere interpretive reframing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_committer_declaration, conceptual, 'Committer-frame kernel reading as empirically distinguishable structural claim, not mere interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__competence_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(prep_tr_t8, observed).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__competence_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(prep_tr_t16, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__competence_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__competence_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(prep_tr_t32, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__competence_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__competence_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement_basis(prep_be_t8, observed).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__competence_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(prep_be_t16, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__competence_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__competence_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement_basis(prep_be_t32, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__competence_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel decomposes into three structurally distinct readings based on empirically distinguishable knowledge-transmission outcomes. Competence_reading claims live exercise maintains adaptive capacity and novel-failure recognition. Husk_reading claims performative ritual masks hollowed knowledge. Hybrid_reading claims infrastructure competence persists while civilian coordination has decayed. Each reading instantiates different structural predictions about knowledge persistence, failure-mode recognition speed, and generational turnover outcomes. A system's reading membership is empirically testable via post-disaster performance trajectories and can shift if structural conditions change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
