% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Flood Drill and Inspection Regime — Memorial Ritual Reading
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   A regional flood preparedness regime — annual evacuation drills, levee
 *   inspections, municipal coordination exercises — has been performed
 *   continuously for four decades. Attendance is logged, checklists are
 *   completed, certifications are issued on schedule. This story authors the
 *   HUSK reading of the preparedness_transmission kernel: the ritual form
 *   persists and organizational memory of THAT a drill happens is intact, but
 *   the operational knowledge of WHY each step matters and HOW to adapt it to
 *   a novel flood configuration has hollowed out. The drills detect only the
 *   failure modes they were originally scripted to detect; anything outside
 *   that pre-specified set (compound rainfall-surge events, multi-basin
 *   simultaneous overtopping) is invisible to the inspection regime
 *   regardless of how faithfully the regime is executed. This is a distinct
 *   constraint from the competence_reading (which holds that live exercise
 *   genuinely re-validates capability each cycle) and the hybrid_reading
 *   (which holds that engineering competence remains high while only civilian
 *   coordination knowledge has decayed) — each of those is a separate story
 *   with its own epsilon and its own stakeholder structure, linked here via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - civil_defense_bureaucracy: agenda_setter (institutional/constrained) — administers the drill calendar, collects legitimacy from its continuation
 *   - compliance_auditors: beneficiary (organized/mobile) — certifies form, funded by the audit cycle's persistence
 *   - floodplain_residents: payer (powerless/trapped) — bears the consequence if the certified readiness is a false signal
 *   - frontline_emergency_responders: payer (moderate/constrained) — executes the script, improvises unsupported when reality diverges from it
 *   - retired_flood_engineers: excluded (powerless/trapped) — held the tacit knowledge the checklist encoded, no longer consulted
 *   - regional_flood_modeling_office: observer (institutional/analytical) — sees the gap between tested scenarios and actual risk, its findings acknowledged but not operationalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.58).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.42).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Flood Drill and Inspection Regime — Memorial Ritual Reading").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '503ee7e5-25bf-4b47-bf3c-53a0c82632c9').
narrative_ontology:cs_kernel_codification('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', formalized).
narrative_ontology:cs_authority_grounding('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', practice).
narrative_ontology:cs_interpretation_layer_present('503ee7e5-25bf-4b47-bf3c-53a0c82632c9').
narrative_ontology:cs_reading_relation('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', foundational, protocol_performance_decouples_from_operational_judgment_over_generational_transmission).
narrative_ontology:cs_axiom_status(protocol_performance_decouples_from_operational_judgment_over_generational_transmission, holdable).
narrative_ontology:cs_axiom_grounding('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', protocol_performance_decouples_from_operational_judgment_over_generational_transmission, empirically_contingent).
narrative_ontology:cs_axiom('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', secondary, compliance_certification_cannot_detect_failure_modes_outside_its_original_specification).
narrative_ontology:cs_axiom_status(compliance_certification_cannot_detect_failure_modes_outside_its_original_specification, holdable).
narrative_ontology:cs_axiom_grounding('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', compliance_certification_cannot_detect_failure_modes_outside_its_original_specification, empirically_contingent).
narrative_ontology:cs_reference_frame('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', post_founding_engineer_designed_playbook).
narrative_ontology:cs_drift_state('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', contemporary_post_compound_flood_events, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('503ee7e5-25bf-4b47-bf3c-53a0c82632c9', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_bureaucracy).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, compliance_auditors).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_emergency_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the annual drill calendar and inspection checklist inherited from a prior generation of flood engineers. Could redesign the drills around current flood modeling but the checklist format is easier to defend in budget hearings than a redesigned curriculum would be to justify. Bears no personal cost from the hollowing — the drills still happen, attendance is still logged, funding still flows on schedule.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_bureaucracy, agenda_setter,
    institutional, generational, constrained, regional).

% Certify that drills occurred and checklists were completed. Their job is to verify form, not adaptive competence, so the ritual's persistence is exactly what makes their function legible and fundable. They collect fees and continued employment from the audit cycle regardless of whether the drills produce real capability.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, compliance_auditors, beneficiary,
    organized, biographical, mobile, regional).

% Live behind levees whose maintenance schedules are certified through the same inspection regime. Told the system is drilled and inspected regularly, they have no independent way to verify whether the inspections detect novel failure modes (levee overtopping under compound rainfall-surge events) or only the pre-specified checklist items. They cannot relocate easily and bear the full consequence if the ritual's form conceals a real capability gap.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, floodplain_residents, payer,
    powerless, biographical, trapped, local).

% Execute the drills as scripted and pass the inspections, but privately report that the scenarios never include the flood configurations actually observed in the last decade — multi-basin simultaneous overtopping, night-time evacuation under power loss. When a real event diverges from the drilled script, they improvise without institutional support, because the institution's own memory of how to improvise was never re-encoded — only the performance of readiness was.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_emergency_responders, payer,
    moderate, immediate, constrained, local).

% Designed the original drill protocols decades ago when the checklist items mapped to live operational judgment calls. Now retired, they are not consulted when the checklist is renewed each cycle; the tacit knowledge of WHY each drill step existed left the institution with them and was never re-derived by their successors, who inherited only the procedure.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, retired_flood_engineers, excluded,
    powerless, biographical, trapped, local).

% Produces updated flood risk models showing scenario classes the current drill regime does not test. Its findings are cited in the civil defense bureaucracy's annual report but not incorporated into the drill design itself — the report satisfies the requirement to acknowledge new risk without requiring the drills to change.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, regional_flood_modeling_office, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: standardize evacuation and levee-response procedures across multiple municipalities so that no jurisdiction's flood response depended on ad hoc improvisation during a crisis.
% TRANSFER_FUNCTION: Moves budget allocation, staff time, and public confidence from the population at risk toward the maintenance of a compliance record, on the understanding that the record certifies real readiness rather than merely certifying that a scripted performance occurred.
% ABSENT_VOICES: Retired flood engineers who understood the original rationale behind each checklist item are not consulted in protocol renewal; the regional flood modeling office's scenario updates are cited but not operationalized. Both would argue the drills no longer test what matters.
% DISAPPEARANCE_RATIONALE: The bureaucracy and auditors would say the world rearranges catastrophically — funding, certification, and public confidence structures all depend on the drill calendar continuing. Frontline responders and the modeling office would say comparatively little would change in ACTUAL flood outcomes, because the drills are already failing to build the adaptive capacity needed for the flood configurations that occur; removing the ritual would mainly remove the false signal of readiness, which is itself contested as a net harm or a net neutral.
% FOUNDING_PROBLEM: Historic flood events exposed uncoordinated, improvised municipal responses that cost lives; the drill and inspection regime was built to encode and continuously re-validate a shared operational playbook across jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Frontline emergency responders and the regional flood modeling office, both outside the beneficiary set, attest that the drills no longer test the flood configurations actually observed and that the checklist has not been substantively revised in step with updated risk models. Retired flood engineers, also outside the beneficiary set, attest that the tacit judgment the checklist once encoded was never transmitted to successor staff. No party inside the civil defense bureaucracy or the compliance auditors independently corroborates that the founding problem remains live in the form the drills test.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is authored high and rising (0.28 to 0.81) because under the husk reading the visible activity — drills occurring, checklists completed, certifications issued — increasingly substitutes for the function it originally indexed (real adaptive capacity). Extractiveness is authored as moderate and rising (0.31 to 0.58), not extreme, because the bureaucracy is not actively looting; it is failing to update, and the cost this failure imposes on floodplain residents and responders accumulates as risk exposure rather than as a direct transfer. Suppression is moderate (0.42): no one is actively blocking protocol reform, but the institutional path of least resistance (renew the existing checklist) functions as a soft barrier to redesign. Resistance is low (0.35) because the hollowing is largely invisible until a real event exposes it — there is little active pushback against a ritual that looks, on its face, like diligence.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this looks like a rope — a functioning coordination mechanism sustained by regular practice. From the payer seats, the same structure computes as a piton: a form that once had real function, now sustained mostly by institutional inertia and the difficulty of proving a negative (that the drills fail to test what matters) against a compliance record that says otherwise. The engine's per-seat computation is expected to surface exactly this divergence from the authored structural data, not from any narrative framing choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The civil defense bureaucracy and compliance auditors sit near the beneficiary end: their legitimacy and funding derive from the ritual's continued performance, independent of whether it produces real capability. Floodplain residents and frontline responders sit near the target end: they bear the consequence of a capability gap that the ritual's own instrumentation cannot detect, and their exit options are structurally constrained (residents are trapped by geography and asset investment; responders are constrained by professional and institutional dependency on the very apparatus that has hollowed out).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate — coordinated flood response competence — has not been formally resolved as obsolete; the founding problem (uncoordinated improvised response causing preventable deaths) has structurally atrophied into an unaddressed drift (new flood configurations the drills don't test) rather than being solved and retired. Because the ritual's form is indistinguishable from genuine function to anyone outside the responder and modeling seats, the classification as piton (rather than snare) hinges on there being no concentrated beneficiary extracting rents from the gap — the bureaucracy and auditors benefit from institutional continuity, not from actively worsening resident outcomes. If a beneficiary were found to be knowingly suppressing the modeling office's findings for gain, this would reclassify toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_determination,
    'Is the observed drill regime genuinely hollowed out (husk reading), or does live performance still re-validate real capability in ways not visible from the compliance record (competence reading)?',
    'After-action review of a real flood event that diverges from drilled scenarios: if responders successfully improvise using knowledge traceable to drill training, competence reading gains support; if response collapses outside the scripted scenario space, husk reading is corroborated.',
    'Determines whether the constraint is better modeled as a functioning rope with drift risk (competence) or a piton whose form has decoupled from function (husk) — this story commits to the husk reading and authors its metrics accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_determination, empirical, 'Structural ambiguity between the husk and competence readings of the same drill regime.').

omega_variable(
    stratification_boundary_with_hybrid_reading,
    'Does the hollowing described here apply uniformly across engineering and civilian-coordination knowledge domains, or is it stratified as the hybrid reading claims (engineering competence intact, coordination knowledge decayed)?',
    'Disaggregated inspection audit separating levee/infrastructure engineering checks from civilian evacuation coordination checks, scored independently for adaptive capacity against novel scenarios.',
    'If stratification holds, this story''s uniform-hollowing claim overstates the engineering-competence side and the hybrid_reading constraint should carry the more accurate epsilon for that domain; the husk reading would then apply most precisely to the civilian coordination layer only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stratification_boundary_with_hybrid_reading, conceptual, 'Whether hollowing is uniform (husk) or domain-stratified (hybrid) — the location of disagreement between sibling readings.').

omega_variable(
    natural_atrophy_vs_engineered_neglect,
    'Is the hollowing an inevitable feature of any long-running bureaucratic ritual (a kind of institutional entropy), or was it enabled by specific decisions (e.g., defunding the modeling-integration step, not consulting retired engineers) that could have been avoided?',
    'Institutional history review: trace budget and staffing decisions across the interval to identify whether specific choices accelerated the decoupling of drill design from updated risk models.',
    'If engineered neglect, responsibility and fixing_cost analysis shift toward the bureaucracy''s discretionary choices rather than an unavoidable drift; if natural atrophy, the piton classification is more strongly supported as inertial rather than negligent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_atrophy_vs_engineered_neglect, conceptual, 'Whether the transmission failure was avoidable policy neglect or structural institutional entropy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__husk_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__husk_reading, theater_ratio, 16, 0.55).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__husk_reading, theater_ratio, 24, 0.68).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__husk_reading, theater_ratio, 32, 0.76).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__husk_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__husk_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__husk_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__husk_reading, base_extractiveness, 32, 0.54).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__husk_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'preparedness transmission' per the ε-invariance principle. competence_reading claims drills genuinely re-validate capability each cycle (lower ε, closer to rope); husk_reading (this story) claims the ritual form has decoupled from operational substance (moderate-rising ε, piton); hybrid_reading claims stratification by domain (engineering intact, coordination decayed — mixed ε across sub-domains). Each carries its own epsilon and stakeholder structure; they are linked, not merged, because measuring 'the drill regime' by different observables (engineering test results vs. civilian coordination outcomes vs. aggregate compliance record) yields genuinely different epsilon values, indicating three distinct constraints under one colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
