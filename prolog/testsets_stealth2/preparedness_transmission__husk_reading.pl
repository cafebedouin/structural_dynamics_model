% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Civil-Defense Drill Cycle as Memorial Ritual (Husk Reading)
 *   domain: disaster_risk_management/institutional_memory
 *
 * SUMMARY:
 *   Mid-century catastrophic floods produced a civil-defense settlement: a
 *   standing drill-and-inspection regime chartered to transmit operational
 *   flood-response competence across generations of staff and volunteers.
 *   Fifty years on, the forms are immaculate — attendance rates, inspection
 *   pass rates, unbroken documentation chains — while the founding cohort's
 *   tacit knowledge retired with it. Drills replay scripts written decades
 *   ago, inspections detect only the failure modes the checklist anticipated,
 *   and successive post-flood inquiries have found real response performance
 *   diverging sharply from drill scores. This file instantiates the HUSK
 *   READING of the preparedness_transmission kernel: the regime as memorial
 *   ritual over hollowed operational knowledge. Epsilon's referent is the
 *   standing drill-and-inspection arrangement as the husk reading assesses it
 *   — the budget, hours, and false assurance the ritual consumes — never the
 *   reformed adaptive-training regime this reading would prefer. Claim and
 *   metrics are independent authored facts: claimed_type is the husk
 *   reading's structural assessment; the metrics describe observed operation;
 *   the engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - civil_defense_directorate: agenda-setter (institutional/constrained) — administers the ritual calendar and could redesign it, but bears little cost from not doing so
 *   - municipal_emergency_planners: payer with secondary beneficiary position (organized/identity_locked) — execute the cycle; careers fused to it
 *   - frontline_response_volunteers: payer (moderate/mobile) — supply the drill hours; the most capable exit first
 *   - floodplain_residents: principal payer (powerless/trapped) — bear false confidence and residual exposure
 *   - municipal_taxpayers: payer (moderate/constrained) — fund the cycle, scrutinize it episodically
 *   - compliance_audit_office: beneficiary (organized/constrained) — caseload and remit scale with the program
 *   - training_curriculum_vendors: beneficiary (moderate/arbitrage) — sell decades-old scenario packages
 *   - municipal_liability_insurers: beneficiary (powerful/arbitrage) — consume documented diligence in underwriting
 *   - adaptive_training_reformers: excluded (moderate/mobile) — propose unscripted exercises, hold no seat in drill design
 *   - post_flood_inquiry_commissioners: observer (institutional/analytical) — publish drill-score versus outcome divergences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.6).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.38).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Civil-Defense Drill Cycle as Memorial Ritual (Husk Reading)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '6a19e3ee-2433-4263-b4f9-fdab709123cc').
narrative_ontology:cs_kernel_codification('6a19e3ee-2433-4263-b4f9-fdab709123cc', formalized).
narrative_ontology:cs_authority_grounding('6a19e3ee-2433-4263-b4f9-fdab709123cc', lineage).
narrative_ontology:cs_interpretation_layer_present('6a19e3ee-2433-4263-b4f9-fdab709123cc').
narrative_ontology:cs_reading_relation('6a19e3ee-2433-4263-b4f9-fdab709123cc', preparedness_transmission__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('6a19e3ee-2433-4263-b4f9-fdab709123cc', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('6a19e3ee-2433-4263-b4f9-fdab709123cc', foundational, ritual_compliance_not_capability).
narrative_ontology:cs_axiom_status(ritual_compliance_not_capability, holdable).
narrative_ontology:cs_axiom_grounding('6a19e3ee-2433-4263-b4f9-fdab709123cc', ritual_compliance_not_capability, empirically_contingent).
narrative_ontology:cs_axiom('6a19e3ee-2433-4263-b4f9-fdab709123cc', secondary, adaptive_capacity_over_protocol_adherence).
narrative_ontology:cs_axiom_status(adaptive_capacity_over_protocol_adherence, holdable).
narrative_ontology:cs_axiom_grounding('6a19e3ee-2433-4263-b4f9-fdab709123cc', adaptive_capacity_over_protocol_adherence, instrumental).
narrative_ontology:cs_reference_frame('6a19e3ee-2433-4263-b4f9-fdab709123cc', founding_competence_transmission_regime).
narrative_ontology:cs_drift_state('6a19e3ee-2433-4263-b4f9-fdab709123cc', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6a19e3ee-2433-4263-b4f9-fdab709123cc', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_directorate).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, compliance_audit_office).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, training_curriculum_vendors).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, municipal_liability_insurers).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_response_volunteers).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, municipal_emergency_planners).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, municipal_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, municipal_emergency_planners).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, documented_compliance_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chartered after the founding floods to run the national flood-preparedness program. Publishes the annual drill calendar, owns the inspection protocols, and reports compliance statistics to parliament. Its staffing plan, budget line, and statutory remit are all sized to the drill-and-inspection cycle; redesigning the cycle would mean renegotiating its own mandate. Exit is not meaningful — the directorate is the program's institutional home; what it could do is reform, which successive leaderships have declined to attempt.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_directorate, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, civil_defense_directorate, beneficiary).

% Employed by municipalities to execute the drill calendar: book venues, brief participants, walk teams through scripted flood scenarios, and file the inspection paperwork. They spend hundreds of hours a year on cycles whose scenarios have changed little in decades. Their professional credentials, promotion paths, and peer networks are built around administering this cycle; leaving it would mean abandoning the expertise that defines their careers. They also draw role-standing and budget relevance from running it.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, municipal_emergency_planners, payer,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, municipal_emergency_planners, beneficiary).

% Show up for weekend drills, sandbag-line rehearsals, and equipment checks. The experienced ones increasingly decline to attend, citing repetition; those who remain learn the script rather than the trade. Resignation from the volunteer roster carries no penalty, and the departures of the most capable are part of how the practical knowledge base thins.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_response_volunteers, payer,
    moderate, biographical, mobile, regional).

% Live behind levees and along known flood corridors. They fund the program through municipal rates, see its drill footage in local media, and carry the resulting impression that their valley is prepared. Relocation is not realistically available; property, family, and livelihoods are tied to the floodplain. When floods arrive, they are the population the rehearsed response either serves or fails.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, floodplain_residents, payer,
    powerless, generational, trapped, local).

% Pay municipal rates and national levies that fund the drill cycle, encountering it mainly as line items and local news footage of exercises. Their scrutiny is episodic — electoral cycles occasionally surface the program's cost — but no organized constituency exists for ending a preparedness program, and opposing one reads publicly as opposing safety.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, municipal_taxpayers, payer,
    moderate, biographical, constrained, national).

% Inspects drill records, equipment stores, and plan currency against a checklist written alongside the founding charter. Each cycle generates findings, recommendations, and follow-up inspections; the office's caseload, headcount, and statutory importance scale with the program's size. It does not set drill content; it verifies that the forms were completed.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, compliance_audit_office, beneficiary,
    organized, biographical, constrained, national).

% Sell standardized drill packages, scenario binders, and compliance templates to municipalities under multi-year contracts. The product line has been refreshed cosmetically for decades while the underlying scenarios date to the founding era. They could pivot to other public-safety training markets if the cycle ended.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, training_curriculum_vendors, beneficiary,
    moderate, biographical, arbitrage, continental).

% Price municipal flood liability partly on documented preparedness. Completed drill registers and current inspection certificates enter the underwriting file as evidence of due diligence; their absence would raise premiums or complicate claims defense. The insurers take no role in whether the drilled skills would work.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, municipal_liability_insurers, beneficiary,
    powerful, generational, arbitrage, continental).

% A loose circle of younger emergency-management academics, some planners, and veteran responders who argue for scenario-generative, unscripted exercises and rotation through real flood deployments. They publish critiques and pilot alternatives outside the statutory cycle but hold no seat in the drill-design process, which is set by the directorate with vendor input and verified only by the audit office.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, adaptive_training_reformers, excluded,
    moderate, biographical, mobile, national).

% Convened after significant flood events to examine response performance. Their terms of reference give them access to drill records and response logs; successive commissions have compared drill scores against actual outcomes and published the divergence. They recommend; they do not administer.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, post_flood_inquiry_commissioners, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes a nationwide calendar of readiness activity across municipalities, maintains a shared protocol vocabulary and equipment-standard checklist, and produces a documented record of due diligence that insurers, auditors, and parliament can read. It coordinates scheduling and documentation; whether it coordinates capability is precisely what the kernel contest turns on.
% TRANSFER_FUNCTION: Moves municipal budget and staff/volunteer hours into scheduled ritual performance and compliance paperwork; moves documented-diligence assurance from municipalities to insurers and oversight bodies; moves comparatively little operational capability in any direction.
% ABSENT_VOICES: Adaptive-training reformers and flood-experienced residents are outside the design conversation: drill content is set by the directorate with vendor input, and the only verification loop is the audit office checking form. The unanimity of the compliance statistics arises partly because the seats that would report hollowness were never in the room.
% DISAPPEARANCE_RATIONALE: Overnight removal would force statutory amendment of the civil-defense act, collapse the audit office's caseload and remit, strand multi-year vendor contracts, void the documented-diligence clauses in municipal insurance underwriting, and obsolete the planner career structure — a wide administrative rearrangement. Actual flood-response capability would change little at first, because the drills currently transmit little; the rearrangement would be in the compliance economy built around the ritual, not in rescue capacity.
% FOUNDING_PROBLEM: After the mid-century flood disasters, the state needed to rebuild and transmit operational flood-response competence: a standing cadre able to coordinate levee defense, evacuation, and relief across municipalities. The drill-and-inspection cycle was chartered to carry that competence across generations of staff and volunteers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: successive post-flood inquiry commissions, convened ad hoc rather than drawn from program staff, have published drill-score-versus-outcome divergences; disaster-sociology researchers and retired responders' associations attest the retirement of the founding cohort's tacit knowledge. The directorate and audit office dispute the finding from inside, citing unchanged compliance statistics — the dispute itself is signal.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Theater is high (0.80) because the majority of drill-hours are scripted replays of founding-era scenarios with pre-briefed roles, and the only verification loop scores form. Extractiveness is moderate-high (0.60): the cycle consumes municipal budget, staff and volunteer hours, and manufactures reassurance that real floods have repeatedly undercut — a real cost borne diffusely, short of impoverishment. Suppression is moderate-low (0.38): attendance mandates and audit findings exist, but alternatives are smothered bureaucratically rather than coerced; the regime would likely persist even unenforced, which is why requires_active_enforcement is authored false — enforcement is a prop, not the load-bearing wall. Accessibility_collapse is moderate (0.40): adaptive alternatives keep being proposed and keep dying in committee rather than becoming unthinkable. Resistance is low (0.25): inquiry criticism and reformer pilots occur, but compliance is cheap and opposing a preparedness program reads as opposing safety. The suppression_requirement series is authored deliberately: enforcement machinery was built up mid-interval (mandatory attendance, audit penalties) to hold the form together as intrinsic motivation died, then plateaued as routine — the story specifically traces enforcement-capacity change, so the series belongs. All three series share one time grid (t=0,10,20,30,40,50) with endpoint values matching the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   From the directorate and audit seats the regime presents as functioning governance: calendars met, checklists passed, statistics reported — an experience of order that would compute near the coordination end. From the volunteer and taxpayer seats it presents as time and rates paid for reassurance; from the resident seat, as a safety promise real water has repeatedly tested. The planner seat straddles: identity-fused administrators who privately concede the scripts are stale while filing this year's identical paperwork. The engine computes these per-seat classifications from the structural data; the divergence between the administrator's orderly experience and the residents' target-side experience is the measurement the corpus exists to take, not a defect to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (audit office, vendors, insurers, plus the directorate's own perpetuation interest via its secondary beneficiary position) derive low directionality — the arrangement subsidizes them, damping effective extraction. Declared victims derive elevated directionality: volunteers and taxpayers straightforwardly; planners moderated by their secondary beneficiary role; residents pushed nearest the full-target end by trapped exit, a generational horizon, and the false-assurance channel. Scope is national with local bearing; the engine scales effective extraction modestly upward for verification difficulty at that scope. No directionality overrides are authored: the derivation from beneficiary/victim data plus exit options captures the structure, and the override keys available (power atoms) are too coarse for this story — training_curriculum_vendors and frontline_response_volunteers share the moderate atom with opposite structural positions, so any moderate-keyed override would corrupt one side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rebuilding and transmitting operational flood-response competence after the mid-century disasters — is dead in this reading: the knowledge stock it maintained drained out with the founding cohort, and what continues is the container. Yet the arrangement persists at growing cost because no seat gains enough to defend it and no seat hurts enough to fix it: the classic cost-asymmetry. Misclassification risks run both ways. Calling it a snare demands a capturer, and none concentrates — vendors and auditors collect thin slices, insufficient to explain persistence. Calling it a rope credits a coordination function that no longer operates adaptively; what is coordinated is scheduling and documentation, not capability. The R5 mismatch consumer (founding_problem_status dead x disappearance_verdict world_rearranges) flags the zombie pattern, which is exactly what the husk reading asserts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_resolution,
    'Does the standing drill-and-inspection regime instantiate the husk reading (hollowed ritual), the competence reading (live exercised knowledge), or the hybrid reading (stratified transmission)?',
    'Unscripted adaptive field exercises under novel flood scenarios, scored blind against outcome rather than checklist: sustained improvisational success supports the competence reading; collapse off-script supports the husk reading; sector-split results support the hybrid reading.',
    'Classification is reading-relative: a competence verdict would strip the ritual-extraction account and recompute toward a low-extraction coordination profile; a hybrid verdict splits the referent into two constraints with different epsilon values over the same arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_resolution, empirical, 'Which reading of the preparedness_transmission kernel the observed regime instantiates.').

omega_variable(
    hidden_capture_check,
    'Does any seat quietly capture concentrated value from the ritual — vendor contract margins, insurer premium effects, audit-office aggrandizement — sufficient to make the arrangement actively defended extraction rather than inertial residue?',
    'Forensic tracing of drill-program expenditure against delivered service, vendor margin disclosure, and counterfactual premium analysis on municipalities with lapsed drill documentation.',
    'A concentrated capturer would push the account from inertial persistence toward enforced extraction with identifiable winners; diffuse results confirm the no-capturer picture this story asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_capture_check, empirical, 'Whether concealed rent concentration underlies the ritual''s persistence.').

omega_variable(
    theater_measurement_validity,
    'Is the high theater ratio a real property of the drills or an artifact of evaluating them by their own scripted outputs — the same form the audit office scores?',
    'Blind field exercises scored on unscripted outcomes, cross-referenced with after-action divergence data from actual flood events.',
    'If substantial functional content survives unscripted testing, the theater ratio falls materially and the account shifts toward a mixed coordination/extraction profile; if it collapses, the ritual reading is confirmed at full strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_measurement_validity, empirical, 'Measurement-basis risk in the theater_ratio estimate.').

omega_variable(
    cs_kernel_framing_ambiguity,
    'Is the commitment kernel grounding the regime''s authority the founding flood charter itself, or the compliance-documentation apparatus that has grown around it?',
    'Trace which artifact the directorate and audit office invoke when defending the regime against reform proposals: the charter''s founding narrative or the documentation chain''s completeness requirements.',
    'If the documentation apparatus is the operative kernel, authority grounds shift from lineage toward extraction and the interpretive layer re-reads as the audit office''s checklist practice rather than charter transmission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_ambiguity, conceptual, 'Under-determination in which stabilized commitment anchors the regime''s legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__husk_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.72).
narrative_ontology:measurement_basis(prep_tr_t40, observed).
narrative_ontology:measurement(prep_tr_t50, preparedness_transmission__husk_reading, theater_ratio, 50, 0.8).
narrative_ontology:measurement_basis(prep_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__husk_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(prep_be_t40, observed).
narrative_ontology:measurement(prep_be_t50, preparedness_transmission__husk_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(prep_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__husk_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__husk_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__husk_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement_basis(prep_su_t30, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__husk_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement_basis(prep_su_t40, observed).
narrative_ontology:measurement(prep_su_t50, preparedness_transmission__husk_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement_basis(prep_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'preparedness transmission' decomposes into three structurally distinct constraints over the same standing drill-and-inspection arrangement: competence_reading (low epsilon — function live), husk_reading (this file; epsilon 0.60 — ritual over hollowed knowledge), and hybrid_reading (split referent — engineering competence via professional channels, civilian coordination decayed). Upstream/downstream: the competence reading is the regime's self-description and is cited BY the regime as evidence of function; the husk reading is the critical downstream account built from after-action divergence data. Each sibling file should reciprocate these links. The confusion lives in the shared label, not in the structure; each reading keeps one stable epsilon over the fixed referent (the standing arrangement), per the epsilon-invariance rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
