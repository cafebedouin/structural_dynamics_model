% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Scheduled-Simulation Competence Maintenance — Hybrid Decay Reading (Procedure Retained, Judgment Decaying)
 *   domain: organizational/safety_engineering/crisis_preparedness
 *
 * SUMMARY:
 *   Across safety-critical industries, scheduled simulation exercises have
 *   become the dominant mechanism for maintaining crisis-response competence
 *   between rare real events. This story instantiates ONE reading of the
 *   contested kernel exercise_as_competence_maintenance: the
 *   hybrid_decay_reading, which holds that the competence kernel has two
 *   components with different exercise requirements — a procedural component
 *   (checklists, callouts, equipment sequences, role choreography) that
 *   simulation genuinely exercises and retains, and a judgment-under-stakes
 *   component (improvised prioritization, deviation recognition, triage under
 *   ambiguity) that simulation does not exercise, so it decays. The epsilon
 *   referent is the STANDING ARRANGEMENT — organization-wide reliance on
 *   scheduled simulation as the primary competence-maintenance mechanism —
 *   assessed by this reading's own lights: partial retention credited,
 *   partial abandonment charged. It is NOT the blended
 *   real-stakes-plus-simulation program this reading would endorse; that
 *   alternative appears nowhere in the metrics. Sibling readings
 *   (simulation_sufficiency_reading, lived_catastrophe_necessity_reading) are
 *   separate constraints with their own epsilon, beneficiary structures, and
 *   classifications; they are linked only through the network and the reading
 *   relations declared in cs_structure. The claimed type (tangled_rope) and
 *   the metric values are independently authored facts: the claim states what
 *   this reading believes is structurally true of the arrangement; the
 *   metrics state what it believes is descriptively true of its operation.
 *
 * KEY AGENTS:
 *   - exercising_organizations: agenda setter (institutional/constrained) — mandates and administers the exercise regime; collects cost-avoidance and defensible documentation
 *   - exercise_vendors_consultants: beneficiary (organized/arbitrage) — sells the exercises; revenue scales with mandated volume
 *   - compliance_regulators: beneficiary (institutional/constrained) — collects inspectable audit artifacts without measuring judgment
 *   - liability_insurers: beneficiary (powerful/arbitrage) — accepts documentation as duty-of-care evidence and prices on it
 *   - frontline_responders: primary bearer of the non-simulated component's decay (moderate/identity_locked) — retains procedure, loses judgment edge; secondary beneficiary of the procedural half
 *   - downstream_public: primary victim of real-event failures in the non-exercised component (powerless/trapped)
 *   - independent_training_effectiveness_researchers: analytical observer — measures the gap the other seats do not price
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.64).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.43).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.43).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Scheduled-Simulation Competence Maintenance — Hybrid Decay Reading (Procedure Retained, Judgment Decaying)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "organizational/safety_engineering/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, 'b6267e91-ff5f-4e02-bff1-2d4613117218').
narrative_ontology:cs_kernel_codification('b6267e91-ff5f-4e02-bff1-2d4613117218', distributed).
narrative_ontology:cs_authority_grounding('b6267e91-ff5f-4e02-bff1-2d4613117218', expertise).
narrative_ontology:cs_interpretation_layer_present('b6267e91-ff5f-4e02-bff1-2d4613117218').
narrative_ontology:cs_reading_relation('b6267e91-ff5f-4e02-bff1-2d4613117218', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('b6267e91-ff5f-4e02-bff1-2d4613117218', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('b6267e91-ff5f-4e02-bff1-2d4613117218', foundational, competence_kernel_is_bicomponent).
narrative_ontology:cs_axiom_status(competence_kernel_is_bicomponent, holdable).
narrative_ontology:cs_axiom_grounding('b6267e91-ff5f-4e02-bff1-2d4613117218', competence_kernel_is_bicomponent, empirically_contingent).
narrative_ontology:cs_axiom('b6267e91-ff5f-4e02-bff1-2d4613117218', secondary, asymmetric_requirements_imply_partial_maintenance).
narrative_ontology:cs_axiom_status(asymmetric_requirements_imply_partial_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('b6267e91-ff5f-4e02-bff1-2d4613117218', asymmetric_requirements_imply_partial_maintenance, instrumental).
narrative_ontology:cs_reference_frame('b6267e91-ff5f-4e02-bff1-2d4613117218', simulation_trains_procedure_not_judgment).
narrative_ontology:cs_drift_state('b6267e91-ff5f-4e02-bff1-2d4613117218', contemporary_post_failure_inquiry_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6267e91-ff5f-4e02-bff1-2d4613117218', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, exercising_organizations).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_vendors_consultants).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, compliance_regulators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, liability_insurers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, downstream_public).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hospitals, fire services, utilities, airlines, refineries, and similar operators. They schedule and run the exercises, choose scenario scope and fidelity within compliance minima, and file completion records. Recurring simulation avoids the far larger ongoing expense of sustained real-stakes rotation programs and produces the documentation that defends them in accreditation review and litigation. Dropping the regime would mean failing accreditors, regulators, and insurers simultaneously.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercising_organizations, agenda_setter,
    institutional, biographical, constrained, continental).

% Commercial firms selling scenario design, simulators, facilitation, and after-action reporting. Revenue scales with mandated exercise volume, and the product line favors repeatable, auditable formats over bespoke judgment-building programs that resist standardization and resale. Pivoting to adjacent corporate-training markets is straightforward if demand shifts.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_vendors_consultants, beneficiary,
    organized, immediate, arbitrage, global).

% Accrediting bodies and safety authorities that require documented exercise schedules. Completion records give them inspectable evidence of diligence without requiring observation of judgment under stress, which no routine inspection regime can measure. Tightening or loosening requirements carries political and statutory cost, binding them to the documentation paradigm they inherited.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, compliance_regulators, beneficiary,
    institutional, generational, constrained, national).

% Underwriters who price premiums partly on evidenced training compliance. Documented exercise histories lower assessed liability exposure and give claims adjusters a defensible paper trail. At portfolio scale they can reprice or exit lines quickly, so their exposure to any single operator's unreadiness is limited.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, liability_insurers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Firefighters, ICU nurses, plant operators, flight crews, and equivalent hands-on staff. Scheduled drills keep their checklists, callouts, and equipment sequences sharp, and they carry that procedural sharpness as real skill. The parts of the job that require improvised prioritization when reality departs from every rehearsed script receive no comparable repetition. Leaving the profession means forfeiting licensed careers and a self-concept built on being the prepared one; union representation gives them voice but no seat in exercise design.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders, beneficiary).

% Patients, residents near industrial facilities, air travelers, building occupants — everyone whose safety depends on an organization's worst-hour performance. They fund the system through taxes, rates, and fares, absorb its failures first and most completely, never see an exercise syllabus, sit on no design committee, and cannot opt out of dependence on the local hospital, utility, or emergency service.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, downstream_public, payer,
    powerless, generational, trapped, regional).

% Academics and investigative journalists who study whether accumulated exercise hours predict real-event performance. They publish after disasters, testify to inquiries, and accumulate longitudinal datasets, but hold no seat in curriculum or scenario design; their findings shape the system only when a visible failure forces attention onto it.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, independent_training_effectiveness_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, exercising_organizations).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes recurring practice of core emergency procedures — evacuation sequences, equipment handling, communication protocols, role assignments — so that distributed teams retain shared scripts between rare real events, solving inter-event skill decay once, centrally, through scheduled exercises.
% TRANSFER_FUNCTION: Moves training budget away from real-stakes preparation (live rotations, full-scale deployments, apprenticeship under operators mid-event) and toward scheduled simulation vendors; moves the risk of unreadiness in the non-exercised component onto whoever depends on response quality during real events; moves liability evidence upward to regulators and insurers.
% ABSENT_VOICES: Downstream publics and the people harmed in past real events are absent from exercise design rooms entirely; independent training-effectiveness researchers are heard only in post-disaster inquiries, never before design decisions; junior responders' long-run judgment development is represented by managers optimizing compliance metrics rather than by the juniors themselves.
% DISAPPEARANCE_RATIONALE: If the simulation-maintenance arrangement vanished overnight, organizations would face an immediate fork: rebuild expensive real-stakes pipelines (slow, hazardous, and incompatible with current staffing models) or accept open skill decay. The vendor market, the compliance-audit industry, insurer evidentiary practices, and accreditation frameworks would all lose their object of exchange and reorganize around whichever replacement emerged.
% FOUNDING_PROBLEM: Real catastrophes are rare, and teams forget life-safety procedures between them; early mandatory drilling (post-fire and post-accident drill regimes) was built to solve exactly that forgetting curve.
% FOUNDING_PROBLEM_CORROBORATION: Official post-incident inquiry reports — which repeatedly find crews executing rehearsed steps correctly while failing at the unrehearsed adaptation the event actually demanded — and the peer-reviewed training-transfer literature corroborate the split verdict from outside the benefiting parties. The benefiting parties themselves (operators, vendors, accreditors, insurers) attest full sufficiency of the arrangement, which is precisely the claim this reading declines to grant.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64 at interval end) reflects the wedge between the readiness the arrangement certifies and the readiness it delivers: the entire unpracticed judgment component is carried as uncompensated risk by responders and public. Suppression (0.43) is moderate and structural rather than personal — mandatory drill schedules, accreditation conditions, and insurer pricing close off the alternative of simply not exercising, while the expensive real-stakes alternatives are crowded out rather than banned; per the standing rule, suppression is authored as a raw structural property and is NOT scaled by directionality or scope — the engine owns any context scaling of extractiveness alone. Theater ratio (0.48) is high-but-sub-threshold: a large share of exercise activity now optimizes for documentation and inspectability rather than retention, approaching but not crossing the proxy-replacement line. Accessibility collapse (0.38): real-stakes alternatives (rotation programs, secondments, uncertainty-injected full-scale exercises) still exist and are occasionally built, but the compliance economy makes them rare, so alternatives are crowded out rather than eliminated. Resistance (0.5): post-disaster inquiries, safety-science criticism, and periodic practitioner revolt against checkbox drills constitute persistent, real, but institutionally weak opposition. The temporal series run on one shared grid (points 0, 8, 16, 24, 32, 40; roughly the modern compliance-era of mandatory documented exercising) with all three tracked metrics authored at every point; the drift is monotonic rather than cyclical — no intermittent-reinforcement dynamic is claimed — and the gently rising suppression_requirement series tracks genuine enforcement-infrastructure maturation (audit regimes, documentation mandates hardening over the era), not noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the exercising_organization seat, the arrangement looks substantially cooperative: it delivers genuine procedural retention, satisfies every external checker, and costs far less than the alternative — a coordination success with overhead. From the frontline_responder seat it is double-edged: real skill flows in through the procedural half while the judgment half of their professional capability quietly erodes, and they cannot exit without abandoning vocation and identity. From the downstream_public seat nothing about the bargain is visible at all — they experience only the tail risk of the unpracticed component, with no offsetting receipt, which computes as the harshest extraction in the story despite having the least contact with the mechanism. The vendor and insurer seats see a functioning market and a pricing input respectively. The engine derives these divergent per-seat classifications from the structural data; this commentary only explains why they must diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Exercising organizations derive near-beneficiary directionality: they collect cost-avoidance and liability evidence and face only constrained exit (accreditation and insurability bind them). Vendors and insurers, with arbitrage-grade exit, sit nearest the beneficiary pole. Regulators collect audit artifacts without running anything — genuine but passive beneficiaries. Downstream publics, trapped and powerless, sit at the full-target pole: they bear the unpracticed component's failures with zero offsetting receipt. Frontline responders are the deliberate complication: declared victims (they bear the judgment decay) whose secondary beneficiary position (retained procedural skill) damps their effective extraction below what a trapped-victim default would yield. The directionality override on the moderate power atom encodes this damping — without it, the derivation from victim declaration plus identity_locked exit would push responders to the extreme target end and erase the procedural half of their actual position. No other agent shares the moderate atom, so the override isolates exactly the dual-positioned seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters unusually much here because both neighboring mislabels are live temptations. Reading the arrangement as pure extraction erases the real coordination achievement this reading explicitly credits: procedural competence IS retained, inter-event forgetting IS largely solved, and the founding problem was genuine. Reading it as pure coordination erases the victims: the arrangement's expansion from 'keep procedures alive' to 'maintain competence, full stop' was never earned, and the unpracticed component's failure cost lands on people who never agreed to carry it. Tangled rope holds both halves in one structure: genuine coordination function (procedures), active enforcement (mandatory documented exercising), identifiable coordinated parties, and identifiable payers (responders' lost judgment edge, public's tail risk). Mandatrophy status is split, not resolved: the founding problem (inter-event procedural forgetting) is still live and still served, but the expanded mandate the arrangement now claims is not fulfilled by the mechanism — hence founding_problem_status 'contested' rather than 'dead', and no mandatrophy_resolved declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the hybrid_decay_reading of kernel exercise_as_competence_maintenance. What changes structurally if a sibling reading is adopted instead?',
    'Comparative adjudication across organizations: correlate real-event performance in the judgment-demanding component against exercise dosage and fidelity; whichever reading''s predicted retention curve best fits the outcome data carries the kernel.',
    'Adopting the simulation_sufficiency_reading empties the victim set and drives epsilon toward the coordination-cost floor (rope-like classification). Adopting the lived_catastrophe_necessity_reading withdraws even the procedural credit, raising epsilon sharply and pushing the classification toward pure extraction. The bicomponent premise of this reading sits between them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption changes victim set, epsilon, and classification.').

omega_variable(
    differential_component_decay,
    'Do procedural competence and judgment-under-stakes actually decay at different rates without their respective forms of exercise?',
    'Longitudinal competency assessment plus after-action datasets separating errors into scripted-execution failures versus adaptive-judgment failures, correlated with exercise history.',
    'If judgment decays as this reading holds, the victim set stands and epsilon is calibrated correctly; if simulation transfers judgment partially, epsilon drops toward a coordination-dominated profile and the harm claim narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differential_component_decay, empirical, 'The empirical core of the reading: unequal decay rates across competence components.').

omega_variable(
    compliance_theater_share,
    'What fraction of accumulated exercise hours produce durable procedural retention versus artifacts that merely satisfy auditors and accreditors?',
    'Randomized variation in exercise format with blinded follow-up of real-event and high-fidelity-assessment performance, separating retention-producing activity from documentation-producing activity.',
    'Calibrates the theater ratio; a high documentation-dominant share signals continued drift toward inertial maintenance and would justify monitoring for piton transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_theater_share, empirical, 'Functional-versus-performative split inside the exercise economy.').

omega_variable(
    bicomponent_decomposition_validity,
    'Is the two-component decomposition of the competence kernel (procedural vs judgment-under-stakes) the correct cut, or does competence resist decomposition such that this reading collapses into the lived_catastrophe_necessity_reading?',
    'Task-analytic and cognitive studies of expert performance under stress testing whether scripted and adaptive performance load on separable trainable factors.',
    'If the kernel is effectively unitary, this reading merges toward necessity (raising epsilon further); if competence decomposes into more than two components, additional victim sets attach to each unsimulated component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bicomponent_decomposition_validity, conceptual, 'Whether the bicomponent premise that distinguishes this reading is itself sound.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(exer_tr_t32, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(exer_be_t32, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 40, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement(exer_su_t32, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 40, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial commitment 'regular exercises maintain crisis competence' decomposes under the epsilon-invariance principle into three structurally distinct stories. The simulation_sufficiency_reading yields low epsilon and a coordination-dominated profile (its proponents are concentrated in the vendor and accreditor seats). The lived_catastrophe_necessity_reading yields high epsilon and an extraction-dominated profile (it discounts even procedural retention). This hybrid_decay_reading sits between: moderate-high epsilon, genuine coordination function plus asymmetric extraction, tangled-rope structure. Upstream/downstream pressure runs from sufficiency to hybrid: vendor marketing, accreditation guidance, and insurer evidentiary standards all cite the sufficiency premise as the warrant for the standing arrangement whose decay costs this reading prices. Each family member links to the others through affects_constraints; no member is orphaned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
