% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Drill and Inspection Regime as Live Exercised Knowledge (Competence Reading)
 *   domain: institutional/disaster-preparedness
 *
 * SUMMARY:
 *   A regional emergency-management arrangement requires scheduled drills
 *   (evacuation, mass-casualty, continuity) and recurring systems inspections
 *   on the premise that exercised knowledge is what remains deployable when
 *   an event arrives, because unexercised capability decays on documented
 *   curves. This story instantiates the competence_reading of the
 *   preparedness_persistence kernel: the arrangement is live exercised
 *   knowledge, its costs are the cost of the coordination itself, and no seat
 *   extracts rent from it. The sibling readings (husk, hybrid) instantiate
 *   different constraints over the same standing arrangement and are separate
 *   stories linked in the network section; their contest is carried in omega
 *   variables and in kernel_context, not folded into this classification.
 *   Claim and metrics are independently authored from this reading's own
 *   lights: claimed_type rope, and the metric values this reading takes to be
 *   descriptively true. Assumptions: the interval maps T0 to the early-1990s
 *   professionalization of formal exercise programs and T30 to the present;
 *   the metrics describe a competently operating instance of the arrangement,
 *   not a degraded one. KEY AGENTS (by structural relationship): -
 *   operating_agency: agenda-setter and beneficiary
 *   (institutional/constrained) — runs the exercise calendar, retains the
 *   exercised capability - readiness_regulator: agenda-setter
 *   (institutional/constrained) — mandates intervals, audits compliance -
 *   emergency_response_personnel: principal cost-bearer and competence-holder
 *   (moderate/constrained) — pays drill hours, holds the skills -
 *   protected_public: beneficiary (powerless/constrained) — receives the
 *   protection maintained readiness provides - exercise_evaluators:
 *   analytical observer (moderate/analytical) — scores performance, feeds the
 *   corrective loop - disrupted_service_units: excluded voice
 *   (moderate/constrained) — surrenders operating hours to the calendar,
 *   absent from readiness policy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Drill and Inspection Regime as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "institutional/disaster-preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, 'cd3a025b-2311-4008-b252-6b2b0f9549fb').
narrative_ontology:cs_kernel_codification('cd3a025b-2311-4008-b252-6b2b0f9549fb', formalized).
narrative_ontology:cs_authority_grounding('cd3a025b-2311-4008-b252-6b2b0f9549fb', expertise).
narrative_ontology:cs_interpretation_layer_present('cd3a025b-2311-4008-b252-6b2b0f9549fb').
narrative_ontology:cs_reading_relation('cd3a025b-2311-4008-b252-6b2b0f9549fb', preparedness_persistence__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('cd3a025b-2311-4008-b252-6b2b0f9549fb', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('cd3a025b-2311-4008-b252-6b2b0f9549fb', foundational, practice_maintains_operational_readiness).
narrative_ontology:cs_axiom_status(practice_maintains_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('cd3a025b-2311-4008-b252-6b2b0f9549fb', practice_maintains_operational_readiness, empirically_contingent).
narrative_ontology:cs_axiom('cd3a025b-2311-4008-b252-6b2b0f9549fb', secondary, exercise_is_functional_verification).
narrative_ontology:cs_axiom_status(exercise_is_functional_verification, holdable).
narrative_ontology:cs_axiom_grounding('cd3a025b-2311-4008-b252-6b2b0f9549fb', exercise_is_functional_verification, empirically_contingent).
narrative_ontology:cs_reference_frame('cd3a025b-2311-4008-b252-6b2b0f9549fb', exercised_readiness_regime).
narrative_ontology:cs_drift_state('cd3a025b-2311-4008-b252-6b2b0f9549fb', post_drill_theater_reforms, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('cd3a025b-2311-4008-b252-6b2b0f9549fb', '2026-08-04T09:30:00Z').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, protected_public).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, operating_agency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, emergency_response_personnel).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, exercised_readiness_doctrine).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, stress_inoculation_training_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the emergency-management organization: sets the annual exercise calendar, funds the training division, and answers for response performance when events occur. It mandates participation in drills and inspections and receives the exercised capability they produce. It cannot exit the arrangement without abandoning its statutory mission, and its leadership answers to oversight bodies for readiness failures.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, operating_agency, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, operating_agency, beneficiary).

% Sets the minimum exercise and inspection intervals the agency must meet, such as quarterly evacuation drills, annual full-scale exercises, and scheduled systems inspections. It evaluates compliance and can downgrade readiness ratings or issue findings for missed or failed exercises. It is bound by its own statute and cannot waive the intervals it enforces without formal rulemaking.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, readiness_regulator, agenda_setter,
    institutional, generational, constrained, national).

% Staff the response units. They give drill hours and inspection-preparation time out of working schedules, and in return they hold current skills, rehearsed coordination with their teammates, and equipment they have personally verified. Leaving for another employer moves them onto that employer's exercise calendar rather than out of the arrangement; within a career, the competence they exercise is theirs to carry.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_response_personnel, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, emergency_response_personnel, payer).

% Lives and works under the protection that maintained readiness provides: evacuation routes that have been walked, shelters that have been stocked and checked, responders who have rehearsed the region's specific hazards. They pay through taxes and through compliance with drill-day instructions, and they have no practical exit from the arrangement short of moving out of the hazard zone.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, protected_public, beneficiary,
    powerless, biographical, constrained, regional).

% Design scenarios, observe exercises, score performance against stated objectives, and write the after-action findings that feed the corrective-action loop. They hold no command authority and collect no revenue from the arrangement; their product is the evaluation record that tells the agency and the regulator whether the exercise program is working.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, exercise_evaluators, observer,
    moderate, biographical, analytical, regional).

% Hospital wards closed for mass-casualty drills, transit lines paused for tunnel exercises, court schedules moved for lockdown drills: these units surrender operating hours to the exercise calendar. They are consulted on timing through scheduling committees but are not principals in readiness policy, and their objection that drill timing and scope give too little weight to service continuity reaches the calendar only indirectly.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, disrupted_service_units, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the decay problem: individual skills fade without practice on documented curves, coordination links between units weaken without rehearsal, and physical systems and written procedures drift from their documented state. The scheduled exercise and inspection cycle converts documented plans into rehearsed, deployable capability, verifies equipment and procedures before an event exposes the gap, and synchronizes many separate actors on a shared, current playbook.
% TRANSFER_FUNCTION: Moves staff time and attention from routine operations into exercised practice, and moves verification findings such as equipment defects, procedure gaps, and coordination failures observed in drills from the field into the corrective-action process. Money flows only as budget for the training division; there is no significant wealth or status transfer between parties.
% ABSENT_VOICES: The units whose operations are displaced by exercises would tighten the drill calendar and cap its claim on service hours. Communities whose real behavior diverges from the drilled plan, including disabled residents, non-English speakers, and shift workers, are represented by planners in scenario design rather than present as themselves. Both would object that the arrangement optimizes for the scenario it rehearses rather than the event that occurs.
% DISAPPEARANCE_RATIONALE: Unexercised skills decay within months on documented curves, inter-unit coordination links weaken without rehearsal, and equipment and procedural drift goes undetected until failure. Response performance would degrade measurably within one to two years, and after the first visible failure the agency would reconstruct some exercise function under oversight pressure. The arrangements reorganize around the loss because the decay problem the cycle solves never stops regenerating.
% FOUNDING_PROBLEM: Response organizations repeatedly held written plans that failed under stress: procedures no one had executed, equipment that did not work as documented, and coordination links between units that had never been rehearsed. The exercise and inspection cycle was built to close that gap, converting plans into practiced capability and finding the defects before an event did.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: skill-decay research in cognitive psychology documents the decay curves for unrehearsed emergency skills; accident investigations by bodies independent of the operating agencies repeatedly attribute performance gaps to lapsed practice and unverified equipment; and actuarial loss data correlates exercise frequency with reduced loss outcomes. No party outside the arrangement disputes that unexercised capability decays. The dispute inside this kernel is over whether the existing cycle actually exercises the capability or merely performs the exercise of it.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).
:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.15: the arrangement's costs are dominated by the inherent coordination cost of maintaining exercised knowledge against a decay process that never stops; the residual above the enforcement-mechanism floor is scheduling friction, not rent, so no victims are declared. Suppression 0.12: participation is mandated, but the mandate is administrative rather than suppressive; informal practice complements rather than competes, and no exit or alternative is fenced off. Theater_ratio 0.15: on this reading the activity is functional, and the residual theater is box-checking around evaluated exercises. Accessibility_collapse 0.5: workable alternatives existed (informal on-the-job practice), but once skill decay is understood, unpracticed readiness collapses as an option; the formal cycle dominates without erasing alternatives. Resistance 0.2: drill fatigue and scheduling friction are the main resistance; participants mostly accept the value because they hold the competence it produces. Measurement arc on one shared time grid (T=0,5,10,15,20,25,30): theater crept upward through the compliance-formalization era (announced, checklist-style exercises peaking at T15), dropped at T20 under the realism reform wave (no-notice drills, evaluated exercises, stress inoculation), then re-crept slightly as the reforms aged; extractiveness drifted mildly upward and flatlined. This is a single reform arc, not a recurring oscillation, so no intermittent-reinforcement mechanism is claimed. Receipt surface: gain_flow 'diffuse' is an affirmative claim — every named seat was checked and none converts the arrangement into rent; the product (exercised competence) is held by the trained personnel themselves and manifests as public protection. fixing_cost 'prohibitive': removing the cycle would be administratively trivial but functionally ruinous against a permanently regenerating decay problem, so no rational fixer removes it. The (diffuse, prohibitive) receipt signature coincides with the piton cell; the separation from a piton is carried by the metrics (theater 0.15, function live) and by the founding-problem interview (status live), not by the receipt surface, which cannot see function.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence under this reading is small but real: personnel bear the time cost and would compute a mildly costly seat if the regime's value were not visible to them, while the agenda-setter seats compute near-pure coordination; the engine computes this from the structural data, and the dual role (beneficiary with secondary payer) is what keeps the personnel seat near the beneficiary end. Inter-institutional dynamics: the agency and the regulator hold the same agenda-setter role with different relationships — the agency executes the calendar and owns the failure risk, the regulator sets intervals and audits from outside, and the regulator's only exit is rulemaking, which makes it structurally more rigid than the agency it oversees. Same-level lateral dynamics: emergency_response_personnel and disrupted_service_units sit at the same power atom with different relationships — the personnel hold the competence and accept the cost; the disrupted units bear displaced operations without holding the competence, which is why they appear as the excluded voice rather than as a payer seat. The largest perspectival gap in this kernel is between readings, not within seats: the husk reading's seat sees the same activity as memorial performance. That divergence belongs to the sibling stories and is routed through the reading_commitment_location omega, not averaged into this file.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party sits at or near the beneficiary end. Emergency_response_personnel pay drill hours but hold the exercised competence, so their derived directionality sits low-moderate rather than at the target end — the dual role declaration is what encodes this. Protected_public is a pure beneficiary with no exit. Operating_agency and readiness_regulator are agenda-setters who are also beneficiaries: they hold the capability and the mandate, and neither collects a rent stream distinct from the coordination itself. No seat is structurally targeted: victims are undeclared because, on this reading's lights, the arrangement extracts no net rent — costs convert into competence held by the payers themselves. No directionality overrides are used because the beneficiary/victim-plus-exit derivation produces the correct d for every seat without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — decay of unexercised capability — is a permanently regenerating process, so mandate-outliving-function is structurally blocked under this reading: the function cannot outlive the problem it solves because the problem regenerates continuously, which is why founding_problem_status is live and no mandatrophy resolution is declared. The mandatrophy risk in this kernel is real but belongs to the sibling readings: if the husk reading is correct for an institution, the arrangement there IS a mandate outliving its function, maintained theatrically. This story's classification prevents mislabeling in both directions: authoring low theater and a live founding problem keeps a genuinely functional arrangement from being misread as a piton via the shared (diffuse, prohibitive) receipt signature, while the exercise-validity omega keeps reclassification open if the function is shown absent for a given institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_commitment_location,
    'This constraint is the competence reading of the preparedness_persistence kernel: is a given institution''s drill/inspection arrangement functionally load-bearing (live exercised knowledge, as this reading holds), performative over atrophied function (the husk sibling), or stratified between the two (the hybrid sibling)?',
    'Exercise-validity auditing: correlate evaluated drill and inspection performance with performance in actual events, institution by institution. Institutions where drill scores predict field outcomes instantiate this reading; institutions where they diverge instantiate the husk or hybrid reading.',
    'If the husk reading is correct for an institution, this story''s low theater_ratio and rope claim are wrong for that institution and the sibling story (high theater, atrophied function) applies instead. The readings cannot be merged because they author different epsilon over the same referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_commitment_location, conceptual, 'Which reading of the preparedness_persistence kernel a given institution''s arrangement instantiates.').

omega_variable(
    decay_problem_natural_law_status,
    'Is the skill-and-coordination decay problem the arrangement solves a natural-law process (irreducible human and equipment decay), or is part of it constructed by the arrangement''s own procedural complexity?',
    'Compare decay rates for simple versus procedurally complex response tasks; if the decay burden scales with self-imposed procedure complexity, part of the problem the cycle maintains against is endogenous to procedural design.',
    'If substantially constructed, part of the arrangement''s coordination justification is self-imposed and its effective extraction is higher than this reading authors; if natural law, the rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_problem_natural_law_status, empirical, 'Whether the decay problem is exogenous natural law or partly endogenous to procedural design.').

omega_variable(
    drill_field_transfer_validity,
    'Does competence exercised in drills transfer to field performance, or does it hold only in a narrow band around the drilled scenario?',
    'No-notice exercise outcomes and after-action correlations between evaluated drill scores and actual-event performance across hazard types.',
    'Narrow transfer would move this reading toward the hybrid sibling (competent core, ritualized periphery) and raise the honest theater_ratio; broad transfer confirms the rope classification and this reading''s causal claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_field_transfer_validity, empirical, 'Training-transfer breadth underlying the competence reading''s maintenance claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__competence_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__competence_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__competence_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__competence_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__competence_reading, theater_ratio, 25, 0.14).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__competence_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(prep_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__competence_reading, base_extractiveness, 5, 0.13).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__competence_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__competence_reading, base_extractiveness, 15, 0.14).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__competence_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__competence_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__competence_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement_basis(prep_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% One colloquial claim — 'drills and inspections maintain readiness' — decomposes into three structurally distinct constraints over the same referent (the standing exercise/inspection arrangement): this file (competence_reading: live exercised knowledge, epsilon 0.15, low theater, rope), preparedness_persistence__husk_reading (memorial performance over atrophied competence, high theater, extraction without function), and preparedness_persistence__hybrid_reading (stratified: competent inspection core, ritualized drill periphery). The readings author different epsilon, different theater ratios, and different types against the same arrangement; per the epsilon-invariance principle they are separate stories linked by these network edges, not one story with a measurement parameter. The competence reading is upstream: where it holds, it sets the legitimacy and resource conditions under which the sibling critiques are raised.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
