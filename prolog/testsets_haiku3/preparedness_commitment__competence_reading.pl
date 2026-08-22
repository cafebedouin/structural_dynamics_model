% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: institutional/commitment_system
 *
 * SUMMARY:
 *   In the competence reading, preparedness is a live, continuously exercised
 *   system of knowledge transfer that maintains organizational
 *   decision-making capacity across generational turnover. Responders learn
 *   through realistic drills where real decision-making is tested under
 *   pressure; mentors embed knowledge in apprentices through shared
 *   scenarios; after-action reviews surface failures and refine the system.
 *   The constraint stabilizes against the decay that occurs in purely
 *   documentary systems. This reading is one instantiation of a contested
 *   kernel: the husk reading sees the same apparatus as memorial performance
 *   disconnected from operational reality; the hybrid reading layers both
 *   memorial and competence elements. The competence reading asserts that
 *   effective preparedness must be exercised knowledge, not archived
 *   knowledge.
 *
 * KEY AGENTS:
 *   - trained_responders: beneficiaries; their competence is maintained through drills
 *   - at_risk_populations: beneficiaries; their safety depends on responder competence
 *   - budget_administrators: payers; they fund the recurring cost of exercises
 *   - regulatory_authority: agenda_setter; they set mandates and standards
 *   - disaster_victims: excluded; they experience the constraint's success or failure but have no voice in its definition
 *   - generational_transition_nodes: observer; the analytical frame where knowledge transfer succeeds or fails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.31).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.18).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "institutional/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '4a6bcb9e-a27b-4754-8f15-e2f8118f41f8').
narrative_ontology:cs_kernel_codification('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', distributed).
narrative_ontology:cs_authority_grounding('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', expertise).
narrative_ontology:cs_interpretation_layer_present('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8').
narrative_ontology:cs_reading_relation('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', foundational, live_exercise_necessary_for_competence).
narrative_ontology:cs_axiom_status(live_exercise_necessary_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', live_exercise_necessary_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', foundational, generational_knowledge_transfer_through_mentoring).
narrative_ontology:cs_axiom_status(generational_knowledge_transfer_through_mentoring, holdable).
narrative_ontology:cs_axiom_grounding('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', generational_knowledge_transfer_through_mentoring, empirically_contingent).
narrative_ontology:cs_reference_frame('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', competence_through_live_exercise).
narrative_ontology:cs_drift_state('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', contemporary_budget_austerity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4a6bcb9e-a27b-4754-8f15-e2f8118f41f8', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, trained_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, at_risk_populations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, organizational_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, budget_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Emergency management professionals, fire chiefs, hospital disaster coordinators, and incident commanders whose competence is maintained through regular scenario drills, tabletop exercises, and hands-on simulation. They benefit from a living system that keeps their muscle memory sharp and surfaces decision-making under realistic uncertainty. Exit is constrained by professional obligation and organizational continuity; a responder cannot 'opt out' without abandoning the preparedness system that sustains their operational capacity.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, trained_responders, beneficiary,
    organized, biographical, constrained, regional).

% Communities that depend on the operational competence of responders: residents in flood zones, seismic risk areas, wildfire corridors, hospitals in regions prone to mass casualty events. They benefit from responders whose decision-making is tested and refined through realistic preparation. They have no exit from the constraint itself—they depend on it—and no direct control over its maintenance. Their welfare is the latent justification for the preparedness commitment.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, at_risk_populations, beneficiary,
    powerless, immediate, trapped, regional).

% A non-agent entity: the institutional capacity to respond effectively across generational turnover. Live exercised knowledge transfers competence from retiring responders to apprentices through shared drills, after-action reviews, and embedded storytelling. The constraint stabilizes organizational memory against attrition.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, organizational_continuity, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_commitment__competence_reading, organizational_continuity).

% Government officials and nonprofit leaders who fund ongoing training, drill infrastructure, and scenario development. They bear the recurring cost of exercises that do not produce visible incident response—the opportunity cost of responder time spent in drills rather than other duties, plus capital for simulation facilities. Their exit is constrained by legal mandate (many jurisdictions require documented preparedness) and by the tacit obligation to maintain competence.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, budget_administrators, payer,
    moderate, biographical, constrained, national).

% People actually experiencing a disaster when the constraint fails or succeeds. They are excluded from the preparedness-planning conversations—the drills, after-action reviews, and competence refinement happen without their voice. If the constraint works, they benefit retroactively; if it fails, they bear the cost. They have no seat at the table where preparedness is defined.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, disaster_victims, excluded,
    powerless, immediate, trapped, regional).

% The analytical frame capturing moments when experienced responders retire or move to new roles and apprentice staff must absorb their decision-making knowledge. In a live competence-based system, these transitions are explicitly managed through drills and mentoring; in a husk system, the knowledge is lost. This is the test of whether preparedness is genuine or memorial.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, generational_transition_nodes, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_commitment__competence_reading, generational_transition_nodes).

% Standards bodies, homeland security directives, and accreditation agencies that mandate preparedness documentation and often define what counts as adequate training. They set the ceiling and floor for what the constraint must accomplish. Their power is constrained by the need to avoid mandate creep that would make preparedness performative rather than competent.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, regulatory_authority, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the temporal problem of organizational memory across generational turnover: live exercised knowledge (drills, tabletop exercises, after-action reviews) transfers decision-making competence from experienced responders to apprentices, preventing the knowledge loss that would occur if training were only classroom-based or documentary.
% TRANSFER_FUNCTION: Moves responder time, organizational attention, and budget resources from other activities into recurring exercises and training refinement. The constraint asks responders to spend hours in scenarios that will (hopefully) never match any real incident, and administrators to fund infrastructure that produces no visible output unless a disaster occurs.
% ABSENT_VOICES: Disaster victims are structurally excluded from preparedness planning. They experience the constraint's success or failure acutely but have no voice in defining what preparedness means or whether the exercises actually test the decisions that matter most. Communities most vulnerable to disaster (low-income neighborhoods, remote areas, institutionalized populations) are rarely present in the room where preparedness is exercised.
% DISAPPEARANCE_RATIONALE: If live exercised preparedness vanished, organizations would revert to static documentation and theoretical training. Responders' real decision-making capacity would degrade within 3–5 years as experienced staff retired and apprentices lacked mentoring in decision-making under uncertainty. Organizational response to major incidents would become reactive problem-solving rather than rehearsed adaptation, with cascading failures. Generational knowledge would be lost at each transition.
% FOUNDING_PROBLEM: Early disaster response systems discovered that responders trained only on classroom instruction and written protocols performed poorly under real incident pressure. Decision-making froze, communication broke down, and experienced knowledge was lost when veteran responders retired. The problem: how to maintain organizational competence across generational turnover when the expertise is embedded in people, not in documents.
% FOUNDING_PROBLEM_CORROBORATION: After-action reviews from real disasters (Hurricane Katrina, COVID-19 surge response, 2023 Maui wildfires) document failures traceable to responder training gaps and knowledge loss during staff turnover. Incident commanders and emergency management directors outside the benefiting parties (federal auditors, academic researchers in disaster response, incident reviewers from unaffected jurisdictions) corroborate that organizations with robust drill programs performed better than those relying on documentation alone. The founding problem remains unresolved for many jurisdictions.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 at interval end) because the constraint solves a genuine coordination problem—maintaining competence across turnover—and the beneficiaries (responders, protected populations) are net positive from the arrangement. Suppression is minimal (0.18) because participation in drills is neither coercive nor hidden; responders engage voluntarily (within professional obligation) and the public largely supports preparedness mandates. Theater rises slightly over the interval (0.12 to 0.24) as budget pressure incentivizes counting 'preparedness activities' rather than measuring actual decision-making improvement—a common drift in institutionalized training. The measurement series shows modest extractiveness growth and rising theater ratio, characteristic of a rope that is starting to accumulate defensive overhead. The rhythm is stable across the interval with no cyclical oscillation: preparedness exercises happen regularly, quarterly or annually, not as crisis response. The temporal profile models a constraint that is working (low extraction, low suppression) but is under pressure to become more performative (rising theater) as budget scrutiny intensifies.
 *
 * PERSPECTIVAL GAP:
 *   From the responder and protected-population seats, the constraint is clearly beneficial—they depend on it for competence and safety. From the budget-administrator seat, the constraint is a recurring cost with deferred, uncertain payoff (exercises prevent incidents that might not have happened anyway). From the regulatory seat, preparedness is a compliance obligation whose adequacy is contested. The engine computes these divergences from the structural data: beneficiaries get low d (subsidy effect from the arrangement), payers get moderate d (they bear cost but also benefit incidentally from the organization's overall capacity). The divergence models the real institutional tension: responders see preparedness as essential practice; administrators see it as budget line item competing for resources; regulators see it as a mandate that must be met with minimal excess.
 *
 * DIRECTIONALITY LOGIC:
 *   Trained responders are structural beneficiaries (d near 0.2): they gain competence maintenance, professional development, and organizational memory. At_risk populations are beneficiaries (d near 0.15) but trapped and powerless—they cannot opt into or out of reliance on responder competence. Budget administrators are payers (d near 0.65): they fund the constraint's operation from general revenue and see no direct rents. Regulatory authority is the agenda_setter (d near 0.5): they mandate preparedness but also depend on its outcomes for legitimacy; a major disaster with visible responder failure delegitimizes the regulator. The directionality profile models a genuine coordination constraint with moderate asymmetry—not pure extraction, not pure mutual benefit, but a shared-problem solution with distributed costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem—maintaining competence across generational turnover—is live and unresolved in many jurisdictions (evidenced by post-incident reviews showing knowledge gaps). The mandatrophy risk is low because the constraint's function remains aligned with its founding purpose. The risk rises if budget pressure forces preparedness exercises to become purely documentary or if theater ratio rises above 0.45 (indicating exercises are counted as metrics rather than measured for actual competence growth). A high theater ratio with stable low extractiveness would signal the constraint is becoming a husk—a reading covered by omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_distinction,
    'How would an observer distinguish a live competence-based preparedness system from a husk system that performs preparedness without operational competence? What measurable indicators separate real decision-making growth from theatrical compliance?',
    'Incident-based validation: compare response quality (decision speed, error rates, adaptation under uncertainty) between organizations with strong drill cultures vs. documentary-only training, controlling for responder experience and incident type. Post-incident reviews should explicitly assess whether responder decisions reflected trained patterns or novel problem-solving.',
    'If a clear separation emerges, the competence reading is structurally distinct from the husk reading and deserves independent classification. If incidents show no correlation between drill intensity and response quality, the readings may be indistinguishable in practice, and the constraint would be reclassified toward husk or hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_distinction, empirical, 'Whether live drills produce measurably better incident response than documentary-only training').

omega_variable(
    generational_knowledge_transfer_mechanism,
    'What specific mechanisms in the competence reading transfer knowledge from experienced responders to apprentices? Is this transfer effective across demographic or organizational boundaries?',
    'Ethnographic study of organizations with high generational turnover vs. stable staffing; tracking of specific decisions made by apprentices vs. their mentors across multiple drills; interviews with retired responders about what knowledge was or was not transmitted.',
    'If transfer mechanisms are robust and cross-boundary effective, the constraint''s claim to coordinate across generations is well-founded. If transfer is fragile, depends on individual relationships, or fails at demographic boundaries, the constraint may operate only for organizations with stable privileged cohorts, reducing its scope and generalizability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_mechanism, empirical, 'Whether generational knowledge transfer actually occurs through the competence system').

omega_variable(
    budget_extraction_under_austerity,
    'As budget constraints tighten, does the preparedness constraint shift from genuine coordination (low extraction) to a rationing mechanism that allocates constrained resources disproportionately to well-resourced jurisdictions or to visible compliance metrics?',
    'Analysis of preparedness funding allocation and drill frequency across jurisdictions with different resource levels before and after fiscal austerity; measurement of theater_ratio correlation with budget constraint severity.',
    'If austerity forces the constraint toward extractive rationing (redistributing resources from vulnerable areas to compliance-measurable ones), the constraint would reclassify toward snare or tangled_rope. The founding problem would remain live but the solution mechanism would break down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(budget_extraction_under_austerity, empirical, 'Whether budget austerity converts the coordination constraint into an extraction mechanism').

omega_variable(
    regulatory_capture_of_preparedness_standards,
    'Do regulatory agencies define preparedness standards in ways that protect their own legitimacy rather than optimize for actual disaster response? Does the competence reading''s claim of ''exercised knowledge'' remain true, or does it drift toward metrics that regulators can audit?',
    'Comparison of preparedness standards across jurisdictions; analysis of post-incident findings on whether disaster response failures correlate with regulatory compliance or with gaps in actual training; interviews with incident commanders on whether regulatory drills match real decision-making challenges.',
    'If regulatory capture occurs, the constraint''s extractiveness rises and its beneficiary structure shifts: regulators gain the benefit of compliance metrics while responders bear the cost of performative training. The constraint would reclassify toward tangled_rope (coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_preparedness_standards, conceptual, 'Whether regulatory definitions of preparedness shape the competence system or whether the competence system shapes regulations').

omega_variable(
    kernel_reading_choice_point,
    'This constraint is ONE reading of a contested kernel. The sibling readings (husk and hybrid) would compute different beneficiary structures, different extraction profiles, and different classifications. What structural fact about the preparedness system would you observe to determine which reading is correct?',
    'Propose an ensemble of observables: (a) responder decision-making quality post-training; (b) knowledge retention across staff turnover; (c) correlation between drill intensity and incident response; (d) budget allocation patterns during austerity; (e) regulatory standard drift. If (a)-(c) show strong correlation with drill quality, the competence reading holds. If (a)-(c) show no correlation, the husk reading is supported. If (d)-(e) show capture dynamics layered onto genuine training, the hybrid reading is supported.',
    'The choice between readings determines the constraint''s classification, its beneficiary set, its victim set, and its mandatrophy status. This omega documents the under-determination: the kernel is real but the readings are genuinely contested and require empirical resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice_point, conceptual, 'Which reading of the preparedness_commitment kernel is structurally correct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__competence_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__competence_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__competence_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__competence_reading, base_extractiveness, 5, 0.21).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__competence_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__competence_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__competence_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__competence_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__competence_reading, suppression_requirement, 15, 0.16).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__competence_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__competence_reading, suppression_requirement, 25, 0.19).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__competence_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement_basis(prep_su_t30, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel decomposes into three structurally distinct constraint readings. The competence_reading asserts that effective preparedness requires live exercised knowledge maintained through regular drills and mentoring. The husk_reading asserts the same institutional apparatus operates as memorial performance disconnected from operational competence. The hybrid_reading layers both: memorial elements (formal training records, accreditation requirements) stabilize the institutional commitment while competence elements (hands-on drills, decision-making under pressure) maintain actual capacity. These are not three measurements of one constraint—they are three different ε values assigned to the SAME kernel by three different epistemic frames. The competence_reading instantiates the frame that prioritizes decision-making effectiveness; ε measures extraction in a system that achieves its stated purpose (low). The husk_reading instantiates the frame that questions whether effectiveness is actually achieved; ε measures extraction in a system that is mostly theatrical (high). The hybrid_reading instantiates the frame that layers both functions; ε measures extraction in a system that does both (moderate). The three files must be linked through network.affects_constraints so the engine can detect the kernel contest and compute cross-reading directionality divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__competence_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
