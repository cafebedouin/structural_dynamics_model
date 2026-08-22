% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_necessity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity for Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This reading instantiates one interpretation of the kernel
 *   'catastrophe_proxy_sufficiency': only actual catastrophic events provide
 *   irreducible stress and uncertainty necessary to maintain genuine
 *   operational competence; simulation is structurally insufficient. The
 *   constraint is claimed as Mountain (a natural law of human stress
 *   physiology and tacit knowledge that no technology can overcome), but
 *   organizations' behavior suggests they treat it as optional—investing
 *   heavily in simulation as a substitute for real-event exposure. The
 *   reading generates an FSM candidate by declaring catastrophe-necessity as
 *   beneficiary to the kernel itself: that catastrophes ARE irreducibly
 *   necessary is the proposition vindicated by the constraint's persistence.
 *   The sibling readings contest this by asserting that simulation fidelity,
 *   hybrid training, or generational degradation curves offer
 *   alternatives—that catastrophe necessity is contingent, not categorical.
 *   This reading claims the necessity is categorical (Mountain), not
 *   contingent on technology (simulation_fidelity_threshold reading) or
 *   salvageable through hybrid methods (hybrid_degradation reading) or fully
 *   substitutable (simulation_as_proxy reading).
 *
 * KEY AGENTS:
 *   - High-reliability operators (trapped payers maintaining competence across decades-long catastrophe-free periods)
 *   - Simulation technology providers (beneficiaries of the assumption that simulation suffices)
 *   - Regulatory bodies (agenda-setters enforcing training mandates biased toward simulation due to cost)
 *   - Personnel subject to training (constrained payers, psychologically invested in simulation sufficiency)
 *   - Experienced catastrophe-response operators (excluded; carry tacit knowledge simulation cannot capture)
 *   - Accident investigation boards (observers; repeatedly document simulator-trained competence gaps)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.68).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.71).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '65d1cb93-1d5a-4265-86e4-4bc479d7bffc').
narrative_ontology:cs_kernel_codification('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', implicit).
narrative_ontology:cs_authority_grounding('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', practice).
narrative_ontology:cs_interpretation_layer_present('65d1cb93-1d5a-4265-86e4-4bc479d7bffc').
narrative_ontology:cs_reading_relation('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', foundational, catastrophe_necessity_categorical).
narrative_ontology:cs_axiom_status(catastrophe_necessity_categorical, holdable).
narrative_ontology:cs_axiom_grounding('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', catastrophe_necessity_categorical, empirically_contingent).
narrative_ontology:cs_axiom('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', secondary, tacit_knowledge_irreplaceable).
narrative_ontology:cs_axiom_status(tacit_knowledge_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', tacit_knowledge_irreplaceable, deontological).
narrative_ontology:cs_reference_frame('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', catastrophe_capacity_as_natural_limit).
narrative_ontology:cs_drift_state('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', contemporary_simulation_investment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65d1cb93-1d5a-4265-86e4-4bc479d7bffc', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_as_irreducible_epistemic_constraint).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_technology_providers).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, personnel_subject_to_training).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aviation, nuclear power, emergency response, maritime operations. Must maintain competence across catastrophe-free periods measured in decades. Competence decays during calm, forcing continuous costly retraining, tabletop exercises, and rotation of personnel into live operations. Their structural bind: simulation feels like competence-building until catastrophe arrives and reveals gaps simulation could not instantiate. Exit looks like abandoning the domain or accepting catastrophe risk.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_operators, payer,
    organized, generational, trapped, global).

% Sell high-fidelity simulators, virtual reality training, scenario software. Economically vested in the hypothesis that simulation suffices—higher fidelity, more investment. Benefit from organizations' need to train during catastrophe-free periods; the belief that simulation can fully substitute for real events drives market demand and revenue.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_technology_providers, beneficiary,
    institutional, biographical, mobile, global).

% Set training mandates: how many hours of simulation, what fidelity standards, how often recertification. Officially agnostic between simulation and live operations, but under budget pressure favor simulators (cheaper than rotating personnel into real incidents). Enforce training requirements but cannot mandate that simulation actually maintains competence—that gap is what the constraint inhabits.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Pilots, nuclear operators, emergency commanders. Must sit through repeated simulator training, convinced it maintains their competence, but carry the tacit knowledge that simulation differs from real stakes. When catastrophe comes after years of simulation-only training, they discover gaps (stress response, tacit decision-making under true uncertainty, embodied reaction time). Their psychological investment in simulation's sufficiency protects the system from their own accurate assessment.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, personnel_subject_to_training, payer,
    moderate, biographical, constrained, global).

% The background rate of genuine operational catastrophes—rare enough that long periods pass with no real incidents, frequent enough that history records each catastrophe as a shock that reveals simulator-trained competence gaps. Not an agent, but the empirical phenomenon the constraint describes.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_event_stream, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_event_stream).

% Experienced operators who have faced real catastrophes and carry embodied, non-articulated competence. Excluded from the planning conversation because their knowledge cannot be easily transferred to simulator software or regulatory metrics. If heard, they would attest that simulation-trained personnel lack crucial tacit responses and that competence maintenance requires periodic real-event exposure.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, tacit_knowledge_holders, excluded,
    moderate, biographical, identity_locked, global).

% NTSB, accident investigation boards. Analyze failures post-incident. They observe the pattern: personnel trained exclusively on simulators show competence gaps in real catastrophes that simulators could not replicate. Their findings are archived, then forgotten; the next training cycle reverts to simulation assumption.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_aftermath_investigators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading describes a psychological/physiological limit, not a coordination problem. Catastrophic events are not being coordinated; they are naturally-occurring stress events that the constraint claims cannot be replicated.
% TRANSFER_FUNCTION: The transfer is opportunity cost: resources spent on simulation training (which this reading holds as insufficient for competence maintenance) cannot be spent on alternative methods (live-risk apprenticeship, periodic real-operation exposure, tacit-knowledge transfer). The beneficiary is the simulationist worldview; the payer is operational safety margin.
% ABSENT_VOICES: Experienced catastrophe-response operators who have felt the gap between simulator performance and real-event performance. They are excluded because their tacit knowledge cannot be formalized into regulatory policy, simulator specifications, or training metrics. If present, they would testify that simulation training generates false confidence in competence.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if real catastrophes somehow ceased to be irreducible, if simulation became demonstrably sufficient—nothing would reorganize. The organizational landscape would simply shift: simulator investment would expand, real-operation training rotations would shrink, competence maintenance would rely purely on simulation fidelity curves. The disappearance would be semantic (the constraint would no longer describe reality), not structural.
% FOUNDING_PROBLEM: How do high-reliability organizations maintain operational competence across decades-long periods when genuine catastrophes are rare or forbidden by safety doctrine? Catastrophic events provide irreducible stress and uncertainty that no simulation can fully instantiate; without them, competence erodes, particularly the tacit, embodied, stress-response components.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation reports (NTSB, Aviation Safety Board analyses of incidents where simulator-trained personnel showed critical gaps) and post-catastrophe analyses from aerospace, nuclear, and emergency-response fields all attest to the founding problem. Cognitive science research on stress inoculation and simulation fidelity limits corroborates the claim outside the benefiting parties (simulation vendors do not fund research showing simulation is insufficient). The problem remains live as long as high-reliability organizations maintain their competence requirements across catastrophe-free periods.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, world_unchanged).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint imposes opportunity costs—resources spent on expensive but allegedly-insufficient simulation training cannot be spent on alternative methods (real-operation apprenticeship, mentoring by catastrophe-experienced personnel, live-event exposure). Suppression (0.71) is higher still because the constraint's persistence depends on suppressing accurate assessment: both regulatory bodies (who mandate simulation to save costs and avoid catastrophe exposure) and simulator-trained personnel (who must believe simulation maintains competence) actively suppress doubt. Theater ratio (0.42) is moderate-high because simulator training performs competence-building activity (exercises, metrics, certification) while the constraint claims the actual competence maintenance function cannot be delivered by simulation. The measurement series show rising extractiveness and theater from t=0 to t=30, then plateau: as simulation technology and investment mature, the false-confidence trap deepens (higher theater), but the underlying competence gap remains (extractiveness plateaus at natural limit). This is the signature of a mountain with rising institutional capture—the natural law (catastrophe necessity) combines with organizational dynamics (simulator-industry incentives, regulatory capture) to produce sustained extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the simulator-vendor and regulatory seat, this is a technology-roadmap problem: fidelity is improving, sufficiency is approaching, and the constraint will dissolve as fidelity crosses threshold. From the high-reliability-operator and catastrophe-survivor seats, the constraint is already saturated—simulation will never bridge the gap because tacit knowledge and true stress response require irreducible contingency. The engine should compute this divergence from the structural data: vendors and regulators have high exit_options (mobile, constrained), while operators have trapped exit_options and identity_locked personnel; their different power and exit positions should produce different computed types even on the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation technology providers are beneficiaries (d near 0.0): they collect revenue from training mandates that assume simulation suffices, and every endorsement of simulation as sufficient drives investment. High-reliability operators are payers (d near 1.0): they bear the costs of inadequate competence maintenance and the asymmetric constraint that they cannot run real catastrophes for training. Regulatory bodies sit near payer (d=0.6–0.7) because they enforce the simulation mandate they benefit from (cost savings) while bearing the long-term risk of competence degradation. Personnel are deeply trapped: high d (0.8+) because they must undergo repeated simulator training and carry false confidence into real events, but also secondary beneficiary (d dual-positioned) because they initially experience training completion as competence gain. This dual positioning captures a critical dynamic: simulator training provides illusory competence until catastrophe exposes the gap—the beneficiary status is real in the training period, the payer status becomes visible only in failure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT exhibit mandatrophy in the classical sense (mandate outlived its function). Instead, it exhibits false-naturalness capture: the founding problem (how to maintain competence across catastrophe-free periods) is real and live, but the solution encoded in the constraint (simulate, assume simulation suffices) is held aloft by institutional incentives rather than by evidence. Regulatory mandates enforce simulation, not because simulation is proven sufficient, but because simulation is cheaper than alternatives (real-operation training rotations, mentoring by experienced personnel). The constraint persists not because competence is actually maintained, but because failure only emerges in actual catastrophes (rare), and when failure is discovered post-incident, the same cycle restarts. This is closer to piton (performance of competence maintenance without actual delivery) than to mandate obsolescence—the function the constraint was built for (competence maintenance) persists as live, but the method (simulation) is captured by vendor and cost incentives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_of_psychological_limit,
    'Is the claim that simulation cannot maintain competence across catastrophe-free periods a natural law of human psychology and stress physiology, or a contingent fact about current simulation technology and training regimes?',
    'Longitudinal studies comparing competence decay in simulation-trained vs. real-operation-exposed cohorts, controlling for inherent ability and motivation. Alternatively, technological advancement: if fidelity reaches threshold where simulation-induced stress physiologically matches real catastrophe, the constraint would degrade from Mountain to technology-dependent.',
    'If natural law (human stress response requires irreducible contingency), the constraint holds as Mountain regardless of technology; if contingent, it becomes a specification problem for simulation engineering and the constraint downgrades to snare/tangled_rope (simulator industry captures policy, suppresses investment in alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_of_psychological_limit, empirical, 'Whether simulation''s insufficiency is a psychological universal or a current-technology limitation.').

omega_variable(
    tacit_knowledge_formalizability,
    'Can the tacit, embodied knowledge held by catastrophe-experienced operators be captured, formalized, and transferred via simulator training, or is it fundamentally tacit and transmissible only through direct mentoring and live-event exposure?',
    'Experimentation: train two cohorts identically except one includes mentoring from catastrophe-experienced operators and one includes simulator-only. Test competence under realistic stress conditions. Alternatively, linguistic/cognitive analysis of whether tacit responses can be decomposed into formalizable rules.',
    'If formalizable, simulation could in principle capture all needed competence (constraint reduces to engineering problem). If irreducibly tacit, the constraint stands: simulation cannot substitute for real-event learning and mentorship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_formalizability, conceptual, 'Whether catastrophe-induced tacit knowledge is transferable via simulation or only via direct experience.').

omega_variable(
    beneficiary_identity_ambiguity,
    'Is ''catastrophe as irreducible epistemic constraint'' a beneficiary in the conventional sense (an agent that collects from the arrangement), or a vindicated proposition that operates differently—neither collecting nor bearing costs, but being proven true by the arrangement''s persistence?',
    'Clarify the metaphysical status: if catastrophe-necessity is a physical/psychological law, it collects nothing and should migrate to vindicated_propositions. If it is a social commitment (high-reliability organizations embrace catastrophe necessity as a doctrine), it becomes a beneficiary with institutional backing. The schema accepts beneficiaries on mountains when FSM is intended; the question is whether this is FSM-legitimate or a category error.',
    'If proposition (not beneficiary), remove from beneficiaries[], add to vindicated_propositions[], and verify the FSM omega requirement is met. If institutional beneficiary, accept the FSM charge and document which entities benefit from catastrophe-necessity doctrine being accepted as true.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, conceptual, 'Whether catastrophe-necessity is a natural law (vindicated proposition) or an institutional doctrine with beneficiaries (FSM candidate).').

omega_variable(
    kernel_contest_framing,
    'This reading interprets the kernel (''catastrophe_proxy_sufficiency'') as asserting that catastrophes are irreplaceably necessary for competence maintenance. The sibling readings dispute this: hybrid_degradation claims partial substitution is possible over generational timescales; simulation_as_proxy claims substitution is complete; simulation_fidelity_threshold claims sufficiency is technology-dependent. What frames each reading and determines which is true?',
    'The readings cannot be simultaneously true within any single framework. The kernel contest is about whether real catastrophes are categorically irreplaceable (this reading) or whether their necessity is contingent on technology, training regime, or organizational choice (siblings). Resolution requires either: (a) empirical evidence of competence trajectories, or (b) clarification of which reading''s frame is the authorized interpretation of the kernel (if the kernel is a formal regulation or doctrine, its interpreter''s reading determines truth).',
    'If this reading is upheld, catastrophe avoidance and competence maintenance are in structural tension, and organizations face irreducible tradeoff. If a sibling reading prevails, competence can be maintained indefinitely through simulation or hybrid methods, resolving the tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether real catastrophes are categorically irreplaceable or whether sufficiency is technology/regime-dependent.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) of catastrophe-necessity primarily structural (organizations are prevented by safety doctrine and liability concerns from running real catastrophes for training) or internalized (personnel believe simulation suffices so deeply that they suppress their own experiential doubts)?',
    'Post-incident analysis: do personnel who survived real catastrophes after simulator-only training report that their doubts about simulation were suppressed by institutional pressure, or by their own internalized belief? Alternatively, compare post-catastrophe testimony in jurisdictions with different liability regimes (where real-event training is legal vs. forbidden).',
    'If structural, the constraint is maintained by external barriers that could be removed. If internalized, personnel carry the suppression into real events, creating a positive-feedback loop where simulation-induced false confidence makes catastrophe more likely and more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of catastrophe-necessity doubt is externally imposed or self-maintained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cnr_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cnr_tr_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(cnr_tr_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(cnr_tr_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(cnr_tr_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(cnr_tr_t25, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(cnr_tr_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cnr_tr_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cnr_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cnr_be_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cnr_be_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(cnr_be_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(cnr_be_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cnr_be_t25, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cnr_be_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cnr_be_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cnr_su_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cnr_su_t5, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(cnr_su_t10, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(cnr_su_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(cnr_su_t20, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(cnr_su_t25, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(cnr_su_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(cnr_su_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This reading is one of four constraints in the catastrophe_proxy_sufficiency kernel family. All four share a common referent (how to maintain high-reliability competence across catastrophe-free periods) but instantiate different readings of whether real catastrophes are categorically necessary or contingently so. This reading (catastrophe_necessity_reading) claims categorical necessity and is claimed as Mountain; it forecloses simulation_as_proxy and influences the hybrid and threshold readings. Each reading has its own ε derived from its own authority-framing and its own victim set (competence gap vs. simulation investment inefficiency vs. generational knowledge loss vs. fidelity boundary crossing). They are NOT one constraint viewed from multiple angles—they are four genuinely distinct constraints with different structural implications, linked by kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
