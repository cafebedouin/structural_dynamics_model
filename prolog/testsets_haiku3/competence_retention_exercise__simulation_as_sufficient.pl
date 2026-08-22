% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: High-Fidelity Simulation as Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Safety-critical organizations train operators using high-fidelity
 *   simulators to exercise catastrophe-avoidance competence without waiting
 *   for real catastrophes. This reading asserts that cognitive and procedural
 *   demands in simulators are structurally equivalent to real events, making
 *   simulation a sufficient competence-maintenance mechanism. The constraint
 *   extracts value by centralizing competence validation through simulator
 *   performance metrics while suppressing alternative learning mechanisms
 *   (catastrophe-response learning, near-miss investigation, blended
 *   approaches). Extraction is moderate because the coordination problem
 *   (safe competence maintenance) is genuine; suppression is moderate because
 *   practitioners can still learn through catastrophes or near-misses, though
 *   the institutional framing discredits those pathways.
 *
 * KEY AGENTS:
 *   - Training infrastructure operators: Set and administer simulator curricula; institutional power; benefit from continued simulator dependence
 *   - Front-line operators: Forced to perform competence through simulator metrics; identity-locked to training establishments; dual-positioned as beneficiary (safer training) and payer (never experience real high-consequence decisions)
 *   - Safety-critical workforce: Powerless actors who depend on trained operators' competence; bears risk; no voice in training design
 *   - Equipment manufacturers: Powerful; profit from upgrade cycles and market lock-in; incentivized to resist evidence of simulator limitations
 *   - Catastrophe-prevention skeptics: Excluded; argue real catastrophes are necessary for genuine competence; marginalized from curriculum authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.62).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.41).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "High-Fidelity Simulation as Competence Maintenance").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, 'd63b28c3-c515-4e07-bcc0-2ae2edcc9480').
narrative_ontology:cs_kernel_codification('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', distributed).
narrative_ontology:cs_authority_grounding('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', extraction).
narrative_ontology:cs_interpretation_layer_present('d63b28c3-c515-4e07-bcc0-2ae2edcc9480').
narrative_ontology:cs_reading_relation('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_reading_relation('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', foundational, cognitive_equivalence_doctrine).
narrative_ontology:cs_axiom_status(cognitive_equivalence_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', cognitive_equivalence_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', foundational, fidelity_sufficiency_thesis).
narrative_ontology:cs_axiom_status(fidelity_sufficiency_thesis, holdable).
narrative_ontology:cs_axiom_grounding('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', fidelity_sufficiency_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', simulator_fidelity_adequate_for_competence).
narrative_ontology:cs_drift_state('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', contemporary_incident_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d63b28c3-c515-4e07-bcc0-2ae2edcc9480', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulation_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, organizational_learning_establishment).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, front_line_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, safety_critical_workforce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, front_line_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, maintains, and certifies high-fidelity simulators as the authorized competence-maintenance mechanism. Sets curriculum, defines passing performance metrics, and certifies operators as competent based on simulator performance. Controls what constitutes evidence of readiness and what stays hidden from real operations.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Must train exclusively through simulators and never experience real high-consequence events unless catastrophe occurs naturally. Bears the cognitive and career costs of performing competence through metrics that may not capture real-world judgment. Depends on simulator validity for their own and others' safety but has no seat in determining what the simulator tests.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, front_line_operators, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, front_line_operators, beneficiary).

% Profits from sustained simulator deployment, upgrade cycles, and market lock-in. Revenue depends on continuous assertion that simulation remains state-of-the-art and indispensable. Has incentive to resist evidence that near-misses or catastrophes reveal simulator limitations.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulation_equipment_manufacturers, beneficiary,
    powerful, biographical, mobile, global).

% The broader population of safety-critical workers (nurses, air traffic controllers, nuclear technicians, surgeons) who depend on the competence of trained operators. Bears the risk if simulator training is insufficient but has no voice in its design or validation. Cannot exit the system that trains the people their safety depends on.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, safety_critical_workforce, payer,
    powerless, immediate, trapped, global).

% Academic and institutional research communities whose funding, publication record, and professional identity depend on the simulator-as-sufficient model. Publishes papers validating simulator fidelity; funding flows to refinement of simulation rather than investigation of its limits.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, organizational_learning_establishment, beneficiary,
    institutional, generational, analytical, national).

% Investigates accidents and incidents after they occur. May discover that operator training via simulation was inadequate, but their findings come post-hoc and are often attributed to 'human error' rather than training validity. Regulatory incentives favor accepting simulator certification rather than mandating expensive overhauls.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, incident_investigation_authorities, observer,
    institutional, generational, analytical, national).

% Practitioners, researchers, and safety advocates who argue that real catastrophes or near-misses are necessary to maintain genuine competence and that simulator-only training creates brittleness. Excluded from curriculum design and certification authority; their views are marginalized as 'defeatist' or 'risk-accepting.'
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, catastrophe_prevention_skeptics, excluded,
    moderate, biographical, constrained, global).

% Advocates for robust near-miss investigation and integration into training curricula. Argue that minor failures provide real-world feedback loops that simulators cannot fully capture. Excluded from resource allocation and curriculum priority-setting; funding flows to simulation rather than systematic near-miss analysis.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, near_miss_investigation_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_operators).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a stable, controlled, scalable mechanism for exercising safety-critical competence without waiting for catastrophes to occur naturally. Provides consistent exposure to high-consequence decision-making across a distributed workforce without requiring everyone to experience real accidents.
% TRANSFER_FUNCTION: Transfers authority over competence validation from individual operator judgment and real-world experience to simulator performance metrics and institutional certification. Moves resources (training budgets, career advancement, legitimacy) from catastrophe-response learning to simulation-based learning infrastructure.
% ABSENT_VOICES: Operators who have experienced real catastrophes and near-misses and can articulate what simulators do not capture; safety-critical populations (patients, passengers, public) who depend on trained operators but have no input into training adequacy; practitioners in domains where near-miss investigation revealed simulator gaps.
% DISAPPEARANCE_RATIONALE: If high-fidelity simulation vanished as the mandated training mode, the field would splinter: some would revert to learning-by-catastrophe (slow, high-cost); others would shift to mandatory near-miss investigation (costly but calibrated); some would demand blended approaches. The catastrophe-prevention ecosystem would reorganize, but not uniformly — the reading itself is contested.
% FOUNDING_PROBLEM: Safety-critical competence requires exposure to high-consequence decision-making and rare scenarios that occur unpredictably. Real catastrophes cannot be relied on for training (they are random, fatal, and reveal failure post-hoc). A scalable mechanism was needed to exercise catastrophe-avoidance competence deliberately and repeatedly across the entire workforce.
% FOUNDING_PROBLEM_CORROBORATION: The training establishment attests the founding problem is still live and severe; simulator operators cite the rarity and fatality of real events as justification for simulation's necessity. Practitioners and incident investigators attest the problem is partly solved but at the cost of competence brittleness — they report cases where simulator training did not prepare operators for real-world sensory complexity, time pressure, equipment degradation, or social dynamics. The contest is not whether the founding problem existed, but whether simulation solved it or only shifted the competence-maintenance problem onto a different axis.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, contested).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) reflects that training infrastructure operators and manufacturers collect sustained resources, authority, and legitimacy by asserting simulator sufficiency, while front-line operators pay by ceding competence validation to metrics rather than judgment. The constraint is gently extractive rather than highly extractive because the coordination problem is real — high-consequence training is genuinely needed — and many operators do benefit from safe, repeatable exposure. Theater ratio (0.48) indicates that roughly half of simulator activity is maintenance of the certification framework rather than genuine competence exercise. Suppression (0.41) is moderate because the constraint does NOT use coercion to prevent real-world learning; instead, it uses institutional legitimacy and resource allocation to make alternative learning pathways costly and professionally risky. Measurement trajectory: extraction and theater both plateau after t=30, indicating the constraint has reached stable institutional form; suppression requirement is flat throughout, indicating enforcement burden does not increase despite the extractive creep — the suppression is structural (authority-based) rather than active (force-based). Accessibility collapse (0.72) is moderately high because once an operator is certified via simulator, real-world alternatives (catastrophe learning, near-miss investigation) become professionally unavailable — the simulator-success path absorbs all legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   Payer seat (front-line operators): simulation is competence exercise IF high-fidelity is genuine and IF metrics capture real judgment. If fidelity is incomplete or metrics are gaming-prone, the constraint becomes certification theater. Beneficiary seat (training operators): the constraint is coordination because scalable, safe training is the alternative to unpredictable catastrophe learning. Observer seat (incident investigators): the constraint works as long as catastrophes remain rare; if simulator-trained operators fail in real events, the constraint reveals itself as false-sufficient.
 *
 * DIRECTIONALITY LOGIC:
 *   Training infrastructure operators hold institutional power, arbitrage-grade exit (can shift to other industries), and collect the legitimacy and resources of the constraint — directionality near 0.0 (full beneficiary). Front-line operators hold moderate power, identity-locked exit (professional identity fused to training credentials), and bear dual costs (validation through metrics, no real-world learning) — directionality near 0.5 (symmetric, with slight tilt toward target given identity-lock). Safety-critical workforce holds powerless position, trapped exit (cannot choose who trains their protectors), and bears distributed risk — directionality near 1.0 (full target). Manufacturers hold powerful position, mobile exit, and profit from constraint sustainability — directionality near 0.1 (light beneficiary). Catastrophe skeptics are excluded and hold moderate power/constrained exit — directionally ambiguous, not authored into beneficiary/victim, so derivation is indeterminate; commentary-level treatment only.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy risk is present but not yet manifest. The founding problem (safe competence maintenance for rare, high-consequence scenarios) is genuinely live. The founding problem's status is 'contested' because practitioners and investigators dispute whether simulation alone solves it or only shifts the competence-maintenance problem. The disappearance verdict is 'contested' for the same reason: if simulators vanished, some would say competence training must find another mechanism (catastrophe-response or near-miss blending); others would say catastrophes cannot be prevented without simulators. The constraint avoids pure mandatrophy because it solves a real coordination problem. It risks creeping toward mandatrophy if: (1) near-miss investigation reveals simulator gaps but the findings are suppressed by institutional investment in simulators; (2) real catastrophes occur to simulator-trained cohorts and the simulator industry responds by asserting the training was correct but the operator failed; (3) theater ratio rises above 0.6, indicating the constraint sustains itself through certification rather than through real competence gains. Current metrics do not yet indicate mandatrophy — the constraint is a working rope with extractive overlay, not a zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_gap_vs_claimed_equivalence,
    'Do high-fidelity simulators truly provide cognitive and procedural demands equivalent to real catastrophic events, or is there a systematically unmeasured gap (time pressure, equipment failure cascades, social dynamics, sensory feedback) that simulators do not capture?',
    'Post-hoc analysis of catastrophes/serious incidents occurring to simulator-trained cohorts: if operators systematically fail at specific real-world dimensions not present in simulator training, fidelity is incomplete. Matched-cohort studies comparing simulator-trained vs. catastrophe-exposed operators on subsequent performance.',
    'If substantial equivalence holds, the reading''s core axiom (cognitive_equivalence_doctrine) is vindicated and simulation is genuinely sufficient. If gap is systematic and large, the axiom is challenged and the constraint becomes false-sufficient theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_gap_vs_claimed_equivalence, empirical, 'Whether cognitive equivalence between simulator and real catastrophe is genuine or covers unmeasured gaps.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative learning mechanisms (catastrophe-response, near-miss investigation, blended approaches) structural (institutional barriers, resource allocation, legitimacy denial) or internalized (operators have adopted the simulator-sufficiency narrative and believe alternatives are irrational)?',
    'Ethnographic study of operators in jurisdictions that mandate near-miss investigation alongside simulators: do operators embrace the blended approach, or do they experience cognitive dissonance and default to simulator authority? If suppression is primarily structural, operators will engage near-miss learning when cost/risk is removed. If internalized, operators will resist it even when legitimized.',
    'If structural, the constraint''s effective suppression is lower than the authored 0.41 and can be reduced by institutional redesign. If internalized, the constraint carries deeper cognitive capture and would require cultural shift to undo.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative learning pathways is structural or internalized.').

omega_variable(
    theater_escalation_risk,
    'Is the rising theater_ratio (0.35 → 0.48) evidence of genuine metric creep (simulators developing richer fidelity) or evidence of Goodhart drift (the simulator industry optimizing for measurable performance rather than real competence)?',
    'Monitor whether simulator scenarios evolve to become more ecologically valid (richer time pressure, equipment variance, human-factor complexity) or become more predictable and game-able. Track whether operators who perform well on simulators consistently perform well in real-world performance reviews or only in subsequent simulator sessions. Survey practicing operators for their confidence that simulator training predicted real performance.',
    'If metric creep is genuine escalation of fidelity, the constraint''s rope status holds. If Goodhart drift, the constraint is transitioning toward piton (performative maintenance of a defunct competence framework).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_escalation_risk, empirical, 'Whether theater ratio rise indicates genuine fidelity improvement or Goodhart metric drift.').

omega_variable(
    kernel_reading_underspecification,
    'Is ''simulation_as_sufficient'' a coherent epistemological claim, or is it under-determined by the evidence? Specifically: what counts as ''genuine exercise of competence'' — is it the match between simulator and catastrophe, or is it the operator''s subjective experience, or is it the institutional closure over alternative learning mechanisms?',
    'Conceptual analysis and philosophical reconstruction of what the reading''s core axioms (cognitive_equivalence_doctrine, fidelity_sufficiency_thesis) actually require. Do they rest on observable equivalence, or on a claim about internal experience, or on institutional authority? Once the grounding is specified, empirical tests can be designed.',
    'Different groundings point to different sibling readings: if competence is defined by subjective experience, then catastrophe_as_necessary becomes forceful (no simulation matches the visceral learning). If competence is defined by measurable outcomes, then near_miss_as_bridge becomes competitive (near-misses provide real feedback at lower cost). If competence is defined by institutional closure, the constraint is self-vindicating (it defines competence as simulator performance by definition).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, conceptual, 'Whether ''simulation sufficiency'' is grounded in observable equivalence, subjective experience, or institutional definition.').

omega_variable(
    identity_lock_asymmetry,
    'Front-line operators carry ''identity_locked'' exit — their professional identity is fused to training credentials. Is this identity-lock a feature of the constraint (it requires the lock to maintain control) or an incidental effect (operators could defect but choose not to)?',
    'Case analysis of operators who have advocated for alternative learning mechanisms: were they professionally sanctioned (the constraint enforces lock-in)? Historical analysis of whether the training establishment explicitly cultivated operator identity-fusion or whether it arose as an unintended consequence. Thought experiment: if simulator-trained operators had full professional mobility and reputation preservation while dissenting from simulator sufficiency, would the constraint''s effectiveness decline?',
    'If identity-lock is a feature (intentionally cultivated), the constraint is more extractive than metrics suggest — the extraction includes cognitive capture. If incidental, the constraint is less powerful — operators could coordinate around alternative mechanisms if institutional barriers were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_asymmetry, empirical, 'Whether operator identity-lock is structurally required by the constraint or an unintended byproduct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 5, 0.38).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 10, 0.4).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 15, 0.43).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 20, 0.46).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 25, 0.47).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 30, 0.48).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 30, 0.41).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel shared across three stories: simulation_as_sufficient (this one), catastrophe_as_necessary, and near_miss_as_bridge. The kernel is the commitment to maintaining safety-critical competence through deliberate exposure. Each reading has different ε values, different beneficiary/victim structures, and different type classifications. They are linked because they are alternative institutionalizations of the same founding problem. Decomposition follows ε-invariance: the readings would produce different extraction assessments for the same agents, so they are separate constraints. See cs_structure.reading_relations for structural relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__simulation_as_sufficient, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
