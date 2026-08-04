% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: High-Fidelity Simulation as Competence-Maintenance Standard
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel about
 *   competence maintenance in high-stakes, low-frequency-event domains. The
 *   kernel is 'how should organizations maintain genuine competence in
 *   catastrophe-avoidance when real catastrophic events are (rightly)
 *   prevented?' This reading claims that high-fidelity simulation constitutes
 *   sufficient genuine exercise of competence — the cognitive and procedural
 *   demands are structurally equivalent to real events. This is the
 *   training-infrastructure-dominant reading: it makes simulation the primary
 *   competence-validation mechanism, centralizes standard-setting in training
 *   operators and regulators, and relies on simulator fidelity improvements
 *   to stay aligned with real-world demands. The competing readings
 *   (catastrophe_as_necessary, near_miss_as_bridge) dispute whether
 *   simulation can ever be sufficient without real-event data or
 *   real-incident feedback. This story authors ONLY the
 *   simulation-as-sufficient reading and routes the disagreement to omega
 *   variables and cs_structure, per the kernel-reading rules.
 *
 * KEY AGENTS:
 *   - Training infrastructure operators: institutional authority, set simulator standards and certifications
 *   - Regulatory authorities: adopt simulator performance as official competence proxy
 *   - Field operators: must pass simulators but experience identity lock to simulator metrics
 *   - Organizational leadership: benefits from cost-controlled, auditable training
 *   - Near-miss advocates: excluded, argue real-world feedback is irreplaceable
 *   - Catastrophe-learning advocates: silenced, claim only real events drive genuine learning
 *   - Families at risk: excluded entirely from standard-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.62).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.71).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "High-Fidelity Simulation as Competence-Maintenance Standard").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4').
narrative_ontology:cs_kernel_codification('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', distributed).
narrative_ontology:cs_authority_grounding('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', extraction).
narrative_ontology:cs_interpretation_layer_present('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4').
narrative_ontology:cs_reading_relation('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_reading_relation('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', foundational, cognitive_procedural_equivalence_possible).
narrative_ontology:cs_axiom_status(cognitive_procedural_equivalence_possible, holdable).
narrative_ontology:cs_axiom_grounding('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', cognitive_procedural_equivalence_possible, empirically_contingent).
narrative_ontology:cs_axiom('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', foundational, simulator_fidelity_sufficient_for_authority).
narrative_ontology:cs_axiom_status(simulator_fidelity_sufficient_for_authority, holdable).
narrative_ontology:cs_axiom_grounding('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', simulator_fidelity_sufficient_for_authority, instrumental).
narrative_ontology:cs_reference_frame('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', training_infrastructure_as_primary_competence_validator).
narrative_ontology:cs_drift_state('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', contemporary_post_catastrophe_questioning, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8f6de636-bf06-4f64-8ac5-8f6d9e8d32f4', '2026-06-20T14:32:18Z').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_authorities).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, organizational_leadership).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, field_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_responders).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, organizational_reliability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, field_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_design_engineers).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, insurance_and_compliance_auditors).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, simulator_fidelity_adequacy_doctrine).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, cognitive_equivalence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, maintains, and operates high-fidelity simulation facilities and certification programs. Sets the official standard that simulator performance equals real-world competence. Collects budget allocation, institutional authority, and career advancement by managing the training infrastructure as the primary competence-validation mechanism. Sets pass/fail standards for certifications.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Adopts simulator performance metrics as the official competence standard, reducing direct oversight burden and centralizing measurement. Benefits from the appearance of systematic, scalable competence assurance without deploying regulatory resources into real-world event monitoring. Becomes dependent on trainer certification as the legitimate proxy for competence.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_authorities, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, regulatory_authorities, agenda_setter).

% Must achieve certification via simulator performance to maintain employment and licensing. May retain genuine competence through simulation, but also bears the cost if simulator design gaps go undetected — actual performance in real events exposes whether sim training matched real-world demands. Identity as 'certified professional' becomes fused with simulator-passing performance metrics rather than real-world outcomes.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, field_operators, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, field_operators, beneficiary).

% Benefits from a cost-controlled, centralized, auditable training regime that can be documented for compliance and insurance purposes. Reduces direct exposure to catastrophic events by investing in simulation instead of operational redundancy or conservative real-world practice. Can claim systematic competence maintenance through metrics.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, organizational_leadership, beneficiary,
    powerful, biographical, mobile, global).

% Argue that near-miss events and minor failures provide irreplaceable real-world feedback that simulators miss; are structurally excluded from setting competence standards because their framework (learning-from-failure) conflicts with the dominant framework (competence-from-certified-training). Would advocate for near-miss reporting and response as competence validation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, near_miss_advocates, excluded,
    moderate, biographical, constrained, global).

% Hold that actual catastrophic events, though tragic, are the only mechanism for genuine organizational learning and the only way to validate whether competence is real or theatrical. Are silenced by the institutional consensus that catastrophes should be prevented; their experiential account of how organizations actually learn from disasters is systematically devalued.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, catastrophe_necessary_advocates, excluded,
    powerless, biographical, trapped, global).

% Benefit from continued investment in simulator development and refinement. Career advancement and research funding depend on the assumption that simulator fidelity can be continuously improved toward perfect equivalence. Benefit from the closure of their field against real-event-based validation (which would expose simulator gaps).
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_design_engineers, beneficiary,
    organized, generational, mobile, global).

% Can audit and certify competence against documented simulator performance metrics rather than conducting invasive on-site observation or waiting for real-world performance data. Benefit from a legible, standardized, defensible assurance process.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, insurance_and_compliance_auditors, beneficiary,
    institutional, biographical, mobile, global).

% Are excluded from the competence-validation process entirely. Their safety depends on whether the simulation-validated competence transfers to real events, but they have no seat at the standard-setting table and no voice in whether near-miss or catastrophic-event data should override simulator metrics.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, families_of_potential_casualties, excluded,
    powerless, immediate, trapped, global).

% Analyzes whether the simulation-as-sufficient reading is actually measuring competence or performing competence; whether field outcomes diverge from simulator predictions; whether the constraint serves organizational learning or replaces it.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, organizational_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_operators).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a centralized, scalable mechanism for maintaining competence in high-stakes, rare-event domains where real-world training is prohibitively dangerous or costly. Simulation solves the collective-action problem of continuous competence maintenance without requiring organizations to experience or allow preventable catastrophes.
% TRANSFER_FUNCTION: Transfers responsibility for competence validation from real-world performance observation to simulator performance metrics. Transfers authority for standard-setting from frontline responders and near-miss investigators to training infrastructure operators and regulatory authorities. Transfers the burden of proof from 'demonstrate real competence in the field' to 'pass the certified simulator test.'
% ABSENT_VOICES: Field operators who have survived real catastrophes and near-miss events are systematically excluded from standard-setting. Organizations that advocate for near-miss reporting as a primary learning mechanism are marginalized. Families and communities at risk from incompetence are not represented. Simulator-design critics (who would argue fidelity gaps exist and cannot be closed by design alone) are excluded from legitimacy conversations.
% DISAPPEARANCE_RATIONALE: If this reading vanished and organizations reverted to learning from near-misses and real events, certification standards would shift; regulatory focus would move to real-world performance monitoring; training budgets would reorient toward response to actual incidents rather than proactive simulator investment; organizational culture would shift from 'pass the test' to 'learn from failure'; professional identity would decouple from simulator metrics.
% FOUNDING_PROBLEM: High-stakes, low-frequency domains (nuclear operations, aviation safety, emergency response) face a dilemma: competence requires rehearsal of rare catastrophic scenarios, but allowing those scenarios to occur for training purposes is ethically unacceptable and operationally catastrophic. Simulation was built to solve this by providing safe, repeatable rehearsal of decision-making under high-stakes conditions without real-world consequences.
% FOUNDING_PROBLEM_CORROBORATION: Training infrastructure operators and regulatory authorities attest the founding problem is still live and simulation is the solution. Field operators and catastrophe-response researchers from outside the training establishment attest the founding problem is being REPLACED with a different problem: whether simulator performance actually predicts real performance. Independent studies (Dekker, Leveson, Cook on safety) and near-miss analysis programs (NASA ASRS, FAA CALLBACK) from outside the training beneficiary set argue that the founding problem solution is incomplete — simulators cannot capture the organizational learning that only real events and near-misses provide.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
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
 *   Extractiveness rises over the 40-year interval (0.38 → 0.62) as investment in simulator infrastructure grows and field operators become increasingly dependent on simulator certification for employment and licensing — the constraint's power over operators increases as alternatives (real-world demonstration of competence) are foreclosed. Theater rises sharply (0.32 → 0.58), indicating that a growing share of competence-validation activity is performative — passing the test — rather than exercising competence. Suppression remains high (0.55 → 0.71) because the constraint's persistence requires suppressing and marginalizing real-world event data and near-miss learning mechanisms that would contradict the simulator-sufficiency claim. Accessibility_collapse (0.68): operators cannot easily exit the simulator standard without losing licensure and employment, but the collapse is not total because real-world competence demonstration remains theoretically available (the constraint enforces simulator testing, not prohibition of field learning). Resistance (0.55): substantial resistance comes from incident-response communities, safety researchers, and catastrophe-learning advocates who argue for near-miss or real-event-based validation; however, institutional investment in simulators creates inertia that dampens the resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (training operators, regulators), the constraint is genuine coordination: it solves the founding problem (how to maintain competence without allowing catastrophes). From the payer seats (field operators, near-miss advocates), the constraint is extraction: it centralizes authority in infrastructure operators, measures competence by metrics that may not track real performance, and suppresses the learning mechanisms (real incidents, near-misses) that operators trust. The engine computes this divergence from the structural data. The authored claim (rope) reflects the training-operator reading; the metrics (high theater, rising extraction, suppression) reflect the payer reading. The gap IS the measurement the framework takes: a claimed rope with metrics suggesting extraction-with-suppression is a tangled_rope or snare candidate that the engine will detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Training infrastructure operators are structural beneficiaries (d ≈ 0.1–0.2): they collect budget allocation, career advancement, and institutional authority; they have arbitrage exit (can shift to other training domains). Regulatory authorities are beneficiaries (d ≈ 0.15–0.25): they reduce direct oversight burden by delegating to simulator metrics; they are institutionally powerful. Organizational leadership are beneficiaries (d ≈ 0.15–0.25): training is cost-controlled and auditable relative to operational redundancy. Field operators are high-target seats (d ≈ 0.75–0.85): they must pass simulators to maintain employment; identity_locked exit (professional identity fused to certification); moderate power (negotiating capacity is limited). Near-miss advocates and catastrophe-learning advocates are victims (d ≈ 0.80–0.90): their knowledge is systematically devalued; they are excluded from standard-setting; their voice is suppressed. Families at risk are structural victims (d ≈ 0.90–1.0): they bear the cost if simulator-validated competence is insufficient in real events but have zero voice in the process.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is live (organizations do need to maintain competence without causing catastrophes) but the constraint's function has shifted: it is now about centralizing authority in training infrastructure rather than purely about competence maintenance. The reading's original mandate — 'maintain competence safely' — remains, but it has been overlaid with extraction (training-operator authority, field-operator identity lock, suppression of alternative learning pathways). The theater_ratio rise (0.32 → 0.58) indicates growing theatrical maintenance: compliance certifications, simulator metrics, documentation of training — the visible apparatus of competence assurance — grow while the constraint's actual competence-transfer function remains contested and unvalidated. A mandatrophy reading would hold that the constraint persists not because simulator training maintains competence but because it maintains the institutional authority structures and budgets built on that claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_fidelity_closure,
    'Can high-fidelity simulation ever achieve complete cognitive and procedural equivalence to real catastrophic events, or are there irreducible dimensions of real-world knowledge (organizational dynamics, material surprises, visceral consequence) that simulators structurally cannot replicate?',
    'Comparative analysis of field-operator performance post-incident vs. simulator performance pre-incident in matched real-event domains (aviation accidents, nuclear events, emergency responses). If simulator-certified operators systematically perform differently in real events than simulator data predicts, fidelity closure is incomplete.',
    'If fidelity closure is impossible, the constraint''s claim that simulation is sufficient collapses; the reading reframes to ''simulation + real-world feedback mechanisms is necessary.'' If fidelity closure is possible in principle, the reading''s structural ground holds but depends on continuous simulator improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_fidelity_closure, empirical, 'Whether high-fidelity simulation can achieve structural equivalence to real-world competence demands or whether a residual gap is irreducible.').

omega_variable(
    organizational_learning_pathway_divergence,
    'Does organizational learning about competence maintenance occur primarily through controlled simulator training and refinement, or primarily through real-world incident response, near-miss analysis, and post-event investigation?',
    'Historical and organizational analysis of where competence improvements originate: (a) simulator redesigns triggered by internal feedback loops, or (b) simulator redesigns triggered by real-event data and near-miss findings. Compare learning-generation rates across organizations with high simulator investment vs. high near-miss reporting investment.',
    'If most improvements come from real-world events and near-misses, then simulation is a theater of rehearsal but not the primary learning mechanism; competence retention depends on continuing real-world data flow, not simulation closure. If improvements come from simulator iteration, the reading''s coordination function holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_learning_pathway_divergence, empirical, 'Whether high-fidelity simulation is the primary source of competence improvement or a secondary reinforcement of learning driven by real-world feedback.').

omega_variable(
    kernel_reading_committer_grounding,
    'Is the assertion that ''simulation is sufficient'' grounded in a claim about cognitive equivalence (empirical question about learning mechanisms) or about legitimate institutional authority (committer question about who has the right to set competence standards)?',
    'Examine whether the constraint persists if simulator performance diverges from real-world performance — does regulatory authority shift back to real-world performance observation, or does it remain locked to simulator metrics despite divergence? If the latter, the constraint is fundamentally about authority, not equivalence.',
    'If the constraint is authority-based (institutional decision that simulation counts as sufficient), then it coexists with the catastrophe_as_necessary reading without logical foreclosure. If empirical equivalence is the grounding, the readings genuinely foreclose each other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_grounding, conceptual, 'Whether this reading grounds in cognitive equivalence (can be refuted empirically) or institutional authority (a committer choice that survives divergence).').

omega_variable(
    identity_lock_internalization,
    'Is the suppression of near-miss-based and catastrophe-based learning sustained by external institutional barriers (rules preventing incident reporting) or by internalized acceptance of simulator performance as the legitimate competence measure?',
    'Exit analysis: if field operators gain access to real-incident and near-miss data through regulatory or cultural changes, do they realign competence standards toward learning-from-incidents, or do they remain locked to simulator metrics? Sustained lock despite access indicates internalization.',
    'If suppression is partially internalized, then field operators'' identity_locked exit is sustained by their own belief in simulator legitimacy — breaking the constraint requires identity-reframing, not just structural changes. If suppression is purely structural, regulatory or institutional change could shift operators'' allegiance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether field-operator suppression of real-world learning pathways is structurally enforced or cognitively internalized as acceptance of simulator-performance legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 25, 0.55).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 30, 0.57).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.18).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_retention_exercise kernel. It instantiates the position that high-fidelity simulation is a sufficient competence-maintenance mechanism. The sibling readings (catastrophe_as_necessary, near_miss_as_bridge) are separate constraint stories with different ε values, different stakeholder beneficiary/victim structures, and different computed types. The three stories together form a constraint family linked by the shared kernel: they are not alternative measurements of one constraint but rather structurally distinct constraints each instantiating a different answer to 'how should organizations maintain competence in rare, catastrophic scenarios?' See commentary.kernel_context for the reading contest details.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__simulation_as_sufficient, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
