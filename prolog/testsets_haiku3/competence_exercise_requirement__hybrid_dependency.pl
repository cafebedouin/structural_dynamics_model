% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Simulation-Real-World Competence Exercise Requirement
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Commercial aviation and military pilot competence is maintained through
 *   regulatory mandates requiring hybrid training regimes: high-fidelity
 *   simulation provides foundational, repeatable exercise; periodic
 *   real-world anchoring through line operations, non-jeopardy audits, and
 *   actual aircraft time validates transfer-of-training and exercises
 *   judgment under irreducible uncertainty. This constraint story
 *   instantiates the hybrid_dependency reading of the
 *   competence_exercise_requirement kernel — the middle position between
 *   catastrophe_as_necessary_anchor (only real jeopardy trains adequately)
 *   and simulation_as_adequate_exercise (simulation with high fidelity is
 *   sufficient). The hybrid reading asserts that both are needed, neither
 *   alone is adequate, and the arrangement coordinates safety assurance by
 *   rejecting both extremes while accepting significant costs from operators
 *   and simulator centers.
 *
 * KEY AGENTS:
 *   - regulatory_authority: sets and enforces the dual-pathway requirement
 *   - line_operators: bear scheduling and fatigue costs of real-world anchoring
 *   - aircraft_operators: allocate non-revenue time to competence maintenance
 *   - simulator_training_centers: provide necessary but insufficient exercise
 *   - catastrophe_prevention_advocates: hold that only real jeopardy teaches; excluded
 *   - simulation_adequacy_advocates: hold that high-fidelity simulation suffices; excluded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.58).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Simulation-Real-World Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, 'c2e3cb76-8649-4b99-bd72-5b03e4f534e1').
narrative_ontology:cs_kernel_codification('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', formalized).
narrative_ontology:cs_authority_grounding('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', expertise).
narrative_ontology:cs_interpretation_layer_present('c2e3cb76-8649-4b99-bd72-5b03e4f534e1').
narrative_ontology:cs_reading_relation('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_reading_relation('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', competence_exercise_requirement__simulation_as_adequate_exercise, influences).
narrative_ontology:cs_axiom('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', foundational, transfer_of_training_gap_is_real).
narrative_ontology:cs_axiom_status(transfer_of_training_gap_is_real, holdable).
narrative_ontology:cs_axiom_grounding('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', transfer_of_training_gap_is_real, empirically_contingent).
narrative_ontology:cs_axiom('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', foundational, hybrid_necessity_from_irreducible_limitation).
narrative_ontology:cs_axiom_status(hybrid_necessity_from_irreducible_limitation, holdable).
narrative_ontology:cs_axiom_grounding('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', hybrid_necessity_from_irreducible_limitation, instrumental).
narrative_ontology:cs_reference_frame('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', competence_requires_measurable_transfer_validation).
narrative_ontology:cs_drift_state('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', contemporary_commercial_aviation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c2e3cb76-8649-4b99-bd72-5b03e4f534e1', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, safety_assurance_framework).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, line_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, simulator_training_centers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, simulator_training_centers).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, aircraft_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional commitment that competence must be demonstrably maintained through hybrid exercise regimes. Receives validation from dual-pathway evidence: simulation metrics + real-world performance. The framework itself does not pay directly but licenses the regulatory authority that enforces the requirement.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, safety_assurance_framework, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_exercise_requirement__hybrid_dependency, safety_assurance_framework).

% Audits and certifies that pilots meet competence standards. Administers the rule that simulation alone is insufficient and mandates real-world anchoring. Justifies the requirement by appeal to safety data and the psychological science of transfer-of-training. Bears administrative cost of designing, monitoring, and enforcing dual pathways.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Pilot crews and line operations personnel required to maintain real-world anchoring through periodic line audits, non-jeopardy flights, and actual aircraft time allocation. Bear the direct cost: scheduling friction, fatigue risk from non-routine operations, opportunity cost against revenue-generating flights. Cannot opt out without losing certification.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, line_operators, payer,
    organized, biographical, constrained, national).

% High-fidelity simulators are necessary under the hybrid regime but their sufficiency is explicitly rejected by the rule. Must maintain expensive equipment and instructors while accepting regulatory verdict that simulation alone does not constitute competence maintenance. Revenue from simulation training continues but faces structural ceiling because line anchoring cannot be eliminated.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulator_training_centers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, simulator_training_centers, beneficiary).

% Airlines and operators allocate aircraft and crew time to non-jeopardy audits and real-world anchoring exercises. Directly bear the cost of aircraft utilization not generating revenue, crew scheduling complexity, and the risk that real-world operations reveal latent training gaps that disrupt service.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, aircraft_operators, payer,
    powerful, biographical, constrained, global).

% Design curricula and protocols implementing the hybrid regime. Must balance simulation fidelity with real-world transferability, knowing that the rule presupposes neither alone is sufficient. Their expertise is required to operationalize the hybrid requirement but they do not set the mandate.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_program_designers, observer,
    organized, biographical, constrained, global).

% Hold the view that only real catastrophic events or irreversible near-misses exercise competence adequately, and that simulation-based regimes (hybrid or otherwise) cannot capture the irreducibility of actual stakes. Excluded from competence certification standards-setting bodies; their position is treated as empirically unfalsifiable rather than normatively engaged.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, catastrophe_prevention_advocates, excluded,
    organized, generational, mobile, global).

% Argue that high-fidelity simulation with rigorous debriefing provides adequate competence maintenance, and that real-world anchoring is theatrically demanded but empirically unnecessary. Excluded from certification authority; their position drives commercial simulator development but does not alter regulatory requirements.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulation_adequacy_advocates, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, regulatory_authority).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates safety-critical competence maintenance across heterogeneous learning pathways: simulation provides controlled, repeatable, low-jeopardy exercise; real-world anchoring provides context-specificity, irreducible decision-making under genuine uncertainty, and transfer-of-training validation. The hybrid regime solves the coordination problem of maintaining competence at scale without requiring each operator to generate catastrophic near-misses as the primary learning mechanism.
% TRANSFER_FUNCTION: Moves pilot and airline resources (time, aircraft utilization, scheduling complexity, fatigue risk) from revenue-generating operations to non-revenue-generating competence maintenance. The regulatory authority retains legitimacy; the safety assurance framework receives the benefit of validated dual-pathway evidence; operators and simulator centers bear the direct cost of maintaining the regime.
% ABSENT_VOICES: Pure catastrophe-as-exercise advocates and pure simulation-adequacy advocates are institutionally excluded from competence certification bodies. They would argue that the hybrid regime is either a timid compromise that misses the irreducible learning that only real jeopardy provides, or an unnecessary ritualization of already-solved competence maintenance. Their exclusion is structural, not accidental — the regime presupposes both can be wrong.
% DISAPPEARANCE_RATIONALE: If the hybrid requirement vanished, operators would reduce real-world anchoring toward simulation-only training (cost savings), and the regulatory authority would shift legitimacy claims toward pure-simulation pathways or would revert to catastrophe-driven auditing. The competence maintenance system would reorganize around whichever remaining pathway captured regulatory authority.
% FOUNDING_PROBLEM: Pilot competence in the jet age cannot be maintained through simulation alone because transfer-of-training from high-fidelity simulation to actual aircraft differs substantially from full-envelope training. Real-world anchoring is necessary to validate the transfer and to exercise judgment under irreducible uncertainty. Yet competence cannot rely primarily on catastrophic near-misses because the stakes are too high and near-misses are too rare to serve as the primary teaching tool.
% FOUNDING_PROBLEM_CORROBORATION: Transfer-of-training research (Wickens, Cummings, et al.) outside the regulatory authority confirms that simulation-to-reality gaps exist and persist. Line operations data from carriers shows that crews maintaining real-world anchoring respond faster to non-routine events than simulation-only-trained crews. Independent accident investigation boards (NTSB, ICAO) have attributed skill degradation in accident sequences to insufficient real-world currency. Catastrophe advocates attest the problem is under-solved; simulation adequacy advocates attest the problem is over-solved. The founding problem itself is stable in the technical literature — the dispute is about solution proportionality, not problem existence.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the constraint imposes real costs on operators and simulator centers without concentrating visible benefits in a single capturer — the benefit is diffuse (safety assurance framework legitimacy) and the cost is concentrated (operators). The regime extracts credible validation work from multiple parties. Suppression is moderate (0.42) because the rule is justified by transfer-of-training science and line operations data; operators resist through cost arguments and scheduling friction, not through denial of the underlying problem. Theater is moderate (0.31) because real-world anchoring does perform its declared function (transfer-of-training validation) but also performs a legitimacy function (demonstrating to regulators and accident investigators that competence is being monitored). The measurement series shows extractiveness rising gently from t=0 to t=15 (as airlines operationalize and cost-account the hybrid regime) then plateauing (indicating the mature steady-state cost has been reached and operators have structurally adapted).
 *
 * PERSPECTIVAL GAP:
 *   The regulatory authority and safety framework view the constraint as legitimate coordination solving a genuine technical problem: simulation alone leaves a demonstrable transfer gap, catastrophe-based learning is ethically and practically untenable, hybrid regimes bridge the gap. Line operators and airlines view it as imposed cost without proportional benefit: simulation is already extensive, real-world anchoring is ritual theater justified ex-post by accident investigations that could just as well be attributed to decision-making under uncertainty as to training gaps. Simulator centers sit between: real-world anchoring validates simulation's necessity but explicitly rejects simulation's sufficiency. The engine computes these divergences from the structural asymmetry: operators pay and cannot exit (high d); the framework receives legitimacy and can defer costs (low d for beneficiaries); excluded advocates have no seat in the conversation but would argue the regime misframes the competence problem itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The regulatory authority is the agenda-setter with institutional power and analytical exit (can change the standard but is not trapped by it). Line operators and aircraft operators are payers with organized or powerful status but constrained exit — they must comply or lose certification, cannot negotiate the rule, and face direct resource costs. Simulator centers are ambiguous: they benefit from simulation's necessity but are constrained by simulation's insufficiency — their role is secondary_beneficiary + payer. The safety assurance framework is the nominal beneficiary but is an abstract institutional commitment (agent: false) that receives legitimacy, not rents. Catastrophe and simulation adequacy advocates are excluded: their positions would reshape the constraint's entire logic if they had seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids pure mandatrophy by carrying a genuine coordination function — solving the transfer-of-training problem — but sits near the boundary where real-world anchoring could degrade to theater. The theater_ratio oscillating around 0.31 indicates that roughly one-third of the enforcement activity is legitimacy-maintenance (compliance audits, documentation, regulatory demonstrations) rather than direct competence validation. Mandatrophy would manifest as theater_ratio rising above 0.5 while accessibility_collapse remains high (pilots cannot escape the requirement even as its functional value degrades to ritual). The current metrics do not support mandatrophy classification, but the reading explicitly names the risk in omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfer_of_training_measurability,
    'Is the transfer-of-training gap from high-fidelity simulation to real-world aircraft operation empirically measurable with sufficient precision to justify the cost of real-world anchoring, or does it remain a plausible but unquantified psychological construct?',
    'Prospective controlled trials comparing crews trained on simulation alone vs. simulation + real-world anchoring, evaluated on identical line-operations performance metrics (e.g., non-standard procedure response time, decision quality under uncertainty, error recovery). Randomization at the carrier level, blinded evaluation, sustained follow-up to detect attrition effects.',
    'If transfer gap is large and measurable, the hybrid regime is justified as necessary coordination. If transfer gap is small or unmeasurable, the regime is theater justified by proxies (accident investigation narratives, regulatory tradition) rather than empirical evidence, supporting mandatrophy classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_of_training_measurability, empirical, 'Whether the competence gap between simulation-only and hybrid training can be quantified independently of accident narratives.').

omega_variable(
    catastrophe_irreducibility,
    'Is there a residual irreducible component of competence that only real catastrophic jeopardy can exercise — something that even the most high-fidelity simulation with genuine real-world anchoring cannot reach?',
    'Accident investigation data that attributes failures to training gaps specific to irreducible jeopardy (stakes that cannot be replicated without accepting unacceptable risk). Contrast with failures attributed to decision-making under uncertainty, which hybrid training can address. Requires triangulation across incident classes and regulatory jurisdictions.',
    'If catastrophe-irreducible gaps exist, the hybrid reading is incomplete and should coexist-with catastrophe reading rather than influence it; the constraint may need periodic real catastrophic near-misses as irreducible refresh. If no such gaps are found, catastrophe reading is empirically foreclosed (not by logical contradiction but by evidence), supporting pure hybrid sufficiency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_irreducibility, empirical, 'Whether there exists a psychological or procedural competence component accessible only through genuine jeopardy, not replicable by simulation or audits.').

omega_variable(
    theater_cost_threshold,
    'As theater_ratio rises and real-world anchoring becomes increasingly about regulatory demonstration rather than competence validation, at what point does the cost-to-benefit ratio cross from justified coordination to pure extraction?',
    'Cost accounting of real-world anchoring activities disaggregated into competence-validation components (high functional value) and legitimacy-maintenance components (regulatory theater). Compare marginal cost per unit of measurable competence improvement against direct risk reduction in line operations. Set the threshold where theater exceeds 0.5 and extraction is acknowledged as exceeding coordination.',
    'Crossing the threshold supports mandatrophy reclassification from tangled_rope to piton (inertial maintenance without beneficiary capture). Stays below threshold if operators continue to realize measurable safety improvements and justify the resource allocation through accident prevention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_cost_threshold, conceptual, 'The cost-justified boundary between competence maintenance and regulatory theater maintenance.').

omega_variable(
    simulation_fidelity_ceiling,
    'Is there a structural limit to how faithful a simulation can become to real-world aviation without accepting unacceptable risk (i.e., recreating the jeopardy that the constraint is designed to avoid)? If so, does that limit validate hybrid dependence or does it just defer the problem?',
    'Engineering and human-factors analysis of scenario dimensions that are routinely suppressed in simulation (genuine loss-of-life consequences, irreversible errors, sustained cognitive overload to the point of medical risk) and assessment of whether these dimensions are pedagogically necessary or merely psychologically salient. Does higher fidelity in these dimensions yield better transfer, or does it create training toxicity (crews over-trained to catastrophe)?',
    'If suppressed dimensions are pedagogically necessary, simulation''s insufficiency is structural and hybrid dependency is justified. If they are merely psychologically salient (crews feel more real pressure but this does not improve performance), pure simulation adequacy is empirically supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_ceiling, conceptual, 'Whether simulation''s inherent boundaries vindicate hybrid necessity or are over-constrained by medical/legal risk aversion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__hybrid_dependency, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__hybrid_dependency, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__hybrid_dependency, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_requirement__hybrid_dependency, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(comp_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(comp_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(comp_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__hybrid_dependency, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__simulation_as_adequate_exercise).

% DUAL FORMULATION NOTE:
% This constraint (hybrid_dependency) is one reading of the competence_exercise_requirement kernel. It coexists with catastrophe_as_necessary_anchor (holds that only real jeopardy trains adequately) and simulation_as_adequate_exercise (holds that high-fidelity simulation with debriefing is sufficient). The three readings share a common referent — the kernel: competence must be exercised and validated — but instantiate different structures for how that requirement is met. The hybrid_dependency reading asserts that both simulation and real-world anchoring are necessary and neither alone is sufficient. The reading influences both siblings by creating empirical constraints on their claims: catastrophe advocates must show irreducible jeopardy components; simulation advocates must show deployment data supporting sufficiency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__hybrid_dependency, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
