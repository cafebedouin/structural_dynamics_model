% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulation-Sufficiency Reading of Exercise-Based Competence Maintenance
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint models the regulatory and organizational arrangement
 *   built on the claim that simulated catastrophe is a genuine exercise of
 *   the competence kernel — not a substitute for it, but the real thing, with
 *   retention effectiveness determined by simulation fidelity. Under this
 *   reading, drill completion against certified scenario packs is treated as
 *   sufficient evidence of maintained competence: pass the simulator, you are
 *   certified competent, full stop. The claim has a real coordination
 *   function (competence verification at scale without manufacturing real
 *   disasters) but the metrics track a growing gap between what the
 *   compliance standard verifies (drill completion) and what it is presented
 *   as verifying (transferable real-world competence), while enforcement
 *   (regulatory mandate + certification gatekeeping) holds the reading in
 *   place regardless of accumulating fidelity-gap evidence from incident
 *   investigations.
 *
 * KEY AGENTS:
 *   - regulatory_compliance_officers: agenda_setter (institutional/analytical) — writes and enforces the sufficiency standard
 *   - simulator_vendors: beneficiary (organized/mobile) — commercial interest in simulation-as-sufficient
 *   - training_department_leadership: beneficiary/agenda_setter (organized/constrained) — administers drills, reports compliance metrics upward
 *   - frontline_operators_undertrained_by_low_fidelity_drills: payer (moderate/constrained) — certified but potentially under-prepared
 *   - downstream_public_exposed_to_real_incidents: payer (powerless/trapped) — bears the tail risk of the fidelity gap
 *   - incident_investigators: observer (institutional/analytical) — documents but cannot revise the standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.42).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.38).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation-Sufficiency Reading of Exercise-Based Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'f3004a61-73cc-43ff-b64f-3da991c09adb').
narrative_ontology:cs_kernel_codification('f3004a61-73cc-43ff-b64f-3da991c09adb', formalized).
narrative_ontology:cs_authority_grounding('f3004a61-73cc-43ff-b64f-3da991c09adb', extraction).
narrative_ontology:cs_interpretation_layer_present('f3004a61-73cc-43ff-b64f-3da991c09adb').
narrative_ontology:cs_reading_relation('f3004a61-73cc-43ff-b64f-3da991c09adb', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('f3004a61-73cc-43ff-b64f-3da991c09adb', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('f3004a61-73cc-43ff-b64f-3da991c09adb', foundational, simulated_activation_is_genuine_kernel_exercise).
narrative_ontology:cs_axiom_status(simulated_activation_is_genuine_kernel_exercise, holdable).
narrative_ontology:cs_axiom_grounding('f3004a61-73cc-43ff-b64f-3da991c09adb', simulated_activation_is_genuine_kernel_exercise, empirically_contingent).
narrative_ontology:cs_axiom('f3004a61-73cc-43ff-b64f-3da991c09adb', secondary, fidelity_is_the_sole_retention_determinant).
narrative_ontology:cs_axiom_status(fidelity_is_the_sole_retention_determinant, holdable).
narrative_ontology:cs_axiom_grounding('f3004a61-73cc-43ff-b64f-3da991c09adb', fidelity_is_the_sole_retention_determinant, empirically_contingent).
narrative_ontology:cs_reference_frame('f3004a61-73cc-43ff-b64f-3da991c09adb', post_war_civil_defense_drill_paradigm).
narrative_ontology:cs_drift_state('f3004a61-73cc-43ff-b64f-3da991c09adb', contemporary_high_fidelity_simulator_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3004a61-73cc-43ff-b64f-3da991c09adb', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_officers).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, training_department_leadership).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_operators_undertrained_by_low_fidelity_drills).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, downstream_public_exposed_to_real_incidents).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, drill_completion_equals_competence_maintained).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce the drill-frequency and pass/fail criteria that define regulatory compliance. Their job is discharged when the logged exercise is completed to spec; they have no mandate or budget line to evaluate whether the simulated scenario resembles a real one closely enough to transfer skill.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_officers, agenda_setter,
    institutional, generational, analytical, national).

% Sell certified simulation platforms and scenario packs. Revenue depends on the regulatory reading that simulated exercise satisfies the competence requirement; a shift toward requiring real-incident exposure or radically higher-fidelity (and thus more expensive, harder-to-certify) simulation would restructure their market.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_vendors, beneficiary,
    organized, biographical, mobile, global).

% Run the internal drill programs, report completion rates upward, and are evaluated on throughput and compliance metrics rather than on operator performance during real incidents. They administer the constraint and could push for fidelity investment, but the cost of doing so falls on their own budget while the benefit of catching fidelity gaps accrues to operators and the public they rarely meet.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, training_department_leadership, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, training_department_leadership, agenda_setter).

% Complete the mandated drills, are certified as competent on that basis, and then face real incidents where the simulated scenario's simplifications (timing, sensory load, ambiguity, consequence-weight) did not prepare them for what actually happens. They cannot decline the drill regime without losing certification, and have no channel to contest the fidelity standard itself.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_operators_undertrained_by_low_fidelity_drills, payer,
    moderate, immediate, constrained, local).

% Are the people present when an operator's simulation-certified competence proves insufficient in the real event — passengers, patients, residents near the facility. They have no visibility into drill fidelity and no standing to demand it be raised; they experience the gap only as outcome, after the fact.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, downstream_public_exposed_to_real_incidents, payer,
    powerless, biographical, trapped, regional).

% Conduct post-incident reviews and can trace failures back to gaps between drill scenarios and the real event, but their findings enter a system that treats drill completion as the operative competence standard regardless of what the review shows.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, incident_investigators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a repeatable, auditable, scalable mechanism for verifying and refreshing operator competence without waiting for or manufacturing real catastrophes, which would be far more costly and dangerous to use as the training vehicle.
% TRANSFER_FUNCTION: Moves the cost of competence verification from expensive real-world exposure onto simulation infrastructure spend, while moving the residual risk of any fidelity shortfall onto operators (who are certified but may remain under-prepared) and the public who encounters them during real incidents.
% ABSENT_VOICES: Frontline operators who have experienced the fidelity gap directly rarely have a formal channel into standard-setting; incident investigators' fidelity findings enter the record but do not automatically revise the compliance definition of competence.
% DISAPPEARANCE_RATIONALE: If simulated-exercise-as-sufficient-proof were withdrawn overnight, regulators would need an alternative competence standard, simulator vendors would lose their certification market, training departments would need new metrics, and organizations would face either far higher training costs (real-incident exposure or radically upgraded fidelity) or a genuine competence gap becoming visible rather than papered over by compliance metrics.
% FOUNDING_PROBLEM: Real catastrophes are too rare, too dangerous, and too costly to use as the primary vehicle for maintaining operator competence at scale; simulation was built to let organizations exercise and verify competence repeatedly without waiting for or causing real disasters.
% FOUNDING_PROBLEM_CORROBORATION: Simulator vendors and training leadership attest the founding problem is fully addressed by current drill regimes. Incident investigators and independent safety researchers outside the training and vendor ecosystem attest that fidelity gaps between drills and real events recur in post-incident findings, suggesting the founding problem (real transferable competence) is only partially solved by the current standard, even though the compliance proxy (drill completion) is fully solved.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).
:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42 at interval end, rising from 0.28) tracks the growing divergence between the compliance proxy (drill completion) and the underlying competence it claims to certify — a Goodhart-style drift where the metric substitutes for the goal it was meant to measure. Suppression (0.38, mild) reflects the mandatory nature of the drill regime and the limited channels for contesting fidelity standards, but this is not a high-coercion constraint: participants are not trapped in an obviously exploitative arrangement, they are trapped inside a proxy that looks adequate until it fails. Theater ratio (0.40, rising from 0.20) captures the increasing share of drill activity oriented toward passing the certified metric rather than building transferable skill — a slow institutional drift toward performance as fidelity investment lags compliance requirements. Accessibility collapse (0.45) is moderate: alternative approaches (higher-fidelity investment, hybrid real-incident exposure) remain conceptually available but are foreclosed in practice by cost and regulatory lock-in to the existing standard. Resistance (0.50) is real — incident investigators and some operators do push back on fidelity adequacy — but has not yet forced standard revision.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory/training seats, this looks like a functioning rope: a real coordination problem (verifying competence without real disasters) solved by an auditable mechanism. From the frontline-operator and public seats, the same structure can look like a tangled rope or worse: coordination on paper, but the enforcement of a proxy standard that has decoupled from what it certifies, with the decoupling cost landing on people who didn't design the standard and can't revise it. The engine's per-seat computation should reflect this: agenda_setter and beneficiary seats classify closer to rope/coordination; payer seats closer to tangled_rope/snare given low exit and the growing extraction trend.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory compliance officers and simulator vendors sit near the beneficiary end: the reading's sufficiency claim is precisely what makes their function/product adequate by definition, low or no exit cost to them if fidelity gaps surface elsewhere. Training leadership benefits from the compliance metric it is evaluated on while bearing some administrative burden — a mixed beneficiary/agenda_setter position, constrained exit because career and budget structures are built around the existing drill regime. Frontline operators and the downstream public sit near the target end: they bear the consequence of any real fidelity shortfall while having no standing to redefine what counts as adequate exercise. The public's powerless/trapped position is the sharpest asymmetry — they receive no certification, no drill, no voice, only the tail-risk outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — verifying competence at scale without relying on real catastrophe — remains genuinely live; simulation-based verification is not obsolete machinery. What has drifted is the sufficiency claim layered on top: the founding problem was 'verify competence,' and the arrangement has substituted 'verify drill completion' as though the two were identical. This is not mandatrophy in the classic sense (mandate fully dead, arrangement purely inertial) — it is a live coordination function with an accumulating extraction layer riding on it, which is why tangled_rope rather than piton or snare is the structurally accurate claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_ceiling_ambiguity,
    'Is there a fidelity ceiling beyond which simulation genuinely becomes equivalent to real catastrophe exercise for competence-retention purposes, or does simulation asymptotically approach but never reach equivalence regardless of investment?',
    'Longitudinal comparison of operator performance in real incidents against documented drill fidelity levels, controlling for incident type and operator tenure; convergence of high-fidelity-drill performance with real-incident performance would support the sufficiency reading''s core premise.',
    'If a fidelity ceiling exists at which simulation genuinely equals real exercise, this reading is vindicated as structurally correct and the extraction found here is remediable purely by fidelity investment (making this closer to a genuine rope with fixable friction). If no such ceiling exists, this reading''s foundational premise is wrong and the arrangement is better described by the hybrid_decay_reading or lived_catastrophe_necessity_reading, reclassifying the persistent extraction as irreducible rather than a fidelity-investment problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fidelity_ceiling_ambiguity, empirical, 'Whether simulation fidelity can in principle reach parity with real-catastrophe exercise for competence retention.').

omega_variable(
    committer_framing_alternative,
    'Is the correct unit of analysis ''the drill mandate as administered'' (this story''s framing) or ''the underlying claim about what constitutes genuine exercise of a competence kernel'' (a framing shared across all three sibling readings, which would treat this story''s tangled_rope classification as itself downstream of a contested epistemic premise rather than a free-standing structural fact)?',
    'Compare classification outcomes if the story were authored around the contested epistemic claim itself (is simulated activation ontologically the same kind of event as real activation) versus around the administrative arrangement (how drills are mandated and certified). If the two framings produce different claimed types, the disagreement is located in the definition of ''genuine exercise,'' not in the administrative facts.',
    'If the epistemic-claim framing dominates, this story''s tangled_rope reading is better understood as an artifact of accepting the sufficiency premise as a background assumption; under the epistemic framing the constraint could shift toward snare (extraction dressed as a settled competence claim) if the sufficiency premise is judged false, or toward rope if judged true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Alternative framings of the kernel dispute produce different classification pressure; this omega documents which framing was chosen and why.').

omega_variable(
    compliance_proxy_drift_trajectory,
    'Is the observed rise in theater_ratio and base_extractiveness over the measured interval a stable equilibrium drift (proxy substitution settling at a tolerable level) or an accelerating trend that will eventually force standard revision via accumulated incident evidence?',
    'Extend the measurement series and track whether incident-investigation findings begin to visibly move the regulatory standard, or whether the drift plateaus without triggering revision.',
    'An accelerating, unaddressed trend would support reclassification toward snare over time (T17-style extraction accumulation on a mountain-adjacent coordination claim); a plateauing trend supports the tangled_rope reading as a stable, if imperfect, equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_proxy_drift_trajectory, empirical, 'Whether the compliance-proxy drift is stabilizing or accelerating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the exercise_as_competence_maintenance kernel, decomposed per the ε-invariance principle: measuring 'exercise-based competence maintenance' by the sufficiency-of-simulation observable yields a different ε (0.42, moderate, rising) than measuring it by the real-stakes-necessity observable (expected much higher for lived_catastrophe_necessity_reading, since under that reading nearly all drill-certified competence claims are false) or the two-component observable (hybrid_decay_reading, expected intermediate, since procedural competence is genuinely exercised while judgment-under-stakes is not). Each reading gets its own file, own stakeholders, own victim set; this file's victim set is narrowly the fidelity-gap-harmed, not the broader real-stakes-gap-harmed set that the lived_catastrophe_necessity_reading would name.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
