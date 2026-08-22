% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulation Sufficiency Competence Kernel Reading
 *   domain: safety/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This story instantiates the simulation_sufficiency_reading of the
 *   contested kernel 'exercise_as_competence_maintenance.' The reading holds
 *   that simulated catastrophe, when sufficiently faithful to real-stakes
 *   conditions, constitutes genuine exercise of the competence kernel—that
 *   is, it truly maintains the judgment and procedural capability that
 *   catastrophe demands. The regulatory framework treats simulation as
 *   competence-constituting and measures readiness by simulator performance
 *   metrics. The reading is contested: alternative readings
 *   (lived_catastrophe_necessity and hybrid_decay) posit that only actual
 *   catastrophe, or a hybrid regime mixing real and simulated stakes, can
 *   preserve the full competence kernel. This story measures the
 *   simulation_sufficiency_reading's actual operation: how much extraction
 *   occurs when regulators mandate simulation as sufficient, how much theater
 *   is present in the measurement regime, and what cost field responders and
 *   organizations bear when real-stakes learning is structurally excluded.
 *
 * KEY AGENTS:
 *   - regulatory_bodies: Institutional agenda-setter (certified authority to mandate simulations as competence-constituting)
 *   - simulation_infrastructure_operators: Institutional beneficiary (revenue-dependent on the mandate)
 *   - field_responders_unprepared_for_lived_catastrophe: Organized payers (competence cost-bearers if simulation fidelity inadequate)
 *   - organizations_dependent_on_real_stakes_learning: Powerful payers (organizational-memory cost-bearers from loss of catastrophe learning)
 *   - external_observers_and_researchers: Analytical observers (assess adequacy of fidelity metrics and reading viability)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.68).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.71).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation Sufficiency Competence Kernel Reading").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '56e37874-386a-4d63-908a-f55c454b54d0').
narrative_ontology:cs_kernel_codification('56e37874-386a-4d63-908a-f55c454b54d0', formalized).
narrative_ontology:cs_authority_grounding('56e37874-386a-4d63-908a-f55c454b54d0', extraction).
narrative_ontology:cs_interpretation_layer_present('56e37874-386a-4d63-908a-f55c454b54d0').
narrative_ontology:cs_reading_relation('56e37874-386a-4d63-908a-f55c454b54d0', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('56e37874-386a-4d63-908a-f55c454b54d0', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('56e37874-386a-4d63-908a-f55c454b54d0', foundational, simulation_fidelity_transmits_competence_kernel).
narrative_ontology:cs_axiom_status(simulation_fidelity_transmits_competence_kernel, holdable).
narrative_ontology:cs_axiom_grounding('56e37874-386a-4d63-908a-f55c454b54d0', simulation_fidelity_transmits_competence_kernel, empirically_contingent).
narrative_ontology:cs_axiom('56e37874-386a-4d63-908a-f55c454b54d0', secondary, regulatory_mandated_exercise_ensures_readiness).
narrative_ontology:cs_axiom_status(regulatory_mandated_exercise_ensures_readiness, holdable).
narrative_ontology:cs_axiom_grounding('56e37874-386a-4d63-908a-f55c454b54d0', regulatory_mandated_exercise_ensures_readiness, instrumental).
narrative_ontology:cs_reference_frame('56e37874-386a-4d63-908a-f55c454b54d0', simulation_as_competence_constituting).
narrative_ontology:cs_drift_state('56e37874-386a-4d63-908a-f55c454b54d0', post_second_order_failures_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('56e37874-386a-4d63-908a-f55c454b54d0', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_infrastructure_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, field_responders_unprepared_for_lived_catastrophe).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, organizations_dependent_on_real_stakes_learning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce simulation drill mandates. Define fidelity standards and certification criteria. Gain authority and legitimacy from competence certification. Justify the framework as enabling cost-effective readiness. Administer the constraint and could modify it, but do not face pressure to do so from dominant constituencies.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate simulators, define fidelity specifications, sell equipment and services. Revenue depends on the regulatory mandate making simulation obligatory. Can exit to other markets if the mandate changes, but benefit from expansion of simulation standards globally. Not dependent on any specific reading; benefit from any reading that mandates repeated exercise.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_infrastructure_operators, beneficiary,
    institutional, generational, mobile, global).

% Required to maintain competence through mandated simulation. When actual catastrophe occurs, face decision-making situations where real stakes, time pressure, incomplete information, and irreversibility operate differently than in simulation. If simulation fidelity is inadequate, they bear the cost of competence gaps. Professional identity locks them to responder roles; they cannot exit the regime or demand alternative training.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, field_responders_unprepared_for_lived_catastrophe, payer,
    organized, biographical, identity_locked, national).

% Organizations (hospitals, power grids, military, financial regulators) that historically refined decision-making and organizational memory through actual catastrophes are now required to substitute simulation for real-stakes learning. They lose the irreplaceable learning that catastrophe provides and risk degraded judgment when novel catastrophes occur. Exit through regulatory capture or jurisdictional arbitrage is possible but difficult and costly; direct exit is infeasible.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, organizations_dependent_on_real_stakes_learning, payer,
    powerful, generational, constrained, global).

% Assess whether simulation fidelity actually preserves competence, whether the reading's foundational axioms are empirically sound, and whether the regulatory framework is extracting from field readiness by substituting lower-fidelity training for irreplaceable stakes-driven learning. Maintain independent analytical perspective.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, external_observers_and_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_bodies).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes competence maintenance at scale without requiring catastrophes: by mandating regular simulation exercises with specified fidelity, distributes the cognitive and procedural renewal burden across predictable, budgeted, repeatable drills rather than waiting for unpredictable, costly real events. Solves the collective-action problem of readiness without catastrophe.
% TRANSFER_FUNCTION: Transfers the cost of competence maintenance from catastrophe-contingent (expensive, unpredictable, destructive) to simulation-contingent (budgetable, recurring, controlled). Moves regulatory authority to the bodies that define drill standards and fidelity metrics. Moves revenue to simulation infrastructure operators. Implicitly transfers the risk of competence atrophy from the regulatory system to field responders if fidelity is inadequate.
% ABSENT_VOICES: Practitioners and organizations whose competence was sharpened by actual catastrophes and who believe that simulation, however faithful, cannot replace the stakes-driven learning catastrophe provides. Safety researchers who posit that judgment-under-real-stakes operates by mechanisms simulation cannot exercise. This reading structurally excludes them from the competence-verification process: their objection is framed as nostalgia for dangerous training methods, not as structural insight.
% DISAPPEARANCE_RATIONALE: If this constraint—the regulatory mandate treating simulation as competence-sufficient—disappeared, organizations would revert to mixed training regimes incorporating live-stakes learning, simulation infrastructure spending would contract, regulatory bodies would lose authority over competence certification, and competence standards themselves would shift to emphasize judgment-under-stakes over procedure-in-controlled-conditions. The entire logic of preparedness would reorganize around a different kernel reading.
% FOUNDING_PROBLEM: Organizations face catastrophes that require high-fidelity decision-making, but catastrophes are infrequent and unpredictable; competence attenuates between them. Waiting for real catastrophes to train responders is indefensible. A competence maintenance system must work without relying on catastrophe occurrence.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and simulation infrastructure operators attest the founding problem is solved: simulation, properly managed, maintains competence cost-effectively. Safety researchers, practitioners, and post-catastrophe reviews (independent of the regulatory bodies that certify simulation sufficiency) dispute the adequacy of the solution, citing judgment-atrophy and the irreplaceable value of stakes-driven learning. Legislative testimony from field responders supports the contested reading.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is substantial because the reading substitutes a regulatory-controlled, simulation-fidelity-metric-based competence regime for one in which catastrophe itself verified readiness. The regulatory bodies and infrastructure operators benefit from the mandate; field responders and organizations bear the cost of potential competence gap if simulation fidelity is inadequate. Suppression (0.71) is high because the constraint actively excludes alternative training models and marginalizes practitioners who believe real-stakes learning is irreplaceable. Theater (0.58, rising from 0.42) is substantial and rising: a growing share of simulation activity is audit-and-reporting performance (demonstrating compliance with fidelity metrics) rather than actual competence refinement. The measurement series tracks this drift: extraction plateaus as the reading stabilizes in regulatory practice; theater rises as fidelity certification becomes the metric of competence, and actual competence becomes harder to verify. Suppression stabilizes once the enforcement architecture is in place. All three metrics are measured on the same shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory-body and infrastructure-operator seats should compute the constraint as a rope or scaffold (genuine coordination, transitional improvement over catastrophe-contingency). The field-responder and dependent-organization seats should compute it as a snare (enforced extraction of the real-stakes learning they need, without compensating coordination benefit). The engine computes this divergence from the structural data: beneficiary/victim declarations, exit options (operators have arbitrage and mobility; responders are identity_locked into organizations and constrained in their training methods), and power atoms (institutional vs. organized vs. powerful, with different time horizons and spatial scopes). The claim/metric divergence (claimed tangled_rope but high theater_ratio) is itself diagnostic: if theater were very low (0.1–0.2), the reading would be sustaining genuine competence verification; at 0.58 and rising, it suggests fidelity metrics are becoming performance markers rather than competence markers, which is exactly what a snare-type constraint exhibits.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies are the structural beneficiaries (d near 0.1–0.2): they gain authority and legitimacy from certification rights and avoid the unpredictability of catastrophe-contingent learning. Simulation operators also benefit (d near 0.2): mandatory drills generate recurring revenue. Field responders and dependent organizations are the targets (d near 0.75–0.85): they are required to maintain competence through simulation, face potential competence gaps if fidelity is inadequate, and cannot exit the regime. External observers sit near symmetric (d near 0.5): they depend on the regime's legitimacy to maintain their analytical standing but are not structurally dependent on it. The Tangled Rope classification rests on: (1) a real coordination function (maintaining competence at scale without catastrophe), (2) asymmetric extraction (regulatory authority and infrastructure revenue benefit, responder readiness cost-bears), and (3) active enforcement (regulators mandate simulation, exclude alternatives, validate fidelity metrics). The reading's victim set is narrow and structural: only those harmed by inadequate simulation fidelity, not those who simply prefer different training methods.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is contested, and the reading exhibits mandatrophy candidates. Regulatory bodies and operators attest the problem of maintaining competence without catastrophe is solved by simulation. Practitioners and researchers attest that judgment-under-stakes, and the organizational memory catastrophe provides, are irreplaceable and atrophying under simulation-only regimes. The theater-ratio rise (0.42 to 0.58) is the key diagnostic: as fidelity certification becomes the performance metric, actual competence maintenance becomes harder to measure and may be degrading. This is the Goodhart drift pattern: the metric (simulator performance) replaced the goal (real-stakes decision-making), and the metric is now gaming-vulnerable. The constraint does not yet qualify as a piton (there is still some real competence maintained through simulation, and organizations still invest in drills), but the rising theater_ratio and the contestation of the founding_problem position it as a tangled_rope with mandatrophy risk. The reading's viability depends on empirical resolution of whether simulation fidelity can actually preserve judgment-under-stakes; that question is permanently contested and is the reason the omegas focus on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_sufficiency_boundary,
    'At what level of simulation fidelity does simulated exercise actually exercise the judgment-under-real-stakes competence kernel, or is there an irreducible gap between simulated and lived catastrophe?',
    'Longitudinal comparison of competence decay rates between simulation-only and hybrid (simulation+live-event) training regimes. Post-incident performance reviews comparing actual decision quality in field responders trained under different regimes, controlling for experience and organizational factors.',
    'If fidelity-gap is small (competence decay comparable), the simulation_sufficiency reading holds and the constraint functions as claimed coordination. If fidelity-gap is large, the constraint operates as mandatrophy (exercising procedures but not judgment), and reclassifies toward snare or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fidelity_sufficiency_boundary, empirical, 'Whether simulation fidelity can transmit the full competence kernel or leaves judgment-under-stakes permanently attenuated.').

omega_variable(
    organizational_memory_irreplaceability,
    'Is organizational memory of real-stakes decision-making irreplaceable, or can it be reconstructed from simulated exercise and post-incident analysis?',
    'Comparative study of organizational learning in systems that undergo catastrophes vs. those that do not: does competence renewal through catastrophe-learning exceed what simulation can sustain? Analysis of institutional knowledge retention in high-reliability organizations.',
    'If organizational memory from catastrophe is irreplaceable, organizations dependent on real-stakes learning bear a structural cost that simulation cannot offset, and the victim set expands. If organizational learning is substrate-independent, simulation + analysis can substitute for catastrophe-learning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_memory_irreplaceability, empirical, 'Whether catastrophe-driven organizational learning has structural properties simulation cannot replicate.').

omega_variable(
    reading_foreclosure_via_empirical_cascade,
    'Can the simulation_sufficiency reading coexist with systematic post-incident findings that competence was inadequate to the actual catastrophe, or does empirical falsification of simulated sufficiency logically foreclose the reading within frameworks that must account for such evidence?',
    'Accumulation of post-incident reviews showing competence gaps traceable to reliance on simulation-only training; regulatory and institutional uptake of hybrid-or-lived-catastrophe readings in response to such evidence.',
    'If empirical cascades systematically disconfirm the reading, it may transition from holdable axiom to overridden axiom within its own tradition—the simulation_sufficiency reading''s grounding_type is empirically_contingent, so substantial disconfirming evidence can trigger axiom_overriding drift. The reading would persist but as an overridden claim, not a live one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_empirical_cascade, empirical, 'Whether lived catastrophe evidence can override the simulation_sufficiency axiom within frameworks that must integrate performance data.').

omega_variable(
    suppression_of_hybrid_and_necessity_readings,
    'To what extent is the suppression score (0.71) attributable to active enforcement of the simulation_sufficiency reading vs. passive marginalization of alternative readings?',
    'Documentary evidence of regulatory rejection of hybrid-training proposals, exclusion of real-stakes-learning advocates from competence-standard setting, and funding allocation favoring simulation infrastructure over catastrophe-learning integration.',
    'If suppression is active enforcement, the reading is actively defended and the snare classification is more likely. If suppression is passive marginalization, the reading may be more fragile and the theater_ratio may better explain persistence. Characterization affects the reading''s structural robustness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_hybrid_and_necessity_readings, empirical, 'Whether the simulation_sufficiency reading is actively enforced or passively sustained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 5, 0.46).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 15, 0.53).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(exer_tr_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement(exer_tr_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(exer_be_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(exer_be_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(exer_su_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(exer_su_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% The kernel 'exercise_as_competence_maintenance' decomposes into three structurally distinct constraint stories, each instantiating a different reading of what counts as genuine exercise of competence. The simulation_sufficiency_reading treats simulation as competence-constituting; the lived_catastrophe_necessity_reading treats only actual catastrophe as competence-constituting; the hybrid_decay_reading treats the kernel as having two components requiring different exercise types. The three readings converge on the same referent (the standing organizational commitment to maintain readiness) but diverge on epistemic premises about what 'exercise' means. Epsilon values differ: simulation_sufficiency exhibits ε=0.68 (substantial extraction through regulatory mandate); lived_catastrophe_necessity exhibits lower ε if catastrophes are rare and learning is incidental; hybrid_decay exhibits intermediate ε as a mixed regime with lower theater. All three stories are linked via network.affects_constraints; this story (simulation_sufficiency) influences the necessity and hybrid readings by setting the regulatory baseline they contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
