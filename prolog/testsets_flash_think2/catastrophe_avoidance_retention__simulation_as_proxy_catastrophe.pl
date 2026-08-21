% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: Simulation as Proxy Catastrophe for Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This constraint represents the reading that high-fidelity simulation is
 *   functionally equivalent to real catastrophic events for maintaining
 *   operational competence in high-risk systems. It posits that scheduled
 *   drills in simulated environments can fully substitute for the learning
 *   derived from actual incidents, allowing organizations to avoid the costs
 *   and trauma of real failures while still ensuring safety. This reading is
 *   often embedded in regulatory frameworks and industry best practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.65).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.75).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation as Proxy Catastrophe for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'f5602c19-2d0a-4f1a-b54e-790bcd119228').
narrative_ontology:cs_kernel_codification('f5602c19-2d0a-4f1a-b54e-790bcd119228', formalized).
narrative_ontology:cs_authority_grounding('f5602c19-2d0a-4f1a-b54e-790bcd119228', expertise).
narrative_ontology:cs_interpretation_layer_present('f5602c19-2d0a-4f1a-b54e-790bcd119228').
narrative_ontology:cs_reading_relation('f5602c19-2d0a-4f1a-b54e-790bcd119228', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('f5602c19-2d0a-4f1a-b54e-790bcd119228', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('f5602c19-2d0a-4f1a-b54e-790bcd119228', foundational, simulation_is_functionally_equivalent_to_catastrophe).
narrative_ontology:cs_axiom_status(simulation_is_functionally_equivalent_to_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('f5602c19-2d0a-4f1a-b54e-790bcd119228', simulation_is_functionally_equivalent_to_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('f5602c19-2d0a-4f1a-b54e-790bcd119228', secondary, competence_decay_is_manageable_via_scheduled_drills).
narrative_ontology:cs_axiom_status(competence_decay_is_manageable_via_scheduled_drills, holdable).
narrative_ontology:cs_axiom_grounding('f5602c19-2d0a-4f1a-b54e-790bcd119228', competence_decay_is_manageable_via_scheduled_drills, empirically_contingent).
narrative_ontology:cs_reference_frame('f5602c19-2d0a-4f1a-b54e-790bcd119228', controlled_learning_environment).
narrative_ontology:cs_drift_state('f5602c19-2d0a-4f1a-b54e-790bcd119228', contemporary_regulatory_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f5602c19-2d0a-4f1a-b54e-790bcd119228', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_with_high_risk_operations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_developers_and_trainers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, public_safety).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_efficacy_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, controlled_learning_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and enforce standards for high-fidelity simulation as a primary method for competence maintenance. They benefit from the perceived effectiveness of these programs in preventing catastrophes and maintaining public trust, and from a standardized, auditable training regime.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Utilize high-fidelity simulations to meet regulatory requirements and train their staff for rare, high-consequence events. They benefit by avoiding the immense costs and reputational damage of actual catastrophes, and by having a predictable, controlled training environment.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_with_high_risk_operations, beneficiary,
    institutional, biographical, constrained, global).

% Provide the technology, infrastructure, and expertise for high-fidelity simulations. They profit directly from the widespread adoption and regulatory endorsement of simulation as a proxy for real catastrophic events.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_developers_and_trainers, beneficiary,
    organized, biographical, mobile, global).

% Undergo simulation training to maintain their operational competence. They bear the primary risk if the simulations are not truly functionally equivalent to real events, potentially facing actual catastrophes with insufficient preparation. Their professional identity is tied to their role, making exit difficult.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, payer,
    powerless, immediate, identity_locked, local).

% Relies on the competence of high-risk organizations to prevent harm. If simulation is an insufficient proxy for real-world learning, public safety is implicitly compromised, bearing the ultimate cost of any actual catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, public_safety, payer,
    powerless, generational, trapped, national).

% Conduct studies on the effectiveness of simulation, the nature of competence decay, and the dynamics of organizational learning. They provide evidence that may challenge or support the functional equivalence claim, but do not directly enforce or benefit from the constraint's operation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, academic_researchers_in_safety, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_with_high_risk_operations).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes and centralizes the process of maintaining operational competence for rare, high-consequence events across diverse high-risk industries, providing a controlled and repeatable training environment.
% TRANSFER_FUNCTION: Transfers the primary burden of learning and competence maintenance from costly, unpredictable real-world incidents to managed, scheduled, and less disruptive simulated environments. It also transfers a sense of security and compliance to regulators and organizations.
% ABSENT_VOICES: Survivors or victims of actual catastrophic failures, and those who advocate for more experiential, distributed, or 'messy' forms of learning that cannot be fully replicated in a simulation. They would argue that the psychological, social, and systemic chaos of real events is irreducible to simulation.
% DISAPPEARANCE_RATIONALE: If the belief in simulation's functional equivalence vanished overnight, high-risk industries would face a crisis in competence maintenance. Regulatory frameworks would collapse, organizations would scramble for alternative (likely more expensive and disruptive) training methods, and public trust in safety protocols would erode, leading to a fundamental reorganization of safety engineering practices.
% FOUNDING_PROBLEM: How to effectively and safely train personnel and maintain high levels of operational competence for extremely rare, high-impact catastrophic events without relying on actual failures as learning opportunities.
% FOUNDING_PROBLEM_CORROBORATION: Industry bodies, safety regulators, and many academic researchers attest to the ongoing challenge of maintaining competence in complex, high-risk systems. However, the *solution* (simulation's functional equivalence) is contested by other researchers and safety advocates, making the status of the *solution* contested, even if the problem is live.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the claim of functional equivalence can lead to under-investment in other, potentially more robust, safety measures or a false sense of security, effectively extracting from overall system resilience and shifting risk to frontline operators and public safety. Suppression (0.75) is high as this reading actively suppresses the perceived need for alternative, more costly, or less controllable learning mechanisms (e.g., from actual near-misses or distributed learning). The theater ratio (0.45) is moderate; while simulation provides genuine training benefits, the *claim of full equivalence* can introduce performative aspects where compliance with simulation standards is prioritized over genuine, holistic competence. The metrics show a gradual increase in extractiveness and suppression over time as reliance on simulation deepens.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries (regulators, organizations), this constraint is a highly effective coordination mechanism that solves a critical safety problem efficiently. From the perspective of victims (operators, public), it may be an extractive mechanism that shifts risk and suppresses more comprehensive safety approaches. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators, high-risk organizations, and simulation developers are beneficiaries, gaining from perceived safety, avoided costs, and market demand, respectively. Frontline operators and public safety are victims, bearing the risk if simulation proves insufficient. Academic researchers observe and analyze, while other learning advocates are excluded from the dominant discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope helps prevent mislabeling the constraint as a pure Rope. While it provides a genuine coordination function (standardized training), the high extractiveness and suppression indicate that it also serves to extract benefits (avoided costs, perceived safety) at the expense of others (shifted risk, potentially degraded actual competence), requiring active enforcement to maintain this asymmetry. The rising theater ratio suggests a drift towards performative compliance over genuine functional equivalence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_ambiguity,
    'Is the ''high-fidelity'' of simulations truly sufficient to replicate the full complexity, chaos, and emergent properties of real catastrophic events?',
    'Longitudinal studies comparing performance outcomes of simulation-trained operators in real-world incidents versus those trained via alternative methods, or detailed post-incident analyses of simulation gaps.',
    'If fidelity is insufficient, the constraint''s effective extractiveness (risk shifting) and suppression (of alternative learning) are higher than measured, pushing it closer to a Snare. If fidelity is consistently proven sufficient, it strengthens the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_ambiguity, empirical, 'Uncertainty regarding the actual functional equivalence of simulation to real events.').

omega_variable(
    psychological_equivalence_ambiguity,
    'Can high-fidelity simulation genuinely replicate the psychological, emotional, and social pressures (e.g., mortality salience, organizational trauma) that drive learning and adaptation in actual catastrophic events?',
    'Neuroscientific and psychological studies comparing stress responses and learning retention in high-stakes simulations versus real-world critical incidents, or ethnographic studies of post-catastrophe organizational learning.',
    'If psychological equivalence is low, the ''competence maintenance'' claim is weaker, increasing the constraint''s theater ratio and extractiveness, as it fails to deliver on a core promise. This would push it towards a Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_equivalence_ambiguity, empirical, 'Whether simulation can replicate the non-cognitive aspects of catastrophic learning.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative learning methods (e.g., from near-misses, distributed learning) primarily structural (regulatory mandates, resource allocation) or internalized (organizational belief in simulation''s supremacy)?',
    'Analysis of organizational responses to regulatory changes that open pathways for alternative learning: if suppression persists despite structural changes, it indicates internalized mechanisms. Post-incident interviews with operators about perceived training gaps.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher and more resilient to external intervention, making it harder to shift towards a more balanced learning approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative learning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1980, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(cata_tr_t2020, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(cata_tr_t2025, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t1980, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(cata_be_t1990, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(cata_be_t2000, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(cata_be_t2010, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(cata_be_t2020, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(cata_be_t2025, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1980, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(cata_su_t1990, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 1990, 0.63).
narrative_ontology:measurement(cata_su_t2000, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement(cata_su_t2010, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(cata_su_t2020, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(cata_su_t2025, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__hybrid_near_miss_learning).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulation_compliance_burden).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_avoidance_retention' kernel, focusing on simulation as a proxy for real catastrophic events. It is linked to its sibling readings which offer alternative perspectives on competence maintenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
