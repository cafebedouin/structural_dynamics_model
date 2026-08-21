% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Competence Occupation via Simulation Sufficiency
 *   domain: safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint describes the belief and institutional practice that
 *   simulation-based drills are sufficient for maintaining competence in
 *   high-reliability organizations (HROs). It is a reading of the broader
 *   'competence_occupation' kernel, where the core contest is how competence
 *   is genuinely maintained. This 'simulation_sufficiency' reading emphasizes
 *   auditable compliance and risk reduction through simulation, often at the
 *   expense of real-world readiness. The claimed type is 'tangled_rope'
 *   because it offers a genuine coordination function (standardized training)
 *   but also involves asymmetric extraction (benefits for simulation
 *   industry/management, costs for operators/auditors).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.65).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.7).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Competence Occupation via Simulation Sufficiency").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "safety_training/competence_maintenance").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '9d114f36-648f-4ac3-ac92-0893ee03670b').
narrative_ontology:cs_kernel_codification('9d114f36-648f-4ac3-ac92-0893ee03670b', formalized).
narrative_ontology:cs_authority_grounding('9d114f36-648f-4ac3-ac92-0893ee03670b', extraction).
narrative_ontology:cs_interpretation_layer_present('9d114f36-648f-4ac3-ac92-0893ee03670b').
narrative_ontology:cs_reading_relation('9d114f36-648f-4ac3-ac92-0893ee03670b', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_reading_relation('9d114f36-648f-4ac3-ac92-0893ee03670b', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('9d114f36-648f-4ac3-ac92-0893ee03670b', foundational, simulated_experience_transfers_fully).
narrative_ontology:cs_axiom_status(simulated_experience_transfers_fully, holdable).
narrative_ontology:cs_axiom_grounding('9d114f36-648f-4ac3-ac92-0893ee03670b', simulated_experience_transfers_fully, empirically_contingent).
narrative_ontology:cs_axiom('9d114f36-648f-4ac3-ac92-0893ee03670b', secondary, auditable_compliance_equals_competence).
narrative_ontology:cs_axiom_status(auditable_compliance_equals_competence, holdable).
narrative_ontology:cs_axiom_grounding('9d114f36-648f-4ac3-ac92-0893ee03670b', auditable_compliance_equals_competence, conventional).
narrative_ontology:cs_reference_frame('9d114f36-648f-4ac3-ac92-0893ee03670b', simulation_centric_competence_model).
narrative_ontology:cs_drift_state('9d114f36-648f-4ac3-ac92-0893ee03670b', contemporary_operational_reality, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d114f36-648f-4ac3-ac92-0893ee03670b', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, hro_management).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, safety_auditors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and sells simulation platforms and training programs. Benefits directly from the widespread adoption of simulation as the primary method for competence maintenance. Actively promotes the 'simulation sufficiency' narrative.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_industry, agenda_setter,
    organized, biographical, mobile, global).

% Benefits from a quantifiable, auditable training compliance metric that reduces perceived risk and liability. Can demonstrate 'due diligence' through simulation records, often at lower cost and risk than real-world exercises. Faces pressure to maintain operational uptime, making real-world drills costly.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, hro_management, beneficiary,
    institutional, generational, constrained, national).

% Participate in simulation drills, often experiencing a gap between simulated and real-world conditions. Bear the cost of potential skill decay if simulations are truly insufficient, but are identity-locked by professional norms and career paths within the HRO. Their resistance is often expressed as 'workarounds' or informal training.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, immediate, identity_locked, local).

% Are tasked with verifying competence but are often limited to auditing simulation records and compliance metrics, rather than directly assessing real-world skill. Bear the reputational cost if a real incident reveals competence gaps despite high simulation compliance. Their ability to challenge the 'sufficiency' claim is constrained by established regulatory frameworks.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_auditors, payer,
    organized, biographical, constrained, national).

% Often codify simulation requirements into training mandates, influenced by industry lobbying and the perceived benefits of standardized, auditable training. Their role is to enforce compliance, which reinforces the simulation-sufficiency model.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulators, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, repeatable, and safe method for training and assessing operator competence across a high-reliability organization, ensuring a baseline level of skill and regulatory compliance.
% TRANSFER_FUNCTION: Transfers training budget from operational departments to the simulation industry, and transfers perceived risk from HRO management to frontline operators and safety auditors, by certifying competence through simulation metrics.
% ABSENT_VOICES: Experienced operators who have witnessed real incidents and understand the limitations of simulation, and independent researchers who highlight the 'fidelity gap' between simulation and reality, are often marginalized in policy discussions.
% DISAPPEARANCE_RATIONALE: If the belief in simulation sufficiency vanished, HROs would face a crisis in competence maintenance. Training budgets would need to be reallocated to more costly and risky real-world exercises, regulatory frameworks would need complete overhaul, and the simulation industry would lose its primary market. The entire safety training ecosystem would reorganize.
% FOUNDING_PROBLEM: Maintaining high-level competence in complex, high-risk environments without exposing personnel or equipment to actual danger during training, and providing auditable proof of training for regulatory bodies.
% FOUNDING_PROBLEM_CORROBORATION: HRO management and the simulation industry attest the problem is live, citing the ongoing need for safe, scalable training. Frontline operators and safety auditors acknowledge the problem but contest the 'sufficiency' of the proposed solution, arguing that the problem of 'real-world readiness' remains unsolved by simulation alone.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the simulation industry captures significant revenue, and HRO management offloads risk without necessarily ensuring full competence. Suppression is high because regulatory frameworks and institutional inertia make it difficult for operators or auditors to challenge the 'sufficiency' claim or demand more costly real-world training. Theater ratio is moderate and rising, as the focus shifts from genuine skill transfer to compliance with simulation metrics. The increasing extractiveness and suppression over time reflect the hardening of this institutional arrangement.
 *
 * PERSPECTIVAL GAP:
 *   HRO management and the simulation industry perceive this as a highly efficient and effective coordination mechanism. Frontline operators and safety auditors, however, experience it as an extractive mechanism that prioritizes auditable compliance over genuine, robust competence, leading to a 'fidelity gap' between training and reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation industry and HRO management are beneficiaries, gaining revenue and reduced liability respectively. Frontline operators and safety auditors are payers, bearing the costs of potential skill decay and reputational risk. Regulators act as agenda-setters, codifying and enforcing the simulation-sufficiency model, which reinforces the constraint's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as pure extraction by acknowledging the genuine coordination function of standardized, safe training. However, it highlights how the 'sufficiency' claim, when unexamined, allows for the accumulation of extraction and the suppression of alternatives, pushing it towards a tangled_rope classification rather than a pure rope. The founding problem (safe, auditable training) is still live, but the 'sufficiency' of the solution is contested, indicating a potential drift towards mandatrophy if the gap between simulated and real competence widens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_gap,
    'What is the actual fidelity gap between simulated conditions and real-world operational demands for critical skills?',
    'Independent, blinded empirical studies comparing performance outcomes of simulation-trained vs. real-world-trained operators in high-stress, unexpected scenarios.',
    'A large fidelity gap would undermine the ''sufficiency'' claim, reclassifying the constraint closer to a snare or piton due to increased theater and extraction from operators. A small gap would reinforce the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_gap, empirical, 'The degree to which simulation accurately prepares operators for real-world challenges.').

omega_variable(
    competence_kernel_definition,
    'Is the ''competence kernel'' truly occupied by simulation, or does it require exposure to irreducible real-world complexity and consequence?',
    'Conceptual analysis and expert consensus from cognitive science and HRO theory, distinguishing between ''procedural fluency'' (simulatable) and ''adaptive expertise'' (requiring real-world consequence).',
    'If adaptive expertise is deemed essential, the ''simulation_sufficiency'' reading would be conceptually foreclosed, shifting the constraint towards a snare or piton, as its core claim would be invalidated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_kernel_definition, conceptual, 'The fundamental nature of competence and whether it can be fully developed in simulation.').

omega_variable(
    resource_allocation_priority,
    'Should training resources prioritize auditable compliance metrics (simulation) or unquantifiable real-world readiness (costly live exercises)?',
    'Policy debate and stakeholder negotiation, weighing the benefits of compliance against the risks of unaddressed competence gaps.',
    'A shift in priority towards real-world readiness would reduce the extractiveness of the simulation industry and potentially lead to a re-evaluation of regulatory mandates, moving the constraint towards a more balanced rope or even a scaffold if transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_allocation_priority, preference, 'Societal and organizational preference for auditable compliance vs. robust, unquantifiable readiness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.3).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__simulation_sufficiency, theater_ratio, 10, 0.35).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__simulation_sufficiency, theater_ratio, 15, 0.38).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comp_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(comp_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(comp_be_t15, competence_occupation__simulation_sufficiency, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comp_su_t5, competence_occupation__simulation_sufficiency, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(comp_su_t10, competence_occupation__simulation_sufficiency, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(comp_su_t15, competence_occupation__simulation_sufficiency, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'competence_occupation' kernel. The 'simulation_sufficiency' reading emphasizes auditable compliance and risk reduction through simulation, often at the expense of real-world readiness. It is linked to sibling readings 'real_incident_necessity' and 'hybrid_occupation', which offer alternative views on how competence is maintained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
