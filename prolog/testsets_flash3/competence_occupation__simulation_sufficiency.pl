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
 *   This constraint represents the reading that simulation-based drills are
 *   sufficient for competence maintenance in High-Reliability Organizations
 *   (HROs). It is a contested claim, with this reading emphasizing the
 *   benefits of standardization and measurability, while other readings
 *   (real_incident_necessity, hybrid_occupation) argue for more robust or
 *   diverse training methods. The constraint operates as a Tangled Rope,
 *   providing a coordination function (standardized training) but with
 *   significant asymmetric extraction from frontline operators and public
 *   safety, benefiting the simulation industry and HRO management.
 *
 * KEY AGENTS:
 *   - simulation_industry: Agenda setter (institutional/arbitrage) — promotes and benefits from simulation sufficiency.
 *   - hro_management: Beneficiary (institutional/constrained) — benefits from auditable compliance and reduced costs.
 *   - frontline_operators: Payer (moderate/identity_locked) — bears the risk of skill decay from insufficient training.
 *   - public_safety_advocates: Payer (organized/constrained) — bears diffuse costs of potential safety failures.
 *   - regulators: Observer (institutional/analytical) — codifies standards, influenced by industry.
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
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '12d609a4-1075-415f-a470-d2379bbcdc50').
narrative_ontology:cs_kernel_codification('12d609a4-1075-415f-a470-d2379bbcdc50', formalized).
narrative_ontology:cs_authority_grounding('12d609a4-1075-415f-a470-d2379bbcdc50', extraction).
narrative_ontology:cs_interpretation_layer_present('12d609a4-1075-415f-a470-d2379bbcdc50').
narrative_ontology:cs_reading_relation('12d609a4-1075-415f-a470-d2379bbcdc50', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_reading_relation('12d609a4-1075-415f-a470-d2379bbcdc50', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('12d609a4-1075-415f-a470-d2379bbcdc50', foundational, simulation_as_equivalent_experience).
narrative_ontology:cs_axiom_status(simulation_as_equivalent_experience, holdable).
narrative_ontology:cs_axiom_grounding('12d609a4-1075-415f-a470-d2379bbcdc50', simulation_as_equivalent_experience, empirically_contingent).
narrative_ontology:cs_axiom('12d609a4-1075-415f-a470-d2379bbcdc50', secondary, measurable_compliance_as_competence_proxy).
narrative_ontology:cs_axiom_status(measurable_compliance_as_competence_proxy, holdable).
narrative_ontology:cs_axiom_grounding('12d609a4-1075-415f-a470-d2379bbcdc50', measurable_compliance_as_competence_proxy, conventional).
narrative_ontology:cs_reference_frame('12d609a4-1075-415f-a470-d2379bbcdc50', standardized_auditable_training).
narrative_ontology:cs_drift_state('12d609a4-1075-415f-a470-d2379bbcdc50', contemporary_cost_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('12d609a4-1075-415f-a470-d2379bbcdc50', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, hro_management).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, public_safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and sells simulation platforms and services, benefiting directly from the widespread adoption of simulation as the primary method for competence maintenance. Actively promotes the 'sufficiency' narrative.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_industry, agenda_setter,
    organized, biographical, arbitrage, global).

% Benefits from a quantifiable, auditable training compliance metric that reduces perceived liability and operational costs compared to more complex, multi-modal training regimes. Supports the simulation-sufficiency claim for its administrative convenience.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, hro_management, beneficiary,
    institutional, generational, constrained, national).

% Are required to undergo simulation-based training, which may not fully prepare them for the cognitive and emotional demands of real incidents. They bear the risk of skill decay and inadequate preparation, often without a voice in training design.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Bear the diffuse costs of potential safety failures if simulation proves insufficient. They advocate for more robust, real-world training but face resistance from industry and management due to cost and logistical complexity.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, public_safety_advocates, payer,
    organized, generational, constrained, national).

% Are tasked with setting and enforcing training standards. They often rely on industry-provided data and face lobbying pressure, leading to a tendency to codify simulation-based training as sufficient without deep independent validation.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes competence maintenance across large, distributed High-Reliability Organizations (HROs) by providing a scalable, measurable, and auditable training method.
% TRANSFER_FUNCTION: Transfers training budgets from HROs to the simulation industry, and transfers the burden of skill maintenance from diverse, complex methods to a single, auditable simulation metric, often at the expense of frontline operator preparedness.
% ABSENT_VOICES: Experienced operators who have faced real incidents and found simulation inadequate are often marginalized in training policy discussions. Their experiential knowledge would challenge the 'sufficiency' claim.
% DISAPPEARANCE_RATIONALE: If the belief in simulation sufficiency vanished, HROs would face a crisis in competence maintenance, scrambling to implement more diverse and costly training methods. The simulation industry would lose a significant market, and regulatory bodies would need to redefine compliance.
% FOUNDING_PROBLEM: Ensuring consistent, measurable competence across a large workforce in complex, high-risk environments, especially when real incidents are rare and costly to use for training.
% FOUNDING_PROBLEM_CORROBORATION: HRO management and the simulation industry attest the problem is live, emphasizing the ongoing need for scalable training. Public safety advocates and some independent researchers corroborate the problem's existence but contest the sufficiency of the proposed solution, arguing for a broader approach to competence occupation.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the cost savings and revenue generation for beneficiaries come at the expense of potentially suboptimal competence for operators. Suppression is high due to regulatory capture and the administrative convenience of simulation, which makes it difficult for alternative training paradigms to gain traction. Theater ratio is moderate and rising, as the 'sufficiency' claim increasingly serves to justify the existing training regime rather than genuinely optimize competence. The increasing extractiveness and suppression over time reflect the hardening of this claim into an institutionalized practice.
 *
 * PERSPECTIVAL GAP:
 *   HRO management and the simulation industry perceive this as a highly efficient and effective coordination mechanism, solving a complex training problem. Frontline operators and public safety advocates, however, experience it as an extractive mechanism that prioritizes cost and compliance over genuine preparedness, leading to a divergence in computed constraint types.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation industry and HRO management are clear beneficiaries, with the former actively setting the agenda. Frontline operators are targets, as their professional identity is tied to the HRO, making exit difficult even if they perceive training as inadequate. Public safety advocates bear diffuse costs. Regulators are observers, whose directionality depends on their independence from industry influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (which would ignore the asymmetric extraction and suppression of alternatives) or a Snare (which would ignore the genuine, albeit contested, coordination function of standardized training). The Mandatrophy analysis suggests that while the founding problem (scalable competence maintenance) is live, the 'sufficiency' claim may be leading to a degradation of the actual competence kernel, turning the coordination into a cover for rent-seeking and risk externalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'At what level of fidelity and complexity do simulations genuinely occupy the competence kernel, and is this level consistently achieved in practice?',
    'Independent, longitudinal studies comparing performance in high-fidelity simulations to performance in real incidents, controlling for operator experience and incident type.',
    'If current simulations fall below a critical fidelity threshold, the ''sufficiency'' claim is empirically false, reclassifying the constraint towards Snare due to the false promise of competence. If fidelity is sufficient, it strengthens the Rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Empirical validation of simulation effectiveness for competence.').

omega_variable(
    cost_benefit_externalization,
    'To what extent are the true costs of potential skill decay (e.g., incident response, reputational damage, loss of life) externalized from HRO management and the simulation industry to frontline operators and the public?',
    'Comprehensive economic modeling that internalizes all social costs of incidents into the HRO''s and simulation industry''s balance sheets.',
    'If externalization is high, the extractiveness of the constraint is significantly underestimated, pushing it further towards Snare. If costs are largely internalized, the Tangled Rope classification is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_externalization, empirical, 'Assessment of externalized costs of simulation-based training.').

omega_variable(
    framing_of_competence_kernel,
    'Is the ''competence kernel'' primarily a set of technical skills (amenable to simulation) or does it include non-technical skills like stress management, improvisation, and ethical decision-making under pressure (less amenable to simulation)?',
    'Conceptual analysis and expert consensus from cognitive psychology, human factors engineering, and ethics in high-risk domains.',
    'If the kernel is broader than technical skills, the ''simulation_sufficiency'' reading is conceptually flawed, weakening its legitimacy and pushing the constraint towards Snare. If it''s primarily technical, the reading is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_competence_kernel, conceptual, 'Conceptual scope of the ''competence kernel''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.28).
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
% This constraint is one reading ('simulation_sufficiency') of the 'competence_occupation' kernel. It is linked to sibling readings 'real_incident_necessity' and 'hybrid_occupation', which offer alternative perspectives on how competence is maintained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
