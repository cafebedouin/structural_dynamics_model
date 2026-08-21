% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Competence Retention via Hybrid Near-Miss Learning
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint describes the organizational learning paradigm in
 *   high-reliability systems, where competence is maintained not by isolated
 *   simulation or by waiting for actual catastrophes, but through a hybrid
 *   approach of distributed learning from near-misses, foreign incidents, and
 *   high-realism drills. It is a reading of the
 *   'catastrophe_avoidance_retention' kernel, emphasizing the necessity of
 *   continuous, multi-source learning networks to prevent systemic failures.
 *
 * KEY AGENTS:
 *   - High_reliability_organizations: Agenda setter / Beneficiary (institutional/constrained)
 *   - Safety_regulators: Agenda setter / Observer (institutional/analytical)
 *   - Frontline_operators: Payer / Beneficiary (moderate/constrained)
 *   - Organizational_budgets: Payer (powerless/trapped)
 *   - Catastrophe_as_necessary_selector_proponents: Excluded (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.25).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.4).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Competence Retention via Hybrid Near-Miss Learning").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '22642dba-77d4-49c1-8d21-9f62d5392e0e').
narrative_ontology:cs_kernel_codification('22642dba-77d4-49c1-8d21-9f62d5392e0e', formalized).
narrative_ontology:cs_authority_grounding('22642dba-77d4-49c1-8d21-9f62d5392e0e', expertise).
narrative_ontology:cs_interpretation_layer_present('22642dba-77d4-49c1-8d21-9f62d5392e0e').
narrative_ontology:cs_reading_relation('22642dba-77d4-49c1-8d21-9f62d5392e0e', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('22642dba-77d4-49c1-8d21-9f62d5392e0e', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, influences).
narrative_ontology:cs_axiom('22642dba-77d4-49c1-8d21-9f62d5392e0e', foundational, learning_from_failure_is_distributed_and_hybrid).
narrative_ontology:cs_axiom_status(learning_from_failure_is_distributed_and_hybrid, holdable).
narrative_ontology:cs_axiom_grounding('22642dba-77d4-49c1-8d21-9f62d5392e0e', learning_from_failure_is_distributed_and_hybrid, empirically_contingent).
narrative_ontology:cs_axiom('22642dba-77d4-49c1-8d21-9f62d5392e0e', foundational, catastrophe_is_avoidable_through_proactive_learning).
narrative_ontology:cs_axiom_status(catastrophe_is_avoidable_through_proactive_learning, holdable).
narrative_ontology:cs_axiom_grounding('22642dba-77d4-49c1-8d21-9f62d5392e0e', catastrophe_is_avoidable_through_proactive_learning, empirically_contingent).
narrative_ontology:cs_reference_frame('22642dba-77d4-49c1-8d21-9f62d5392e0e', proactive_safety_culture).
narrative_ontology:cs_drift_state('22642dba-77d4-49c1-8d21-9f62d5392e0e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('22642dba-77d4-49c1-8d21-9f62d5392e0e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, public_safety).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizational_budgets).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, individual_reporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_consultants_and_trainers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, public_safety_advocates).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations operating in high-consequence environments (e.g., aviation, nuclear power) that actively implement and benefit from distributed learning systems, drills, and incident sharing to maintain operational competence and avoid catastrophic failures. They bear the direct costs of these systems.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organizations, beneficiary).

% Governmental bodies that mandate safety standards, incident reporting, and training requirements. They oversee compliance and contribute to the shared knowledge base, benefiting from a safer operating environment.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators, observer).

% Individuals who perform high-consequence tasks. They participate in drills, report near-misses, and implement safety protocols. While bearing the burden of training and reporting, they are primary beneficiaries of enhanced safety.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators, beneficiary).

% External experts and firms that provide specialized knowledge, tools, and training for implementing and maintaining hybrid learning systems. They benefit financially from the demand for these services.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_consultants_and_trainers, beneficiary,
    organized, biographical, mobile, global).

% Non-governmental organizations and researchers focused on improving public safety in high-risk domains. They benefit from the reduction of catastrophic events and contribute to the discourse on best practices.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, public_safety_advocates, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, public_safety_advocates, observer).

% The financial resources allocated within organizations for safety initiatives, training, and incident investigation. These budgets bear the direct costs of maintaining the hybrid learning system.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizational_budgets, payer,
    powerless, immediate, trapped, local).

% Frontline personnel who report near-misses and incidents. While essential for learning, they bear the psychological burden and potential career risk if a 'just culture' is not perfectly maintained, making them victims of potential reprisal or blame.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, individual_reporters, payer,
    powerless, immediate, constrained, local).

% Advocates for the view that only actual catastrophes provide the necessary selection pressure for competence. Their perspective is excluded from the core design principles of this hybrid learning system, which seeks to avoid catastrophes.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector_proponents, excluded,
    organized, generational, analytical, global).

% Advocates for the view that high-fidelity simulation is functionally equivalent to real catastrophic events for competence maintenance. While simulation is a component of hybrid learning, their view that it is *sufficient* alone is excluded from this constraint's core premise.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe_proponents, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate distributed learning from diverse sources (near-misses, foreign incidents, high-realism drills) across multiple organizations and timeframes, ensuring continuous competence retention and systemic catastrophe avoidance in high-consequence domains.
% TRANSFER_FUNCTION: Transfers resources (time, money, data, attention) from individual organizational budgets and operational schedules into shared incident databases, cross-organizational learning networks, and high-fidelity training programs, in exchange for enhanced collective safety and resilience.
% ABSENT_VOICES: Proponents of 'catastrophe as necessary selector' and 'simulation as proxy catastrophe' are structurally excluded from the design and primary justification of this hybrid learning approach, as their core premises are either directly contradicted or deemed insufficient by this constraint. They would argue for different resource allocations and learning methodologies.
% DISAPPEARANCE_RATIONALE: If this hybrid learning constraint vanished, high-reliability organizations would lose their primary mechanism for proactive competence retention. Learning would become fragmented, incident data would not be shared effectively, and the frequency and severity of catastrophic failures would likely increase, forcing a reorganization of safety practices around less effective, more reactive methods.
% FOUNDING_PROBLEM: High-consequence industries faced recurrent catastrophic failures because neither isolated organizational learning nor infrequent, high-cost real-world disasters provided sufficient, timely, or comprehensive feedback for continuous competence maintenance. Over-reliance on pure simulation also proved inadequate for real-world complexity.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing need for this approach is corroborated by decades of safety research in aviation, nuclear power, and medicine, as well as by independent accident investigation reports and academic studies on high-reliability organizations, which consistently highlight the importance of distributed, multi-modal learning for preventing systemic failures.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).
:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is relatively low (0.25) because the primary function is genuine coordination for safety, and the 'extraction' represents the necessary investment in complex learning systems rather than rent-seeking. Suppression (0.40) is moderate, reflecting the need for a 'just culture' to encourage reporting, alongside regulatory mandates for compliance. Theater ratio (0.30) is also moderate; while drills can sometimes become performative, the core intent is genuine learning and adaptation. Accessibility collapse is high (0.80) because the alternative to this robust learning is a higher risk of catastrophic failure, which is unacceptable in these domains. Resistance (0.50) exists due to the significant resource commitment and cultural shifts required for effective implementation.
 *
 * PERSPECTIVAL GAP:
 *   High-reliability organizations and safety regulators largely view this as an essential, beneficial coordination mechanism. However, organizational budgets experience it as a continuous cost, and individual reporters may feel pressure or fear reprisal if the 'just culture' is not perfectly upheld. Proponents of alternative learning paradigms (e.g., catastrophe as necessary selector) would view this constraint as an insufficient or even misguided approach, leading to a fundamental divergence in perceived effectiveness and necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations and safety regulators are beneficiaries, as they gain enhanced safety and legitimacy from the system. Frontline operators are both payers (time, effort, risk of reporting) and beneficiaries (personal safety). Organizational budgets are clear payers. Safety consultants are beneficiaries, profiting from the system's implementation. Individual reporters are victims if the system's 'just culture' fails to protect them. Proponents of alternative views are excluded, as their core premises are not integrated into this constraint's operational logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively addresses the risk of mandatrophy by continuously adapting through learning from near-misses and incidents. Its mandate is to prevent catastrophes, and as long as that risk is live (which it is in high-consequence domains), the constraint's function remains vital. The hybrid nature prevents over-reliance on any single, potentially atrophied, learning mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''hybrid_near_miss_learning'' reading of the ''catastrophe_avoidance_retention'' kernel, or does it conflate elements of sibling readings?',
    'Detailed textual analysis of foundational documents and expert testimony from proponents of each reading to ensure strict adherence to the unique premises of ''hybrid_near_miss_learning''.',
    'If conflated, the classification may inaccurately reflect the structural properties unique to this reading, potentially misrepresenting its coordination function or extractive components.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the precise instantiation of the ''hybrid_near_miss_learning'' reading within the ''catastrophe_avoidance_retention'' kernel.').

omega_variable(
    effectiveness_of_hybrid_learning,
    'To what extent does the investment in hybrid near-miss learning genuinely prevent catastrophes, versus merely delaying them or shifting risk to unmeasured areas?',
    'Longitudinal epidemiological studies of accident rates in industries adopting this approach, compared to control groups or historical baselines, alongside detailed qualitative analysis of ''black swan'' events.',
    'If effectiveness is lower than assumed, the constraint''s ''rope'' classification might shift towards ''tangled_rope'' or ''snare'' due to higher effective extraction (costs without commensurate safety benefits).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_hybrid_learning, empirical, 'Assesses the true impact of hybrid learning on catastrophe prevention.').

omega_variable(
    just_culture_integrity,
    'Is the ''just culture'' necessary for effective near-miss reporting genuinely maintained, or do individual reporters face subtle or overt reprisal, making them true victims?',
    'Anonymous surveys of frontline operators, analysis of incident reporting trends (e.g., underreporting of certain types of incidents), and independent audits of organizational safety culture.',
    'If the ''just culture'' is compromised, the suppression metric for individual reporters would be higher, increasing their effective extraction and potentially shifting the constraint''s classification for that seat towards ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_culture_integrity, empirical, 'Evaluates the integrity of the ''just culture'' for incident reporting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 10, 0.28).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 30, 0.3).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.29).
narrative_ontology:measurement(cata_tr_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(cata_be_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(cata_su_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, information_standard).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulation_compliance).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizational_risk_management).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_avoidance_retention' kernel. The other readings are 'simulation_as_proxy_catastrophe' and 'catastrophe_as_necessary_selector', each representing a distinct approach to maintaining competence and avoiding systemic failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
