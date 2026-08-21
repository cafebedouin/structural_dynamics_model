% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__hybrid_degradation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation-Maintained Competence with Tacit Knowledge Degradation
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint describes the situation where simulation effectively
 *   maintains procedural competence in high-reliability organizations, but
 *   simultaneously allows for a hidden, generational degradation of tacit
 *   knowledge and stress-response capacity due to the absence of real
 *   catastrophic events. The constraint is claimed as a 'rope' by its
 *   beneficiaries (certification and simulation industries) who emphasize its
 *   coordination function, but its actual operation, as described by the
 *   metrics, reveals it to be a 'tangled_rope' due to the asymmetric
 *   extraction of long-term safety margins.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.65).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.7).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Maintained Competence with Tacit Knowledge Degradation").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '9b2f37be-067a-45dc-b1ea-01f30d3ef18d').
narrative_ontology:cs_kernel_codification('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', formalized).
narrative_ontology:cs_authority_grounding('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', expertise).
narrative_ontology:cs_interpretation_layer_present('9b2f37be-067a-45dc-b1ea-01f30d3ef18d').
narrative_ontology:cs_reading_relation('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, influences).
narrative_ontology:cs_reading_relation('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, coexists_with).
narrative_ontology:cs_axiom('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', foundational, simulation_maintains_procedural_competence).
narrative_ontology:cs_axiom_status(simulation_maintains_procedural_competence, holdable).
narrative_ontology:cs_axiom_grounding('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', simulation_maintains_procedural_competence, empirically_contingent).
narrative_ontology:cs_axiom('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', foundational, tacit_knowledge_and_stress_response_degrade_generationally).
narrative_ontology:cs_axiom_status(tacit_knowledge_and_stress_response_degrade_generationally, holdable).
narrative_ontology:cs_axiom_grounding('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', tacit_knowledge_and_stress_response_degrade_generationally, empirically_contingent).
narrative_ontology:cs_reference_frame('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', competence_through_simulated_experience).
narrative_ontology:cs_drift_state('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', contemporary_safety_engineering_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b2f37be-067a-45dc-b1ea-01f30d3ef18d', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_developers).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, high_reliability_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, long_term_safety_margins).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, high_reliability_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and enforces standards for simulation-based training and competence certification. Benefits from the continuous demand for training and assessment services, which are seen as essential for safety. Their revenue stream depends on the perceived sufficiency of simulation.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_industry, beneficiary).

% Invests heavily in simulation training and certification to maintain procedural competence and meet regulatory requirements. Benefits from avoiding real catastrophes and maintaining a trained workforce, but bears the long-term risk of degraded tacit knowledge and stress-response capacity.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, high_reliability_organizations, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, high_reliability_organizations, beneficiary).

% Establishes and oversees safety regulations, often mandating simulation-based training. Relies on the certification industry and simulation developers to provide effective tools. Their primary goal is public safety, but they may not fully perceive the hidden degradation.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_regulators, observer).

% Designs, builds, and sells simulation technologies and training programs. Benefits from the increasing reliance on simulation for competence maintenance. Their business model is tied to the perceived effectiveness and necessity of their products.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_developers, beneficiary,
    organized, biographical, mobile, global).

% Undergoes regular simulation training to maintain procedural competence. Benefits from enhanced skills in routine and anticipated non-catastrophic scenarios. However, they are the ultimate bearers of the risk associated with degraded tacit knowledge and stress-response capacity in actual, unforeseen catastrophic events.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operators, payer).

% Represents the systemic resilience and capacity to absorb unforeseen shocks. It is not an active agent but bears the cost of the constraint's hidden degradation, manifesting as increased vulnerability to novel or extreme events over generational timescales.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, long_term_safety_margins, excluded,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_proxy_sufficiency__hybrid_degradation_reading, long_term_safety_margins).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the maintenance of procedural competence in high-stakes, low-frequency catastrophic domains by providing a standardized, repeatable training environment through simulation, avoiding the need for real-world catastrophic events.
% TRANSFER_FUNCTION: Transfers financial resources from high-reliability organizations to the certification and simulation industries for training and assessment. It also implicitly transfers the risk of degraded tacit knowledge and stress-response capacity to frontline operators and long-term safety margins.
% ABSENT_VOICES: Future generations of operators and the public, who would bear the consequences of degraded tacit knowledge and stress-response capacity in a real catastrophe, are absent from the current discourse. Researchers focused on 'normal accidents' or 'drift into failure' also represent a critical, often marginalized, perspective.
% DISAPPEARANCE_RATIONALE: If simulation-based competence maintenance vanished overnight, high-reliability organizations would face an immediate and severe decline in procedural competence, leading to a rapid increase in operational failures and a breakdown of safety protocols. The entire safety engineering paradigm would need to be rethought.
% FOUNDING_PROBLEM: How to maintain high levels of operational competence and readiness for rare, high-consequence events without relying on actual catastrophic experience, which is unacceptable.
% FOUNDING_PROBLEM_CORROBORATION: While the problem of maintaining procedural competence without real catastrophes is widely acknowledged as live, safety researchers and accident investigators (outside the direct beneficiaries of the simulation industry) attest that the problem of tacit knowledge and stress-response degradation remains unaddressed or is actively obscured, making the 'live' status of the *full* founding problem contested.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because while simulation provides a clear benefit, it also imposes a hidden cost of degrading critical, non-procedural competencies over time, which is borne by the system's long-term resilience. Suppression (0.7) is high as the dominant narrative of simulation's sufficiency actively discourages or marginalizes alternative approaches or critiques that highlight its limitations. Theater ratio (0.4) is moderate; simulation has a genuine function, but a growing portion of its maintenance becomes performative compliance rather than genuine readiness for novel catastrophes. Accessibility collapse (0.75) is high because there are few viable alternatives to simulation for maintaining competence in high-stakes, low-frequency domains. Resistance (0.4) is moderate, primarily from academic researchers and some internal safety critics, but not widespread among operational stakeholders who rely on simulation.
 *
 * PERSPECTIVAL GAP:
 *   The certification industry and simulation developers perceive this as a highly effective 'rope' that solves a critical coordination problem, justifying their ongoing revenue. High-reliability organizations experience it as a necessary cost for safety and compliance, benefiting from procedural competence while unknowingly bearing the long-term risk. Frontline operators benefit from training but are the ultimate victims of the hidden degradation. Safety regulators aim for public safety but may be slow to recognize the subtle, generational degradation.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification and simulation industries are clear beneficiaries, collecting revenue for services. High-reliability organizations are beneficiaries of procedural competence but payers of the hidden degradation. Frontline operators are beneficiaries of training but victims of the degraded tacit knowledge. Long-term safety margins are the ultimate victims, bearing the cost of reduced systemic resilience. Safety regulators are agenda-setters and observers, tasked with ensuring safety but potentially blind to the full scope of the problem.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to maintain competence is partially fulfilled (procedural competence), but its function has atrophied in terms of maintaining tacit knowledge and stress-response capacity. This hidden degradation prevents it from being a true 'rope' and pushes it towards a 'tangled_rope' or even 'snare' for long-term safety. The ongoing revenue streams for the certification and simulation industries ensure its persistence despite this functional atrophy, preventing it from becoming a 'piton'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''hybrid_degradation_reading'' of the ''catastrophe_proxy_sufficiency'' kernel, or does it align more closely with a sibling reading?',
    'Detailed comparative analysis of the structural claims and empirical evidence for each reading, particularly focusing on the specific mechanisms of competence maintenance and degradation.',
    'Reclassification to a different reading would alter the core claims about simulation''s sufficiency and the nature of competence, leading to different policy implications for safety engineering.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific interpretation of the catastrophe proxy sufficiency kernel.').

omega_variable(
    tacit_knowledge_quantification,
    'How can the degradation of tacit knowledge and stress-response capacity be reliably quantified and measured over generational timescales?',
    'Development of novel longitudinal studies, advanced cognitive psychology metrics, and post-incident analysis methodologies that specifically target non-procedural competencies.',
    'Robust quantification would provide empirical grounding for the ''degradation'' claim, potentially shifting the constraint''s classification towards higher extraction and suppression if the degradation is severe and unacknowledged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tacit_knowledge_quantification, empirical, 'Quantifying the hidden degradation of non-procedural competencies.').

omega_variable(
    generational_timescale_validation,
    'Is the ''generational timescale'' for degradation empirically supported, or is the degradation occurring more rapidly or slowly?',
    'Long-term historical analysis of high-reliability organizations, comparing periods of frequent real-world events with periods of heavy simulation reliance, and correlating with incident rates and severity.',
    'A shorter degradation timescale would imply more immediate and severe risks, potentially accelerating calls for systemic change. A longer timescale might reduce perceived urgency but not negate the underlying problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_timescale_validation, empirical, 'Validating the temporal scale of competence degradation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(cata_tr_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(cata_be_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(cata_su_t50, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'catastrophe_proxy_sufficiency' kernel, each representing a distinct structural claim about the role and sufficiency of simulation in maintaining operational competence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
