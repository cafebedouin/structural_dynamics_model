% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: AI Alignment: Existential Risk Priority Reading
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.85).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, snare).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Alignment: Existential Risk Priority Reading").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '60cfc320-b3de-4122-bb5a-d57b738531b7').
narrative_ontology:cs_kernel_codification('60cfc320-b3de-4122-bb5a-d57b738531b7', distributed).
narrative_ontology:cs_authority_grounding('60cfc320-b3de-4122-bb5a-d57b738531b7', expertise).
narrative_ontology:cs_interpretation_layer_present('60cfc320-b3de-4122-bb5a-d57b738531b7').
narrative_ontology:cs_reading_relation('60cfc320-b3de-4122-bb5a-d57b738531b7', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_reading_relation('60cfc320-b3de-4122-bb5a-d57b738531b7', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('60cfc320-b3de-4122-bb5a-d57b738531b7', foundational, existential_risk_is_primary_ai_safety_concern).
narrative_ontology:cs_axiom_status(existential_risk_is_primary_ai_safety_concern, holdable).
narrative_ontology:cs_axiom_grounding('60cfc320-b3de-4122-bb5a-d57b738531b7', existential_risk_is_primary_ai_safety_concern, empirically_contingent).
narrative_ontology:cs_axiom('60cfc320-b3de-4122-bb5a-d57b738531b7', secondary, long_term_future_trumps_near_term_harms).
narrative_ontology:cs_axiom_status(long_term_future_trumps_near_term_harms, holdable).
narrative_ontology:cs_axiom_grounding('60cfc320-b3de-4122-bb5a-d57b738531b7', long_term_future_trumps_near_term_harms, deontological).
narrative_ontology:cs_reference_frame('60cfc320-b3de-4122-bb5a-d57b738531b7', catastrophic_risk_prevention_framework).
narrative_ontology:cs_drift_state('60cfc320-b3de-4122-bb5a-d57b738531b7', contemporary_ai_development_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60cfc320-b3de-4122-bb5a-d57b738531b7', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_day_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_ai_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, ai_developers_diverted_resources).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Focus on preventing catastrophic loss of control over advanced AI systems, prioritizing existential safety. They advocate for significant resource allocation to long-term alignment research, often through adversarial red-teaming methodologies. Their careers and funding depend on the perceived urgency and severity of this risk.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_researchers, agenda_setter,
    institutional, civilizational, constrained, global).

% The ultimate beneficiary of successful existential risk mitigation, representing all future generations whose existence is secured by preventing catastrophic AI outcomes. This entity is an abstract good, not an active agent.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, long_term_future_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).

% Bear the costs of diverted attention and resources from immediate AI harms (e.g., algorithmic bias, surveillance, job displacement) to speculative future risks. Their present suffering is deprioritized in favor of a distant, uncertain future.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_day_marginalized_populations, payer,
    powerless, immediate, trapped, global).

% Focus on addressing present discriminatory and extractive harms from deployed AI. They find their research and policy recommendations deprioritized and underfunded compared to existential risk work, leading to a struggle for relevance and resources.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_ai_ethics_researchers, payer,
    moderate, biographical, constrained, global).

% Are pressured to integrate existential safety measures into their development pipelines, potentially diverting resources from immediate product development, ethical deployment, or addressing known biases. While they benefit from long-term safety, the immediate costs are borne by their development cycles and profit margins.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_developers_diverted_resources, payer,
    powerful, biographical, constrained, global).

% Advocate for a balanced approach that addresses both catastrophic and present harms. They are often marginalized in the discourse, as the dominant framing forces a choice between 'existential' and 'nearterm' priorities, rather than allowing for complementarity.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, integrated_alignment_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research efforts and policy discussions around a singular, overarching goal: the prevention of AI-induced existential catastrophe, ensuring a common framework for risk assessment and mitigation.
% TRANSFER_FUNCTION: Transfers significant intellectual, financial, and political capital from addressing present-day AI harms and developing beneficial AI applications to speculative, long-term existential risk research and mitigation strategies.
% ABSENT_VOICES: Marginalized communities experiencing immediate AI harms, and researchers focused on near-term ethical deployment, are largely absent from the core decision-making bodies and funding allocations, their concerns deemed secondary to existential threats.
% DISAPPEARANCE_RATIONALE: If the existential risk priority vanished overnight, the global AI safety discourse would immediately reorient towards near-term harms and beneficial applications. Funding streams would shift, research agendas would change, and policy efforts would focus on current societal impacts, fundamentally altering the trajectory of AI governance.
% FOUNDING_PROBLEM: The potential for advanced AI systems to develop goals misaligned with human values, leading to an uncontrollable intelligence explosion and the extinction of humanity.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by a significant portion of the AI research community, prominent public intellectuals, and some government bodies, who cite theoretical arguments and escalating AI capabilities. Critics from near-term AI ethics and social justice fields contest the framing and urgency, arguing it distracts from verifiable present harms.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_justification,
    'Is the current allocation of resources to existential AI risk proportional to its empirically verifiable probability and severity compared to near-term AI harms?',
    'Independent, interdisciplinary risk assessment incorporating social science, economics, and technical AI safety research, with transparent methodology and diverse expert input.',
    'If disproportionate, it would strengthen the ''snare'' classification by revealing a misallocation of resources driven by speculative rather than evidence-based prioritization, increasing effective extraction for near-term victims. If proportionate, it would lend more credence to the ''rope'' or ''tangled_rope'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_justification, empirical, 'Assesses whether resource allocation aligns with evidence-based risk assessment.').

omega_variable(
    framing_impact_on_action,
    'Does the ''existential risk'' framing genuinely accelerate effective AI safety measures, or does it primarily serve to centralize power and resources within a specific research paradigm?',
    'Longitudinal study of policy outcomes, funding trends, and the diversity of voices in AI governance, comparing periods of dominant existential framing with periods of more integrated or near-term focus.',
    'If it centralizes power without clear acceleration of effective safety, it would increase the ''snare'' classification by revealing a performative aspect (theater_ratio would rise) and a hidden beneficiary (power centralization). If it genuinely accelerates, it would support a ''rope'' or ''scaffold'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_impact_on_action, conceptual, 'Examines the functional impact of the existential risk framing on AI safety outcomes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of near-term harms discourse structural (e.g., funding mechanisms, institutional inertia) or internalized (e.g., self-censorship by researchers fearing irrelevance)?',
    'Post-exit suppression trajectory: if researchers shift focus to near-term harms after leaving existential risk-focused institutions, it suggests structural suppression. If the deprioritization persists, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. If structural, targeted policy interventions could more easily alleviate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for near-term AI ethics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2015, ai_alignment_priority__existential_risk_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_priority__existential_risk_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(ai_a_tr_t2025, ai_alignment_priority__existential_risk_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement(ai_a_tr_t2030, ai_alignment_priority__existential_risk_reading, theater_ratio, 2030, 0.19).
narrative_ontology:measurement(ai_a_tr_t2035, ai_alignment_priority__existential_risk_reading, theater_ratio, 2035, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2015, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(ai_a_be_t2025, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2025, 0.8).
narrative_ontology:measurement(ai_a_be_t2030, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2030, 0.83).
narrative_ontology:measurement(ai_a_be_t2035, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2035, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2015, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(ai_a_su_t2025, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2025, 0.65).
narrative_ontology:measurement(ai_a_su_t2030, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2030, 0.68).
narrative_ontology:measurement(ai_a_su_t2035, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2035, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__existential_risk_reading, 0.15).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI Alignment Priority' kernel. It focuses on existential risk, influencing and being influenced by other readings that prioritize near-term harms or an integrated approach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
