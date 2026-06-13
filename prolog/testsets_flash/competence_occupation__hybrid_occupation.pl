% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Hybrid Competence Occupation Model
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   In high-reliability organizations (HROs), maintaining operator competence
 *   is critical for safety. This constraint describes the prevailing model
 *   where competence is 'occupied' through a continuous, multi-faceted
 *   regimen including simulations, refreshers, procedural reinforcement, and
 *   line audits. There is no consensus on the optimal mix or frequency,
 *   leading to a complex and often costly system. This constraint is a
 *   reading of the 'competence_occupation' kernel, emphasizing a hybrid
 *   approach over simpler alternatives.
 *
 * KEY AGENTS:
 *   - frontline_operators: Primary target (moderate/constrained) — bears the direct burden of continuous training.
 *   - organizational_budgets: Primary target (institutional/constrained) — bears the financial cost of extensive training programs.
 *   - training_providers: Primary beneficiary (organized/arbitrage) — profits from the demand for diverse training mechanisms.
 *   - safety_regulators: Primary beneficiary (institutional/analytical) — benefits from a robust (if complex) safety regime, avoids liability.
 *   - hro_management: Agenda setter (institutional/constrained) — implements and enforces the training regimen, balancing cost and safety.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.6).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.7).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Competence Occupation Model").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '68734842-0524-489f-a361-9974eb6a0061').
narrative_ontology:cs_kernel_codification('68734842-0524-489f-a361-9974eb6a0061', formalized).
narrative_ontology:cs_authority_grounding('68734842-0524-489f-a361-9974eb6a0061', expertise).
narrative_ontology:cs_interpretation_layer_present('68734842-0524-489f-a361-9974eb6a0061').
narrative_ontology:cs_reading_relation('68734842-0524-489f-a361-9974eb6a0061', competence_occupation__simulation_sufficiency, influences).
narrative_ontology:cs_reading_relation('68734842-0524-489f-a361-9974eb6a0061', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('68734842-0524-489f-a361-9974eb6a0061', foundational, competence_requires_multi_modal_exercise).
narrative_ontology:cs_axiom_status(competence_requires_multi_modal_exercise, holdable).
narrative_ontology:cs_axiom_grounding('68734842-0524-489f-a361-9974eb6a0061', competence_requires_multi_modal_exercise, empirically_contingent).
narrative_ontology:cs_axiom('68734842-0524-489f-a361-9974eb6a0061', foundational, no_single_mechanism_is_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('68734842-0524-489f-a361-9974eb6a0061', no_single_mechanism_is_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('68734842-0524-489f-a361-9974eb6a0061', comprehensive_safety_regime).
narrative_ontology:cs_drift_state('68734842-0524-489f-a361-9974eb6a0061', contemporary_cost_pressure_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('68734842-0524-489f-a361-9974eb6a0061', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_providers).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_regulators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, organizational_budgets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals directly performing high-reliability tasks. They must continuously engage in all mandated training activities to maintain their 'competence occupation' status and employment. They bear the direct time and mental load of the training regimen.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    moderate, biographical, constrained, local).

% The financial resources allocated by the High-Reliability Organization (HRO) for training, equipment, and personnel. This budget is significantly impacted by the extensive, multi-mechanism training requirements, leading to pressure for efficiency.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, organizational_budgets, payer,
    institutional, generational, constrained, national).

% Companies and internal departments that design, deliver, and certify the various training components (simulations, refreshers, procedural reinforcement, line audits). They benefit from the continuous demand for their services due to the hybrid occupation model.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_providers, beneficiary,
    organized, biographical, arbitrage, national).

% Governmental or industry bodies responsible for setting and enforcing safety standards in HROs. They benefit from the comprehensive training approach as it provides a clear framework for compliance and reduces their liability, even if the optimal configuration is unclear.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_regulators, beneficiary,
    institutional, generational, analytical, national).

% The leadership and executive teams of the High-Reliability Organization. They are responsible for implementing and overseeing the competence occupation programs, balancing regulatory compliance, operational costs, and safety outcomes. They set the internal agenda for training within the external regulatory framework.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, hro_management, agenda_setter,
    institutional, generational, constrained, national).

% Academics and industry experts who study training efficacy, skill decay, and optimal learning strategies in HROs. They analyze the effectiveness of current hybrid models and propose improvements, often highlighting the lack of empirical consensus on optimal configuration.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, training_providers).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous maintenance of operator competence across multiple dimensions (cognitive, procedural, behavioral) to ensure safe and reliable operation in high-stakes environments, preventing skill decay and ensuring readiness for rare events.
% TRANSFER_FUNCTION: Transfers significant financial resources from organizational budgets and time/effort from frontline operators to training providers and regulatory compliance efforts, in exchange for certified competence and reduced safety risk.
% ABSENT_VOICES: Operators who have developed highly efficient, personalized training methods that deviate from the mandated hybrid model are excluded; their innovations are not easily integrated into the standardized, multi-mechanism system. Also, alternative, potentially more cost-effective training methodologies that challenge the 'more is better' assumption are often marginalized in favor of comprehensive, regulator-approved programs.
% DISAPPEARANCE_RATIONALE: If the requirement for continuous, multi-mechanism competence occupation vanished, HROs would face immediate regulatory non-compliance, potential loss of operating licenses, and a rapid decline in perceived (and likely actual) safety. Training budgets would be reallocated, and operators would lose a structured path for skill maintenance, leading to a chaotic and dangerous operational environment.
% FOUNDING_PROBLEM: The problem of maintaining high-level operator competence in complex, dynamic, and high-consequence environments, where skill decay is a constant threat and real-world incidents are too rare and costly to serve as primary training events.
% FOUNDING_PROBLEM_CORROBORATION: Safety regulators, HRO management, and training researchers (outside the direct beneficiaries) consistently attest that the core problem of competence maintenance is live and ongoing, citing evolving threats, technological changes, and the inherent challenge of human performance in extreme conditions. While the 'how' is debated, the 'what' (the need for competence) is universally acknowledged.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) due to the significant time, effort, and financial resources required for continuous, multi-mechanism training, often without clear evidence of optimal configuration. Suppression is high (0.7) because operators have little choice but to comply with mandated training to maintain their 'competence occupation' and employment. Theater ratio is low (0.2) as most activities are genuinely aimed at competence, but some elements may be maintained due to regulatory compliance rather than proven efficacy. Accessibility collapse is moderate (0.4) as alternatives (simpler training, less frequent refreshers) are often suppressed by regulatory or organizational inertia. Resistance is moderate (0.3) from operators and budget holders who question the efficiency but cannot easily opt out.
 *
 * PERSPECTIVAL GAP:
 *   Frontline operators experience this as a demanding, often inefficient, and mandatory burden. Training providers and safety regulators, however, view it as a necessary and robust system for maintaining safety and compliance. HRO management attempts to balance these perspectives, often defaulting to the most comprehensive (and costly) approach due to liability concerns.
 *
 * DIRECTIONALITY LOGIC:
 *   Training providers and safety regulators are beneficiaries, as the system creates demand for their services and reduces their liability exposure, respectively. Frontline operators and organizational budgets are victims, bearing the direct costs and time commitments. HRO management acts as the agenda setter, enforcing the system while also being subject to its costs and regulatory pressures.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining competence for safety) is still live. However, the lack of consensus on optimal configuration means the 'how' of the mandate is inefficient. If a simpler, equally effective method were found (e.g., simulation_sufficiency), the current hybrid model would be revealed as a tangled rope with unnecessary extraction. The current complexity prevents it from being a pure rope, as the 'coordination' of multiple mechanisms is not necessarily efficient or optimal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_configuration_ambiguity,
    'What is the optimal configuration of multi-mechanism competence exercise (simulation, refresher, procedural reinforcement, line audits) for a given operational context?',
    'Longitudinal empirical studies correlating different training mixes with incident rates and skill decay curves in specific high-reliability domains.',
    'Resolving this would allow for more efficient and effective training, potentially reducing costs for operators and organizations, and shifting the constraint towards a more ''rope-like'' coordination function by reducing unnecessary overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_configuration_ambiguity, empirical, 'Lack of consensus on optimal training mix for competence occupation.').

omega_variable(
    kernel_reading_hybrid_vs_siblings,
    'Is this ''hybrid occupation'' reading of the competence kernel genuinely superior to ''simulation sufficiency'' or ''real incident necessity'' readings, or is it a compromise driven by institutional inertia?',
    'Comparative analysis of safety outcomes and cost-effectiveness across organizations adopting different primary readings of the competence_occupation kernel.',
    'If ''simulation sufficiency'' were proven adequate, the constraint would become less extractive for operators. If ''real incident necessity'' were proven, the entire training paradigm would shift, potentially making the current constraint obsolete or a ''piton''. This reading''s persistence depends on the perceived inadequacy of simpler alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_hybrid_vs_siblings, conceptual, 'This constraint is the ''hybrid_occupation'' reading of the ''competence_occupation'' kernel. Sibling readings (''simulation_sufficiency'', ''real_incident_necessity'') propose alternative, potentially less complex, means of achieving competence. The disagreement is located in the necessary conditions for ''occupying'' the competence kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.18).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comp_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comp_su_t5, competence_occupation__hybrid_occupation, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(comp_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'competence_occupation' kernel, focusing on a hybrid, multi-mechanism approach. It is linked to sibling readings 'simulation_sufficiency' and 'real_incident_necessity' which propose alternative, simpler, or more extreme conditions for competence occupation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
