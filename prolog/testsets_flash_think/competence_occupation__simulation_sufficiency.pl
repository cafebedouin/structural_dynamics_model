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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation Sufficiency for Competence Occupation
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint represents the claim that simulation-based drills are
 *   sufficient for occupying the competence kernel and preventing skill decay
 *   in high-reliability organizations (HROs). This reading frames simulation
 *   as the primary, and often sole, mechanism for competence maintenance,
 *   leading to its widespread adoption and the growth of the simulation
 *   industry. It is one reading of the broader 'competence_occupation'
 *   kernel, which is contested by other views on what constitutes effective
 *   competence maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.7).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.8).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.7).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation Sufficiency for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '0b7e2d17-c819-4039-82c6-bb9e0719b684').
narrative_ontology:cs_kernel_codification('0b7e2d17-c819-4039-82c6-bb9e0719b684', formalized).
narrative_ontology:cs_authority_grounding('0b7e2d17-c819-4039-82c6-bb9e0719b684', expertise).
narrative_ontology:cs_interpretation_layer_present('0b7e2d17-c819-4039-82c6-bb9e0719b684').
narrative_ontology:cs_reading_relation('0b7e2d17-c819-4039-82c6-bb9e0719b684', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('0b7e2d17-c819-4039-82c6-bb9e0719b684', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('0b7e2d17-c819-4039-82c6-bb9e0719b684', foundational, simulation_fidelity_equals_real_world_transfer).
narrative_ontology:cs_axiom_status(simulation_fidelity_equals_real_world_transfer, holdable).
narrative_ontology:cs_axiom_grounding('0b7e2d17-c819-4039-82c6-bb9e0719b684', simulation_fidelity_equals_real_world_transfer, empirically_contingent).
narrative_ontology:cs_axiom('0b7e2d17-c819-4039-82c6-bb9e0719b684', foundational, competence_is_measurable_via_simulation_metrics).
narrative_ontology:cs_axiom_status(competence_is_measurable_via_simulation_metrics, holdable).
narrative_ontology:cs_axiom_grounding('0b7e2d17-c819-4039-82c6-bb9e0719b684', competence_is_measurable_via_simulation_metrics, conventional).
narrative_ontology:cs_reference_frame('0b7e2d17-c819-4039-82c6-bb9e0719b684', simulation_centric_competence_model).
narrative_ontology:cs_drift_state('0b7e2d17-c819-4039-82c6-bb9e0719b684', contemporary_safety_discourse, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('0b7e2d17-c819-4039-82c6-bb9e0719b684', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_compliance_officers).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops, sells, and consults on simulation-based training programs. Actively promotes the idea that their solutions are sufficient for competence maintenance, benefiting directly from the widespread adoption of this claim.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Invest heavily in simulation training to meet regulatory requirements and internal safety standards. They bear the financial cost of these programs and the operational risk if the 'sufficiency' claim proves false in real-world scenarios.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, high_reliability_organizations, payer,
    powerful, biographical, constrained, national).

% Undergo simulation training to maintain their skills and certifications. Their professional competence and safety depend on the efficacy of these drills, yet they have limited input on training design or the 'sufficiency' claim itself.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Responsible for ensuring HROs meet training mandates. The 'simulation sufficiency' claim simplifies their role by providing a clear, measurable standard for compliance, reducing the complexity of competence assessment.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, training_compliance_officers, beneficiary,
    organized, biographical, constrained, national).

% Oversee safety standards in HROs. They often accept simulation-based training as a valid method for competence maintenance, but may investigate its actual efficacy following incidents or new research.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_regulators, observer,
    institutional, generational, analytical, national).

% Safety researchers and former operators who argue that only real-world incidents or highly realistic, unscripted scenarios can truly occupy the competence kernel. Their voices are often marginalized by the dominant 'simulation sufficiency' narrative.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, real_incident_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes and formalizes the process of competence maintenance for high-reliability organizations, providing a scalable and measurable training pathway that avoids the costs and risks of real-world incidents.
% TRANSFER_FUNCTION: Transfers significant financial resources from high-reliability organizations to the simulation industry for training programs and technology. It also transfers perceived competence assurance to regulators and the public.
% ABSENT_VOICES: Advocates for real-incident-based training or more comprehensive, multi-modal competence occupation strategies are often excluded from the discourse that establishes 'sufficiency,' as their arguments challenge the economic and logistical convenience of simulation-only approaches.
% DISAPPEARANCE_RATIONALE: If the claim of simulation sufficiency vanished overnight, high-reliability organizations would face a crisis in competence maintenance. They would scramble to find alternative, likely more costly and risky, methods to ensure operator skill, potentially leading to increased incidents or operational paralysis.
% FOUNDING_PROBLEM: How to effectively and safely maintain high-level operational competence in complex, high-risk environments without relying on actual catastrophic incidents for training, or incurring prohibitive costs for real-world drills.
% FOUNDING_PROBLEM_CORROBORATION: The simulation industry and some HRO management corroborate the ongoing problem of competence maintenance and the sufficiency of simulation as a solution. However, many frontline operators and independent safety researchers contest the 'sufficiency' aspect, citing evidence of skill gaps not adequately addressed by simulation alone, as documented in post-incident analyses and academic studies.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.7) is high because the 'sufficiency' claim allows the simulation industry to capture significant value from HROs, often without rigorous, independent validation of its full efficacy across all competence domains. Suppression (0.8) is high as this claim marginalizes and often excludes alternative, potentially more effective but costly, training methods. The theater ratio (0.4) reflects a genuine coordination function (providing a structured training path) but also a growing performative aspect, where compliance with simulation hours may overshadow actual skill transfer. Accessibility collapse (0.75) is high because once 'sufficiency' is accepted, other training paths are deemed unnecessary. Resistance (0.5) is moderate, as some frontline operators and safety experts continue to challenge the claim, but institutional inertia and regulatory acceptance limit their impact.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the simulation industry, this constraint is a Rope, providing a vital coordination function for safety. From the perspective of HROs and frontline operators, it functions more as a Tangled Rope, coordinating training but extracting resources and potentially leaving competence gaps. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation industry is the primary beneficiary and agenda-setter, profiting directly from the widespread acceptance of simulation sufficiency. Training compliance officers also benefit from a clear, measurable standard. High-reliability organizations and frontline operators are the payers, bearing the financial costs and the ultimate risk if competence is not fully maintained. Real incident advocates are structurally excluded, as their arguments directly contradict the 'sufficiency' claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'How does this ''simulation_sufficiency'' reading of the ''competence_occupation'' kernel structurally differ from its siblings, ''real_incident_necessity'' and ''hybrid_occupation''?',
    'Comparative analysis of the core axioms and their empirical grounding across all readings, identifying points of logical foreclosure, coexistence, or influence.',
    'Understanding these structural differences clarifies the specific mechanisms of extraction and suppression inherent to each reading, informing targeted interventions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Contextualizes this constraint as one reading within a contested kernel.').

omega_variable(
    empirical_sufficiency_validation,
    'Is the claimed ''sufficiency'' of simulation empirically validated across all critical competence domains and operational contexts for high-reliability organizations?',
    'Longitudinal studies comparing competence outcomes of simulation-only training with real-world performance, and with outcomes from hybrid or real-incident-based training regimes.',
    'If empirical validation is lacking or negative, the constraint''s extractiveness and theater_ratio would be re-evaluated upward, potentially reclassifying it closer to a Snare due to false claims of efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_sufficiency_validation, empirical, 'Whether simulation truly provides sufficient competence occupation.').

omega_variable(
    competence_kernel_definition,
    'What constitutes ''occupation of the competence kernel,'' and is it fully measurable by simulation-derived metrics alone, or does it require real-world performance indicators and adaptive capacity assessments?',
    'Consensus-building among HROs, safety researchers, and regulators on a multi-dimensional definition of competence, and the development of validated metrics that capture adaptive performance beyond scripted simulation scenarios.',
    'A broader definition of competence would challenge the ''sufficiency'' claim, increasing resistance and potentially leading to a re-evaluation of training requirements and resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_kernel_definition, conceptual, 'Ambiguity in defining and measuring competence kernel occupation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__simulation_sufficiency, theater_ratio, 4, 0.3).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__simulation_sufficiency, theater_ratio, 8, 0.35).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__simulation_sufficiency, theater_ratio, 12, 0.38).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__simulation_sufficiency, theater_ratio, 16, 0.39).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t4, competence_occupation__simulation_sufficiency, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(comp_be_t8, competence_occupation__simulation_sufficiency, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(comp_be_t12, competence_occupation__simulation_sufficiency, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(comp_be_t16, competence_occupation__simulation_sufficiency, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(comp_su_t4, competence_occupation__simulation_sufficiency, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(comp_su_t8, competence_occupation__simulation_sufficiency, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(comp_su_t12, competence_occupation__simulation_sufficiency, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(comp_su_t16, competence_occupation__simulation_sufficiency, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'simulation_sufficiency' reading of the 'competence_occupation' kernel. It is structurally distinct from the 'real_incident_necessity' and 'hybrid_occupation' readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
