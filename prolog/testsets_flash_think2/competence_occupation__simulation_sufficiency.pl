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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Simulation-Based Competence Maintenance Sufficiency
 *   domain: High-Reliability Organizations / Safety Training / Competence Maintenance
 *
 * SUMMARY:
 *   This constraint represents the 'simulation_sufficiency' reading of the
 *   'competence_occupation' kernel. It asserts that simulation-based drills
 *   alone are sufficient to maintain critical competence and prevent skill
 *   decay in high-reliability organizations. This reading emphasizes the
 *   scalability, safety, and cost-effectiveness of simulation, often leading
 *   to significant investment in simulation technology and mandatory training
 *   programs. The simulation industry emerges as a primary beneficiary, while
 *   frontline operators and organizations bear the costs of compliance and
 *   potential gaps in real-world readiness.
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
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Based Competence Maintenance Sufficiency").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "High-Reliability Organizations / Safety Training / Competence Maintenance").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, 'f9771058-ab5c-4caa-8893-e29e253e5b47').
narrative_ontology:cs_kernel_codification('f9771058-ab5c-4caa-8893-e29e253e5b47', formalized).
narrative_ontology:cs_authority_grounding('f9771058-ab5c-4caa-8893-e29e253e5b47', expertise).
narrative_ontology:cs_interpretation_layer_present('f9771058-ab5c-4caa-8893-e29e253e5b47').
narrative_ontology:cs_reading_relation('f9771058-ab5c-4caa-8893-e29e253e5b47', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('f9771058-ab5c-4caa-8893-e29e253e5b47', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('f9771058-ab5c-4caa-8893-e29e253e5b47', foundational, simulation_fidelity_is_sufficient).
narrative_ontology:cs_axiom_status(simulation_fidelity_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('f9771058-ab5c-4caa-8893-e29e253e5b47', simulation_fidelity_is_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('f9771058-ab5c-4caa-8893-e29e253e5b47', foundational, competence_is_simulable).
narrative_ontology:cs_axiom_status(competence_is_simulable, holdable).
narrative_ontology:cs_axiom_grounding('f9771058-ab5c-4caa-8893-e29e253e5b47', competence_is_simulable, empirically_contingent).
narrative_ontology:cs_reference_frame('f9771058-ab5c-4caa-8893-e29e253e5b47', simulation_as_gold_standard_competence_maintenance).
narrative_ontology:cs_drift_state('f9771058-ab5c-4caa-8893-e29e253e5b47', contemporary_safety_culture, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f9771058-ab5c-4caa-8893-e29e253e5b47', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_providers).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, organizational_leadership).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and sells simulation technologies and platforms. Actively promotes the idea that high-fidelity simulation is sufficient for competence maintenance, benefiting directly from increased adoption and investment in simulation solutions.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Deliver simulation-based training programs, often certified by the simulation industry or regulatory bodies. They benefit from the mandate for simulation training and the associated revenue streams.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, training_providers, beneficiary,
    organized, biographical, mobile, national).

% Implements simulation-based training programs to meet regulatory compliance, manage perceived risk, and demonstrate commitment to safety. They benefit from a standardized, scalable training solution and the narrative of competence maintenance, even if the actual efficacy is debated.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, organizational_leadership, beneficiary,
    institutional, biographical, constrained, national).

% Undergo mandatory simulation drills to maintain their professional certifications and operational readiness. They bear the direct cost of time and effort, and may experience skill decay or gaps if simulations do not fully replicate real-world conditions, but their professional identity is tied to compliance.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Invest heavily in simulation technology, infrastructure, and training programs. They bear the financial cost of implementation and maintenance, driven by regulatory requirements and the perceived necessity of simulation for competence.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, organizations, payer,
    organized, generational, constrained, national).

% Oversee and enforce training standards, often incorporating simulation requirements. They observe the outcomes of simulation-based training and can influence its scope and fidelity, but are also influenced by industry lobbying and the perceived efficiency of simulation.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, safety_regulators, agenda_setter).

% Conduct independent research into the efficacy and limitations of simulation for competence maintenance. They often highlight gaps between simulated and real-world performance, or advocate for hybrid training models, but their findings may be marginalized in policy discussions dominated by industry interests.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, critical_safety_researchers, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, scalable, and relatively safe method for training and assessing critical operational competence across a large workforce, addressing the logistical and safety challenges of real-world training.
% TRANSFER_FUNCTION: Transfers significant financial resources from organizations to the simulation industry and training providers, in exchange for perceived competence maintenance, regulatory compliance, and a reduction in real-world training risks.
% ABSENT_VOICES: Researchers and practitioners who emphasize the inherent limitations of simulation, or advocate for real-world experience and hybrid training models, are often excluded from policy-setting bodies that define 'sufficient' competence maintenance. Their concerns are often framed as impractical or overly costly.
% DISAPPEARANCE_RATIONALE: If the belief in simulation sufficiency vanished overnight, organizations would face a crisis in competence maintenance. They would be forced to rapidly re-evaluate training strategies, likely increasing reliance on costly and risky real-world exercises, leading to significant operational disruption and a complete restructuring of the simulation and training industries.
% FOUNDING_PROBLEM: The high cost, inherent risks, and logistical complexities of providing sufficient real-world training for complex, high-consequence tasks, coupled with the need for standardized and measurable competence assessment across large, distributed workforces.
% FOUNDING_PROBLEM_CORROBORATION: The simulation industry and organizational leadership attest that the founding problems (cost, risk, scalability of real-world training) remain live. However, critical safety researchers and some frontline operators argue that while the original problems persist, the *sufficiency* claim of simulation has become an overreach, and the current arrangement primarily serves the economic interests of the simulation and training providers, rather than optimal competence maintenance. Independent safety boards often issue reports that implicitly or explicitly question the 'sufficiency' claim, supporting the contested status.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (standardized competence maintenance) but also involves significant asymmetric extraction. Extractiveness is high (0.65) due to the substantial financial transfers to the simulation industry and training providers, often exceeding the marginal cost of the service. Suppression is also high (0.70) as alternative training methodologies or critiques of simulation's limitations are often marginalized or actively suppressed in favor of the 'sufficiency' narrative. The theater ratio (0.40) reflects that while some simulation activity is genuinely functional, a growing portion is performative, aimed at regulatory compliance or demonstrating commitment to safety rather than optimizing actual competence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the simulation industry and organizational leadership, this constraint is a necessary and efficient coordination mechanism for safety. From the perspective of frontline operators and critical safety researchers, it functions as an extractive mechanism that may compromise actual competence for the benefit of industry and administrative convenience. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation industry and training providers are clear beneficiaries and agenda-setters, collecting revenue and shaping the narrative. Organizational leadership also benefits from perceived compliance and risk mitigation. Frontline operators and organizations are payers, bearing the direct costs and potential risks of an over-reliance on simulation. Critical safety researchers are often excluded, as their findings may challenge the core premise of simulation sufficiency.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_limits,
    'To what extent can simulation truly replicate the full range of cognitive, emotional, and physical stressors and complexities of real-world high-consequence incidents?',
    'Longitudinal studies comparing performance outcomes of simulation-only trained personnel versus those with real-world or hybrid training, particularly in novel or high-stress scenarios.',
    'If fidelity limits are substantial and unaddressable by current technology, the ''sufficiency'' claim collapses, reclassifying the constraint as a Snare or Piton due to its extractive nature without adequate coordination. If limits are minor, the Tangled Rope classification holds, or shifts towards Rope if extraction is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_limits, empirical, 'The empirical limits of simulation''s ability to fully occupy the competence kernel.').

omega_variable(
    competence_definition_ambiguity,
    'Is the ''competence kernel'' being defined in a way that is amenable to simulation, potentially narrowing the definition of competence itself?',
    'Conceptual analysis of competence definitions used by simulation advocates versus those used by critical safety researchers, followed by expert consensus workshops to identify definitional gaps.',
    'If the definition is being narrowed to fit simulation capabilities, the constraint''s coordination function is compromised, and its extractive elements are amplified, pushing it towards Snare. If the definition remains robust, the Tangled Rope classification is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_definition_ambiguity, conceptual, 'Whether the definition of competence is being shaped by the available simulation technology.').

omega_variable(
    mandatrophy_risk_simulation_sufficiency,
    'Is the continued emphasis on simulation sufficiency driven by the genuine, evolving needs of competence maintenance, or by the institutional inertia and economic interests of the simulation industry?',
    'Independent audits of training program effectiveness and cost-benefit analyses that explicitly account for alternative training methods and the long-term impact on operational safety, conducted by bodies with no financial ties to the simulation industry.',
    'If economic interests are the primary driver, the constraint is at high risk of mandatrophy, potentially reclassifying as a Piton if its functional benefits atrophy further, or a Snare if extraction remains high despite functional decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_risk_simulation_sufficiency, empirical, 'Assessing if the mandate for simulation sufficiency has outlived its original function.').


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
narrative_ontology:measurement(comp_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(comp_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(comp_be_t15, competence_occupation__simulation_sufficiency, base_extractiveness, 15, 0.64).
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

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, safety_certification_standards).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, regulatory_compliance_frameworks).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, organizational_risk_management).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
