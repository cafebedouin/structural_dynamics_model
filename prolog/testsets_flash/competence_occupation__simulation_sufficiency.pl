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
 *   human_readable: Simulation Sufficiency for Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This constraint represents the reading that simulation-based drills are
 *   sufficient for competence occupation and skill decay prevention in
 *   High-Reliability Organizations (HROs). It is one reading of the broader
 *   'competence_occupation' kernel. This reading emphasizes compliance with
 *   training hours and simulation fidelity as key metrics, leading to the
 *   simulation industry becoming a primary beneficiary. The constraint is
 *   claimed as a Tangled Rope because it offers a coordination function
 *   (standardized training) but also involves significant asymmetric
 *   extraction from frontline operators and public safety, whose actual
 *   competence may be compromised by over-reliance on simulations.
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
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation Sufficiency for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '8fd426ab-195c-47f9-8047-b39f03315f56').
narrative_ontology:cs_kernel_codification('8fd426ab-195c-47f9-8047-b39f03315f56', formalized).
narrative_ontology:cs_authority_grounding('8fd426ab-195c-47f9-8047-b39f03315f56', lineage).
narrative_ontology:cs_interpretation_layer_present('8fd426ab-195c-47f9-8047-b39f03315f56').
narrative_ontology:cs_reading_relation('8fd426ab-195c-47f9-8047-b39f03315f56', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('8fd426ab-195c-47f9-8047-b39f03315f56', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('8fd426ab-195c-47f9-8047-b39f03315f56', foundational, simulation_as_equivalent_experience).
narrative_ontology:cs_axiom_status(simulation_as_equivalent_experience, holdable).
narrative_ontology:cs_axiom_grounding('8fd426ab-195c-47f9-8047-b39f03315f56', simulation_as_equivalent_experience, conventional).
narrative_ontology:cs_axiom('8fd426ab-195c-47f9-8047-b39f03315f56', secondary, measurable_compliance_as_competence_proxy).
narrative_ontology:cs_axiom_status(measurable_compliance_as_competence_proxy, holdable).
narrative_ontology:cs_axiom_grounding('8fd426ab-195c-47f9-8047-b39f03315f56', measurable_compliance_as_competence_proxy, instrumental).
narrative_ontology:cs_reference_frame('8fd426ab-195c-47f9-8047-b39f03315f56', standardized_simulation_training_paradigm).
narrative_ontology:cs_drift_state('8fd426ab-195c-47f9-8047-b39f03315f56', contemporary_safety_science_critiques, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8fd426ab-195c-47f9-8047-b39f03315f56', '').
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

% Provides simulation platforms and services, benefiting directly from the widespread adoption of simulation-based training as the primary method for competence maintenance. Actively promotes the 'sufficiency' narrative.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_industry, beneficiary,
    organized, generational, mobile, global).

% Implements and enforces simulation-based training protocols, viewing them as a cost-effective and compliant way to meet regulatory requirements for competence. Benefits from reduced operational disruption and perceived compliance.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, hro_management, agenda_setter,
    institutional, biographical, constrained, national).

% Participate in simulation drills, often perceiving them as insufficient for real-world readiness but necessary for job retention and compliance. Bear the cost of potential skill decay if simulations are indeed inadequate.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Certify training programs and compliance, often relying on simulation hours and fidelity metrics as proxies for actual competence. Their mandates are fulfilled by the 'sufficiency' claim, simplifying oversight.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Bear the ultimate risk of competence decay leading to incidents. Advocate for more robust, real-world training and challenge the 'sufficiency' of simulations, but often lack direct influence over training mandates.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, public_safety_advocates, payer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes competence maintenance across a large workforce, ensuring a baseline level of training and compliance through a scalable, repeatable method.
% TRANSFER_FUNCTION: Transfers training budget from operational departments to the simulation industry, and transfers the burden of competence maintenance from diverse operational experience to structured simulation hours.
% ABSENT_VOICES: Experienced operators who have witnessed the limitations of simulations in high-stress, novel situations are often marginalized in policy discussions, their anecdotal evidence dismissed in favor of quantitative simulation metrics.
% DISAPPEARANCE_RATIONALE: If the belief in simulation sufficiency vanished, HROs would face a crisis in competence maintenance, scrambling for alternative, more costly, and disruptive training methods. Regulatory frameworks would need complete overhaul, and the simulation industry would lose its primary market.
% FOUNDING_PROBLEM: Ensuring consistent, measurable competence across a large, geographically dispersed workforce in high-risk environments, while minimizing the cost and risk of real-world training.
% FOUNDING_PROBLEM_CORROBORATION: HRO management and regulatory bodies attest that the problem of scalable competence maintenance is still live. Public safety advocates and some frontline operators contest the 'sufficiency' aspect, arguing the problem is being 'solved' inadequately, leading to latent risks.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) stems from the cost of simulations and the potential for skill decay if they are not truly sufficient, leading to higher risk for operators and the public. Suppression (0.70) is high due to regulatory mandates and the difficulty for individual operators to challenge established training paradigms. The theater ratio (0.40) reflects that while simulations have a genuine training function, a significant portion of their maintenance is performative, aimed at satisfying compliance metrics rather than optimizing real-world readiness. The metrics show a trend of increasing extractiveness and suppression over time as the 'sufficiency' claim becomes more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   HRO management and the simulation industry perceive this as a legitimate, efficient coordination mechanism (a Rope), while frontline operators and public safety advocates experience it as an extractive system that may compromise actual competence (a Snare). The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation industry and HRO management are beneficiaries, gaining revenue and simplified compliance, respectively. Frontline operators and public safety advocates are payers, bearing the costs of inadequate training and latent risk. Regulatory bodies act as agenda-setters, enforcing the 'sufficiency' standard. The 'identity_locked' exit for frontline operators reflects their professional dependence on compliance with mandated training, even if they privately doubt its efficacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_vs_real_world_transfer,
    'To what extent does high simulation fidelity translate into actual skill transfer and performance in novel, high-stress real-world scenarios?',
    'Longitudinal studies comparing simulation performance to real-world incident outcomes, controlling for other training variables.',
    'If transfer is low, the extractiveness and theater ratio of this constraint are significantly underestimated, pushing it closer to a Snare. If high, the Rope aspects are stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_vs_real_world_transfer, empirical, 'Empirical gap between simulation performance and real-world competence.').

omega_variable(
    cost_benefit_of_alternative_training,
    'What is the true cost-benefit ratio of alternative, more real-world-intensive training methods compared to simulation-based approaches, accounting for risk and operational disruption?',
    'Comprehensive economic and risk analysis of alternative training regimes, including pilot programs for hybrid models.',
    'If alternatives are found to be cost-effective and safer, the suppression metric of this constraint is artificially inflated, and its justification as a coordination mechanism weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_of_alternative_training, empirical, 'Economic and safety trade-offs of different training approaches.').

omega_variable(
    reading_contest_competence_occupation,
    'Is ''simulation_sufficiency'' a genuine solution to competence maintenance, or a convenient institutional framing that benefits the simulation industry and HRO management?',
    'Resolution of the ''simulation_fidelity_vs_real_world_transfer'' omega, combined with a shift in regulatory mandates to prioritize demonstrated real-world competence over simulation hours.',
    'If resolved against ''sufficiency'', this reading would be reclassified from Tangled Rope to Snare, and the ''hybrid_occupation'' reading would gain legitimacy. If resolved for ''sufficiency'', it would move closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_competence_occupation, conceptual, 'This constraint is one reading of the ''competence_occupation'' kernel. Sibling readings (''real_incident_necessity'', ''hybrid_occupation'') offer alternative framings of what constitutes sufficient competence occupation. The contest is over the structural necessity and efficacy of simulation as the primary mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1990, competence_occupation__simulation_sufficiency, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(comp_tr_t1998, competence_occupation__simulation_sufficiency, theater_ratio, 1998, 0.28).
narrative_ontology:measurement(comp_tr_t2006, competence_occupation__simulation_sufficiency, theater_ratio, 2006, 0.33).
narrative_ontology:measurement(comp_tr_t2014, competence_occupation__simulation_sufficiency, theater_ratio, 2014, 0.37).
narrative_ontology:measurement(comp_tr_t2024, competence_occupation__simulation_sufficiency, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t1990, competence_occupation__simulation_sufficiency, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(comp_be_t1998, competence_occupation__simulation_sufficiency, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(comp_be_t2006, competence_occupation__simulation_sufficiency, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(comp_be_t2014, competence_occupation__simulation_sufficiency, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(comp_be_t2024, competence_occupation__simulation_sufficiency, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1990, competence_occupation__simulation_sufficiency, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(comp_su_t1998, competence_occupation__simulation_sufficiency, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(comp_su_t2006, competence_occupation__simulation_sufficiency, suppression_requirement, 2006, 0.62).
narrative_ontology:measurement(comp_su_t2014, competence_occupation__simulation_sufficiency, suppression_requirement, 2014, 0.67).
narrative_ontology:measurement(comp_su_t2024, competence_occupation__simulation_sufficiency, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, hro_regulatory_compliance).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, hro_incident_reporting).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
