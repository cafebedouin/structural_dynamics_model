% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Competence Occupation: Real Incident Necessity
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint posits that only actual catastrophic incidents provide
 *   the authentic conditions necessary to truly occupy the competence kernel
 *   in high-reliability domains. It is a 'Mountain' in the sense that it
 *   describes an irreducible, tragic truth about the nature of competence in
 *   extreme environments, where the full spectrum of stress, uncertainty, and
 *   consequence cannot be replicated by any artificial means. While no one
 *   'benefits' from catastrophes, analytical observers might gain 'pure'
 *   data. High-reliability organizations and frontline operators are the
 *   victims, trapped by a standard they cannot meet without unacceptable
 *   loss.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.95).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.98).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.95).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Competence Occupation: Real Incident Necessity").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'ece6f40b-66b2-4871-bbf6-b521d8b884e9').
narrative_ontology:cs_kernel_codification('ece6f40b-66b2-4871-bbf6-b521d8b884e9', implicit).
narrative_ontology:cs_authority_grounding('ece6f40b-66b2-4871-bbf6-b521d8b884e9', diffuse_epistemic).
narrative_ontology:cs_reading_relation('ece6f40b-66b2-4871-bbf6-b521d8b884e9', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('ece6f40b-66b2-4871-bbf6-b521d8b884e9', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('ece6f40b-66b2-4871-bbf6-b521d8b884e9', foundational, authenticity_requires_uncontrolled_consequence).
narrative_ontology:cs_axiom_status(authenticity_requires_uncontrolled_consequence, holdable).
narrative_ontology:cs_axiom_grounding('ece6f40b-66b2-4871-bbf6-b521d8b884e9', authenticity_requires_uncontrolled_consequence, empirically_contingent).
narrative_ontology:cs_axiom('ece6f40b-66b2-4871-bbf6-b521d8b884e9', secondary, simulated_stress_is_insufficient).
narrative_ontology:cs_axiom_status(simulated_stress_is_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('ece6f40b-66b2-4871-bbf6-b521d8b884e9', simulated_stress_is_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('ece6f40b-66b2-4871-bbf6-b521d8b884e9', unavoidable_tragic_truth).
narrative_ontology:cs_drift_state('ece6f40b-66b2-4871-bbf6-b521d8b884e9', contemporary_safety_science, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ece6f40b-66b2-4871-bbf6-b521d8b884e9', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, analytical_observers).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These organizations are structurally committed to preventing catastrophic failure, yet this constraint implies their competence can only be truly 'occupied' through the very incidents they strive to avoid. They bear the unresolvable tension of this paradox.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, trapped, global).

% Individuals whose competence is directly at stake. They are trained to respond to incidents, but the 'real incident necessity' implies their training is never truly complete or validated until a catastrophe occurs, placing an immense psychological burden on them.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Tasked with ensuring safety, they are caught between the imperative to prevent incidents and the implicit 'truth' of this constraint. They must set standards and conduct audits, knowing that the 'ultimate test' is one they cannot permit.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Academics and researchers who study high-reliability systems. This constraint, while tragic, provides a 'pure' observable for their theories on competence and failure, even if they advocate for its mitigation.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, analytical_observers, beneficiary,
    analytical, civilizational, analytical, universal).

% They develop sophisticated training environments, but this constraint fundamentally devalues their work by asserting that no simulation can replicate the 'authenticity' of a real catastrophe for competence occupation. They are excluded from the core definition of competence.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_designers, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It defines the ultimate, albeit tragic, standard for competence in high-stakes environments, implicitly coordinating the understanding of what 'true' readiness entails.
% TRANSFER_FUNCTION: It transfers the burden of 'unproven' competence onto high-reliability organizations and their operators, demanding an impossible standard that can only be met through unacceptable loss.
% ABSENT_VOICES: The victims of catastrophic incidents, whose suffering is implicitly framed as the 'cost' of occupying the competence kernel, are permanently absent. Simulation designers would also object, arguing for the efficacy of their methods.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the entire framework for competence assessment in high-reliability organizations would shift. The existential tension would ease, allowing for a re-evaluation of simulation and hybrid approaches as truly sufficient, rather than merely preparatory. The industry would reorganize around achievable, non-catastrophic competence metrics.
% FOUNDING_PROBLEM: The problem of ensuring genuine, tested competence in systems where failure is unacceptable, and where the complexity of real-world incidents cannot be fully replicated in training.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, corroborated by ongoing debates in safety science, military training, and disaster preparedness literature, which consistently grapple with the gap between simulated and real-world performance. Analytical observers and safety regulators attest to this persistent challenge.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is extremely high (0.95) because it demands an unacceptable price (catastrophe) for 'true' competence. Suppression is near total (0.98) because the 'truth' of this constraint is perceived as an unchangeable feature of reality, leaving no viable alternatives for 'authentic' competence occupation. Theater ratio is very low (0.05) as there's little performative maintenance; the constraint is a stark, unvarnished 'truth' rather than a managed illusion. Accessibility collapse is near total (0.99) as no other path to 'authentic' competence is recognized. Resistance is negligible (0.01) because it's seen as an immutable, tragic fact, not a policy to be resisted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of high-reliability organizations and frontline operators, this constraint is a profound, unresolvable paradox that extracts an unacceptable price. From an analytical observer's perspective, it might be seen as a 'natural law' of extreme competence, a tragic but undeniable truth about the limits of human preparation.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations and frontline operators are full targets (d=1.0) as they bear the impossible burden and potential cost of this 'truth'. Safety regulators are agenda-setters, but also targets, as they must operate within this tragic reality. Analytical observers are beneficiaries (d=0.0) as the constraint provides a 'pure' observable for their theories, even if they lament its implications. Simulation designers are excluded, as their efforts are deemed insufficient by this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it describes a perceived fundamental truth rather than a human-designed mandate. Its 'function' is to define the ultimate, tragic standard of competence. The challenge is not its obsolescence, but its inherent unacceptability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_cognitive_bias,
    'Is the ''real incident necessity'' a genuine natural law of competence, or a cognitive bias (e.g., availability heuristic, fundamental attribution error) that overvalues direct experience and undervalues synthetic training?',
    'Longitudinal studies comparing performance outcomes of teams trained exclusively via advanced simulation vs. those with real incident experience, controlling for other variables. Analysis of cognitive mechanisms underlying ''authenticity'' judgments.',
    'If a cognitive bias, the constraint''s ''naturalness'' would be reclassified as a constructed belief, potentially opening pathways for alternative, non-catastrophic competence occupation strategies. If a natural law, the tragic implications remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_cognitive_bias, empirical, 'Whether the constraint is an objective truth or a psychological artifact.').

omega_variable(
    authenticity_definition_ambiguity,
    'What constitutes ''authenticity'' in competence occupation, and is it an irreducible property of real incidents, or can it be synthetically generated through sufficiently immersive and high-fidelity simulations?',
    'Philosophical and psychological analysis of ''authenticity'' in human experience, combined with empirical studies on the transferability of skills from high-fidelity simulations to real-world performance under stress.',
    'If authenticity can be synthetically generated, the ''real incident necessity'' would be undermined, allowing for a re-evaluation of ''simulation_sufficiency'' or ''hybrid_occupation'' as viable alternatives. If irreducible, the constraint''s force remains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_definition_ambiguity, conceptual, 'The nature and source of ''authenticity'' in competence.').

omega_variable(
    kernel_reading_impact,
    'How would the classification of this constraint change if the ''simulation_sufficiency'' or ''hybrid_occupation'' readings of the competence_occupation kernel were adopted?',
    'By analyzing the structural deltas declared in the ''simulation_sufficiency'' and ''hybrid_occupation'' constraint stories and re-running the classification engine with those parameters.',
    'If ''simulation_sufficiency'' were adopted, this constraint would likely be reclassified as a ''Snare'' or ''Tangled Rope'' (a constructed belief extracting an unacceptable cost). If ''hybrid_occupation'' were adopted, it would likely be seen as a ''Piton'' (an atrophied, extreme view maintained by inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_impact, conceptual, 'Impact of alternative kernel readings on this constraint''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1950, competence_occupation__real_incident_necessity, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(comp_tr_t1970, competence_occupation__real_incident_necessity, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(comp_tr_t1990, competence_occupation__real_incident_necessity, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(comp_tr_t2010, competence_occupation__real_incident_necessity, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(comp_tr_t2024, competence_occupation__real_incident_necessity, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(comp_be_t1950, competence_occupation__real_incident_necessity, base_extractiveness, 1950, 0.9).
narrative_ontology:measurement(comp_be_t1970, competence_occupation__real_incident_necessity, base_extractiveness, 1970, 0.92).
narrative_ontology:measurement(comp_be_t1990, competence_occupation__real_incident_necessity, base_extractiveness, 1990, 0.94).
narrative_ontology:measurement(comp_be_t2010, competence_occupation__real_incident_necessity, base_extractiveness, 2010, 0.95).
narrative_ontology:measurement(comp_be_t2024, competence_occupation__real_incident_necessity, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1950, competence_occupation__real_incident_necessity, suppression_requirement, 1950, 0.95).
narrative_ontology:measurement(comp_su_t1970, competence_occupation__real_incident_necessity, suppression_requirement, 1970, 0.96).
narrative_ontology:measurement(comp_su_t1990, competence_occupation__real_incident_necessity, suppression_requirement, 1990, 0.97).
narrative_ontology:measurement(comp_su_t2010, competence_occupation__real_incident_necessity, suppression_requirement, 2010, 0.98).
narrative_ontology:measurement(comp_su_t2024, competence_occupation__real_incident_necessity, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
