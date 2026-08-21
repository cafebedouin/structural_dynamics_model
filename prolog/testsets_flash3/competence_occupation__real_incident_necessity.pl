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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Competence Occupation: Real Incident Necessity Reading
 *   domain: organizational_theory/safety_science
 *
 * SUMMARY:
 *   This constraint represents a specific, stark reading of 'competence
 *   occupation' in high-reliability organizations (HROs): that only actual
 *   catastrophic incidents provide the authentic conditions necessary for
 *   true competence. It posits a natural, irreducible limit to competence
 *   maintenance, making it a Mountain. The high extractiveness and
 *   suppression reflect the impossible burden this reading places on HROs and
 *   their personnel, who are perpetually 'victims' of a standard they cannot
 *   meet without incurring unacceptable costs. The claim is Mountain, but the
 *   implications for human systems are profoundly extractive.
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
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Competence Occupation: Real Incident Necessity Reading").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational_theory/safety_science").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '6a3270a7-18fe-493f-8310-0d82b1b871bc').
narrative_ontology:cs_kernel_codification('6a3270a7-18fe-493f-8310-0d82b1b871bc', implicit).
narrative_ontology:cs_authority_grounding('6a3270a7-18fe-493f-8310-0d82b1b871bc', diffuse_epistemic).
narrative_ontology:cs_reading_relation('6a3270a7-18fe-493f-8310-0d82b1b871bc', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('6a3270a7-18fe-493f-8310-0d82b1b871bc', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('6a3270a7-18fe-493f-8310-0d82b1b871bc', foundational, authenticity_requires_catastrophe).
narrative_ontology:cs_axiom_status(authenticity_requires_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('6a3270a7-18fe-493f-8310-0d82b1b871bc', authenticity_requires_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('6a3270a7-18fe-493f-8310-0d82b1b871bc', secondary, simulation_is_insufficient).
narrative_ontology:cs_axiom_status(simulation_is_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('6a3270a7-18fe-493f-8310-0d82b1b871bc', simulation_is_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('6a3270a7-18fe-493f-8310-0d82b1b871bc', untested_competence_paradox).
narrative_ontology:cs_drift_state('6a3270a7-18fe-493f-8310-0d82b1b871bc', contemporary_safety_science, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6a3270a7-18fe-493f-8310-0d82b1b871bc', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, safety_regulators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These organizations (e.g., nuclear power plants, air traffic control) are structurally committed to maintaining competence in extreme conditions. This reading implies they can never truly achieve or prove competence without experiencing the very catastrophes they exist to prevent, creating an unresolvable paradox.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, trapped, global).

% Tasked with ensuring safety and competence, regulators are caught in the bind of this reading: they cannot certify 'occupied competence' without real incidents, which are unacceptable. This forces them into a position of perpetual uncertainty or reliance on proxies they know are insufficient.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_regulators, payer,
    institutional, generational, constrained, national).

% The individuals who must perform competently in high-stakes situations. This reading places an impossible burden on them: their true competence can only be 'occupied' through an event that would likely result in their failure or death. Their professional identity is tied to this unattainable standard.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Academics and theorists who analyze the nature of competence and high-reliability. This reading, while grim, is a coherent (if unpalatable) theoretical position for them, highlighting the inherent limits of safety systems.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts that the 'competence kernel' is a natural, irreducible feature of reality that can only be truly occupied under specific, extreme conditions. It doesn't 'solve' a coordination problem but describes a fundamental limitation.
% TRANSFER_FUNCTION: It implicitly transfers the burden of 'proving' competence onto the occurrence of catastrophic events, extracting an impossible cost (catastrophe itself) from organizations and individuals.
% ABSENT_VOICES: Any organization or individual committed to proactive safety and prevention would object, as this reading renders their efforts fundamentally insufficient for 'true' competence occupation. They are excluded by the very definition of competence offered by this reading.
% DISAPPEARANCE_RATIONALE: If this constraint (as a natural law) disappeared, the underlying reality it describes would remain. Competence would still be tested by extreme events, but the *understanding* of its occupation might shift towards more achievable, proactive measures. The world's physics would not change, only our interpretation of competence.
% FOUNDING_PROBLEM: The problem of how to genuinely assess and maintain competence in systems designed to prevent failure, where the most critical tests are also the most destructive.
% FOUNDING_PROBLEM_CORROBORATION: This problem is attested by safety scientists, organizational theorists, and accident investigators who grapple with the limits of training and simulation. Their analyses often highlight the 'unforeseen' or 'unprecedented' nature of real incidents as the ultimate test, corroborating the persistence of this problem from outside the immediate victims.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_unchanged).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The extractiveness (0.95) is extreme because the 'cost' of occupying the competence kernel is the very catastrophe HROs exist to prevent. Suppression (0.98) is near-total because there is no 'exit' from the reality that extreme conditions test competence uniquely. Theater ratio is low (0.05) because this reading is not about performative maintenance; it's a grim assessment of an irreducible truth. Accessibility collapse is near-total (0.99) as no alternative (e.g., simulation) is considered sufficient. Resistance is minimal (0.02) because, as a claimed natural law, it's seen as an unavoidable truth, not something to be actively resisted.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in the sense of different parties experiencing different 'types' of constraint, as this is a Mountain. However, there is a profound gap between the theoretical 'truth' of this reading and the practical, ethical imperative to prevent the very incidents it deems necessary for competence. The engine's classification as Mountain reflects the structural claim, while the extreme extractiveness highlights the human cost of such a 'natural law'.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries, as no party genuinely benefits from the necessity of catastrophe. All named stakeholders are victims: HROs, regulators, and operators are all subject to this impossible standard. The analytical observer, while understanding the structural truth, is not a beneficiary. The 'emerges_naturally: true' flag is critical here, as this reading asserts a fundamental, unchangeable aspect of reality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_conceptual_trap,
    'Is this constraint a genuine natural law of competence, or a conceptual trap arising from an overly narrow definition of ''authentic conditions''?',
    'Empirical studies demonstrating that advanced simulation, combined with other training modalities, can produce competence indistinguishable from that gained in real incidents, or philosophical re-evaluation of ''authenticity'' in competence assessment.',
    'If a conceptual trap, the constraint would reclassify from Mountain to a Snare or Tangled Rope, as the ''necessity'' of real incidents would be revealed as a constructed, extractive standard. If a natural law, the classification remains Mountain, but the implications for HROs are dire.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_conceptual_trap, conceptual, 'Ambiguity between an irreducible natural limit and a definitional artifact.').

omega_variable(
    competence_definition_scope,
    'Does ''competence'' in high-reliability contexts necessarily include performance under conditions of extreme, unforeseen catastrophe, or can it be meaningfully defined as effective prevention and mitigation?',
    'Consensus shift in safety science and regulatory bodies towards a definition of competence that prioritizes proactive risk management and resilience, rather than post-catastrophe performance.',
    'A broader definition of competence would reduce the perceived extractiveness and suppression of this constraint, potentially allowing for reclassification towards a Rope or Scaffold, as achievable means of competence occupation would become available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_definition_scope, preference, 'The scope of what constitutes ''competence'' in extreme environments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(comp_tr_t50, competence_occupation__real_incident_necessity, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(comp_be_t50, competence_occupation__real_incident_necessity, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.98).
narrative_ontology:measurement(comp_su_t50, competence_occupation__real_incident_necessity, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
