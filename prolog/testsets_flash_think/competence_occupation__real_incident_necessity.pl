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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Competence Occupation: Real Incident Necessity
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   This constraint represents a reading of the 'competence_occupation'
 *   kernel that posits only actual catastrophic incidents provide the
 *   authentic conditions necessary for true competence. It is a claim about
 *   an irreducible necessity in high-reliability domains. From this reading's
 *   perspective, it is a Mountain, an unchangeable feature of reality.
 *   However, its operation is highly extractive, as it implies that the
 *   immense costs and human suffering of incidents are an unavoidable 'price'
 *   for competence. The high extractiveness and suppression reflect the
 *   severe consequences and the perceived lack of alternatives, while the low
 *   theater ratio indicates minimal performative maintenance, as the 'real'
 *   conditions are external and catastrophic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.9).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.85).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.9).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Competence Occupation: Real Incident Necessity").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '447f5569-feda-431a-b6d7-c13e1211afaa').
narrative_ontology:cs_kernel_codification('447f5569-feda-431a-b6d7-c13e1211afaa', implicit).
narrative_ontology:cs_authority_grounding('447f5569-feda-431a-b6d7-c13e1211afaa', self_enforcing).
narrative_ontology:cs_reading_relation('447f5569-feda-431a-b6d7-c13e1211afaa', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('447f5569-feda-431a-b6d7-c13e1211afaa', competence_occupation__hybrid_occupation, forecloses).
narrative_ontology:cs_axiom('447f5569-feda-431a-b6d7-c13e1211afaa', foundational, catastrophe_as_sole_competence_occupier).
narrative_ontology:cs_axiom_status(catastrophe_as_sole_competence_occupier, holdable).
narrative_ontology:cs_axiom_grounding('447f5569-feda-431a-b6d7-c13e1211afaa', catastrophe_as_sole_competence_occupier, empirically_contingent).
narrative_ontology:cs_reference_frame('447f5569-feda-431a-b6d7-c13e1211afaa', unavoidable_catastrophe_learning).
narrative_ontology:cs_drift_state('447f5569-feda-431a-b6d7-c13e1211afaa', contemporary_safety_culture, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('447f5569-feda-431a-b6d7-c13e1211afaa', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, affected_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These organizations operate in environments where failure is catastrophic. They bear the direct and indirect costs of actual incidents, which this constraint posits as necessary for true competence. Their identity is often tied to managing these extreme conditions, making exit from this 'necessity' unthinkable.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, high_reliability_organizations, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, high_reliability_organizations, agenda_setter).

% These are the individuals and communities directly impacted by catastrophic incidents. They bear the ultimate cost of the 'learning' process, with no agency in the competence occupation framework.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, affected_populations, payer,
    powerless, immediate, trapped, local).

% Tasked with preventing incidents, they operate under a mandate that conflicts with the idea that incidents are necessary for competence. They observe the outcomes and try to impose controls, but this constraint implies their preventative measures are not the source of 'authentic' competence.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_regulators, observer,
    institutional, biographical, constrained, national).

% Providers of simulation-based training and drills. Their methods are deemed insufficient by this reading, effectively excluding them from the 'authentic' competence occupation process, despite their efforts to create realistic training environments.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, training_simulators, excluded,
    moderate, biographical, constrained, global).

% Academics and researchers who study high-reliability systems and competence. They analyze the structural implications of this constraint, observing its effects on organizations and populations, and comparing it to alternative theories of competence development.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This constraint describes an irreducible condition for competence, not a mechanism for coordinating action. Its 'function' is to define the limits of competence acquisition.
% TRANSFER_FUNCTION: Transfers the cost of competence maintenance from proactive investment in alternatives (simulations, training) to the reactive, catastrophic costs of actual incidents, borne by organizations and affected populations.
% ABSENT_VOICES: Those who advocate for proactive, non-catastrophic competence building, such as simulation experts, safety engineers, and victims' advocates. They would argue that competence can and must be achieved without real incidents.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, it would imply that other methods (simulations, drills, procedural reinforcement) *are* sufficient for occupying the competence kernel. This would fundamentally alter safety protocols, training regimes, and risk management strategies across all high-reliability organizations, shifting focus entirely to prevention and simulated learning.
% FOUNDING_PROBLEM: The inherent unpredictability and extreme complexity of high-stakes operational environments, where only the stress and reality of actual catastrophic failure are believed to reveal and forge true, resilient competence.
% FOUNDING_PROBLEM_CORROBORATION: While proponents within high-reliability organizations might attest to this, independent corroboration is scarce. Safety scientists and regulators actively dispute the necessity of real incidents for competence, citing ethical and practical reasons. Corroboration primarily comes from anecdotal accounts of 'lessons learned' from past disasters, rather than external, objective validation.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The claimed type is 'mountain' because this reading asserts a fundamental, unchangeable necessity for competence. The high extractiveness (0.90) reflects the catastrophic costs borne by victims and organizations, which are implicitly 'extracted' as the price of competence. Suppression (0.85) is high because this reading dismisses or devalues alternative, proactive methods of competence building, effectively suppressing their perceived efficacy. Theater ratio is low (0.05) because, if only real incidents count, there is little 'performance' in competence maintenance; the 'real' conditions are external and unavoidable. Accessibility collapse is very high (0.95) as all other paths to 'authentic' competence are deemed insufficient. Resistance is low (0.10) because, if it's a natural necessity, resistance to the idea itself is seen as futile, though resistance to the incidents themselves is high.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a natural law, an unavoidable truth about competence. However, from the perspective of affected populations and safety regulators, it is a devastatingly extractive and suppressive force, leading to immense suffering. The engine's classification will highlight this divergence between the claimed 'mountain' and the highly extractive, victim-producing reality.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations are payers (bear incident costs) but also implicitly agenda-setters (their practices and beliefs perpetuate this constraint). Affected populations are pure victims. Safety regulators are observers, attempting to mitigate the consequences but not directly part of the competence occupation. Training simulators are excluded, as their methods are deemed insufficient. The 'necessity' itself is the structural force, extracting from all who operate within its shadow.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_belief,
    'Is the necessity of real incidents for competence a genuine natural law, or a constructed belief system that rationalizes the failure to prevent incidents and underinvest in alternatives?',
    'Empirical studies comparing competence outcomes in organizations that rigorously implement advanced simulation training versus those that rely on ''lessons learned'' from real incidents, controlling for operational complexity. If simulation-trained organizations achieve comparable or superior safety records, the ''necessity'' is likely a constructed belief.',
    'If a constructed belief, the constraint''s classification would shift from Mountain to a highly extractive Snare or Tangled Rope, as its persistence would depend on suppressing alternatives and extracting costs from victims under a false premise of naturalness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_belief, empirical, 'Ambiguity between a natural law and a rationalizing belief system regarding competence acquisition.').

omega_variable(
    competence_definition_ambiguity,
    'What constitutes ''authentic'' competence? Is it the ability to survive and learn from catastrophe, or the ability to prevent catastrophe through proactive measures and robust systems?',
    'Conceptual analysis and consensus-building among safety experts, philosophers of technology, and organizational theorists to define ''competence'' in high-reliability contexts. This would involve examining the ethical implications of each definition.',
    'If competence is defined as prevention, this reading''s foundational axiom would be overridden, leading to a reclassification away from Mountain. If competence is defined as resilience through crisis, the Mountain claim would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''authentic'' competence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.05).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.05).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__real_incident_necessity, theater_ratio, 30, 0.05).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.05).
narrative_ontology:measurement(comp_tr_t50, competence_occupation__real_incident_necessity, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.9).
narrative_ontology:measurement(comp_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.9).
narrative_ontology:measurement(comp_be_t30, competence_occupation__real_incident_necessity, base_extractiveness, 30, 0.9).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.9).
narrative_ontology:measurement(comp_be_t50, competence_occupation__real_incident_necessity, base_extractiveness, 50, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(comp_su_t20, competence_occupation__real_incident_necessity, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(comp_su_t30, competence_occupation__real_incident_necessity, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(comp_su_t50, competence_occupation__real_incident_necessity, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
