% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selector for Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This constraint represents a reading of organizational learning theory
 *   that posits a harsh truth: only the extreme selection pressure of actual
 *   catastrophes (chaos, mortality salience, organizational trauma) can truly
 *   maintain competence in complex, high-stakes systems. Long periods of
 *   peace inevitably lead to competence decay, and simulations create a false
 *   sense of security. This reading implies that industries become vulnerable
 *   to black swan re-emergence due to this underlying dynamic. It is claimed
 *   as a 'mountain' due to its assertion of a fundamental, unchangeable truth
 *   about organizational dynamics, but its high extractiveness and
 *   identifiable victims position it as a false summit candidate.
 *
 * KEY AGENTS:
 *   - safety_engineers_proponents_of_this_reading: Observer/Agenda Setter (analytical/analytical)
 *   - high_reliability_organizations: Payer (institutional/constrained)
 *   - regulators: Observer (institutional/analytical)
 *   - society: Payer (powerless/trapped)
 *   - simulation_training_industry: Excluded (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.82).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.75).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.82).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, mountain).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selector for Competence").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:emerges_naturally(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '48277c35-5602-4b98-b6b2-44ca87c1113a').
narrative_ontology:cs_kernel_codification('48277c35-5602-4b98-b6b2-44ca87c1113a', implicit).
narrative_ontology:cs_authority_grounding('48277c35-5602-4b98-b6b2-44ca87c1113a', practice).
narrative_ontology:cs_reading_relation('48277c35-5602-4b98-b6b2-44ca87c1113a', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('48277c35-5602-4b98-b6b2-44ca87c1113a', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('48277c35-5602-4b98-b6b2-44ca87c1113a', foundational, catastrophe_as_ultimate_selection_pressure).
narrative_ontology:cs_axiom_status(catastrophe_as_ultimate_selection_pressure, holdable).
narrative_ontology:cs_axiom_grounding('48277c35-5602-4b98-b6b2-44ca87c1113a', catastrophe_as_ultimate_selection_pressure, empirically_contingent).
narrative_ontology:cs_axiom('48277c35-5602-4b98-b6b2-44ca87c1113a', foundational, peacetime_leads_to_competence_atrophy).
narrative_ontology:cs_axiom_status(peacetime_leads_to_competence_atrophy, holdable).
narrative_ontology:cs_axiom_grounding('48277c35-5602-4b98-b6b2-44ca87c1113a', peacetime_leads_to_competence_atrophy, empirically_contingent).
narrative_ontology:cs_reference_frame('48277c35-5602-4b98-b6b2-44ca87c1113a', catastrophic_selection_dynamic).
narrative_ontology:cs_drift_state('48277c35-5602-4b98-b6b2-44ca87c1113a', long_peacetime_periods, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48277c35-5602-4b98-b6b2-44ca87c1113a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_organizations).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the researchers and practitioners who articulate and defend the view that only real catastrophes provide the necessary selection pressure for competence. They observe the system and advocate for a realistic understanding of organizational learning, even if it's a harsh truth.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_engineers_proponents_of_this_reading, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_engineers_proponents_of_this_reading, agenda_setter).

% Organizations operating in high-stakes environments (e.g., nuclear power, aviation, complex healthcare) that strive for zero accidents. They are the primary targets of this constraint's 'extraction' in the form of competence decay during long periods of peace, making them vulnerable to future catastrophic events.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_organizations, payer,
    institutional, biographical, constrained, national).

% Governmental bodies responsible for setting and enforcing safety standards. They observe the dynamics of organizational competence but may struggle to implement policies that account for the 'catastrophe as selector' dynamic, often favoring simulation-based compliance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulators, observer,
    institutional, generational, analytical, national).

% The ultimate bearer of the costs of organizational competence decay, suffering the consequences of catastrophic failures in critical systems. Society is largely unaware of this underlying dynamic until a major event occurs.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, society, payer,
    powerless, generational, trapped, global).

% Companies and institutions that provide high-fidelity simulation and training solutions. This reading fundamentally challenges the efficacy of their offerings as a substitute for real-world catastrophic learning, effectively excluding their claims from the core mechanism of competence retention.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_training_industry, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint describes a structural reality of organizational learning, not a coordination function. It posits a natural dynamic rather than a mechanism for collective action.
% TRANSFER_FUNCTION: Describes the transfer of organizational competence (decaying during periods of stability, renewed by the chaos and trauma of catastrophe) and the accumulation of systemic risk (which is released through catastrophic events).
% ABSENT_VOICES: Proponents of high-fidelity simulation as a proxy for real-world experience, and those who believe in continuous improvement through near-miss learning, would object. This reading dismisses their methods as insufficient to maintain competence over the long term.
% DISAPPEARANCE_RATIONALE: If the structural truth that 'only actual catastrophes provide the necessary selection pressure' vanished, organizations might falsely believe that competence can be maintained indefinitely through simulations and peacetime drills. This could lead to a dangerous complacency, different investment patterns in safety, and potentially more frequent or severe catastrophic events as the true drivers of competence decay are ignored.
% FOUNDING_PROBLEM: How to maintain high levels of organizational competence and safety over extended periods, especially in complex, high-stakes systems where direct experience of failure is rare but critical for learning.
% FOUNDING_PROBLEM_CORROBORATION: Historical analysis of major organizational failures (e.g., Challenger disaster, Chernobyl, financial crises) and some schools of thought in safety science and organizational theory attest to the problem's persistence. Independent post-mortem reports often highlight a decay in vigilance or competence during long periods of success, corroborating the underlying dynamic from outside the benefiting parties (if any).
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_avoidance_retention__catastrophe_as_necessary_selector),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.82) reflects the cost of competence decay and the accumulating risk borne by organizations and society during 'peacetime' when the necessary selection pressure is absent. Suppression (0.75) is high because this structural reality suppresses the efficacy of alternative learning methods (like simulation) by deeming them insufficient. The theater ratio (0.60) is significant because organizations often engage in performative safety measures (e.g., extensive simulation training) that, according to this reading, do not genuinely address the underlying competence decay. Accessibility collapse (0.88) is very high, as this reading suggests that no true alternative exists for the 'selection pressure' provided by catastrophe. Resistance (0.50) is moderate, as organizations naturally resist such a fatalistic view, even if some safety researchers acknowledge its truth. The metrics reflect the harsh reality of this 'mountain' claim, which, despite its naturalistic framing, imposes significant costs on identifiable victims.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading (safety_engineers) view it as a necessary, albeit grim, truth for long-term safety, advocating for a clear-eyed understanding of organizational dynamics. High-reliability organizations, however, perceive it as a fatalistic and demotivating perspective that undermines proactive safety efforts and the value of their investments in prevention. The engine's classification will highlight this divergence between the claimed 'mountain' and its extractive operation.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations and society are the primary victims/payers, as they bear the costs of competence decay and the ultimate consequences of catastrophic failure. Safety engineers who champion this reading act as observers and agenda-setters, articulating this structural truth. Regulators are also observers, grappling with the implications for policy. The simulation training industry is excluded, as their core offering is deemed insufficient by this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a human-mandated arrangement, but a claim about a fundamental dynamic. Mandatrophy analysis would focus on how mislabeling this as a 'snare' (a human-imposed extractive mechanism) would obscure the underlying structural reality it describes. Conversely, if it were truly a 'mountain' with no victims, the high extractiveness and victim declarations would signal a false summit, indicating that a natural law is being invoked to justify a costly, perhaps avoidable, dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_belief,
    'Is the ''catastrophe as necessary selector'' a genuine natural law of organizational learning, or a constructed belief that, while perhaps empirically observed, is not an irreducible limit and can be overcome by alternative learning paradigms?',
    'Longitudinal studies of organizations that successfully maintain competence over extended periods without major catastrophes, using alternative learning methods (e.g., advanced simulation, near-miss analysis, distributed learning), demonstrating sustained high performance.',
    'If it is a genuine natural law, the classification as a Mountain (albeit a false summit due to victims) is appropriate. If it is a constructed belief, its high extractiveness and suppression would reclassify it towards a Snare or Tangled Rope, implying that the ''necessity'' is a product of current understanding or institutional inertia rather than an irreducible limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_belief, conceptual, 'Ambiguity between a fundamental organizational dynamic and a potentially surmountable challenge.').

omega_variable(
    sufficiency_of_proxy_learning,
    'Can high-fidelity simulations, near-miss analyses, and distributed learning from foreign incidents truly provide the ''selection pressure'' necessary to maintain competence, or are they fundamentally insufficient as this reading claims?',
    'Empirical evidence from organizations that have rigorously implemented and measured the long-term effectiveness of these proxy learning methods in preventing competence decay and catastrophic failure, compared to organizations that have experienced catastrophic selection.',
    'If proxy learning is proven sufficient, the suppression and extractiveness metrics of this constraint would decrease significantly, potentially reclassifying it away from a highly extractive Mountain towards a Piton (if the belief persists by inertia) or even a Rope (if the belief is genuinely overcome).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_of_proxy_learning, empirical, 'Whether alternative learning methods can substitute for catastrophic selection.').

omega_variable(
    definition_of_competence_decay,
    'How is ''competence decay'' precisely defined and measured in the absence of catastrophic events, and is the observed decay truly irreversible without such events?',
    'Development of robust, validated, and independently verifiable metrics for latent competence decay in complex systems, and empirical studies demonstrating the reversibility or irreversibility of such decay through non-catastrophic interventions.',
    'If competence decay is found to be less severe or more reversible than this reading suggests, the extractiveness metric would decrease, challenging the ''necessity'' of catastrophe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_competence_decay, empirical, 'Clarity on the definition and reversibility of competence decay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.5).
narrative_ontology:measurement(cata_tr_t60, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 60, 0.55).
narrative_ontology:measurement(cata_tr_t80, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 80, 0.58).
narrative_ontology:measurement(cata_tr_t100, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(cata_be_t60, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 60, 0.79).
narrative_ontology:measurement(cata_be_t80, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 80, 0.81).
narrative_ontology:measurement(cata_be_t100, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(cata_su_t60, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(cata_su_t80, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(cata_su_t100, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
