% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence reading' of state execution
 *   authority, where capital punishment is justified by its purported ability
 *   to prevent future murders by raising the cost of capital crimes. This
 *   reading positions potential victims as beneficiaries and executed
 *   offenders as instrumental costs. The constraint is classified as a
 *   Tangled Rope due to its genuine coordination function (deterrence)
 *   coupled with asymmetric extraction (the life of the offender) and active
 *   enforcement. The metrics reflect the ongoing debate about its efficacy
 *   and the high cost of its administration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.65).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.75).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '209c5e0a-607a-4cf2-9549-466bd0c2757d').
narrative_ontology:cs_kernel_codification('209c5e0a-607a-4cf2-9549-466bd0c2757d', formalized).
narrative_ontology:cs_authority_grounding('209c5e0a-607a-4cf2-9549-466bd0c2757d', lineage).
narrative_ontology:cs_interpretation_layer_present('209c5e0a-607a-4cf2-9549-466bd0c2757d').
narrative_ontology:cs_reading_relation('209c5e0a-607a-4cf2-9549-466bd0c2757d', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('209c5e0a-607a-4cf2-9549-466bd0c2757d', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('209c5e0a-607a-4cf2-9549-466bd0c2757d', foundational, capital_punishment_deters_future_crime).
narrative_ontology:cs_axiom_status(capital_punishment_deters_future_crime, holdable).
narrative_ontology:cs_axiom_grounding('209c5e0a-607a-4cf2-9549-466bd0c2757d', capital_punishment_deters_future_crime, empirically_contingent).
narrative_ontology:cs_axiom('209c5e0a-607a-4cf2-9549-466bd0c2757d', secondary, state_has_right_to_protect_citizens_via_deterrence).
narrative_ontology:cs_axiom_status(state_has_right_to_protect_citizens_via_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('209c5e0a-607a-4cf2-9549-466bd0c2757d', state_has_right_to_protect_citizens_via_deterrence, deontological).
narrative_ontology:cs_reference_frame('209c5e0a-607a-4cf2-9549-466bd0c2757d', utilitarian_crime_prevention_framework).
narrative_ontology:cs_drift_state('209c5e0a-607a-4cf2-9549-466bd0c2757d', contemporary_criminological_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('209c5e0a-607a-4cf2-9549-466bd0c2757d', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, potential_victims_of_murder).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, the_state).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, families_of_executed_offenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, families_of_murder_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the death penalty, claiming it as a necessary tool for public safety and crime prevention. Bears the cost of legal appeals and execution procedures, but benefits from perceived public order and political support for being 'tough on crime'.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, the_state, agenda_setter,
    institutional, generational, constrained, national).

% Are theorized to benefit from the deterrent effect of capital punishment, as it supposedly reduces the risk of them becoming victims of capital crimes. Their benefit is indirect and statistical.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, potential_victims_of_murder, beneficiary,
    powerless, biographical, trapped, local).

% Bear the ultimate cost of the constraint, losing their lives. Their agency is entirely suppressed by the state's authority.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Bear the social, emotional, and economic costs of their family member's execution. Their identity is often tied to the offender, making 'exit' from the social stigma and grief impossible.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, families_of_executed_offenders, payer,
    powerless, generational, identity_locked, local).

% May perceive a benefit from the execution of offenders, believing it brings closure or prevents future harm to others. However, studies on closure are mixed, and the benefit is often emotional rather than direct deterrence.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, families_of_murder_victims, beneficiary,
    moderate, biographical, constrained, local).

% Actively campaign against capital punishment, arguing it is immoral, ineffective as a deterrent, and prone to error. They are excluded from the direct decision-making process but influence public opinion and legal challenges.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the efficacy of capital punishment as a deterrent, its economic costs, and its ethical implications. Their findings often challenge the deterrence reading but do not directly alter policy without political will.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, legal_scholars_and_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal behavior by establishing a severe consequence for capital crimes, thereby deterring potential offenders and ensuring public safety.
% TRANSFER_FUNCTION: Transfers the life of the executed offender, and the associated social and emotional costs, from the offender and their families to the state, in exchange for the theorized benefit of reduced future murders for potential victims.
% ABSENT_VOICES: The executed offenders themselves, whose voices are permanently silenced. Also, a significant portion of the global human rights community and many legal scholars who argue against the practice on moral and empirical grounds.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished overnight, the criminal justice system would need to fundamentally reorganize its approach to capital crimes, likely shifting to life imprisonment without parole. The perceived deterrent effect would be lost, and public discourse on punishment would shift dramatically.
% FOUNDING_PROBLEM: The problem of preventing heinous crimes and ensuring public safety, particularly murder, by imposing the most severe possible penalty.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the state and some segments of the public attest that the problem of deterring murder is still live and requires capital punishment. However, a broad consensus among criminologists and legal scholars, corroborated by decades of empirical studies, indicates that capital punishment does not have a unique deterrent effect beyond life imprisonment, suggesting the founding problem is either dead or solvable by less extractive means.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the ultimate cost (life) is borne by the offender, and the benefit (deterrence) is unproven or minimal. Suppression is also high (0.75) as the state actively enforces the penalty, suppressing the offender's agency and any alternatives to execution. Theater ratio is moderate (0.4) because while the state performs the ritual of justice, the primary stated function (deterrence) is widely contested by empirical evidence, suggesting a performative aspect to its continued use. Accessibility collapse is moderate (0.4) because alternatives like life imprisonment exist, but the state actively suppresses their perceived equivalence as a deterrent. Resistance is high (0.7) due to ongoing legal challenges and abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective (and some public segments), this is a necessary, albeit harsh, coordination mechanism for public safety. From the perspective of the executed, their families, and abolitionists, it is a highly extractive and suppressive act, with a coordination story that serves as cover for state power or retribution. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The State acts as the agenda-setter, benefiting from perceived public order and political support. Potential victims are indirect beneficiaries, though their benefit is theoretical. Executed offenders and their families are the primary payers, bearing the ultimate and profound costs. Families of murder victims may experience a psychological benefit, but this is distinct from a direct deterrent effect. Abolitionist advocates are excluded, actively resisting the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope, rather than a pure Snare, acknowledges the genuine, if contested, coordination function of deterrence. However, the high extractiveness and suppression, coupled with the contested status of the 'founding problem' (deterrence efficacy), suggest a strong potential for mandatrophy, where the original mandate has atrophied but the constraint persists due to inertia, political will, or other functions (e.g., retribution, which is a different reading of the same kernel).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical_uncertainty,
    'Does capital punishment actually deter murder more effectively than life imprisonment without parole?',
    'Longitudinal, controlled empirical studies comparing murder rates in jurisdictions with and without capital punishment, controlling for socioeconomic factors and other criminal justice policies.',
    'If capital punishment is shown to have no unique deterrent effect, the primary coordination function of this reading collapses, reclassifying it closer to a Snare or Piton. If a unique deterrent effect is robustly demonstrated, its coordination function would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical_uncertainty, empirical, 'Empirical evidence on the unique deterrent effect of capital punishment.').

omega_variable(
    wrongful_execution_error_rate,
    'What is the irreducible error rate of wrongful convictions in capital cases, and how does it impact the utilitarian calculus of deterrence?',
    'Systematic review of exonerations in capital cases, statistical modeling of undetected wrongful convictions, and ethical analysis of the ''cost'' of executing an innocent person within a deterrence framework.',
    'A high and irreducible error rate would significantly undermine the utilitarian justification of this reading, as the ''cost'' of wrongful execution would outweigh the theorized deterrent ''benefit'', pushing the classification towards Snare due to unmitigated victimhood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_error_rate, empirical, 'The impact of wrongful executions on the deterrence justification.').

omega_variable(
    reading_distinction_deterrence_vs_retribution,
    'Is the state''s justification for execution primarily deterrence or retribution, and how does this distinction affect the constraint''s structural properties?',
    'Analysis of legislative intent, judicial opinions, and public discourse: if arguments consistently emphasize ''just deserts'' over crime prevention, the dominant reading shifts to retribution, altering the beneficiary/victim structure and the claimed coordination function.',
    'If the primary justification is found to be retribution, the constraint would be reclassified under the ''retributive_reading'' kernel, which has a different set of beneficiaries (moral order, victims'' families seeking justice) and a different coordination function (restoring moral balance), potentially altering extractiveness and suppression metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinction_deterrence_vs_retribution, conceptual, 'Distinguishing deterrence from retribution as the primary justification for capital punishment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__deterrence_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__deterrence_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__deterrence_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__deterrence_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__deterrence_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__deterrence_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__deterrence_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__deterrence_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__deterrence_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__deterrence_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__deterrence_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__deterrence_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_execution_authority' kernel. This 'deterrence_reading' focuses on crime prevention. The 'retributive_reading' focuses on proportionate punishment, and the 'abolition_reading' on categorical impermissibility. All three are distinct constraints linked by their common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
