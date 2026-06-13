% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: Categorical Impermissibility of State Execution (Abolitionist Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of state execution
 *   authority, asserting its categorical impermissibility. It views state
 *   execution as an inherently immoral act, regardless of the crime committed
 *   or the procedural safeguards in place. The constraint is framed as a
 *   Snare because it extracts the ultimate cost (life) from individuals and
 *   imposes a moral cost on society, with no legitimate coordination function
 *   from this perspective. The high extractiveness reflects the irreversible
 *   nature of the penalty, and high suppression reflects the state's power to
 *   enforce it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.88).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "Categorical Impermissibility of State Execution (Abolitionist Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1').
narrative_ontology:cs_kernel_codification('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', formalized).
narrative_ontology:cs_authority_grounding('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', lineage).
narrative_ontology:cs_interpretation_layer_present('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1').
narrative_ontology:cs_reading_relation('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', secondary, state_has_no_right_to_take_life).
narrative_ontology:cs_axiom_status(state_has_no_right_to_take_life, holdable).
narrative_ontology:cs_axiom_grounding('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', state_has_no_right_to_take_life, deontological).
narrative_ontology:cs_reference_frame('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', contemporary_global_abolitionist_movement, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4bcf381b-0e6b-44d3-8f9c-ed28ad077aa1', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, society_as_moral_agent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively campaign for the legal and moral prohibition of state execution, viewing it as an inherent violation of human rights. They seek to dismantle the legal and institutional structures that permit capital punishment.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_advocates, agenda_setter,
    organized, generational, constrained, global).

% Bear the ultimate cost of the state's authority to execute. From this reading, their execution is an unjustifiable taking of life, regardless of their crime or the legal process.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, local).

% Experience profound and lasting harm from the execution of their loved ones, often compounded by the perceived injustice of the act itself. They are left to cope with the aftermath of a state-sanctioned killing.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed_persons, payer,
    powerless, biographical, trapped, local).

% Is morally diminished by engaging in state execution, which this reading views as an act that brutalizes society and undermines its commitment to human dignity. The cost is a degradation of collective moral standing.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, society_as_moral_agent, payer,
    institutional, civilizational, identity_locked, national).

% Administers the legal framework that permits or prohibits capital punishment. From this reading, its continued participation in executions makes it an agent of an impermissible act, even if it follows due process.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Often seek retribution or deterrence through execution, but this reading argues that their desire, while understandable, cannot justify an inherently immoral act. Their voices are excluded from the moral calculus of categorical impermissibility.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, victims_of_capital_crimes_families, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not identify a legitimate coordination function for state execution; its purpose is to prohibit, not to coordinate.
% TRANSFER_FUNCTION: The constraint transfers the ultimate cost of life from the executed person to the state, which then bears the moral cost of having committed an impermissible act.
% ABSENT_VOICES: Proponents of retribution and deterrence are excluded from the moral framework of this reading, as their justifications for execution are deemed invalid. Their arguments, while present in public discourse, are not considered legitimate within this categorical prohibition.
% DISAPPEARANCE_RATIONALE: If the authority for state execution vanished overnight, the legal and moral landscape of criminal justice would fundamentally shift. No more executions would occur, and the state would be forced to find alternative, non-lethal means of punishment, leading to a re-evaluation of justice systems globally.
% FOUNDING_PROBLEM: The problem of state-sanctioned killing, viewed as an inherent violation of human dignity and an irreversible act that risks executing the innocent.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, legal scholars, and a growing number of national governments attest that the problem of state execution's moral impermissibility remains live, citing ongoing human rights concerns and the risk of wrongful convictions. This corroboration comes from outside the direct beneficiaries of capital punishment.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because the constraint involves the irreversible taking of life, which is the ultimate form of extraction. Suppression is high (0.88) because the state possesses a monopoly on legitimate force and can enforce execution despite moral objections. Theater ratio is low (0.1) because, from this reading, there is little performative justification; the act itself is the core issue. Accessibility collapse is high (0.9) as there is no 'alternative' to being executed once the process is complete, and resistance is high (0.75) due to ongoing global abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between this abolitionist reading and the retributive/deterrence readings. While other readings might see execution as a legitimate function of the state, this reading sees it as an illegitimate act of extraction. The engine's classification will highlight this divergence by computing a Snare for this reading, while other readings might compute a Tangled Rope or even a Rope (if their justifications were accepted as coordination).
 *
 * DIRECTIONALITY LOGIC:
 *   Executed persons and their families are clear targets (payers). Society as a moral agent is also a target, bearing the moral cost. Abolitionist advocates act as agenda-setters, seeking to impose this constraint. Proponents of retribution and deterrence are excluded, as their justifications are rejected by this reading's foundational axioms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_of_state_killing,
    'Is state execution an inherently immoral act, or can it be morally justified under certain conditions (e.g., for heinous crimes, with due process)?',
    'Philosophical consensus on the foundations of human rights and state power, or a global legal precedent establishing a universal prohibition.',
    'If it is not inherently immoral, the extractiveness and suppression metrics would need re-evaluation, potentially shifting the classification towards a Tangled Rope if a coordination function (like deterrence) is accepted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_of_state_killing, conceptual, 'The fundamental moral status of state-sanctioned killing.').

omega_variable(
    wrongful_execution_impact,
    'Does the risk of wrongful execution fundamentally undermine the legitimacy of capital punishment, even if it is otherwise deemed morally permissible?',
    'Empirical data on the frequency of wrongful convictions in capital cases, combined with a legal and ethical determination of acceptable risk.',
    'If the risk is deemed unacceptable, it strengthens the Snare classification by highlighting an unmitigable flaw in the mechanism, regardless of other justifications. If the risk is deemed negligible or acceptable, it weakens this specific argument for abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_impact, empirical, 'The impact of wrongful execution risk on legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(stat_be_t1948, state_execution_authority__abolition_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement(stat_be_t1972, state_execution_authority__abolition_reading, base_extractiveness, 1972, 0.92).
narrative_ontology:measurement(stat_be_t1990, state_execution_authority__abolition_reading, base_extractiveness, 1990, 0.93).
narrative_ontology:measurement(stat_be_t2005, state_execution_authority__abolition_reading, base_extractiveness, 2005, 0.94).
narrative_ontology:measurement(stat_be_t2024, state_execution_authority__abolition_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1948, state_execution_authority__abolition_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(stat_su_t1972, state_execution_authority__abolition_reading, suppression_requirement, 1972, 0.82).
narrative_ontology:measurement(stat_su_t1990, state_execution_authority__abolition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(stat_su_t2005, state_execution_authority__abolition_reading, suppression_requirement, 2005, 0.87).
narrative_ontology:measurement(stat_su_t2024, state_execution_authority__abolition_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
