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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story represents the 'abolition reading' of state
 *   execution authority, which holds that state execution is categorically
 *   impermissible regardless of crime severity or procedural safeguards. From
 *   this perspective, the constraint operates as a snare, extracting the
 *   ultimate cost (life) from individuals and society without legitimate
 *   justification. The high extractiveness (0.95) reflects the absolute
 *   nature of the harm, and high suppression (0.9) reflects the state's
 *   monopoly on legitimate force to carry out executions. Resistance is high
 *   (0.8) due to ongoing abolitionist movements. The claimed type is 'snare'
 *   because the coordination story (retribution, deterrence) is rejected as
 *   cover for pure extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.9).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority (Abolition Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '538ec321-ad35-44f0-90cc-2f0772799865').
narrative_ontology:cs_kernel_codification('538ec321-ad35-44f0-90cc-2f0772799865', formalized).
narrative_ontology:cs_authority_grounding('538ec321-ad35-44f0-90cc-2f0772799865', lineage).
narrative_ontology:cs_interpretation_layer_present('538ec321-ad35-44f0-90cc-2f0772799865').
narrative_ontology:cs_reading_relation('538ec321-ad35-44f0-90cc-2f0772799865', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('538ec321-ad35-44f0-90cc-2f0772799865', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('538ec321-ad35-44f0-90cc-2f0772799865', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('538ec321-ad35-44f0-90cc-2f0772799865', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('538ec321-ad35-44f0-90cc-2f0772799865', secondary, state_killing_degrades_society).
narrative_ontology:cs_axiom_status(state_killing_degrades_society, holdable).
narrative_ontology:cs_axiom_grounding('538ec321-ad35-44f0-90cc-2f0772799865', state_killing_degrades_society, deontological).
narrative_ontology:cs_reference_frame('538ec321-ad35-44f0-90cc-2f0772799865', universal_human_dignity).
narrative_ontology:cs_drift_state('538ec321-ad35-44f0-90cc-2f0772799865', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('538ec321-ad35-44f0-90cc-2f0772799865', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, society_as_a_whole).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces capital punishment statutes, including sentencing and appeals. Its legitimacy is tied to upholding the law, even when that law permits execution. Could abolish capital punishment but faces political and legal resistance.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Bear the ultimate cost of the constraint: their lives. From the abolitionist perspective, this cost is categorically impermissible, regardless of their guilt or the procedural safeguards in place. They have no exit from the system once sentenced.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, local).

% Bear the emotional and social costs of the execution, often experiencing trauma and stigma. They are trapped by the state's action and have no recourse to reverse the outcome.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed_persons, payer,
    powerless, biographical, trapped, local).

% Actively resist capital punishment, arguing for its categorical impermissibility. They work through legal challenges, public education, and political lobbying to dismantle the constraint. Their 'exit' is the constraint's abolition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_advocates, observer,
    organized, generational, mobile, global).

% Bears the moral cost of state-sanctioned killing, which, from this reading, degrades the ethical fabric of society. Also bears the financial costs of capital appeals, which are often higher than life imprisonment. Exit is through legislative change or constitutional amendment.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, society_as_a_whole, payer,
    moderate, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, from this reading. The constraint is seen as a pure act of state violence, not a coordination mechanism.
% TRANSFER_FUNCTION: Transfers the life of the executed person from themselves to the state, in the name of a perceived 'justice' or 'order' that is rejected as illegitimate by this reading.
% ABSENT_VOICES: The executed persons themselves, whose voices are silenced by the act. Future generations, who would inherit a society that permits state killing.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished overnight, the criminal justice system would be forced to re-evaluate its punitive philosophy, focusing entirely on rehabilitation and life imprisonment. The moral landscape of the state would fundamentally shift, aligning with a categorical imperative against state killing.
% FOUNDING_PROBLEM: The perceived need for ultimate punishment for heinous crimes, and the belief that some crimes forfeit the right to life.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist legal scholars and human rights organizations universally attest that the founding problem (justifying state killing) is morally dead, and that the practice persists due to inertia, political expediency, and a flawed understanding of justice. No external corroboration for the 'live' status of the founding problem exists outside of those who benefit from the constraint's persistence.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   The metrics are stable over time because the categorical impermissibility is a fixed moral stance; the 'extraction' of life is constant. The theater ratio is low (0.1) because, while procedural safeguards might be seen as performative by abolitionists, the act of execution itself is a direct, non-theatrical act of state power. Accessibility collapse is low (0.1) because the abolitionist reading actively seeks and promotes alternatives (life imprisonment), and resistance is high (0.8) due to persistent advocacy.
 *
 * PERSPECTIVAL GAP:
 *   The state judicial system, operating under a retributive or deterrence reading, would perceive itself as a legitimate enforcer of justice, potentially even a beneficiary of social order. However, from the abolitionist reading, its actions are purely extractive and morally indefensible, creating a fundamental divergence in perceived constraint type and legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Executed persons and their families are the primary targets (d=1.0). Society as a whole is also a target, bearing the moral and financial costs (d=0.8). The state judicial system is the agenda-setter, enforcing the constraint, but from this reading, it is not a legitimate beneficiary, as the act itself is illegitimate. Abolitionist advocates are observers, actively resisting the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling state execution as a 'rope' or 'tangled rope' by rejecting the coordination claims of retribution or deterrence. By classifying it as a 'snare,' the framework highlights the pure extraction and suppression inherent in the practice, aligning with the abolitionist view that its mandate (to kill) is illegitimate and harmful, rather than a legitimate function that has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_of_state_killing,
    'Is state killing categorically impermissible, or can it be justified under certain conditions (e.g., retribution, deterrence)?',
    'Philosophical consensus shift, or a global legal norm establishing a jus cogens prohibition on capital punishment.',
    'If categorically impermissible, the constraint is a pure snare. If justifiable, it might be reclassified as a tangled rope (if coordination function is found) or even a rope (if benefits outweigh costs and alternatives are not suppressed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_of_state_killing, conceptual, 'The fundamental moral status of state execution.').

omega_variable(
    founding_problem_legitimacy,
    'Is the ''founding problem'' (need for ultimate punishment) a legitimate basis for state action, or a morally flawed premise?',
    'Societal re-evaluation of justice, punishment, and human rights, leading to a rejection of retributive or deterrence-based justifications for state killing.',
    'If the founding problem is deemed illegitimate, the constraint''s persistence is purely extractive. If deemed legitimate, the constraint might be seen as a degraded rope or piton, where the original function has been corrupted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_legitimacy, preference, 'Legitimacy of the historical justifications for capital punishment.').

omega_variable(
    wrongful_execution_impact,
    'Does the inevitability of wrongful execution fundamentally undermine the legitimacy of capital punishment, even if its theoretical justifications (retribution/deterrence) were accepted?',
    'Empirical data on wrongful convictions and exonerations in capital cases, combined with a societal threshold for acceptable error in irreversible state actions.',
    'If wrongful execution is deemed an unacceptable systemic risk, it reinforces the snare classification by demonstrating the constraint''s inherent flaw and its disproportionate harm, regardless of other justifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_impact, empirical, 'Impact of wrongful executions on the constraint''s legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.95).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.95).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.95).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
