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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'abolition reading' of state execution
 *   authority, which holds that state execution is categorically
 *   impermissible, irrespective of the crime committed or the procedural
 *   safeguards in place. From this perspective, the act of execution itself
 *   is a fundamental violation of human rights and state legitimacy. The
 *   constraint is classified as a Snare because it extracts the ultimate cost
 *   (life) from its victims, with no legitimate coordination function, and
 *   its persistence relies on the suppression of alternative moral
 *   frameworks.
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
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '7573ed84-1b57-4052-a567-3cf86132fc9e').
narrative_ontology:cs_kernel_codification('7573ed84-1b57-4052-a567-3cf86132fc9e', formalized).
narrative_ontology:cs_authority_grounding('7573ed84-1b57-4052-a567-3cf86132fc9e', lineage).
narrative_ontology:cs_interpretation_layer_present('7573ed84-1b57-4052-a567-3cf86132fc9e').
narrative_ontology:cs_reading_relation('7573ed84-1b57-4052-a567-3cf86132fc9e', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('7573ed84-1b57-4052-a567-3cf86132fc9e', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('7573ed84-1b57-4052-a567-3cf86132fc9e', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('7573ed84-1b57-4052-a567-3cf86132fc9e', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('7573ed84-1b57-4052-a567-3cf86132fc9e', foundational, state_lacks_ultimate_authority_over_life).
narrative_ontology:cs_axiom_status(state_lacks_ultimate_authority_over_life, holdable).
narrative_ontology:cs_axiom_grounding('7573ed84-1b57-4052-a567-3cf86132fc9e', state_lacks_ultimate_authority_over_life, deontological).
narrative_ontology:cs_reference_frame('7573ed84-1b57-4052-a567-3cf86132fc9e', universal_human_rights_framework).
narrative_ontology:cs_drift_state('7573ed84-1b57-4052-a567-3cf86132fc9e', contemporary_global_abolition_movement, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7573ed84-1b57-4052-a567-3cf86132fc9e', '').
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

% Bear the ultimate cost of the constraint: their lives. Even if guilty, their execution is seen as an impermissible act by the state. No exit once sentenced and appeals exhausted.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, local).

% Bear the emotional and social costs of the execution, often experiencing prolonged grief and stigma. Their suffering is a direct consequence of the state's action.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed_persons, payer,
    powerless, biographical, trapped, local).

% Actively campaign against capital punishment, viewing it as a fundamental violation of human rights. They collect data on wrongful convictions, racial bias, and the moral implications of state killing. Their goal is to dismantle the constraint entirely.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_advocates, observer,
    organized, generational, mobile, global).

% Bears the moral cost of a state that engages in what this reading considers an impermissible act. The existence of capital punishment degrades the moral standing of the society, even if individual citizens support it.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, society_as_a_whole, payer,
    moderate, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, there is no legitimate coordination function. The state's authority to execute is a power, not a coordination mechanism.
% TRANSFER_FUNCTION: Transfers the life of the executed person from themselves to the state, ostensibly in the name of justice or public safety, but fundamentally as an impermissible act.
% ABSENT_VOICES: The executed persons themselves, whose voices are permanently silenced. Their perspective on the impermissibility of their own execution is central to this reading.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished overnight, the criminal justice system would be forced to re-evaluate its ultimate punishment, likely shifting to life imprisonment without parole. The moral landscape of the state would fundamentally change, aligning with a categorical imperative against state killing.
% FOUNDING_PROBLEM: The problem of how a state should respond to the most heinous crimes, and whether it possesses the ultimate authority over its citizens' lives.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist advocates and human rights organizations globally attest that the problem of state killing remains live and unresolved, with ongoing debates about the moral limits of state power. This is corroborated by international legal bodies and philosophical traditions outside the state's direct beneficiaries.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is maximal (0.95) because the constraint takes a life, which is an irreversible and absolute cost. Suppression is high (0.9) because the state's authority to execute is maintained through legal and institutional power, actively suppressing challenges to its legitimacy in this domain. Theater ratio is low (0.1) as the act of execution is a direct, unambiguous exercise of state power, not a performance masking an atrophied function. Resistance is high (0.8) due to ongoing, organized abolitionist movements globally. Accessibility collapse is low (0.1) because the moral argument for abolition remains accessible and widely articulated, even if not universally adopted.
 *
 * PERSPECTIVAL GAP:
 *   The state judicial system's perspective would likely frame execution as a legitimate exercise of justice, whereas this abolitionist reading sees it as an impermissible act. This divergence is central to the contest over the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Executed persons and their families are full targets (d=1.0) as they bear the direct, irreversible cost. Society as a whole is also a target (d=0.8) as it bears the moral degradation of state killing. The state judicial system, while administering the constraint, is not a beneficiary in this reading, as the act itself is impermissible; its directionality is closer to symmetric (d=0.5) as it is bound by the laws it enforces, even if those laws are morally flawed. Abolitionist advocates are observers (d=0.5) seeking to dismantle the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''abolition reading'' of state execution authority, or does it implicitly incorporate elements of other readings?',
    'Careful textual analysis of the underlying philosophical arguments and legal precedents to ensure strict adherence to the categorical impermissibility axiom, without recourse to arguments about wrongful conviction rates or disproportionate application (which are secondary to the categorical claim).',
    'If elements of other readings are present, the extractiveness and suppression metrics might be diluted, and the classification could shift towards a ''tangled_rope'' or ''piton'' if the categorical imperative is weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring the purity of the abolitionist reading''s structural claims.').

omega_variable(
    moral_cost_quantification,
    'How can the ''moral cost to society as a whole'' be empirically or conceptually quantified to justify its high extractiveness in this reading?',
    'Development of a robust framework for measuring societal moral degradation, potentially through public opinion shifts, international human rights indices, or philosophical consensus on state legitimacy.',
    'If the moral cost is deemed less significant, the overall extractiveness might be lower, potentially weakening the ''snare'' classification to a ''tangled_rope'' if some coordination function (e.g., perceived justice by some segments of society) is implicitly acknowledged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_cost_quantification, conceptual, 'Quantifying the diffuse moral cost of state execution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
