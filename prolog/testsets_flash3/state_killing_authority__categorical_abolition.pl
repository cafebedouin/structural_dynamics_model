% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Categorical Abolition of State Killing
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the categorical abolitionist reading of state
 *   killing authority: that human life is inalienable and state killing is
 *   inherently impermissible, regardless of crime or consequence. It is
 *   presented as a moral 'mountain' — a fundamental, unchangeable principle.
 *   The metrics reflect this: very low extractiveness and suppression, as it
 *   asserts a natural moral law rather than an enforced extraction. This
 *   reading places the condemned person within the rights-holder set and the
 *   state as a potential violator if it executes. Victims' families are
 *   split, with abolitionist families finding alignment and retributive
 *   families experiencing a cost.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.05).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.1).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.05).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, mountain).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition of State Killing").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:emerges_naturally(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '3e146bc5-3f83-4831-9954-d794fdafc239').
narrative_ontology:cs_kernel_codification('3e146bc5-3f83-4831-9954-d794fdafc239', formalized).
narrative_ontology:cs_authority_grounding('3e146bc5-3f83-4831-9954-d794fdafc239', lineage).
narrative_ontology:cs_interpretation_layer_present('3e146bc5-3f83-4831-9954-d794fdafc239').
narrative_ontology:cs_reading_relation('3e146bc5-3f83-4831-9954-d794fdafc239', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('3e146bc5-3f83-4831-9954-d794fdafc239', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('3e146bc5-3f83-4831-9954-d794fdafc239', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('3e146bc5-3f83-4831-9954-d794fdafc239', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('3e146bc5-3f83-4831-9954-d794fdafc239', foundational, state_killing_is_inherently_wrong).
narrative_ontology:cs_axiom_status(state_killing_is_inherently_wrong, holdable).
narrative_ontology:cs_axiom_grounding('3e146bc5-3f83-4831-9954-d794fdafc239', state_killing_is_inherently_wrong, deontological).
narrative_ontology:cs_reference_frame('3e146bc5-3f83-4831-9954-d794fdafc239', universal_human_dignity).
narrative_ontology:cs_drift_state('3e146bc5-3f83-4831-9954-d794fdafc239', contemporary_global_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3e146bc5-3f83-4831-9954-d794fdafc239', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, victims_families_abolitionist).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, state_authorities).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, victims_families_retributive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their right to life is asserted as inalienable, regardless of their crime. This constraint protects them from state execution, shifting the state's role from executioner to custodian.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% Actively promote and defend the principle of inalienable life and the categorical impermissibility of state killing. They work to codify this principle into law and international treaties.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Are constrained from exercising the ultimate punitive power of execution. They must instead pursue alternative forms of punishment, such as life imprisonment, which may be seen as a cost by those who believe in retributive justice.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_authorities, payer,
    institutional, biographical, constrained, national).

% Families of victims who oppose capital punishment find their moral stance aligned with this constraint, which prevents further state-sanctioned violence. They often feel marginalized by the dominant retributive narratives.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_abolitionist, beneficiary,
    moderate, biographical, constrained, local).

% Families of victims who seek retributive justice (death for death) experience this constraint as a denial of their desired outcome, forcing them to accept alternative punishments. They bear the emotional cost of this perceived injustice.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_retributive, payer,
    organized, biographical, constrained, local).

% Analyze and articulate the philosophical and legal arguments for the inherent impermissibility of state killing, grounding it in deontological ethics and fundamental human rights. They provide the intellectual framework for the constraint.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, legal_scholars_deontological, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal moral and legal baseline for the sanctity of human life, coordinating state action around non-lethal forms of punishment and affirming a shared commitment to human dignity.
% TRANSFER_FUNCTION: Transfers the ultimate power over life and death from the state to an inalienable right held by every individual, regardless of their actions. It transfers the burden of punishment from execution to incarceration.
% ABSENT_VOICES: Those who believe in a state's absolute right to exact retributive justice, or who prioritize deterrence above all else, are structurally excluded from the moral framework of this constraint. Their arguments are deemed irrelevant to the categorical impermissibility of state killing.
% DISAPPEARANCE_RATIONALE: If the principle of categorical abolition vanished, states would be free to reintroduce or expand capital punishment, leading to a significant increase in executions. The legal and moral landscape of criminal justice would fundamentally shift, with profound implications for human rights and state power.
% FOUNDING_PROBLEM: The historical problem of states exercising arbitrary or disproportionate power over the lives of their citizens, leading to irreversible injustices and violations of fundamental human dignity.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal bodies, and historical records of wrongful executions corroborate the ongoing relevance of this problem. The potential for state overreach and irreversible error remains a live concern, attested by independent observers and legal experts globally.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, ExtMetricName, E),
    domain_priors:suppression_score(state_killing_authority__categorical_abolition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_killing_authority__categorical_abolition),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and suppression (0.1) reflect the nature of a moral principle claimed as a 'mountain' — it is not actively extracting from or suppressing agents in the way a constructed constraint would. Its persistence is due to its inherent moral force, not coercion. The accessibility collapse is high (0.95) because, within this framework, there are no legitimate alternatives to respecting the inalienable right to life. Resistance is low (0.05) because, as a moral principle, it is not 'resisted' in the same way a policy is, though its implementation faces political opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates and deontological legal scholars, this constraint is a fundamental moral truth (a mountain). From the perspective of state authorities or retributive victims' families, it is a policy choice that imposes costs and limits their desired actions. The engine's classification will reflect the structural position of each seat, even if the claimed type is 'mountain'.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons and human rights advocates are beneficiaries, as the constraint protects and aligns with their core interests. State authorities and retributive victims' families are payers, as they are constrained from exercising or receiving the desired punitive outcome. Abolitionist victims' families are beneficiaries, finding their moral stance vindicated. Legal scholars are observers, analyzing the principle.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a moral principle, is not subject to mandatrophy in the same way an institutional arrangement might be. Its mandate is inherent and timeless. The classification prevents mislabeling a fundamental moral claim as a temporary or extractive arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_moral_construct,
    'Is the inalienable right to life a genuine natural law (a Mountain) or a moral construct (a Rope/Tangled Rope) that benefits identifiable agents?',
    'Philosophical consensus on the grounding of moral rights, or empirical observation of its universal acceptance across diverse cultures independent of enforcement.',
    'If a construct, its classification would shift to a Rope or Tangled Rope, indicating its persistence relies on active defense and coordination rather than inherent truth, and its beneficiaries are more clearly defined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_moral_construct, conceptual, 'Ambiguity between a fundamental moral truth and a socially constructed norm.').

omega_variable(
    state_authority_limits_grounding,
    'Is the state''s authority to take life limited by inherent moral principles, or by social contract and instrumental considerations?',
    'Analysis of constitutional jurisprudence and international human rights law, tracing the historical evolution of state power and its limitations.',
    'If limited by social contract, the constraint''s ''mountain'' claim is weakened, as its persistence would depend on ongoing agreement rather than inherent truth, potentially shifting it towards a Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_limits_grounding, conceptual, 'The source of limits on state punitive power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.0).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__categorical_abolition, theater_ratio, 10, 0.0).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__categorical_abolition, theater_ratio, 20, 0.0).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__categorical_abolition, theater_ratio, 30, 0.0).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.0).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__categorical_abolition, theater_ratio, 50, 0.0).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__categorical_abolition, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__categorical_abolition, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__categorical_abolition, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__categorical_abolition, base_extractiveness, 50, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__categorical_abolition, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__categorical_abolition, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__categorical_abolition, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__categorical_abolition, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
