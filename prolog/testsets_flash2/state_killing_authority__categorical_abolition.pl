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
    narrative_ontology:affects_constraint/2,
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
 *   killing authority, asserting that life is an inalienable right and state
 *   killing is inherently impermissible, regardless of crime or consequence.
 *   It is presented as a 'mountain' from this reading's perspective,
 *   reflecting a fundamental moral truth. The metrics reflect minimal
 *   extraction and suppression, as the principle itself is seen as a natural
 *   limit on state power, not a human-constructed mechanism for rent-seeking.
 *   The presence of beneficiaries (condemned persons, human rights advocates)
 *   on a claimed mountain triggers False Summit Mountain (FSM) evaluation,
 *   which is appropriate for a principle presented as natural law but with
 *   clear beneficiaries.
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
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '7146a7a5-d271-4662-a762-434acc20c6af').
narrative_ontology:cs_kernel_codification('7146a7a5-d271-4662-a762-434acc20c6af', formalized).
narrative_ontology:cs_authority_grounding('7146a7a5-d271-4662-a762-434acc20c6af', lineage).
narrative_ontology:cs_interpretation_layer_present('7146a7a5-d271-4662-a762-434acc20c6af').
narrative_ontology:cs_reading_relation('7146a7a5-d271-4662-a762-434acc20c6af', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('7146a7a5-d271-4662-a762-434acc20c6af', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('7146a7a5-d271-4662-a762-434acc20c6af', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('7146a7a5-d271-4662-a762-434acc20c6af', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('7146a7a5-d271-4662-a762-434acc20c6af', foundational, state_killing_is_inherently_wrong).
narrative_ontology:cs_axiom_status(state_killing_is_inherently_wrong, holdable).
narrative_ontology:cs_axiom_grounding('7146a7a5-d271-4662-a762-434acc20c6af', state_killing_is_inherently_wrong, deontological).
narrative_ontology:cs_reference_frame('7146a7a5-d271-4662-a762-434acc20c6af', universal_human_dignity).
narrative_ontology:cs_drift_state('7146a7a5-d271-4662-a762-434acc20c6af', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7146a7a5-d271-4662-a762-434acc20c6af', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, victims_families_anti_execution).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, state_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their right to life is asserted as inalienable, regardless of their crimes. This constraint protects them from execution, reclassifying their sentence to life imprisonment.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% Actively promote and defend the principle of inalienable life and the categorical impermissibility of state killing. They work to codify this principle into law and international treaties.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Are constrained from exercising capital punishment, even for heinous crimes. They must re-evaluate their punitive frameworks and invest in alternative sentencing and rehabilitation programs. Their authority to take life is curtailed.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_authorities, payer,
    institutional, biographical, constrained, national).

% Seek retribution and closure through the execution of offenders. This constraint denies them that specific form of justice, often leading to feelings of marginalization and injustice within the legal system.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_pro_execution, excluded,
    moderate, biographical, identity_locked, local).

% Align with the abolitionist stance, believing that state killing perpetuates violence and does not honor their loved ones. This constraint supports their moral and ethical positions.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_anti_execution, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the constitutional and philosophical implications of inalienable rights and the state's power. They provide academic arguments supporting or critiquing the categorical abolitionist position.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, legal_scholars_constitutionalists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal moral baseline for human life, coordinating legal and ethical systems around the principle that the state's power does not extend to taking a citizen's life, thereby preventing arbitrary or politically motivated executions.
% TRANSFER_FUNCTION: Transfers the ultimate power over life and death from the state to the individual, asserting an inalienable right that cannot be forfeited, even by criminal acts. This shifts the burden of punishment to non-lethal means.
% ABSENT_VOICES: The voices of those who believe in retributive justice as a fundamental right, particularly some victims' families, are often marginalized in the discourse that prioritizes inalienable rights. They would argue for the state's right to exact 'a life for a life'.
% DISAPPEARANCE_RATIONALE: If the principle of categorical abolition vanished, states would immediately regain the perceived moral and legal authority to execute, potentially leading to a resurgence of capital punishment and a fundamental shift in human rights jurisprudence. The legal and ethical landscape would be profoundly altered.
% FOUNDING_PROBLEM: The historical problem of states exercising arbitrary or disproportionate power over the lives of their citizens, leading to unjust executions and the devaluation of human life.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal bodies, and historical records corroborate the ongoing risk of state overreach and the need for fundamental protections against arbitrary killing. The problem remains live in many parts of the world, even where abolition is codified.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.05) and suppression (0.1) reflect the view that this principle is a fundamental moral truth, not a mechanism for extracting resources or coercing behavior. Its persistence is due to its inherent rightness, not active enforcement against widespread resistance. The 'emerges_naturally: true' flag is consistent with a mountain claim. The 'accessibility_collapse' is high (0.95) because, from this perspective, there are no legitimate alternatives to respecting inalienable life. Resistance is low (0.05) because, within this moral framework, there is no valid basis for resisting the principle itself, only for resisting its implementation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state authorities or victims' families seeking retribution, this constraint would likely be experienced as a 'snare' or 'tangled rope' that unjustly limits their power or right to justice. However, this story is authored from the categorical abolitionist reading, which views the principle as a fundamental, non-negotiable limit on state power, hence the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons and human rights advocates are beneficiaries, as the constraint directly protects and aligns with their interests. State authorities are payers, as their power is curtailed. Victims' families are split: those seeking retribution are excluded/payers, while those aligned with abolition are beneficiaries. This reflects the moral and legal contestation around the principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the categorical impermissibility of state killing a genuine natural law (mountain) or a constructed moral/legal norm (rope/tangled_rope)?',
    'Cross-cultural and historical analysis of legal systems: if the principle is universally recognized without coercion, it supports natural law; if its adoption correlates with specific political/social movements, it supports a constructed norm.',
    'If a constructed norm, its classification would shift to a rope (if genuinely coordinating) or tangled_rope (if it benefits some at others'' expense), implying active maintenance and potential for contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Ambiguity between inherent moral truth and human-made legal principle.').

omega_variable(
    inalienable_right_scope,
    'Does the ''inalienable right to life'' apply absolutely, or are there extreme circumstances (e.g., self-defense, just war) where it can be overridden, even by the state?',
    'Philosophical debate and legal precedent analysis: examination of cases where the principle is challenged by other moral imperatives.',
    'If the right is not absolute, the constraint''s ''mountain'' status would be weakened, potentially shifting towards a ''rope'' or ''tangled_rope'' that requires more nuanced coordination and enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inalienable_right_scope, conceptual, 'Scope of the inalienable right to life.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1948, state_killing_authority__categorical_abolition, theater_ratio, 1948, 0.0).
narrative_ontology:measurement(stat_tr_t1970, state_killing_authority__categorical_abolition, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(stat_tr_t1990, state_killing_authority__categorical_abolition, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(stat_tr_t2010, state_killing_authority__categorical_abolition, theater_ratio, 2010, 0.0).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__categorical_abolition, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(stat_be_t1948, state_killing_authority__categorical_abolition, base_extractiveness, 1948, 0.05).
narrative_ontology:measurement(stat_be_t1970, state_killing_authority__categorical_abolition, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(stat_be_t1990, state_killing_authority__categorical_abolition, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(stat_be_t2010, state_killing_authority__categorical_abolition, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__categorical_abolition, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1948, state_killing_authority__categorical_abolition, suppression_requirement, 1948, 0.1).
narrative_ontology:measurement(stat_su_t1970, state_killing_authority__categorical_abolition, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(stat_su_t1990, state_killing_authority__categorical_abolition, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(stat_su_t2010, state_killing_authority__categorical_abolition, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__categorical_abolition, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, identity_coordination).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, deterrence_instrument).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_authority' kernel. Its core premise of inalienable life directly contradicts the retributive and deterrence readings, which justify state killing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
