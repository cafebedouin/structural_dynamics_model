% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Categorical Prohibition of State Killing (Abolitionist Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of state killing
 *   legitimacy, asserting a categorical prohibition against capital
 *   punishment based on inherent human dignity. It is one reading of the
 *   'state_killing_legitimacy' kernel, which also includes retributive and
 *   deterrence readings. This reading frames the condemned person as a
 *   rights-bearer whose dignity is violated by state killing, making them a
 *   beneficiary of the constraint, while the state's punitive power is the
 *   victim. The constraint is highly extractive from the state's power, as it
 *   demands the relinquishment of the ultimate sanction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.95).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.88).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.92).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Categorical Prohibition of State Killing (Abolitionist Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '621f84c0-dffb-4a96-8c1f-ce2a214ef262').
narrative_ontology:cs_kernel_codification('621f84c0-dffb-4a96-8c1f-ce2a214ef262', formalized).
narrative_ontology:cs_authority_grounding('621f84c0-dffb-4a96-8c1f-ce2a214ef262', lineage).
narrative_ontology:cs_interpretation_layer_present('621f84c0-dffb-4a96-8c1f-ce2a214ef262').
narrative_ontology:cs_reading_relation('621f84c0-dffb-4a96-8c1f-ce2a214ef262', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('621f84c0-dffb-4a96-8c1f-ce2a214ef262', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('621f84c0-dffb-4a96-8c1f-ce2a214ef262', foundational, human_dignity_is_inviolable).
narrative_ontology:cs_axiom_status(human_dignity_is_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('621f84c0-dffb-4a96-8c1f-ce2a214ef262', human_dignity_is_inviolable, deontological).
narrative_ontology:cs_axiom('621f84c0-dffb-4a96-8c1f-ce2a214ef262', foundational, state_power_is_limited_by_dignity).
narrative_ontology:cs_axiom_status(state_power_is_limited_by_dignity, holdable).
narrative_ontology:cs_axiom_grounding('621f84c0-dffb-4a96-8c1f-ce2a214ef262', state_power_is_limited_by_dignity, deontological).
narrative_ontology:cs_reference_frame('621f84c0-dffb-4a96-8c1f-ce2a214ef262', universal_human_rights_declaration).
narrative_ontology:cs_drift_state('621f84c0-dffb-4a96-8c1f-ce2a214ef262', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('621f84c0-dffb-4a96-8c1f-ce2a214ef262', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, human_rights_advocates).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_punitive_power).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, victims_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As rights-bearers, their inherent dignity is affirmed by the abolition of state killing, regardless of their past actions. They are the direct beneficiaries of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% The state's power to inflict capital punishment is directly curtailed and deemed illegitimate by this constraint. It bears the 'cost' of relinquishing a traditional form of ultimate sanction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_punitive_power, payer,
    institutional, generational, constrained, national).

% Actively campaign for the universal abolition of capital punishment, grounding their arguments in the inherent dignity of all persons. They seek to enforce this constraint globally.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Their arguments for capital punishment based on proportional desert are fundamentally rejected by this reading. They are excluded from the normative framework that grounds this constraint.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retributivist_theorists, excluded,
    moderate, civilizational, analytical, universal).

% Their utilitarian arguments for capital punishment based on crime prevention are deemed irrelevant by this categorical prohibition. They are excluded from the moral calculus of this constraint.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, deterrence_theorists, excluded,
    moderate, biographical, analytical, national).

% May experience a sense of injustice or lack of closure if the state is prohibited from executing offenders, feeling that the dignity of the condemned is prioritized over their suffering or desire for retribution. They bear the emotional cost of this prohibition.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, victims_families, payer,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a universal moral standard that affirms the inherent dignity of every human being, providing a consistent ethical baseline for state action and international human rights law.
% TRANSFER_FUNCTION: Transfers the ultimate power over life and death from the state to the individual, affirming an inviolable right to life regardless of actions, thereby 'extracting' this power from the state.
% ABSENT_VOICES: The voices of those who advocate for capital punishment based on retribution or deterrence are absent from the normative foundation of this constraint, as their premises are categorically rejected. They would argue for the state's right to execute based on desert or utility.
% DISAPPEARANCE_RATIONALE: If this categorical prohibition vanished, the moral landscape of state power would fundamentally shift, potentially re-legitimizing capital punishment and eroding the foundation of universal human rights claims against state violence. Legal and ethical frameworks would need to be re-evaluated.
% FOUNDING_PROBLEM: The problem of state power being used to extinguish human life, leading to irreversible injustices, disproportionate application, and the inherent moral contradiction of a state violating the dignity it purports to protect.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, legal scholars, and philosophical ethicists from outside state punitive apparatuses consistently corroborate that the problem of state killing's legitimacy remains live, citing ongoing executions, wrongful convictions, and the moral imperative to uphold human dignity.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.95) is high because this reading demands a complete surrender of the state's power to execute, regardless of any perceived justification. Suppression (0.88) is high because this constraint actively suppresses alternative justifications for state killing (retribution, deterrence) within its normative framework. Resistance (0.92) is high, reflecting the ongoing global struggle against capital punishment and the strong opposition from proponents of other readings. Accessibility collapse (0.15) is low, as alternative justifications for state killing remain widely accessible and debated. Theater ratio is low (0.1) as the constraint's function is direct and not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates, this constraint is a moral imperative, a 'mountain' of ethical truth. From the perspective of the state's punitive power, it is a 'snare' that binds its traditional authority. The engine's classification will reflect the high extraction from the state, even if proponents view it as a fundamental right.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are full beneficiaries (d=0.0) as their right to life is affirmed. Human rights advocates are agenda-setters and beneficiaries (d=0.1) as they drive the enforcement of this moral standard. The state's punitive power is a full target (d=1.0) as it is directly curtailed. Victims' families are payers (d=0.9) as they bear the emotional cost of the prohibition. Retributivist and deterrence theorists are excluded (d=1.0) as their foundational premises are rejected.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate is a categorical moral prohibition, which is not expected to 'atrophy' but rather to be either upheld or violated. The 'founding problem' of state violence against human dignity remains live, ensuring the constraint's continued relevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''abolition_reading'' of the ''state_killing_legitimacy'' kernel?',
    'Comparison with canonical texts and arguments of abolitionist philosophy and international human rights law.',
    'If misidentified, the analysis of inter-reading relations and axiom status would be flawed, potentially misrepresenting the structural conflict within the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the correct identification of this specific reading within the kernel.').

omega_variable(
    categorical_vs_contingent_prohibition,
    'Is the prohibition against state killing truly categorical, or are there implicit contingent exceptions (e.g., in cases of self-defense or just war)?',
    'Detailed analysis of the philosophical grounding of human dignity and its application in extreme cases, and examination of international legal instruments for any explicit or implicit carve-outs.',
    'If contingent, the extractiveness from state power would be lower, and the constraint might shift towards a ''tangled_rope'' or ''rope'' depending on the nature of the contingencies, as it would no longer be an absolute prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_contingent_prohibition, conceptual, 'Examines the absolute nature of the prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1948, state_killing_legitimacy__abolition_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(stat_tr_t1970, state_killing_legitimacy__abolition_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(stat_tr_t1990, state_killing_legitimacy__abolition_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(stat_tr_t2010, state_killing_legitimacy__abolition_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__abolition_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1948, state_killing_legitimacy__abolition_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(stat_be_t1970, state_killing_legitimacy__abolition_reading, base_extractiveness, 1970, 0.85).
narrative_ontology:measurement(stat_be_t1990, state_killing_legitimacy__abolition_reading, base_extractiveness, 1990, 0.9).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__abolition_reading, base_extractiveness, 2010, 0.93).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__abolition_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1948, state_killing_legitimacy__abolition_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(stat_su_t1970, state_killing_legitimacy__abolition_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(stat_su_t1990, state_killing_legitimacy__abolition_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__abolition_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__abolition_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_legitimacy' kernel. The other readings are 'retributive_reading' and 'deterrence_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
